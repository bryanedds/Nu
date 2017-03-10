// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open FSharp.Reflection
open Prime
open Prime.Scripting
#nowarn "21"
#nowarn "40"
module ScriptingMarshalling =

    let rec tryImport (tryImportExt : obj -> Type -> Expr option) (value : obj) (ty : Type) : Expr option =

        // try to import from the custom types
        let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
        match Importers.TryGetValue ty.Name with
        | (true, tryImportFn) -> tryImportFn tryImportExt value ty
        | (false, _) ->

            // failing that, try import as Tuple
            if FSharpType.IsTuple ty then
                let tupleFields = FSharpValue.GetTupleFields value
                let tupleElementTypes = FSharpType.GetTupleElements ty
                let tupleFieldOpts = Array.mapi (fun i tupleField -> tryImport tryImportExt tupleField tupleElementTypes.[i]) tupleFields
                match Array.definitizePlus tupleFieldOpts with
                | (true, tupleFields) -> Some (Tuple tupleFields)
                | (false, _) -> None

            // or as Record
            elif FSharpType.IsRecord ty then
                let recordFields = FSharpValue.GetRecordFields value
                let recordFieldInfos = FSharpType.GetRecordFields ty
                let recordFieldOpts = Array.mapi (fun i recordField -> tryImport tryImportExt recordField recordFieldInfos.[i].PropertyType) recordFields
                match Array.definitizePlus recordFieldOpts with
                | (true, recordFields) ->
                    let recordName = match ty.Name.IndexOf '`' with -1 -> ty.Name | index -> ty.Name.Substring (0, index)
                    let recordFieldMap = recordFieldInfos |> Array.mapi (fun i (field : Reflection.PropertyInfo) -> (field.Name, i)) |> Map.ofArray
                    Some (Record (recordName, recordFieldMap, recordFields))
                | (false, _) -> None

            // or as Union
            elif FSharpType.IsUnion ty then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (value, ty)
                let unionFieldTypes = unionCase.GetFields ()
                if not ^ Array.isEmpty unionFields then
                    let unionFieldOpts = Array.mapi (fun i unionField -> tryImport tryImportExt unionField unionFieldTypes.[i].PropertyType) unionFields
                    match Array.definitizePlus unionFieldOpts with
                    | (true, unionFields) -> Some (Phrase (unionCase.Name, unionFields))
                    | (false, _) -> None
                else Some (Phrase (unionCase.Name, [||]))

            // otherwise, we have no conversion
            else None

    and tryImportGuid (value : obj) (_ : Type) =
        Some (String ((GuidConverter ()).ConvertToString value))

    and tryImportKeyValuePair tryImportExt (value : obj) (ty : Type) =
        let gargs = ty.GetGenericArguments ()
        let kvp = Reflection.objToKeyValuePair value
        let keyOpt = tryImport tryImportExt kvp.Key gargs.[0]
        let valueOpt = tryImport tryImportExt kvp.Value gargs.[1]
        match (keyOpt, valueOpt) with
        | (Some key, Some value) -> Some (Tuple [|key; value|])
        | (_, _) -> None

    and tryImportAddress (value : obj) (ty : Type) =
        Some (String ((AddressConverter ty).ConvertToString value))

    and tryImportRelation (value : obj) (ty : Type) =
        Some (String ((RelationConverter ty).ConvertToString value))

    and tryImportOption tryImportExt (value : obj) (ty : Type) =
        let valueType = (ty.GetGenericArguments ()).[0]
        let opt = Reflection.objToOption value
        match opt with
        | Some value ->
            match tryImport tryImportExt value valueType with
            | Some value -> Some (Option (Some value))
            | None -> None
        | None -> Some (Option None)

    and tryImportList tryImportExt (value : obj) (ty : Type) =
        let itemType = (ty.GetGenericArguments ()).[0]
        let objList = Reflection.objToObjList value
        let itemOpts = List.map (fun item -> tryImport tryImportExt item itemType) objList
        match List.definitizePlus itemOpts with
        | (true, items) -> Some (List items)
        | (false, _) -> None

    and tryImportSet tryImportExt (value : obj) (ty : Type) =
        let itemType = (ty.GetGenericArguments ()).[0]
        let items = Reflection.objToComparableSet value
        let itemOpts = Seq.map (fun item -> tryImport tryImportExt item itemType) items
        match Seq.definitizePlus itemOpts with
        | (true, items) -> Some (Ring (Set.ofSeq items))
        | (false, _) -> None

    and tryImportMap tryImportExt (value : obj) (ty : Type) =
        let gargs = ty.GetGenericArguments ()
        let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
        let items = Reflection.objToObjList value
        let itemOpts = List.map (fun item -> tryImport tryImportExt item itemType) items
        match Seq.definitizePlus itemOpts with
        | (true, items) -> Some (Ring (Set.ofSeq items))
        | (false, _) -> None

    and Importers : Dictionary<string, (obj -> Type -> Expr option) -> obj -> Type -> Expr option> =
        [(typeof<unit>.Name, (fun _ _ _ -> Unit |> Some))
         (typeof<bool>.Name, (fun _ (value : obj) _ -> match value with :? bool as bool -> Some (Bool bool) | _ -> None))
         (typeof<int>.Name, (fun _ (value : obj) _ -> match value with :? int as int -> Some (Int int) | _ -> None))
         (typeof<int64>.Name, (fun _ (value : obj) _ -> match value with :? int64 as int64 -> Some (Int64 int64) | _ -> None))
         (typeof<single>.Name, (fun _ (value : obj) _ -> match value with :? single as single -> Some (Single single) | _ -> None))
         (typeof<double>.Name, (fun _ (value : obj) _ -> match value with :? double as double -> Some (Double double) | _ -> None))
         (typeof<char>.Name, (fun _ (value : obj) _ -> match value with :? char as char -> Some (String (string char)) | _ -> None))
         (typeof<string>.Name, (fun _ (value : obj) _ -> match value with :? string as str -> Some (String str) | _ -> None))
         (typedefof<Guid>.Name, (fun _ value ty -> tryImportGuid value ty))
         (typedefof<KeyValuePair<_, _>>.Name, (fun tryImportExt value ty -> tryImportKeyValuePair tryImportExt value ty))
         (typedefof<_ Address>.Name, (fun _ value ty -> tryImportAddress value ty))
         (typedefof<_ Relation>.Name, (fun _ value ty -> tryImportRelation value ty))
         (typedefof<_ option>.Name, (fun tryImportExt value ty -> tryImportOption tryImportExt value ty))
         (typedefof<_ list>.Name, (fun tryImportExt value ty -> tryImportList tryImportExt value ty))
         (typedefof<_ Set>.Name, (fun tryImportExt value ty -> tryImportSet tryImportExt value ty))
         (typedefof<Map<_, _>>.Name, (fun tryImportExt value ty -> tryImportMap tryImportExt value ty))] |>
        dictPlus

    let rec tryExport tryExportExt (value : Expr) (ty : Type) =

        // try to export from the custom types
        match Exporters.TryGetValue ty.Name with
        | (true, tryExport) -> tryExport tryExportExt value ty
        | (false, _) ->

            // failing that, try export as Tuple
            if FSharpType.IsTuple ty then
                match value with
                | Tuple fields
                | Phrase (_, fields)
                | Record (_, _, fields) ->
                    let fieldTypes = FSharpType.GetTupleElements ty
                    let fieldOpts = Array.mapi (fun i fieldSymbol -> tryExport tryExportExt fieldSymbol fieldTypes.[i]) fields
                    match Array.definitizePlus fieldOpts with
                    | (true, fields) -> Some (FSharpValue.MakeTuple (fields, ty))
                    | (false, _) -> None
                | _ -> None

            // or as Record
            elif FSharpType.IsRecord ty then
                match value with
                | Phrase (_, fields)
                | Record (_, _, fields) ->
                    let fieldTypes = FSharpType.GetRecordFields ty
                    let fieldOpts = Array.mapi (fun i fieldSymbol -> tryExport tryExportExt fieldSymbol fieldTypes.[i].PropertyType) fields
                    match Array.definitizePlus fieldOpts with
                    | (true, fields) -> Some (FSharpValue.MakeRecord (ty, fields))
                    | (false, _) -> None
                | _ -> None

            // or as Union
            elif FSharpType.IsUnion ty && ty <> typeof<string list> then
                let unionCases = FSharpType.GetUnionCases ty
                match value with
                | Keyword name ->
                    match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = name) unionCases with
                    | Some unionCase -> Some (FSharpValue.MakeUnion (unionCase, [||]))
                    | None -> None
                | Phrase (name, fields)
                | Record (name, _, fields) ->
                    match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = name) unionCases with
                    | Some unionCase ->
                        let unionFieldTypes = unionCase.GetFields ()
                        let unionValueOpts = Array.mapi (fun i unionSymbol -> tryExport tryExportExt unionSymbol unionFieldTypes.[i].PropertyType) fields
                        match Array.definitizePlus unionValueOpts with
                        | (true, unionValues) -> Some (FSharpValue.MakeUnion (unionCase, unionValues))
                        | (false, _) -> None
                    | None -> None
                | _ -> None

            // otherwise, we have no conversion
            else None

    and tryExportGuid (address : Expr) (_ : Type) =
        match address with
        | String str | Keyword str -> Some ((GuidConverter ()).ConvertFromString str)
        | _ -> None

    and tryExportKvp tryExportExt (tuple : Expr) (ty : Type) =
        match tuple with
        | Tuple [|fst; snd|] ->
            match ty.GetGenericArguments () with
            | [|fstType; sndType|] ->
                let pairType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|fstType; sndType|]
                let fstOpt = tryExport tryExportExt fst fstType
                let sndOpt = tryExport tryExportExt snd sndType
                match (fstOpt, sndOpt) with
                | (Some fst, Some snd) -> Some (Reflection.objsToKeyValuePair fst snd pairType)
                | (_, _) -> None
            | _ -> None
        | _ -> None

    and tryExportAddress (address : Expr) (ty : Type) =
        match address with
        | String str | Keyword str -> Some ((AddressConverter ty).ConvertFromString str)
        | _ -> None

    and tryExportRelation (relation : Expr) (ty : Type) =
        match relation with
        | String str | Keyword str -> Some ((RelationConverter ty).ConvertFromString str)
        | _ -> None

    and tryExportOption tryExportExt (opt : Expr) (ty : Type) =
        match opt with
        | Option opt ->
            match opt with
            | Some value ->
                let valueType = (ty.GetGenericArguments ()).[0]
                match tryExport tryExportExt value valueType with
                | Some value -> Some (Some value :> obj)
                | None -> None
            | None -> Some (None :> obj)
        | _ -> None

    and tryExportList tryExportExt (list : Expr) (ty : Type) =
        match list with
        | List list ->
            let garg = ty.GetGenericArguments () |> Array.item 0
            let itemType = if garg.IsGenericTypeDefinition then garg.GetGenericTypeDefinition () else garg
            let itemOpts = List.map (fun item -> tryExport tryExportExt item itemType) list
            match List.definitizePlus itemOpts with
            | (true, items) -> Some (Reflection.objsToList ty items)
            | (false, _) -> None
        | _ -> None

    and tryExportSet tryExportExt (ring : Expr) (ty : Type) =
        match ring with
        | Ring set ->
            let elementType = (ty.GetGenericArguments ()).[0]
            let elementOpts = Seq.map (fun item -> tryExport tryExportExt item elementType) set
            match Seq.definitizePlus elementOpts with
            | (true, elements) -> Some (Reflection.objsToSet ty elements)
            | (false, _) -> None
        | _ -> None

    and tryExportMap tryExportExt (table : Expr) (ty : Type) =
        match table with
        | Table map ->
            match ty.GetGenericArguments () with
            | [|fstType; sndType|] ->
                let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                let pairOpts = Seq.map (fun (kvp : KeyValuePair<_, _>) -> tryExport tryExportExt (Tuple [|kvp.Key; kvp.Value|]) pairType) map
                match Seq.definitizePlus pairOpts with
                | (true, pairs) -> Some (Reflection.pairsToMap ty pairs)
                | (false, _) -> None
            | _ -> None
        | _ -> None

    and Exporters : Dictionary<string, (Expr -> Type -> obj option) -> Expr -> Type -> obj option> =
        [(typeof<unit>.Name, fun _ _ _ -> () :> obj |> Some)
         (typeof<bool>.Name, fun _ evaled _ -> match evaled with Bool value -> value :> obj |> Some | _ -> None)
         (typeof<int>.Name, fun _ evaled _ -> match evaled with Int value -> value :> obj |> Some | _ -> None)
         (typeof<int64>.Name, fun _ evaled _ -> match evaled with Int64 value -> value :> obj |> Some | _ -> None)
         (typeof<single>.Name, fun _ evaled _ -> match evaled with Single value -> value :> obj |> Some | _ -> None)
         (typeof<double>.Name, fun _ evaled _ -> match evaled with Double value -> value :> obj |> Some | _ -> None)
         (typeof<char>.Name, fun _ evaled _ -> match evaled with String value when value.Length = 1 -> value.[0] :> obj |> Some | _ -> None)
         (typeof<string>.Name, fun _ evaled _ -> match evaled with String value -> value :> obj |> Some | _ -> None)
         (typedefof<Guid>.Name, fun _ evaled ty -> tryExportGuid evaled ty)
         (typedefof<KeyValuePair<_, _>>.Name, tryExportKvp)
         (typedefof<_ Address>.Name, fun _ evaled ty -> tryExportAddress evaled ty)
         (typedefof<_ Relation>.Name, fun _ evaled ty -> tryExportRelation evaled ty)
         (typedefof<_ option>.Name, tryExportOption)
         (typedefof<_ list>.Name, tryExportList)
         (typedefof<_ Set>.Name, tryExportSet)
         (typedefof<Map<_, _>>.Name, tryExportMap)] |>
        dictPlus