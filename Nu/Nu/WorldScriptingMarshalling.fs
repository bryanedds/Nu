// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Diagnostics
open FSharp.Reflection
open OpenTK
open Prime
open Nu
open Nu.Scripting
#nowarn "21"
#nowarn "40"

[<AutoOpen>]
module WorldScriptingMarshalling =

    module Scripting =

        let rec tryImport (value : obj) (ty : Type) =

            // try to import from the custom types
            let ty = if ty.IsGenericTypeDefinition then ty.GetGenericTypeDefinition () else ty
            match Importers.TryGetValue ty.Name with
            | (true, tryImportFn) -> tryImportFn value ty
            | (false, _) ->

                // failing that, try import as Tuple
                if FSharpType.IsTuple ty then
                    let tupleFields = FSharpValue.GetTupleFields value
                    let tupleElementTypes = FSharpType.GetTupleElements ty
                    let tupleFieldOpts = Array.mapi (fun i tupleField -> tryImport tupleField tupleElementTypes.[i]) tupleFields
                    match Array.definitizePlus tupleFieldOpts with
                    | (true, tupleFields) -> Some (Tuple tupleFields)
                    | (false, _) -> None

                // or as Record
                elif FSharpType.IsRecord ty then
                    let recordFields = FSharpValue.GetRecordFields value
                    let recordFieldTypes = FSharpType.GetRecordFields ty
                    let recordFieldOpts = Array.mapi (fun i recordField -> tryImport recordField recordFieldTypes.[i].PropertyType) recordFields
                    match Array.definitizePlus recordFieldOpts with
                    | (true, recordFields) ->
                        let recordName = match ty.Name.IndexOf '`' with -1 -> ty.Name | index -> ty.Name.Substring (0, index)
                        Some (Keyphrase (recordName, recordFields))
                    | (false, _) -> None

                // or as Union
                elif FSharpType.IsUnion ty then
                    let (unionCase, unionFields) = FSharpValue.GetUnionFields (value, ty)
                    let unionFieldTypes = unionCase.GetFields ()
                    if not ^ Array.isEmpty unionFields then
                        let unionFieldOpts = Array.mapi (fun i unionField -> tryImport unionField unionFieldTypes.[i].PropertyType) unionFields
                        match Array.definitizePlus unionFieldOpts with
                        | (true, unionFields) -> Some (Keyphrase (unionCase.Name, unionFields))
                        | (false, _) -> None
                    else Some (Keyphrase (unionCase.Name, [||]))

                // otherwise, we have no conversion
                else None

        and tryImportList (value : obj) (ty : Type) =
            try let garg = (ty.GetGenericArguments ()).[0]
                let objList = Reflection.objToObjList value
                let evaledListOpts = List.map (fun item -> tryImport item garg) objList
                match List.definitizePlus evaledListOpts with
                | (true, evaledList) -> Some (List evaledList)
                | (false, _) -> None
            with _ -> None

        and Importers : Dictionary<string, obj -> Type -> Expr option> =
            [(typeof<unit>.Name, (fun _ _ -> Unit |> Some))
             (typeof<bool>.Name, (fun (value : obj) _ -> match value with :? bool as bool -> Some (Bool bool) | _ -> None))
             (typeof<int>.Name, (fun (value : obj) _ -> match value with :? int as int -> Some (Int int) | _ -> None))
             (typeof<int64>.Name, (fun (value : obj) _ -> match value with :? int64 as int64 -> Some (Int64 int64) | _ -> None))
             (typeof<single>.Name, (fun (value : obj) _ -> match value with :? single as single -> Some (Single single) | _ -> None))
             (typeof<double>.Name, (fun (value : obj) _ -> match value with :? double as double -> Some (Double double) | _ -> None))
             (typeof<Vector2>.Name, (fun (value : obj) _ -> match value with :? Vector2 as vector2 -> Some (Vector2 vector2) | _ -> None))
             (typedefof<_ list>.Name, (fun value ty -> tryImportList value ty))] |>
             // TODO: remaining importers
            dictPlus

        let rec tryExport (value : Expr) (ty : Type) =
            
            // try to export from the custom types
            match Exporters.TryGetValue ty.Name with
            | (true, tryExport) -> tryExport value ty
            | (false, _) ->

                // failing that, try export as Tuple
                if FSharpType.IsTuple ty then
                    match value with
                    | Tuple elements ->
                        let elementTypes = FSharpType.GetTupleElements ty
                        let elementOpts = Array.mapi (fun i elementSymbol -> tryExport elementSymbol elementTypes.[i]) elements
                        match Array.definitizePlus elementOpts with
                        | (true, elements) -> Some (FSharpValue.MakeTuple (elements, ty))
                        | (false, _) -> None
                    | _ -> None

                // or as Record
                elif FSharpType.IsRecord ty then
                    match value with
                    | Keyphrase (_, phrase) ->
                        let fieldTypes = FSharpType.GetRecordFields ty
                        let fieldOpts = Array.mapi (fun i fieldSymbol -> tryExport fieldSymbol fieldTypes.[i].PropertyType) phrase
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
                    | Keyphrase (name, phrase) ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = name) unionCases with
                        | Some unionCase ->
                            let unionFieldTypes = unionCase.GetFields ()
                            let unionValueOpts = Array.mapi (fun i unionSymbol -> tryExport unionSymbol unionFieldTypes.[i].PropertyType) phrase
                            match Array.definitizePlus unionValueOpts with
                            | (true, unionValues) -> Some (FSharpValue.MakeUnion (unionCase, unionValues))
                            | (false, _) -> None
                        | None -> None
                    | _ -> None

                // otherwise, we have no conversion
                else None

        and tryExportList (evaledList : Expr) (ty : Type) =
            match evaledList with
            | List list ->
                let garg = ty.GetGenericArguments () |> Array.item 0
                let itemType = if garg.IsGenericTypeDefinition then garg.GetGenericTypeDefinition () else garg
                let itemOpts = List.map (fun evaledItem -> tryExport evaledItem itemType) list
                match List.definitizePlus itemOpts with
                | (true, items) -> Some (Reflection.objsToList ty items)
                | (false, _) -> None
            | _ -> None

        and Exporters : Dictionary<string, Expr -> Type -> obj option> =
            [(typeof<unit>.Name, (fun _ _ -> () :> obj |> Some))
             (typeof<bool>.Name, (fun evaled _ -> match evaled with Bool value -> value :> obj |> Some | _ -> None))
             (typeof<int>.Name, (fun evaled _ -> match evaled with Int value -> value :> obj |> Some | _ -> None))
             (typeof<int64>.Name, (fun evaled _ -> match evaled with Int64 value -> value :> obj |> Some | _ -> None))
             (typeof<single>.Name, (fun evaled _ -> match evaled with Single value -> value :> obj |> Some | _ -> None))
             (typeof<double>.Name, (fun evaled _ -> match evaled with Double value -> value :> obj |> Some | _ -> None))
             (typeof<Vector2>.Name, (fun evaled _ -> match evaled with Vector2 value -> value :> obj |> Some | _ -> None))
             (typedefof<_ list>.Name, tryExportList)] |>
             // TODO: remaining exporters
            dictPlus