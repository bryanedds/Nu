// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

type [<NoEquality; NoComparison>] AlgebraicSource =
    { AlgebraicValue : obj }

type AlgebraicConverter (targetType : Type) =
    inherit TypeConverter ()

    // NOTE: had to do some reflection hacking get this assembly as it was the only way I could
    // access ListModule.OfSeq dynamically.
    static let FSharpCoreAssembly =
        Array.find
            (fun (assembly : Assembly) -> assembly.FullName.StartsWith ("FSharp.Core,", StringComparison.Ordinal))
            (AppDomain.CurrentDomain.GetAssemblies ())

    let objToObjList (source : obj) =
        let iEnumerable = source :?> IEnumerable
        List.ofSeq ^ enumerable<obj> iEnumerable

    let objToKeyValuePair (source : obj) =
        let kvpType = source.GetType ()
        let key = (kvpType.GetProperty "Key").GetValue source
        let value = (kvpType.GetProperty "Value").GetValue source
        KeyValuePair (key, value)

    let objToComparableSet (source : obj) =
        let iEnumerable = source :?> IEnumerable
        Set.ofSeq ^ enumerable<IComparable> iEnumerable

    let rec toString source (sourceType : Type) =

        match sourceType.TryGetCustomTypeConverter () with
        | Some typeConverter ->
            let typeConverterType = typeConverter.GetType ()
            let convertToString = typeConverterType.GetMethod ("ConvertToString", [|typeof<obj>|])
            convertToString.Invoke (typeConverter, [|source|]) :?> string
        | None ->

            if sourceType.Name = typedefof<KeyValuePair<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let kvp = objToKeyValuePair source
                let kvpStrs = KeyValuePair (toString kvp.Key gargs.[0], toString kvp.Value gargs.[1])
                let kvpsStr = kvpStrs.Key + AlgebraicReader.SeparatorStr + kvpStrs.Value
                AlgebraicReader.OpenComplexValueStr + kvpsStr + AlgebraicReader.CloseComplexValueStr

            elif sourceType.Name = typedefof<_ list>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToObjList source
                let itemsStrs = List.map (fun item -> toString item gargs.[0]) items
                let itemsStr = String.Join (AlgebraicReader.SeparatorStr, itemsStrs)
                AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr

            elif sourceType.Name = typedefof<_ Set>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToComparableSet source
                let itemsStrs = Set.map (fun item -> toString item gargs.[0]) items
                let itemsStr = String.Join (AlgebraicReader.SeparatorStr, itemsStrs)
                AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr

            elif sourceType.Name = typedefof<Map<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let itemsStrs = List.map (fun item -> toString item itemType) items
                let itemsStr = String.Join (AlgebraicReader.SeparatorStr, itemsStrs)
                AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr

            elif sourceType.Name = typedefof<Vmap<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<Tuple<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let itemsStrs = List.map (fun item -> toString item itemType) items
                let itemsStr = String.Join (AlgebraicReader.SeparatorStr, itemsStrs)
                AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr

            elif FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleElementTypes = FSharpType.GetTupleElements sourceType
                let tupleFieldStrs =
                    List.mapi
                        (fun i tupleField ->
                            let tupleFieldType = tupleElementTypes.[i]
                            toString tupleField tupleFieldType)
                        (List.ofArray tupleFields)
                let tupleStr = String.Join (AlgebraicReader.SeparatorStr, tupleFieldStrs)
                AlgebraicReader.OpenComplexValueStr + tupleStr + AlgebraicReader.CloseComplexValueStr

            elif FSharpType.IsRecord sourceType then
                let recordFields = FSharpValue.GetRecordFields source
                let recordFieldTypes = FSharpType.GetRecordFields sourceType
                let recordFieldStrs =
                    List.mapi
                        (fun i recordField ->
                            let recordFieldType = recordFieldTypes.[i].PropertyType
                            toString recordField recordFieldType)
                        (List.ofArray recordFields)
                let recordStr = String.Join (AlgebraicReader.SeparatorStr, recordFieldStrs)
                AlgebraicReader.OpenComplexValueStr + recordStr + AlgebraicReader.CloseComplexValueStr

            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let unionFieldTypes = unionCase.GetFields ()
                if not ^ Array.isEmpty unionFields then
                    let unionFieldStrs =
                        List.mapi
                            (fun i unionField ->
                                let unionFieldType = unionFieldTypes.[i].PropertyType
                                toString unionField unionFieldType)
                            (List.ofArray unionFields)
                    let unionStrs = unionCase.Name :: unionFieldStrs
                    let unionStr = String.Join (AlgebraicReader.SeparatorStr, unionStrs)
                    AlgebraicReader.OpenComplexValueStr + unionStr + AlgebraicReader.CloseComplexValueStr
                else unionCase.Name

            else (TypeDescriptor.GetConverter sourceType).ConvertToString source

    let rec fromReaderValue (destType : Type) (readerValue : obj) =

        // determine if conversion is trivial enough to ask of a custom TypeConverter
        let conversionIsTrivial =
            let readerValueType = readerValue.GetType ()
            readerValueType = typeof<string> || readerValueType = destType

        // convert reader value to destination type
        match destType.TryGetCustomTypeConverter () with
        | Some typeConverter when conversionIsTrivial ->

            // convert using custom TypeConverter
            let typeConverterType = typeConverter.GetType ()
            let convertFrom = typeConverterType.GetMethod ("ConvertFrom", [|typeof<obj>|])
            convertFrom.Invoke (typeConverter, [|readerValue|])

        // convert using more detailed algebraic conversion process
        | Some _ | None ->
            if destType.Name = typedefof<_ list>.Name then
                match readerValue with
                | :? (obj list) as readerValueList ->
                    let gargs = destType.GetGenericArguments ()
                    let elementType = gargs.[0]
                    let list = List.map (fromReaderValue elementType) readerValueList
                    let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                    let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.ListModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                    ofSeq.Invoke (null, [|cast.Invoke (null, [|list|])|])
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

            elif destType.Name = typedefof<_ Set>.Name then
                match readerValue with
                | :? (obj list) as readerValueList ->
                    let gargs = destType.GetGenericArguments ()
                    let elementType = gargs.[0]
                    let list = List.map (fromReaderValue elementType) readerValueList
                    let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                    let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.SetModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                    ofSeq.Invoke (null, [|cast.Invoke (null, [|list|])|])
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

            elif destType.Name = typedefof<Map<_, _>>.Name then
                match readerValue with
                | :? (obj list) as readerPairList ->
                    let gargs = destType.GetGenericArguments ()
                    match gargs with
                    | [|fstType; sndType|] ->
                        let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                        let pairList = List.map (fromReaderValue pairType) readerPairList
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.MapModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                    | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

            elif destType.Name = typedefof<Vmap<_, _>>.Name then
                match readerValue with
                | :? (obj list) as readerPairList ->
                    let gargs = destType.GetGenericArguments ()
                    match gargs with
                    | [|fstType; sndType|] ->
                        let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                        let pairList = List.map (fromReaderValue pairType) readerPairList
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                        let ofSeq = ((typedefof<Vmap<_, _>>.Assembly.GetType "Prime.VmapModule").GetMethod ("ofSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                    | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

            elif FSharpType.IsTuple destType then
                let tupleReaderValues = readerValue :?> obj list
                let tupleElementTypes = FSharpType.GetTupleElements destType
                let tupleValues =
                    List.mapi
                        (fun i tupleReaderValue ->
                            let tupleElementType = tupleElementTypes.[i]
                            fromReaderValue tupleElementType tupleReaderValue)
                        tupleReaderValues
                FSharpValue.MakeTuple (Array.ofList tupleValues, destType)
    
            elif FSharpType.IsRecord destType then
                let recordReaderValues = readerValue :?> obj list
                let recordFieldTypes = FSharpType.GetRecordFields destType
                let recordValues =
                    List.mapi
                        (fun i recordReaderValue ->
                            let recordFieldType = recordFieldTypes.[i].PropertyType
                            fromReaderValue recordFieldType recordReaderValue)
                        recordReaderValues
                FSharpValue.MakeRecord (destType, Array.ofList recordValues)
    
            elif FSharpType.IsUnion destType && destType <> typeof<string list> then
                let unionCases = FSharpType.GetUnionCases destType
                match readerValue with
                | :? (obj list) ->
                    match readerValue :?> obj list with
                    | readerValueHead :: readerValueTail ->
                        let unionName = readerValueHead :?> string
                        let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                        let unionFieldTypes = unionCase.GetFields ()
                        let unionValues =
                            List.mapi
                                (fun i unionReaderValue ->
                                    let unionFieldType = unionFieldTypes.[i].PropertyType
                                    fromReaderValue unionFieldType unionReaderValue)
                                readerValueTail
                        FSharpValue.MakeUnion (unionCase, Array.ofList unionValues)
                    | _ -> failwith "Invalid AlgebraicConverter conversion from union reader value."
                | :? string as unionName ->
                    let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                    FSharpValue.MakeUnion (unionCase, [||])
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

            else
                match readerValue with
                | :? string as readerValueStr -> (TypeDescriptor.GetConverter destType).ConvertFromString readerValueStr
                | _ -> (TypeDescriptor.GetConverter destType).ConvertFrom { AlgebraicValue = readerValue }

    let fromString (destType : Type) (source : string) =
        let readerValue = AlgebraicReader.stringToValue source
        fromReaderValue destType readerValue

    override this.CanConvertTo (_, _) =
        true
    
    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            match source with
            | null ->
                if FSharpType.IsUnion targetType
                then (FSharpType.GetUnionCases targetType).[0].Name :> obj
                else String.Empty :> obj
            | _ ->
                let sourceType = source.GetType ()
                toString source sourceType :> obj
        else
            let sourceType = source.GetType ()
            if destType = sourceType then source
            else failwith "Invalid AlgebraicConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = targetType
    
    override this.ConvertFrom (_, _, source) =
        match source with
        | null -> source
        | _ ->
            let sourceType = source.GetType ()
            if sourceType <> targetType then
                match source with
                | :? string as sourceStr -> fromString targetType sourceStr
                | _ -> failwith "Invalid AlgebraicConverter conversion from string."
            else source