namespace Prime
open System
open System.Collections
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

[<AutoOpen>]
module AlgebraicConverterModule =

    let FSharpAssembly =
        Array.find
            (fun (assembly : Assembly) -> assembly.FullName.Contains "FSharp.Core")
            (AppDomain.CurrentDomain.GetAssemblies ())

    type AlgebraicConverter (targetType : Type) =
        inherit TypeConverter ()

        let objToObjList (source : obj) =
            let iEnumerable = source :?> IEnumerable
            List.ofSeq <| enumerable<obj> iEnumerable

        let objToComparableSet (source : obj) =
            let iEnumerable = source :?> IEnumerable
            Set.ofSeq <| enumerable<IComparable> iEnumerable

        let rec toString source (sourceType : Type) =

            match sourceType.TryGetCustomTypeConverter () with
            | Some typeConverter ->
                let typeConverterType = typeConverter.GetType ()
                let convertToString = typeConverterType.GetMethod ("ConvertToString", [|typeof<obj>|])
                convertToString.Invoke (typeConverter, [|source|]) :?> string
            | None ->
            
                if sourceType.Name = typedefof<_ list>.Name then
                    let items = objToObjList source
                    let itemsStrs =
                        List.map
                            (fun item -> toString item (sourceType.GetGenericArguments ()).[0])
                            items
                    let itemsStr = String.Join (AlgebraicReader.SpacedSeparatorStr, itemsStrs)
                    AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr
            
                elif sourceType.Name = typedefof<_ Set>.Name then
                    let items = objToComparableSet source
                    let itemsStrs =
                        Set.map
                            (fun item -> toString item (sourceType.GetGenericArguments ()).[0])
                            items
                    let itemsStr = String.Join (AlgebraicReader.SpacedSeparatorStr, itemsStrs)
                    AlgebraicReader.OpenComplexValueStr + itemsStr + AlgebraicReader.CloseComplexValueStr

                elif FSharpType.IsTuple sourceType then
                    let tupleFields = FSharpValue.GetTupleFields source
                    let tupleFieldStrs =
                        List.mapi
                            (fun i tupleField ->
                                let tupleFieldType = (FSharpType.GetTupleElements sourceType).[i]
                                toString tupleField tupleFieldType)
                            (List.ofArray tupleFields)
                    let tupleStr = String.Join (AlgebraicReader.SpacedSeparatorStr, tupleFieldStrs)
                    AlgebraicReader.OpenComplexValueStr + tupleStr + AlgebraicReader.CloseComplexValueStr
        
                elif FSharpType.IsRecord sourceType then
                    let recordFields = FSharpValue.GetRecordFields source
                    let recordFieldStrs =
                        List.mapi
                            (fun i recordField ->
                                let recordFieldType = (FSharpType.GetRecordFields sourceType).[i].PropertyType
                                toString recordField recordFieldType)
                            (List.ofArray recordFields)
                    let recordStr = String.Join (AlgebraicReader.SpacedSeparatorStr, recordFieldStrs)
                    AlgebraicReader.OpenComplexValueStr + recordStr + AlgebraicReader.CloseComplexValueStr

                elif FSharpType.IsUnion sourceType then
                    let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                    if not <| Array.isEmpty unionFields then
                        let unionFieldStrs =
                            List.mapi
                                (fun i unionField ->
                                    let unionFieldType = (unionCase.GetFields ()).[i].PropertyType
                                    toString unionField unionFieldType)
                                (List.ofArray unionFields)
                        let unionStrs = unionCase.Name :: unionFieldStrs
                        let unionStr = String.Join (AlgebraicReader.SpacedSeparatorStr, unionStrs)
                        AlgebraicReader.OpenComplexValueStr + unionStr + AlgebraicReader.CloseComplexValueStr
                    else unionCase.Name
        
                else (TypeDescriptor.GetConverter sourceType).ConvertToString source

        let rec fromReaderValue (destType : Type) (readerValue : obj) =

            match destType.TryGetCustomTypeConverter () with
            | Some typeConverter ->
                let typeConverterType = typeConverter.GetType ()
                match readerValue with
                | :? string ->
                    let convertFromString = typeConverterType.GetMethod ("ConvertFromString", [|typeof<string>|])
                    convertFromString.Invoke (typeConverter, [|readerValue|])
                | _ ->
                    let convertFrom = typeConverterType.GetMethod ("ConvertFrom", [|typeof<obj>|])
                    convertFrom.Invoke (typeConverter, [|readerValue|])
            | None ->

                if destType.Name = typedefof<_ list>.Name then
                    match readerValue with
                    | :? (obj list) as readerValueList ->
                        let elementType = (destType.GetGenericArguments ()).[0]
                        let list = List.map (fromReaderValue elementType) readerValueList
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpAssembly.GetType "Microsoft.FSharp.Collections.ListModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|list|])|])
                    | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

                elif destType.Name = typedefof<_ Set>.Name then
                    match readerValue with
                    | :? (obj list) as readerValueList ->
                        let elementType = (destType.GetGenericArguments ()).[0]
                        let list = List.map (fromReaderValue elementType) readerValueList
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpAssembly.GetType "Microsoft.FSharp.Collections.SetModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|list|])|])
                    | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."

                elif FSharpType.IsTuple destType then
                    let tupleReaderValues = readerValue :?> obj list
                    let tupleValues =
                        List.mapi
                            (fun i tupleReaderValue ->
                                let tupleElementType = (FSharpType.GetTupleElements destType).[i]
                                fromReaderValue tupleElementType tupleReaderValue)
                            tupleReaderValues
                    FSharpValue.MakeTuple (Array.ofList tupleValues, destType)
        
                elif FSharpType.IsRecord destType then
                    let recordReaderValues = readerValue :?> obj list
                    let recordValues =
                        List.mapi
                            (fun i recordReaderValue ->
                                let recordFieldType = (FSharpType.GetRecordFields destType).[i].PropertyType
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
                            let unionValues =
                                List.mapi
                                    (fun i unionReaderValue ->
                                        let unionCaseType = (unionCase.GetFields ()).[i].PropertyType
                                        fromReaderValue unionCaseType unionReaderValue)
                                    readerValueTail
                            FSharpValue.MakeUnion (unionCase, Array.ofList unionValues)
                        | _ -> failwith "Invalid AlgebraicConverter conversion from union reader value."
                    | :? string as unionName ->
                        let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                        FSharpValue.MakeUnion (unionCase, [||])
                    | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."
        
                else
                    let readerValueStr = readerValue :?> string
                    (TypeDescriptor.GetConverter destType).ConvertFromString readerValueStr

        let fromString (destType : Type) (source : string) =
            let readerValue = AlgebraicReader.stringToValue source
            fromReaderValue destType readerValue

        override this.CanConvertTo (_, _) =
            true
        
        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<string> then
                match source with
                | null ->
                    // here we infer that we're dealing with the zero case of a union
                    (FSharpType.GetUnionCases targetType).[0].Name :> obj
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
                if sourceType = targetType then source
                else
                    match source with
                    | :? string as sourceStr -> fromString targetType sourceStr
                    | _ -> failwith "Invalid AlgebraicConverter conversion from string."