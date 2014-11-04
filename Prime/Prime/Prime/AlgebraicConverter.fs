namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<AutoOpen>]
module AlgebraicConverterModule =

    type AlgebraicConverter (targetType : Type) =
        inherit TypeConverter ()

        let objToObjList source =
            let sourceType = source.GetType ()
            let headProperty = sourceType.GetProperty "Head"
            let tailProperty = sourceType.GetProperty "Tail"
            let isEmptyProperty = sourceType.GetProperty "IsEmpty"
            let mutable list = []
            let mutable tail = source
            while not (isEmptyProperty.GetValue tail :?> bool) do
                let head = headProperty.GetValue tail
                list <- head :: list
                tail <- tailProperty.GetValue tail
            list

        let rec toString source (sourceType : Type) =

            if sourceType.Name = typedefof<_ list>.Name then
                let items = objToObjList source
                let itemsStrs =
                    List.map
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
        
            if destType.Name = typedefof<_ list>.Name then
                match readerValue with
                | :? (obj list) as readerValueList ->
                    let list = List.map (fromReaderValue (destType.GetGenericArguments ()).[0]) readerValueList
                    list :> obj
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

[<RequireQualifiedAccess>]
module AlgebraicConverter =

    /// Initialize the type converters that we need out-of-the-box.
    /// NOTE: This is nearly impossible to make comprehensive due to -
    /// http://stackoverflow.com/questions/26694912/generically-apply-a-generic-typeconverter-to-an-existing-generic-type
    let initTypeConverters () =

        // list converters
        assignTypeConverter<bool list, AlgebraicConverter> ()
        assignTypeConverter<int list, AlgebraicConverter> ()
        assignTypeConverter<single list, AlgebraicConverter> ()
        assignTypeConverter<string list, AlgebraicConverter> ()
        
        // option converters
        assignTypeConverter<bool option, AlgebraicConverter> ()
        assignTypeConverter<int option, AlgebraicConverter> ()
        assignTypeConverter<single option, AlgebraicConverter> ()
        assignTypeConverter<string option, AlgebraicConverter> ()

[<AutoOpen>]
module AlgebraicDescriptor =
    
    /// Query that a value of the source type can be converted to the destination type.
    let CanConvertTo sourceType destType =
        (AlgebraicConverter sourceType).CanConvertTo destType
    
    /// Query that a value of the source type can be converted to a string.
    let CanConvertToString sourceType =
        (AlgebraicConverter sourceType).CanConvertTo typeof<string>

    /// Query that a value of the destination type can be converted from the source type.
    let CanConvertFrom sourceType destType =
        (AlgebraicConverter destType).CanConvertFrom sourceType

    /// Query that a value of the destination type can be converted from a string.
    let CanConvertFromString sourceType =
        (AlgebraicConverter sourceType).CanConvertFrom typeof<string>

    /// Convert a value to the given type using its assigned type converter.
    let ConvertTo (source : obj, destType) =
        (AlgebraicConverter (source.GetType ())).ConvertTo (source, destType)

    /// Convert a value from given type using its assigned type converter.
    let ConvertFrom source destType =
        (AlgebraicConverter destType).ConvertFrom source

    /// Convert a value to a string using its assigned type converter.
    let ConvertToString source =
        ConvertTo (source, typeof<string>) :?> string

    /// Convert a value from a string using its assigned type converter.
    let ConvertFromString (str : string) destType =
        ConvertFrom str destType

[<AutoOpen>]
module AlgebraicStringModule =

    /// Uses an algebraic converter to convert source to a string.
    let xstring (source : obj) =
        let converter = (AlgebraicConverter (source.GetType ()))
        converter.ConvertToString source