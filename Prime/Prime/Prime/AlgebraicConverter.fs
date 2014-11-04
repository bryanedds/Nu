namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<AutoOpen>]
module AlgebraicConverterModule =

    type [<StructuralEquality; NoComparison>] AlgebraicConversionCircumventor =
        { AccValue : string }

    type AlgebraicConverter () =
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
        
            else TypeDescriptor.ConvertToString source

        static member fromReaderValue (destType : Type) (readerValue : obj) =
        
            if destType.Name = typedefof<_ list>.Name then
                match readerValue with
                | :? (obj list) as readerValueList ->
                    let list = List.map (AlgebraicConverter.fromReaderValue (destType.GetGenericArguments ()).[0]) readerValueList
                    list :> obj
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."
        
            elif FSharpType.IsTuple destType then
                let tupleReaderValues = readerValue :?> obj list
                let tupleValues =
                    List.mapi
                        (fun i tupleReaderValue ->
                            let tupleElementType = (FSharpType.GetTupleElements destType).[i]
                            AlgebraicConverter.fromReaderValue tupleElementType tupleReaderValue)
                        tupleReaderValues
                FSharpValue.MakeTuple (Array.ofList tupleValues, destType)
        
            elif FSharpType.IsRecord destType then
                let recordReaderValues = readerValue :?> obj list
                let recordValues =
                    List.mapi
                        (fun i recordReaderValue ->
                            let recordFieldType = (FSharpType.GetRecordFields destType).[i].PropertyType
                            AlgebraicConverter.fromReaderValue recordFieldType recordReaderValue)
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
                                    AlgebraicConverter.fromReaderValue unionCaseType unionReaderValue)
                                readerValueTail
                        FSharpValue.MakeUnion (unionCase, Array.ofList unionValues)
                    | _ -> failwith "Invalid AlgebraicConverter conversion from union reader value."
                | :? string as unionName ->
                    let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                    FSharpValue.MakeUnion (unionCase, [||])
                | _ -> failwith "Unexpected match failure in Nu.AlgebraicConverter.fromReadValue."
        
            else
                let readerValueStr = readerValue :?> string
                TypeDescriptor.ConvertFromString readerValueStr destType

        static member fromString (destType : Type) (source : string) =
            let readerValue = AlgebraicReader.stringToValue source
            AlgebraicConverter.fromReaderValue destType readerValue

        static member canConvertFromString (_ : Type) =
            true

        static member canConvertFrom (sourceType : Type) (destType : Type) =
            sourceType = typeof<string> ||
            sourceType = destType

        static member convertFrom (source : obj) (sourceType : Type) (destType : Type) =
            if sourceType <> destType then
                match source with
                | :? string as sourceStr -> AlgebraicConverter.fromString destType sourceStr
                | _ -> failwith "Invalid AlgebraicConverter conversion from string."
            else source

        static member convertFromString source destType =
            AlgebraicConverter.convertFrom source typeof<string> destType

        override this.CanConvertTo (_, _) =
            true
        
        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<string> then
                match source with
                | null ->
                    // this case represents a particularly penercious turn of events; here we have
                    // no actual type data in this context, yet the source object may also be null.
                    // In F#, this is actually an untenable situation since we can have an
                    // unbounded number of valid values represented by a null thanks to the
                    // 'CompilationRepresentation (CompilationRepresentationFlags.UseNullAsTrueValue)'
                    // attribute.
                    //
                    // Unfortunately, there is no real way to solve this other than to assume here
                    // that null is to be represented as None.
                    "None" :> obj
                | _ ->
                    let sourceType = source.GetType ()
                    toString source sourceType :> obj
            else
                let sourceType = source.GetType ()
                if destType = sourceType then source
                else failwith "Invalid AlgebraicConverter conversion to source."

        override this.CanConvertFrom (_, _) =
            true
        
        override this.ConvertFrom (_, _, source) =
            { AccValue = source :?> string } :> obj

module AlgebraicConverter =

    /// Initialize the type converters that we need out-of-the-box.
    /// Unfortunately, this is very hard to make comprehensive -
    /// http://stackoverflow.com/questions/26694912/generically-apply-a-generic-typeconverter-to-an-existing-generic-type/26701678?noredirect=1#comment42014989_26701678
    let initTypeConverters () =
        assignTypeConverter<string option, AlgebraicConverter> ()
        assignTypeConverter<string list, AlgebraicConverter> ()