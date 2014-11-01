namespace Nu
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<AutoOpen>]
module AlgebraicConverterModule =

    type AlgebraicConverter<'t> () =
        inherit TypeConverter ()

        let rec toString source =
            let sourceType = source.GetType ()
            if FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleFieldStrs = List.map toString <| List.ofArray tupleFields
                let tupleStr = String.Join (" | ", tupleFieldStrs)
                "{" + tupleStr + "}"
            elif FSharpType.IsRecord sourceType then
                let recordFields = FSharpValue.GetRecordFields source
                let recordFieldStrs = List.map toString <| List.ofArray recordFields
                let recordStr = String.Join (" | ", recordFieldStrs)
                "{" + recordStr + "}"
            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                if not <| Array.isEmpty unionFields then
                    let unionFieldStrs = List.map toString <| List.ofArray unionFields
                    let unionStrs = unionCase.Name :: unionFieldStrs
                    let unionStr = String.Join (" | ", unionStrs)
                    "{" + unionStr + "}"
                else unionCase.Name
            else
                let converter = TypeDescriptor.GetConverter sourceType
                converter.ConvertToString source // TODO: should we check for convertability?

        let rec fromReaderValue destType (readerValue : obj) =
            if FSharpType.IsTuple destType then
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
                let converter = TypeDescriptor.GetConverter destType
                let readerValueStr = readerValue :?> string
                converter.ConvertFromString readerValueStr // TODO: should we check for convertability?

        let fromString (source : string) =
            let readerValue = AlgebraicReader.stringToValue source
            fromReaderValue typeof<'t> readerValue

        override this.CanConvertTo (_, destType) =
            destType = typeof<string> ||
            destType = typeof<'t>
        
        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<string> then toString source :> obj
            elif destType = typeof<'t> then source
            else failwith "Invalid AlgebraicConverter conversion to source."

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<string> ||
            sourceType = typeof<'t>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? string ->
                match source with
                | :? string as sourceStr -> fromString sourceStr
                | _ -> failwith "Invalid AlgebraicConverter conversion from string."
            | :? 't -> source
            | _ -> failwith "Invalid AlgebraicConverter conversion from source."