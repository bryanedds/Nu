namespace Nu
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

[<AutoOpen>]
module ConverterModule =

    type FSharpTypeConverter<'t> () =
        inherit TypeConverter ()

        let rec toString source =
            let sourceType = source.GetType ()
            if FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleFieldStrs = List.map toString <| List.ofArray tupleFields
                let tupleStr = String.Join ("; ", tupleFieldStrs)
                "[" + tupleStr + "]"
            elif FSharpType.IsRecord sourceType then
                let recordFields = FSharpValue.GetRecordFields source
                let recordFieldStrs = List.map toString <| List.ofArray recordFields
                let recordStr = String.Join ("; ", recordFieldStrs)
                "[" + recordStr + "]"
            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                if not <| Array.isEmpty unionFields then
                    let unionFieldStrs = List.map toString <| List.ofArray unionFields
                    let unionStrs = unionCase.Name :: unionFieldStrs
                    let unionStr = String.Join ("; ", unionStrs)
                    "{" + unionStr + "}"
                else unionCase.Name
            else failwith "Invalid FSharpTypeConverter conversion to string."

        let rec fromString (_ : string) =
            (*let destType = typeof<'t>
            if FSharpType.IsTuple destType then
                let tupleStr = source.Trim ()
                let tupleStr = tupleStr.Substring (1, tupleStr.Length - 1)
                let tupleStrs = 
                let tupleFieldStrs = source.Split
                FSharpValue.MakeTuple (Array.map fromString tupleFieldStrs, destType)
            else*) failwith "Invalid FSharpTypeConverter conversion from string."

        override this.CanConvertTo (_, destType) =
            destType = typeof<string>
        
        override this.ConvertTo (_, _, source, _) =
            toString source :> obj

        override this.CanConvertFrom (_, sourceType) =
            FSharpType.IsTuple sourceType ||
            FSharpType.IsRecord sourceType ||
            FSharpType.IsUnion sourceType
        
        override this.ConvertFrom (_, _, source) =
            if source.GetType () <> typeof<'t> then
                match source with
                | :? string as sourceStr -> fromString sourceStr
                | _ -> failwith "Invalid FSharpTypeConverter conversion from source."
            else source