// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

/// Expands a record so that its property are named.
type SymbolicExpansionAttribute () =
    inherit Attribute ()

/// Compresses two unions into a single union in a symbolic-expression.
type SymbolicCompression<'a, 'b> =
    | SymbolicCompressionA of 'a
    | SymbolicCompressionB of 'b

type SymbolicConverter (printing : bool, pointType : Type) =
    inherit TypeConverter ()

    let padWithDefaults' (fieldTypes : Type array) (values : obj array) =
        if values.Length < fieldTypes.Length then
            let valuesPadded =
                fieldTypes |>
                Array.skip values.Length |>
                Array.map (fun info -> info.GetDefaultValue ()) |>
                Array.append values
            valuesPadded
        else values

    let padWithDefaults (fieldInfos : PropertyInfo array) (values : obj array) =
        padWithDefaults' (Array.map (fun (info : PropertyInfo) -> info.PropertyType) fieldInfos) values

    let rec toSymbol (sourceType : Type) (source : obj) =
        match sourceType.TryGetCustomTypeConverter () with
        | Some typeConverter ->

            // symbolize user-defined type
            if not (typeConverter.CanConvertTo typeof<Symbol>)
            then failconv ("Cannot convert type '" + getTypeName source + "' to Prime.Symbol.") None
            else typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol

        | None ->

            // symbolize .NET primitive
            if sourceType.IsPrimitive then
                let converted = (TypeDescriptor.GetConverter sourceType).ConvertTo (source, typeof<string>) :?> string
                if sourceType = typeof<bool> then Atom (converted, None)
                elif sourceType = typeof<char> then String (converted, None)
                else Number (converted, None)

            // symbolize string
            elif sourceType = typeof<string> then
                if printing then
                    let sourceStr = string source
                    if pointType = typeof<string> then String (sourceStr, None)
                    elif Symbol.isNumber sourceStr then Number (sourceStr, None)
                    else Atom (sourceStr, None)
                else
                    let sourceStr = string source
                    if Symbol.shouldBeExplicit sourceStr then String (sourceStr, None)
                    elif Symbol.isNumber sourceStr then Number (sourceStr, None)
                    else Atom (sourceStr, None)

            // symbolize Symbol (no transformation)
            elif sourceType = typeof<Symbol> then
                source :?> Symbol

            // symbolize KeyValuePair
            elif sourceType.Name = typedefof<KeyValuePair<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let kvp = Reflection.objToKeyValuePair source
                let keySymbol = toSymbol gargs.[0] kvp.Key
                let valueSymbol = toSymbol gargs.[1] kvp.Value
                Symbols ([keySymbol; valueSymbol], None)

            // symbolize array
            elif sourceType.IsArray then
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol (sourceType.GetElementType ())) items
                Symbols (symbols, None)

            // symbolize unit
            elif sourceType.Name = typeof<unit>.Name then
                Symbols ([], None)

            // symbolize list
            elif sourceType.Name = typedefof<_ list>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, None)

            // symbolize Set
            elif sourceType.Name = typedefof<_ Set>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = Reflection.objToComparableSet source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, None)

            // symbolize Map
            elif sourceType.Name = typedefof<Map<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = Reflection.objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Symbols (symbols, None)

            // symbolize SymbolicCompression
            elif sourceType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let value = unionFields.[0]
                let valueType = value.GetType ()
                if unionCase.Tag = 0 then toSymbol valueType value
                else
                    let (_, unionFields) = FSharpValue.GetUnionFields (value, valueType)
                    let value = unionFields.[0]
                    let valueType = value.GetType ()
                    toSymbol valueType value

            // symbolize Tuple
            elif FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleElementTypes = FSharpType.GetTupleElements sourceType
                let tupleFieldSymbols = Array.mapi (fun i tupleField -> toSymbol tupleElementTypes.[i] tupleField) tupleFields
                Symbols (List.ofArray tupleFieldSymbols, None)

            // symbolize Record
            elif FSharpType.IsRecord sourceType then
                if sourceType.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                    let recordFieldInfos = FSharpType.GetRecordFields sourceType
                    let recordFields = Array.map (fun info -> (info, FSharpValue.GetRecordField (source, info))) recordFieldInfos
                    let recordFieldSymbols =
                        recordFields |>
                        Array.map (fun (info, field) -> Symbols ([Atom (info.Name, None); toSymbol info.PropertyType field], None))
                    Symbols (List.ofArray recordFieldSymbols, None)
                else
                    let recordFields = FSharpValue.GetRecordFields source
                    let recordFieldTypes = FSharpType.GetRecordFields sourceType
                    let recordFieldSymbols = Array.mapi (fun i recordField -> toSymbol recordFieldTypes.[i].PropertyType recordField) recordFields
                    Symbols (List.ofArray recordFieldSymbols, None)

            // symbolize Union
            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let unionFieldInfos = unionCase.GetFields ()
                if not (Array.isEmpty unionFields) then
                    let unionFieldSymbols = Array.mapi (fun i unionField -> toSymbol unionFieldInfos.[i].PropertyType unionField) unionFields
                    let unionSymbols = Array.cons (Atom (unionCase.Name, None)) unionFieldSymbols
                    Symbols (List.ofArray unionSymbols, None)
                else Atom (unionCase.Name, None)

            // symbolize vanilla .NET type
            else
                let typeConverter = TypeDescriptor.GetConverter sourceType
                match typeConverter with
                | :? DateTimeConverter ->
                    // HACK: we do not want to use this converter here as it strips the time when converting to string!
                    let dateTimeStr = string source
                    String (dateTimeStr, None)
                | _ ->
                    if typeConverter.CanConvertTo typeof<Symbol>
                    then typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
                    else (typeConverter.ConvertTo (source, typeof<string>) :?> string, None) |> Atom

    let toString (sourceType : Type) (source : obj) =
        let symbol = toSymbol sourceType source
        Symbol.toString symbol

    let rec fromSymbol (destType : Type) (symbol : Symbol) =

        // desymbolize .NET primitive
        if destType.IsPrimitive then
            match symbol with
            | Atom (str, _) | Number (str, _) | String (str, _) ->
                (TypeDescriptor.GetConverter destType).ConvertFromString str
            | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol, Number, or String for conversion to .NET primitive." (Some symbol)

        // desymbolize string
        elif destType = typeof<string> then
            match symbol with
            | Atom (str, _) ->
                if Symbol.isExplicit str
                then str.Substring (1, str.Length - 2) :> obj
                else str :> obj
            | Number (str, _) ->
                str :> obj
            | String (str, _) ->
                if printing
                then Symbol.OpenStringStr + Symbol.distillate str + Symbol.CloseStringStr :> obj
                else str :> obj
            | Quote (_, _) | Symbols (_, _) ->
                failconv "Expected Symbol, Number, or String for conversion to string." (Some symbol)

        // desymbolize Symbol (no tranformation)
        elif destType = typeof<Symbol> then
            symbol :> obj

        else
            match destType.TryGetCustomTypeConverter () with
            | Some typeConverter ->

                // desymbolize user-defined type
                if typeConverter.CanConvertFrom typeof<Symbol>
                then typeConverter.ConvertFrom symbol
                else failconv ("Expected ability to convert from Symbol for custom type converter '" + getTypeName typeConverter + "'.") (Some symbol)

            | None ->

                // desymbolize array
                if destType.IsArray then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let elements = List.map (fromSymbol (destType.GetElementType ())) symbols
                        Reflection.objsToArray destType elements
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to array." (Some symbol)

                // desymbolize unit
                elif destType.Name = typeof<unit>.Name then
                    match symbol with
                    | Symbols ([], _) -> () :> obj
                    | _ -> failconv "Expected empty Symbols for conversion to unit." (Some symbol)

                // desymbolize list
                elif destType.Name = typedefof<_ list>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        Reflection.objsToList destType elements
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to list." (Some symbol)

                // desymbolize Set
                elif destType.Name = typedefof<_ Set>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        Reflection.objsToSet destType elements
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Set." (Some symbol)

                // desymbolize Map
                elif destType.Name = typedefof<Map<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairs = List.map (fromSymbol pairType) symbols
                            Reflection.pairsToMap destType pairs
                        | _ -> failwithumf ()
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Map." (Some symbol)

                // desymbolize SymbolicCompression
                elif destType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: _ ->
                            let gargs = destType.GetGenericArguments ()
                            let aType = gargs.[0]
                            let aCases = FSharpType.GetUnionCases aType
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = symbolHead) aCases with
                            | Some aCase ->
                                let a = fromSymbol aCase.DeclaringType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[0]
                                FSharpValue.MakeUnion (compressionUnion, [|a|])
                            | None ->
                                let bType = gargs.[1]
                                let b = fromSymbol bType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[1]
                                FSharpValue.MakeUnion (compressionUnion, [|b|])
                        | _ -> failconv "Expected Atom value for SymbolicCompression union name." (Some symbol)
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to SymbolicCompression." (Some symbol)

                // desymbolize Tuple
                elif FSharpType.IsTuple destType then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let elementTypes = FSharpType.GetTupleElements destType
                        let elements = symbols |> Array.ofList |> Array.mapi (fun i elementSymbol -> fromSymbol elementTypes.[i] elementSymbol)
                        let elements = padWithDefaults' elementTypes elements
                        FSharpValue.MakeTuple (elements, destType)
                    | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Symbols for conversion to Tuple." (Some symbol)

                // desymbolize Record
                elif FSharpType.IsRecord destType then
                        match symbol with
                        | Symbols (symbols, _) ->
                            if destType.IsDefined (typeof<SymbolicExpansionAttribute>, true) then
                                let fieldInfos = FSharpType.GetRecordFields destType
                                if List.forall (function Symbols ([Atom _; _], _) -> true | _ -> false) symbols then
                                    let fieldMap =
                                        symbols |>
                                        List.map (function Symbols ([Atom (fieldName, _); fieldSymbol], _) -> (fieldName, fieldSymbol) | _ -> failwithumf ()) |>
                                        Map.ofList
                                    let fields =
                                        Array.map
                                            (fun (info : PropertyInfo) ->
                                                match Map.tryFind info.Name fieldMap with
                                                | Some fieldSymbol -> fromSymbol info.PropertyType fieldSymbol
                                                | None -> info.PropertyType.GetDefaultValue ())
                                            fieldInfos
                                    FSharpValue.MakeRecord (destType, fields)
                                else failconv "Expected Symbols in pairs for expanded Record" (Some symbol)
                            else
                                let fieldInfos = FSharpType.GetRecordFields destType
                                let fields = symbols |> Array.ofList |> Array.mapi (fun i fieldSymbol -> fromSymbol fieldInfos.[i].PropertyType fieldSymbol)
                                let fields = padWithDefaults fieldInfos fields
                                FSharpValue.MakeRecord (destType, fields)
                        | Atom (_, _) | Number (_, _) | String (_, _) | Quote (_, _) ->
                            failconv "Expected Symbols for conversion to unexpanded Record." (Some symbol)

                // desymbolize Union
                elif FSharpType.IsUnion destType && destType <> typeof<string list> then
                    let unionCases = FSharpType.GetUnionCases destType
                    match symbol with
                    | Atom (unionName, _) ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                        | Some unionCase -> FSharpValue.MakeUnion (unionCase, [||])
                        | None ->
                            let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> String.concat " | "
                            failconv ("Expected one of the following Atom values for Union name: '" + unionNames + "'.") (Some symbol)
                    | Symbols (symbols, _) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: symbolTail ->
                            let unionName = symbolHead
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                            | Some unionCase ->
                                let unionFieldInfos = unionCase.GetFields ()
                                let unionValues = symbolTail |> Array.ofList |> Array.mapi (fun i unionSymbol -> fromSymbol unionFieldInfos.[i].PropertyType unionSymbol)
                                let unionValues = padWithDefaults unionFieldInfos unionValues
                                FSharpValue.MakeUnion (unionCase, unionValues)
                            | None ->
                                let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> String.concat " | "
                                failconv ("Expected one of the following Atom values for Union name: '" + unionNames + "'.") (Some symbol)
                        | (Number (_, _) | String (_, _) | Quote (_, _) | Symbols (_, _)) :: _ ->
                            failconv "Expected Atom value for Union name." (Some symbol)
                        | [] ->
                            failconv "Expected Atom value for Union name." (Some symbol)
                    | Number (_, _) | String (_, _) | Quote (_, _) ->
                        failconv "Expected Atom or Symbols value for conversion to Union." (Some symbol)

                // desymbolize vanilla .NET type
                else
                    match symbol with
                    | Atom (str, _) | Number (str, _) | String (str, _) ->
                        (TypeDescriptor.GetConverter destType).ConvertFromString str
                    | Quote (_, _) | Symbols (_, _) ->
                        failconv ("Expected Atom, Number, or String value for conversion to vanilla .NET type '" + destType.Name + "'.") (Some symbol)

    let fromString (destType : Type) (source : string) =
        let symbol = Symbol.fromString source
        fromSymbol destType symbol

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            match source with
            | null ->
                if FSharpType.IsUnion pointType
                then (FSharpType.GetUnionCases pointType).[0].Name :> obj
                // here we are totally fucked because PropertyGrid passes typeof<obj> to the converter's ctor and we
                // have no information about what the fuck to do...
                else source
            | _ -> toString pointType source :> obj
        elif destType = typeof<Symbol> then toSymbol pointType source :> obj
        elif destType = pointType then source
        else failconv "Invalid SymbolicConverter conversion to source." None

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | null -> source
        | _ ->
            let sourceType = source.GetType ()
            if sourceType <> pointType then
                match source with
                | :? string as sourceStr -> fromString pointType sourceStr
                | :? Symbol as sourceSymbol -> fromSymbol pointType sourceSymbol
                | _ -> failconv "Invalid SymbolicConverter conversion from string." None
            else source

    new (pointType : Type) = SymbolicConverter (false, pointType)