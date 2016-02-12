// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

/// Compresses two unions into a single union in a symbolic-expression.
type SymbolicCompression<'a, 'b> =
    | SymbolicCompressionA of 'a
    | SymbolicCompressionB of 'b

type SymbolicConverter (targetType : Type) =
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

    let rec toSymbol (sourceType : Type) (source : obj) =
        match sourceType.TryGetCustomTypeConverter () with
        | Some typeConverter ->
            if not ^ typeConverter.CanConvertTo typeof<Symbol>
            then failwith ^ "Cannot convert type '" + getTypeName source + "' to Prime.Symbol."
            else typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
        | None ->
            if sourceType.IsPrimitive then
                (TypeDescriptor.GetConverter sourceType).ConvertToString source |> Atom
            elif sourceType = typeof<string> then
                source :?> string |> Atom
            elif sourceType = typeof<Symbol> then
                source :?> Symbol
            elif sourceType.Name = typedefof<KeyValuePair<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let kvp = objToKeyValuePair source
                let keySymbol = toSymbol gargs.[0] kvp.Key
                let valueSymbol = toSymbol gargs.[1] kvp.Value
                Molecule [keySymbol; valueSymbol]
            elif sourceType.Name = typedefof<_ list>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToObjList source
                let symbols = List.map (toSymbol gargs.[0]) items
                Molecule symbols
            elif sourceType.Name = typedefof<_ Set>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToComparableSet source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Molecule symbols
            elif sourceType.Name = typedefof<Map<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Molecule symbols
            elif sourceType.Name = typedefof<Vmap<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<Tuple<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Molecule symbols
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
            elif FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleElementTypes = FSharpType.GetTupleElements sourceType
                let tupleFieldSymbols = List.mapi (fun i tupleField -> toSymbol tupleElementTypes.[i] tupleField) (List.ofArray tupleFields)
                Molecule tupleFieldSymbols
            elif FSharpType.IsRecord sourceType then
                let recordFields = FSharpValue.GetRecordFields source
                let recordFieldTypes = FSharpType.GetRecordFields sourceType
                let recordFieldSymbols = List.mapi (fun i recordField -> toSymbol recordFieldTypes.[i].PropertyType recordField) (List.ofArray recordFields)
                Molecule recordFieldSymbols
            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let unionFieldTypes = unionCase.GetFields ()
                if not ^ Array.isEmpty unionFields then
                    let unionFieldSymbols = List.mapi (fun i unionField -> toSymbol unionFieldTypes.[i].PropertyType unionField) (List.ofArray unionFields)
                    let unionSymbols = Atom unionCase.Name :: unionFieldSymbols
                    Molecule unionSymbols
                else Atom unionCase.Name
            else
                let typeConverter = TypeDescriptor.GetConverter sourceType
                if typeConverter.CanConvertTo typeof<Symbol>
                then typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
                else typeConverter.ConvertToString source |> Atom

    let toString (sourceType : Type) (source : obj) =
        let symbol = toSymbol sourceType source
        Symbol.toString symbol

    let rec fromSymbol (destType : Type) (symbol : Symbol) =
        if symbol = Molecule [] then
            // kind of a weird special-case when .NET type converters throw null at us for no
            // seeming reason
            null
        elif destType.IsPrimitive then
            match symbol with
            | Atom str -> (TypeDescriptor.GetConverter destType).ConvertFromString str
            | Quote _ -> failwith "Expected Symbol.Atom for conversion to string."
            | Molecule _ -> failwith "Expected Symbol.Atom for conversion to string."
        elif destType = typeof<string> then
            match symbol with
            | Atom str -> str :> obj
            | Quote _ -> failwith "Expected Symbol.Atom for conversion to string."
            | Molecule _ -> failwith "Expected Symbol.Atom for conversion to string."
        elif destType = typeof<Symbol> then
            // nothing to do when we want a symbol and already have it
            symbol :> obj
        else
            match destType.TryGetCustomTypeConverter () with
            | Some typeConverter ->
                if typeConverter.CanConvertFrom typeof<Symbol>
                then typeConverter.ConvertFrom symbol
                else failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
            | None ->
                if destType.Name = typedefof<_ list>.Name then
                    match symbol with
                    | Molecule symbols ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.ListModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|elements|])|])
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif destType.Name = typedefof<_ Set>.Name then
                    match symbol with
                    | Molecule symbols ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.SetModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|elements|])|])
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif destType.Name = typedefof<Map<_, _>>.Name then
                    match symbol with
                    | Molecule symbols ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairList = List.map (fromSymbol pairType) symbols
                            let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                            let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.MapModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                            ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                        | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif destType.Name = typedefof<Vmap<_, _>>.Name then
                    match symbol with
                    | Molecule symbols ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairList = List.map (fromSymbol pairType) symbols
                            let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                            let ofSeq = ((typedefof<Vmap<_, _>>.Assembly.GetType "Prime.VmapModule").GetMethod ("ofSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                            ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                        | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif destType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                    match symbol with
                    | Molecule symbols ->
                        match symbols with
                        | (Atom symbolHead) :: _ ->
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
                        | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif FSharpType.IsTuple destType then
                    match symbol with
                    | Molecule symbols ->
                        let elementTypes = FSharpType.GetTupleElements destType
                        let elements = List.mapi (fun i elementSymbol -> fromSymbol elementTypes.[i] elementSymbol) symbols
                        FSharpValue.MakeTuple (Array.ofList elements, destType)
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif FSharpType.IsRecord destType then
                    match symbol with
                    | Molecule symbols ->
                        let fieldTypes = FSharpType.GetRecordFields destType
                        let fields = List.mapi (fun i fieldSymbol -> fromSymbol fieldTypes.[i].PropertyType fieldSymbol) symbols
                        FSharpValue.MakeRecord (destType, Array.ofList fields)
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                elif FSharpType.IsUnion destType && destType <> typeof<string list> then
                    let unionCases = FSharpType.GetUnionCases destType
                    match symbol with
                    | Atom unionName ->
                        let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                        FSharpValue.MakeUnion (unionCase, [||])
                    | Molecule symbols ->
                        match symbols with
                        | (Atom symbolHead) :: symbolTail ->
                            let unionName = symbolHead
                            let unionCase = Array.find (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases
                            let unionFieldTypes = unionCase.GetFields ()
                            let unionValues = List.mapi (fun i unionSymbol -> fromSymbol unionFieldTypes.[i].PropertyType unionSymbol) symbolTail
                            FSharpValue.MakeUnion (unionCase, Array.ofList unionValues)
                        | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"
                else
                    match symbol with
                    | Atom str -> (TypeDescriptor.GetConverter destType).ConvertFromString str
                    | _ -> failwith "Unexpected match failure in Nu.SymbolicConverter.fromSymbol. TODO: better error message!"

    let fromString (destType : Type) (source : string) =
        let symbol = Symbol.fromString source
        fromSymbol destType symbol

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
                toString sourceType source :> obj
        elif destType = typeof<Symbol> then
            match source with
            | null -> Molecule [] :> obj
            | _ -> toSymbol destType source :> obj
        else
            let sourceType = source.GetType ()
            if destType = sourceType then source
            else failwith "Invalid SymbolicConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = targetType

    override this.ConvertFrom (_, _, source) =
        match source with
        | null -> source
        | _ ->
            let sourceType = source.GetType ()
            if sourceType <> targetType then
                match source with
                | :? string as sourceStr -> fromString targetType sourceStr
                | :? Symbol as sourceSymbol -> fromSymbol targetType sourceSymbol
                | _ -> failwith "Invalid SymbolicConverter conversion from string."
            else source