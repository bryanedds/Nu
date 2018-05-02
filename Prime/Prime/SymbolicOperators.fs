// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime
open System
open Prime

[<AutoOpen>]
module SymbolicOperators =

    /// Convert a value to a symbol.
    let symbolize<'a> (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        let converter = SymbolicConverter (true, None, ty)
        converter.ConvertTo (value, typeof<Symbol>) :?> Symbol

    /// Convert a symbol to a value.
    let valueize<'a> (symbol : Symbol) : 'a =
        let converter = SymbolicConverter (false, None, typeof<'a>)
        converter.ConvertFrom symbol :?> 'a

    /// Uses a symbolic converter to convert a value to a string.
    let scstring<'a> (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        let converter = SymbolicConverter (true, None, ty)
        converter.ConvertToString value

    /// Uses a symbolic converter to convert a string to a value.
    let scvalue<'a> (str : string) : 'a =
        let converter = SymbolicConverter (false, None, typeof<'a>)
        converter.ConvertFromString str :?> 'a

    /// Get the default value of type 'a taking into account DefaultValue decorations.
    let scdefaultof<'a> () : 'a =
        let defaultPropertyType = typeof<'a>
        let defaultValueAttributeOpt =
            defaultPropertyType.GetCustomAttributes (typeof<DefaultValueAttribute>, true) |>
            Array.map (fun attr -> attr :?> DefaultValueAttribute) |>
            Array.tryHead
        match defaultValueAttributeOpt with
        | Some defaultValueAttribute ->
            match defaultValueAttribute.DefaultValue with
            | :? 'a as defaultValue -> defaultValue
            | _ as defaultValue ->
                let defaultValueType = defaultValue.GetType ()
                let converter = SymbolicConverter (false, None, defaultValueType)
                if converter.CanConvertFrom defaultPropertyType
                then converter.ConvertFrom defaultValue :?> 'a
                else failwith ("Cannot convert '" + scstring defaultValue + "' to type '" + defaultPropertyType.Name + "'.")
        | None -> Unchecked.defaultof<'a>
