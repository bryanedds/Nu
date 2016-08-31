// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

[<AutoOpen>]
module SymbolicOperators =

    /// Convert a value to a symbol.
    let symbolize<'a> (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        let converter = SymbolicConverter ty
        converter.ConvertTo (value, typeof<Symbol>) :?> Symbol

    /// Convert a symbol to a value.
    let valueize<'a> (symbol : Symbol) :'a =
        let converter = SymbolicConverter typeof<'a>
        converter.ConvertFrom symbol :?> 'a

    /// Uses a symbolic converter to convert a value to a string.
    let scstring<'a> (value : 'a) =
        let ty = if isNull (value :> obj) then typeof<'a> else getType value
        let converter = SymbolicConverter ty
        converter.ConvertToString value

    /// Uses a symbolic converter to convert a string to a value.
    let scvalue<'a> (str : string) : 'a =
        let converter = SymbolicConverter typeof<'a>
        converter.ConvertFromString str :?> 'a