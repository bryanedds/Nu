// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

[<AutoOpen>]
module SymbolicStringModule =

    /// Uses an algebraic converter to convert a value to a string.
    let symstring (value : obj) =
        let converter = SymbolicConverter (value.GetType ())
        converter.ConvertToString value

    /// Uses an algebraic converter to convert a string to a value.
    let symvalue<'a> (str : string) =
        let converter = SymbolicConverter typeof<'a>
        converter.ConvertFromString str :?> 'a