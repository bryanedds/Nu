// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

[<AutoOpen>]
module AlgebraicStringModule =

    /// Uses an algebraic converter to convert a value to a string.
    let acstring (value : obj) =
        let converter = AlgebraicConverter (value.GetType ())
        converter.ConvertToString value

    /// Uses an algebraic converter to convert a string to a value.
    let acvalue<'a> (str : string) =
        let converter = AlgebraicConverter typeof<'a>
        converter.ConvertFromString str :?> 'a