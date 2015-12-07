// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open Prime

[<AutoOpen>]
module AlgebraicStringModule =

    /// Uses an algebraic converter to convert source to a string.
    let acstring (source : obj) =
        let converter = AlgebraicConverter (source.GetType ())
        converter.ConvertToString source

    /// Uses an algebraic converter to a string to a value.
    let acvalue<'a> (str : string) =
        let converter = AlgebraicConverter typeof<'a>
        converter.ConvertFromString str :?> 'a