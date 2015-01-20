// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System

[<AutoOpen>]
module ReferenceEquality =

    /// Test for reference equality.
    let inline (===) x y =
        Object.ReferenceEquals (x, y)

    /// Test for reference inequality.
    let inline (<<>>) x y =
        not (x === y)