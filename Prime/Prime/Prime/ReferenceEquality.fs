// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpen>]
module ReferenceEquality
open System

/// Test for reference equality.
let inline (===) x y =
    Object.ReferenceEquals (x, y)

/// Test for reference inequality.
let inline (<<>>) x y =
    not (x === y)