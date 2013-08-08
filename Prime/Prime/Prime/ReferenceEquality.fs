// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpen>]
module ReferenceEquality
open System

/// Test for reference equality.
let (===) x y =
    Object.ReferenceEquals (x, y)

/// Test for reference inequality.
let (<<>>) x y =
    not (x === y)