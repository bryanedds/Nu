// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<AutoOpen>]
module Pair =

    /// The first item in a pair.
    let inline a_ pair =
        match pair with
        | (a, _) -> a
    
    /// The second item in a pair.
    let inline _b pair =
        match pair with
        | (_, b) -> b
    
    /// Make a pair.
    let make a b =
        (a, b)