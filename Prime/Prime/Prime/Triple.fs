// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<AutoOpen>]
module TripleModule =

    /// The first item in a triple.
    let inline a__ triple =
        match triple with
        | (a, _, _) -> a
    
    /// The second item in a triple.
    let inline _b_ triple =
        match triple with
        | (_, b, _) -> b
    
    /// The third item in a triple.
    let inline __c triple =
        match triple with
        | (_, _, c) -> c

    /// The first and second items in a triple.
    let inline ab_ triple =
        match triple with
        | (a, b, _) -> (a, b)
    
    /// The first and third items in a triple.
    let inline a_c triple =
        match triple with
        | (a, _, c) -> (a, c)
    
    /// The second and third items in a triple.
    let inline _bc triple =
        match triple with
        | (_, b, c) -> (b, c)

module Triple =

    /// Prepend a pair to build a triple.
    let inline prepend a (b, c) =
        (a, b, c)

    /// Insert in a pair to build a triple.
    let inline insert b (a, c) =
        (a, b, c)

    /// Append a pair to build a triple.
    let inline append c (a, b) =
        (a, b, c)

    /// Make a triple.
    let make a b c =
        (a, b, c)