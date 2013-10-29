// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpen>]
module Triple

/// The first item in a triple.
let a__ triple =
    match triple with
    | (a, _, _) -> a
    
/// The second item in a triple.
let _b_ triple =
    match triple with
    | (_, b, _) -> b
    
/// The third item in a triple.
let __c triple =
    match triple with
    | (_, _, c) -> c

/// The first and second items in a triple.
let ab_ triple =
    match triple with
    | (a, b, _) -> (a, b)
    
/// The first and third items in a triple.
let a_c triple =
    match triple with
    | (a, _, c) -> (a, c)
    
/// The second and third items in a triple.
let _bc triple =
    match triple with
    | (_, b, c) -> (b, c)