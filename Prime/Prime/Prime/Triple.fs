// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Triple

/// The first item in a triple.
let fst triple =
    match triple with
    | (x, _, _) -> x
    
/// The second item in a triple.
let snd triple =
    match triple with
    | (_, x, _) -> x
    
/// The third item in a triple.
let thd triple =
    match triple with
    | (_, _, x) -> x