// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Option

/// Concatenate an option option.
let concat opt =
    match opt with
    | None -> None
    | Some None -> None
    | Some (Some s) -> Some s

/// Apply a function to a value if possible, creating a new Option with the result.
let reduce fn opt =
    match opt with
    | None -> None
    | Some value -> fn value