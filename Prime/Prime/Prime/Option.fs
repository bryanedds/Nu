// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Option

/// Concatenate an option option.
let inline concat opt =
    match opt with
    | None -> None
    | Some None -> None
    | Some (Some s) -> Some s