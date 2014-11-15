// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

[<RequireQualifiedAccess>]
module Option =

    /// Join an option option.
    let inline join opt =
        match opt with
        | Some (Some s) -> Some s
        | Some None -> None
        | None -> None

    /// Get an option's value, or missing that, return a default value.
    let getOrDefault aDefault opt =
        match opt with
        | Some value -> value
        | None -> aDefault

    /// Convert a nullable value into an option.
    let denull obj =
        match obj with
        | null -> None
        | obj -> Some obj