// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime

/// Alternative type of option when its name is reified.
type 'a FSharpOption = 'a option

[<RequireQualifiedAccess>]
module Option =

    /// Flatten an option option.
    let inline flatten opt =
        match opt with
        | Some (Some s) -> Some s
        | Some None -> None
        | None -> None

    /// Get an option's value, or missing that, return a default value.
    let getOrDefault aDefault opt =
        match opt with
        | Some value -> value
        | None -> aDefault

    /// Map an option's value, or missing that, return a default value.
    let mapOrDefault mapper aDefault opt =
        match opt with
        | Some value -> mapper value
        | None -> aDefault