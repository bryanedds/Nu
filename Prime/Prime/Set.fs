// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace global
open System
open Prime

/// Alternative type of set when its name is reified.
type FSharpSet<'a when 'a : comparison> = Set<'a>

[<RequireQualifiedAccess>]
module Set =

    /// Check that a set is not empty.
    let rec notEmpty set =
        not ^ Set.isEmpty set

    /// Make a singleton set.
    let singleton value =
        Set.add value Set.empty

    /// Add multiple values to a set.
    let addMany values set =
        Seq.fold
            (fun set value -> Set.add value set)
            set
            values