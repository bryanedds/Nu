// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

[<RequireQualifiedAccess>]
module Set
open Prime

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