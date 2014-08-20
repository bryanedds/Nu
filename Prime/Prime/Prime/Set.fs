// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

[<RequireQualifiedAccess>]
module Set

/// Make a singleton map.
let singleton value =
    Set.add value Set.empty

/// Add multiple values to a map.
let addMany values set =
    Seq.fold
        (fun set value -> Set.add value set)
        set
        values