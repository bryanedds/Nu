// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

[<AutoOpen>]
module HashSet
open System.Collections.Generic

/// Make a hash set with a single element.
let singleton item =
    List.toHashSet [item]

type HashSet<'a> with
    /// Force the addition of an element, removing the existing one if necessary.
    member this.ForceAdd item =
        let forced = this.Remove item
        ignore <| this.Add item
        forced