// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpen>]
module HashSet
open System.Collections.Generic

/// Create a hash set with a single item.
let singleton elem = List.toHashSet [elem]

type HashSet<'a> with
    /// Force the addition of an element, removing the existing one if necessary.
    member this.ForceAdd elem =
        let forced = this.Remove elem
        ignore <| this.Add elem
        forced