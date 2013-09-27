// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<AutoOpen>]
module Dictionary
open System.Collections.Generic

/// Create a dictionary with a single item.
let singleton elem = List.toDictionary [elem]

/// Map over a dictionary. A new dictionary is produced.
let map (mapper : KeyValuePair<'k, 'v> -> 'v) (dictionary : Dictionary<'k, 'v>) =
    let dictionary2 = Dictionary<'k, 'v> ()
    for kvp in dictionary do dictionary2.Add (kvp.Key, mapper kvp)
    dictionary2

/// Dictionary extension methods.
type Dictionary<'k, 'v> with

    /// Force the addition of an element, removing the existing one if necessary.
    member this.ForceAdd (key, value) =
        let forced = this.Remove key
        this.Add (key, value)
        forced

    /// Check value equality of dictionary.
    /// NOTE: be wary the highly imperative nature of this code.
    member this.ValueEquals (other : Dictionary<'k, 'v>) =
        let mutable enr = this.GetEnumerator ()
        let mutable enr2 = other.GetEnumerator ()
        let mutable moving = true
        let mutable equal = true
        while moving && equal do
            if enr.MoveNext () then
                if enr2.MoveNext () then equal <- enr.Current = enr2.Current
                else equal <- false
            else
                if enr2.MoveNext () then equal <- false
                else moving <- false
        equal