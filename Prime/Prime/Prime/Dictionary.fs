// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

[<AutoOpen>]
module Dictionary
open System.Collections.Generic

/// Make a dictionary with a single element.
let singleton elem =
    List.toDictionary [elem] // TODO: change implementation to accommodate 2 separate params (1 for key, 1 for value)

/// Map over a dictionary. A new dictionary is produced.
let map (mapper : KeyValuePair<'k, 'v> -> 'v) (dictionary : Dictionary<'k, 'v>) =
    let dictionary' = Dictionary<'k, 'v> ()
    for kvp in dictionary do dictionary'.Add (kvp.Key, mapper kvp)
    dictionary'

/// Try to find a value in a dictonary.
let inline tryFind key (dictionary : Dictionary<'k, 'v>) =
    let valueRef = ref Unchecked.defaultof<'v>
    if dictionary.TryGetValue (key, valueRef)
    then Some valueRef.Value
    else None

/// Like dict, but returns a concrete Dictionary instance.
let dictC kvps =
    let dictionary = Dictionary ()
    for (key, value) in kvps do dictionary.Add (key, value)
    dictionary

/// Dictionary extension methods.
type Dictionary<'k, 'v> with

    /// Force the addition of an element, removing the existing one if necessary.
    member this.ForceAdd (key, value) =
        let forced = this.Remove key
        this.Add (key, value)
        forced

    /// Check value equality of dictionary.
    /// NOTE: be wary the highly imperative nature of this code.
    member this.ValueEquals (that : Dictionary<'k, 'v>) =
        let mutable enr = this.GetEnumerator ()
        let mutable enr2 = that.GetEnumerator ()
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