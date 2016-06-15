// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module Dictionary =

    /// Make a dictionary with a single element.
    let singleton key value =
        List.toDict [(key, value)]

    /// Map over a dictionary. A new dictionary is produced.
    let map (mapper : KeyValuePair<'k, 'v> -> 'v) (dictionary : Dictionary<'k, 'v>) =
        let result = Dictionary<'k, 'v> dictionary.Comparer
        for kvp in dictionary do result.Add (kvp.Key, mapper kvp)
        result

    /// Try to find a value in a dictonary.
    let inline tryFind key (dictionary : Dictionary<'k, 'v>) =
        match dictionary.TryGetValue key with
        | (true, value) -> Some value
        | (false, _) -> None

[<AutoOpen>]
module DictionaryExtension =

    /// Dictionary extension methods.
    type Dictionary<'k, 'v> with

        /// Force the addition of an element, replacing the existing one if necessary.
        member this.ForceAdd (key, value) =
            this.[key] <- value

        /// Try to add an element, returning false upon failure.
        member this.TryAdd (key, value) =
            if not ^ this.ContainsKey key then
                this.Add (key, value)
                true
            else false

        /// Check value equality of dictionary.
        /// NOTE: be wary the highly imperative nature of this code.
        member this.ValueEquals (that : Dictionary<'k, 'v>) =
            use mutable enr = this.GetEnumerator ()
            use mutable enr2 = that.GetEnumerator ()
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

[<AutoOpen>]
module DictionaryOperators =

    /// Like dict, but returns a concrete Dictionary instance with structural hashing.
    /// NOTE: Also uses forced adding, allowing multiple of the same key in the kvps.
    let dictC kvps =
        let dictionary = Dictionary HashIdentity.Structural
        for (kvp : KeyValuePair<_, _>) in kvps do dictionary.ForceAdd (kvp.Key, kvp.Value)
        dictionary