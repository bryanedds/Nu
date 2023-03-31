// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace System.Collections.Generic

[<RequireQualifiedAccess>]
module OrderedDictionary =

    /// Make a dictionary with a single entry.
    let inline singleton (comparer : KeyValuePair<'k, 'v> IEqualityComparer) key value =
        let dictionary = OrderedDictionary comparer
        dictionary.Add (key, value)
        dictionary

    /// Map over an ordered dictionary. A new ordered dictionary is produced.
    let map (mapper : KeyValuePair<'k, 'v> -> 'v) (dictionary : OrderedDictionary<'k, 'v>) =
        let result = Dictionary<'k, 'v> dictionary.Comparer
        for kvp in dictionary do result.Add (kvp.Key, mapper kvp)
        result

    /// Fold over an ordered dictionary.
    let fold<'s, 'k, 'v> folder (state : 's) (dictionary : OrderedDictionary<'k, 'v>) =
        let folder = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder
        let mutable state = state
        let mutable enr = dictionary.GetEnumerator ()
        while enr.MoveNext () do
            let kvp = enr.Current
            state <- folder.Invoke (state, kvp.Key, kvp.Value)
        state