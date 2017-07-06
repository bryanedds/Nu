// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module HashSet =

    /// Make a hash set with a single item.
    let singleton item =
        List.toHashSet [item]

[<AutoOpen>]
module HashSetExtension =

    /// HashSet extension methods.
    type HashSet<'a> with

        /// Try to add an item, returning false upon failure.
        member this.TryAdd item =
            if not (this.Contains item)
            then this.Add item
            else false

[<AutoOpen>]
module HashSetOperators =

    /// Make a concrete HashSet instance populated with the given items and using vanilla hashing.
    let hashset items =
        let hashSet = HashSet ()
        for item in items do hashSet.TryAdd item |> ignore
        hashSet

    /// Make a concrete HashSet instance populated with the given items and using structural hashing.
    let hashsetPlus items =
        let hashSet = HashSet HashIdentity.Structural
        for item in items do hashSet.TryAdd item |> ignore
        hashSet