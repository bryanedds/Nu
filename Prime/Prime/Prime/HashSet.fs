// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

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

        /// Force the addition of an item, replacing the existing one if necessary.
        member this.ForceAdd item =
            let forced = this.Remove item
            this.Add item |> ignore
            forced

        /// Try to add an item, returning false upon failure.
        member this.TryAdd item =
            if not ^ this.Contains item
            then this.Add item
            else false