// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System.Collections.Generic

[<RequireQualifiedAccess>]
module HashSet =

    /// Make a hash set with a single element.
    let singleton item =
        List.toHashSet [item]

[<AutoOpen>]
module HashSetExtension =

    type HashSet<'a> with

        /// Force the addition of an element, removing the existing one if necessary.
        member this.ForceAdd item =
            let forced = this.Remove item
            this.Add item |> ignore
            forced