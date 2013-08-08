// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Seq

/// Get Some head of the seq or None.
let tryHead seq =
    Seq.tryFind (fun _ -> true) seq

/// Convert option values to definite values.
let definitize opts =
    Seq.choose id opts

/// Check if no elements satisfy a predicate in a seq.
let fornone pred (seq : 'a seq) =
    let notPred = not << pred
    Seq.forall notPred seq