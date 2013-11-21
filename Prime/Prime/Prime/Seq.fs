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

/// Fold, now with a counter!
let foldi folder state seq =
    let state' =
        Seq.fold
            (fun (i, state) item -> (i + 1, folder i state item))
            (0, state)
            seq
    snd state'

/// Check if no elements satisfy a predicate in a seq.
let fornone pred (seq : 'a seq) =
    let notPred = not << pred
    Seq.forall notPred seq

/// Implement a fold while predicate f passes.
/// Implementation thanks to Tomas Petricek!
let foldWhile f initial input =
    input |>
        Seq.scan (fun stateOpt inp -> stateOpt |> Option.bind (fun state -> f state inp)) (Some initial) |>
        Seq.takeWhile Option.isSome |>
        Seq.last |>
        Option.get