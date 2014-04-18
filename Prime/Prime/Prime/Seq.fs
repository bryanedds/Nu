// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Seq
open Prime

/// Get Some head of the seq or None.
let inline tryHead seq =
    Seq.tryFind tautology seq

/// Get a seq head or a default value if there is none.
let inline headOrDefault seq aDefault =
    match tryHead seq with
    | None -> aDefault
    | Some _ as head -> head

/// Convert option values to definite values.
let inline definitize opts =
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
let fornone pred seq =
    let notPred = not << pred
    Seq.forall notPred seq

/// Implement a fold while a predicate passes.
/// Implementation thanks to Tomas Petricek!
let foldWhile fn initial input =
    input |>
        Seq.scan (fun stateOpt inp -> stateOpt |> Option.bind (fun state -> fn state inp)) (Some initial) |>
        Seq.takeWhile Option.isSome |>
        Seq.last |>
        Option.get