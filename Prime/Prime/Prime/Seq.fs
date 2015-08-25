// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

[<RequireQualifiedAccess>]
module Seq
open System
open Prime

/// Get Some head of the seq or None.
let inline tryHead seq =
    Seq.tryFind tautology seq

/// Get a seq head or a default value if there is none.
let inline headOrDefault seq aDefault =
    match tryHead seq with
    | Some _ as head -> head
    | None -> aDefault

/// Convert option values to definite values.
let inline definitize opts =
    Seq.choose id opts

/// Fold with two inputs (plus state).
let fold2 folder state seq seq2 =
    let zipped = Seq.zip seq seq2
    Seq.fold (fun state (a, b) -> folder state a b) state zipped

/// Fold, now with a counter!
let foldi folder state seq =
    let (_, result) =
        Seq.fold
            (fun (i, state) item -> (i + 1, folder i state item))
            (0, state)
            seq
    result

/// Fold-back for seqs.
let foldBack folder values state =
    List.foldBack folder (List.ofSeq values) state

/// Check if no elements satisfy a predicate in a seq.
let fornone pred seq =
    let notPred = not << pred
    Seq.forall notPred seq

/// A more tolerant and open-minded take.
let tryTake (n : int) (s : _ seq) =
    let e = s.GetEnumerator ()
    let i = ref 0
    seq {
        while e.MoveNext () && !i < n do
            i := !i + 1
            yield e.Current }

/// Implement a fold while fn results in Some.
/// Implementation thanks to Tomas Petricek!
let foldWhile fn initial input =
    input |>
        Seq.scan (fun stateOpt inp -> stateOpt |> Option.bind (fun state -> fn state inp)) (Some initial) |>
        Seq.takeWhile Option.isSome |>
        Seq.last |>
        Option.get

/// Check that a predicate passes for NO items in a sequence.
let rec notExists pred seq =
    match tryHead seq with
    | Some head -> not ^ pred head && notExists pred (Seq.skip 1 seq)
    | None -> true