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
let tryTake (n : int) (seq_ : _ seq) =
    let e = seq_.GetEnumerator ()
    let i = ref 0
    seq {
        while e.MoveNext () && !i < n do
            i := !i + 1
            yield e.Current }

/// Project the first sequence onto the second.
let project projector (seq_ : 'a seq) (seq2 : 'b option seq) =
    let e = seq_.GetEnumerator ()
    let e2 = seq2.GetEnumerator ()
    seq {
        while e.MoveNext () do
            let projection = 
                if e2.MoveNext () then
                    match projector e2.Current with
                    | Some projection -> projection
                    | None -> e.Current
                else e.Current
            yield projection }

/// Implement a fold while folder results in Some.
let foldWhile folder (state : 's) (seq : 't seq) =
    let mutable lastState = state
    let mutable optState = Some lastState
    let mutable enr = seq.GetEnumerator ()
    while optState.IsSome && enr.MoveNext () do
        lastState <- optState.Value
        optState <- folder lastState enr.Current
    match optState with
    | Some state -> state
    | None -> lastState

/// Check that a predicate passes for NO items in a sequence.
let rec notExists pred seq =
    match tryHead seq with
    | Some head -> not ^ pred head && notExists pred (Seq.skip 1 seq)
    | None -> true