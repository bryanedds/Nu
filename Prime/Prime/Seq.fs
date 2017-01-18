// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

[<RequireQualifiedAccess>]
module Seq
open System
open Prime

/// Check that a sequence is not empty.
let rec notEmpty seq =
    not ^ Seq.isEmpty seq

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

/// Convert option values to definite values, returning an additional flag to indicate that any were none.
let definitizePlus opts =
    let (flag, list) =
        Seq.foldBack
            (fun opt (anyNone, values) ->
                match opt with
                | Some value -> (anyNone, value :: values)
                | None -> (false, values))
            opts (true, [])
    (flag, Seq.ofList list)

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

/// Check if no items satisfy a predicate in a seq.
let fornone pred seq =
    let notPred = not << pred
    Seq.forall notPred seq

/// A more tolerant and open-minded take.
let tryTake (n : int) (seq : _ seq) =
    System.Linq.Enumerable.Take (seq, n)

/// Project the first sequence onto the second.
let project projector (seq_ : 'a seq) (seq2 : 'b option seq) =
    use enr = seq_.GetEnumerator ()
    use enr2 = seq2.GetEnumerator ()
    seq {
        while enr.MoveNext () do
            let projection = 
                if enr2.MoveNext () then
                    match projector enr2.Current with
                    | Some projection -> projection
                    | None -> enr.Current
                else enr.Current
            yield projection }

/// Implement a fold while folder results in Some.
let foldWhile folder (state : 's) (seq : 't seq) =
    let mutable lastState = state
    let mutable stateOpt = Some lastState
    use mutable enr = seq.GetEnumerator ()
    while stateOpt.IsSome && enr.MoveNext () do
        lastState <- stateOpt.Value
        stateOpt <- folder lastState enr.Current
    match stateOpt with
    | Some state -> state
    | None -> lastState

/// Implement a fold while folder results in Right.
let foldWhileRight folder (state : Either<_, _>) (seq : 't seq) =
    let mutable state = state // make mutable
    use mutable enr = seq.GetEnumerator ()
    while Either.isRight state && enr.MoveNext () do
        state <- folder (Either.getRightValue state) enr.Current
    state

/// Implement a fold until folder results in Some.
let foldUntil folder (state : 's) (seq : 't seq) =
    let mutable isFirst = true // no do while necessitates this flag
    let mutable lastState = state
    let mutable stateOpt = Some lastState
    use mutable enr = seq.GetEnumerator ()
    while (isFirst || stateOpt.IsNone) && enr.MoveNext () do
        isFirst <- false
        lastState <- stateOpt.Value
        stateOpt <- folder lastState enr.Current
    match stateOpt with
    | Some state -> state
    | None -> lastState

/// Implement a fold until folder results in Right.
let foldUntilRight folder (state : Either<_, _>) (seq : 't seq) =
    let mutable state = state // make mutable
    use mutable enr = seq.GetEnumerator ()
    while Either.isLeft state && enr.MoveNext () do
        state <- folder (Either.getLeftValue state) enr.Current
    state

/// Check that a predicate passes for NO items in a sequence.
let rec notExists pred seq =
    match tryHead seq with
    | Some head -> not ^ pred head && notExists pred (Seq.skip 1 seq)
    | None -> true

/// Split a sequence on a predicate.
let split pred seq =
    let rec splitInner pred left right seq =
        match tryHead seq with
        | Some head ->
            if pred head
            then splitInner pred (head :: left) right (Seq.tail seq)
            else splitInner pred left (head :: right) (Seq.tail seq)
        | None -> (left, right)
    splitInner pred [] [] seq