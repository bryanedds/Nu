// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

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
    | None -> aDefault
    | Some _ as head -> head

/// Convert option values to definite values.
let inline definitize opts =
    Seq.choose id opts

/// Fold, now with a counter!
let foldi folder state seq =
    let (_, result) =
        Seq.fold
            (fun (i, state) item -> (i + 1, folder i state item))
            (0, state)
            seq
    result

/// Check if no elements satisfy a predicate in a seq.
let fornone pred seq =
    let notPred = not << pred
    Seq.forall notPred seq

/// Implement a fold while fn results in Some.
/// Implementation thanks to Tomas Petricek!
let foldWhile fn initial input =
    input |>
        Seq.scan (fun stateOpt inp -> stateOpt |> Option.bind (fun state -> fn state inp)) (Some initial) |>
        Seq.takeWhile Option.isSome |>
        Seq.last |>
        Option.get

/// Compare two sequences.
let rec compare<'a when 'a :> IComparable<'a>> (left : 'a seq) (right : 'a seq) =
    let mutable leftEnr = left.GetEnumerator ()
    let mutable rightEnr = right.GetEnumerator ()
    let mutable compareResult = 0
    let mutable noResult = true
    while noResult do
        let leftIsDone = not <| leftEnr.MoveNext ()
        let rightIsDone = not <| rightEnr.MoveNext ()
        if leftIsDone then
            if not rightIsDone then compareResult <- 1
            noResult <- false
        elif rightIsDone then
            compareResult <- -1
            noResult <- false
        else
            compareResult <- leftEnr.Current.CompareTo rightEnr.Current
            if compareResult <> 0 then noResult <- false
    compareResult