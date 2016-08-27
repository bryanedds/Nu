// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

[<RequireQualifiedAccess>]
module List
open System
open System.Collections.Generic
open Prime

/// Check that a list is not empty.
let rec notEmpty list =
    not ^ List.isEmpty list

/// Make a singleton list.
let singleton value =
    [value]

/// The missing cons function.
let cons head tail =
    head :: tail

/// Cons with flipped arguments.
let inline flipCons tail head =
    head :: tail

/// Partition a list.
let partitionPlus fnOptU list =
    let rec subpartitionPlus fnOptU list left right =
        match list with
        | [] -> (left, right)
        | head :: tail ->
            let optU = fnOptU head
            match optU with
            | Some u -> subpartitionPlus fnOptU tail (u :: left) right
            | None -> subpartitionPlus fnOptU tail left (head :: right)
    subpartitionPlus fnOptU list [] []

/// Check that a list has at least n items.
let rec hasAtLeast n (list : 'a list) =
    if n = 0 then true
    elif list.IsEmpty then false
    else hasAtLeast (n - 1) list.Tail

/// Check that a list has at most n items.
let rec hasAtMost n (list : 'a list) =
    if list.IsEmpty then true
    elif n = 0 then false
    else hasAtMost (n - 1) list.Tail

/// Check that a list has exactly n items.
let rec hasExactly n (list : 'a list) =
    if n = 0 then list.IsEmpty
    elif list.IsEmpty then false
    else hasExactly (n - 1) list.Tail

/// Check that a list has at least n items.
let rec hasBetween n m (list : 'a list) =
    if n = 0 then hasAtMost m list
    elif list.IsEmpty then false
    else hasBetween (n - 1) (m - 1) list.Tail

/// Check that two lists are of the same length.
let rec areSameLength (list : 'a list) (list2 : 'b list) =
    if list.IsEmpty then list2.IsEmpty
    elif list2.IsEmpty then false
    else areSameLength list.Tail list2.Tail

/// Try to find a value.
let rec tryFindPlus pred list =
    match list with
    | [] -> None
    | head :: tail ->
        match pred head with
        | Some _ as someValue -> someValue
        | None -> tryFindPlus pred tail

/// Try to find a value at index n.
let rec tryFindAt n list =
    match list with
    | [] -> None
    | head :: tail ->
        if n = 0 then Some head
        else tryFindAt (n - 1) tail

/// For all 2 that indicates uneven lists by returning false rather than raising.
let rec forall2Plus pred list list2 =
    match list with
    | [] ->
        match list2 with
        | [] -> true
        | _ :: _ -> false
    | head :: tail ->
        match list2 with
        | [] -> false
        | head2 :: tail2 ->
            if pred head head2 then forall2Plus pred tail tail2
            else false

/// Threads a computation state through the adjacent members of a list.
let rec roll roller state (list : 'a list) =
    if list.IsEmpty then state
    else
        let curr = list.[0]
        let next = list.[1]
        let rest = list.Tail
        let state = roller state curr next
        if rest.Tail.IsEmpty then state
        else roll roller state rest

/// Zip two lists by a function.
/// TODO: optimize with program fusion.
let zipBy by first second =
    let zipped = List.zip first second
    List.map by zipped

/// A more tolerant and open-minded take.
let tryTake (n : int) (list : _ list) =
    Seq.tryTake n list |> List.ofSeq

/// Project the first list onto the second.
let project pred (list : 'a list) (list2 : 'b option list) =
    Seq.project pred list list2 |> List.ofSeq

/// Replace a list's head.
let replaceHead list head =
    head :: List.tail list

/// Fornone for lists.
let rec fornone pred (list : _ list) =
    match list with
    | [] -> true
    | head :: tail -> not (pred head) && fornone pred tail
    
/// Foldi for lists.
let foldi folder state (list : _ list) =
    Seq.foldi folder state list

/// Get all but the last item from a list.
let allButLast list =
    List.take (List.length list - 1) list

/// Convert option values to definite values.
let definitize opts =
    List.choose id opts

/// Convert option values to definite values, returning an additional flag to indicate that any were none.
let definitizePlus opts =
    let (flag, seq) = Seq.definitizePlus opts
    (flag, List.ofSeq seq)

/// Make a list of options an all or nothing proposition.
/// TODO: optimize with program fusion.
let allOrEmpty (opts : 'a option list) =
    let definites = definitize opts
    if areSameLength definites opts then definites else []

/// Make a transformed list of options an all or nothing proposition.
/// TODO: optimize with program fusion.
let allOrEmptyBy by list =
    let definites = List.choose (fun item -> by item) list
    if areSameLength definites list then definites else []

/// Resize a list with the given elem for expanded elements.
let resize count elem list =
    let taken = List.tryTake size list
    let takenCount = List.length taken
    let delta = count - takenCount
    if delta < 1 then taken else List.append taken (List.init delta (fun _ -> elem))

/// Pad a list with count instances of its last item, removing items from back if count is negative.
let pad count elem list =
    if count = 0 then list
    elif count > 0 then list @ List.init count (fun _ -> elem)
    else List.take (List.length list + count) list

/// Pad a list with count instances of its last item.
let padWithLast count list =
    pad count (List.last list) list

/// Pad a list with instances of its last item so that it is proportion to another list.
let padWithLastToProportion list list2 =
    let deficit = List.length list2 - List.length list
    padWithLast deficit list

/// Join a list into a string separated by sep.
let join sep list =
    if List.isEmpty list then String.Empty
    else List.reduce (fun acc item -> acc + sep + item) list

/// Join a list into a string separated by sep.
/// TODO: consider optimizing with a StringBuilder.
let joinBy by sep list =
    if List.isEmpty list then String.Empty
    else
        List.fold 
            (fun acc item ->
                let elemStr = by item
                if String.length acc = 0 then elemStr
                else acc + sep + elemStr)
            String.Empty
            list

/// Join a list of lists into a list separated by sep.
let joinList sep list =
    if List.isEmpty list then []
    else List.reduce (fun acc item -> acc @ sep @ item) list

/// Take items until an item satisfies a predicate, taking also that element.
let takeTillInclusive pred list =
    let optIndex = List.tryFindIndex pred list
    match optIndex with
    | Some index ->
        let incIndex = index + 1
        if hasAtLeast incIndex list then List.take incIndex list
        else List.take index list
    | None -> list

/// Runs a binary set operation on two lists that are converted to sets.
let setBinop binop firstList secondList =
    let firstSet = Set.ofList firstList
    let secondSet = Set.ofList secondList
    binop firstSet secondSet

/// Check that the first list a subset of the second list.
let isSubset firstList secondList =
    setBinop Set.isSubset firstList secondList

/// Check that the first list a superset of the second list.
let isSuperset firstList secondList =
    setBinop Set.isSuperset firstList secondList

/// Get the set interesection of two lists.
let intersect firstList secondList =
    setBinop Set.intersect firstList secondList

/// Add a list of pairs to a Dictionary.
let addToDict (dictionary : Dictionary<'k, 'v>) list =
    List.iter (fun (k, v) -> dictionary.Add (k, v)) list
    
/// Add a list of values to a Dictionary.
let addToDictBy by (dictionary : Dictionary<'k, 'v>) list =
    List.iter (fun value -> dictionary.Add (by value)) list

/// Convert a list of pairs to a Dictionary.
let toDict list =
    let dictionary = Dictionary HashIdentity.Structural
    addToDict dictionary list
    dictionary

/// Convert a list of values to a Dictionary.
let toDictBy by list =
    let dictionary = Dictionary HashIdentity.Structural
    addToDictBy by dictionary list
    dictionary

/// Convert a list to a HashSet.
let toHashSet list =
    let hashSet = HashSet HashIdentity.Structural
    List.iter (fun item -> hashSet.Add item |> ignore) list
    hashSet

/// Implement a fold while folder results in Some.
let foldWhile folder (state : 's) (list : 't list) =
    Seq.foldWhile folder state ^ List.toSeq list

/// Implement a fold until folder results in Nome.
let foldUntil folder (state : 's) (list : 't list) =
    Seq.foldUntil folder state ^ List.toSeq list

/// Check that a predicate passes for NO items in a list.
let notExists pred (list : 't list) =
    Seq.notExists pred list

/// Split a list on a predicate.
let split pred (list : 't list) =
    Seq.split pred list

/// Remove all items from a list that satisfy a predicate.
let rec remove pred list =
    List.foldBack
        (fun item listAcc -> if pred item then listAcc else item :: listAcc)
        list
        []

/// Compute the 'power set' of a list.
/// Example - [0, 1, 2] becomes [[]; [0]; [0; 1]; [0; 1; 2]]
let power list =
    [for x in 0 .. List.length list do
        yield List.take x list]

/// Compare a list of strings lexicographically.
let rec inline compareStrings (list : string list) (list2 : string list) =
    match (list, list2) with
    | ([], []) -> 0
    | (_ :: _, []) -> 1
    | ([], _ :: _) -> -1
    | (head :: tail, head2 :: tail2) ->
        let result = strCmp head head2
        if result = 0 then compare tail tail2
        else result

/// Check that a list contains duplicate entries.
/// O(n^2) complexity, but does not allocate.
let rec containsDuplicates list =
    match list with
    | [] -> false
    | [_] -> false
    | head :: tail -> List.contains head tail || containsDuplicates tail

/// Check that a list contains triplicate entries.
let rec containsTriplicates list =
    list |>
    Array.ofList |>
    Array.groupBy id |>
    Array.exists (fun (_, group) -> group.Length >= 3)

/// Hash a list.
/// NOTE: May be a pessimization.
let inline hash list =
    List.fold
        (fun hashValue name -> hashValue ^^^ name.GetHashCode ())
        0
        list