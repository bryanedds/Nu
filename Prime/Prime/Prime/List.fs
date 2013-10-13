// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module List
open System
open System.Collections.Generic

// TODO: for speed, implement List functions _without_ using Seq functions.

/// Create a singleton list.
let singleton elem = [elem]

let rec private subpartitionPlus fnOptU list left right =
    match list with
    | [] -> (left, right)
    | head :: tail ->
        let optU = fnOptU head
        match optU with
        | None -> subpartitionPlus fnOptU tail left (head :: right)
        | Some u -> subpartitionPlus fnOptU tail (u :: left) right

let partitionPlus fnOptU list =
    subpartitionPlus fnOptU list [] []

/// Query that a list has at least n elements.
let rec hasAtLeast n (list : 'a list) =
    if n = 0 then true
    elif list.IsEmpty then false
    else hasAtLeast (n - 1) list.Tail

/// Query that a list has at most n elements.
let rec hasAtMost n (list : 'a list) =
    if list.IsEmpty then true
    elif n = 0 then false
    else hasAtMost (n - 1) list.Tail

/// Query that a list has exactly n elements.
let rec hasExactly n (list : 'a list) =
    if n = 0 then list.IsEmpty
    elif list.IsEmpty then false
    else hasExactly (n - 1) list.Tail

/// Query that a list has at least n elements.
let rec hasBetween n m (list : 'a list) =
    if n = 0 then hasAtMost m list
    elif list.IsEmpty then false
    else hasBetween (n - 1) (m - 1) list.Tail

/// Query that two lists are of the same length.
let rec areSameLength (list : 'a list) (list2 : 'b list) =
    if list.IsEmpty then list2.IsEmpty
    elif list2.IsEmpty then false
    else areSameLength list.Tail list2.Tail

/// Try to find a value.
let rec tryFindPlus pred list =
    match list with
    | [] -> None
    | head :: tail ->
        let (pass, value) = pred head
        if pass then Some value
        else tryFindPlus pred tail

/// Try to find a value at index n.
let rec tryFindAt n list =
    match list with
    | [] -> None
    | head :: tail ->
        if n = 0 then Some head
        else tryFindAt (n - 1) list

/// Pair up a list.
let pairUp list =
    let even = ref false
    let (evens, odds) =
        List.partition
            (fun elem ->
                let result = not !even
                even := result
                result)
            list
    List.zip evens odds

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
        let state2 = roller state curr next
        if rest.Tail.IsEmpty then state2
        else roll roller state2 rest

/// Windowed for lists.
let windowed count (list : 'a list) =
    List.ofSeq (Seq.windowed count list)

/// Zip two lists by a function.
/// TODO: optimize with program fusion.
let zipBy by first second =
    let zipped = List.zip first second
    List.map by zipped

/// Get Some head of the list or None.
let tryHead list =
    List.tryFind (fun _ -> true) list
    
/// Replace a list's head.
let replaceHead list head =
    head :: List.tail list

/// Truncate for lists.
let truncate count list =
    let results = Seq.truncate count list
    List.ofSeq results

/// Forall for lists.
/// TODO: make this tail-recursive.
let rec forall pred list =
    match list with
    | [] -> true
    | head :: tail -> pred head && forall pred tail

/// Fornone for lists.
/// TODO: make this tail-recursive.
let rec fornone pred list =
    match list with
    | [] -> true
    | head :: tail -> not (pred head) && fornone pred tail

/// Take for lists.
/// TODO: make this tail-recursive.
let rec take number (list : 'a list) =
    match number with
    | 0 -> []
    | _ -> list.Head :: take (number - 1) list.Tail

/// Skip for lists.
let rec skip number (list : 'a list) =
    match number with
    | 0 -> list
    | _ -> skip (number - 1) list.Tail

/// Take while for lists.
let takeWhile pred list =
    let results = Seq.takeWhile pred list
    List.ofSeq results

/// Skip while for lists.
let skipWhile pred list =
    let results = Seq.skipWhile pred list
    List.ofSeq results

/// Distinct for lists.
let distinct list =
    let results = Seq.distinct list
    List.ofSeq results

/// DistinctBy for lists.
let distinctBy pred list =
    let results = Seq.distinctBy pred list
    List.ofSeq results

/// Get the last item from a list.
let last (list : 'a list) =
    let length = list.Length
    List.nth list (length - 1)

/// Get all but the last item from a list.
let allButLast (list : 'a list) =
    take (list.Length - 1) list

/// Convert option values to definite values.
let definitize opts =
    List.choose id opts

/// Make a list of options an all or nothing proposition.
/// TODO: optimize with program fusion.
let allOrEmpty (opts : 'a option list) =
    let definites = definitize opts
    if areSameLength definites opts then definites else []

/// Make a transformed list of options an all or nothing proposition.
/// TODO: optimize with program fusion.
let allOrEmptyBy by list =
    let definites = List.choose (fun elem -> by elem) list
    if areSameLength definites list then definites else []

/// Pad a list with count instances of its last elem.
let padWithLast count list =
    let lastElem = last list
    let padding = List.init count (fun _ -> lastElem)
    list @ padding

/// Pad a list with instances of its last elem so that it is proportion to another list.
let padWithLastToProportion (list : 'b list) (list2 : 'a list) =
    let deficit = list2.Length - list.Length
    padWithLast deficit list

/// Join a list into a string separated by sep.
let join sep (list : string list) =
    if list.IsEmpty then ""
    else List.reduce (fun acc elem -> acc + sep + elem) list

/// Join a list into a string separated by sep.
/// TODO: consider optimizing with a StringBuilder.
let joinBy by sep (list : 'a list) =
    if list.IsEmpty then ""
    else
        List.fold 
            (fun (acc : string) elem ->
                let elemStr = by elem
                if acc.Length = 0 then elemStr
                else acc + sep + elemStr)
            ""
            list

/// Join a list of lists into a list separated by sep.
let joinList sep (list : 'a list list) =
    if list.IsEmpty then []
    else List.reduce (fun acc elem -> acc @ sep @ elem) list

/// Take elements until an element satisfies a predicate, taking also that element.
let takeTillInclusive pred list =
    let optIndex = List.tryFindIndex pred list
    match optIndex with
    | None -> list
    | Some index ->
        let incIndex = index + 1
        if hasAtLeast incIndex list then take incIndex list
        else take index list

/// Runs a binary set operation on two lists that are converted to sets.
let setBinop binop (firstList : 'a list) (secondList : 'a list) =
    let firstSet = Set.ofList firstList
    let secondSet = Set.ofList secondList
    binop firstSet secondSet

/// Query that the first list a subset of the second list.
let isSubset firstList secondList = setBinop Set.isSubset firstList secondList

/// Query that the first list a superset of the second list.
let isSuperset firstList secondList = setBinop Set.isSuperset firstList secondList

/// Get the set interesection of two lists.
let intersect firstList secondList = setBinop Set.intersect firstList secondList

/// Add a list of pairs to a Dictionary.
let addToDictionary (dict : Dictionary<'k, 'v>) list =
    List.iter (fun (k, v) -> dict.Add (k, v)) list
    
/// Add a list of values to a Dictionary.
let addToDictionaryBy by (dict : Dictionary<'k, 'v>) list =
    List.iter (fun value -> dict.Add (by value)) list

/// Convert a list of pairs to a Dictionary.
let toDictionary list =
    let dict = Dictionary ()
    addToDictionary dict list
    dict

/// Convert a list of values to a Dictionary.
let toDictionaryBy by list =
    let dict = Dictionary ()
    addToDictionaryBy by dict list
    dict

/// Convert a list to a HashSet.
let toHashSet list =
    let hashSet = HashSet ()
    List.iter (fun elem -> ignore (hashSet.Add elem)) list
    hashSet