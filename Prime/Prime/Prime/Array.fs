// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2013.

[<RequireQualifiedAccess>]
module Array

/// Index into an array.
let inline at index (arr : 'a array)=
    arr.[index]

/// Try to find an index in reverse.
let tryFindIndexRev pred arr =
    let mutable index = Array.length arr - 1
    let mutable found = false
    while index >= 0 && not found do
        if pred arr.[index] then found <- true
        else index <- index - 1
    if found then Some index else None

/// Get the last element of an array
let last arr =
    let arrLength = Array.length arr
    if arrLength = 0 then failwith "Cannot get the last element of an empty array."
    arr.[arrLength - 1]