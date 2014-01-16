module Array

/// Try to find an index in reverse.
let tryFindIndexRev pred (arr : 'a array) =
    let mutable index = arr.Length - 1
    let mutable found = false
    while index >= 0 && not found do
        if pred arr.[index] then found <- true
        else index <- index - 1
    if found then Some index else None

/// Get the last element of an array
let last (arr : 'a array) =
    let arrLength = arr.Length
    if arrLength = 0 then failwith "Cannot get the last element of an empty array."
    arr.[arrLength - 1]