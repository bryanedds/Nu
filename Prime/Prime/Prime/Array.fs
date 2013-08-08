module Array

/// Try to find an index in reverse.
let tryFindIndexRev pred (arr : 'a array) =
    let mutable index = arr.Length - 1
    let mutable found = false
    while index >= 0 && not found do
        if pred arr.[index] then found <- true
        else index <- index - 1
    if found then Some index else None