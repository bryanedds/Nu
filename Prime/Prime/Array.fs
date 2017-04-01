// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

[<RequireQualifiedAccess>]
module Array
open System
open Prime

/// 'Cons' a value to the front of an array.
let cons elem tail =
    let tailLength = Array.length tail
    let arr2 = Array.zeroCreate (inc tailLength)
    arr2.[0] <- elem
    Array.Copy (tail, 0, arr2, 1, tailLength)
    arr2

/// Add a value to the end of an array.
let add elem arr =
    let tailLength = Array.length arr
    let arr2 = Array.zeroCreate (inc tailLength)
    arr2.[tailLength] <- elem
    Array.Copy (arr, 0, arr2, 0, tailLength)
    arr2

/// Check that an array is not empty.
let rec notEmpty arr =
    not ^ Array.isEmpty arr

/// Check that a predicate passes for NO items in an array.
let rec notExists pred arr =
    not ^ Array.exists pred arr

/// Convert option values to definite values.
let definitize opts =
    Array.choose id opts

/// Convert option values to definite values, returning an additional flag to indicate that all values were some.
let definitizePlus opts =
    let (flag, seq) = Seq.definitizePlus opts
    (flag, Array.ofSeq seq)

/// A more tolerant and open-minded take.
let tryTake (count : int) (arr : _ array) =
    Seq.tryTake count arr |> Array.ofSeq

/// A more tolerant and open-minded skip.
let trySkip (count : int) (arr : _ array) =
    Seq.trySkip count arr |> Array.ofSeq