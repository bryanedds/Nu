// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

[<RequireQualifiedAccess>]
module Array
open Prime

/// Check that an array is not empty.
let rec notEmpty arr =
    not ^ Array.isEmpty arr

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