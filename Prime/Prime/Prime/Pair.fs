// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime

[<AutoOpen>]
module PairOperators =

    /// Replace pair member fst.
    let inline withFst fst (_, snd) =
        (fst, snd)

    /// Replace pair member snd.
    let inline withSnd snd (fst, _) =
        (fst, snd)

    /// Map over pair member fst.
    let inline mapFst mapper (fst, snd) =
        (mapper fst, snd)

    /// Map over pair member snd.
    let inline mapSnd mapper (fst, snd) =
        (fst, mapper snd)

[<RequireQualifiedAccess>]
module Pair =

    /// Make a pair.
    let make a b =
        (a, b)