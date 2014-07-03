// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime

module Pair =

    /// Replace pair member fst.
    let inline withFst fst (_, snd) =
        (fst, snd)

    /// Replace pair member snd.
    let inline withSnd snd (fst, _) =
        (fst, snd)

    /// Replace triple member b.
    let inline withB b (a, _, c) =
        (a, b, c)

    /// Make a pair.
    let make a b =
        (a, b)