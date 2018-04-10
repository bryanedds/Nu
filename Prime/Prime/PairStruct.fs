// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Prime

[<AutoOpen>]
module PairStructOperators =

    /// Get the fst value in a struct pair.
    let inline fst' struct (fst, _) =
        fst

    /// Get the snd value in a struct pair.
    let inline snd' struct (_, snd) =
        snd

    /// Replace struct pair member fst.
    let inline withFst' fst struct (_, snd) =
        struct (fst, snd)

    /// Replace struct pair member snd.
    let inline withSnd' snd struct (fst, _) =
        struct (fst, snd)

    /// Map over struct pair member fst.
    let inline mapFst' mapper struct (fst, snd) =
        struct (mapper fst, snd)

    /// Map over struct pair member snd.
    let inline mapSnd' mapper struct (fst, snd) =
        struct (fst, mapper snd)

[<RequireQualifiedAccess>]
module PairStruct =

    /// Make a pair.
    let make a b =
        (a, b)