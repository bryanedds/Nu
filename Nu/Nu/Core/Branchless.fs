// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
#nowarn "42"

/// Provides operators for branchless programming.
module Branchless =

    let inline private reinterpret<'a, 'b> (a : 'a) : 'b = (# "" a : 'b #)

    /// Convert a bool as an int without branching.
    let inline boolToInt bool = reinterpret bool : int

    /// Convert an int as a bool without branching.
    let inline intToBool int = reinterpret int : bool

    /// Convert a bool as an int64 without branching.
    let inline boolToInt64 bool = (reinterpret bool : int) |> int64

    /// Convert an int64 as a bool without branching.
    let inline int64ToBool int64 = reinterpret (int int64) : bool

    /// Convert a bool as a single without branching.
    let inline boolToSingle bool = (reinterpret bool : int) |> single

    /// Convert a single as a bool without branching.
    let inline singleToBool single = reinterpret (int single) : bool

    /// Convert a bool as a single without branching.
    let inline boolToDouble bool = (reinterpret bool : int) |> double

    /// Convert a single as a bool without branching.
    let inline doubleToBool double = reinterpret (int double) : bool

    /// Branchless min for ints.
    let inline min a b =
        let a' =  boolToInt (a <= b) * a
        let b' =  boolToInt (b > a) * b
        a' + b'

    /// Branchless max for ints.
    let inline max a b =
        let a' =  boolToInt (a >= b) * a
        let b' =  boolToInt (b < a) * b
        a' + b'

    /// Branchless min for int64s.
    let inline min64 a b =
        let a' =  boolToInt64 (a <= b) * a
        let b' =  boolToInt64 (b > a) * b
        a' + b'

    /// Branchless max for int64s.
    let inline max64 a b =
        let a' =  boolToInt64 (a >= b) * a
        let b' =  boolToInt64 (b < a) * b
        a' + b'

    /// Branchless min for singles.
    let inline minf a b =
        let a' =  boolToSingle (a <= b) * a
        let b' =  boolToSingle (b > a) * b
        a' + b'

    /// Branchless max for singles.
    let inline maxf a b =
        let a' =  boolToSingle (a >= b) * a
        let b' =  boolToSingle (b < a) * b
        a' + b'

    /// Branchless min for doubles.
    let inline mind a b =
        let a' =  boolToDouble (a <= b) * a
        let b' =  boolToDouble (b > a) * b
        a' + b'

    /// Branchless max for doubles.
    let inline maxd a b =
        let a' =  boolToDouble (a >= b) * a
        let b' =  boolToDouble (b < a) * b
        a' + b'