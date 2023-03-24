// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
#nowarn "42"

/// Provides operators for branchless programming.
type Branchless () =

    static member inline private reinterpret<'a, 'b> (a : 'a) : 'b = (# "" a : 'b #)

    /// Convert a bool as an int without branching.
    static member inline boolToInt bool = Branchless.reinterpret bool : int

    /// Convert an int as a bool without branching.
    static member inline intToBool int = Branchless.reinterpret int : bool

    /// Convert a bool as an int64 without branching.
    static member inline boolToInt64 bool = (Branchless.reinterpret bool : int) |> int64

    /// Convert an int64 as a bool without branching.
    static member inline int64ToBool int64 = Branchless.reinterpret (int int64) : bool

    /// Convert a bool as a single without branching.
    static member inline boolToSingle bool =
        (Branchless.reinterpret bool : int) |> single
        //let int = (Branchless.reinterpret bool : int)
        //let intFraction = int <<< 23
        //let intExponent = (int <<< 24) ||| (int <<< 25) ||| (int <<< 26) ||| (int <<< 27) ||| (int <<< 28) ||| (int <<< 29)
        //Branchless.reinterpret (intFraction ||| intExponent) : single

    /// Convert a single as a bool without branching.
    static member inline singleToBool single =
        Branchless.reinterpret (int single) : bool
        //let int = Branchless.reinterpret single : int
        //let intFractionMask = 8388608 // 1 << 23
        //Branchless.reinterpret (int &&& intFractionMask >>> 23) : bool

    /// Branchless min for ints.
    static member inline min a = fun b ->
        let a' = Branchless.boolToInt (a <= b) * a
        let b' = Branchless.boolToInt (b < a) * b
        a' + b'

    /// Branchless max for ints.
    static member inline max a = fun b ->
        let a' = Branchless.boolToInt (a >= b) * a
        let b' = Branchless.boolToInt (b > a) * b
        a' + b'

    /// Branchless min for int64s.
    static member inline min a = fun b ->
        let a' = Branchless.boolToInt64 (a <= b) * a
        let b' = Branchless.boolToInt64 (b < a) * b
        a' + b'

    /// Branchless max for int64s.
    static member inline max a = fun b ->
        let a' = Branchless.boolToInt64 (a >= b) * a
        let b' = Branchless.boolToInt64 (b > a) * b
        a' + b'

    /// Branchless min for singles.
    static member inline min a = fun b ->
        let a' = Branchless.boolToSingle (a <= b) * a
        let b' = Branchless.boolToSingle (b < a) * b
        a' + b'

    /// Branchless max for singles.
    static member inline max a = fun b ->
        let a' = Branchless.boolToSingle (a >= b) * a
        let b' = Branchless.boolToSingle (b > a) * b
        a' + b'