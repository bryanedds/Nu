// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
#nowarn "42"

/// Provides operators for branchless programming.
/// NOTE: implemented in terms of a static class to enable overloading.
/// TODO: consider moving into Prime.
type [<AbstractClass; Sealed>] Branchless () =

    /// Reinterpret cast a value from 'a to 'b.
    /// Dangerous if used incorrectly.
    /// In fact, try not to use this directly at all.
    static member inline reinterpret<'a, 'b> (a : 'a) : 'b = (# "" a : 'b #)

    /// Convert a bool as an int without branching.
    static member inline boolToInt (bool : bool) = Branchless.reinterpret bool : int

    /// Convert an int as a bool without branching.
    static member inline intToBool (int : int) = Branchless.reinterpret int : bool

    /// Convert a bool as an int64 without branching.
    static member inline boolToInt64 (bool : bool) = (Branchless.reinterpret bool : int) |> int64

    /// Convert an int64 as a bool without branching.
    static member inline int64ToBool (int64 : int64) = Branchless.reinterpret (int int64) : bool

    // NOTE: this code has been dummied out due to broken performance. Broken perf is likely due to the resulting
    // value not landing in a floating-point enabled register. I don't know of a performant way to land an int
    // value into a fp register with .NET.
    // Convert a bool as a single without branching.
    //static member inline boolToSingle (bool : bool) =
    //    let int = (Branchless.reinterpret bool : int)
    //    let intFraction = int <<< 23
    //    let intExponent = (int <<< 24) ||| (int <<< 25) ||| (int <<< 26) ||| (int <<< 27) ||| (int <<< 28) ||| (int <<< 29)
    //    Branchless.reinterpret (intFraction ||| intExponent) : single

    // NOTE: this code has been dummied out since a binary cmp between a floating point register and an int seems
    // to always result in unequal. Like stated above, I'm don't know how to efficiently land an fp register value
    // in a non-fp register.
    // Convert a single as a bool without branching.
    //static member inline singleToBool (single : single) = (Branchless.reinterpret single : int) <> 0

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