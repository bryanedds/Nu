// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
#nowarn "42" // for reinterpret definition

/// Provides operators for branchless programming.
/// NOTE: implemented in terms of a static class to enable overloading.
type [<AbstractClass; Sealed>] Branchless () =

    /// Reinterpret cast a value from 'a to 'b.
    /// Dangerous if used incorrectly. In fact, try not to use this directly at all.
    static member inline reinterpret<'a, 'b> (a : 'a) : 'b = (# "" a : 'b #)

    /// Convert a bool as an int without branching.
    static member inline boolToInt (bool : bool) = Branchless.reinterpret bool : int

    /// Convert an int as a bool without branching.
    static member inline intToBool (int : int) = Branchless.reinterpret int : bool

    /// Convert a bool as an int64 without branching.
    static member inline boolToInt64 (bool : bool) = (Branchless.reinterpret bool : int) |> int64

    /// Convert an int64 as a bool without branching.
    static member inline int64ToBool (int64 : int64) = Branchless.reinterpret (int int64) : bool

    // Convert a bool as a single without branching.
    static member inline boolToSingle (bool : bool) = Convert.ToSingle bool // JIT should lower this to (setcc + movd + cvtsi2ss)

    // Convert a single as a bool without branching.
    static member inline singleToBool (single : single) = single <> 0.0f // JIT should lower this to (ucomiss + setne)

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