// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Diagnostics
open System.Runtime
open System.Threading
open Prime

/// Provides core operations for basic Nu functionality.
[<RequireQualifiedAccess>]
module Core =

    let mutable private LastTimeStamp = Stopwatch.GetTimestamp ()
    let private LastTimeStampLock = obj ()

    /// Get a time stamp at the highest-available resolution.
    /// Thread-safe.
    let getTimeStamp () =
        Stopwatch.GetTimestamp ()

    /// Get a unique, monotonically increasing time stamp.
    /// Thread-safe.
    let getTimeStampUnique () =
        let mutable result = 0L
        let mutable going = true
        while going do
            let last = Volatile.Read &LastTimeStamp
            let now = getTimeStamp ()
            let next = if now > last then now else last + 1L
            if Interlocked.CompareExchange (&LastTimeStamp, next, last) = last then
                result <- next
                going <- false
        result

    /// Do a full garbage collection pass, optionally also compacting the LOH.
    /// Thread-safe.
    let collectGarbage compactLoh =
        if compactLoh then GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce;
        GC.Collect ()

/// The core operators that come with the Nu Game Engine.
[<AutoOpen>]
module CoreOperators =

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let inline ($) f g = f g

    /// Test for object equality.
    /// Same as the (===) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (===) (a : obj) (b : obj) = objEq a b

    /// Test for object inequality.
    /// Same as the (=/=) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (=/=) (a : obj) (b : obj) = objNeq a b