// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open Prime

[<RequireQualifiedAccess>]
module Core =

    let mutable private LastTimeStamp = Stopwatch.GetTimestamp ()
    let private LastTimeStampLock = obj ()

    /// Get a time stamp at the highest-available resolution.
    let getTimeStamp () =
        Stopwatch.GetTimestamp ()

    /// Get a unique time stamp, spinning until the time stamp advances if need be. Thead-safe.
    let getTimeStampUnique () =
        lock LastTimeStampLock $ fun () ->
            let mutable nextStamp = getTimeStamp ()
            while nextStamp = LastTimeStamp do nextStamp <- getTimeStamp ()
            LastTimeStamp <- nextStamp
            nextStamp

[<AutoOpen>]
module CoreOperators =

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let inline ($) f g = f g

    /// Test for object equality.
    /// Same as the (===) operator found in Prime, but placed here to expose it directly from Nu.
    /// OPTIMIZATION: always tests reference equality first.
    let inline (===) (a : obj) (b : obj) =
        objEq a b

    /// Test for object inequality.
    /// Same as the (=/=) operator found in Prime, but placed here to expose it directly from Nu.
    /// OPTIMIZATION: always tests reference inequality first.
    let inline (=/=) (a : obj) (b : obj) =
        objNeq a b

// TODO: P1: remove this after updating Prime.
[<RequireQualifiedAccess>]
module Array =

    /// Pad an array with count instances of its last item, removing items from back if count is negative.
    let pad count elem arr =
        if count = 0 then arr
        elif count > 0 then Array.append arr (Array.init count (fun _ -> elem))
        else Array.take (Array.length arr + count) arr

    /// Pad an array with count instances of its last item.
    let padWithLast count arr =
        pad count (Array.last arr) arr

    /// Pad an array with instances of its last item so that it is proportion to another array.
    let padWithLastToProportion arr arr2 =
        let deficit = Array.length arr2 - Array.length arr
        padWithLast deficit arr