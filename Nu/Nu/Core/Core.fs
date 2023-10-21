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

    /// Test for object equality.
    /// OPTIMIZATION: always tests reference equality first.
    /// TODO: remove this after updating Prime.
    let inline objEq (a : obj) (b : obj) =
        obj.ReferenceEquals (a, b) ||
        match a with
        | :? Array -> a = b // NOTE: arrays are given special deep equality semantics in F#.
        | _ -> obj.Equals (a, b)

    /// Test for object inequality.
    /// OPTIMIZATION: always tests reference equality first.
    /// TODO: remove this after updating Prime.
    let inline objNeq (a : obj) (b : obj) =
        not (obj.ReferenceEquals (a, b) ||
        match a with
        | :? Array -> a <> b // NOTE: arrays are given special deep equality semantics in F#.
        | _ -> obj.Equals (a, b))

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