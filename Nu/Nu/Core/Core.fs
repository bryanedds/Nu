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

// TODO: remove after updating Prime.
[<AutoOpen>]
module Operators =

    /// Inspect two options for equality.
    let inline optEq aOpt bOpt =
        match aOpt with
        | Some a -> (match bOpt with Some b -> a = b | None -> false)
        | None -> (match bOpt with Some _ -> false | None -> true)

    /// Inspect two options for inequality.
    let inline optNeq aOpt bOpt =
        not (optEq aOpt bOpt)

    /// Inspect two voptions for equality.
    let inline voptEq aOpt bOpt =
        match aOpt with
        | ValueSome a -> (match bOpt with ValueSome b -> a = b | ValueNone -> false)
        | ValueNone -> (match bOpt with ValueSome _ -> false | ValueNone -> true)

    /// Inspect two voptions for inequality.
    let inline voptNeq aOpt bOpt =
        not (voptEq aOpt bOpt)