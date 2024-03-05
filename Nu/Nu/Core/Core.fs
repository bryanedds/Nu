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

/// An struct representation of a pair.
/// TODO: removed this after updating Prime.
type [<Struct>] StructPair<'a, 'b> =
    { Fst : 'a; Snd : 'b }
    static member make<'a, 'b> (fst : 'a) (snd : 'b) =
        { Fst = fst; Snd = snd }