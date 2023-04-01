// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics

[<RequireQualifiedAccess>]
module Core =

    let mutable private LastTimeStamp = Stopwatch.GetTimestamp ()

    /// Get a time stamp at the highest-available resolution.
    let getTimeStamp () =
        Stopwatch.GetTimestamp ()

    /// Get a unique time stamp, spinning until the time stamp advances if need be.
    let getTimeStampUnique () =
        let mutable nextStamp = getTimeStamp ()
        while nextStamp = LastTimeStamp do nextStamp <- getTimeStamp ()
        LastTimeStamp <- nextStamp
        nextStamp

[<AutoOpen>]
module CoreOperators =

    /// The implicit conversion operator.
    /// Same as the (!!) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (!!) (arg : ^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) arg)

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let inline ($) f g = f g

[<RequireQualifiedAccess>]
module ValueOption =

    /// Convert an Option to a ValueOption.
    let ofOption<'a> (opt : 'a option) =
        match opt with
        | Some a -> ValueSome a
        | None -> ValueNone

    /// Convert a ValueOption to an Option.
    let toOption<'a> (opt : 'a voption) =
        match opt with
        | ValueSome a -> Some a
        | ValueNone -> None

[<RequireQualifiedAccess>]
module Option =

    /// Convert a ValueOption to an Option.
    let ofValueOption<'a> (opt : 'a voption) =
        match opt with
        | ValueSome a -> Some a
        | ValueNone -> None

    /// Convert an Option to a ValueOption.
    let toOption<'a> (opt : 'a option) =
        match opt with
        | Some a -> ValueSome a
        | None -> ValueNone