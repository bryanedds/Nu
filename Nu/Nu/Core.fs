// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Runtime.InteropServices
open System.Configuration
open Prime

[<RequireQualifiedAccess>]
module internal CoreInternal =

// NOTE: the drawbacks of using PLATFORM_AGNOSTIC_TIMESTAMPING are two-fold -
//  1) Stopwatch.GetTimestamp is too low resolution on Windows .NET.
//  2) Even on Mono, Stopwatch.GetTimestamp, which is ultimately implemented in terms of
//     mono_100ns_ticks as defined here - https://github.com/mono/mono/blob/master/mono/utils/mono-time.c
//     does not necessarily lead to a high-resolution, linear-time path on all platforms.
#if PLATFORM_AGNOSTIC_TIMESTAMPING
    /// Get a time stamp at the highest-available resolution on linux.
    let internal getTimeStampInternal () =
        System.Diagnostics.Stopwatch.GetTimestamp ()
#else
    /// Query the windows performance counter.
    [<DllImport "Kernel32.dll">]
    extern bool private QueryPerformanceCounter (int64&)

    /// Get a time stamp at the highest-available resolution on windows.
    let internal getTimeStampInternal () =
        let mutable t = 0L
        QueryPerformanceCounter &t |> ignore
        t
#endif

[<RequireQualifiedAccess>]
module Core =

    let mutable private lastStamp =
        CoreInternal.getTimeStampInternal ()

    /// Get a time stamp at the highest-available resolution.
    let getTimeStamp () =
        CoreInternal.getTimeStampInternal ()

    /// Get a unique time stamp, spin until the time stamp advances if need be.
    let getUniqueTimeStamp () =
        let mutable nextStamp = getTimeStamp ()
        while nextStamp = lastStamp do
            nextStamp <- getTimeStamp ()
        lastStamp <- nextStamp
        nextStamp

    /// Get a resolution along either an X or Y dimension.
    let getResolutionOrDefault isX defaultResolution =
        match ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"] with
        | null -> defaultResolution
        | resolution -> scvalue<int> resolution

    /// Check that events should be trace as specified in the App.config file.
    let getEventTracing () =
        match ConfigurationManager.AppSettings.["EventTracing"] with
        | null -> false
        | eventTracing -> scvalue<bool> eventTracing

    /// Get the event filter as specified in the App.config file.
    let getEventFilter () =
        match ConfigurationManager.AppSettings.["EventFilter"] with
        | null -> EventFilter.Empty
        | eventFilter -> scvalue<EventFilter.Filter> eventFilter

[<AutoOpen>]
module CoreOperators =

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let ($) f g = f g

    /// The implicit conversion operator.
    /// Same as the (!!) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (!!) (arg : ^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) arg)

// TODO: move this into prime.
module Lens =

    let name<'a, 'w> (lens : Lens<'a, 'w>) =
        lens.Name

    let get<'a, 'w> world (lens : Lens<'a, 'w>) =
        lens.Get world

    let indexOut i (lens : Lens<'a seq, 'w>) : Lens<'a option, 'w> =
        lens.MapOut (Seq.tryItem i)

    let explodeOut (lens : Lens<'a seq, 'w>) : Lens<'a option, 'w> seq =
        Seq.initInfinite id |>
        Seq.map (flip indexOut lens)

    let dereferenceOut (lens : Lens<'a option, 'w>) : Lens<'a, 'w> =
        lens.MapOut (Option.get)

/// Specifies the screen-clearing routine.
type ScreenClear =
    | NoClear
    | ColorClear of byte * byte * byte