// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Diagnostics
#if !PLATFORM_AGNOSTIC_TIMESTAMPING
open System.Runtime.InteropServices
#endif
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
        Stopwatch.GetTimestamp ()
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

    /// Get a unique time stamp, spinning until the time stamp advances if need be.
    let getUniqueTimeStamp () =
        let mutable nextStamp = getTimeStamp ()
        while nextStamp = lastStamp do nextStamp <- getTimeStamp ()
        lastStamp <- nextStamp
        nextStamp

[<AutoOpen>]
module CoreOperators =

    /// The implicit conversion operator.
    /// Same as the (!!) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (!!) (arg : ^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) arg)

    /// Sequences two functions like Haskell ($).
    /// Same as the ($) operator found in Prime, but placed here to expose it directly from Nu.
    let inline ($) f g = f g

/// Denotes that a value should not be altered by the consumer of the API.
/// TODO: remove this after updating Prime.
type [<AttributeUsage (AttributeTargets.Field)>] UniformAttribute () =
    inherit Attribute ()

/// The desired frame rate.
type [<NoComparison>] DesiredFps =
    | Fps60
    | Fps30

// TODO: remove after updating Prime.
[<RequireQualifiedAccess>]
module Array =

    /// Foldi for arrays.
    let foldi folder state (array : _ array) =
        Seq.foldi folder state array