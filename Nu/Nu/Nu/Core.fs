// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Configuration
open Prime
open Nu

[<RequireQualifiedAccess>]
module internal CoreInternal =

#if PLATFORM_AGNOSTIC_TIMESTAMPING
    // NOTE: the risk of using PLATFORM_AGNOSTIC_TIMESTAMPING is two-fold -
    //  1) Stopwatch.GetTimestamp is too low resolution on Windows .NET.
    //  2) Even on Mono, Stopwatch.GetTimestamp, which is ultimately implemented in terms of
    //     mono_100ns_ticks as defined here - https://github.com/mono/mono/blob/master/mono/utils/mono-time.c
    //     does not necessarily lead to a high-resolution, linear-time path on all platforms.
    // Still, a programmer can always pray for his stuff to work in the practical cases...
    
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

/// Specifies the screen-clearing routine.
type ScreenClear =
    | NoClear
    | ColorClear of byte * byte * byte

/// Specifies whether the engine is running or exiting.
type Liveness =
    | Running
    | Exiting

[<AutoOpen>]
module CoreModule =

    /// Sequences two functions like Haskell ($).
    /// Same as the (^) operator found in Prime, but placed here to expose it directly from Nu.
    let inline (^) f g = f g

[<RequireQualifiedAccess>]
module Core =

    /// Make a Nu Id.
    let makeId () =
        Guid.NewGuid ()

    /// Get a time stamp at the highest-available resolution.
    let getTimeStamp () =
        CoreInternal.getTimeStampInternal ()

    /// Spin until the time stamp advances, taking the next time stamp.
    let getNextTimeStamp () =
        let currentStamp = getTimeStamp ()
        let mutable nextStamp = getTimeStamp ()
        while currentStamp = nextStamp do
            nextStamp <- getTimeStamp ()
        nextStamp

    /// Get a resolution along either an X or Y dimension.
    let getResolutionOrDefault isX defaultResolution =
        let appSetting = ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"]
        match Int32.TryParse appSetting with
        | (true, resolution) -> resolution
        | (false, _) -> defaultResolution