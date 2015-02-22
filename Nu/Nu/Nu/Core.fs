// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Runtime.InteropServices
open System.Configuration
open Prime
open Nu

[<RequireQualifiedAccess>]
module internal CoreInternal =

#if LINUX
    // NOTE: this code has not been tested, and I have no idea if it works correctly, or even well!

    /// The linux representation of time.
    type internal timeval =
        struct
            val tv_sec : int
            val tv_usec : int
            end

    /// Query the linux gettimeofday system call.
    [<DllImport("libc")>]
    extern void private gettimeofday (timeval&, int64)
    
    /// Get a time stamp at the highest-available resolution on linux.
    let getTimeStampInternal () =
        let mutable t = timeval ()
        gettimeofday(&t, -1L);
        int64 t.tv_sec * 1000000L + int64 t.tv_usec
#else
    /// Query the windows performance counter.
    [<DllImport("Kernel32.dll")>]
    extern bool private QueryPerformanceCounter (int64&)

    /// Get a time stamp at the highest-available resolution on windows.
    let getTimeStampInternal () =
        let mutable t = 0L
        ignore <| QueryPerformanceCounter &t
        t
#endif

[<AutoOpen>]
module CoreModule =

    /// Specifies the screen-clearing routine.
    type ScreenClear =
        | NoClear
        | ColorClear of byte * byte * byte

    /// Specifies whether the engine is running or exiting.
    type Liveness =
        | Running
        | Exiting
        
    /// Sequences two functions like Haskell ($).
    /// Same as the (^) operator found in Prime, but placed here to expose it directly from Nu.
    let (^) = (^)

[<RequireQualifiedAccess>]
module Core =

    /// Make a Nu Id.
    let makeId () =
        Guid.NewGuid ()

    /// Get a time stamp at the highest-available resolution.
    let getTimeStamp () = CoreInternal.getTimeStampInternal ()

    /// Get a resolution along either an X or Y dimension.
    let getResolutionOrDefault isX defaultResolution =
        let appSetting = ConfigurationManager.AppSettings.["Resolution" + if isX then "X" else "Y"]
        match Int32.TryParse appSetting with
        | (true, resolution) -> resolution
        | (false, _) -> defaultResolution