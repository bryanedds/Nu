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

    // TODO: remove this once Prime is updated.
    let inline bind source signal : Binding<'m, 'c, 's, 'w> = source => signal

    // TODO: remove this once Prime is updated.
    let inline react source signal : Binding<'m, 'c, 's, 'w> = source =|> signal

[<AutoOpen>]
module Gen =

    let mutable private Counter = -1L
    let private CounterLock = obj ()

    /// Generates values on-demand.
    type Gen =
        private | Gen of unit

        /// Generate a unique counter.
        static member counter =
            lock CounterLock (fun () -> Counter <- inc Counter; Counter)

        /// The prefix of a generated name
        static member namePrefix =
            "@"

        /// Generate a unique name.
        static member name =
            Gen.namePrefix + scstring Gen.counter

        /// Generate a unique name if given none.
        static member nameIf nameOpt =
            match nameOpt with
            | Some name -> name
            | None -> Gen.name

        /// Check that a name is generated.
        static member isName (name : string) =
            name.StartsWith Gen.namePrefix

        /// Generate a unique id.
        static member id =
            Guid.NewGuid ()
        
        /// Generate an id from a couple of ints.
        /// It is the user's responsibility to ensure uniqueness when using the resulting ids.
        static member idFromInts m n =
            let bytes = Array.create<byte> 8 (byte 0)
            Guid (m, int16 (n >>> 16), int16 n, bytes)

        /// Generate an id deterministically.
        /// HACK: this is an ugly hack to create a deterministic sequance of guids.
        /// Limited to creating 65,536 guids.
        static member idDeterministic offset (guid : Guid) =
            let arr = guid.ToByteArray ()
            if arr.[15] + byte offset < arr.[15] then arr.[14] <- arr.[14] + byte 1
            arr.[15] <- arr.[15] + byte offset                    
            Guid arr

        /// Derive a unique id and name if given none.
        static member idAndNameIf nameOpt =
            let id = Gen.id
            let name = Gen.nameIf nameOpt
            (id, name)

/// Generates values on-demand.
type Gen = Gen.Gen

/// Specifies the screen-clearing routine.
type ScreenClear =
    | NoClear
    | ColorClear of byte * byte * byte