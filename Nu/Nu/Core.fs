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

/// Specifies the screen-clearing routine.
type ScreenClear =
    | NoClear
    | ColorClear of byte * byte * byte

[<RequireQualifiedAccess>]
module Core =

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

/// Allow for a pair of values to be partially-compared.
/// NOTE: this will be in the next version of Prime.
type [<Struct; CustomEquality; CustomComparison>] PartialComparable<'a, 'b when 'a : comparison> =
    { Comparable : 'a
      Noncomparable : 'b }

    static member equals left right =
        left.Comparable = right.Comparable

    static member compare left right =
        compare left.Comparable right.Comparable

    override this.GetHashCode () =
        hash this.Comparable

    override this.Equals that =
        match that with
        | :? PartialComparable<'a, 'b> as that -> PartialComparable<'a, 'b>.equals this that
        | _ -> failwithumf ()

    interface PartialComparable<'a, 'b> IComparable with
        member this.CompareTo that =
            PartialComparable<'a, 'b>.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? PartialComparable<'a, 'b> as that -> PartialComparable<'a, 'b>.compare this that
            | _ -> failwith "Invalid PartialComparable comparison (comparee not of type PartialComparable)."

/// PartialComparable functions.
module PartialComparable =

    let make comparable noncomparable =
        { Comparable = comparable
          Noncomparable = noncomparable }

    let unmake partialCompare =
        (partialCompare.Comparable, partialCompare.Noncomparable)