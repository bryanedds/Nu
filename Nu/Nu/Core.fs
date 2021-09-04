// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Diagnostics
#if !PLATFORM_AGNOSTIC_TIMESTAMPING
open System.Runtime.InteropServices
#endif
open System.Configuration
open Prime

/// Specifies the screen-clearing routine.
/// NOTE: this type is here only to make the screen clear constant defineable in Constants.fs.
type ScreenClear =
    | NoClear
    | ColorClear of byte * byte * byte

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

    /// Get a unique time stamp, spin until the time stamp advances if need be.
    let getUniqueTimeStamp () =
        let mutable nextStamp = getTimeStamp ()
        while nextStamp = lastStamp do nextStamp <- getTimeStamp ()
        lastStamp <- nextStamp
        nextStamp

    /// Get a resolution along either an X or Y dimension.
    let getVirtualScalarOrDefault defaultScalar =
        match ConfigurationManager.AppSettings.["VirtualScalar"] with
        | null -> defaultScalar
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

/// TODO: P1: remove the below code as soon as Prime is updated!
namespace Prime
open System
open Prime

/// Haskell-style Either type with value semantics.
type [<StructuralEquality; StructuralComparison; Struct>] ValueEither<'l, 'r> =
    | ValueRight of Right : 'r
    | ValueLeft of Left : 'l

/// Builds an either monad.
type ValueEitherBuilder () =
    member inline this.Return a = ValueRight a
    member inline this.ReturnFrom a = a
    member inline this.Bind (a, f) = match a with ValueRight r -> f r | ValueLeft l -> ValueLeft l
    member this.Using (d, b) = use u = d in b u
    member this.TryWith (b, h) = try b () with exn -> h exn
    member this.TryFinally (b, h) = try b () finally h ()
    member this.Delay f = f ()
    member this.Run f = f ()
    member this.Zero () = ValueRight ()
    member this.Yield a = ValueRight a
    member this.YieldFrom e = e
    member this.Combine (a, b) = this.Bind (a, b)

    member this.While (g, b) =
        if g ()
        then match b () with ValueRight () -> this.While (g, b) | error -> error
        else this.Zero ()

    member this.For (sequence : _ seq, body) =
        use enr = sequence.GetEnumerator ()
        let mutable errorOpt = None
        while enr.MoveNext () && Option.isNone errorOpt do
            match body enr.Current with
            | ValueRight () -> ()
            | left -> errorOpt <- Some left
        match errorOpt with
        | Some error -> error
        | None -> this.Zero ()

[<AutoOpen>]
module ValueEitherBuilder =

    /// Builds the either monad.
    let valueEither = ValueEitherBuilder ()

[<RequireQualifiedAccess>]
module ValueEither =

    /// Monadic return for ValueEither.
    let inline returnM a = valueEither.Return a

    /// Monadic 'return from' for ValueEither.
    let inline returnFrom a = valueEither.ReturnFrom a

    /// Monadic bind for ValueEither.
    let inline bind a f = valueEither.Bind (a, f)

    /// Check whether a ValueEither is ValueLeft.
    let isLeft eir =
        match eir with
        | ValueRight _ -> false
        | ValueLeft _ -> true
    
    /// Check whether a ValueEither is ValueRight.
    let isRight eir =
        match eir with
        | ValueRight _ -> true
        | ValueLeft _ -> false

    /// Get the ValueLeft of a ValueEither, failing if not available.
    let getLeft eir =
        match eir with
        | ValueRight _ -> failwith "Could not get ValueLeft value from a ValueRight value."
        | ValueLeft l -> l

    /// Get the ValueRight of a ValueEither, failing if not available.
    let getRight eir =
        match eir with
        | ValueRight r -> r
        | ValueLeft _ -> failwith "Could not get ValueRight value from a ValueLeft value."

    /// Get only the ValueLefts of a sequence of a ValueEithers.
    let getLefts eirs =
        List.foldBack
            (fun eir lefts -> match eir with ValueRight _ -> lefts | ValueLeft left -> left :: lefts)
            (List.ofSeq eirs)
            []

    /// Get only the ValueRights of a sequence of ValueEithers.
    let getRights eirs =
        List.foldBack
            (fun eir rights -> match eir with ValueRight right -> right :: rights | ValueLeft _ -> rights)
            (List.ofSeq eirs)
            []

    /// Map over the left side of a ValueEither.
    let mapLeft mapper eir =
        match eir with
        | ValueRight r -> ValueRight r
        | ValueLeft l -> ValueLeft (mapper l)

    /// Map over the right side of a ValueEither.
    let mapRight mapper eir =
        match eir with
        | ValueRight r -> ValueRight (mapper r)
        | ValueLeft l -> ValueLeft l

    /// Map both sides of a ValueEither.
    let map fnl fnr eir =
        eir |>
        mapLeft fnl |>
        mapRight fnr

    /// Split a sequence of ValueEithers into a pair of left and right lists.
    let split eirs =
        List.foldBack
            (fun eir (ls, rs) ->
                match eir with
                | ValueRight r -> (ls, r :: rs)
                | ValueLeft l -> (l :: ls, rs))
            (List.ofSeq eirs)
            ([], [])

    /// Pick whichever of the ValueEithers exists so long as they are the same type.
    let amb (eir : ValueEither<'a, 'a>) =
        match eir with
        | ValueRight value -> value
        | ValueLeft value -> value

    /// Pick whichever of the ValueEithers exists.
    let ambBy pickFst pickSnd (eir : ValueEither<'a, 'b>) =
        match eir with
        | ValueRight value -> pickFst value
        | ValueLeft value -> pickSnd value