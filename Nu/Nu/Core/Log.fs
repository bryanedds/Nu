// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open Prime
open System.Collections.Concurrent

[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false
#if DEBUG
    let mutable private InfoOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private WarnOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private DebugOnceMessages = ConcurrentDictionary StringComparer.Ordinal
#endif
    let mutable private TraceOnceMessages = ConcurrentDictionary StringComparer.Ordinal

    let private getDateTimeNowStr () =
        let now = DateTimeOffset.Now
        now.ToString "yyyy-MM-dd HH\:mm\:ss.fff zzz"

    /// Log a remark with a custom header with Trace.WriteLine.
    /// Thread-safe.
    let remark header message =
        Trace.WriteLine (getDateTimeNowStr () + "|" + header + "|" + message)

    /// Log a purely informational message with Trace.WriteLine.
    /// Thread-safe.
    let info message =
        remark "Info" message

    /// Log a purely informational message once with Trace.WriteLine.
    /// Thread-safe.
    let infoOnce (message : string) =
#if DEBUG
        if InfoOnceMessages.TryAdd (message, 0) then info message
#else
        ignore message
#endif

    /// Log a warning message with Trace.WriteLine.
    /// Thread-safe.
    let warn message =
        remark "Warning" message

    /// Log a warning message once with Trace.WriteLine.
    /// Thread-safe.
    let warnOnce (message : string) =
#if DEBUG
        if WarnOnceMessages.TryAdd (message, 0) then warn message
#else
        ignore message
#endif

    /// Log a debug message with Debug.Fail and call to info.
    /// Thread-safe.
    let debug (message : string) =
#if DEBUG
        Debug.Fail (getDateTimeNowStr () + "|Debug|" + message)
#else
        ignore message
#endif

    /// Log a debug message once with Debug.Fail and call to info.
    /// Thread-safe.
    let debugOnce (message : string) =
#if DEBUG
        if DebugOnceMessages.TryAdd (message, 0) then debug message
#else
        ignore message
#endif

    /// Conditional debug message call where condition is lazily evaluated.
    /// Thread-safe.
    let debugIf (predicate : unit -> bool) (message : string) =
#if DEBUG
        if predicate () then debug message
#else
        (predicate, message) |> ignore
#endif

    /// Log a trace message using Trace.Fail and call to info.
    /// Thread-safe.
    let trace message =
        Trace.Fail (getDateTimeNowStr () + "|Trace|" + message)

    /// Log a trace message once with Trace.Fail and call to info.
    /// Thread-safe.
    let traceOnce (message : string) =
        if TraceOnceMessages.TryAdd (message, 0) then trace message

    /// Conditional trace message call where condition is eagerly evaluted.
    /// Thread-safe.
    let traceIf bl message =
        if bl then trace message

    /// Initialize logging.
    let init (fileNameOpt : string option) =

        // init only once
        if not Initialized then

            // add listener
            let listeners = Trace.Listeners
            listeners.Add (new TextWriterTraceListener (Console.Out)) |> ignore
            match fileNameOpt with
            | Some fileName -> listeners.Add (new TextWriterTraceListener (fileName)) |> ignore
            | None -> ()

            // automatically flush logs
            Trace.AutoFlush <- true

            // mark as Initialized
            Initialized <- true