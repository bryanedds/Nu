// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open Prime

[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false
#if DEBUG
    let mutable private InfoOnceMessages = hashSetPlus StringComparer.Ordinal []
    let mutable private DebugOnceMessages = hashSetPlus StringComparer.Ordinal []
#endif
    let mutable private TraceOnceMessages = hashSetPlus StringComparer.Ordinal []

    let private getDateTimeNowStr () =
        let now = DateTimeOffset.Now
        now.ToString "yyyy-MM-dd HH\:mm\:ss.fff zzz"

    /// Log a remark with a custom header with Trace.WriteLine.
    let remark header message =
        Trace.WriteLine (getDateTimeNowStr () + "|" + header + "|" + message)

    /// Log a purely informational message with Trace.WriteLine.
    let info message =
        remark "Info" message

    /// Log a purely informational message once with Trace.WriteLine.
    let infoOnce (message : string) =
#if DEBUG
        if InfoOnceMessages.Add message then info message
#else
        ignore message
#endif

    /// Log a debug message with Debug.Fail and call to info.
    let debug (message : string) =
#if DEBUG
        Debug.Fail (getDateTimeNowStr () + "|Debug|" + message)
#else
        ignore message
#endif

    /// Log a debug message once with Debug.Fail and call to info.
    let debugOnce (message : string) =
#if DEBUG
        if DebugOnceMessages.Add message then debug message
#else
        ignore message
#endif

    /// Conditional debug message call where condition is lazily evaluated.
    let debugIf (predicate : unit -> bool) (message : string) =
#if DEBUG
        if predicate () then debug message
#else
        (predicate, message) |> ignore
#endif

    /// Log a trace message using Trace.Fail and call to info.
    let trace message =
        Trace.Fail (getDateTimeNowStr () + "|Trace|" + message)

    /// Log a trace message once with Trace.Fail and call to info.
    let traceOnce (message : string) =
        if TraceOnceMessages.Add message then trace message

    /// Conditional trace message call where condition is eagerly evaluted.
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