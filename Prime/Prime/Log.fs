// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Prime
open System
open System.Diagnostics

[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized =
        false

    let private getUtcNowStr () =
        let now = DateTime.UtcNow
        now.ToString "yyyy-MM-dd HH\:mm\:ss.ffff"

    /// Log a remark with a custom header using Trace.WriteLine.
    let remark header message =
        Trace.WriteLine (getUtcNowStr () + "|" + header + "|" + message)

    /// Log a purely informational message using Trace.WriteLine.
    let info message =
        remark "Info" message

    /// Log a debug message with Debug.Fail and call to info.
    let debug (message : string) =
#if DEBUG
        Debug.Fail (getUtcNowStr () + "|Debug|" + message)
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
        Trace.Fail (getUtcNowStr () + "|Trace|" + message)

    /// Conditional trace message call where condition is eagerly evaluted.
    let traceIf bl message =
        if bl then trace message

    /// Initialize logging.
    let init (fileNameOpt : string option) =

        // init only once
        if not Initialized then

            // add listeners
            let listeners =
#if DEBUG
                Debug.Listeners
#else
                Trace.Listeners
#endif
            listeners.Add (new TextWriterTraceListener (Console.Out)) |> ignore
            match fileNameOpt with
            | Some fileName -> listeners.Add (new TextWriterTraceListener (fileName)) |> ignore
            | None -> ()

            // automatically flush all logs
            Debug.AutoFlush <- true
            Trace.AutoFlush <- true

            // mark as Initialized
            Initialized <- true