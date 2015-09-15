// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2015.

namespace Prime
open System
open System.Diagnostics

[<AutoOpen>]
module Log =

    let mutable private initialized =
        false

    let private getUtcNowStr () =
        let now = DateTime.UtcNow
        now.ToString "yyyy-MM-dd HH\:mm\:ss.ffff"

    /// Log a message with a Trace.WriteLine.
    let note message =
        Trace.WriteLine (getUtcNowStr () + "|Note|" + message)

    /// Log a message with Debug.Fail and call to note.
    let debug message =
#if DEBUG
        Debug.Fail (getUtcNowStr () + "|Debug|" + message)
#else
        ignore message
#endif

    /// Conditional debug call where condition is lazily evaluated.
    let debugIf predicate message =
#if DEBUG
        if predicate () then debug message
#else
        ignore (predicate, message)
#endif

    /// Log a message with a Trace.Fail and call to note.
    let trace message =
        Trace.Fail (getUtcNowStr () + "|Trace|" + message)

    /// Conditional trace call where condition is eagerly evaluted.
    let traceIf bl message =
        if bl then trace message

    /// Initialize logging.
    let init (optFileName : string option) =

        // init only once
        if not initialized then

            // add listeners
            let listeners =
#if DEBUG
                Debug.Listeners
#else
                Trace.Listeners
#endif
            listeners.Add (new TextWriterTraceListener (Console.Out)) |> ignore
            match optFileName with
            | Some fileName -> listeners.Add (new TextWriterTraceListener (fileName)) |> ignore
            | None -> ()

            // automatically flush all logs
            Debug.AutoFlush <- true
            Trace.AutoFlush <- true

            // mark as initialized
            initialized <- true