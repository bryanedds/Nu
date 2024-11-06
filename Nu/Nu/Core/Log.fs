
// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Concurrent
open System.Diagnostics
open Prime

[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false
    let mutable private InfoOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private WarnOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private ErrorOnceMessages = ConcurrentDictionary StringComparer.Ordinal

    let private getDateTimeNowStr () =
        let now = DateTimeOffset.Now
        now.ToString "yyyy-MM-dd HH\:mm\:ss.fff zzz"

    /// Log a purely informational message with Trace.TraceInformation.
    /// Thread-safe.
    let info message =
        Trace.WriteLine (getDateTimeNowStr () + "|Info|" + message)

    /// Log a purely informational message once with Trace.WriteLine.
    /// Thread-safe.
    let infoOnce (message : string) =
        if InfoOnceMessages.TryAdd (message, 0) then info message

    /// Log a warning message with Trace.TraceWarning.
    /// Thread-safe.
    let warn message =
        Trace.WriteLine (getDateTimeNowStr () + "|Warning|" + message)

    /// Log a warning message once with Trace.WriteLine.
    /// Thread-safe.
    let warnOnce (message : string) =
        if WarnOnceMessages.TryAdd (message, 0) then warn message

    /// Log an error message with Trace.TraceError.
    /// Thread-safe.
    let error message =
        Trace.WriteLine (getDateTimeNowStr () + "|Error|" + message)

    /// Log an error message once with Trace.WriteLine.
    /// Thread-safe.
    let errorOnce (message : string) =
        if ErrorOnceMessages.TryAdd (message, 0) then warn message

    /// Log a failure message using Trace.Fail.
    /// Thread-safe.
    let fail message =
        Trace.Fail (getDateTimeNowStr () + "|Fatal|" + message)
        failwith "Log.fail exception."

    /// Log an custom log type with Trace.TraceInformation.
    /// Thread-safe.
    let custom header message =
        Trace.WriteLine (getDateTimeNowStr () + "|" + header + "|" + message)

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