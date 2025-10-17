// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Globals
open System
open System.Configuration
open Prime

/// Global mutable log values. Change tracking must be done manually by dependant code.
[<RequireQualifiedAccess>]
module Log =

    /// The global mutable display scalar. This may be changed by the engine at run-time. However it should be changed
    /// only via Log.setLogSynchronously as that approach keeps Trace.AutoFlush in sync as well.
    let [<Uniform>] mutable LogSynchronously = match ConfigurationManager.AppSettings.["LogSynchronously"] with null -> true | value -> scvalue value

namespace Nu
open System
open System.Collections.Concurrent
open System.Diagnostics
open Nu

/// The synchronized global logging API.
[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false
    let mutable private InfoOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private WarnOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private ErrorOnceMessages = ConcurrentDictionary StringComparer.Ordinal

    let private getDateTimeNowStr () =
        let now = DateTimeOffset.Now
        now.ToString "yyyy-MM-dd HH\:mm\:ss.fff zzz"

    /// Configure synchronous logging.
    /// Because logging is initialized _before_ Configure.fromAppConfig is called, this procedure is provided in order
    /// configure synchronous logging _after_ logging initialization.
    let setLogSynchronously value =
        Trace.AutoFlush <- value
        Globals.Log.LogSynchronously <- value

    /// Flush all log output streams.
    /// Thread-safe.
    let flush () =
        Trace.Flush ()

    /// Log a purely informational message with Trace.WriteLine.
    /// Thread-safe.
    let info message =
        Trace.WriteLine (getDateTimeNowStr () + "|Info|" + message)

    /// Log a purely informational message once with Trace.WriteLine.
    /// Thread-safe.
    let infoOnce (message : string) =
        if InfoOnceMessages.TryAdd (message, 0) then info message

    /// Log a warning message with Trace.WriteLine.
    /// Thread-safe.
    let warn message =
        Trace.WriteLine (getDateTimeNowStr () + "|Warning|" + message)

    /// Log a warning message once with Trace.WriteLine.
    /// Thread-safe.
    let warnOnce (message : string) =
        if WarnOnceMessages.TryAdd (message, 0) then warn message

    /// Log an error message with Trace.WriteLine.
    /// Thread-safe.
    let error message =
        Trace.WriteLine (getDateTimeNowStr () + "|Error|" + message)

    /// Log an error message once with Trace.WriteLine.
    /// Thread-safe.
    let errorOnce (message : string) =
        if ErrorOnceMessages.TryAdd (message, 0) then error message

    /// Log a failure message using Trace.Fail.
    /// Thread-safe.
    let fail message =
        Trace.Fail (getDateTimeNowStr () + "|Fatal|" + message)
        failwith "Log.fail exception."

    /// Log an custom log type with Trace.WriteLine.
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

            // explicitly flush on unhandled exceptions and process exit
            AppDomain.CurrentDomain.UnhandledException.AddHandler (fun _ _ -> flush ())
            AppDomain.CurrentDomain.ProcessExit.AddHandler (fun _ _ -> flush ())

            // configure synchronous logging
            setLogSynchronously Globals.Log.LogSynchronously

            // mark as Initialized
            Initialized <- true