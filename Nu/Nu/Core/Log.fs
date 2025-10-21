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
open System.IO
open System.Runtime.InteropServices
open System.Text
open Nu

/// The synchronized global logging API.
[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false
    let mutable private InfoOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private WarnOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let mutable private ErrorOnceMessages = ConcurrentDictionary StringComparer.Ordinal
    let private ConsoleListenerName = "Console.Out"
    let private LogFileListenerName = "LogFile"

    let private getDateTimeNowStr () =
        let now = DateTimeOffset.Now
        now.ToString "yyyy-MM-dd HH\:mm\:ss.fff zzz"

    /// Custom TextWriter that redirects output to the Nu logging system.
    type private LogTextWriter (logFunc : string -> unit) =
        inherit TextWriter ()

        let buffer = StringBuilder ()

        override _.Encoding = Encoding.UTF8

        override this.Write (value : char) =
            if value = '\n' then
                let message = buffer.ToString ()
                if not (String.IsNullOrWhiteSpace message) then
                    logFunc (message.TrimEnd ('\r'))
                buffer.Clear () |> ignore
            else
                buffer.Append value |> ignore

        override this.Write (value : string) =
            if not (isNull value) then
                for c in value do
                    this.Write c

        override this.WriteLine (value : string) =
            if not (isNull value) then
                this.Write value
            this.Write '\n'

        override _.Flush () =
            if buffer.Length > 0 then
                let message = buffer.ToString ()
                if not (String.IsNullOrWhiteSpace message) then
                    logFunc (message.TrimEnd ('\r', '\n'))
                buffer.Clear () |> ignore



    /// Configure synchronous logging.
    /// Because logging is initialized _before_ Configure.fromAppConfig is called, this procedure is provided in order
    /// configure synchronous logging _after_ logging initialization.
    let setLogSynchronously value =
        let listeners = Trace.Listeners
        listeners.Remove ConsoleListenerName |> ignore
        if value then listeners.Add (new TextWriterTraceListener (Console.Out, ConsoleListenerName)) |> ignore
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

    /// Platform-specific interop for redirecting native stdout/stderr
    [<RequireQualifiedAccess>]
    module private NativeRedirect =

        [<DllImport("libc", SetLastError = true)>]
        extern int dup (int oldfd)

        [<DllImport("libc", SetLastError = true)>]
        extern int dup2 (int oldfd, int newfd)

        [<DllImport("libc", SetLastError = true)>]
        extern int pipe (int[] pipefd)

        [<DllImport("libc", SetLastError = true)>]
        extern int close (int fd)

        let STDOUT_FILENO = 1
        let STDERR_FILENO = 2

        /// Start a background thread to read from a pipe and log the output
        let startPipeReader (pipeReadFd : int) (logFunc : string -> unit) =
            let bufferSize = 4096
            let task = System.Threading.Tasks.Task.Run (fun () ->
                try
                    use fileStream = new FileStream (new Microsoft.Win32.SafeHandles.SafeFileHandle (nativeint pipeReadFd, true), FileAccess.Read, bufferSize)
                    use reader = new StreamReader (fileStream, Encoding.UTF8)
                    let mutable line = reader.ReadLine ()
                    while not (isNull line) do
                        if not (String.IsNullOrWhiteSpace line) then
                            logFunc line
                        line <- reader.ReadLine ()
                with
                | :? IOException -> () // Pipe closed, exit gracefully
                | ex -> 
                    try
                        logFunc (sprintf "NativeRedirect pipe reader exception: %s" ex.Message)
                    with _ -> ())
            task

        /// Redirect native stdout/stderr on Unix-like systems
        let tryRedirectUnix (stdoutLogFunc : string -> unit) (stderrLogFunc : string -> unit) =
            try
                // Create pipes for stdout and stderr
                let stdoutPipe = Array.zeroCreate<int> 2
                let stderrPipe = Array.zeroCreate<int> 2

                if pipe stdoutPipe = 0 && pipe stderrPipe = 0 then
                    // Save original stdout and stderr (currently unused, but kept for potential future restoration needs)
                    let _stdoutBackup = dup STDOUT_FILENO
                    let _stderrBackup = dup STDERR_FILENO

                    // Redirect stdout and stderr to pipe write ends
                    dup2 (stdoutPipe.[1], STDOUT_FILENO) |> ignore
                    dup2 (stderrPipe.[1], STDERR_FILENO) |> ignore

                    // Close write ends in this process (they're duplicated to stdout/stderr)
                    close (stdoutPipe.[1]) |> ignore
                    close (stderrPipe.[1]) |> ignore

                    // Start reader threads for both pipes
                    let _stdoutTask = startPipeReader stdoutPipe.[0] stdoutLogFunc
                    let _stderrTask = startPipeReader stderrPipe.[0] stderrLogFunc

                    true
                else
                    false
            with
            | _ -> false

        /// Attempt to redirect native stdout/stderr to the logging system
        let tryRedirect (stdoutLogFunc : string -> unit) (stderrLogFunc : string -> unit) =
            try
                if RuntimeInformation.IsOSPlatform OSPlatform.Linux || 
                   RuntimeInformation.IsOSPlatform OSPlatform.OSX then
                    tryRedirectUnix stdoutLogFunc stderrLogFunc
                else
                    // On Windows and other platforms, Console.SetOut/SetError should handle most cases
                    false
            with
            | _ -> false

    /// Initialize logging.
    let init (fileNameOpt : string option) =

        // init only once
        if not Initialized then

            // add file listener
            match fileNameOpt with
            | Some fileName ->
                let listeners = Trace.Listeners
                listeners.Add (new TextWriterTraceListener (fileName, LogFileListenerName)) |> ignore
            | None -> ()

            // redirect managed Console.Out and Console.Error to log
            let stdoutWriter = new LogTextWriter (fun msg -> custom "Native" msg)
            let stderrWriter = new LogTextWriter (fun msg -> error ("Native: " + msg))
            Console.SetOut stdoutWriter
            Console.SetError stderrWriter

            // attempt to redirect native stdout/stderr
            let nativeRedirected = 
                NativeRedirect.tryRedirect 
                    (fun msg -> custom "Native" msg)
                    (fun msg -> error ("Native: " + msg))
            if nativeRedirected then
                info "Native stdout/stderr redirection enabled."
            else
                info "Native stdout/stderr redirection not available on this platform (using managed Console redirection only)."

            // explicitly flush on unhandled exceptions and process exit
            AppDomain.CurrentDomain.UnhandledException.AddHandler (fun _ _ -> flush (); stdoutWriter.Flush (); stderrWriter.Flush ())
            AppDomain.CurrentDomain.ProcessExit.AddHandler (fun _ _ -> flush (); stdoutWriter.Flush (); stderrWriter.Flush ())

            // configure synchronous logging
            setLogSynchronously Globals.Log.LogSynchronously

            // mark as Initialized
            Initialized <- true