// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Threading

/// Redirects native C/C++ stdout and stderr output to the .NET logging system.
/// This is necessary because native libraries (SDL, Jolt Physics, etc.) use
/// printf/cout which write to native file descriptors and bypass .NET Console.
[<RequireQualifiedAccess>]
module NativeOutputRedirect =

    // Platform detection
    let private isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
    let private isLinux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
    let private isOSX = RuntimeInformation.IsOSPlatform(OSPlatform.OSX)

    // Native interop for Unix-like systems (Linux, macOS)
    [<DllImport("libc", SetLastError = true)>]
    extern int pipe(int[] pipefd)

    [<DllImport("libc", SetLastError = true)>]
    extern int dup2(int oldfd, int newfd)

    [<DllImport("libc", SetLastError = true)>]
    extern int close(int fd)

    [<DllImport("libc", SetLastError = true)>]
    extern nativeint read(int fd, byte[] buf, nativeint count)

    // Native interop for Windows
    [<DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = true)>]
    extern int _pipe(int[] pipefd, uint pipesize, int textmode)

    [<DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = true)>]
    extern int _dup2(int fd1, int fd2)

    [<DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = true)>]
    extern int _close(int fd)

    [<DllImport("msvcrt.dll", CallingConvention = CallingConvention.Cdecl, SetLastError = true)>]
    extern int _read(int fd, byte[] buffer, uint count)

    // File descriptors for stdout and stderr
    let private STDOUT_FILENO = 1
    let private STDERR_FILENO = 2
    let private O_BINARY = 0x8000
    let private PIPE_SIZE = 65536u

    // Mutable state for tracking initialization and threads
    let mutable private Initialized = false
    let mutable private StdoutThread : Thread option = None
    let mutable private StderrThread : Thread option = None
    let mutable private StopRequested = false

    /// Creates a pipe and redirects a file descriptor to it.
    /// Returns the read end of the pipe.
    let private createAndRedirectPipe (fdToRedirect : int) =
        try
            if isWindows then
                // Windows implementation
                let pipefd = Array.zeroCreate 2
                let result = _pipe(pipefd, PIPE_SIZE, O_BINARY)
                if result = 0 then
                    let readFd = pipefd.[0]
                    let writeFd = pipefd.[1]
                    
                    // Redirect fdToRedirect to write end of pipe
                    if _dup2(writeFd, fdToRedirect) = -1 then
                        _close(readFd) |> ignore
                        _close(writeFd) |> ignore
                        None
                    else
                        // Close original write end as it's been duplicated
                        _close(writeFd) |> ignore
                        Some readFd
                else
                    None
            elif isLinux || isOSX then
                // Unix implementation
                let pipefd = Array.zeroCreate 2
                let result = pipe(pipefd)
                if result = 0 then
                    let readFd = pipefd.[0]
                    let writeFd = pipefd.[1]
                    
                    // Redirect fdToRedirect to write end of pipe
                    if dup2(writeFd, fdToRedirect) = -1 then
                        close(readFd) |> ignore
                        close(writeFd) |> ignore
                        None
                    else
                        // Close original write end as it's been duplicated
                        close(writeFd) |> ignore
                        Some readFd
                else
                    None
            else
                // Unsupported platform
                None
        with
        | ex ->
            Log.warn ("Failed to create pipe for native output redirection: " + ex.Message)
            None

    /// Reads from a pipe file descriptor and logs the output.
    let private readPipeAndLog (readFd : int) (logFunc : string -> unit) (streamName : string) =
        let buffer = Array.zeroCreate<byte> 4096
        let sb = StringBuilder()
        
        try
            while not StopRequested do
                try
                    let bytesRead =
                        if isWindows then
                            _read(readFd, buffer, uint buffer.Length)
                        else
                            int (read(readFd, buffer, nativeint buffer.Length))
                    
                    if bytesRead > 0 then
                        // Convert bytes to string
                        let text = Encoding.UTF8.GetString(buffer, 0, bytesRead)
                        sb.Append(text) |> ignore
                        
                        // Process complete lines
                        let mutable content = sb.ToString()
                        let lines = content.Split([|'\n'|], StringSplitOptions.None)
                        
                        // Log all complete lines (all but the last, which may be incomplete)
                        for i = 0 to lines.Length - 2 do
                            let line = lines.[i].TrimEnd('\r')
                            if not (String.IsNullOrWhiteSpace(line)) then
                                logFunc (streamName + ": " + line)
                        
                        // Keep the last incomplete line in the buffer
                        sb.Clear() |> ignore
                        if lines.Length > 0 then
                            sb.Append(lines.[lines.Length - 1]) |> ignore
                    elif bytesRead = 0 then
                        // EOF reached
                        Thread.Sleep(100)
                    else
                        // Error
                        Thread.Sleep(100)
                with
                | :? ThreadInterruptedException -> ()
                | ex ->
                    Log.warn ("Error reading from " + streamName + " pipe: " + ex.Message)
                    Thread.Sleep(100)
            
            // Log any remaining content in the buffer
            let remaining = sb.ToString().TrimEnd('\r', '\n')
            if not (String.IsNullOrWhiteSpace(remaining)) then
                logFunc (streamName + ": " + remaining)
        finally
            // Clean up file descriptor
            if isWindows then
                _close(readFd) |> ignore
            else
                close(readFd) |> ignore

    /// Initialize native output redirection.
    /// This should be called after Log.init in the Nu initialization process.
    let init () =
        if not Initialized then
            try
                // Only attempt redirection on supported platforms
                if isWindows || isLinux || isOSX then
                    // Redirect stdout
                    match createAndRedirectPipe STDOUT_FILENO with
                    | Some readFd ->
                        let thread = Thread(fun () -> readPipeAndLog readFd Log.info "Native stdout")
                        thread.IsBackground <- true
                        thread.Name <- "NativeStdoutReader"
                        thread.Start()
                        StdoutThread <- Some thread
                        Log.info "Native stdout redirection initialized."
                    | None ->
                        Log.warnOnce "Failed to redirect native stdout."
                    
                    // Redirect stderr
                    match createAndRedirectPipe STDERR_FILENO with
                    | Some readFd ->
                        let thread = Thread(fun () -> readPipeAndLog readFd Log.error "Native stderr")
                        thread.IsBackground <- true
                        thread.Name <- "NativeStderrReader"
                        thread.Start()
                        StderrThread <- Some thread
                        Log.info "Native stderr redirection initialized."
                    | None ->
                        Log.warnOnce "Failed to redirect native stderr."
                else
                    Log.infoOnce "Native output redirection not supported on this platform."
                
                Initialized <- true
            with
            | ex ->
                Log.warnOnce ("Failed to initialize native output redirection: " + ex.Message)

    /// Shutdown native output redirection.
    let shutdown () =
        if Initialized then
            StopRequested <- true
            
            // Wait for threads to finish (with timeout)
            match StdoutThread with
            | Some thread -> 
                if not (thread.Join(1000)) then
                    thread.Interrupt()
            | None -> ()
            
            match StderrThread with
            | Some thread -> 
                if not (thread.Join(1000)) then
                    thread.Interrupt()
            | None -> ()
            
            StdoutThread <- None
            StderrThread <- None
            Initialized <- false
