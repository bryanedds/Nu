// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open SDL
open Prime

/// Describes the initial configuration of a window created via SDL.
type SdlWindowConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowFlags : SDL_WindowFlags }

    /// A default SdlWindowConfig.
    static member val defaultConfig =
        { WindowTitle = "Nu Game"
          WindowX = int SDL3.SDL_WINDOWPOS_UNDEFINED
          WindowY = int SDL3.SDL_WINDOWPOS_UNDEFINED
          WindowFlags = SDL_WindowFlags.SDL_WINDOW_RESIZABLE ||| SDL_WindowFlags.SDL_WINDOW_VULKAN }

/// Describes the general configuration of SDL.
type [<ReferenceEquality>] SdlConfig =
    { WindowConfig : SdlWindowConfig }

    /// A default SdlConfig.
    static member val defaultConfig =
        { WindowConfig = SdlWindowConfig.defaultConfig }

[<RequireQualifiedAccess>]
module SdlEvents =

    let private PolledEvents = Queue ()

    /// Accumulate SDL events. Necessary to call when you have a long-running process on the main thread to keep OS's
    /// like Windows from eco-hanging the application when it sees user input not getting processed in a timely
    /// fashion.
    let poll () =
        let mutable polledEvent = SDL_Event ()
        while (SDL3.SDL_PollEvent &&polledEvent : bool) do
            PolledEvents.Enqueue polledEvent

    /// Attempt to consume an SDL event. Usually only the engine should call this, but there might be cases where the
    /// user needs to utilize it to cancel a long-running process or something.
    let tryConsume (event : SDL_Event outref) =
        PolledEvents.TryDequeue &event

[<RequireQualifiedAccess>]
module SdlDeps =

    /// The dependencies needed to initialize SDL.
    type [<ReferenceEquality>] SdlDeps =
        private
            { WindowOpt : SDL_Window nativeptr option
              Config : SdlConfig
              Destroy : unit -> unit }
    
        interface IDisposable with
            member this.Dispose () =
                this.Destroy ()

    type private LogOutputDelegate =
        delegate of nativeint * int * SDL_LogPriority * nativeptr<byte> -> unit

    let private sdlVersionToString version =
        string (SDL3.SDL_VERSIONNUM_MAJOR version) + "." +
        string (SDL3.SDL_VERSIONNUM_MINOR version) + "." +
        string (SDL3.SDL_VERSIONNUM_MICRO version)

    /// Attempt to initalize an SDL module.
    let private attemptPerformSdlInit create destroy =
        let initResult = create ()
        let error = SDL3.SDL_GetError ()
        if initResult
        then Right ((), destroy)
        else Left error

    /// Attempt to initalize an SDL resource.
    let private tryMakeSdlResource create destroy =
        let resource = create ()
        if NativePtr.isNullPtr resource
        then Left ("SDL3# resource creation failed due to '" + SDL3.SDL_GetError () + "'.")
        else Right (resource, destroy)

    /// Attempt to initalize a global SDL resource.
    let private tryMakeSdlGlobalResource create destroy =
        let resource : SDLBool = create ()
        if SDLBool.op_Implicit resource
        then Right ((), destroy)
        else Left ("SDL3# global resource creation failed due to '" + SDL3.SDL_GetError () + "'.")

    /// Get an sdlDep's optional window.
    let getWindowOpt sdlDeps =
        sdlDeps.WindowOpt

    /// Get an sdlDep's config.
    let getConfig sdlDeps =
        sdlDeps.Config

    /// Get the desktop display mode.
    let getDesktopDisplayMode () =
        let display = SDL3.SDL_GetPrimaryDisplay ()
        let displayMode = SDL3.SDL_GetDesktopDisplayMode display
        if NativePtr.isNullPtr displayMode then
            Log.error ("Failed to get desktop display mode: " + SDL3.SDL_GetError ())
            Unchecked.defaultof<_>
        else NativePtr.read displayMode

    /// Attempt to set the window's full screen state.
    let trySetWindowFullScreen fullScreen sdlDeps =
        match sdlDeps.WindowOpt with
        | Some window ->

            // get a snapshot of whether screen was full
            let mutable width, height = 0, 0
            SDL3.SDL_GetWindowSize (window, &&width, &&height) |> ignore<SDLBool>
            let displayMode = getDesktopDisplayMode ()
            let wasFullScreen = width = displayMode.w || height = displayMode.h

            // change full screen status via flags
            SDL3.SDL_SetWindowFullscreen (window, fullScreen) |> ignore<SDLBool>

            // when changing from full screen, set window to windowed size and make sure its title bar is visible
            if wasFullScreen && not fullScreen then
                let windowSizeWindowed = Constants.Render.DisplayVirtualResolution * 2
                SDL3.SDL_RestoreWindow window |> ignore<SDLBool>
                SDL3.SDL_SetWindowSize (window, windowSizeWindowed.X, windowSizeWindowed.Y) |> ignore<SDLBool>
                SDL3.SDL_SetWindowPosition (window, 100, 100) |> ignore<SDLBool>

        | None -> ()
        sdlDeps

    /// An empty SdlDeps.
    let empty =
        { WindowOpt = None
          Config = SdlConfig.defaultConfig
          Destroy = id }

    /// Attempt to make an SdlDeps instance.
    let tryMake sdlConfig accompanied (windowSize : Vector2i) =
        match attemptPerformSdlInit
            (fun () ->

                // setup SDL logging
                SDL3.SDL_SetLogOutputFunction
                    (Marshal.GetFunctionPointerForDelegate<LogOutputDelegate>(fun _ category priority message ->
                        let message = SDL3.PtrToStringUTF8 message
                        match priority with
                        | SDL_LogPriority.SDL_LOG_PRIORITY_VERBOSE
                        | SDL_LogPriority.SDL_LOG_PRIORITY_DEBUG
                        | SDL_LogPriority.SDL_LOG_PRIORITY_INFO -> Log.info (message + " (Category " + string category + ")")
                        | SDL_LogPriority.SDL_LOG_PRIORITY_WARN -> Log.warn (message + " (Category " + string category + ")")
                        | SDL_LogPriority.SDL_LOG_PRIORITY_ERROR -> Log.error (message + " (Category " + string category + ")")
                        | SDL_LogPriority.SDL_LOG_PRIORITY_CRITICAL -> Log.fail (message + " (Category " + string category + ")")
                        | _ -> ()),
                     0n)

                // attempt to initialize sdl
                Log.info "Initializing SDL 3..."
                SDL3.SDL_SetHint (SDL3.SDL_HINT_WINDOWS_CLOSE_ON_ALT_F4, "0") |> ignore<SDLBool>
                SDL3.SDL_SetHint (SDL3.SDL_HINT_ORIENTATIONS, "LandscapeLeft LandscapeRight") |> ignore<SDLBool>
                let initConfig =
                    SDL_InitFlags.SDL_INIT_AUDIO |||
                    SDL_InitFlags.SDL_INIT_VIDEO |||
                    SDL_InitFlags.SDL_INIT_JOYSTICK |||
                    SDL_InitFlags.SDL_INIT_HAPTIC |||
                    SDL_InitFlags.SDL_INIT_GAMEPAD |||
                    SDL_InitFlags.SDL_INIT_EVENTS
                SDL3.SDL_Init initConfig)

            (fun () -> SDL3.SDL_Quit ()) with
        | Left error -> Left error
        | Right ((), destroy) ->
            match tryMakeSdlResource
                (fun () ->

                    // attempt to create window
                    let windowConfig = sdlConfig.WindowConfig
                    let windowOpt = SDL3.SDL_CreateWindow (windowConfig.WindowTitle, windowSize.X, windowSize.Y, windowConfig.WindowFlags)
                    if not (NativePtr.isNullPtr windowOpt) then

                        // set window position
                        let window = windowOpt
                        SDL3.SDL_SetWindowPosition (window, windowConfig.WindowX, windowConfig.WindowY) |> ignore<SDLBool>

                        // start text input except on platforms that would obscure the game with a virtual keyboard
                        if not (SDL3.SDL_HasScreenKeyboardSupport ()) then
                            SDL3.SDL_StartTextInput window |> ignore<SDLBool>

                        // set to full screen when window taking up entire screen and unaccompanied
                        let mutable displayMode = getDesktopDisplayMode ()
                        if (windowSize.X = displayMode.w || windowSize.Y = displayMode.h) && not accompanied then
                            SDL3.SDL_SetWindowFullscreen (window, true) |> ignore<SDLBool>

                    // fin
                    windowOpt)

                (fun window -> SDL3.SDL_DestroyWindow window; destroy ()) with
            | Left error -> Left error
            | Right (window, destroy) ->
                match tryMakeSdlGlobalResource SDL3_ttf.TTF_Init (fun () -> SDL3_ttf.TTF_Quit (); destroy window) with
                | Left error -> Left error
                | Right ((), destroy) ->
                    match tryMakeSdlGlobalResource SDL3_mixer.MIX_Init (fun () -> SDL3_mixer.MIX_Quit (); destroy ()) with
                    | Left error -> Left error
                    | Right ((), destroy) ->
                        Log.info
                            ("Initialized SDL " + sdlVersionToString (SDL3.SDL_GetVersion ()) +
                             ", SDL_ttf " + sdlVersionToString (SDL3_ttf.TTF_Version ()) +
                             ", SDL_mixer " + sdlVersionToString (SDL3_mixer.MIX_Version ()) +
                             ", SDL_image " + sdlVersionToString (SDL3_image.IMG_Version ()) +
                             ".")
                        GamepadState.init ()
                        Right { WindowOpt = Some window; Config = sdlConfig; Destroy = destroy }

/// The dependencies needed to initialize SDL.
type SdlDeps = SdlDeps.SdlDeps