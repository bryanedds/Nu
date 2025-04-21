﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open SDL2
open Prime

/// A window for rendering in SDL OpenGL.
type [<ReferenceEquality>] SglWindow =
    { SglWindow : nativeint }

/// A window for rendering.
type [<ReferenceEquality>] Window =
    | SglWindow of SglWindow

/// Describes the initial configuration of a window created via SDL.
type SdlWindowConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowFlags : SDL.SDL_WindowFlags }

    /// A default SdlWindowConfig.
    static member defaultConfig =
        { WindowTitle = "Nu Game"
          WindowX = SDL.SDL_WINDOWPOS_UNDEFINED
          WindowY = SDL.SDL_WINDOWPOS_UNDEFINED
          WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE ||| SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL }

/// Describes the general configuration of SDL.
type [<ReferenceEquality>] SdlConfig =
    { WindowConfig : SdlWindowConfig
      AudioChunkSize : int }

    /// A default SdlConfig.
    static member defaultConfig =
        { WindowConfig = SdlWindowConfig.defaultConfig
          AudioChunkSize = Constants.Audio.BufferSizeDefault }

[<RequireQualifiedAccess>]
module SdlDeps =

    /// The dependencies needed to initialize SDL.
    type [<ReferenceEquality>] SdlDeps =
        private
            { WindowOpt : Window option
              Config : SdlConfig
              Destroy : unit -> unit }
    
        interface IDisposable with
            member this.Dispose () =
                this.Destroy ()

    /// An empty SdlDeps.
    let empty =
        { WindowOpt = None
          Config = SdlConfig.defaultConfig
          Destroy = id }

    /// Get an sdlDep's optional window.
    let getWindowOpt sdlDeps =
        sdlDeps.WindowOpt

    /// Get an sdlDep's config.
    let getConfig sdlDeps =
        sdlDeps.Config

    /// Attempt to set the window's full screen state.
    let trySetWindowFullScreen fullScreen sdlDeps =
        match sdlDeps.WindowOpt with
        | Some (SglWindow window) ->

            // get a snapshot of whether screen was full
            let (width, height) = (ref 0, ref 0)
            SDL.SDL_GetWindowSize (window.SglWindow, width, height) |> ignore
            let mutable displayMode = Unchecked.defaultof<_>
            SDL.SDL_GetDesktopDisplayMode (0, &displayMode) |> ignore<int>
            let wasFullScreen = width.Value = displayMode.w || height.Value = displayMode.h

            // change full screen status via flags
            let flags = if fullScreen then uint SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN_DESKTOP else 0u
            SDL.SDL_SetWindowFullscreen (window.SglWindow, flags) |> ignore

            // when changing from full screen, set window to windowed size and make sure its title bar is visible
            if wasFullScreen && not fullScreen then
                let windowSizeWindowed = Constants.Render.DisplayVirtualResolution * 2
                SDL.SDL_RestoreWindow window.SglWindow
                SDL.SDL_SetWindowSize (window.SglWindow, windowSizeWindowed.X, windowSizeWindowed.Y)
                SDL.SDL_SetWindowPosition (window.SglWindow, 100, 100)

        | _ -> ()
        sdlDeps

    /// Attempt to initalize an SDL module.
    let internal attemptPerformSdlInit create destroy =
        let initResult = create ()
        let error = SDL.SDL_GetError ()
        if initResult = 0
        then Right ((), destroy)
        else Left error

    /// Attempt to initalize an SDL resource.
    let internal tryMakeSdlResource create destroy =
        let resource = create ()
        if resource <> IntPtr.Zero
        then Right (resource, destroy)
        else Left ("SDL2# resource creation failed due to '" + SDL.SDL_GetError () + "'.")

    /// Attempt to initalize a global SDL resource.
    let internal tryMakeSdlGlobalResource create destroy =
        let resource = create ()
        if resource = 0
        then Right ((), destroy)
        else Left ("SDL2# global resource creation failed due to '" + SDL.SDL_GetError () + "'.")

    /// Attempt to make an SdlDeps instance.
    let tryMake sdlConfig accompanied (windowSize : Vector2i) =
        match attemptPerformSdlInit
            (fun () ->
                SDL.SDL_SetHint (SDL.SDL_HINT_VIDEO_HIGHDPI_DISABLED, "1") |> ignore<SDL.SDL_bool>
                SDL.SDL_SetHint (SDL.SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4, "1") |> ignore<SDL.SDL_bool>
                let initConfig =
                    SDL.SDL_INIT_TIMER |||
                    SDL.SDL_INIT_AUDIO |||
                    SDL.SDL_INIT_VIDEO |||
                    SDL.SDL_INIT_JOYSTICK |||
                    SDL.SDL_INIT_HAPTIC |||
                    SDL.SDL_INIT_GAMECONTROLLER |||
                    SDL.SDL_INIT_EVENTS
                let result = SDL.SDL_Init initConfig
                if result = 0 then
                    let mutable sdlVersion = Unchecked.defaultof<_>
                    SDL.SDL_GetVersion (&sdlVersion)
                    Log.info ("Initialized SDL " + string sdlVersion.major + "." + string sdlVersion.minor + "." + string sdlVersion.patch + ".")
                result)
            (fun () -> SDL.SDL_Quit ()) with
        | Left error -> Left error
        | Right ((), destroy) ->
            match tryMakeSdlResource
                (fun () ->
                    
                    // create window
                    let windowConfig = sdlConfig.WindowConfig
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ACCELERATED_VISUAL, 1) |> ignore<int>
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, Constants.OpenGL.VersionMajor) |> ignore<int>
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, Constants.OpenGL.VersionMinor) |> ignore<int>
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, Constants.OpenGL.Profile) |> ignore<int>
#if DEBUG
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, int SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG) |> ignore<int>
#endif
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore<int>
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore<int>
                    SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore<int>
                    let window = SDL.SDL_CreateWindow (windowConfig.WindowTitle, windowConfig.WindowX, windowConfig.WindowY, windowSize.X, windowSize.Y, windowConfig.WindowFlags)

                    // set to full screen when window taking up entire screen and unaccompanied
                    let mutable displayMode = Unchecked.defaultof<_>
                    SDL.SDL_GetDesktopDisplayMode (0, &displayMode) |> ignore<int>
                    if (windowSize.X = displayMode.w || windowSize.Y = displayMode.h) && not accompanied then
                        SDL.SDL_SetWindowFullscreen (window, uint SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN_DESKTOP) |> ignore
                    window)

                (fun window -> SDL.SDL_DestroyWindow window; destroy ()) with
            | Left error -> Left error
            | Right (window, destroy) ->
                match tryMakeSdlGlobalResource
                    (fun () -> SDL_ttf.TTF_Init ())
                    (fun () -> SDL_ttf.TTF_Quit (); destroy window) with
                | Left error -> Left error
                | Right ((), destroy) ->
                    match tryMakeSdlGlobalResource
                        (fun () -> SDL_mixer.Mix_Init (enum<SDL_mixer.MIX_InitFlags> 0))
                        (fun () -> SDL_mixer.Mix_Quit (); destroy ()) with
                    | Left error -> Left error
                    | Right ((), destroy) ->
                        match tryMakeSdlGlobalResource
                            (fun () -> SDL_mixer.Mix_OpenAudio (Constants.Audio.Frequency, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                            (fun () -> SDL_mixer.Mix_CloseAudio (); destroy ()) with
                        | Left error -> Left error
                        | Right ((), destroy) ->
                            GamepadState.init ()
                            let context = SglWindow { SglWindow = window }
                            Right { WindowOpt = Some context; Config = sdlConfig; Destroy = destroy }

type SdlDeps = SdlDeps.SdlDeps