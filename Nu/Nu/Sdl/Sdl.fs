// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open SDL2
open Prime

/// A window for rendering in SDL OpenGL.
type [<ReferenceEquality>] SglWindow =
    { SglWindow : nativeint }
    
/// An interface for specifying a Windows Forms control that uses OpenGL.
type WfglWindow =
    interface
        abstract CreateSdlWindowFrom : unit -> unit
        abstract CreateWfglContext : unit -> unit
        abstract Swap : unit -> unit
        abstract CleanUp : unit -> unit
        end

/// A window for rendering.
type [<ReferenceEquality>] Window =
    | SglWindow of SglWindow
    | WfglWindow of WfglWindow

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
          WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL }

/// Describes the view that SDL will use to render.
type [<ReferenceEquality>] SdlViewConfig =
    | NewWindow of SdlWindowConfig
    | ExistingWindow of WfglWindow

/// Describes the general configuration of SDL.
type [<ReferenceEquality>] SdlConfig =
    { ViewConfig : SdlViewConfig
      ViewW : int
      ViewH : int
      AudioChunkSize : int }

    /// A default SdlConfig.
    static member defaultConfig =
        { ViewConfig = NewWindow SdlWindowConfig.defaultConfig
          ViewW = Constants.Render.ResolutionX
          ViewH = Constants.Render.ResolutionY
          AudioChunkSize = Constants.Audio.BufferSizeDefault }

[<AutoOpen>]
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
            let flags =
                if fullScreen
                then uint SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN
                else 0u
            SDL.SDL_SetWindowFullscreen (window.SglWindow, flags) |> ignore
        | _ -> ()
        sdlDeps

    /// Attempt to initalize an SDL module.
    let internal attemptPerformSdlInit create destroy =
        let initResult = create ()
        let error = SDL.SDL_GetError ()
        if initResult = 0 then Right ((), destroy)
        else Left error

    /// Attempt to initalize an SDL resource.
    let internal tryMakeSdlResourcePlus create destroy =
        let resourceEir = create ()
        match resourceEir with
        | Right resource ->
            if resource <> IntPtr.Zero then Right (resourceEir, destroy)
            else
                let error = "SDL2# resource creation failed due to '" + SDL.SDL_GetError () + "'."
                Left error
        | Left _ -> Right (resourceEir, destroy)

    /// Attempt to initalize an SDL resource.
    let internal tryMakeSdlResource create destroy =
        let resource = create ()
        if resource <> IntPtr.Zero then Right (resource, destroy)
        else
            let error = "SDL2# resource creation failed due to '" + SDL.SDL_GetError () + "'."
            Left error

    /// Attempt to initalize a global SDL resource.
    let internal tryMakeSdlGlobalResource create destroy =
        let resource = create ()
        if resource = 0 then Right ((), destroy)
        else
            let error = "SDL2# global resource creation failed due to '" + SDL.SDL_GetError () + "'."
            Left error

    /// Attempt to make an SdlDeps instance.
    let tryMake sdlConfig =
        match attemptPerformSdlInit
            (fun () ->
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
            match tryMakeSdlResourcePlus
                (fun () ->
                    match sdlConfig.ViewConfig with
                    | NewWindow windowConfig ->
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ACCELERATED_VISUAL, 1) |> ignore<int>
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, Constants.OpenGL.VersionMajor) |> ignore<int>
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, Constants.OpenGL.VersionMinor) |> ignore<int>
#if DEBUG
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, int SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG) |> ignore<int>
#endif
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore<int>
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore<int>
                        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore<int>
                        let window = SDL.SDL_CreateWindow (windowConfig.WindowTitle, windowConfig.WindowX, windowConfig.WindowY, sdlConfig.ViewW, sdlConfig.ViewH, windowConfig.WindowFlags)
                        Right window
                    | ExistingWindow window ->
                        window.CreateSdlWindowFrom ()
                        Left window)
                (fun windowOpt ->
                    match windowOpt with
                    | Right window -> SDL.SDL_DestroyWindow window
                    | Left (window : WfglWindow) -> window.CleanUp ()
                    destroy ()) with
            | Left error -> Left error
            | Right (window, destroy) ->
                match tryMakeSdlGlobalResource
                    (fun () -> SDL_ttf.TTF_Init ())
                    (fun () -> SDL_ttf.TTF_Quit (); destroy window) with
                | Left error -> Left error
                | Right ((), destroy) ->
                    match tryMakeSdlGlobalResource
#if MIX_INIT_OGG
                        (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG) // NOTE: for some reason this line fails on 32-bit builds... WHY?
#else
                        (fun () -> SDL_mixer.Mix_Init (enum<SDL_mixer.MIX_InitFlags> 0))
#endif
                        (fun () -> SDL_mixer.Mix_Quit (); destroy ()) with
                    | Left error -> Left error
                    | Right ((), destroy) ->
                        match tryMakeSdlGlobalResource
                            (fun () -> SDL_mixer.Mix_OpenAudio (Constants.Audio.Frequency, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                            (fun () -> SDL_mixer.Mix_CloseAudio (); destroy ()) with
                        | Left error -> Left error
                        | Right ((), destroy) ->
                            GamepadState.init ()
                            let context =
                                match window with
                                | Right window ->
                                    SDL.SDL_RaiseWindow window
                                    SglWindow { SglWindow = window }
                                | Left wfglWindow ->
                                    WfglWindow wfglWindow
                            Right { WindowOpt = Some context; Config = sdlConfig; Destroy = destroy }

type SdlDeps = SdlDeps.SdlDeps