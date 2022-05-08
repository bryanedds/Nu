// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open SDL2
open Prime
open Nu

/// Describes the initial configuration of a window created via SDL.
type [<StructuralEquality; NoComparison>] SdlWindowConfig =
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
type [<NoEquality; NoComparison>] SdlViewConfig =
    | NewWindow of SdlWindowConfig
    | WglWindow of WglContext

/// Describes the general configuration of SDL.
type [<NoEquality; NoComparison>] SdlConfig =
    { ViewConfig : SdlViewConfig
      ViewW : int
      ViewH : int
      RendererFlags : SDL.SDL_RendererFlags
      AudioChunkSize : int }

    /// A default SdlConfig.
    static member defaultConfig =
        { ViewConfig = NewWindow SdlWindowConfig.defaultConfig
          ViewW = Constants.Render.ResolutionX
          ViewH = Constants.Render.ResolutionY
          RendererFlags = Constants.Render.RendererFlagsDefault
          AudioChunkSize = Constants.Audio.BufferSizeDefault }

[<AutoOpen>]
module SdlDeps =

    /// The dependencies needed to initialize SDL.
    type [<ReferenceEquality; NoComparison>] SdlDeps =
        private
            { RenderContextOpt : RenderContext option
              Config : SdlConfig
              Destroy : unit -> unit }
    
        interface IDisposable with
            member this.Dispose () =
                this.Destroy ()

    /// An empty SdlDeps.
    let empty =
        { RenderContextOpt = None
          Config = SdlConfig.defaultConfig
          Destroy = id }

    /// Get an sdlDep's optional render context.
    let getRenderContextOpt sdlDeps =
        sdlDeps.RenderContextOpt

    /// Get an sdlDep's config.
    let getConfig sdlDeps =
        sdlDeps.Config

    /// Attempt to set the window's full screen state.
    let trySetWindowFullScreen fullScreen sdlDeps =
        match sdlDeps.RenderContextOpt with
        | Some (SglContext context) ->
            let flags =
                if fullScreen
                then uint SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN
                else 0u
            SDL.SDL_SetWindowFullscreen (context.SglWindow, flags) |> ignore
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
                let initConfig = SDL.SDL_INIT_EVERYTHING - SDL.SDL_INIT_SENSOR // NOTE: avoids depending on the presence of tilt sensor hardware.
                SDL.SDL_Init initConfig)
            (fun () -> SDL.SDL_Quit ()) with
        | Left error -> Left error
        | Right ((), destroy) ->
            match tryMakeSdlResourcePlus
                (fun () ->
                    match sdlConfig.ViewConfig with
                    | NewWindow windowConfig ->
                        let window = SDL.SDL_CreateWindow (windowConfig.WindowTitle, windowConfig.WindowX, windowConfig.WindowY, sdlConfig.ViewW, sdlConfig.ViewH, windowConfig.WindowFlags)
                        Right window
                    | WglWindow wglContext ->
                        Left wglContext)
                (fun windowOpt ->
                    match windowOpt with
                    | Right window -> SDL.SDL_DestroyWindow window
                    | Left _ -> ()
                    destroy ()) with
            | Left error -> Left error
            | Right (contextOrWindow, destroy) ->
                match tryMakeSdlGlobalResource
                    (fun () -> SDL_ttf.TTF_Init ())
                    (fun () -> SDL_ttf.TTF_Quit (); destroy contextOrWindow) with
                | Left error -> Left error
                | Right ((), destroy) ->
                    match tryMakeSdlGlobalResource
#if MIX_INIT_OGG
                        (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG) // NOTE: for some reason this line fails on 32-bit builds.. WHY?
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
                                match contextOrWindow with
                                | Right window ->
                                    SDL.SDL_RaiseWindow window
                                    SglContext { SglWindow = window }
                                | Left context ->
                                    WglContext context
                            Right { RenderContextOpt = Some context; Config = sdlConfig; Destroy = destroy }

type SdlDeps = SdlDeps.SdlDeps