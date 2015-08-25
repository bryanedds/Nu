// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Diagnostics
open System.Threading
open SDL2
open Prime
open Nu

/// Describes the initial configuration of a window created via SDL.
type SdlWindowConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowFlags : SDL.SDL_WindowFlags }

/// Describes the view that SDL will use to render.
type SdlViewConfig =
    | NewWindow of SdlWindowConfig
    | ExistingWindow of nativeint
    //| FullScreen TODO: implement

/// Describes the general configuration of SDL.
type SdlConfig =
    { ViewConfig : SdlViewConfig
      ViewW : int
      ViewH : int
      RendererFlags : SDL.SDL_RendererFlags
      AudioChunkSize : int }

/// The dependencies needed to initialize SDL.
type SdlDeps =
    { OptRenderContext : nativeint option
      OptWindow : nativeint option
      Config : SdlConfig }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SdlViewConfig =

    /// An empty SdlViewConfig.
    let empty =
        NewWindow
            { WindowTitle = "Nu Game"
              WindowX = SDL.SDL_WINDOWPOS_UNDEFINED
              WindowY = SDL.SDL_WINDOWPOS_UNDEFINED
              WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SdlConfig =

    /// An empty SdlConfig.
    let empty =
        { ViewConfig = SdlViewConfig.empty
          ViewW = Constants.Render.ResolutionX
          ViewH = Constants.Render.ResolutionY
          RendererFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
          AudioChunkSize = Constants.Audio.AudioBufferSizeDefault }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SdlDeps =

    /// An empty SdlDeps.
    let empty =
        { OptRenderContext = None
          OptWindow = None
          Config = SdlConfig.empty }

[<RequireQualifiedAccess>]
module Sdl =

    let resourceNop (_ : nativeint) = ()

    /// Initalize SDL and continue into a given action (AKA, continuation).
    let withSdlInit create destroy action =
        let initResult = create ()
        let error = SDL.SDL_GetError ()
        if initResult <> 0 then
            trace ^ "SDL2# initialization failed due to '" + error + "'."
            Constants.Engine.FailureExitCode
        else
            try action ()
            finally destroy ()

    /// Initalize an SDL resources and continue into a given action (AKA, continuation).
    let withSdlResource create destroy action =
        let resource = create ()
        if resource = IntPtr.Zero then
            let error = SDL.SDL_GetError ()
            trace ^ "SDL2# resource creation failed due to '" + error + "'."
            Constants.Engine.FailureExitCode
        else
            try action resource
            finally destroy resource

    /// Initalize a global SDL resources and continue into a given action (AKA, continuation).
    let withSdlGlobalResource create destroy action =
        let resource = create ()
        if resource <> 0 then
            let error = SDL.SDL_GetError ()
            trace ^ "SDL2# global resource creation failed due to '" + error + "'."
            Constants.Engine.FailureExitCode
        else
            try action ()
            finally destroy ()

    /// Update the game engine's state.
    let update handleEvent handleUpdate world =
        if SDL.SDL_WasInit SDL.SDL_INIT_TIMER <> 0u then
            let mutable result = (Running, world)
            let polledEvent = ref ^ SDL.SDL_Event ()
            while
                SDL.SDL_PollEvent polledEvent <> 0 &&
                (match fst result with Running -> true | Exiting -> false) do
                result <- handleEvent !polledEvent (snd result)
            match fst result with
            | Exiting -> ()
            | Running -> result <- handleUpdate (snd result)
            result
        else handleUpdate world

    /// Render the game engine's current frame.
    let render handleRender sdlDeps world =
        match sdlDeps.OptRenderContext with
        | Some renderContext ->
            match Constants.Render.ScreenClearing with
            | NoClear -> ()
            | ColorClear (r, g, b) ->
                ignore ^ SDL.SDL_SetRenderDrawColor (renderContext, r, g, b, 255uy)
                ignore ^ SDL.SDL_RenderClear renderContext
            let world = handleRender world
            SDL.SDL_RenderPresent renderContext
            world
        | None -> handleRender world

    /// Play the game engine's current audio.
    let play handlePlay world =
        if SDL.SDL_WasInit SDL.SDL_INIT_AUDIO <> 0u
        then handlePlay world // doesn't need any extra sdl processing here
        else handlePlay world

    /// Run the game engine with the given handlers, but don't clean up at the end, and return the world.
    let rec runWithoutCleanUp handleEvent handleUpdate handleRender handlePlay sdlDeps optFrames liveness world =
        let (anotherFrame, optFrames) =
            match optFrames with
            | Some frames ->
                if frames > 0 then (true, Some ^ frames - 1)
                elif frames < 0 then (true, Some frames)
                else (false, Some frames)
            | None -> (true, None)
        if anotherFrame then
            match liveness with
            | Running ->
                let (liveness, world) = update handleEvent handleUpdate world
                match liveness with
                | Running ->
                    let world = render handleRender sdlDeps world
                    let world = play handlePlay world
                    runWithoutCleanUp handleEvent handleUpdate handleRender handlePlay sdlDeps optFrames liveness world
                | Exiting -> world
            | Exiting -> world
        else world

    /// Run the game engine with the given handlers.
    let run8 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps optFrames liveness world =
        try let world = runWithoutCleanUp handleEvent handleUpdate handleRender handlePlay sdlDeps optFrames liveness world
            handleExit world
        with _ ->
            handleExit world

    /// Run the game engine with the given handlers.
    let run handleTryMakeWorld handleEvent handleUpdate handleRender handlePlay handleExit sdlConfig =
        withSdlInit
            (fun () -> SDL.SDL_Init SDL.SDL_INIT_EVERYTHING)
            (fun () -> SDL.SDL_Quit ())
            (fun () ->
            withSdlResource
                (fun () ->
                    match sdlConfig.ViewConfig with
                    | NewWindow windowConfig -> SDL.SDL_CreateWindow (windowConfig.WindowTitle, windowConfig.WindowX, windowConfig.WindowY, sdlConfig.ViewW, sdlConfig.ViewH, windowConfig.WindowFlags)
                    | ExistingWindow hwindow -> SDL.SDL_CreateWindowFrom hwindow)
                (fun window -> SDL.SDL_DestroyWindow window)
                (fun window ->
                withSdlResource
                    (fun () -> SDL.SDL_CreateRenderer (window, -1, uint32 sdlConfig.RendererFlags))
                    (fun renderContext -> SDL.SDL_DestroyRenderer renderContext)
                    (fun renderContext ->
                    withSdlGlobalResource
                        (fun () -> SDL_ttf.TTF_Init ())
                        (fun () -> SDL_ttf.TTF_Quit ())
                        (fun () ->
                        withSdlGlobalResource
#if MIX_INIT_OGG
                            (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG) // NOTE: for some reason this line fails on 32-bit builds.. WHY?
#else
                            (fun () -> SDL_mixer.Mix_Init ^ enum<SDL_mixer.MIX_InitFlags> 0)
#endif
                            (fun () -> SDL_mixer.Mix_Quit ())
                            (fun () ->
                            withSdlGlobalResource
                                (fun () -> SDL_mixer.Mix_OpenAudio (Constants.Audio.AudioFrequency, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                                (fun () -> SDL_mixer.Mix_CloseAudio ())
                                (fun () ->
                                    let sdlDeps = { OptRenderContext = Some renderContext; OptWindow = Some window; Config = sdlConfig }
                                    let eitherWorld = handleTryMakeWorld sdlDeps
                                    match eitherWorld with
                                    | Right world ->
                                        run8 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps None Running world
                                        Constants.Engine.SuccessExitCode
                                    | Left error ->
                                        trace error
                                        Constants.Engine.FailureExitCode))))))