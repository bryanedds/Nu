// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open SDL2
open Prime
open Nu

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
          WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }

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

    /// A default SdlConfig.
    static member defaultConfig =
        { ViewConfig = NewWindow SdlWindowConfig.defaultConfig
          ViewW = Constants.Render.ResolutionX
          ViewH = Constants.Render.ResolutionY
          RendererFlags = Constants.Render.DefaultRendererFlags
          AudioChunkSize = Constants.Audio.DefaultBufferSize }

[<AutoOpen>]
module SdlDepsModule =

    /// The dependencies needed to initialize SDL.
    type [<ReferenceEquality>] SdlDeps =
        private
            { RenderContextOpt : nativeint option
              WindowOpt : nativeint option
              Config : SdlConfig
              Destroy : unit -> unit }
    
        interface IDisposable with
            member this.Dispose () =
                this.Destroy ()

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module SdlDeps =
    
        /// An empty SdlDeps.
        let empty =
            { RenderContextOpt = None
              WindowOpt = None
              Config = SdlConfig.defaultConfig
              Destroy = id }
    
        /// Get an sdlDep's optional render context.
        let getRenderContextOpt sdlDeps =
            sdlDeps.RenderContextOpt
    
        /// Get an sdlDep's optional window.
        let getWindowOpt sdlDeps =
            sdlDeps.WindowOpt
    
        /// Get an sdlDep's config.
        let getConfig sdlDeps =
            sdlDeps.Config
    
        /// Attempt to initalize an SDL module.
        let internal attemptPerformSdlInit create destroy =
            let initResult = create ()
            let error = SDL.SDL_GetError ()
            if initResult = 0 then Right ((), destroy)
            else Left error
    
        /// Attempt to initalize an SDL resource.
        let internal attemptMakeSdlResource create destroy =
            let resource = create ()
            if resource <> IntPtr.Zero then Right (resource, destroy)
            else
                let error = "SDL2# resource creation failed due to '" + SDL.SDL_GetError () + "'."
                Left error
    
        /// Attempt to initalize a global SDL resource.
        let internal attemptMakeSdlGlobalResource create destroy =
            let resource = create ()
            if resource = 0 then Right ((), destroy)
            else
                let error = "SDL2# global resource creation failed due to '" + SDL.SDL_GetError () + "'."
                Left error
    
        /// Attempt to make an SdlDeps instance.
        let attemptMake sdlConfig =
            match attemptPerformSdlInit
                (fun () -> SDL.SDL_Init SDL.SDL_INIT_EVERYTHING)
                (fun () -> SDL.SDL_Quit ()) with
            | Left error -> Left error
            | Right ((), destroy) ->
                match attemptMakeSdlResource
                    (fun () ->
                        match sdlConfig.ViewConfig with
                        | NewWindow windowConfig -> SDL.SDL_CreateWindow (windowConfig.WindowTitle, windowConfig.WindowX, windowConfig.WindowY, sdlConfig.ViewW, sdlConfig.ViewH, windowConfig.WindowFlags)
                        | ExistingWindow hwindow -> SDL.SDL_CreateWindowFrom hwindow)
                    (fun window -> SDL.SDL_DestroyWindow window; destroy ()) with
                | Left error -> Left error
                | Right (window, destroy) ->
                    match attemptMakeSdlResource
                        (fun () -> SDL.SDL_CreateRenderer (window, -1, uint32 sdlConfig.RendererFlags))
                        (fun renderContext -> SDL.SDL_DestroyRenderer renderContext; destroy window) with
                    | Left error -> Left error
                    | Right (renderContext, destroy) ->
                        match attemptMakeSdlGlobalResource
                            (fun () -> SDL_ttf.TTF_Init ())
                            (fun () -> SDL_ttf.TTF_Quit (); destroy renderContext) with
                        | Left error -> Left error
                        | Right ((), destroy) ->
                            match attemptMakeSdlGlobalResource
#if MIX_INIT_OGG
                                (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG) // NOTE: for some reason this line fails on 32-bit builds.. WHY?
#else
                                (fun () -> SDL_mixer.Mix_Init (enum<SDL_mixer.MIX_InitFlags> 0))
#endif
                                (fun () -> SDL_mixer.Mix_Quit (); destroy ()) with
                            | Left error -> Left error
                            | Right ((), destroy) ->
                                match attemptMakeSdlGlobalResource
                                    (fun () -> SDL_mixer.Mix_OpenAudio (Constants.Audio.Frequency, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                                    (fun () -> SDL_mixer.Mix_CloseAudio (); destroy ()) with
                                | Left error -> Left error
                                | Right ((), destroy) ->
                                    GamepadState.init ()
                                    Right { RenderContextOpt = Some renderContext; WindowOpt = Some window; Config = sdlConfig; Destroy = destroy }

[<RequireQualifiedAccess>]
module Sdl =

    /// Update the game engine's state.
    let update handleEvent handleUpdate world =
        if SDL.SDL_WasInit SDL.SDL_INIT_TIMER <> 0u then
            let mutable result = (Running, world)
            let polledEvent = ref (SDL.SDL_Event ())
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
        match SdlDeps.getRenderContextOpt sdlDeps with
        | Some renderContext ->
            match Constants.Render.ScreenClearing with
            | NoClear -> ()
            | ColorClear (r, g, b) ->
                SDL.SDL_SetRenderDrawColor (renderContext, r, g, b, 255uy) |> ignore
                SDL.SDL_RenderClear renderContext |> ignore
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
    let rec runWithoutCleanUp runWhile handleEvent handleUpdate handleRender handlePlay sdlDeps liveness world =
        if runWhile world then
            match liveness with
            | Running ->
                let (liveness, world) = update handleEvent handleUpdate world
                match liveness with
                | Running ->
                    let world = render handleRender sdlDeps world
                    let world = play handlePlay world
                    runWithoutCleanUp runWhile handleEvent handleUpdate handleRender handlePlay sdlDeps liveness world
                | Exiting -> world
            | Exiting -> world
        else world

    /// Run the game engine with the given handlers.
    let run9 runWhile handleEvent handleUpdate handleRender handlePlay handleChoose handleExit sdlDeps liveness world =
        try let world = runWithoutCleanUp runWhile handleEvent handleUpdate handleRender handlePlay sdlDeps liveness world
            handleExit world
            Constants.Engine.SuccessExitCode
        with exn ->
            let world = handleChoose world
            Log.trace (scstring exn)
            handleExit world
            Constants.Engine.FailureExitCode

    /// Run the game engine with the given handlers.
    let run handleAttemptMakeWorld handleEvent handleUpdate handleRender handlePlay handleChoose handleExit sdlConfig =
        match SdlDeps.attemptMake sdlConfig with
        | Right sdlDeps ->
            use sdlDeps = sdlDeps // bind explicitly to dispose automatically
            match handleAttemptMakeWorld sdlDeps with
            | Right world -> run9 tautology handleEvent handleUpdate handleRender handlePlay handleChoose handleExit sdlDeps Running world
            | Left error -> Log.trace error; Constants.Engine.FailureExitCode
        | Left error -> Log.trace error; Constants.Engine.FailureExitCode

/// The dependencies needed to initialize SDL.
type SdlDeps = SdlDepsModule.SdlDeps