namespace Nu
open System
open System.Diagnostics
open System.Threading
open SDL2

type SdlWindowConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowFlags : SDL.SDL_WindowFlags }

type SdlViewConfig =
    | NewWindow of SdlWindowConfig
    | ExistingWindow of nativeint // NOTE: exposed pointer
    //| FullScreen

type SdlConfig =
    { ViewConfig : SdlViewConfig
      ViewW : int
      ViewH : int
      RendererFlags : SDL.SDL_RendererFlags
      AudioChunkSize : int }

type SdlDeps =
    { RenderContext : nativeint // NOTE: exposed pointer
      Window : nativeint // NOTE: exposed pointer
      Config : SdlConfig }

module Sdl =

    let [<Literal>] SuccessReturnCode = 0
    let [<Literal>] FailureReturnCode = 1

    let makeSdlConfig viewConfig viewW viewH rendererFlags audioChunkSize =
        { ViewConfig = viewConfig
          ViewW = viewW
          ViewH = viewH
          RendererFlags = rendererFlags
          AudioChunkSize = audioChunkSize }

    let makeSdlDeps renderContext window config =
        { RenderContext = renderContext
          Window = window
          Config  = config }

    let resourceNop (_ : nativeint) = ()

    let withSdlInit create destroy action =
        let initResult = create ()
        let error = SDL.SDL_GetError ()
        if initResult <> 0 && error <> "CoInitialize() DirectX error -2147417850" then
            trace ("SDL2# initialization failed due to '" + error + "'.")
            FailureReturnCode
        else
            let result = action ()
            destroy ()
            result

    let withSdlResource create destroy action =
        let resource = create ()
        if resource = IntPtr.Zero then
            let error = SDL.SDL_GetError ()
            Console.WriteLine ("SDL2# resource creation failed due to '" + error + "'.")
            FailureReturnCode
        else
            let result = action resource
            destroy resource
            result

    let withSdlGlobalResource create destroy action =
        let resource = create ()
        if resource <> 0 then
            let error = SDL.SDL_GetError ()
            Console.WriteLine ("SDL2# global resource creation failed due to '" + error + "'.")
            FailureReturnCode
        else
            let result = action ()
            destroy ()
            result

    let advanceSdl handleEvent handleUpdate sdlDeps world =
        let mutable result = (true, world)
        let polledEvent = ref (SDL.SDL_Event ())
        while SDL.SDL_PollEvent polledEvent <> 0 do
            if fst result then
                result <- handleEvent polledEvent (snd result)
        if fst result then
            result <- handleUpdate (snd result)
        result

    let renderSdl handleRender sdlDeps world =
        ignore (SDL.SDL_SetRenderDrawColor (sdlDeps.RenderContext, 0uy, 0uy, 179uy, 255uy))
        ignore (SDL.SDL_RenderClear sdlDeps.RenderContext)
        let world2 = handleRender world
        SDL.SDL_RenderPresent sdlDeps.RenderContext
        world2

    let playSdl handlePlay world =
        handlePlay world

    let rec runSdl7 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps world_ keepRunning =
        if keepRunning then
            let (keepRunning_, world_) = advanceSdl handleEvent handleUpdate sdlDeps world_
            if keepRunning_ then
                let world_ = renderSdl handleRender sdlDeps world_
                let world_ = playSdl handlePlay world_
                runSdl7 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps world_ keepRunning_
            else ignore (handleExit world_)

    let runSdl tryCreateWorld handleEvent handleUpdate handleRender handlePlay handleExit sdlConfig : int =
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
                            (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG)
                            (fun () -> SDL_mixer.Mix_Quit ())
                            (fun () ->
                            withSdlGlobalResource
                                (fun () -> SDL_mixer.Mix_OpenAudio (SDL_mixer.MIX_DEFAULT_FREQUENCY, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                                (fun () -> SDL_mixer.Mix_CloseAudio ())
                                (fun () ->
                                    let sdlDeps = makeSdlDeps renderContext window sdlConfig
                                    let optWorld = tryCreateWorld sdlDeps
                                    match optWorld with
                                    | Left errorMsg ->
                                        trace errorMsg
                                        FailureReturnCode
                                    | Right world ->
                                        runSdl7 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps world true
                                        SuccessReturnCode))))))