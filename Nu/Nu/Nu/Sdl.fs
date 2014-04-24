namespace Nu
open System
open System.Diagnostics
open System.Threading
open SDL2
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module SdlModule =

    type SdlWindowConfig =
        { WindowTitle : string
          WindowX : int
          WindowY : int
          WindowFlags : SDL.SDL_WindowFlags }

    type SdlViewConfig =
        | NewWindow of SdlWindowConfig
        | ExistingWindow of nativeint
        //| FullScreen

    type SdlConfig =
        { ViewConfig : SdlViewConfig
          ViewW : int
          ViewH : int
          RendererFlags : SDL.SDL_RendererFlags
          AudioChunkSize : int }

    type SdlDeps =
        { RenderContext : nativeint
          Window : nativeint
          Config : SdlConfig }

module Sdl =

    let private SuccessReturnCode = 0
    let private FailureReturnCode = 1

    let makeMouseButton sdlMouseButton =
        if sdlMouseButton = byte SDL.SDL_BUTTON_LEFT then MouseLeft
        elif sdlMouseButton = byte SDL.SDL_BUTTON_MIDDLE then MouseCenter
        else MouseRight

    let makeSdlMouseButton mouseButton =
        match mouseButton with
        | MouseLeft -> byte SDL.SDL_BUTTON_LEFT
        | MouseCenter -> byte SDL.SDL_BUTTON_MIDDLE
        | MouseRight -> byte SDL.SDL_BUTTON_RIGHT

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
            trace <| "SDL2# initialization failed due to '" + error + "'."
            FailureReturnCode
        else
            let result = action ()
            destroy ()
            result

    let withSdlResource create destroy action =
        let resource = create ()
        if resource = IntPtr.Zero then
            let error = SDL.SDL_GetError ()
            trace <| "SDL2# resource creation failed due to '" + error + "'."
            FailureReturnCode
        else
            let result = action resource
            destroy resource
            result

    let withSdlGlobalResource create destroy action =
        let resource = create ()
        if resource <> 0 then
            let error = SDL.SDL_GetError ()
            trace <| "SDL2# global resource creation failed due to '" + error + "'."
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
        match ScreenClearing with
        | NoClear -> ()
        | ColorClear (r, g, b) ->
            ignore (SDL.SDL_SetRenderDrawColor (sdlDeps.RenderContext, r, g, b, 255uy))
            ignore (SDL.SDL_RenderClear sdlDeps.RenderContext)
        let world' = handleRender world
        SDL.SDL_RenderPresent sdlDeps.RenderContext
        world'

    let playSdl handlePlay world =
        handlePlay world

    let rec runSdl8 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps keepRunning world =
        if keepRunning then
            let (keepRunning', world') = advanceSdl handleEvent handleUpdate sdlDeps world
            if not keepRunning' then ignore (handleExit world')
            else
                let world'' = renderSdl handleRender sdlDeps world'
                let world'3 = playSdl handlePlay world''
                runSdl8 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps keepRunning' world'3

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
#if MIX_INIT_OGG
                            (fun () -> SDL_mixer.Mix_Init SDL_mixer.MIX_InitFlags.MIX_INIT_OGG) // NOTE: for some reason this line fails on 32-bit builds.. WHY?
#else
                            (fun () -> SDL_mixer.Mix_Init <| enum<SDL_mixer.MIX_InitFlags> 0)
#endif
                            (fun () -> SDL_mixer.Mix_Quit ())
                            (fun () ->
                            withSdlGlobalResource
                                (fun () -> SDL_mixer.Mix_OpenAudio (AudioFrequency, SDL_mixer.MIX_DEFAULT_FORMAT, SDL_mixer.MIX_DEFAULT_CHANNELS, sdlConfig.AudioChunkSize))
                                (fun () -> SDL_mixer.Mix_CloseAudio ())
                                (fun () ->
                                    let sdlDeps = makeSdlDeps renderContext window sdlConfig
                                    let optWorld = tryCreateWorld sdlDeps
                                    match optWorld with
                                    | Left errorMsg ->
                                        trace errorMsg
                                        FailureReturnCode
                                    | Right world ->
                                        runSdl8 handleEvent handleUpdate handleRender handlePlay handleExit sdlDeps true world
                                        SuccessReturnCode))))))