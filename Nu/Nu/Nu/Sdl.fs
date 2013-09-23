module Nu.Sdl
open System
open System.Diagnostics
open System.Threading
open SDL2

let [<Literal>] SuccessCode = 0
let [<Literal>] FailureCode = 1

type SdlConfig =
    { WindowTitle : string
      WindowX : int
      WindowY : int
      WindowW : int
      WindowH : int
      WindowFlags : SDL.SDL_WindowFlags
      RendererFlags : SDL.SDL_RendererFlags }

type SdlDeps =
    { Renderer : nativeint
      Window : nativeint
      Config : SdlConfig }

let makeSdlConfig windowTitle windowX windowY windowW windowH windowFlags rendererFlags =
    { WindowTitle = windowTitle
      WindowX = windowX
      WindowY = windowY
      WindowW = windowW
      WindowH = windowH
      WindowFlags = windowFlags
      RendererFlags = rendererFlags }

let makeSdlDeps renderer window config =
    { Renderer = renderer
      Window = window
      Config  = config }

let resourceNop (_ : nativeint) = ()

let withSdlInit create destroy action =
    let initResult = create ()
    let error = SDL.SDL_GetError ()
    if initResult <> 0 && error <> "CoInitialize() DirectX error -2147417850" then
        Console.WriteLine ("SDL2# initialization failed due to '" + error + "'.")
        FailureCode
    else
        let result = action ()
        destroy ()
        result

let withSdlResource create destroy action =
    let resource = create ()
    if resource = IntPtr.Zero then
        let error = SDL.SDL_GetError ()
        Console.WriteLine ("SDL2# resource creation failed due to '" + error + "'.")
        FailureCode
    else
        let result = action resource
        destroy resource
        result

let advanceSdl handleEvent handleUpdate sdlDeps world =
    let mutable result = (true, world)
    let polledEvent = ref (SDL.SDL_Event ())
    while SDL.SDL_PollEvent polledEvent <> 0 do
        if fst result then
            result <- handleEvent polledEvent sdlDeps (snd result)
    if fst result then
        result <- handleUpdate sdlDeps (snd result)
    result

let renderSdl handleRender sdlDeps world =
    ignore (SDL.SDL_SetRenderDrawColor (sdlDeps.Renderer, 0uy, 0uy, 179uy, 255uy))
    ignore (SDL.SDL_RenderClear sdlDeps.Renderer)
    let newWorld = handleRender sdlDeps world
    SDL.SDL_RenderPresent sdlDeps.Renderer
    newWorld
    
let rec runSdl6 handleEvent handleUpdate handleRender sdlDeps world keepRunning =
    if keepRunning then
        let (newKeepRunning, newWorld) = advanceSdl handleEvent handleUpdate sdlDeps world
        if newKeepRunning then
            let newWorld2 = renderSdl handleRender sdlDeps newWorld
            runSdl6 handleEvent handleUpdate handleRender sdlDeps newWorld2 newKeepRunning

let runSdl createWorld handleEvent handleUpdate handleRender sdlConfig =
    withSdlInit
        (fun () -> SDL.SDL_Init SDL.SDL_INIT_EVERYTHING)
        (fun () -> SDL.SDL_Quit ())
        (fun () ->
            withSdlResource
                (fun () -> SDL.SDL_CreateWindow (sdlConfig.WindowTitle, sdlConfig.WindowX, sdlConfig.WindowY, sdlConfig.WindowW, sdlConfig.WindowH, sdlConfig.WindowFlags))
                (fun window -> SDL.SDL_DestroyWindow window)
                (fun window ->
                    withSdlResource
                        (fun () -> SDL.SDL_CreateRenderer (window, -1, uint32 sdlConfig.RendererFlags))
                        (fun renderer -> SDL.SDL_DestroyRenderer renderer)
                        (fun renderer ->
                            let sdlDeps = makeSdlDeps renderer window sdlConfig
                            let world = createWorld sdlDeps
                            runSdl6 handleEvent handleUpdate handleRender sdlDeps world true
                            SuccessCode)))