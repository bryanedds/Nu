module Nu.Run
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

let resourceNop (_ : nativeint) =
    ()

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

let updateSdl handleEvent handleUpdate sdlDeps =
    let mutable keepRunning = true
    let polledEvent = ref (SDL.SDL_Event ())
    while SDL.SDL_PollEvent polledEvent <> 0 do
        if keepRunning then
            keepRunning <- handleEvent polledEvent sdlDeps
    if keepRunning then
        keepRunning <- handleUpdate sdlDeps
    keepRunning

let renderSdl handleRender sdlDeps =
    let keepRunning = handleRender sdlDeps
    SDL.SDL_RenderPresent sdlDeps.Renderer
    keepRunning
    
let rec runSdl handleEvent handleUpdate handleRender sdlDeps keepRunning =
    if keepRunning then
        let keepRunning2 = updateSdl handleEvent handleUpdate sdlDeps
        let keepRunning3 = keepRunning2 && renderSdl handleRender sdlDeps
        runSdl handleEvent handleUpdate handleRender sdlDeps keepRunning3

let runNu handleEvent handleUpdate handleRender sdlConfig =
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
                            runSdl handleEvent handleUpdate handleRender sdlDeps true
                            SuccessCode)))