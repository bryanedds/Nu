module Nu.Sdl
open System
open System.Diagnostics
open System.Threading
open OpenTK.Graphics.OpenGL
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
      AudioVolume : single }

type SdlDeps =
    { Window : nativeint
      RenderContext : nativeint
      Config : SdlConfig }

let makeSdlConfig windowTitle windowX windowY windowW windowH windowFlags audioVolume =
    { WindowTitle = windowTitle
      WindowX = windowX
      WindowY = windowY
      WindowW = windowW
      WindowH = windowH
      WindowFlags = windowFlags
      AudioVolume = audioVolume }

let makeSdlDeps renderContext window config =
    { Window = window
      RenderContext = renderContext
      Config  = config }

let resourceNop (_ : nativeint) = ()

let withSdlInit create destroy action =
    let initResult = create ()
    let error = SDL.SDL_GetError ()
    if initResult <> 0 && error <> "CoInitialize() DirectX error -2147417850" then
        trace ("SDL2# initialization failed due to '" + error + "'.")
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
            result <- handleEvent polledEvent (snd result)
    if fst result then
        result <- handleUpdate (snd result)
    result

let renderSdl handleRender sdlDeps world =
    GL.BindFramebuffer (FramebufferTarget.Framebuffer, 0)
    GL.ClearColor (0.0f, 0.0f, 0.0f, 0.5f)
    GL.ClearDepth 1.0f
    GL.ClearStencil 0
    GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit ||| ClearBufferMask.StencilBufferBit)
    let world2 = handleRender world
    SDL.SDL_GL_SwapWindow sdlDeps.Window
    world2
    
let rec runSdl6 handleEvent handleUpdate handleRender handleExit sdlDeps world keepRunning =
    if keepRunning then
        let (keepRunning2, world2) = advanceSdl handleEvent handleUpdate sdlDeps world
        if keepRunning2 then
            let world3 = renderSdl handleRender sdlDeps world2
            runSdl6 handleEvent handleUpdate handleRender handleExit sdlDeps world3 keepRunning2
        else ignore (handleExit world)

let runSdl createWorld handleEvent handleUpdate handleRender handleExit sdlConfig : int =
    withSdlInit
        (fun () ->
            SDL.SDL_SetMainReady () // according to flibit, this may keep sdl from blowing up on the iOS
            let result = SDL.SDL_Init SDL.SDL_INIT_EVERYTHING
            SDL.SDL_DisableScreenSaver ()
            result)
        (fun () ->
            SDL.SDL_EnableScreenSaver () // NOTE: not sure if this is necessary...
            SDL.SDL_Quit ())
        (fun () ->
            withSdlResource
                (fun () ->
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_RED_SIZE, 8))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_GREEN_SIZE, 8))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_BLUE_SIZE, 8))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ALPHA_SIZE, 8))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8))
                    ignore (SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1))
                    SDL.SDL_CreateWindow (sdlConfig.WindowTitle, sdlConfig.WindowX, sdlConfig.WindowY, sdlConfig.WindowW, sdlConfig.WindowH, sdlConfig.WindowFlags))
                (fun window -> SDL.SDL_DestroyWindow window)
                (fun window ->
                    withSdlResource
                        (fun () ->
                            let renderContext = SDL.SDL_GL_CreateContext window
                            OpenTK.Graphics.GraphicsContext.CurrentContext <- renderContext
                            OpenTK.Graphics.OpenGL.GL.LoadAll ()
                            ignore (GL.Enable EnableCap.Texture2D)
                            ignore (GL.Viewport (0, 0, sdlConfig.WindowW, sdlConfig.WindowH))
                            ignore (GL.MatrixMode MatrixMode.Projection)
                            ignore (GL.LoadIdentity ())
                            ignore (GL.Ortho (0.0, float sdlConfig.WindowW, float sdlConfig.WindowH, 0.0, -1.0, 1.0))
                            ignore (GL.MatrixMode MatrixMode.Modelview)
                            ignore (GL.LoadIdentity())
                            ignore (SDL.SDL_GL_MakeCurrent (window, renderContext))
                            renderContext)
                        (fun renderContext -> SDL.SDL_GL_DeleteContext renderContext)
                        (fun renderContext ->
                            let sdlDeps = makeSdlDeps window renderContext sdlConfig
                            let world = createWorld sdlDeps
                            runSdl6 handleEvent handleUpdate handleRender handleExit sdlDeps world true
                            SuccessCode)))