namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
open OmniBlade
module Program =

    let [<EntryPoint>] main _ =
        World.initTypeConverters ()

        let sdlViewConfig =
            NewWindow
                { WindowTitle = "OmniBlade"
                  WindowX = 32
                  WindowY = 32
                  WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }
                  
        let sdlRendererFlags =
            enum<SDL.SDL_RendererFlags>
                (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
                 int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)

        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = NuConstants.ResolutionX
              ViewH = NuConstants.ResolutionY
              RendererFlags = sdlRendererFlags
              AudioChunkSize = NuConstants.AudioBufferSizeDefault }

        World.run
            (fun sdlDeps -> OmniFlow.tryMakeOmniBladeWorld sdlDeps ())
            (fun world -> (Running, world))
            sdlConfig