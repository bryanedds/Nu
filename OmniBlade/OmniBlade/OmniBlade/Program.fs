namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
open Nu.Constants
open OmniBlade
module Program =

    let [<EntryPoint>] main _ =
        World.init ()

        let sdlViewConfig =
            NewWindow
                { WindowTitle = "OmniBlade"
                  WindowX = SDL.SDL_WINDOWPOS_UNDEFINED
                  WindowY = SDL.SDL_WINDOWPOS_UNDEFINED
                  WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }
                  
        let sdlRendererFlags =
            enum<SDL.SDL_RendererFlags>
                (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
                 int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)

        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = ResolutionX
              ViewH = ResolutionY
              RendererFlags = sdlRendererFlags
              AudioChunkSize = AudioBufferSizeDefault }

        World.run
            (fun sdlDeps -> OmniProgression.tryMakeOmniBladeWorld () sdlDeps)
            (fun world -> world)
            sdlConfig