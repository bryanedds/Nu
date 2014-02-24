namespace OmniBlade
open System
open SDL2
open OpenTK
open TiledSharp
open Nu
open OmniBlade.OmniBlade
module Program =

    let [<EntryPoint>] main _ =
        WorldModule.initTypeConverters ()
        let sdlViewConfig = NewWindow { WindowTitle = "OmniBlade"; WindowX = 32; WindowY = 32; WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }
        let sdlRenderFlags = enum<SDL.SDL_RendererFlags> (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED ||| int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
        let sdlConfig = Sdl.makeSdlConfig sdlViewConfig Voords.VirtualResolutionX Voords.VirtualResolutionY sdlRenderFlags 1024
        WorldModule.run
            (fun sdlDeps -> tryCreateOmniBladeWorld sdlDeps ())
            (fun world -> WorldModule.updateTransition (fun world' -> (true, world')) world)
            sdlConfig