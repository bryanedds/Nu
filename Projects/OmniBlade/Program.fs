// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open Nu
open SDL2
open OmniBlade
module Program =
    
    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig =
            { SdlWindowConfig.defaultConfig with
                WindowTitle = "OmniBlade"
#if DEBUG
                WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL }
#else
                WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL.SDL_WindowFlags.SDL_WINDOW_OPENGL ||| SDL.SDL_WindowFlags.SDL_WINDOW_FULLSCREEN }
#endif
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init worldConfig.NuConfig
        World.run worldConfig (OmniBladePlugin ())