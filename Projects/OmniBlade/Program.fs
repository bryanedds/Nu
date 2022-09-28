// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open SDL2
open OmniBlade
module Program =

    type OmniPlugin () =
        inherit NuPlugin ()

        override this.Modes =
            Map.ofSeq
                ["Title", fun world -> Simulants.Game.SetModel (Gui Title) world
                 "Credits", fun world -> Simulants.Game.SetModel (Gui Credits) world
                 "Pick", fun world -> Simulants.Game.SetModel (Gui Pick) world
                 "Field", fun world -> Simulants.Game.SetModel (Field (Field.debug world)) world
                 "Battle", fun world -> Simulants.Game.SetModel (Field (Field.debugBattle world)) world]

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
        Nu.init worldConfig.NuConfig
        World.run worldConfig (OmniPlugin ())