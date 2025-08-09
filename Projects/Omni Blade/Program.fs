// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.IO
open SDL2
open Nu
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Omni Blade" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        World.run worldConfig (OmniBladePlugin ())