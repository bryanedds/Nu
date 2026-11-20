// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.IO
open Nu

/// The game's entry point.
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init ()
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Zombie Game" }
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        World.run ignore worldConfig (ZombieGamePlugin ())
