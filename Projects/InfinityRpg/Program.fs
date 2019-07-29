namespace InfinityRpg
open System
open Nu
module Program =

    // this the entry point for the InfinityRpg application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init false

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "InfinityRpg" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // after some configuration it is time to run the game. We're off and running!
        World.run (fun sdlDeps -> World.tryMake WorldConfig.defaultConfig (InfinityRpgPlugin ()) sdlDeps) sdlConfig