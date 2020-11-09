namespace Elmario
open System
open Nu
open Elmario
module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // this specifies the window configuration used to display the game
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Elmario" }
        
        // this specifies the configuration of the game engine's use of SDL
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // use the default world config with the above SDL config
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // initialize Nu
        Nu.init worldConfig.NuConfig
        
        // run the engine with the given config and plugin
        World.run worldConfig (ElmarioPlugin ())