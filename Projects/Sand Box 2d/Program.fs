namespace SandBox2d
open System
open System.IO
open Nu
module Program =

    // this the entry point for your Nu application
    let main () =

        // regardless of where the program is launched from in the command line, always resolve files relative to the application's base directory
        // for Android, we set the current directory to the asset pack location in MainActivity, so we skip this step here
        if not (OperatingSystem.IsAndroid ()) then Directory.SetCurrentDirectory AppContext.BaseDirectory

        // this initializes Nu before other Nu code is run
        Nu.init ()

        // this specifies the window configuration used to display the game
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Sand Box (2D)" }

        // this specifies the configuration of the game engine's use of SDL
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }

        // this specifies the world config using the above SDL config
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // this runs the engine with the given config and plugin, starting the game
        World.run worldConfig (SandBox2dPlugin ())