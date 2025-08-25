namespace Breakout
open System
open System.IO
open Nu
open type WorldConfig
module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // this points the current working directory at application's base directory
        Directory.SetCurrentDirectory AppContext.BaseDirectory

        // this initializes Nu before other Nu code is run
        Nu.init ()

        // this specifies the configuration used to display the game
        let worldConfig = { defaultConfig with WorldConfig.SdlConfig.WindowConfig.WindowTitle = "Breakout ImSim" }

        // this runs the engine with the given config and plugin, starting the game
        World.run worldConfig (BreakoutPlugin ())