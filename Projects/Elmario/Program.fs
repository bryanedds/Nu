namespace Elmario
open System
open Prime
open Nu
open Elmario

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type ElmarioPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.GetStandAloneGameDispatcher () =
        typeof<ElmarioDispatcher>
        
    // this specifies the game dispatcher to use in the editor
    override this.GetEditorGameDispatcher () =
        typeof<ElmarioDispatcher>
            
    // this specifies no screen dispatcher is to be created by the editor
    override this.GetEditorScreenDispatcherOpt () =
        None
        
// this is the module where main is defined (the entry-point for your Nu application)
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