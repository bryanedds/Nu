namespace Nelmish
open System
open Prime
open Nu
open Nelmish

// this is a plugin for the Nu game engine that directs the execution of your application and the
// editor.
type NelmishPlugin () =
    inherit NuPlugin ()
    
    // this specifies the game dispatcher to use in your application
    override this.GetStandAloneGameDispatcher () =
        typeof<NelmishDispatcher>
        
    // this specifies the game dispatcher to use in the editor
    override this.GetEditorGameDispatcher () =
        typeof<NelmishDispatcher>
            
    // this specifies no screen dispatcher to be used by the editor
    override this.GetEditorScreenDispatcherOpt () =
        None

// this is the module where main is defined (the entry-point for your application)
module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Nelmish" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // use the default world config with the above SDL config.
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // initialize Nu.
        Nu.init worldConfig.NuConfig

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let tryMakeWorld sdlDeps worldConfig =

            // an instance of the above plugin.
            let plugin = NelmishPlugin ()

            // here is an attempt to make the world with the the engine plugin, SDL dependencies,
            // and world configuration.
            World.tryMake plugin sdlDeps worldConfig

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld worldConfig