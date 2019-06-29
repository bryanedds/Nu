namespace Elmario
open System
open Prime
open Nu
open Elmario
module Program =

    // this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
    // sorts of types can be obtained by both your application and Gaia.
    type ElmarioPlugin () =
        inherit NuPlugin ()

        // make our game-specific game dispatcher...
        override this.MakeGameDispatchers () =
            [ElmarioDispatcher () :> GameDispatcher]

        // specify the above game dispatcher to use
        override this.GetStandAloneGameDispatcherName () =
            typeof<ElmarioDispatcher>.Name

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init false

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Elmario" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let tryMakeWorld sdlDeps =

            // an instance of the above plugin
            let plugin = ElmarioPlugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.tryMake true 1L () plugin sdlDeps

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld id id sdlConfig