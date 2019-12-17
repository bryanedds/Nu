namespace MyGame
open System
open Nu

// this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
// sorts of types can be obtained by both your application and Gaia.
type MyPlugin () =
    inherit NuPlugin ()

    // make our game-specific screen dispatchers...
    override this.MakeScreenDispatchers () =
        [MyGameplayDispatcher () :> ScreenDispatcher]

    // make our game-specific game dispatchers...
    override this.MakeGameDispatchers () =
        [MyGameDispatcher () :> GameDispatcher]

    // specify the above game dispatcher to use at run-time
    override this.GetStandAloneGameDispatcherName () =
        typeof<MyGameDispatcher>.Name

    // specify the empty game dispatcher to use in the editor
    override this.GetEditorGameDispatcherName () =
        typeof<GameDispatcher>.Name

    // specify the sceen dispatcher to optionally use in the editor
    override this.GetEditorGameplayScreenDispatcherNameOpt () =
        Some typeof<MyGameplayDispatcher>.Name

// this is the main module for our program.
module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // use the default config
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // initialize Nu
        Nu.init worldConfig.NuConfig

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let tryMakeWorld sdlDeps worldConfig =

            // an instance of the above plugin
            let plugin = MyPlugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.tryMake plugin sdlDeps worldConfig

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld worldConfig