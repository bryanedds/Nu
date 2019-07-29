namespace BlazeVector
open System
open SDL2
open Prime
open Nu
open BlazeVector

/// Creates BlazeVector-specific values (here dispatchers and facets).
/// Allows BlazeVector simulation types to be created in the game as well as in Gaia.
type BlazePlugin () =
    inherit NuPlugin ()

    // make our game-specific game dispatcher...
    override this.MakeGameDispatchers () =
        [BlazeDispatcher () :> GameDispatcher]

    // make our game-specific screen dispatchers...
    override this.MakeScreenDispatchers () =
        [GameplayDispatcher () :> ScreenDispatcher]

    // make our game-specific layer dispatchers...
    override this.MakeLayerDispatchers () =
        [SceneDispatcher () :> LayerDispatcher]

    // make our game-specific entity dispatchers...
    override this.MakeEntityDispatchers () =
        [BulletDispatcher () :> EntityDispatcher
         PlayerDispatcher () :> EntityDispatcher
         EnemyDispatcher () :> EntityDispatcher]

    // specify the game dispatcher to use at run-time
    override this.GetStandAloneGameDispatcherName () =
        typeof<BlazeDispatcher>.Name
            
    // specify the sceen dispatcher to use in the editor
    override this.GetGameplayScreenDispatcherName () =
        typeof<GameplayDispatcher>.Name

module Program =

    // this the entry point for the BlazeVector application
    let [<EntryPoint; STAThread>] main _ =

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "BlazeVector" }
        
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
            let plugin = BlazePlugin ()

            // here is an attempt to make the world with the world configuration, the engine
            // plugin, and SDL dependencies.
            World.tryMake plugin sdlDeps worldConfig

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld worldConfig