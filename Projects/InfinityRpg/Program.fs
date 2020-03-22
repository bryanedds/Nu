open System
open Nu
open InfinityRpg

/// Creates InfinityRpg-specific values (here dispatchers and facets).
/// Allows InfinityRpg simulation types to be created in the game as well as in Gaia.
type InfinityRpgPlugin () =
    inherit NuPlugin ()

    // specify the game dispatcher to use at run-time
    override this.GetStandAloneGameDispatcher () =
        typeof<InfinityDispatcher>

    // route overlays to specific dispatchers
    override this.MakeOverlayRoutes () =
        [typeof<ButtonDispatcher>.Name, Some "InfinityButtonDispatcher"]

// this the entry point for the InfinityRpg application
let [<EntryPoint; STAThread>] main _ =

    // this specifies the window configuration used to display the game.
    let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Infinity RPG" }
        
    // this specifies the configuration of the game engine's use of SDL.
    let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

    // use the default world config with the above SDL config.
    let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

    // initialize Nu
    Nu.init worldConfig.NuConfig

    // this is a callback that attempts to make 'the world' in a functional programming
    // sense. In a Nu game, the world is represented as an abstract data type named World.
    let tryMakeWorld sdlDeps worldConfig =

        // an instance of the above plugin
        let plugin = InfinityRpgPlugin ()

        // here is an attempt to make the world with the the engine plugin, SDL dependencies,
        // and world configuration.
        World.tryMake plugin sdlDeps worldConfig

    // after some configuration it is time to run the game. We're off and running!
    World.run tryMakeWorld worldConfig