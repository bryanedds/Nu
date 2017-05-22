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
    override this.MakeGameDispatcher () =
        BlazeDispatcher () :> GameDispatcher

    // make our game-specific screen dispatchers...
    override this.MakeScreenDispatchers () =
        [GameplayScreenDispatcher () :> ScreenDispatcher]

    // make our game-specific layer dispatchers...
    override this.MakeLayerDispatchers () =
        [PlayerLayerDispatcher () :> LayerDispatcher]

    // make our game-specific entity dispatchers...
    override this.MakeEntityDispatchers () =
        [BulletDispatcher () :> EntityDispatcher
         PlayerDispatcher () :> EntityDispatcher
         EnemyDispatcher () :> EntityDispatcher]

module Program =

    // this the entry point for the BlazeVector application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu with synchronous code disabled
        Nu.init false
        
        // this specifies the general configuration of the game engine. With this configuration,
        // a new window is created with a title of "BlazeVector".
        let sdlConfig =
            { ViewConfig = NewWindow { SdlWindowConfig.defaultConfig with WindowTitle = "BlazeVector" }
              ViewW = Constants.Render.ResolutionX
              ViewH = Constants.Render.ResolutionY
              RendererFlags = Constants.Render.DefaultRendererFlags
              AudioChunkSize = Constants.Audio.DefaultBufferSize }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let attemptMakeWorld sdlDeps =

            // an instance of the above plugin
            let plugin = BlazePlugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.attemptMake true None 1L () plugin sdlDeps

        // after some configuration it is time to run the game. We're off and running!
        World.run attemptMakeWorld id id sdlConfig