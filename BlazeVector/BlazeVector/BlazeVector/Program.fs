namespace BlazeVector
open System
open OpenTK
open SDL2
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open BlazeVector
module Program =

    /// Creates BlazeVector-specific values (here dispatchers and facets).
    /// Allows BlazeVector simulation types to be created in the game as well as in NuEdit.
    type BlazePlugin () =
        inherit NuPlugin ()

        // make our game-specific game dispatcher...
        override this.MakeOptGameDispatcher () =
            Some (BlazeDispatcher () :> GameDispatcher)

        // make our game-specific screen dispatchers...
        override this.MakeScreenDispatchers () =
            [GameplayScreenDispatcher () :> ScreenDispatcher]

        // make our game-specific group dispatchers...
        override this.MakeGroupDispatchers () =
            [PlayerGroupDispatcher () :> GroupDispatcher]

        // make our game-specific entity dispatchers...
        override this.MakeEntityDispatchers () =
            [BulletDispatcher () :> EntityDispatcher
             PlayerDispatcher () :> EntityDispatcher
             EnemyDispatcher () :> EntityDispatcher]

    // this the entry point for the BlazeVector application
    let [<EntryPoint; STAThread>] main _ =
    
        // this initializes miscellaneous values required by the engine. This should always be the
        // first line in your game program.
        World.init ()
        
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "BlazeVector".
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "BlazeVector"
                  WindowX = SDL.SDL_WINDOWPOS_UNDEFINED
                  WindowY = SDL.SDL_WINDOWPOS_UNDEFINED
                  WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }
                  
        // this specifies the manner in which the game's rendering takes place. With this
        // configuration, rendering is hardware-accelerated and synchronized with the system's
        // vertical re-trace, making for fast and smooth rendering.
        let sdlRendererFlags =
            enum<SDL.SDL_RendererFlags>
                (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
                 int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)

        // this makes a configuration record with the specifications we set out above.
        let sdlConfig =
            { ViewConfig = sdlViewConfig
              ViewW = ResolutionX
              ViewH = ResolutionY
              RendererFlags = sdlRendererFlags
              AudioChunkSize = AudioBufferSizeDefault }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as a complex record type named World.
        let tryMakeWorld sdlDeps =

            // an instance of the above plugin
            let nuPlugin = BlazePlugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies. Note that the first boolean, when true, protects from
            // a Farseer physics bug but slows down the engine badly, so that's why it's false.
            World.tryMake false true 1L () nuPlugin sdlDeps

        // this is a callback that specifies your game's unique behavior when updating the world
        // every update. The World value is the state of the world after the callback transforms
        // the one it receives. It is here where we first clearly see Nu's purely-functional(ish)
        // design. The World type is almost entirely immutable, and thus the only way to update it
        // is by making a new copy of an existing instance. Since we need no special update
        // behavior in this program, we simply return the world as it was received.
        let updateWorld world = world

        // after some configuration it is time to run the game. We're off and running!
        World.run tryMakeWorld updateWorld sdlConfig