namespace InfinityRpg
open SDL2
open Prime
open Nu
open Nu.Constants
module Program =

    // this is a factory that creates user-defined components such as dispatchers of various sorts
    // and facets. Currently, there are no overrides for its factory methods since there are no
    // user-defined dispatchers defined yet for this project.
    type InfinityRpgComponentFactory () =
        inherit UserComponentFactory ()

    // this the entry point for the your Nu application
    let [<EntryPoint>] main _ =
    
        // this initializes miscellaneous values required by the engine. This should always be the
        // first line in your game program.
        World.init ()
        
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "InfinityRpg".
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "InfinityRpg"
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

            // A component factory is the means by which user-defined dispatchers and facets are
            // made available for creation by the engine, as well as by NuEdit. Note that any
            // user-defined game dispatcher returned from a user-defined component factory will
            // be used as your game's dispatcher.
            let userComponentFactory = InfinityRpgComponentFactory ()
            
            // here is an attempt to make the world using SDL dependencies that will be created
            // from the invoking function using the SDL configuration that we defined above, the
            // component factory above, a boolean that protects from a Farseer physics bug but
            // slows down the engine badly, and a value that could have been used to user-defined
            // data to the world had we needed it (we don't, so we pass unit).
            World.tryMake sdlDeps userComponentFactory UIAndPhysicsAndGamePlay false ()
            
        // this is a callback that specifies your game's unique behavior when updating the world
        // every tick. The World value is the state of the world after the callback has transformed
        // the one it receives. It is here where we first clearly see Nu's purely-functional(ish)
        // design. The World type is almost entirely immutable, and thus the only way to update it
        // is by making a new copy of an existing instance. Since we need no special update
        // behavior in this program, we simply return the world as it was received.
        let updateWorld world = world

        // after some configuration it is time to run your game. We're off and running!
        World.run tryMakeWorld updateWorld sdlConfig