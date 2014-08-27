namespace NuTemplate
open SDL2
open Nu
open Nu.NuConstants
module Program =

    // this the entry point for the empty Nu application
    let [<EntryPoint>] main _ =
    
        // this initializes miscellaneous values required by the engine. This should always be the
        // first line in your game program.
        World.init ()
        
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "Nu Game Engine".
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "Nu Game Engine"
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
            
            // Game dispatchers specify some unique, high-level behavior and data for your game.
            // Since this particular program has no unique behavior, the vanilla base class
            // GameDispatcher is used.            
            let gameDispatcher = GameDispatcher () :> obj
            
            // here is an attempt to make the world using SDL dependencies that will be created
            // from the invoking function using the SDL configuration that we defined above, the
            // gameDispatcher immediately above, and a value that could have been used to
            // user-defined data to the world had we needed it (we don't, so we pass unit).
            World.tryMakeEmpty sdlDeps gameDispatcher GuiAndPhysicsAndGamePlay false ()
            
        // this is a callback that specifies your game's unique behavior when updating the world
        // every tick. The World value is the state of the world after the callback has transformed
        // the one it receives. It is here where we first clearly see Nu's purely-functional(ish)
        // design. The World type is almost entirely immutable, and thus the only way to update it
        // is by making a new copy of an existing instance. Since we need no special update
        // behavior in this program, we simply return the world as it was received.
        let updateWorld world = world

        // after some configuration it is time to run Nu. We're off and running!
        World.run tryMakeWorld updateWorld sdlConfig