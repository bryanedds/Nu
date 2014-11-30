namespace $safeprojectname$
open SDL2
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
module Program =
    
    // this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
    // sorts of values can be obtained by both your application and NuEdit. Currently, there are no
    // overrides for its factory methods since there are no user-defined dispatchers, facets, et al
    // defined for this project yet.
    type $safeprojectname$Plugin () =
        inherit NuPlugin ()

    // this the entry point for the your Nu application
    let [<EntryPoint>] main _ =

        // this initializes miscellaneous values required by the engine. This should always be the
        // first line in your game program.
        World.init ()

        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "$safeprojectname$".
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "$safeprojectname$"
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
            let nuPlugin = $safeprojectname$Plugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies. Note that the first boolean, when true, protects from
            // a Farseer physics bug but slows down the engine badly, so that's why it's false.
            World.tryMake false true GuiAndPhysicsAndGamePlay () nuPlugin sdlDeps

        // this is a callback that specifies your game's unique behavior when updating the world
        // every tick. The World value is the state of the world after the callback has transformed
        // the one it receives. It is here where we first clearly see Nu's purely-functional(ish)
        // design. The World type is almost entirely immutable, and thus the only way to update it
        // is by making a new copy of an existing instance. Since we need no special update
        // behavior in this program, we simply return the world as it was received.
        let updateWorld world = world

        // after some configuration it is time to run your game. We're off and running!
        World.run tryMakeWorld updateWorld sdlConfig