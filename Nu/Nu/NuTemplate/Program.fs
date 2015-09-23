namespace $safeprojectname$
open System
open FSharpx
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Observation
open Nu.Chain

// this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
// sorts of values can be obtained by both your application and NuEdit. Currently, there are no
// overrides for its factory methods since there are no user-defined dispatchers, facets, et al
// defined for this project yet.
type $safeprojectname$Plugin () =
    inherit NuPlugin ()
    
// this is the main module for our program.
module Program =

    // this the entry point for the your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init ()

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
              ViewW = Constants.Render.ResolutionX
              ViewH = Constants.Render.ResolutionY
              RendererFlags = sdlRendererFlags
              AudioChunkSize = Constants.Audio.AudioBufferSizeDefault }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let attemptMakeWorld sdlDeps =

            // an instance of the above plugin
            let plugin = $safeprojectname$Plugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.attemptMake true 1L () plugin sdlDeps

        // this is a callback that specifies your game's unique behavior when updating the world
        // every frame. The World value is the state of the world after the callback transforms
        // the one it receives. It is here where we first clearly see Nu's purely-functional
        // design. The World type is immutable, and thus the only way to update it is by making 
        // a new copy of an existing instance. Since we need no special update behavior in this
        // program, we simply return the world as it was received.
        let updateWorld world = world

        // similar to the above, but for rendering. Most games probably won't do anything here.
        let renderWorld world = world

        // after some configuration it is time to run the game. We're off and running!
        World.run attemptMakeWorld updateWorld renderWorld sdlConfig