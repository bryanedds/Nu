// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open SDL2
open Nu
module Program =

    // this the entry point for the empty Nu application
    let [<EntryPoint>] main _ =
    
        // this initializes all the .Net TypeConverters that the game uses for serialization. This
        // should always be the first line in your game program.
        World.initTypeConverters ()
        
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "Nu Game Engine" and is placed at (32, 32) pixels from
        // the top left of the screen.
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "Nu Game Engine"
                  WindowX = 32
                  WindowY = 32
                  WindowFlags = SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN }
                  
        // this specifies the manner in which the game's rendering takes place. With this
        // configuration, rendering is hardware-accelerated and synchronized with the system's
        // vertical re-trace, making for fast and smooth rendering.
        let sdlRenderFlags =
            enum<SDL.SDL_RendererFlags>
                (int SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED |||
                 int SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC)
                 
        // this makes a configuration record with the specifications we set out above.
        let sdlConfig =
            Sdl.makeSdlConfig
                sdlViewConfig
                NuConstants.ResolutionX
                NuConstants.ResolutionY
                sdlRenderFlags
                NuConstants.AudioBufferSizeDefault

        // this is a callback that attempts to create 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as a complex record type named World.
        let tryCreateWorld sdlDeps =
            
            // Game dispatchers specify some unique, high-level behavior and data for your game.
            // Since this particular program has no unique behavior, the vanilla base class
            // GameDispatcher is used.            
            let gameDispatcher = GameDispatcher () :> obj
            
            // here is an attempt to create the world using SDL dependencies that will be created
            // from the invoking function using the SDL configuration that we defined above, the
            // gameDispatcher immediately above, and a value that could have been used to
            // user-defined data to the world had we needed it (we don't, so we pass unit).
            World.tryCreateEmptyWorld sdlDeps gameDispatcher ()
            
        // this is a callback that specifies your game's unique behavior when updating the world
        // every tick. Its return type is a (bool * World). The bool value is whether the program
        // should continue (true), or exit (false). The World value is the state of the world
        // after the callback has transformed the one it receives. It is here where we first clearly
        // see Nu's purely-functional(ish) design. The World type is almost entirely immutable, and
        // thus the only way to update it is by making a new copy of an existing instance. Since we
        // need no special update behavoir in this program, we simply return the world as it was
        // received.
        let updateWorld world =
            (true, world)

        // after some configuration it is time to run Nu. We're off and running!
        World.run tryCreateWorld updateWorld sdlConfig

    (* WISDOM: Program types and behavior should be closed where possible and open where necessary. *)

    (* WISDOM: From benchmarks. it looks like our mobile target will cost us anywhere from a 75% to 90%
    decrease in speed as compared to the dev machine. However, this can be mitigated in a few ways with
    approximate speed-ups -

    2x gain - Run app at 30fps instead of 60
    2x gain - put physics in another process
    1.5x gain - clip draw calls
    1.5x gain - put rendering in another process, perhaps with physics, and / or render with OpenGL directly
    1.3x gain - store loaded assets in a Dictionary<Dictionary, ...>> rather than a Map<Map, ...>>, or...
    1.3x gain - alternatively, use short-term memoization with a temporary dictionary to cache asset queries during rendering / playing / etc.
    1.2x gain - optimize locality of address usage
    1.2x gain - render tiles layers to their own buffer so that each whole layer can be blitted directly with a single draw call (though this might cause overdraw).
    ? gain - avoid rendering clear tiles! *)

    (*moduleProgram
    open System
    open Propagate

    type Data =
      { A : int
        B : byte }

    type DataRecording =
        | ARecording of int
        | BRecording of byte

    let setA setter =
        ((fun data -> let a2 = setter data.A in { data with A = a2 }),
         (fun data -> ARecording data.A))

    let setB setter =
        ((fun data -> let b2 = setter data.B in { data with B = b2 }),
         (fun data -> BRecording data.B))

    let [<EntryPoint>] main _ =

        Console.WriteLine (
            propagate 0 >.
            plus 2 >.
            mul 5)

        let propagatedData =
            propagate { A = 0; B = 0uy } >>.
            setA incI >>.
            setB incUy*)