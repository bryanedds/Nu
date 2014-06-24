namespace BlazeVector
open System
open SDL2
open Nu
open Nu.NuConstants
open BlazeVector
module Program =

    // this the entry point for the BlazeVector application
    let [<EntryPoint>] main _ =
    
        // this initializes all the .Net TypeConverters that the game uses for serialization. This
        // should always be the first line in your game program.
        World.initTypeConverters ()
        
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "BlazeVector" and is placed at (32, 32) pixels from the
        // top left of the screen.
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "BlazeVector"
                  WindowX = 32
                  WindowY = 32
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

        // after some configuration it is time to run the game. We're off and running!
        World.run
            (fun sdlDeps -> BlazeFlow.tryMakeBlazeVectorWorld sdlDeps ())
            (fun world -> world)
            sdlConfig