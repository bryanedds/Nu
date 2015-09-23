namespace InfinityRpg
open System
open SDL2
open Nu
module Program =

    // this the entry point for the InfinityRpg application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init ()
    
        // this specifies the manner in which the game is viewed. With this configuration, a new
        // window is created with a title of "Infinity Rpg".
        let sdlViewConfig =
            NewWindow
                { WindowTitle = "Infinity Rpg"
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

        // after some configuration it is time to run the game. We're off and running!
        World.run
            (fun sdlDeps -> World.attemptMake true 1L () (InfinityRpgPlugin ()) sdlDeps)
            (fun world -> world)
            (fun world -> world)
            sdlConfig