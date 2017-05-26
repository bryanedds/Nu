namespace InfinityRpg
open System
open SDL2
open Nu
module Program =

    // this the entry point for the InfinityRpg application
    let [<EntryPoint; STAThread>] main _ =

        // initialize Nu
        Nu.init false


        // this specifies the general configuration of the game engine. With this configuration,
        // a new window is created with a title of "InfinityRpg".
        let sdlConfig =
            { ViewConfig = NewWindow { SdlWindowConfig.defaultConfig with WindowTitle = "InfinityRpg" }
              ViewW = Constants.Render.ResolutionX
              ViewH = Constants.Render.ResolutionY
              RendererFlags = Constants.Render.DefaultRendererFlags
              AudioChunkSize = Constants.Audio.DefaultBufferSize }

        // after some configuration it is time to run the game. We're off and running!
        World.run (fun sdlDeps -> World.attemptMake true 1L () (InfinityRpgPlugin ()) sdlDeps) id id sdlConfig