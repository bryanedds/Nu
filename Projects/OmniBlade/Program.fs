namespace OmniBlade
open System
open Nu
open OmniBlade
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "OmniBlade" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Nu.init worldConfig.NuConfig
        World.run worldConfig (OmniPlugin ())