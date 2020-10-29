// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "OmniBlade" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }
        Nu.init worldConfig.NuConfig
        let (map, _) = Rand.makeFromInt 13 |> MapRand.makeFromRand 0.333333f 21 (v2iDup 7) OriginSW
        let tmx = MapRand.toTmx "./Assets/Field/Cave" map
        World.run worldConfig (OmniPlugin ())