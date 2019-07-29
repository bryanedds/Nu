// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Gaia
open System
open Prime
open Nu

// TODO: increase warning level to 5.

module Program =

    let [<EntryPoint; STAThread>] main _ =
        let worldConfig = { WorldConfig.defaultConfig with TickRate = 0L; StandAlone = false }
        Gaia.init worldConfig.NuConfig
        Gaia.run worldConfig