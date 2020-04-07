// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open Prime
open Nu
module Program =

    // TODO: increase warning level to 5.

    let [<EntryPoint; STAThread>] main _ =
        let worldConfig = { WorldConfig.defaultConfig with TickRate = 0L; StandAlone = false }
        Gaia.init worldConfig.NuConfig
        Gaia.run worldConfig