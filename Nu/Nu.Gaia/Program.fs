// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open Prime
open Nu
module Program =

    // TODO: increase warning level to 5.
    let [<EntryPoint; STAThread>] main _ =
        let nuConfig = { NuConfig.defaultConfig with StandAlone = false }
        Gaia.init nuConfig
        Gaia.run nuConfig