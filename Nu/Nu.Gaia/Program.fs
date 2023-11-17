// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.IO
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main _ =
        let nuConfig = { NuConfig.defaultConfig with Accompanied = true }
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Nu.init nuConfig
        Gaia.run nuConfig (GaiaPlugin ())