// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.IO
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        let (gaiaState, targetDir, plugin) = Nu.initPlus (fun () -> Gaia.selectNuPlugin (GaiaPlugin ()))
        Gaia.run gaiaState targetDir plugin