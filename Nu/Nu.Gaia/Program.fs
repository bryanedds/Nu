// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

namespace Nu.Gaia
open System
open System.IO
open Prime
open Nu
open Nu.Gaia
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        let (gaiaState, targetDir, plugin) = Nu.initPlus (fun () -> Gaia.selectNuPlugin (GaiaPlugin ()))
        Gaia.run gaiaState targetDir plugin