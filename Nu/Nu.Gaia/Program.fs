// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.IO
open Prime
open Nu
module Program =

    let [<EntryPoint; STAThread>] main _ =
        Nu.init ()
        Directory.SetCurrentDirectory AppContext.BaseDirectory
        Gaia.run (GaiaPlugin ())