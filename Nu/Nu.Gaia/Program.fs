// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu.Gaia
open System
open Prime
open Nu

// TODO: increase warning level to 5.

module Program =

    let [<EntryPoint; STAThread>] main _ =
        Gaia.init ()
        Gaia.run ()