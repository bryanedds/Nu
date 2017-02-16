// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu.Gaia
open Prime
open System
open Prime
open Nu

// TODO: increase warning level to 5.
// TODO: implement selection box rendering.

module Program =

    let [<EntryPoint; STAThread>] main _ =
        Nu.init false
        Gaia.run ()