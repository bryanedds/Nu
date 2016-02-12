// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu.Gaia
open Prime
open System
open Prime
open Nu

// TODO: increase warning level to 5.
// TODO: implement quick size on create.
// TODO: implement selection box rendering.

module Program =

    let [<EntryPoint; STAThread>] main _ =
        Nu.init false
        Gaia.run ()