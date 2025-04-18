// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds.

namespace Nu.Gaia
open System
open Prime
open Nu
open Nu.Gaia

/// The default plugin used by Gaia when a plugin from a game is not utilized.
type GaiaPlugin () =
    inherit NuPlugin ()
    override this.AllowCodeReload = false