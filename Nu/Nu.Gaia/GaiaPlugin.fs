// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

namespace Nu.Gaia
open System
open Prime
open Nu
open Nu.Gaia

/// The default plugin used by Gaia when a plugin from a game is not utilized.
type GaiaPlugin () =
    inherit NuPlugin ()
    override this.AllowCodeReload = false
    override this.EditContextOpt = Some (Gaia.makeContext None None)