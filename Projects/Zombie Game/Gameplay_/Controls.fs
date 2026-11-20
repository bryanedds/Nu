// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// How to get about, shown along the top while the game's new.
type ControlsDispatcher () =
    inherit GuiDispatcherImSim ()

    override this.Process (_, world) =
        World.doText "Controls"
            [Entity.Position .= v3 0.0f 168.0f 0.0f
             Entity.Size .= v3 620.0f 12.0f 0.0f
             Entity.Text .= "W/S - Walk    A/D - Turn    Shift - Run    \\ - Aim    ] - Auto-Aim    Enter - Act/Fire    Shift+S - Turn Round    Tab - Inventory"
             Entity.TextColor .= Color.White.WithA 0.6f
             Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
