// Zombie Game.
// Copyright (C) Bryan Edds.

namespace ZombieGame
open System
open System.Numerics
open Prime
open Nu

/// What acting would do to what the player faces, as "Space - Examine" - declared while there's something before it,
/// told how it's acted on.
type ActionPromptDispatcher () =
    inherit GuiDispatcherImSim ()

    static member Properties =
        [define Entity.InteractionType Examine]

    override this.Process (entity, world) =
        World.doText "Prompt"
            [Entity.Position .= v3 0.0f -150.0f 0.0f
             Entity.Size .= v3 200.0f 16.0f 0.0f
             Entity.Text @= "Enter - " + getCaseName (entity.GetInteractionType world)
             Entity.TextColor .= Color.White.WithA 0.7f
             Entity.FontSizing .= Constants.Gui.SmallFontSizing] world
