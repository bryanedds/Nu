// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

type OmniBladePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofSeq
            [("Title", fun world -> Simulants.Game.SetModel (Gui Title) world)
             ("Credits", fun world -> Simulants.Game.SetModel (Gui Credits) world)
             ("Pick", fun world -> Simulants.Game.SetModel (Gui Pick) world)
             ("Field", fun world -> Simulants.Game.SetModel (Field (Field.initial (World.getUpdateTime world) Slot1 (World.getViewBounds2dAbsolute world))) world)
             ("FieldDebug", fun world -> Simulants.Game.SetModel (Field (Field.debug (World.getUpdateTime world) (World.getViewBounds2dAbsolute world))) world)
             ("BattleDebug", fun world -> Simulants.Game.SetModel (Field (Field.debugBattle (World.getUpdateTime world) (World.getViewBounds2dAbsolute world))) world)
             ("Slot1", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial (World.getUpdateTime world) Slot1 (World.getViewBounds2dAbsolute world))) world)
             ("Slot2", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial (World.getUpdateTime world) Slot2 (World.getViewBounds2dAbsolute world))) world)
             ("Slot3", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial (World.getUpdateTime world) Slot3 (World.getViewBounds2dAbsolute world))) world)]