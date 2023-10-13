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
            [("Splash", fun world -> Game.SetModel (Gui Splash) world)
             ("Title", fun world -> Game.SetModel (Gui Title) world)
             ("Credits", fun world -> Game.SetModel (Gui Credits) world)
             ("Pick", fun world -> Game.SetModel (Gui Pick) world)
             ("Field", fun world -> Game.SetModel (Field (Field.initial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot1)) world)
             ("FieldDebug", fun world -> Game.SetModel (Field (Field.debug world.UpdateTime (World.getViewBounds2dAbsolute world))) world)
             ("BattleDebug", fun world -> Game.SetModel (Field (Field.debugBattle world.UpdateTime (World.getViewBounds2dAbsolute world))) world)
             ("Slot1", fun world -> Game.SetModel (Field (Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot1)) world)
             ("Slot2", fun world -> Game.SetModel (Field (Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot2)) world)
             ("Slot3", fun world -> Game.SetModel (Field (Field.loadOrInitial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot3)) world)]