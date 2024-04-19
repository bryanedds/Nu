// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open Prime
open Nu

type OmniBladePlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetModel Splash world)
             ("Title", fun world -> Game.SetModel Title world)
             ("Credits", fun world -> Game.SetModel Credits world)
             ("Pick", fun world -> Game.SetModel Pick world)
             ("Field", fun world -> let world = Game.SetModel Field world in Simulants.Field.SetField (Field.initial world.UpdateTime (World.getViewBounds2dAbsolute world) Slot1) world)
             ("FieldDebug", fun world -> let world = Game.SetModel Field world in Simulants.Field.SetField (Field.debug world.UpdateTime (World.getViewBounds2dAbsolute world)) world)]