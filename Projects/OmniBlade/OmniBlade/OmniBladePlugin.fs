// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

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
             ("DebugField", fun world -> Simulants.Game.SetModel (Field (Field.debug world)) world)
             ("DebugBattle", fun world -> Simulants.Game.SetModel (Field (Field.debugBattle world)) world)
             ("Slot1", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial Slot1 world)) world)
             ("Slot2", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial Slot2 world)) world)
             ("Slot3", fun world -> Simulants.Game.SetModel (Field (Field.loadOrInitial Slot3 world)) world)]