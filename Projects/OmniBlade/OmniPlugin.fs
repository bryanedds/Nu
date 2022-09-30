// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open Prime
open Nu
open OmniBlade

type OmniPlugin () =
    inherit NuPlugin ()

    override this.Modes =
        Map.ofSeq
            ["Title", fun world -> Simulants.Game.SetModel (Gui Title) world
             "Credits", fun world -> Simulants.Game.SetModel (Gui Credits) world
             "Pick", fun world -> Simulants.Game.SetModel (Gui Pick) world
             "Field", fun world -> Simulants.Game.SetModel (Field (Field.debug world)) world
             "Battle", fun world -> Simulants.Game.SetModel (Field (Field.debugBattle world)) world]