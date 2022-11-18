// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Tactics
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TacticsPlugin () =
    inherit NuPlugin ()

    override this.Modes =
        Map.ofSeq
            [("Title", fun world -> Simulants.Game.SetModel (Gui Title) world)
             ("Credits", fun world -> Simulants.Game.SetModel (Gui Credits) world)
             ("Pick", fun world -> Simulants.Game.SetModel (Gui Pick) world)
             ("Field", fun world -> Simulants.Game.SetModel (Atlas (Atlas.debug world)) world)]