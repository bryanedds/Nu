// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Tactics
open System
open Prime
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TacticsPlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetModel (Gui Splash) world)
             ("Title", fun world -> Game.SetModel (Gui Title) world)
             ("Credits", fun world -> Game.SetModel (Gui Credits) world)
             ("Pick", fun world -> Game.SetModel (Gui Pick) world)
             ("Field", fun world -> Game.SetModel (Atlas (Atlas.debug world)) world)]