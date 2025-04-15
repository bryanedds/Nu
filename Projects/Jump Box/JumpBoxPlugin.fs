﻿namespace JumpBox
open System
open Nu
open JumpBox

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type JumpBoxPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Initial", fun world -> Game.SetCollisions 0 world)]