namespace MyGame
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Sandbox", fun world -> Game.SetMyGame Sandbox world)]