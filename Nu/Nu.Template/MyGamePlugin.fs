namespace MyGame
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Title", fun world -> Simulants.Game.SetModel Title world)
             ("Credits", fun world -> Simulants.Game.SetModel Credits world)
             ("Gameplay", fun world -> Simulants.Game.SetModel (Gameplay Playing) world)]