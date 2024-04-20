namespace Twenty48
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type Twenty48Plugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetTwenty48 Splash world)
             ("Title", fun world -> Game.SetTwenty48 Title world)
             ("Credits", fun world -> Game.SetTwenty48 Credits world)
             ("Gameplay", fun world -> Game.SetTwenty48 Gameplay world)]