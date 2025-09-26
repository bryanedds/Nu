namespace SandBox2d
open System
open Nu
open SandBox2d

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type SandBox2dPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("ToyBox", fun world -> Game.SetGameState ToyBox world)
             ("RaceCourse", fun world -> Game.SetGameState RaceCourse world)]