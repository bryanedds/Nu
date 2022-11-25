namespace MyGame
open System
open Prime
open Nu
open Nu.Declarative
open MyGame

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Title", fun world -> Game.SetModel Title world)
             ("Credits", fun world -> Game.SetModel Credits world)
             ("Gameplay", fun world -> Game.SetModel (Gameplay Playing) world)]