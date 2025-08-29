namespace Physics2D
open System
open Nu
open Physics2D

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type Physics2DPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [(nameof D01_SingleFixture, Game.SetGameState D01_SingleFixture)]