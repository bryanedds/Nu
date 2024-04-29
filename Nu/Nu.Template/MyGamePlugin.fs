namespace MyGame
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetMyGame Splash world)
             ("Title", fun world -> Game.SetMyGame Title world)
             ("Credits", fun world -> Game.SetMyGame Credits world)
             ("Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetMyGame Gameplay world
                world)]