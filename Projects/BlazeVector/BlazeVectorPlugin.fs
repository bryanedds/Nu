namespace BlazeVector
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type BlazeVectorPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetBlazeVector Splash world)
             ("Title", fun world -> Game.SetBlazeVector Title world)
             ("Credits", fun world -> Game.SetBlazeVector Credits world)
             ("Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetBlazeVector Gameplay world
                world)]