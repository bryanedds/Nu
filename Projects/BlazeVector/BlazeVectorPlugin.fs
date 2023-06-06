namespace BlazeVector
open System
open Prime
open Nu
open BlazeVector

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type BlazeVectorPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Title", fun world -> Simulants.Game.SetModel Title world)
             ("Credits", fun world -> Simulants.Game.SetModel Credits world)
             ("Gameplay", fun world -> Simulants.Game.SetModel (Gameplay { GameplayState = Playing; Score = 0 }) world)]