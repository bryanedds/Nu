namespace TerraFirma
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TerraFirmaPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq
            [("Splash", fun world -> Game.SetTerraFirma Splash world)
             ("Title", fun world -> Game.SetTerraFirma Title world)
             ("Credits", fun world -> Game.SetTerraFirma Credits world)
             ("Gameplay", fun world ->
                let world = Game.SetTerraFirma Gameplay world
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                world)]