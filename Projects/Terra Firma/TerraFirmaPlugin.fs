namespace TerraFirma
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type TerraFirmaPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetTerraFirma Splash world)
             ("Title", fun world -> Game.SetTerraFirma Title world)
             ("Credits", fun world -> Game.SetTerraFirma Credits world)
             ("Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetTerraFirma Gameplay world
                world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName]