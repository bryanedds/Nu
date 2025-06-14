namespace MyGame
open System
open Nu
open MyGame

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetMyGame Splash world)
             ("Title", fun world -> Game.SetMyGame Title world)
             ("Credits", fun world -> Game.SetMyGame Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetMyGame Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]