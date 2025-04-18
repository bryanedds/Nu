﻿namespace Twenty48
open System
open Nu
open Twenty48

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type Twenty48Plugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetTwenty48 Splash world)
             ("Title", fun world -> Game.SetTwenty48 Title world)
             ("Credits", fun world -> Game.SetTwenty48 Credits world)
             ("Gameplay", fun world ->
                let world = Simulants.Gameplay.SetGameplay Gameplay.initial world
                let world = Game.SetTwenty48 Gameplay world
                world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName]