namespace MyGame
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module MyGameplay =

    // this is our Elm-style model type. Either we're playing or we're quitting back to the title screen.
    type Gameplay =
        | Playing
        | Quitting
        | Quit

    // this is our Elm-style message type.
    type GameplayMessage =
        | StartQutting
        | FinishQuitting

    // this is our Elm-style command type. Commands are used instead of messages when things like physics are involved.
    type GameplayCommand =
        | Update
        | PostUpdateEye
        | Jump
        | Nop

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        static member Gameplay = Screen.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place
    type MyGameplayDispatcher () =
        inherit ScreenForger<Gameplay, GameplayMessage, GameplayCommand> (Quit)

        // here we handle the above messages
        override this.Message (_, message, _, _) =
            match message with
            | StartQutting -> just Quitting
            | FinishQuitting -> just Quit

        // here we handle the above commands
        override this.Command (_, command, _, world) =
            match command with
            | Update ->
                let physicsId = Simulants.Gameplay.Player.Character.GetPhysicsId world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Left world then
                        if World.isBodyOnGround physicsId world
                        then World.applyBodyForce (v3 -2500.0f 0.0f 0.0f) physicsId world
                        else World.applyBodyForce (v3 -750.0f 0.0f 0.0f) physicsId world
                    elif World.isKeyboardKeyDown KeyboardKey.Right world then
                        if World.isBodyOnGround physicsId world
                        then World.applyBodyForce (v3 2500.0f 0.0f 0.0f) physicsId world
                        else World.applyBodyForce (v3 750.0f 0.0f 0.0f) physicsId world
                    else world
                just world
            | Jump ->
                let physicsId = Simulants.Gameplay.Player.Character.GetPhysicsId world
                if World.isBodyOnGround physicsId world then
                    let world = World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                    let world = World.applyBodyForce (v3 0.0f 140000.0f 0.0f) physicsId world
                    just world
                else just world
            | PostUpdateEye ->
                if World.getAdvancing world then
                    let characterCenter = Simulants.Gameplay.Player.Character.GetCenter world
                    let world = World.setEyePosition2d characterCenter.V2 world
                    just world
                else just world
            | Nop -> just world

        // here we describe the content of the game including the level, the hud, and the player
        override this.Forge (gameplay, _) =

            // the gameplay screen
            Forge.screen Simulants.Gameplay.Screen.Name Vanilla
                [Screen.UpdateEvent ==> cmd Update
                 Screen.PostUpdateEvent ==> cmd PostUpdateEye
                 Screen.DeselectingEvent ==> msg FinishQuitting
                 Simulants.Game.KeyboardKeyDownEvent ==|> fun evt -> if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated then cmd Jump else cmd Nop
                 Simulants.Gameplay.Gui.Quit.ClickEvent ==> msg StartQutting]

                [// the gui group
                 yield Forge.group Simulants.Gameplay.Gui.Group.Name []
                     [Forge.button Simulants.Gameplay.Gui.Quit.Name
                         [Entity.Text <== "Quit"
                          Entity.Position <== v3 260.0f -260.0f 0.0f
                          Entity.Elevation <== 10.0f
                          Entity.ClickEvent ==> msg Quit]]

                 // the player and scene groups while playing
                 match gameplay with
                 | Playing | Quitting ->
                    yield Forge.group Simulants.Gameplay.Player.Group.Name []
                        [Forge.sideViewCharacter Simulants.Gameplay.Player.Character.Name
                            [Entity.Position <== v3 0.0f 0.0f 0.0f
                             Entity.Size <== v3 108.0f 108.0f 0.0f]]
                    yield Forge.groupFromFile Simulants.Gameplay.Scene.Group.Name "Assets/Gameplay/Scene.nugroup" [] []
                 | Quit -> ()]