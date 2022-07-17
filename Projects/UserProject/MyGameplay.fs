namespace MyGame
open System.Numerics
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module MyGameplay =

    // this is our Elm-style model type. Either we're playing or we're quitting back to the title screen.
    type Gameplay =
        | Playing
        | Quitting

    // this is our Elm-style message type.
    type GameplayMessage =
        | Quit

    // this is our Elm-style command type. Commands are used instead of messages when things like physics are involved.
    type GameplayCommand =
        | Jump
        | MoveLeft
        | MoveRight
        | RotateSpheres
        | UpdateEye
        | Nop

    // this extends the Screen API to expose the above model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place
    type MyGameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Quitting)

        // here we channel from events to signals
        override this.Channel (_, _) =
            [Simulants.Game.KeyboardKeyDownEvent =|> fun evt ->
                if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated then cmd Jump
                else cmd Nop
             Simulants.Gameplay.Screen.UpdateEvent =|> fun _ ->
                if KeyboardState.isKeyDown KeyboardKey.Left then cmd MoveLeft
                elif KeyboardState.isKeyDown KeyboardKey.Right then cmd MoveRight
                else cmd Nop
             Simulants.Gameplay.Screen.UpdateEvent => cmd RotateSpheres
             Simulants.Gameplay.Screen.PostUpdateEvent => cmd UpdateEye]

        // here we handle the above messages
        override this.Message (_, message, _, _) =
            match message with
            | Quit -> just Quitting

        // here we handle the above commands
        override this.Command (_, command, _, world) =
            let world =
                match command with
                | Jump ->
                    let physicsId = Simulants.Gameplay.Player.Character.GetPhysicsId world
                    if World.isBodyOnGround physicsId world then
                        let world = World.applyBodyForce (v3 0.0f 140000.0f 0.0f) physicsId world
                        World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                    else world
                | MoveLeft ->
                    let physicsId = Simulants.Gameplay.Player.Character.GetPhysicsId world
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 -2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 -750.0f 0.0f 0.0f) physicsId world
                | MoveRight ->
                    let physicsId = Simulants.Gameplay.Player.Character.GetPhysicsId world
                    if World.isBodyOnGround physicsId world
                    then World.applyBodyForce (v3 2500.0f 0.0f 0.0f) physicsId world
                    else World.applyBodyForce (v3 750.0f 0.0f 0.0f) physicsId world
                | RotateSpheres ->
                    let sphereLeft = Simulants.Gameplay.Scene.SphereLeft
                    let sphereRight = Simulants.Gameplay.Scene.SphereRight
                    let world = sphereLeft.SetRotation (sphereLeft.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, 0.0025f)) world
                    let world = sphereRight.SetRotation (sphereRight.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, 0.0025f)) world
                    world
                | UpdateEye ->
                    if World.getUpdateRate world <> 0L then
                        let characterCenter = Simulants.Gameplay.Player.Character.GetCenter world
                        Simulants.Game.SetEyePosition2d characterCenter.V2 world
                    else world
                | Nop -> world
            just world

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (_, screen) =

            [// the gui group
             Content.group Simulants.Gameplay.Gui.Group.Name []
                 [Content.button Simulants.Gameplay.Gui.Quit.Name
                     [Entity.Text == "Quit"
                      Entity.Position == v3 260.0f -260.0f 0.0f
                      Entity.Elevation == 10.0f
                      Entity.ClickEvent ==> msg Quit]]

             // the player group
             Content.groupIfScreenSelected screen $ fun _ _ ->
                Content.group Simulants.Gameplay.Player.Group.Name []
                    [Content.sideViewCharacter Simulants.Gameplay.Player.Character.Name
                        [Entity.Position == v3 0.0f 0.0f 0.0f
                         Entity.Size == v3 108.0f 108.0f 0.0f]]

             // the scene group
             Content.groupIfScreenSelected screen $ fun _ _ ->
                Content.groupFromFile Simulants.Gameplay.Scene.Group.Name "Assets/Gameplay/Scene.nugroup"]