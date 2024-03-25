namespace TerraFirma
open System
open Prime
open Nu

[<AutoOpen>]
module GameplayDispatcher =

    // this extends the Screen API to expose the Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Game.IntegrationEvent =|> fun evt -> UpdatePhysics evt.Data
             Game.KeyboardKeyDownEvent =|> fun evt -> UpdatePlayerInputKey evt.Data
             Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => TransformEye
             Screen.TimeUpdateEvent => TimeUpdate
             Screen.DeselectingEvent => FinishQuitting]

        // here we handle the above messages
        override this.Message (gameplay, message, _, world) =

            match message with
            | UpdatePhysics integrationData ->
                let gameplay = Gameplay.updatePhysics integrationData gameplay world
                just gameplay

            | UpdatePlayerInputKey keyboardKeyData ->
                let (signals, gameplay) = Gameplay.updatePlayerInputKey keyboardKeyData gameplay
                withSignals signals gameplay

            | Update ->
                let gameplay = Gameplay.update gameplay world
                just gameplay

            | TimeUpdate ->
                let gameplay = { gameplay with GameplayTime = inc gameplay.GameplayTime }
                just gameplay

            | StartQuitting ->
                let gameplay = { gameplay with GameplayState = Quitting }
                just gameplay

            | FinishQuitting ->
                let gameplay = { gameplay with GameplayState = Quit }
                just gameplay

        // here we handle the above commands
        override this.Command (gameplay, command, _, world) =

            match command with
            | JumpPlayer ->
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                let world = World.jumpBody true Constants.Gameplay.PlayerJumpSpeed bodyId world
                just world

            | TransformEye ->
                let positionInterp = gameplay.Player.PositionInterp
                let rotationInterp = gameplay.Player.RotationInterp
                let world = World.setEye3dCenter (positionInterp + v3Up * 1.5f - rotationInterp.Forward * 3.0f) world
                let world = World.setEye3dRotation rotationInterp world
                just world

        // here we describe the content of the game including the hud, the scene, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while playing or quitting
             match gameplay.GameplayState with
             | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                    [// player
                     Content.composite<CharacterDispatcher> Simulants.GameplayPlayer.Name
                        [Entity.Character := gameplay.Player]
                        [Content.entity<WeaponDispatcher> Simulants.GameplayPlayerWeapon.Name
                            [Entity.PositionLocal := gameplay.Player.WeaponHand.Translation
                             Entity.RotationLocal := gameplay.Player.WeaponHand.Rotation
                             Entity.ScaleLocal := v3Dup 0.1f]]

                     // enemies
                     for (enemyId, enemy) in gameplay.Enemies.Pairs do
                        Content.composite<CharacterDispatcher> (string enemyId)
                            [Entity.Character := enemy]
                            [Content.entity<WeaponDispatcher> (Simulants.GameplayEnemyWeapon enemyId).Name
                                [Entity.PositionLocal := enemy.WeaponHand.Translation
                                 Entity.RotationLocal := enemy.WeaponHand.Rotation
                                 Entity.ScaleLocal := v3Dup 0.1f]]]

             | Quit -> ()]