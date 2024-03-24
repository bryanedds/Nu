namespace TerraFirma
open System
open Prime
open Nu

[<AutoOpen>]
module GameplayDispatcher =

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
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
            | UpdatePlayerInputKey keyboardKeyData ->
                let (signals, gameplay) = Gameplay.updatePlayerInputKey keyboardKeyData gameplay
                withSignals signals gameplay

            | UpdatePhysics integrationData ->
                let gameplay = Gameplay.updatePhysics integrationData gameplay world
                just gameplay

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
                let world = World.setEye3dCenter (gameplay.Player.Position + v3Up * 1.5f - gameplay.Player.Rotation.Forward * 3.0f) world
                let world = World.setEye3dRotation gameplay.Player.Rotation world
                just world

        // here we describe the content of the game including the level, the hud, and the player
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
                     Content.entity<CharacterDispatcher> Simulants.GameplayPlayer.Name
                        [Entity.Character := gameplay.Player]

                     // enemies
                     for (enemyId, enemy) in gameplay.Enemies.Pairs do
                        Content.entity<CharacterDispatcher> (string enemyId)
                            [Entity.Character := enemy]]

             | Quit -> ()]