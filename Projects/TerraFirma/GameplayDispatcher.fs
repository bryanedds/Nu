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
             Screen.SelectEvent => SynchronizeNav3d
             Screen.DeselectingEvent => FinishQuitting]

        // here we handle the gameplay messages
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
                let gameplay = Gameplay.timeUpdate gameplay
                just gameplay

            | StartQuitting ->
                let gameplay = { gameplay with GameplayState = Quitting }
                just gameplay

            | FinishQuitting ->
                let gameplay = { gameplay with GameplayState = Quit }
                just gameplay

        // here we handle the gameplay commands
        override this.Command (gameplay, command, screen, world) =

            match command with
            | SynchronizeNav3d ->
                if world.Unaccompanied // only synchronize if outside editor
                then just (World.synchronizeNav3d screen world)
                else just world

            | JumpPlayer ->
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                let world = World.jumpBody true gameplay.Player.JumpSpeed bodyId world
                just world

            | TransformEye ->
                let positionInterp = gameplay.Player.PositionInterp
                let rotationInterp = gameplay.Player.RotationInterp
                let world = World.setEye3dCenter (positionInterp + v3Up * 1.5f - rotationInterp.Forward * 3.0f) world
                let world = World.setEye3dRotation rotationInterp world
                just world

        // here we describe the content of the game including the hud group and the scene group
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

                    [// characters
                     for (characterId, character) in gameplay.Characters.Pairs do
                        Content.entity<CharacterDispatcher> (string characterId.SubId)
                            [Entity.Character := character]]

             // no scene group
             | Quit -> ()]