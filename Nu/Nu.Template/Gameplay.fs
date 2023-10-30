namespace MyGame
open System
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of the simulation during gameplay.
    type GameplayState =
        | Playing
        | Quitting
        | Quit

    // this is our MMCC model type representing gameplay.
    type Gameplay =
        { Score : int
          State : GameplayState }

    // this is our MMCC message type.
    type GameplayMessage =
        | StartQutting
        | FinishQuitting
        interface Message

    // this is our MMCC command type. Commands are used instead of messages when the world is to be
    // transformed.
    type GameplayCommand =
        | Update
        | UpdateEye
        | Jump
        | Nop
        interface Command

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> ({ Score = 0; State = Quit })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => UpdateEye
             Screen.DeselectingEvent => FinishQuitting
             Game.KeyboardKeyDownEvent =|> fun evt ->
                if evt.Data.KeyboardKey = KeyboardKey.Up && not evt.Data.Repeated
                then Jump
                else Nop]

        // here we handle the above messages
        override this.Message (gameplay, message, _, _) =
            match message with
            | StartQutting -> just { gameplay with State = Quitting }
            | FinishQuitting -> just { gameplay with State = Quit }

        // here we handle the above commands
        override this.Command (_, command, _, world) =
            match command with
            | Update ->
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                let world =
                    if World.isKeyboardKeyDown KeyboardKey.Left world then
                        if World.getBodyGrounded bodyId world
                        then World.applyBodyForce (v3 -2500.0f 0.0f 0.0f) v3Zero bodyId world
                        else World.applyBodyForce (v3 -750.0f 0.0f 0.0f) v3Zero bodyId world
                    elif World.isKeyboardKeyDown KeyboardKey.Right world then
                        if World.getBodyGrounded bodyId world
                        then World.applyBodyForce (v3 2500.0f 0.0f 0.0f) v3Zero bodyId world
                        else World.applyBodyForce (v3 750.0f 0.0f 0.0f) v3Zero bodyId world
                    else world
                just world
            | Jump ->
                let bodyId = Simulants.GameplayPlayer.GetBodyId world
                if world.Advancing && World.getBodyGrounded bodyId world then
                    let world = World.playSound Constants.Audio.SoundVolumeDefault (asset "Gameplay" "Jump") world
                    let world = World.applyBodyLinearImpulse (v3 0.0f 2300.0f 0.0f) v3Zero bodyId world
                    just world
                else just world
            | UpdateEye ->
                if world.Advancing then
                    let characterCenter = Simulants.GameplayPlayer.GetCenter world
                    let world = World.setEyeCenter2d characterCenter.V2 world
                    just world
                else just world
            | Nop -> just world

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []
                [Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQutting]]

             // the scene group while playing or quitting
             match gameplay.State with
             | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []
                    [Content.sideViewCharacter Simulants.GameplayPlayer.Name
                        [Entity.Position == v3 0.0f 24.0f 0.0f
                         Entity.Size == v3 108.0f 108.0f 0.0f]]
             | Quit -> ()]