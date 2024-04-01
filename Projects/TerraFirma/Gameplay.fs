namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module Gameplay =

    // this represents that state of gameplay simulation.
    type GameplayState =
        | Commencing
        | Commence
        | Quitting
        | Quit

    // this is our MMCC model type representing gameplay.
    type [<ReferenceEquality; SymbolicExpansion>] Gameplay =
        { GameplayState : GameplayState }

    // this is our MMCC message type.
    type GameplayMessage =
        | FinishCommencing
        | StartQuitting
        | FinishQuitting
        interface Message

    // this is our MMCC command type.
    type GameplayCommand =
        | SetupScene
        | CharacterAttacked of Entity
        | TrackPlayer
        | PlaySound of int64 * single * Sound AssetTag
        interface Command

    // this extends the Screen API to expose the Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> ({ GameplayState = Quit })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) =
            [Screen.SelectEvent => FinishCommencing
             Screen.DeselectingEvent => FinishQuitting
             Screen.PostUpdateEvent => TrackPlayer
             Events.CharacterAttackedEvent --> Simulants.GameplayScene --> Address.Wildcard =|> fun evt -> CharacterAttacked evt.Data]

        // here we handle the gameplay messages
        override this.Message (gameplay, message, _, _) =

            match message with
            | FinishCommencing ->
                let gameplay = { gameplay with GameplayState = Commence }
                withSignal SetupScene gameplay

            | StartQuitting ->
                let gameplay = { gameplay with GameplayState = Quitting }
                just gameplay

            | FinishQuitting ->
                let gameplay = { gameplay with GameplayState = Quit }
                just gameplay

        // here we handle the gameplay commands
        // notice how in here we handle events from characters to implement intra-character interactions rather than
        // the more complex approach of having characters talk to each other or handle each other's events.
        override this.Command (_, command, screen, world) =

            match command with
            | SetupScene ->
                let world = Simulants.GameplayPlayer.SetPosition (v3 0.0f 2.0f 0.0f) world
                let world = World.synchronizeNav3d screen world
                just world

            | CharacterAttacked attackedCharacter ->
                let character = attackedCharacter.GetCharacter world
                let character = { character with HitPoints = max (dec character.HitPoints) 0 }
                let (signals, character) =
                    if character.HitPoints > 0 then
                        match character.ActionState with
                        | InjuryState _ -> just character
                        | _ ->
                            let character = { character with ActionState = InjuryState { InjuryTime = world.UpdateTime }}
                            let playSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gameplay.InjureSound)
                            withSignal playSound character
                    else
                        let character = { character with ActionState = WoundedState }
                        let playSound = PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gameplay.InjureSound)
                        withSignal playSound character
                let world = attackedCharacter.SetCharacter character world
                withSignals signals world

            | TrackPlayer ->
                
                // update eye to look at player
                let player = Simulants.GameplayPlayer.GetCharacter world
                let position = Simulants.GameplayPlayer.GetPosition world
                let rotation = Simulants.GameplayPlayer.GetRotation world
                let positionInterp = player.PositionInterp position
                let rotationInterp = player.RotationInterp rotation * Quaternion.CreateFromAxisAngle (v3Right, -0.2f)
                let world = World.setEye3dCenter (positionInterp + v3Up * 1.75f - rotationInterp.Forward * 3.0f) world
                let world = World.setEye3dRotation rotationInterp world

                // update sun to shine over player
                let positionInterpFloor = positionInterp.MapX(MathF.Floor).MapY(MathF.Floor).MapZ(MathF.Floor)
                let world = Simulants.GameplaySun.SetPosition (positionInterpFloor + v3Up * 12.0f) world
                just world

            | GameplayCommand.PlaySound (delay, volume, sound) ->
                let world = World.schedule delay (World.playSound volume sound) screen world
                just world

        // here we describe the content of the game including the hud group and the scene group
        override this.Content (gameplay, _) =

            [// the gui group
             Content.group Simulants.GameplayGui.Name []

                [// quit
                 Content.button Simulants.GameplayQuit.Name
                    [Entity.Position == v3 224.0f -144.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while gameplay commences or quitting
             match gameplay.GameplayState with
             | Commence | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                    [// the player that's always present in the scene
                     Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                        [Entity.Persistent == false
                         Events.CharacterDieEvent --> Simulants.GameplayPlayer => StartQuitting]]

             // no scene group otherwise
             | Commencing | Quit -> ()]