namespace TerraFirma
open System
open System.Numerics
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
            [Screen.UpdateEvent => Update
             Screen.PostUpdateEvent => TransformEye
             Screen.TimeUpdateEvent => TimeUpdate
             Events.CharactersAttacked --> Simulants.GameplayScene --> Address.Wildcard =|> fun evt -> CharactersAttacked evt.Data
             Screen.SelectEvent => SynchronizeNav3d
             Screen.DeselectingEvent => FinishQuitting]

        // here we handle the gameplay messages
        override this.Message (gameplay, message, _, world) =

            match message with
            | Update ->
                let player = Simulants.GameplayPlayer.GetCharacter world
                let signals = if player.ActionState = WoundedState then [StartQuitting :> Signal] else []
                withSignals signals gameplay

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

            | CharactersAttacked attackedCharacters ->
                let world =
                    Seq.fold (fun world (attackedCharacter : Entity) ->
                        let character = attackedCharacter.GetCharacter world
                        let character =
                            let hitPoints = max (dec character.HitPoints) 0
                            let actionState =
                                if hitPoints > 0 then
                                    match character.ActionState with
                                    | InjuryState _ as injuryState -> injuryState
                                    | _ -> InjuryState { InjuryTime = gameplay.GameplayTime }
                                else WoundedState
                            { character with HitPoints = hitPoints; ActionState = actionState }
                        attackedCharacter.SetCharacter character world)
                        world attackedCharacters
                withSignal (PlaySound (0L, Constants.Audio.SoundVolumeDefault, Assets.Gameplay.InjureSound)) world

            | TransformEye ->
                let player = Simulants.GameplayPlayer.GetCharacter world
                let position = Simulants.GameplayPlayer.GetPosition world
                let rotation = Simulants.GameplayPlayer.GetRotation world
                let positionInterp = player.PositionInterp position
                let rotationInterp = player.RotationInterp rotation * Quaternion.CreateFromAxisAngle (v3Right, -0.2f)
                let world = World.setEye3dCenter (positionInterp + v3Up * 1.75f - rotationInterp.Forward * 3.0f) world
                let world = World.setEye3dRotation rotationInterp world
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
                    [Entity.Position == v3 336.0f -216.0f 0.0f
                     Entity.Elevation == 10.0f
                     Entity.Text == "Quit"
                     Entity.ClickEvent => StartQuitting]]

             // the scene group while playing or quitting
             match gameplay.GameplayState with
             | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                    [// the player that's always present
                     Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                        [Entity.Persistent == false
                         Events.DieEvent --> Simulants.GameplayPlayer => StartQuitting]]

             // no scene group
             | Quit -> ()]