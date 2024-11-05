namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu
open TerraFirma

// this represents that state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this is our MMCC model type representing gameplay.
type [<SymbolicExpansion>] Gameplay =
    { GameplayState : GameplayState
      Score : int }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayState = Quit
          Score = 0 }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { GameplayState = Playing
          Score = 0 }

// this is our MMCC message type.
type GameplayMessage =
    | StartPlaying
    | FinishQuitting
    | Die of Entity
    interface Message

// this is our MMCC command type.
type GameplayCommand =
    | SetupScene
    | StartQuitting
    | AttackCharacter of Entity
    | DestroyEnemy of Entity
    | TrackPlayer
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module Gameplay =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the screen dispatcher that defines the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.PostUpdateEvent => TrackPlayer
         Events.AttackEvent --> Simulants.GameplayScene --> Address.Wildcard =|> fun evt -> AttackCharacter evt.Data
         Events.DieEvent --> Simulants.GameplayScene --> Address.Wildcard =|> fun evt -> Die evt.Data]

    // here we handle the gameplay messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | StartPlaying ->
            let gameplay = Gameplay.initial
            withSignals [SetupScene; TrackPlayer] gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | Die deadCharacter ->
            let character = deadCharacter.GetCharacter world
            match character.CharacterType with
            | Player -> withSignal StartQuitting gameplay
            | Enemy ->
                let gameplay = { gameplay with Score = gameplay.Score + 100 }
                withSignal (DestroyEnemy deadCharacter) gameplay

    // here we handle the gameplay commands
    // notice how in here we handle events from characters to implement intra-character interactions rather than
    // the more complex approach of having characters talk to each other or handle each other's events.
    override this.Command (_, command, screen, world) =

        match command with
        | SetupScene ->
            let world = Simulants.GameplayPlayer.SetPosition (v3 0.0f 1.65f 0.0f) world
            let world = World.synchronizeNav3d screen world
            just world

        | StartQuitting ->
            let world = World.publish () screen.QuitEvent screen world
            just world

        | AttackCharacter entity ->
            let character = entity.GetCharacter world
            let character = { character with HitPoints = max (dec character.HitPoints) 0 }
            let (signals, character) =
                if character.HitPoints > 0 then
                    match character.ActionState with
                    | InjuryState _ -> just character
                    | _ ->
                        let character = { character with ActionState = InjuryState { InjuryTime = world.UpdateTime }}
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                        just character
                else
                    match character.ActionState with
                    | WoundState _ -> just character
                    | _ ->
                        let character = { character with ActionState = WoundState { WoundTime = world.UpdateTime }}
                        World.playSound Constants.Audio.SoundVolumeDefault Assets.Gameplay.InjureSound world
                        just character
            let world = entity.SetCharacter character world
            withSignals signals world

        | DestroyEnemy entity ->
            let world = World.destroyEntity entity world
            just world

        | TrackPlayer ->
            
            // update eye to look at player
            let player = Simulants.GameplayPlayer.GetCharacter world
            let position = Simulants.GameplayPlayer.GetPosition world
            let rotation = Simulants.GameplayPlayer.GetRotation world
            let positionInterp = player.PositionInterp position
            let rotationInterp = player.RotationInterp rotation * Quaternion.CreateFromAxisAngle (v3Right, -0.1f)
            let world = World.setEye3dCenter (positionInterp + v3Up * 1.75f - rotationInterp.Forward * 3.0f) world
            let world = World.setEye3dRotation rotationInterp world

            // update sun to shine over player as snapped to shadow map's texel grid in shadow space. This is similar
            // in concept to - https://learn.microsoft.com/en-us/windows/win32/dxtecharts/common-techniques-to-improve-shadow-depth-maps?redirectedfrom=MSDN#moving-the-light-in-texel-sized-increments
            let sun = Simulants.GameplaySun
            let mutable shadowViewInverse = Matrix4x4.CreateFromYawPitchRoll (0.0f, -MathF.PI_OVER_2, 0.0f) * Matrix4x4.CreateFromQuaternion (sun.GetRotation world)
            shadowViewInverse.Translation <- sun.GetPosition world
            let shadowView = shadowViewInverse.Inverted
            let shadowWidth = sun.GetLightCutoff world * 2.0f
            let shadowResolution = Constants.Render.ShadowResolution * Constants.Render.ShadowDetailedResolutionScalar
            let shadowTexelSize = shadowWidth / single shadowResolution.X // assuming square, of course
            let positionShadow = positionInterp.Transform shadowView + v3Up * 12.0f // position of player + offset in shadow space
            let positionSnapped =
                v3
                    (floor (positionShadow.X / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Y / shadowTexelSize) * shadowTexelSize)
                    (floor (positionShadow.Z / shadowTexelSize) * shadowTexelSize)
            let position = positionSnapped.Transform shadowViewInverse
            let world = sun.SetPosition position world
            just world

    // here we describe the content of the game including the hud group and the scene group
    override this.Content (gameplay, _) =

        [// the scene group while playing
         if gameplay.GameplayState = Playing then
            
            // loads scene from file edited in Gaia
            Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" []

                [// the player that's always present in the scene
                 Content.entity<PlayerDispatcher> Simulants.GameplayPlayer.Name
                    [Entity.Persistent == false
                     Entity.DieEvent => Die Simulants.GameplayPlayer]]
         
         // the gui group
         Content.group Simulants.GameplayGui.Name []

            [// score
             Content.text Simulants.GameplayScore.Name
                [Entity.Position == v3 260.0f 155.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text := "Score: " + string gameplay.Score]

             // quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Elevation == 10.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]