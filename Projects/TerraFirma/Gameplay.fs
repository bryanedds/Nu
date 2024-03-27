namespace TerraFirma
open System
open Prime
open Nu

// this is our MMCC message type.
type GameplayMessage =
    | UpdatePhysics of IntegrationData
    | UpdatePlayerInputKey of KeyboardKeyData
    | Update
    | TimeUpdate
    | StartQuitting
    | FinishQuitting
    interface Message

// this is our MMCC command type.
type GameplayCommand =
    | SynchronizeNav3d
    | JumpPlayer
    | TransformEye
    interface Command

// this represents that state of gameplay simulation.
type GameplayState =
    | Playing
    | Quitting
    | Quit

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
// if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
// you could use `GameplayTime : single` instead.
type [<ReferenceEquality; SymbolicExpansion>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Player : Character
      Enemies : HMap<Guid, Character> }

    static member updatePhysics (integrationData : IntegrationData) gameplay world =
        SArray.fold (fun gameplay integrationMessage ->
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage ->
                let bodyId = bodyTransformMessage.BodyId
                match bodyId.BodySource with
                | :? Entity as entity when entity.Is<CharacterDispatcher> world ->
                    if entity.Name = Simulants.GameplayPlayer.Name then
                        let player = gameplay.Player
                        let player = Character.transform bodyTransformMessage.Center bodyTransformMessage.Rotation (World.getBodyLinearVelocity bodyId world) (World.getBodyAngularVelocity bodyId world) player
                        let player = { player with Jump.LastTimeOnGround = if World.getBodyGrounded bodyId world then gameplay.GameplayTime else player.Jump.LastTimeOnGround }
                        { gameplay with Player = player }
                    else
                        let enemyId = scvalueMemo entity.Name
                        match gameplay.Enemies.TryGetValue enemyId with
                        | (true, enemy) ->
                            let followOutput = World.nav3dFollow (Some 1.25f) (Some 10.0f) 0.0333f 0.05f bodyTransformMessage.Center bodyTransformMessage.Rotation gameplay.Player.Position entity.Screen world
                            let enemy = Character.transform followOutput.NavPosition followOutput.NavRotation followOutput.NavLinearVelocity followOutput.NavAngularVelocity enemy
                            { gameplay with Enemies = HMap.add enemyId enemy gameplay.Enemies}
                        | (false, _) -> gameplay
                | _ -> gameplay
            | _ -> gameplay)
            gameplay integrationData.IntegrationMessages

    static member updatePlayerInputKey keyboardKeyData gameplay =
        let time = gameplay.GameplayTime
        let player = gameplay.Player
        let (signalJump, player) = Character.updateInputKey time keyboardKeyData player
        let gameplay = { gameplay with Player = player }
        if signalJump then withSignal JumpPlayer gameplay else just gameplay

    static member update gameplay world =
        let gameplay = { gameplay with Player = Character.update gameplay.GameplayTime gameplay.Player world }
        let gameplay = { gameplay with Enemies = HMap.map (fun _ enemy -> Character.update gameplay.GameplayTime enemy world) gameplay.Enemies }
        let gameplay = { gameplay with Player = Character.updateInputScan gameplay.Player Simulants.GameplayPlayer world }
        gameplay

    static member timeUpdate gameplay =
        let gameplay = { gameplay with GameplayTime = inc gameplay.GameplayTime }
        gameplay

    static member initial =
        let enemies =
            [for i in 0 .. dec 7 do
                for j in 0 .. dec 7 do
                    let enemy = Character.initialEnemy (v3 (single i * 8.0f - 8.0f) 2.0f (single j * 8.0f - 8.0f)) quatIdentity
                    (makeGuid (), enemy)]
        { GameplayTime = 0L
          GameplayState = Quit
          Player = Character.initialPlayer (v3 0.0f 2.0f 0.0f) quatIdentity
          Enemies = HMap.ofList enemies }

    static member start =
        let initial = Gameplay.initial
        { initial with GameplayState = Playing }