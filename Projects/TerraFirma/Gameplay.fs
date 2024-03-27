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
    | PlaySound of int64 * single * Sound AssetTag
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
      Characters : Map<CharacterId, Character> }

    member this.Player = this.Characters.[Gameplay.PlayerId]

    member this.WithPlayer player = { this with Characters = Map.add Gameplay.PlayerId player this.Characters }

    static member PlayerId = PlayerId "Player" // additional player would be called "Player2"

    static member updatePhysics (integrationData : IntegrationData) gameplay world =
        SArray.fold (fun (gameplay : Gameplay) integrationMessage ->
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage ->
                let bodyId = bodyTransformMessage.BodyId
                match bodyId.BodySource with
                | :? Entity as entity when entity.Is<CharacterDispatcher> world ->
                    if entity = Simulants.GameplayPlayer then
                        let player = gameplay.Player
                        let player = Character.transform bodyTransformMessage.Center bodyTransformMessage.Rotation bodyTransformMessage.LinearVelocity bodyTransformMessage.AngularVelocity player
                        let player = { player with Character.JumpState.LastTimeOnGround = if World.getBodyGrounded bodyId world then gameplay.GameplayTime else player.JumpState.LastTimeOnGround }
                        gameplay.WithPlayer player
                    else
                        let enemyId = EnemyId (scvalueMemo entity.Name)
                        match gameplay.Characters.TryGetValue enemyId with
                        | (true, enemy) ->
                            let followOutput = World.nav3dFollow (Some 1.25f) (Some 10.0f) 0.0333f 0.05f bodyTransformMessage.Center bodyTransformMessage.Rotation gameplay.Player.Position entity.Screen world
                            let enemy = Character.transform followOutput.NavPosition followOutput.NavRotation followOutput.NavLinearVelocity followOutput.NavAngularVelocity enemy
                            { gameplay with Characters = Map.add enemyId enemy gameplay.Characters}
                        | (false, _) -> gameplay
                | _ -> gameplay
            | BodyCollisionMessage bodyCollisionMessage ->
                match (bodyCollisionMessage.BodyShapeSource.BodyId.BodySource, bodyCollisionMessage.BodyShapeSource2.BodyId.BodySource) with
                | ((:? Entity as child), (:? Entity as entity2)) when entity2.Is<CharacterDispatcher> world ->
                    match child.Parent with
                    | :? Entity as entity when entity.Is<CharacterDispatcher> world ->
                        let playerId = PlayerId entity.Name
                        let enemyId = EnemyId entity2.Name
                        match (gameplay.Characters.TryGetValue playerId, gameplay.Characters.TryGetValue enemyId) with
                        | ((true, player), (true, _)) ->
                            let player = { player with WeaponCollisions = Set.add enemyId player.WeaponCollisions }
                            gameplay.WithPlayer player
                        | _ ->
                            let enemyId = EnemyId entity.Name
                            let playerId = PlayerId entity2.Name
                            match (gameplay.Characters.TryGetValue enemyId, gameplay.Characters.TryGetValue playerId) with
                            | ((true, enemy), (true, _)) ->
                                let enemy = { enemy with WeaponCollisions = Set.add enemyId enemy.WeaponCollisions }
                                { gameplay with Characters = Map.add enemyId enemy gameplay.Characters }
                            | _ -> gameplay
                    | _ -> gameplay
                | _ -> gameplay
            | BodySeparationMessage bodySeparationMessage ->
                match (bodySeparationMessage.BodyShapeSource.BodyId.BodySource, bodySeparationMessage.BodyShapeSource2.BodyId.BodySource) with
                | ((:? Entity as child), (:? Entity as entity2)) when entity2.Is<CharacterDispatcher> world ->
                    match child.Parent with
                    | :? Entity as entity when entity.Is<CharacterDispatcher> world ->
                        let playerId = PlayerId entity.Name
                        let enemyId = EnemyId entity2.Name
                        match (gameplay.Characters.TryGetValue playerId, gameplay.Characters.TryGetValue enemyId) with
                        | ((true, player), (true, _)) ->
                            let player = { player with WeaponCollisions = Set.remove enemyId player.WeaponCollisions }
                            gameplay.WithPlayer player
                        | _ ->
                            let enemyId = EnemyId entity.Name
                            let playerId = PlayerId entity2.Name
                            match (gameplay.Characters.TryGetValue enemyId, gameplay.Characters.TryGetValue playerId) with
                            | ((true, enemy), (true, _)) ->
                                let enemy = { enemy with WeaponCollisions = Set.remove enemyId enemy.WeaponCollisions }
                                { gameplay with Characters = Map.add enemyId enemy gameplay.Characters }
                            | _ -> gameplay
                    | _ -> gameplay
                | _ -> gameplay)
            gameplay integrationData.IntegrationMessages

    static member updatePlayerInputKey keyboardKeyData gameplay =
        let time = gameplay.GameplayTime
        let player = gameplay.Player
        let (signalJump, player) = Character.updateInputKey time keyboardKeyData player
        let gameplay = gameplay.WithPlayer player
        if signalJump then withSignal JumpPlayer gameplay else just gameplay

    static member update gameplay world =

        // process attacks
        let (attackedCharactersList, characters) =
            Map.fold (fun (attackedCharactersList, characters) characterId character ->
                let (attackedCharacters, character) = Character.update gameplay.GameplayTime character world
                let characters = Map.add characterId character characters
                (attackedCharacters :: attackedCharactersList, characters))
                ([], gameplay.Characters)
                gameplay.Characters
        let attackedCharacters = attackedCharactersList |> Seq.concat |> Seq.toArray

        // update attacked character tracking
        let characters =
            Array.fold (fun (characters : Map<_, _>) attackedCharacter ->
                match characters.TryGetValue attackedCharacter with
                | (true, character) ->
                    let character = { character with ActionState = InjuryState { InjuryTime = gameplay.GameplayTime }}
                    Map.add attackedCharacter character characters
                | (false, _) -> characters)
                characters attackedCharacters
        let gameplay = { gameplay with Characters = characters }

        // update player scanned input
        let gameplay = gameplay.WithPlayer (Character.updateInputScan gameplay.Player Simulants.GameplayPlayer world)

        // fin
        (attackedCharacters, gameplay)

    static member timeUpdate gameplay =
        let gameplay = { gameplay with GameplayTime = inc gameplay.GameplayTime }
        gameplay

    static member initial =
        let player = Character.initialPlayer (v3 0.0f 2.0f 0.0f) quatIdentity
        let enemies =
            [for i in 0 .. dec 5 do
                for j in 0 .. dec 5 do
                    let enemy = Character.initialEnemy (v3 (single i * 8.0f - 8.0f) 2.0f (single j * 8.0f - 8.0f)) quatIdentity
                    (makeGuid () |> string |> EnemyId, enemy)]
        let characters = Map.ofList ((Gameplay.PlayerId, player) :: enemies)
        { GameplayTime = 0L
          GameplayState = Quit
          Characters = characters }

    static member start =
        let initial = Gameplay.initial
        { initial with GameplayState = Playing }