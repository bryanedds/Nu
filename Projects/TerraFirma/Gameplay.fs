namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

// this is our MMCC message type.
type GameplayMessage =
    | UpdatePhysics of IntegrationData
    | UpdatePlayerInputKey of KeyboardKeyData
    | Update
    | PostUpdate
    | TimeUpdate
    | StartQuitting
    | FinishQuitting
    interface Message

// this is our MMCC command type.
type GameplayCommand =
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
type [<ReferenceEquality>] Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState
      Player : Character
      Enemies : HMap<Guid, Character> }

    static member private warpCharacter position rotation character =
        { character with
            Position = position
            Rotation = rotation
            LinearVelocity = v3Zero
            AngularVelocity = v3Zero
            PositionPrevious = Array.init 3 (fun _ -> position) |> Queue.ofSeq
            RotationPrevious = Array.init 3 (fun _ -> rotation) |> Queue.ofSeq
            LinearVelocityPrevious = Array.init 3 (fun _ -> v3Zero) |> Queue.ofSeq
            AngularVelocityPrevious = Array.init 3 (fun _ -> v3Zero) |> Queue.ofSeq }

    static member private transformCharacter position rotation linearVelocity angularVelocity character =
        { character with
            Position = position
            Rotation = rotation
            LinearVelocity = linearVelocity
            AngularVelocity = angularVelocity
            PositionPrevious = (if character.PositionPrevious.Length > 3 then character.PositionPrevious |> Queue.tail else character.PositionPrevious) |> Queue.conj character.Position
            RotationPrevious = (if character.RotationPrevious.Length > 3 then character.RotationPrevious |> Queue.tail else character.RotationPrevious) |> Queue.conj character.Rotation
            LinearVelocityPrevious = (if character.LinearVelocityPrevious.Length > 3 then character.LinearVelocityPrevious |> Queue.tail else character.LinearVelocityPrevious) |> Queue.conj character.LinearVelocity
            AngularVelocityPrevious = (if character.AngularVelocityPrevious.Length > 3 then character.AngularVelocityPrevious |> Queue.tail else character.AngularVelocityPrevious) |> Queue.conj character.AngularVelocity }

    static member private computeCharacterTraversalAnimations (character : Character) =
        let linearVelocityInterp = character.LinearVelocityInterp
        let angularVelocityInterp = character.AngularVelocityInterp
        let forwardness = (Vector3.Dot (linearVelocityInterp * 32.0f, character.Rotation.Forward))
        let backness = (Vector3.Dot (linearVelocityInterp * 32.0f, -character.Rotation.Forward))
        let rightness = (Vector3.Dot (linearVelocityInterp * 32.0f, character.Rotation.Right))
        let leftness = (Vector3.Dot (linearVelocityInterp * 32.0f, -character.Rotation.Right))
        let turnRightness = (angularVelocityInterp * v3Up).Length () * 48.0f
        let turnLeftness = -turnRightness
        let animations =
            [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
        let animations =
            if forwardness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.0f; Weight = forwardness; BoneFilterOpt = None } :: animations
            elif backness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBack"; Playback = Loop; Rate = 1.0f; Weight = backness; BoneFilterOpt = None } :: animations
            else animations
        let animations =
            if rightness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRight"; Playback = Loop; Rate = 1.0f; Weight = rightness; BoneFilterOpt = None } :: animations
            elif leftness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeft"; Playback = Loop; Rate = 1.0f; Weight = leftness; BoneFilterOpt = None } :: animations
            else animations
        let animations =
            if turnRightness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = turnRightness; BoneFilterOpt = None } :: animations
            elif turnLeftness >= 0.2f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = turnLeftness; BoneFilterOpt = None } :: animations
            else animations
        animations

    static member private tryComputeCharacterActionAnimation time attackOpt world =
        match attackOpt with
        | Some attack ->
            let localTime = time - attack.AttackTime
            let world =
                match localTime with
                | 0L -> World.playSound 1.0f Assets.Default.Sound world
                | 55L -> if attack.FollowUpBuffered then World.playSound 1.0f Assets.Default.Sound world else world
                | _ -> world
            let animationStartTime = GameTime.ofUpdates (world.UpdateTime - localTime % 55L)
            let animationName = if localTime <= 55 then "Armature|AttackVertical" else "Armature|AttackHorizontal"
            let animation = { StartTime = animationStartTime; LifeTimeOpt = None; Name = animationName; Playback = Once; Rate = 1.0f; Weight = 32.0f; BoneFilterOpt = None }
            Some animation
        | None -> None

    static member private updateCharacterAttack time character =
        let attackOpt =
            match character.AttackOpt with
            | Some attack ->
                let localTime = time - attack.AttackTime
                if localTime < 55 || localTime < 110 && attack.FollowUpBuffered
                then Some attack
                else None
            | None -> None
        { character with AttackOpt = attackOpt }

    static member private updateCharacterAnimation time character world =
        let traversalAnimations = Gameplay.computeCharacterTraversalAnimations character
        let actionAnimationOpt = Gameplay.tryComputeCharacterActionAnimation time character.AttackOpt world
        let animations = Array.append (Option.toArray actionAnimationOpt) (Array.ofList traversalAnimations)
        { character with Animations = animations }

    static member private updateCharacter time character world =
        let character = Gameplay.updateCharacterAttack time character
        let character = Gameplay.updateCharacterAnimation time character world
        character

    static member private updatePlayerInputScan player world =
        let bodyId = Simulants.GameplayPlayer.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        if player.AttackOpt.IsNone || not grounded then

            // update player position
            let forward = player.Rotation.Forward
            let right = player.Rotation.Right
            let walkSpeed = Constants.Gameplay.PlayerWalkSpeed * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
            let position = if walkVelocity <> v3Zero then player.Position + walkVelocity else player.Position
            let player = { player with Position = position }

            // update player rotation
            let turnSpeed = Constants.Gameplay.PlayerTurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then player.Rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else player.Rotation
            { player with Rotation = rotation }

        else player

    static member private postUpdateCharacterWeaponHand (character : Character) (entity : Entity) world =
        match (entity.GetBoneOffsetsOpt world, entity.GetBoneTransformsOpt world) with
        | (Some offsets, Some transforms) ->
            let offset = offsets.[34]
            let transform = transforms.[34]
            let affineMatrix = character.AnimatedModelAffineMatrix
            let weaponHand = offset.Inverted * transform * affineMatrix
            { character with WeaponHand = weaponHand }
        | (_, _) -> character

    static member updatePhysics (integrationData : IntegrationData) gameplay world =
        SArray.fold (fun gameplay integrationMessage ->
            match integrationMessage with
            | BodyTransformMessage bodyTransformMessage ->
                let bodyId = bodyTransformMessage.BodyId
                match bodyId.BodySource with
                | :? Entity as entity when entity.Is<CharacterDispatcher> world ->
                    if entity.Name = Simulants.GameplayPlayer.Name then
                        let player = gameplay.Player
                        let player = Gameplay.transformCharacter bodyTransformMessage.Center bodyTransformMessage.Rotation (World.getBodyLinearVelocity bodyId world) (World.getBodyAngularVelocity bodyId world) player
                        let player = { player with Jump.LastTimeOnGround = if World.getBodyGrounded bodyId world then gameplay.GameplayTime else player.Jump.LastTimeOnGround }
                        { gameplay with Player = player }
                    else
                        let enemyId = scvalueMemo entity.Name
                        match gameplay.Enemies.TryGetValue enemyId with
                        | (true, enemy) ->
                            let followOutput = World.tryNav3dFollow (Some 1.25f) (Some 10.0f) 0.025f bodyTransformMessage.Center bodyTransformMessage.Rotation gameplay.Player.Position entity.Screen world
                            let enemy = Gameplay.transformCharacter followOutput.NavPosition followOutput.NavRotation followOutput.NavLinearVelocity followOutput.NavAngularVelocity enemy
                            { gameplay with Enemies = HMap.add enemyId enemy gameplay.Enemies}
                        | (false, _) -> gameplay
                | _ -> gameplay
            | _ -> gameplay)
            gameplay integrationData.IntegrationMessages

    static member updatePlayerInputKey keyboardKeyData gameplay =
        let time = gameplay.GameplayTime
        let player = gameplay.Player
        let sinceJump = time - player.Jump.LastTime
        let sinceOnGround = time - player.Jump.LastTimeOnGround
        let (signals, player) =
            if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 12L && sinceOnGround < 10L then
                let player = { player with Jump.LastTime = time }
                withSignal JumpPlayer player
            elif keyboardKeyData.KeyboardKey = KeyboardKey.Rshift && not keyboardKeyData.Repeated then
                let player =
                    match player.AttackOpt with
                    | Some attack ->
                        let localTime = time - attack.AttackTime
                        if localTime > 15L && not attack.FollowUpBuffered
                        then { player with AttackOpt = Some { attack with FollowUpBuffered = true }}
                        else player
                    | None ->
                        { player with AttackOpt = Some (AttackState.make time) }
                just player
            else just player
        let gameplay = { gameplay with Player = player }
        withSignals signals gameplay

    static member update gameplay world =
        let gameplay = { gameplay with Player = Gameplay.updateCharacter gameplay.GameplayTime gameplay.Player world }
        let gameplay = { gameplay with Enemies = HMap.map (fun _ enemy -> Gameplay.updateCharacter gameplay.GameplayTime enemy world) gameplay.Enemies }
        let gameplay = { gameplay with Player = Gameplay.updatePlayerInputScan gameplay.Player world }
        gameplay

    static member postUpdate gameplay world =
        let gameplay = { gameplay with Player = Gameplay.postUpdateCharacterWeaponHand gameplay.Player Simulants.GameplayPlayer world }
        let gameplay = { gameplay with Enemies = HMap.map (fun enemyId enemy -> Gameplay.postUpdateCharacterWeaponHand enemy (Simulants.GameplayEnemy enemyId) world) gameplay.Enemies }
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