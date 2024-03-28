namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

type CharacterMessage =
    | UpdateMessage
    | UpdateInputKey of KeyboardKeyData
    interface Message

type CharacterCommand =
    | UpdateAnimatedModel of Vector3 * Quaternion * Animation array
    | PublishCharactersAttacked of Entity Set
    | SyncWeaponTransform
    | SyncChildTransformsWhileHalted
    | SyncTransformWhileHalted
    | Jump
    | Die
    interface Command

type JumpState =
    { LastTime : int64
      LastTimeOnGround : int64 }

    static member initial =
        { LastTime = 0L
          LastTimeOnGround = 0L }

type AttackState =
    { AttackTime : int64
      AttackedCharacters : Entity Set
      FollowUpBuffered : bool }

    static member make time =
        { AttackTime = time
          AttackedCharacters = Set.empty
          FollowUpBuffered = false }

type InjuryState =
    { InjuryTime : int64 }

type ActionState =
    | NormalState
    | AttackState of AttackState
    | InjuryState of InjuryState
    | WoundedState

type [<ReferenceEquality; SymbolicExpansion>] Character =
    { Player : bool
      PositionPrevious : Vector3 Queue
      RotationPrevious : Quaternion Queue
      LinearVelocityPrevious : Vector3 Queue
      AngularVelocityPrevious : Vector3 Queue
      HitPoints : int
      ActionState : ActionState
      JumpState : JumpState
      WeaponCollisions : Entity Set
      WalkSpeed : single
      TurnSpeed : single
      JumpSpeed : single
      WeaponModel : StaticModel AssetTag }

    member this.PositionInterp position =
        if not (Queue.isEmpty this.PositionPrevious) then
            let positions = Queue.conj position this.PositionPrevious
            Seq.sum positions / single positions.Length
        else position

    member this.RotationInterp rotation =
        if not (Queue.isEmpty this.RotationPrevious) then
            let rotations = Queue.conj rotation this.RotationPrevious
            if rotations.Length > 1 then
                let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                unnormalized.Normalized
            else rotation
        else rotation

    member this.LinearVelocityInterp linearVelocity =
        if not (Queue.isEmpty this.LinearVelocityPrevious) then
            let linearVelocities = Queue.conj linearVelocity this.LinearVelocityPrevious
            Seq.sum linearVelocities / single linearVelocities.Length
        else linearVelocity

    member this.AngularVelocityInterp angularVelocity =
        if not (Queue.isEmpty this.AngularVelocityPrevious) then
            let angularVelocities = Queue.conj angularVelocity this.AngularVelocityPrevious
            Seq.sum angularVelocities / single angularVelocities.Length
        else angularVelocity

    member this.AnimatedModelAffineMatrix position rotation =
        Matrix4x4.CreateFromTrs (this.PositionInterp position, this.RotationInterp rotation, v3One)

    static member private computeTraversalAnimations (rotation : Quaternion) linearVelocity angularVelocity (character : Character) =
        if character.ActionState <> WoundedState then
            let linearVelocityInterp = character.LinearVelocityInterp linearVelocity
            let angularVelocityInterp = character.AngularVelocityInterp angularVelocity
            let forwardness = (Vector3.Dot (linearVelocityInterp * 32.0f, rotation.Forward))
            let backness = (Vector3.Dot (linearVelocityInterp * 32.0f, -rotation.Forward))
            let rightness = (Vector3.Dot (linearVelocityInterp * 32.0f, rotation.Right))
            let leftness = (Vector3.Dot (linearVelocityInterp * 32.0f, -rotation.Right))
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
                if turnRightness >= 0.05f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = turnRightness; BoneFilterOpt = None } :: animations
                elif turnLeftness >= 0.05f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = turnLeftness; BoneFilterOpt = None } :: animations
                else animations
            animations
        else []

    static member private tryComputeActionAnimation time character world =
        match character.ActionState with
        | AttackState attack ->
            let localTime = time - attack.AttackTime
            let world =
                match localTime with
                | 7L -> World.playSound 1.0f Assets.Gameplay.SlashSound world
                | 67L -> if attack.FollowUpBuffered then World.playSound 1.0f Assets.Gameplay.Slash2Sound world else world
                | _ -> world
            let animationStartTime = GameTime.ofUpdates (world.UpdateTime - localTime % 55L)
            let animationName = if localTime <= 55 then "Armature|AttackVertical" else "Armature|AttackHorizontal"
            let animation = { StartTime = animationStartTime; LifeTimeOpt = None; Name = animationName; Playback = Once; Rate = 1.0f; Weight = 32.0f; BoneFilterOpt = None }
            Some animation
        | InjuryState injury ->
            let localTime = time - injury.InjuryTime
            let animationStartTime = GameTime.ofUpdates (world.UpdateTime - localTime % 55L)
            let animation = { StartTime = animationStartTime; LifeTimeOpt = None; Name = "Armature|WalkBack"; Playback = Once; Rate = 1.0f; Weight = 32.0f; BoneFilterOpt = None }
            Some animation
        | NormalState | WoundedState -> None

    static member updateInterps position rotation linearVelocity angularVelocity character =
        let character =
            { character with
                PositionPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> position) |> Queue.ofSeq
                RotationPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> rotation) |> Queue.ofSeq
                LinearVelocityPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> linearVelocity) |> Queue.ofSeq
                AngularVelocityPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> angularVelocity) |> Queue.ofSeq }
        (character.PositionInterp position,
         character.RotationInterp rotation,
         character.LinearVelocityInterp linearVelocity,
         character.AngularVelocityInterp angularVelocity,
         character)

    static member overwriteInterps position rotation linearVelocity angularVelocity character =
        { character with
            PositionPrevious = (if character.PositionPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.PositionPrevious |> Queue.tail else character.PositionPrevious) |> Queue.conj position
            RotationPrevious = (if character.RotationPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.RotationPrevious |> Queue.tail else character.RotationPrevious) |> Queue.conj rotation
            LinearVelocityPrevious = (if character.LinearVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.LinearVelocityPrevious |> Queue.tail else character.LinearVelocityPrevious) |> Queue.conj linearVelocity
            AngularVelocityPrevious = (if character.AngularVelocityPrevious.Length >= Constants.Gameplay.CharacterInterpolationSteps then character.AngularVelocityPrevious |> Queue.tail else character.AngularVelocityPrevious) |> Queue.conj angularVelocity }

    static member updateInputKey time keyboardKeyData character =
        let sinceJump = time - character.JumpState.LastTime
        let sinceOnGround = time - character.JumpState.LastTimeOnGround
        if  keyboardKeyData.KeyboardKey = KeyboardKey.Space &&
            not keyboardKeyData.Repeated &&
            sinceJump >= 12L &&
            sinceOnGround < 10L &&
            character.ActionState = NormalState then
            let character = { character with Character.JumpState.LastTime = time }
            (true, character)
        elif keyboardKeyData.KeyboardKey = KeyboardKey.Rshift && not keyboardKeyData.Repeated then
            let character =
                match character.ActionState with
                | NormalState ->
                    { character with ActionState = AttackState (AttackState.make time) }
                | AttackState attack ->
                    let localTime = time - attack.AttackTime
                    if localTime > 10L && not attack.FollowUpBuffered
                    then { character with ActionState = AttackState { attack with FollowUpBuffered = true }}
                    else character
                | InjuryState _ | WoundedState -> character
            (false, character)
        else (false, character)

    static member updateMotion time position (rotation : Quaternion) character (entity : Entity) world =

        // update jump state
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        let lastTimeOnGround = if grounded then time else character.JumpState.LastTimeOnGround
        let character = { character with Character.JumpState.LastTimeOnGround = lastTimeOnGround }

        // update traversal
        let (position, rotation, linearVelocity, angularVelocity, character) =
            if character.Player then
                if character.ActionState = NormalState || not grounded then

                    // compute new position
                    let forward = rotation.Forward
                    let right = rotation.Right
                    let walkSpeed = character.WalkSpeed * if grounded then 1.0f else 0.75f
                    let walkVelocity =
                        (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                        (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                        (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                        (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
                    let position = if walkVelocity <> v3Zero then position + walkVelocity else position

                    // compute new rotation
                    let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
                    let turnVelocity =
                        (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                        (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                    let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation
                    (position, rotation, walkVelocity, v3 0.0f turnVelocity 0.0f, character)

                else (position, rotation, v3Zero, v3Zero, character)
            else
                if character.ActionState = NormalState then
                    let playerPosition = Simulants.GameplayPlayer.GetPosition world
                    let followOutput = World.nav3dFollow (Some 1.25f) (Some 10.0f) 0.0333f 0.1f position rotation playerPosition entity.Screen world
                    (followOutput.NavPosition, followOutput.NavRotation, followOutput.NavLinearVelocity, followOutput.NavAngularVelocity, character)
                else (position, rotation, v3Zero, v3Zero, character)

        // update interps
        Character.updateInterps position rotation linearVelocity angularVelocity character

    static member updateAttackedCharacters time rotation linearVelocity angularVelocity character world =
        match character.ActionState with
        | AttackState attack ->
            let localTime = time - attack.AttackTime
            let attack =
                match localTime with
                | 55L -> { attack with AttackedCharacters = Set.empty } // reset attack tracking at start of buffered attack
                | _ -> attack
            if localTime >= 20 && localTime < 30 || localTime >= 78 && localTime < 88 then
                let attackingCharacters = Set.difference character.WeaponCollisions attack.AttackedCharacters
                let attack = { attack with AttackedCharacters = Set.union attack.AttackedCharacters character.WeaponCollisions }
                (attackingCharacters, { character with ActionState = AttackState attack })
            else (Set.empty, { character with ActionState = AttackState attack })
        | _ -> (Set.empty, character)

    static member updateActionState time position (rotation : Quaternion) linearVelocity angularVelocity character world =
        let actionState =
            match character.ActionState with
            | AttackState attack ->
                let localTime = time - attack.AttackTime
                if localTime < 55 || localTime < 110 && attack.FollowUpBuffered
                then AttackState attack
                else NormalState
            | InjuryState injury ->
                let localTime = time - injury.InjuryTime
                if localTime < 35
                then InjuryState injury
                else NormalState
            | NormalState -> NormalState
            | WoundedState -> WoundedState
        let actionState =
            if not character.Player then
                match actionState with
                | NormalState when not character.Player ->
                    let playerPosition = Simulants.GameplayPlayer.GetPosition world
                    if  Vector3.Distance (position, playerPosition) < 1.5f &&
                        rotation.Forward.AngleBetween (playerPosition - position) < 0.5f then
                        AttackState (AttackState.make world.UpdateTime)
                    else actionState
                | _ -> actionState
            else actionState
        { character with ActionState = actionState }

    static member updateAnimations time position rotation linearVelocity angularVelocity character world =
        ignore<Vector3> position
        let traversalAnimations = Character.computeTraversalAnimations rotation linearVelocity angularVelocity character
        let actionAnimationOpt = Character.tryComputeActionAnimation time character world
        let animations = Array.append (Option.toArray actionAnimationOpt) (Array.ofList traversalAnimations)
        (animations, character)

    static member initial position rotation =
        { Player = false
          PositionPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> position) |> Queue.ofSeq
          RotationPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> rotation) |> Queue.ofSeq
          LinearVelocityPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq
          AngularVelocityPrevious = Array.init (dec Constants.Gameplay.CharacterInterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq
          HitPoints = 3
          ActionState = NormalState
          JumpState = JumpState.initial
          WeaponCollisions = Set.empty
          WalkSpeed = 0.05f
          TurnSpeed = 0.05f
          JumpSpeed = 5.0f
          WeaponModel = Assets.Gameplay.GreatSwordModel }