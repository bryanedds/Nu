namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

type CharacterCommand =
    | PostUpdate
    interface Command

type JumpState =
    { LastTime : int64
      LastTimeOnGround : int64 }

    static member initial =
        { LastTime = 0L
          LastTimeOnGround = 0L }

type AttackState =
    { AttackTime : int64
      FollowUpBuffered : bool }

    static member make time =
        { AttackTime = time
          FollowUpBuffered = false }

type [<ReferenceEquality; SymbolicExpansion>] Character =
    { Position : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      AngularVelocity : Vector3
      PositionPrevious : Vector3 Queue
      RotationPrevious : Quaternion Queue
      LinearVelocityPrevious : Vector3 Queue
      AngularVelocityPrevious : Vector3 Queue
      AttackOpt : AttackState option
      WalkSpeed : single
      TurnSpeed : single
      JumpSpeed : single
      Animations : Animation array
      Jump : JumpState
      BodyShape : BodyShape
      CharacterProperties : CharacterProperties
      AnimatedModel : AnimatedModel AssetTag }

    member this.PositionInterp =
        if not (Queue.isEmpty this.PositionPrevious) then
            let positions = Queue.conj this.Position this.PositionPrevious
            Seq.sum positions / single positions.Length
        else this.Position

    member this.RotationInterp =
        if not (Queue.isEmpty this.RotationPrevious) then
            let rotations = Queue.conj this.Rotation this.RotationPrevious
            if rotations.Length > 1 then
                let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f)
                unnormalized.Normalized
            else this.Rotation
        else this.Rotation

    member this.LinearVelocityInterp =
        if not (Queue.isEmpty this.LinearVelocityPrevious) then
            let linearVelocities = Queue.conj this.LinearVelocity this.LinearVelocityPrevious
            Seq.sum linearVelocities / single linearVelocities.Length
        else this.LinearVelocity

    member this.AngularVelocityInterp =
        if not (Queue.isEmpty this.AngularVelocityPrevious) then
            let angularVelocities = Queue.conj this.AngularVelocity this.AngularVelocityPrevious
            Seq.sum angularVelocities / single angularVelocities.Length
        else this.AngularVelocity

    member this.AnimatedModelAffineMatrix =
        Matrix4x4.CreateFromTrs (this.PositionInterp, this.RotationInterp, v3One)

    static member private computeTraversalAnimations (character : Character) =
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
            if turnRightness >= 0.05f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = turnRightness; BoneFilterOpt = None } :: animations
            elif turnLeftness >= 0.05f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = turnLeftness; BoneFilterOpt = None } :: animations
            else animations
        animations

    static member private tryComputeActionAnimation time attackOpt world =
        match attackOpt with
        | Some attack ->
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
        | None -> None

    static member warp position rotation character =
        { character with
            Position = position
            Rotation = rotation
            LinearVelocity = v3Zero
            AngularVelocity = v3Zero
            PositionPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> position) |> Queue.ofSeq
            RotationPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> rotation) |> Queue.ofSeq
            LinearVelocityPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq
            AngularVelocityPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq }

    static member transform position rotation linearVelocity angularVelocity character =
        { character with
            Position = position
            Rotation = rotation
            LinearVelocity = linearVelocity
            AngularVelocity = angularVelocity
            PositionPrevious = (if character.PositionPrevious.Length = Constants.Gameplay.InterpolationSteps then character.PositionPrevious |> Queue.tail else character.PositionPrevious) |> Queue.conj character.Position
            RotationPrevious = (if character.RotationPrevious.Length = Constants.Gameplay.InterpolationSteps then character.RotationPrevious |> Queue.tail else character.RotationPrevious) |> Queue.conj character.Rotation
            LinearVelocityPrevious = (if character.LinearVelocityPrevious.Length = Constants.Gameplay.InterpolationSteps then character.LinearVelocityPrevious |> Queue.tail else character.LinearVelocityPrevious) |> Queue.conj character.LinearVelocity
            AngularVelocityPrevious = (if character.AngularVelocityPrevious.Length = Constants.Gameplay.InterpolationSteps then character.AngularVelocityPrevious |> Queue.tail else character.AngularVelocityPrevious) |> Queue.conj character.AngularVelocity }

    static member updateInputKey time keyboardKeyData character =
        let sinceJump = time - character.Jump.LastTime
        let sinceOnGround = time - character.Jump.LastTimeOnGround
        if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 12L && sinceOnGround < 10L then
            let character = { character with Jump.LastTime = time }
            (true, character)
        elif keyboardKeyData.KeyboardKey = KeyboardKey.Rshift && not keyboardKeyData.Repeated then
            let character =
                match character.AttackOpt with
                | Some attack ->
                    let localTime = time - attack.AttackTime
                    if localTime > 15L && not attack.FollowUpBuffered
                    then { character with AttackOpt = Some { attack with FollowUpBuffered = true }}
                    else character
                | None ->
                    { character with AttackOpt = Some (AttackState.make time) }
            (false, character)
        else (false, character)

    static member updateInputScan character (entity : Entity) world =
        let bodyId = entity.GetBodyId world
        let grounded = World.getBodyGrounded bodyId world
        if character.AttackOpt.IsNone || not grounded then

            // update position
            let forward = character.Rotation.Forward
            let right = character.Rotation.Right
            let walkSpeed = character.WalkSpeed * if grounded then 1.0f else 0.75f
            let walkVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
            let position = if walkVelocity <> v3Zero then character.Position + walkVelocity else character.Position
            let character = { character with Position = position }

            // update rotation
            let turnSpeed = character.TurnSpeed * if grounded then 1.0f else 0.75f
            let turnVelocity =
                (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
            let rotation = if turnVelocity <> 0.0f then character.Rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else character.Rotation
            { character with Rotation = rotation }

        else character

    static member update time character world =

        // update attack
        let attackOpt =
            match character.AttackOpt with
            | Some attack ->
                let localTime = time - attack.AttackTime
                if localTime < 55 || localTime < 110 && attack.FollowUpBuffered
                then Some attack
                else None
            | None -> None
        let character = { character with AttackOpt = attackOpt }
        
        // update animation
        let traversalAnimations = Character.computeTraversalAnimations character
        let actionAnimationOpt = Character.tryComputeActionAnimation time character.AttackOpt world
        let animations = Array.append (Option.toArray actionAnimationOpt) (Array.ofList traversalAnimations)
        let character = { character with Animations = animations }

        // fin
        character

    static member initial position rotation =
        { Position = position
          Rotation = rotation
          LinearVelocity = v3Zero
          AngularVelocity = v3Zero
          PositionPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> position) |> Queue.ofSeq
          RotationPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> rotation) |> Queue.ofSeq
          LinearVelocityPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq
          AngularVelocityPrevious = Array.init (dec Constants.Gameplay.InterpolationSteps) (fun _ -> v3Zero) |> Queue.ofSeq
          AttackOpt = None
          WalkSpeed = 0.05f
          TurnSpeed = 0.05f
          JumpSpeed = 5.0f
          Animations = [||]
          Jump = JumpState.initial
          BodyShape = CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
          CharacterProperties = CharacterProperties.defaultProperties
          AnimatedModel = Assets.Gameplay.JoanModel }

    static member initialPlayer position rotation =
        let player = Character.initial position rotation
        { player with
            WalkSpeed = Constants.Gameplay.PlayerWalkSpeed
            TurnSpeed = Constants.Gameplay.PlayerTurnSpeed
            JumpSpeed = Constants.Gameplay.PlayerJumpSpeed }

    static member initialEnemy position rotation =
        let enemy = Character.initial position rotation
        { enemy with
            BodyShape = CapsuleShape { Height = 1.3f; Radius = 0.2f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
            Character.CharacterProperties.PenetrationDepthMax = 0.1f } // deeper penetration needed to make enemies able to climb stairs