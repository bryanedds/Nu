namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

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

    type CharacterModel =
        { CharacterTime : int64
          Jump : JumpState
          AttackOpt : AttackState option
          AnimatedModel : AnimatedModel AssetTag }

        static member initial =
            { CharacterTime = 0L
              Jump = JumpState.initial
              AttackOpt = None
              AnimatedModel = Assets.Default.AnimatedModel }

    type CharacterMessage =
        | UpdateMessage
        | TryAct of KeyboardKeyData
        interface Message

    type CharacterCommand =
        | UpdateCommand
        | PostUpdate
        | Jump
        interface Command

    type CharacterDispatcher () =
        inherit Entity3dDispatcher<CharacterModel, CharacterMessage, CharacterCommand> (true, CharacterModel.initial)

        static let [<Literal>] WalkSpeed = 0.06f
        static let [<Literal>] TurnSpeed = 0.035f
        static let [<Literal>] JumpSpeed = 6.0f

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.LinearVelocityPrevious v3Zero
             define Entity.AngularVelocityPrevious v3Zero]

        override this.Initialize (character, _) =
            [Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.AnimatedModel := character.AnimatedModel
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == false
             Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.UpdateEvent => UpdateMessage
             Entity.UpdateEvent => UpdateCommand
             Game.PostUpdateEvent => PostUpdate
             Game.KeyboardKeyDownEvent =|> fun evt -> TryAct evt.Data]

        override this.Message (character, message, entity, world) =

            match message with
            | UpdateMessage ->
                let time = inc character.CharacterTime
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let character = { character with CharacterTime = time }
                let character = if grounded then { character with Jump.LastTimeOnGround = time } else character
                let character =
                    match character.AttackOpt with
                    | Some attack ->
                        let localTime = character.CharacterTime - attack.AttackTime
                        if localTime >= 55 && not attack.FollowUpBuffered || localTime >= 110
                        then { character with AttackOpt = None }
                        else character
                    | None -> character
                just character

            | TryAct keyboardKeyData ->
                let time = inc character.CharacterTime
                let sinceJump = character.CharacterTime - character.Jump.LastTime
                let sinceOnGround = character.CharacterTime - character.Jump.LastTimeOnGround
                if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 12L && sinceOnGround < 10L then
                    let character = { character with Jump.LastTime = character.CharacterTime }
                    withSignal Jump character
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
                    just character
                else just character

        override this.Command (character, command, entity, world) =

            match command with
            | UpdateCommand ->

                // compute physics-based animations
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let linearVelocity = entity.GetLinearVelocity world
                let linearVelocityPrevious = entity.GetLinearVelocityPrevious world
                let linearVelocityAvg = (linearVelocity + linearVelocityPrevious) * 0.5f
                let angularVelocity = entity.GetAngularVelocity world
                let angularVelocityPrevious = entity.GetAngularVelocityPrevious world
                let angularVelocityAvg = (angularVelocity + angularVelocityPrevious) * 0.5f
                let forwardness = (Vector3.Dot (linearVelocityAvg * 32.0f, rotation.Forward))
                let backness = (Vector3.Dot (linearVelocityAvg * 32.0f, -rotation.Forward))
                let rightness = (Vector3.Dot (linearVelocityAvg * 32.0f, rotation.Right))
                let leftness = (Vector3.Dot (linearVelocityAvg * 32.0f, -rotation.Right))
                let turnRightness = (angularVelocityAvg * v3Up).Length () * 48.0f
                let turnLeftness = -turnRightness
                let animations = [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
                let animations =
                    if forwardness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f forwardness; BoneFilterOpt = None } :: animations
                    elif backness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBack"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f backness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if rightness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRight"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f rightness; BoneFilterOpt = None } :: animations
                    elif leftness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeft"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f leftness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if turnRightness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f turnRightness; BoneFilterOpt = None } :: animations
                    elif turnLeftness >= 0.01f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = max 0.025f turnLeftness; BoneFilterOpt = None } :: animations
                    else animations

                // compute action animations
                let animations =
                    match character.AttackOpt with
                    | Some attack ->
                        let localTime = character.CharacterTime - attack.AttackTime
                        let world =
                            match localTime with
                            | 0L -> World.playSound 1.0f Assets.Default.Sound world
                            | 55L -> if attack.FollowUpBuffered then World.playSound 1.0f Assets.Default.Sound world else world
                            | _ -> world
                        let animationStartTime = GameTime.ofUpdates (world.UpdateTime - localTime % 55L)
                        let animationName = if localTime <= 55 then "Armature|AttackVertical" else "Armature|AttackHorizontal"
                        { StartTime = animationStartTime; LifeTimeOpt = None; Name = animationName; Playback = Once; Rate = 1.0f; Weight = 32.0f; BoneFilterOpt = None } :: animations
                    | None -> animations

                // compute transforms
                let (position, rotation) =
                    if character.AttackOpt.IsNone || not grounded then

                        // compute position
                        let forward = rotation.Forward
                        let right = rotation.Right
                        let walkSpeed = if grounded then WalkSpeed else WalkSpeed * 0.75f
                        let walkVelocity =
                            (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                            (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                            (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                            (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
                        let position = if walkVelocity <> v3Zero then position + walkVelocity else position

                        // compute rotation
                        let turnSpeed = if grounded then TurnSpeed else TurnSpeed * 0.75f
                        let turnVelocity =
                            (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                            (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                        let rotation = if turnVelocity <> 0.0f then rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity) else rotation
                        (position, rotation)

                    else (position, rotation)

                // apply computed values
                let world = entity.SetAnimations (List.toArray animations) world
                let world = entity.SetLinearVelocityPrevious linearVelocityAvg world
                let world = entity.SetAngularVelocityPrevious angularVelocityAvg world
                let world = World.setBodyCenter position bodyId world
                let world = World.setBodyRotation rotation bodyId world
                just world

            | PostUpdate ->
                let rotation = entity.GetRotation world
                let position = entity.GetPosition world
                let world = World.setEye3dRotation rotation world
                let world = World.setEye3dCenter (position + v3Up * 1.5f - rotation.Forward * 3.0f) world
                just world

            | Jump ->
                let bodyId = entity.GetBodyId world
                let world = World.jumpBody true JumpSpeed bodyId world
                just world