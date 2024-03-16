namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

    type CharacterModel =
        { CharacterTime : int64
          LastTimeJump : int64
          LastTimeOnGround : int64
          AnimatedModel : AnimatedModel AssetTag }

        static member initial =
            { CharacterTime = 0L
              LastTimeJump = 0L
              LastTimeOnGround = 0L
              AnimatedModel = Assets.Default.AnimatedModel }

    type CharacterMessage =
        | UpdateMessage
        | TryJump of KeyboardKeyData
        interface Message

    type CharacterCommand =
        | UpdateCommand
        | PostUpdate
        | Jump
        interface Command

    type CharacterDispatcher () =
        inherit Entity3dDispatcher<CharacterModel, CharacterMessage, CharacterCommand> (true, CharacterModel.initial)

        static let [<Literal>] WalkSpeed = 0.04f
        static let [<Literal>] TurnSpeed = 0.035f
        static let [<Literal>] JumpSpeed = 6.0f

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>]

        override this.Initialize (character, _) =
            [Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.AnimatedModel := character.AnimatedModel
             Entity.BodyType == KinematicCharacter
             Entity.SleepingAllowed == false
             Entity.BodyShape == CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.UpdateEvent => UpdateMessage
             Entity.UpdateEvent => UpdateCommand
             Game.PostUpdateEvent => PostUpdate
             Game.KeyboardKeyDownEvent =|> fun evt -> TryJump evt.Data]

        override this.Message (character, message, entity, world) =

            match message with
            | UpdateMessage ->
                let time = inc character.CharacterTime
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let character =
                    { character with
                        CharacterTime = time
                        LastTimeOnGround = if grounded then time else character.LastTimeOnGround }
                just character

            | TryJump keyboardKeyData ->
                let sinceJump = character.CharacterTime - character.LastTimeJump
                let sinceOnGround = character.CharacterTime - character.LastTimeOnGround
                if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 12L && sinceOnGround < 10L then
                    let character = { character with LastTimeJump = character.CharacterTime }
                    withSignal Jump character
                else just character

        override this.Command (_, command, entity, world) =

            match command with
            | UpdateCommand ->

                // apply physics-based animations
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let linearVelocity = entity.GetLinearVelocity world
                let angularVelocity = entity.GetAngularVelocity world
                let forwardness = (Vector3.Dot (linearVelocity * 32.0f, rotation.Forward))
                let backness = (Vector3.Dot (linearVelocity * 32.0f, -rotation.Forward))
                let rightness = (Vector3.Dot (linearVelocity * 32.0f, rotation.Right))
                let leftness = (Vector3.Dot (linearVelocity * 32.0f, -rotation.Right))
                let turnRightness = (angularVelocity * v3Up).Length () * 32.0f
                let turnLeftness = -turnRightness
                let animations = [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
                let animations =
                    if forwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.0f; Weight = forwardness; BoneFilterOpt = None } :: animations
                    elif backness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBack"; Playback = Loop; Rate = 1.0f; Weight = backness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if rightness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRight"; Playback = Loop; Rate = 1.0f; Weight = rightness; BoneFilterOpt = None } :: animations
                    elif leftness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeft"; Playback = Loop; Rate = 1.0f; Weight = leftness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if turnRightness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRight"; Playback = Loop; Rate = 1.0f; Weight = turnRightness; BoneFilterOpt = None } :: animations
                    elif turnLeftness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeft"; Playback = Loop; Rate = 1.0f; Weight = turnLeftness; BoneFilterOpt = None } :: animations
                    else animations
                let world = entity.SetAnimations (List.toArray animations) world

                // apply walk velocity
                let forward = rotation.Forward
                let right = rotation.Right
                let walkSpeed = if grounded then WalkSpeed else WalkSpeed * 0.75f
                let walkVelocity = 
                    (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkSpeed else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkSpeed else v3Zero)
                let world =
                    if walkVelocity <> v3Zero
                    then World.setBodyCenter (position + walkVelocity) bodyId world
                    else world

                // apply turn velocity
                let turnSpeed = if grounded then TurnSpeed else TurnSpeed * 0.75f
                let turnVelocity =
                    (if World.isKeyboardKeyDown KeyboardKey.Right world then -turnSpeed else 0.0f) +
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then turnSpeed else 0.0f)
                let world =
                    if turnVelocity <> 0.0f
                    then World.setBodyRotation (rotation * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)) bodyId world
                    else world
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