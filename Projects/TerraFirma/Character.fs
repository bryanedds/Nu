namespace TerraFirma
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

    type CharacterModel =
        { CharacterTime : single
          LastTimeJump : single
          LastTimeOnGround : single
          AnimatedModel : AnimatedModel AssetTag }

        static member initial =
            { CharacterTime = 0.0f
              LastTimeJump = 0.0f
              LastTimeOnGround = 0.0f
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
        inherit EntityDispatcher3d<CharacterModel, CharacterMessage, CharacterCommand> (true, true, CharacterModel.initial)

        static let [<Literal>] WalkForce = 12.0f
        static let [<Literal>] TurnForce = 8.0f
        static let [<Literal>] JumpForce = 7.0f

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>]

        override this.Initialize (character, _) =
            [Entity.MaterialProperties == MaterialProperties.defaultProperties
             Entity.AnimatedModel := character.AnimatedModel
             Entity.Friction == 1.0f
             Entity.LinearDamping == 0.5f
             Entity.AngularDamping == 0.999f
             Entity.AngularFactor == v3 0.0f 0.1f 0.0f
             Entity.BodyShape == BodyCapsule { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None }
             Entity.UpdateEvent => UpdateMessage
             Entity.UpdateEvent => UpdateCommand
             Game.PostUpdateEvent => PostUpdate
             Game.KeyboardKeyDownEvent =|> fun evt -> TryJump evt.Data]

        override this.Message (character, message, entity, world) =

            match message with
            | UpdateMessage ->
                let time = character.CharacterTime + (let d = world.GameDelta in d.Seconds)
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
                if keyboardKeyData.KeyboardKey = KeyboardKey.Space && not keyboardKeyData.Repeated && sinceJump >= 0.1f && sinceOnGround < 0.1f then
                    let character = { character with LastTimeJump = character.CharacterTime }
                    withSignal Jump character
                else just character

        override this.Command (_, command, entity, world) =

            match command with
            | UpdateCommand ->

                // apply movement forces
                let bodyId = entity.GetBodyId world
                let grounded = World.getBodyGrounded bodyId world
                let rotation = entity.GetRotation world
                let forward = rotation.Forward
                let right = rotation.Right
                let contactNormalOpt = World.getBodyToGroundContactNormalOpt bodyId world
                let walkForceScalar = if grounded then WalkForce else WalkForce * 0.5f
                let walkForce = 
                    (if World.isKeyboardKeyDown KeyboardKey.W world || World.isKeyboardKeyDown KeyboardKey.Up world then forward * walkForceScalar else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.S world || World.isKeyboardKeyDown KeyboardKey.Down world then -forward * walkForceScalar else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkForceScalar else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkForceScalar else v3Zero)
                let world =
                    if walkForce <> v3Zero then
                        match contactNormalOpt with
                        | Some contactNormal ->
                            let walkForward = walkForce.Normalized
                            let groundPlane = Plane3 (contactNormal, -1.0f)
                            let slope = Vector3.Project (walkForward, groundPlane) - v3Up
                            let walkForceOriented =
                                if Vector3.Dot (slope, v3Up) > 0.01f then // guard against generating massive force
                                    let angleBetween = walkForward.AngleBetween slope
                                    let rotationMatrix = Matrix4x4.CreateFromAxisAngle (Vector3.Cross (walkForward, v3Up), angleBetween)
                                    let walkForceOriented = Vector3.Transform (walkForce, rotationMatrix)
                                    walkForceOriented
                                else walkForce
                            World.applyBodyForce walkForceOriented v3Zero bodyId world
                        | None -> World.applyBodyForce walkForce v3Zero bodyId world
                    else world

                // apply turn force
                let turnForce = if grounded then TurnForce else TurnForce * 0.5f
                let world = if World.isKeyboardKeyDown KeyboardKey.Right world then World.applyBodyTorque (-v3Up * turnForce) bodyId world else world
                let world = if World.isKeyboardKeyDown KeyboardKey.Left world then World.applyBodyTorque (v3Up * turnForce) bodyId world else world

                // apply physics-based animations
                let linearVelocity = World.getBodyLinearVelocity bodyId world
                let angularVelocity = World.getBodyAngularVelocity bodyId world
                let forwardness = (Vector3.Dot (linearVelocity, rotation.Forward))
                let backwardness = (Vector3.Dot (linearVelocity, -rotation.Forward))
                let rightwardness = (Vector3.Dot (linearVelocity, rotation.Right))
                let leftwardness = (Vector3.Dot (linearVelocity, -rotation.Right))
                let turnRightwardness = (angularVelocity * v3Up).Length ()
                let turnLeftwardness = -turnRightwardness
                let animations = [{ StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
                let animations =
                    if forwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.5f; Weight = forwardness; BoneFilterOpt = None } :: animations
                    elif backwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|WalkBackward"; Playback = Loop; Rate = 1.5f; Weight = backwardness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if rightwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|WalkRightward"; Playback = Loop; Rate = 1.5f; Weight = rightwardness; BoneFilterOpt = None } :: animations
                    elif leftwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|WalkLeftward"; Playback = Loop; Rate = 1.5f; Weight = leftwardness; BoneFilterOpt = None } :: animations
                    else animations
                let animations =
                    if turnRightwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|TurnRightward"; Playback = Loop; Rate = 1.5f; Weight = turnRightwardness; BoneFilterOpt = None } :: animations
                    elif turnLeftwardness >= 0.1f then { StartTime = 0.0f; LifeTimeOpt = None; Name = "Armature|TurnLeftward"; Playback = Loop; Rate = 1.5f; Weight = turnLeftwardness; BoneFilterOpt = None } :: animations
                    else animations
                let world = entity.SetAnimations (List.toArray animations) world
                just world

            | PostUpdate ->
                let rotation = entity.GetRotation world
                let position = entity.GetPosition world
                let world = World.setEyeRotation3d rotation world
                let world = World.setEyeCenter3d (position + v3Up * 1.5f - rotation.Forward * 3.0f) world
                just world

            | Jump ->
                let bodyId = entity.GetBodyId world
                let world = World.applyBodyLinearImpulse (v3Up * JumpForce) v3Zero bodyId world
                just world