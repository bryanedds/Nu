// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace MyGame
open System
open Prime
open Nu

[<AutoOpen>]
module CharacterDispatcher =

    type CharacterDispatcher () =
        inherit EntityDispatcher3d (true, true)

        static member Facets =
            [typeof<AnimatedModelFacet>
             typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.AnimatedModel Assets.Default.AnimatedModel
             define Entity.BodyType Dynamic
             define Entity.Friction 1.0f
             define Entity.LinearDamping 0.5f
             define Entity.AngularDamping 1.0f
             define Entity.AngularFactor (v3 0.0f 0.1f 0.0f)
             define Entity.BodyShape (BodyCapsule { Height = 1.0f; Radius = 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.8f 0.0f)); PropertiesOpt = None })]

        override this.Update (entity, world) =
            let bodyId = entity.GetBodyId world
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let grounded = World.getBodyGrounded bodyId world
            let walkForce = if grounded then 2.0f else 0.5f
            let world = if World.isKeyboardKeyDown KeyboardKey.Up world then World.applyBodyForce (forward * walkForce) v3Zero bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.Down world then World.applyBodyForce (-forward * walkForce) v3Zero bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.A world then World.applyBodyForce (-right * walkForce) v3Zero bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.D world then World.applyBodyForce (right * walkForce) v3Zero bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.Right world then World.applyBodyTorque (v3Up * -5.0f) bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.Left world then World.applyBodyTorque (v3Up * 5.0f) bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.Space world && grounded then World.applyBodyLinearImpulse (v3Up * 0.333f) v3Zero bodyId world else world
            let world = World.setEyeRotation3d rotation world
            let world = World.setEyeCenter3d (position + v3Up * 1.5f - rotation.Forward * 3.0f) world
            let linearVelocity = World.getBodyLinearVelocity bodyId world
            let angularVelocity = World.getBodyAngularVelocity bodyId world
            let forwardness = (linearVelocity * rotation.Forward).Length ()
            let backwardness = -forwardness
            let rightwardness = (linearVelocity * rotation.Right).Length ()
            let leftwardness = -rightwardness
            let turnRightwardness = (angularVelocity * v3Up).Length ()
            let turnLeftwardness = -turnRightwardness
            let animations =
                [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
            let animations =
                if forwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                elif backwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBackward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if rightwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRightward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                elif leftwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeftward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if turnRightwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRightward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                elif turnLeftwardness > 0.1f then
                    { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeftward"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None } :: animations
                else animations
            let world = entity.SetAnimations (List.toArray animations) world
            world