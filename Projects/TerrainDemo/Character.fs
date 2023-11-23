// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace MyGame
open System
open System.Numerics
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
             define Entity.AngularDamping 0.999f
             define Entity.AngularFactor (v3 0.0f 0.1f 0.0f)
             define Entity.BodyShape (BodyCapsule { Height = 1.0f; Radius = 0.3f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.8f 0.0f)); PropertiesOpt = None })]

        override this.Update (entity, world) =

            // apply movement forces
            let bodyId = entity.GetBodyId world
            let grounded = World.getBodyGrounded bodyId world
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let forward = rotation.Forward
            let right = rotation.Right
            let contactNormalOpt = World.getBodyToGroundContactNormalOpt bodyId world
            let walkForceScalar = if grounded then 1.25f else 0.625f
            let walkForce = 
                (if World.isKeyboardKeyDown KeyboardKey.W world then forward * walkForceScalar else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world then -forward * walkForceScalar else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then -right * walkForceScalar else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.D world then right * walkForceScalar else v3Zero)
            let world =
                match contactNormalOpt with
                | Some contactUp when walkForce <> v3Zero ->
                    let walkForward = walkForce.Normalized
                    let groundPlane = Plane3 (contactUp, -1.0f)
                    let slope = Vector3.Project (walkForward, groundPlane) - v3Up
                    let walkForceOriented =
                        if Vector3.Dot (slope, v3Up) > 0.0f then
                            let angleBetween = walkForward.AngleBetween slope
                            let rotationMatrix = Matrix4x4.CreateFromAxisAngle (Vector3.Cross (walkForward, v3Up), angleBetween)
                            let walkForceOriented = Vector3.Transform (walkForce, rotationMatrix)
                            walkForceOriented
                        else walkForce
                    World.applyBodyForce walkForceOriented v3Zero bodyId world
                | Some _ | None -> World.applyBodyForce walkForce v3Zero bodyId world

            // apply turn force
            let turnForce = if grounded then 1.0f else 0.5f
            let world = if World.isKeyboardKeyDown KeyboardKey.Right world then World.applyBodyTorque (-v3Up * turnForce) bodyId world else world
            let world = if World.isKeyboardKeyDown KeyboardKey.Left world then World.applyBodyTorque (v3Up * turnForce) bodyId world else world

            // apply jump force
            let jumpForce = 0.4f
            let world = if World.isKeyboardKeyDown KeyboardKey.Space world && grounded then World.applyBodyLinearImpulse (v3Up * jumpForce) v3Zero bodyId world else world

            // apply physics-based animations
            let linearVelocity = World.getBodyLinearVelocity bodyId world
            let angularVelocity = World.getBodyAngularVelocity bodyId world
            let forwardness = (Vector3.Dot (linearVelocity, rotation.Forward))
            let backwardness = (Vector3.Dot (linearVelocity, -rotation.Forward))
            let rightwardness = (Vector3.Dot (linearVelocity, rotation.Right))
            let leftwardness = (Vector3.Dot (linearVelocity, -rotation.Right))
            let turnRightwardness = (angularVelocity * v3Up).Length ()
            let turnLeftwardness = -turnRightwardness
            let animations = [{ StartTime = 0L; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 0.5f; BoneFilterOpt = None }]
            let animations =
                if forwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkForward"; Playback = Loop; Rate = 1.5f; Weight = forwardness; BoneFilterOpt = None } :: animations
                elif backwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkBackward"; Playback = Loop; Rate = 1.5f; Weight = backwardness; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if rightwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkRightward"; Playback = Loop; Rate = 1.5f; Weight = rightwardness; BoneFilterOpt = None } :: animations
                elif leftwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|WalkLeftward"; Playback = Loop; Rate = 1.5f; Weight = leftwardness; BoneFilterOpt = None } :: animations
                else animations
            let animations =
                if turnRightwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnRightward"; Playback = Loop; Rate = 1.5f; Weight = turnRightwardness; BoneFilterOpt = None } :: animations
                elif turnLeftwardness >= 0.1f then { StartTime = 0L; LifeTimeOpt = None; Name = "Armature|TurnLeftward"; Playback = Loop; Rate = 1.5f; Weight = turnLeftwardness; BoneFilterOpt = None } :: animations
                else animations
            let world = entity.SetAnimations (List.toArray animations) world

            // orient camera            
            let world = World.setEyeRotation3d rotation world
            let world = World.setEyeCenter3d (position + v3Up * 1.5f - rotation.Forward * 3.0f) world
            world