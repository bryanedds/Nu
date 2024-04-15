// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

[<AutoOpen>]
module WorldPhysics =

    type World with

        static member internal getPhysicsEngine2d world =
            world.Subsystems.PhysicsEngine2d

        static member internal getPhysicsEngine3d world =
            world.Subsystems.PhysicsEngine3d

        /// Localize a body shape to a specific size.
        static member localizeBodyShape (size : Vector3) (bodyShape : BodyShape) =
            Physics.localizeBodyShape size bodyShape

        /// Handle a 2d physics message in the world.
        static member handlePhysicsMessage2d (message : PhysicsMessage) world =
            let world =
                match message with
                | CreateBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "" EventTrace.empty
                    World.publishPlus message.BodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                        World.publishPlus bodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "" EventTrace.empty
                    let world = World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    let world = World.publishPlus message.BodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (bodyId : BodyId) ->
                        let world = World.publishPlus { BodyId = bodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                        let world = World.publishPlus bodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
                        world)
                        world message.BodyIds
                | _ -> world
            (World.getPhysicsEngine2d world).HandleMessage message
            world

        /// Send multiple 2d physics messages to the world.
        static member handlePhysicsMessages2d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.handlePhysicsMessage2d message world) world messages

        /// Send a 3d physics message in the world.
        static member handlePhysicsMessage3d (message : PhysicsMessage) world =
            let world =
                match message with
                | CreateBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "" EventTrace.empty
                    World.publishPlus message.BodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                        World.publishPlus bodyId Game.Handle.BodyAddingEvent eventTrace Game.Handle false false world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "" EventTrace.empty
                    let world = World.publishPlus { BodyId = message.BodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    let world = World.publishPlus message.BodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "handlePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (bodyId : BodyId) ->
                        let world = World.publishPlus { BodyId = bodyId } Game.Handle.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                        let world = World.publishPlus bodyId Game.Handle.BodyRemovingEvent eventTrace Game.Handle false false world
                        world)
                        world message.BodyIds
                | _ -> world
            (World.getPhysicsEngine3d world).HandleMessage message
            world

        /// Send multiple 3d physics messages to the world.
        static member handlePhysicsMessages3d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.handlePhysicsMessage3d message world) world messages

        /// Check that the world contains a body with the given physics id.
        static member getBodyExists bodyId world =
            world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId ||
            world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId

        /// Get the contact normals of the body with the given physics id.
        static member getBodyContactNormals bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyContactNormals bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                []

        /// Get the linear velocity of the body with the given physics id.
        static member getBodyLinearVelocity bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyLinearVelocity bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyLinearVelocity bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                v3Zero

        /// Get the angular velocity of the body with the given physics id.
        static member getBodyAngularVelocity bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyAngularVelocity bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyAngularVelocity bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                v3Zero

        /// Get the contact normals where the body with the given physics id is touching the ground.
        static member getBodyToGroundContactNormals bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormals bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                []

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        static member getBodyToGroundContactNormalOpt bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormalOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormalOpt bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                None

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        static member getBodyToGroundContactTangentOpt bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactTangentOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactTangentOpt bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                None

        /// Check that the body with the given physics id is on the ground.
        static member getBodyGrounded bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyGrounded bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyGrounded bodyId
            else
                Log.info ("Body for '" + scstring bodyId + "' not found.")
                false

        /// Ray cast against 3d physics bodies.
        static member rayCast3dBodies start stop collisionCategories collisionMask closestOnly world =
            world.Subsystems.PhysicsEngine3d.RayCast (start, stop, collisionCategories, collisionMask, closestOnly)

        /// Ray cast against 2d physics bodies.
        static member rayCast2dBodies start stop collisionCategories collisionMask closestOnly world =
            world.Subsystems.PhysicsEngine2d.RayCast (start, stop, collisionCategories, collisionMask, closestOnly)

        /// Send a physics message to create a physics body.
        static member createBody is2d bodyId (bodyProperties : BodyProperties) world =
            let createBodyMessage = CreateBodyMessage { BodyId = bodyId; BodyProperties = bodyProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodyMessage world
            else World.handlePhysicsMessage2d createBodyMessage world

        /// Send a physics message to create several physics bodies.
        static member createBodies is2d bodySource bodiesProperties world =
            let createBodiesMessage = CreateBodiesMessage { BodySource = bodySource; BodiesProperties = bodiesProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodiesMessage world
            else World.handlePhysicsMessage2d createBodiesMessage world

        /// Send a physics message to destroy a physics body.
        static member destroyBody is2d bodyId world =
            let destroyBodyMessage = DestroyBodyMessage { BodyId = bodyId }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodyMessage world
            else World.handlePhysicsMessage2d destroyBodyMessage world

        /// Send a physics message to destroy several physics bodies.
        static member destroyBodies is2d bodyIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { BodyIds = bodyIds }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodiesMessage world
            else World.handlePhysicsMessage2d destroyBodiesMessage world

        /// Send a physics message to create a physics joint.
        static member createBodyJoint is2d bodyJointSource bodyJointProperties world =
            let createBodyJointMessage = CreateBodyJointMessage { BodyJointSource = bodyJointSource; BodyJointProperties = bodyJointProperties }
            if not is2d
            then World.handlePhysicsMessage3d createBodyJointMessage world
            else World.handlePhysicsMessage2d createBodyJointMessage world

        /// Send a physics message to destroy a physics joint.
        static member destroyBodyJoint is2d bodyTargets bodyJointId world =
            let destroyBodyJointMessage = DestroyBodyJointMessage { BodyJointId = bodyJointId; BodyTargets = bodyTargets }
            if not is2d
            then World.handlePhysicsMessage3d destroyBodyJointMessage world
            else World.handlePhysicsMessage2d destroyBodyJointMessage world

        /// Send a physics message to set the enabled-ness of a body with the given physics id.
        static member setBodyEnabled enabled bodyId world =
            let setBodyEnabledMessage = SetBodyEnabledMessage { BodyId = bodyId; Enabled = enabled }
            let world = World.handlePhysicsMessage3d setBodyEnabledMessage world
            let world = World.handlePhysicsMessage2d setBodyEnabledMessage world
            world

        /// Send a physics message to set the position of a body with the given physics id.
        static member setBodyCenter center bodyId world =
            let setBodyCenterMessage = SetBodyCenterMessage { BodyId = bodyId; Center = center }
            let world = World.handlePhysicsMessage3d setBodyCenterMessage world
            let world = World.handlePhysicsMessage2d setBodyCenterMessage world
            world

        /// Send a physics message to set the rotation of a body with the given physics id.
        static member setBodyRotation rotation bodyId world =
            let setBodyRotationMessage = SetBodyRotationMessage { BodyId = bodyId; Rotation = rotation }
            let world = World.handlePhysicsMessage3d setBodyRotationMessage world
            let world = World.handlePhysicsMessage2d setBodyRotationMessage world
            world

        /// Send a physics message to set the linear velocity of a body with the given physics id.
        static member setBodyLinearVelocity linearVelocity bodyId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { BodyId = bodyId; LinearVelocity = linearVelocity }
            let world = World.handlePhysicsMessage3d setBodyLinearVelocityMessage world
            let world = World.handlePhysicsMessage2d setBodyLinearVelocityMessage world
            world

        /// Send a physics message to set the angular velocity of a body with the given physics id.
        static member setBodyAngularVelocity angularVelocity bodyId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { BodyId = bodyId; AngularVelocity = angularVelocity }
            let world = World.handlePhysicsMessage3d setBodyAngularVelocityMessage world
            let world = World.handlePhysicsMessage2d setBodyAngularVelocityMessage world
            world

        /// Send a physics message to apply linear impulse to a body with the given physics id.
        static member applyBodyLinearImpulse linearImpulse offset bodyId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { BodyId = bodyId; LinearImpulse = linearImpulse; Offset = offset }
            let world = World.handlePhysicsMessage3d applyBodyLinearImpulseMessage world
            let world = World.handlePhysicsMessage2d applyBodyLinearImpulseMessage world
            world

        /// Send a physics message to apply angular impulse to a body with the given physics id.
        static member applyBodyAngularImpulse angularImpulse bodyId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { BodyId = bodyId; AngularImpulse = angularImpulse }
            let world = World.handlePhysicsMessage3d applyBodyAngularImpulseMessage world
            let world = World.handlePhysicsMessage2d applyBodyAngularImpulseMessage world
            world

        /// Send a physics message to apply force to a body with the given physics id.
        static member applyBodyForce force offset bodyId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { BodyId = bodyId; Force = force; Offset = offset }
            let world = World.handlePhysicsMessage3d applyBodyForceMessage world
            let world = World.handlePhysicsMessage2d applyBodyForceMessage world
            world

        /// Send a physics message to apply torque to a body with the given physics id.
        static member applyBodyTorque torque bodyId world =
            let applyBodyTorqueMessage = ApplyBodyTorqueMessage { BodyId = bodyId; Torque = torque }
            let world = World.handlePhysicsMessage3d applyBodyTorqueMessage world
            let world = World.handlePhysicsMessage2d applyBodyTorqueMessage world
            world

        /// Send a physics message to jump to a body with the given physics id (KinematicCharacter only).
        static member jumpBody canJumpInAir jumpSpeed bodyId world =
            let jumpBodyMessage = JumpBodyMessage { BodyId = bodyId; CanJumpInAir = canJumpInAir; JumpSpeed = jumpSpeed }
            let world = World.handlePhysicsMessage3d jumpBodyMessage world
            let world = World.handlePhysicsMessage2d jumpBodyMessage world
            world