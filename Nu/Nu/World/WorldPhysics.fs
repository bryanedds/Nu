// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldPhysics =

    type World with

        static member internal getPhysicsEngine2d world =
            world.Subsystems.PhysicsEngine2d

        static member internal setPhysicsEngine2d physicsEngine world =
            World.updateSubsystems (fun subsystems -> { subsystems with PhysicsEngine2d = physicsEngine }) world

        static member internal updatePhysicsEngine2d updater world =
            World.setPhysicsEngine2d (updater (World.getPhysicsEngine2d world)) world

        static member internal getPhysicsEngine3d world =
            world.Subsystems.PhysicsEngine3d

        static member internal setPhysicsEngine3d physicsEngine world =
            World.updateSubsystems (fun subsystems -> { subsystems with PhysicsEngine3d = physicsEngine }) world

        static member internal updatePhysicsEngine3d updater world =
            World.setPhysicsEngine3d (updater (World.getPhysicsEngine3d world)) world

        /// Localize a body shape to a specific size.
        [<FunctionBinding>]
        static member localizeBodyShape (size : Vector3) (bodyShape : BodyShape) (world : World) =
            ignore world // for world parameter for scripting
            Physics.localizeBodyShape size bodyShape

        /// Enqueue a 2d physics message in the world.
        static member enqueuePhysicsMessage2d (message : PhysicsMessage) world =
            let world =
                match message with
                | CreateBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    World.publishPlus message.BodyId Events.BodyAddingEvent eventTrace Game.Handle false false world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                        World.publishPlus bodyId Events.BodyAddingEvent eventTrace Game.Handle false false world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    let world = World.publishPlus { BodyId = message.BodyId } Events.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    let world = World.publishPlus message.BodyId Events.BodyRemovingEvent eventTrace Game.Handle false false world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (bodyId : BodyId) ->
                        let world = World.publishPlus { BodyId = bodyId } Events.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                        let world = World.publishPlus bodyId Events.BodyRemovingEvent eventTrace Game.Handle false false world
                        world)
                        world message.BodyIds
                | _ -> world
            World.updatePhysicsEngine2d (fun physicsEngine -> physicsEngine.EnqueueMessage message) world

        /// Enqueue multiple 2d physics messages to the world.
        static member enqueuePhysicsMessages2d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.enqueuePhysicsMessage2d message world) world messages

        /// Enqueue a 3d physics message in the world.
        static member enqueuePhysicsMessage3d (message : PhysicsMessage) world =
            let world =
                match message with
                | CreateBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    World.publishPlus message.BodyId Events.BodyAddingEvent eventTrace Game.Handle false false world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let bodyId = { BodySource = message.BodySource; BodyIndex = bodyProperties.BodyIndex }
                        World.publishPlus bodyId Events.BodyAddingEvent eventTrace Game.Handle false false world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    let world = World.publishPlus { BodyId = message.BodyId } Events.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                    let world = World.publishPlus message.BodyId Events.BodyRemovingEvent eventTrace Game.Handle false false world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (bodyId : BodyId) ->
                        let world = World.publishPlus { BodyId = bodyId } Events.BodySeparationImplicitEvent eventTrace Game.Handle false false world
                        let world = World.publishPlus bodyId Events.BodyRemovingEvent eventTrace Game.Handle false false world
                        world)
                        world message.BodyIds
                | _ -> world
            World.updatePhysicsEngine3d (fun physicsEngine -> physicsEngine.EnqueueMessage message) world

        /// Enqueue multiple 3d physics messages to the world.
        static member enqueuePhysicsMessages3d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.enqueuePhysicsMessage3d message world) world messages

        /// Check that the world contains a body with the given physics id.
        [<FunctionBinding>]
        static member getBodyExists bodyId world =
            world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId ||
            world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId

        /// Get the contact normals of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyContactNormals bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyContactNormals bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); []

        /// Get the linear velocity of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyLinearVelocity bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyLinearVelocity bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyLinearVelocity bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); v3Zero

        /// Get the contact normals where the body with the given physics id is touching the ground.
        [<FunctionBinding>]
        static member getBodyToGroundContactNormals bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormals bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormals bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); []

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactNormalOpt bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormalOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormalOpt bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); None

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactTangentOpt bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactTangentOpt bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactTangentOpt bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); None

        /// Check that the body with the given physics id is on the ground.
        [<FunctionBinding>]
        static member getBodyGrounded bodyId world =
            if world.Subsystems.PhysicsEngine3d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine3d.IsBodyOnGround bodyId
            elif world.Subsystems.PhysicsEngine2d.GetBodyExists bodyId then
                world.Subsystems.PhysicsEngine2d.IsBodyOnGround bodyId
            else Log.debug ("Body for '" + scstring bodyId + "' not found."); false

        /// Send a physics message to create a physics body.
        [<FunctionBinding>]
        static member createBody is2d bodyId (bodyProperties : BodyProperties) world =
            let createBodyMessage = CreateBodyMessage { BodyId = bodyId; BodyProperties = bodyProperties }
            if not is2d
            then World.enqueuePhysicsMessage3d createBodyMessage world
            else World.enqueuePhysicsMessage2d createBodyMessage world

        /// Send a physics message to create several physics bodies.
        [<FunctionBinding>]
        static member createBodies is2d bodySource bodiesProperties world =
            let createBodiesMessage = CreateBodiesMessage { BodySource = bodySource; BodiesProperties = bodiesProperties }
            if not is2d
            then World.enqueuePhysicsMessage3d createBodiesMessage world
            else World.enqueuePhysicsMessage2d createBodiesMessage world

        /// Send a physics message to destroy a physics body.
        [<FunctionBinding>]
        static member destroyBody is2d bodyId world =
            let destroyBodyMessage = DestroyBodyMessage { BodyId = bodyId }
            if not is2d
            then World.enqueuePhysicsMessage3d destroyBodyMessage world
            else World.enqueuePhysicsMessage2d destroyBodyMessage world

        /// Send a physics message to destroy several physics bodies.
        [<FunctionBinding>]
        static member destroyBodies is2d bodyIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { BodyIds = bodyIds }
            if not is2d
            then World.enqueuePhysicsMessage3d destroyBodiesMessage world
            else World.enqueuePhysicsMessage2d destroyBodiesMessage world

        /// Send a physics message to create a physics joint.
        [<FunctionBinding>]
        static member createJoint is2d jointSource jointProperties world =
            let createJointMessage = CreateJointMessage { JointSource = jointSource; JointProperties = jointProperties }
            if not is2d
            then World.enqueuePhysicsMessage3d createJointMessage world
            else World.enqueuePhysicsMessage2d createJointMessage world

        /// Send a physics message to create physics joints.
        [<FunctionBinding>]
        static member createJoints is2d jointSource jointsProperties world =
            let createJointsMessage = CreateJointsMessage { JointsSource = jointSource; JointsProperties = jointsProperties }
            if not is2d
            then World.enqueuePhysicsMessage3d createJointsMessage world
            else World.enqueuePhysicsMessage2d createJointsMessage world

        /// Send a physics message to destroy a physics joint.
        [<FunctionBinding>]
        static member destroyJoint is2d jointId world =
            let destroyJointMessage = DestroyJointMessage { JointId = jointId }
            if not is2d
            then World.enqueuePhysicsMessage3d destroyJointMessage world
            else World.enqueuePhysicsMessage2d destroyJointMessage world

        /// Send a physics message to destroy physics joints.
        [<FunctionBinding>]
        static member destroyJoints is2d jointIds world =
            let destroyJointsMessage = DestroyJointsMessage { JointIds = jointIds }
            if not is2d
            then World.enqueuePhysicsMessage3d destroyJointsMessage world
            else World.enqueuePhysicsMessage2d destroyJointsMessage world

        /// Send a physics message to set the enabled-ness of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyEnabled enabled bodyId world =
            let setBodyEnabledMessage = SetBodyEnabledMessage { BodyId = bodyId; Enabled = enabled }
            let world = World.enqueuePhysicsMessage3d setBodyEnabledMessage world
            let world = World.enqueuePhysicsMessage2d setBodyEnabledMessage world
            world

        /// Send a physics message to set the position of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyCenter center bodyId world =
            let setBodyCenterMessage = SetBodyCenterMessage { BodyId = bodyId; Center = center }
            let world = World.enqueuePhysicsMessage3d setBodyCenterMessage world
            let world = World.enqueuePhysicsMessage2d setBodyCenterMessage world
            world

        /// Send a physics message to set the rotation of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyRotation rotation bodyId world =
            let setBodyRotationMessage = SetBodyRotationMessage { BodyId = bodyId; Rotation = rotation }
            let world = World.enqueuePhysicsMessage3d setBodyRotationMessage world
            let world = World.enqueuePhysicsMessage2d setBodyRotationMessage world
            world

        /// Send a physics message to set the linear velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyLinearVelocity linearVelocity bodyId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { BodyId = bodyId; LinearVelocity = linearVelocity }
            let world = World.enqueuePhysicsMessage3d setBodyLinearVelocityMessage world
            let world = World.enqueuePhysicsMessage2d setBodyLinearVelocityMessage world
            world

        /// Send a physics message to set the angular velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyAngularVelocity angularVelocity bodyId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { BodyId = bodyId; AngularVelocity = angularVelocity }
            let world = World.enqueuePhysicsMessage3d setBodyAngularVelocityMessage world
            let world = World.enqueuePhysicsMessage2d setBodyAngularVelocityMessage world
            world

        /// Send a physics message to apply linear impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyLinearImpulse linearImpulse offset bodyId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { BodyId = bodyId; LinearImpulse = linearImpulse; Offset = offset }
            let world = World.enqueuePhysicsMessage3d applyBodyLinearImpulseMessage world
            let world = World.enqueuePhysicsMessage2d applyBodyLinearImpulseMessage world
            world

        /// Send a physics message to apply angular impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyAngularImpulse angularImpulse bodyId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { BodyId = bodyId; AngularImpulse = angularImpulse }
            let world = World.enqueuePhysicsMessage3d applyBodyAngularImpulseMessage world
            let world = World.enqueuePhysicsMessage2d applyBodyAngularImpulseMessage world
            world

        /// Send a physics message to apply force to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyForce force offset bodyId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { BodyId = bodyId; Force = force; Offset = offset }
            let world = World.enqueuePhysicsMessage3d applyBodyForceMessage world
            let world = World.enqueuePhysicsMessage2d applyBodyForceMessage world
            world

        /// Send a physics message to apply torque to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyTorque torque bodyId world =
            let applyBodyTorqueMessage = ApplyBodyTorqueMessage { BodyId = bodyId; Torque = torque }
            let world = World.enqueuePhysicsMessage3d applyBodyTorqueMessage world
            let world = World.enqueuePhysicsMessage2d applyBodyTorqueMessage world
            world
            
        static member private setBodyObservableInternal allowInternalIndexing observable (bodyId : BodyId) world =
            if allowInternalIndexing || bodyId.BodyIndex <> Constants.Physics.InternalIndex then
                let setBodyObservableMessage = SetBodyObservableMessage { BodyId = bodyId; Observable = observable }
                let world = World.enqueuePhysicsMessage3d setBodyObservableMessage world
                let world = World.enqueuePhysicsMessage2d setBodyObservableMessage world
                world
            else Log.debug "Set the observability of an internally indexed body from outside the engine is prohibited."; world

        /// Send a physics message to set the observability of a body.
        /// Disabling observability where it's not needed can significantly increase performance.
        [<FunctionBinding>]
        static member setBodyObservable observable bodyId world =
            World.setBodyObservableInternal false observable bodyId world

        static member internal updateBodyObservable subscribing (bodySource : Entity) world =
            let observable =
                subscribing ||
                let collisionEventAddress = atooa (Events.BodyCollisionEvent --> bodySource.EntityAddress)
                match (World.getSubscriptions world).TryGetValue collisionEventAddress with
                | (true, subscriptions) -> OMap.notEmpty subscriptions
                | (false, _) ->
                    let separationEventAddress = atooa (Events.BodySeparationExplicitEvent --> bodySource.EntityAddress)
                    match (World.getSubscriptions world).TryGetValue separationEventAddress with
                    | (true, subscriptions) -> OMap.notEmpty subscriptions
                    | (false, _) -> false
            let bodyId = { BodySource = bodySource; BodyIndex = Constants.Physics.InternalIndex }
            World.setBodyObservableInternal true observable bodyId world