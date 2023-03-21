// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

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

        /// Localize a body shape to a specific physics object.
        [<FunctionBinding>]
        static member localizeBodyShape (extent : Vector3) (bodyShape : BodyShape) (world : World) =
            ignore world // for world parameter for scripting
            Physics.localizeBodyShape extent bodyShape

        /// Enqueue a 2d physics message in the world.
        static member enqueuePhysicsMessage2d (message : PhysicsMessage) world =
            let world =
                match message with
                | CreateBodyMessage message ->
                    let physicsId = { SourceId = message.SourceId; CorrelationId = message.BodyProperties.BodyId }
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let physicsId = { SourceId = message.SourceId; CorrelationId = bodyProperties.BodyId }
                        World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    let world = World.publish { BodySourceSimulant = message.SourceSimulant; BodyPhysicsId = message.PhysicsId } Events.BodySeparationImplicit eventTrace Simulants.Game world
                    let world = World.publish message.PhysicsId Events.BodyRemoving eventTrace Simulants.Game world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage2d" "" EventTrace.empty
                    List.fold (fun world (physicsId : PhysicsId) ->
                        let world = World.publish { BodySourceSimulant = message.SourceSimulant; BodyPhysicsId = physicsId } Events.BodySeparationImplicit eventTrace Simulants.Game world
                        let world = World.publish physicsId Events.BodyRemoving eventTrace Simulants.Game world
                        world)
                        world message.PhysicsIds
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
                    let physicsId = { SourceId = message.SourceId; CorrelationId = message.BodyProperties.BodyId }
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world
                | CreateBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let physicsId = { SourceId = message.SourceId; CorrelationId = bodyProperties.BodyId }
                        World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world)
                        world message.BodiesProperties
                | DestroyBodyMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    let world = World.publish { BodySourceSimulant = message.SourceSimulant; BodyPhysicsId = message.PhysicsId } Events.BodySeparationImplicit eventTrace Simulants.Game world
                    let world = World.publish message.PhysicsId Events.BodyRemoving eventTrace Simulants.Game world
                    world
                | DestroyBodiesMessage message ->
                    let eventTrace = EventTrace.debug "World" "enqueuePhysicsMessage3d" "" EventTrace.empty
                    List.fold (fun world (physicsId : PhysicsId) ->
                        let world = World.publish { BodySourceSimulant = message.SourceSimulant; BodyPhysicsId = physicsId } Events.BodySeparationImplicit eventTrace Simulants.Game world
                        let world = World.publish physicsId Events.BodyRemoving eventTrace Simulants.Game world
                        world)
                        world message.PhysicsIds
                | _ -> world
            World.updatePhysicsEngine3d (fun physicsEngine -> physicsEngine.EnqueueMessage message) world

        /// Enqueue multiple 3d physics messages to the world.
        static member enqueuePhysicsMessages3d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.enqueuePhysicsMessage3d message world) world messages

        /// Check that the world contains a body with the given physics id.
        [<FunctionBinding>]
        static member bodyExists physicsId world =
            world.Subsystems.PhysicsEngine3d.BodyExists physicsId ||
            world.Subsystems.PhysicsEngine2d.BodyExists physicsId

        /// Get the contact normals of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyContactNormals physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.GetBodyContactNormals physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyContactNormals physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); []

        /// Get the linear velocity of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyLinearVelocity physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.GetBodyLinearVelocity physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyLinearVelocity physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); v3Zero

        /// Get the contact normals where the body with the given physics id is touching the ground.
        [<FunctionBinding>]
        static member getBodyToGroundContactNormals physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormals physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormals physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); []

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactNormalOpt physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactNormalOpt physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormalOpt physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); None

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactTangentOpt physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.GetBodyToGroundContactTangentOpt physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactTangentOpt physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); None

        /// Check that the body with the given physics id is on the ground.
        [<FunctionBinding>]
        static member isBodyOnGround physicsId world =
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine3d.IsBodyOnGround physicsId
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.IsBodyOnGround physicsId
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); false

        /// Send a physics message to create a physics body.
        [<FunctionBinding>]
        static member createBody (entity : Entity) entityId (bodyProperties : BodyProperties) world =
            let createBodyMessage = CreateBodyMessage { SourceSimulant = entity; SourceId = entityId; BodyProperties = bodyProperties }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d createBodyMessage world
            else World.enqueuePhysicsMessage2d createBodyMessage world

        /// Send a physics message to create several physics bodies.
        [<FunctionBinding>]
        static member createBodies (entity : Entity) entityId bodiesProperties world =
            let createBodiesMessage = CreateBodiesMessage { SourceSimulant = entity; SourceId = entityId; BodiesProperties = bodiesProperties }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d createBodiesMessage world
            else World.enqueuePhysicsMessage2d createBodiesMessage world

        /// Send a physics message to destroy a physics body.
        [<FunctionBinding>]
        static member destroyBody (entity : Entity) physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { SourceSimulant = entity; PhysicsId = physicsId }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d destroyBodyMessage world
            else World.enqueuePhysicsMessage2d destroyBodyMessage world

        /// Send a physics message to destroy several physics bodies.
        [<FunctionBinding>]
        static member destroyBodies (entity : Entity) physicsIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { SourceSimulant = entity; PhysicsIds = physicsIds }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d destroyBodiesMessage world
            else World.enqueuePhysicsMessage2d destroyBodiesMessage world

        /// Send a physics message to create a physics joint.
        [<FunctionBinding>]
        static member createJoint (entity : Entity) entityId jointProperties world =
            let createJointMessage = CreateJointMessage { SourceSimulant = entity; SourceId = entityId; JointProperties = jointProperties }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d createJointMessage world
            else World.enqueuePhysicsMessage2d createJointMessage world

        /// Send a physics message to create physics joints.
        [<FunctionBinding>]
        static member createJoints (entity : Entity) entityId jointsProperties world =
            let createJointsMessage = CreateJointsMessage { SourceSimulant = entity; SourceId = entityId; JointsProperties = jointsProperties }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d createJointsMessage world
            else World.enqueuePhysicsMessage2d createJointsMessage world

        /// Send a physics message to destroy a physics joint.
        [<FunctionBinding>]
        static member destroyJoint (entity : Entity) physicsId world =
            let destroyJointMessage = DestroyJointMessage { SourceSimulant = entity; PhysicsId = physicsId }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d destroyJointMessage world
            else World.enqueuePhysicsMessage2d destroyJointMessage world

        /// Send a physics message to destroy physics joints.
        [<FunctionBinding>]
        static member destroyJoints (entity : Entity) physicsIds world =
            let destroyJointsMessage = DestroyJointsMessage { SourceSimulant = entity; PhysicsIds = physicsIds }
            if entity.GetIs3d world
            then World.enqueuePhysicsMessage3d destroyJointsMessage world
            else World.enqueuePhysicsMessage2d destroyJointsMessage world

        /// Send a physics message to set the enabled-ness of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyEnabled enabled physicsId world =
            let setBodyEnabledMessage = SetBodyEnabledMessage { PhysicsId = physicsId; Enabled = enabled }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d setBodyEnabledMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d setBodyEnabledMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to set the position of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyCenter center physicsId world =
            let setBodyCenterMessage = SetBodyCenterMessage { PhysicsId = physicsId; Center = center }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d setBodyCenterMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d setBodyCenterMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to set the rotation of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d setBodyRotationMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d setBodyRotationMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to set the linear velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d setBodyLinearVelocityMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d setBodyLinearVelocityMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to apply linear impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyLinearImpulse linearImpulse offset physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse; Offset = offset }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d applyBodyLinearImpulseMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d applyBodyLinearImpulseMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to set the angular velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyAngularVelocity angularVelocity physicsId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { PhysicsId = physicsId; AngularVelocity = angularVelocity }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d setBodyAngularVelocityMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d setBodyAngularVelocityMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to apply angular impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyAngularImpulse angularImpulse physicsId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { PhysicsId = physicsId; AngularImpulse = angularImpulse }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d applyBodyAngularImpulseMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d applyBodyAngularImpulseMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to apply force to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyForce force offset physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force; Offset = offset }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d applyBodyForceMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d applyBodyForceMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world

        /// Send a physics message to apply torque to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyTorque torque physicsId world =
            let applyBodyTorqueMessage = ApplyBodyTorqueMessage { PhysicsId = physicsId; Torque = torque }
            if world.Subsystems.PhysicsEngine3d.BodyExists physicsId then
                World.enqueuePhysicsMessage3d applyBodyTorqueMessage world
            elif world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                World.enqueuePhysicsMessage2d applyBodyTorqueMessage world
            else Log.debug ("Body for '" + scstring physicsId + "' not found."); world