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

        /// Localize a body shape to a specific physics object.
        [<FunctionBinding>]
        static member localizeBodyShape (extent : Vector3) (bodyShape : BodyShape) (world : World) =
            ignore world // for world parameter for scripting
            Physics.localizeBodyShape extent bodyShape

        /// Enqueue a 2d physics message in the world.
        static member enqueuePhysicsMessage2d (message : PhysicsMessage) world =
            World.updatePhysicsEngine2d (fun physicsEngine -> physicsEngine.EnqueueMessage message) world

        /// Enqueue multiple 2d physics messages to the world.
        static member enqueuePhysicsMessages2d (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.enqueuePhysicsMessage2d message world) world messages

        /// Check that the world contains a body with the given physics id.
        [<FunctionBinding>]
        static member bodyExists physicsId world =
            world.Subsystems.PhysicsEngine2d.BodyExists physicsId || false

        /// Get the contact normals of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyContactNormals physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyContactNormals physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                []

        /// Get the linear velocity of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyLinearVelocity physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyLinearVelocity physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                v3Zero

        /// Get the contact normals where the body with the given physics id is touching the ground.
        [<FunctionBinding>]
        static member getBodyToGroundContactNormals physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormals physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                []

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactNormalOpt physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactNormalOpt physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                None

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactTangentOpt physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.GetBodyToGroundContactTangentOpt physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                None

        /// Check that the body with the given physics id is on the ground.
        [<FunctionBinding>]
        static member isBodyOnGround physicsId world =
            if  world.Subsystems.PhysicsEngine2d.BodyExists physicsId then
                world.Subsystems.PhysicsEngine2d.IsBodyOnGround physicsId
            else
                Log.debug ("2d body for '" + scstring physicsId + "' not found and 3d physics not yet implemented.")
                false

        /// Send a message to the physics system to create a physics body.
        [<FunctionBinding>]
        static member createBody (entity : Entity) entityId (bodyProperties : BodyProperties) world =
            if entity.GetIs2d world then
                let physicsId = { SourceId = entityId; CorrelationId = bodyProperties.BodyId }
                let eventTrace = EventTrace.debug "World" "createBody" "" EventTrace.empty
                let world = World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world
                let createBodyMessage = CreateBodyMessage { SourceSimulant = entity; SourceId = entityId; BodyProperties = bodyProperties }
                World.enqueuePhysicsMessage2d createBodyMessage world
            else
                failwithnie ()

        /// Send a message to the physics system to create several physics bodies.
        [<FunctionBinding>]
        static member createBodies (entity : Entity) entityId bodiesProperties world =
            if entity.GetIs2d world then
                let eventTrace = EventTrace.debug "World" "createBodies" "" EventTrace.empty
                let world =
                    List.fold (fun world (bodyProperties : BodyProperties) ->
                        let physicsId = { SourceId = entityId; CorrelationId = bodyProperties.BodyId }
                        World.publish physicsId Events.BodyAdding eventTrace Simulants.Game world)
                        world bodiesProperties
                let createBodiesMessage = CreateBodiesMessage { SourceSimulant = entity; SourceId = entityId; BodiesProperties = bodiesProperties }
                World.enqueuePhysicsMessage2d createBodiesMessage world
            else
                failwithnie ()

        /// Send a message to the physics system to destroy a physics body.
        [<FunctionBinding>]
        static member destroyBody physicsId world =
            let eventTrace = EventTrace.debug "World" "destroyBody" "" EventTrace.empty
            let world = World.publish physicsId Events.BodyRemoving eventTrace Simulants.Game world
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            World.enqueuePhysicsMessage2d destroyBodyMessage world

        /// Send a message to the physics system to destroy several physics bodies. Note that all bodies must be either
        /// all 2d or all 3d. A mixture may result in some bodies not being destroyed.
        [<FunctionBinding>]
        static member destroyBodies physicsIds world =
            let eventTrace = EventTrace.debug "World" "destroyBodies" "" EventTrace.empty
            let world =
                List.fold (fun world physicsId ->
                    World.publish physicsId Events.BodyRemoving eventTrace Simulants.Game world)
                    world physicsIds
            let destroyBodiesMessage = DestroyBodiesMessage { PhysicsIds = physicsIds }
            World.enqueuePhysicsMessage2d destroyBodiesMessage world

        /// Send a message to the physics system to create a physics joint.
        [<FunctionBinding>]
        static member createJoint (entity : Entity) entityId jointProperties world =
            if entity.GetIs2d world then
                let createJointMessage = CreateJointMessage { SourceSimulant = entity; SourceId = entityId; JointProperties = jointProperties }
                World.enqueuePhysicsMessage2d createJointMessage world
            else
                failwithnie ()

        /// Send a message to the physics system to create physics joints.
        [<FunctionBinding>]
        static member createJoints (entity : Entity) entityId jointsProperties world =
            if entity.GetIs2d world then
                let createJointsMessage = CreateJointsMessage { SourceSimulant = entity; SourceId = entityId; JointsProperties = jointsProperties }
                World.enqueuePhysicsMessage2d createJointsMessage world
            else
                failwithnie ()

        /// Send a message to the physics system to destroy a physics joint.
        [<FunctionBinding>]
        static member destroyJoint physicsId world =
            let destroyJointMessage = DestroyJointMessage { PhysicsId = physicsId }
            World.enqueuePhysicsMessage2d destroyJointMessage world

        /// Send a message to the physics system to destroy physics joints.
        [<FunctionBinding>]
        static member destroyJoints physicsIds world =
            let destroyJointsMessage = DestroyJointsMessage { PhysicsIds = physicsIds }
            World.enqueuePhysicsMessage2d destroyJointsMessage world

        /// Send a message to the physics system to set the enabled-ness of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyEnabled enabled physicsId world =
            let setBodyEnabledMessage = SetBodyEnabledMessage { PhysicsId = physicsId; Enabled = enabled }
            World.enqueuePhysicsMessage2d setBodyEnabledMessage world

        /// Send a message to the physics system to set the position of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyPosition position physicsId world =
            let setBodyPositionMessage = SetBodyPositionMessage { PhysicsId = physicsId; Position = position }
            World.enqueuePhysicsMessage2d setBodyPositionMessage world

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            World.enqueuePhysicsMessage2d setBodyRotationMessage world

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            World.enqueuePhysicsMessage2d setBodyLinearVelocityMessage world

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyLinearImpulse linearImpulse physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            World.enqueuePhysicsMessage2d applyBodyLinearImpulseMessage world

        /// Send a message to the physics system to set the angular velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyAngularVelocity angularVelocity physicsId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { PhysicsId = physicsId; AngularVelocity = angularVelocity }
            World.enqueuePhysicsMessage2d setBodyAngularVelocityMessage world

        /// Send a message to the physics system to apply angular impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyAngularImpulse angularImpulse physicsId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { PhysicsId = physicsId; AngularImpulse = angularImpulse }
            World.enqueuePhysicsMessage2d applyBodyAngularImpulseMessage world

        /// Send a message to the physics system to apply force to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyForce force physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force }
            World.enqueuePhysicsMessage2d applyBodyForceMessage world