// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldPhysics =

    type World with

        static member internal getPhysicsEngine world =
            world.Subsystems.PhysicsEngine

        static member internal setPhysicsEngine physicsEngine world =
            World.updateSubsystems (fun subsystems -> { subsystems with PhysicsEngine = physicsEngine }) world

        static member internal updatePhysicsEngine updater world =
            World.setPhysicsEngine (updater (World.getPhysicsEngine world)) world

        /// Enqueue a physics message in the world.
        static member enqueuePhysicsMessage (message : PhysicsMessage) world =
            World.updatePhysicsEngine (fun physicsEngine -> PhysicsEngine.enqueueMessage message physicsEngine) world

        /// Enqueue multiple physics messages to the world.
        static member enqueuePhysicsMessages (messages : PhysicsMessage seq) world =
            Seq.fold (fun world message -> World.enqueuePhysicsMessage message world) world messages

        /// Check that the world contains a body with the given physics id.
        [<FunctionBinding>]
        static member bodyExists physicsId world =
            (World.getPhysicsEngine world).BodyExists physicsId

        /// Get the contact normals of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyContactNormals physicsId world =
            (World.getPhysicsEngine world).GetBodyContactNormals physicsId

        /// Get the linear velocity of the body with the given physics id.
        [<FunctionBinding>]
        static member getBodyLinearVelocity physicsId world =
            (World.getPhysicsEngine world).GetBodyLinearVelocity physicsId

        /// Get the contact normals where the body with the given physics id is touching the ground.
        [<FunctionBinding>]
        static member getBodyToGroundContactNormals physicsId world =
            (World.getPhysicsEngine world).GetBodyToGroundContactNormals physicsId

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactNormalOpt physicsId world =
            (World.getPhysicsEngine world).GetBodyToGroundContactNormalOpt physicsId

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        [<FunctionBinding>]
        static member getBodyToGroundContactTangentOpt physicsId world =
            (World.getPhysicsEngine world).GetBodyToGroundContactTangentOpt physicsId

        /// Check that the body with the given physics id is on the ground.
        [<FunctionBinding>]
        static member isBodyOnGround physicsId world =
            (World.getPhysicsEngine world).IsBodyOnGround physicsId

        /// Send a message to the physics system to create a physics body.
        [<FunctionBinding>]
        static member createBody (entity : Entity) entityId bodyProperties world =
            let createBodyMessage = CreateBodyMessage { SourceSimulant = entity; SourceId = entityId; BodyProperties = bodyProperties }
            World.enqueuePhysicsMessage createBodyMessage world

        /// Send a message to the physics system to create several physics bodies.
        [<FunctionBinding>]
        static member createBodies (entity : Entity) entityId bodiesProperties world =
            let createBodiesMessage = CreateBodiesMessage { SourceSimulant = entity; SourceId = entityId; BodiesProperties = bodiesProperties }
            World.enqueuePhysicsMessage createBodiesMessage world

        /// Send a message to the physics system to destroy a physics body.
        [<FunctionBinding>]
        static member destroyBody physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            World.enqueuePhysicsMessage destroyBodyMessage world

        /// Send a message to the physics system to destroy several physics bodies.
        [<FunctionBinding>]
        static member destroyBodies physicsIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { PhysicsIds = physicsIds }
            World.enqueuePhysicsMessage destroyBodiesMessage world

        /// Send a message to the physics system to create a physics joint.
        [<FunctionBinding>]
        static member createJoint (entity : Entity) entityId jointProperties world =
            let createJointMessage = CreateJointMessage { SourceSimulant = entity; SourceId = entityId; JointProperties = jointProperties }
            World.enqueuePhysicsMessage createJointMessage world

        /// Send a message to the physics system to create physics joints.
        [<FunctionBinding>]
        static member createJoints (entity : Entity) entityId jointsProperties world =
            let createJointsMessage = CreateJointsMessage { SourceSimulant = entity; SourceId = entityId; JointsProperties = jointsProperties }
            World.enqueuePhysicsMessage createJointsMessage world

        /// Send a message to the physics system to destroy a physics joint.
        [<FunctionBinding>]
        static member destroyJoint physicsId world =
            let destroyJointMessage = DestroyJointMessage { PhysicsId = physicsId }
            World.enqueuePhysicsMessage destroyJointMessage world

        /// Send a message to the physics system to destroy physics joints.
        [<FunctionBinding>]
        static member destroyJoints physicsIds world =
            let destroyJointsMessage = DestroyJointsMessage { PhysicsIds = physicsIds }
            World.enqueuePhysicsMessage destroyJointsMessage world

        /// Send a message to the physics system to set the position of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyPosition position physicsId world =
            let setBodyPositionMessage = SetBodyPositionMessage { PhysicsId = physicsId; Position = position }
            World.enqueuePhysicsMessage setBodyPositionMessage world

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            World.enqueuePhysicsMessage setBodyRotationMessage world

        /// Send a message to the physics system to set the angular velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyAngularVelocity angularVelocity physicsId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { PhysicsId = physicsId; AngularVelocity = angularVelocity }
            World.enqueuePhysicsMessage setBodyAngularVelocityMessage world

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        [<FunctionBinding>]
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            World.enqueuePhysicsMessage setBodyLinearVelocityMessage world

        /// Send a message to the physics system to apply angular impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyAngularImpulse angularImpulse physicsId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { PhysicsId = physicsId; AngularImpulse = angularImpulse }
            World.enqueuePhysicsMessage applyBodyAngularImpulseMessage world

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyLinearImpulse linearImpulse physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            World.enqueuePhysicsMessage applyBodyLinearImpulseMessage world

        /// Send a message to the physics system to apply force to a body with the given physics id.
        [<FunctionBinding>]
        static member applyBodyForce force physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force }
            World.enqueuePhysicsMessage applyBodyForceMessage world

        /// Localize a body shape to a specific physics object.
        [<FunctionBinding>]
        static member localizeBodyShape (extent : Vector2) (bodyShape : BodyShape) (world : World) =
            ignore world // for world parameter for scripting
            PhysicsEngine.localizeBodyShape extent bodyShape