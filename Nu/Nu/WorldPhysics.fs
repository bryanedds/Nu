// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldPhysicsModule =

    // HACK: exposes BodyShape property for earlier access.
    type Entity with

        member this.GetBodyShape world : BodyShape = this.Get Property? BodyShape world
        member this.SetBodyShape (value : BodyShape) world = this.SetFast Property? BodyShape false false value world
        member this.BodyShape = lens Property? BodyShape this.GetBodyShape this.SetBodyShape this

    /// The subsystem for the world's physics system.
    type [<ReferenceEquality>] PhysicsEngineSubsystem =
        private
            { PhysicsEngine : PhysicsEngine }

        static member private handleIntegrationMessage world integrationMessage =
            match World.getLiveness world with
            | Running ->
                match integrationMessage with
                | BodyTransformMessage bodyTransformMessage ->
                    let bodySource = bodyTransformMessage.BodySource
                    let entity = bodySource.SourceSimulant :?> Entity
                    let bodyShape = entity.GetBodyShape world
                    let bodyCenter = BodyShape.getCenter bodyShape
                    let bodyOffset = bodyCenter * entity.GetSize world
                    let transform = entity.GetTransform world
                    let position = (bodyTransformMessage.Position - bodyOffset) - transform.Size * 0.5f
                    let rotation = bodyTransformMessage.Rotation
                    let world =
                        if bodyTransformMessage.BodySource.SourceBodyId = Guid.Empty then
                            let transform2 = { transform with Position = position; Rotation = rotation }
                            if transform <> transform2
                            then entity.SetTransform transform2 world
                            else world
                        else world
                    let transformAddress = Events.Transform --> entity.EntityAddress
                    let transformData = { BodySource = bodySource; Position = position; Rotation = rotation }
                    let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                    World.publish transformData transformAddress eventTrace Default.Game world
                | BodyCollisionMessage bodyCollisionMessage ->
                    let entity = bodyCollisionMessage.BodySource.SourceSimulant :?> Entity
                    if entity.GetExists world then
                        let collisionAddress = Events.Collision --> entity.EntityAddress
                        let collisionData =
                            { Collider = bodyCollisionMessage.BodySource
                              Collidee = bodyCollisionMessage.BodySource2
                              Normal = bodyCollisionMessage.Normal
                              Speed = bodyCollisionMessage.Speed }
                        let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                        World.publish collisionData collisionAddress eventTrace Default.Game world
                    else world
            | Exiting -> world

        member this.BodyExists physicsId = this.PhysicsEngine.BodyExists physicsId
        member this.GetBodyContactNormals physicsId = this.PhysicsEngine.GetBodyContactNormals physicsId
        member this.GetBodyLinearVelocity physicsId = this.PhysicsEngine.GetBodyLinearVelocity physicsId
        member this.GetBodyToGroundContactNormals physicsId = this.PhysicsEngine.GetBodyToGroundContactNormals physicsId
        member this.GetBodyToGroundContactNormalOpt physicsId = this.PhysicsEngine.GetBodyToGroundContactNormalOpt physicsId
        member this.GetBodyToGroundContactTangentOpt physicsId = this.PhysicsEngine.GetBodyToGroundContactTangentOpt physicsId
        member this.IsBodyOnGround physicsId = this.PhysicsEngine.IsBodyOnGround physicsId

        interface World Subsystem with

            member this.PopMessages () =
                let (messages, physicsEngine) = this.PhysicsEngine.PopMessages ()
                (messages :> obj, { this with PhysicsEngine = physicsEngine } :> World Subsystem)

            member this.ClearMessages () =
                { this with PhysicsEngine = this.PhysicsEngine.ClearMessages () } :> World Subsystem

            member this.EnqueueMessage message =
                { this with PhysicsEngine = this.PhysicsEngine.EnqueueMessage (message :?> PhysicsMessage) } :> World Subsystem

            member this.ProcessMessages messages world =
                let messages = messages :?> PhysicsMessage UList
                let tickRate = World.getTickRate world
                PhysicsEngine.integrate tickRate messages this.PhysicsEngine :> obj

            member this.ApplyResult (integrationMessages, world) =
                let integrationMessages = integrationMessages :?> IntegrationMessage List
                Seq.fold PhysicsEngineSubsystem.handleIntegrationMessage world integrationMessages

            member this.CleanUp world =
                (this :> World Subsystem, world)

        static member make physicsEngine =
            { PhysicsEngine = physicsEngine }

    type World with

        static member internal getPhysicsEngine world =
            world.Subsystems.PhysicsEngine :?> PhysicsEngineSubsystem

        static member internal setPhysicsEngine physicsEngine world =
            World.updateSubsystems (fun subsystems -> { subsystems with PhysicsEngine = physicsEngine }) world

        static member internal updatePhysicsEngine updater world =
            World.setPhysicsEngine (updater (World.getPhysicsEngine world :> World Subsystem)) world

        /// Enqueue a physics message in the world.
        static member enqueuePhysicsMessage (message : PhysicsMessage) world =
            World.updatePhysicsEngine (fun physicsEngine -> Subsystem.enqueueMessage message physicsEngine) world

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
        static member localizeBodyShape (extent : Vector2) (bodyShape : BodyShape) =
            PhysicsEngine.localizeBodyShape extent bodyShape

/// The subsystem for the world's physics system.
type PhysicsEngineSubsystem = WorldPhysicsModule.PhysicsEngineSubsystem