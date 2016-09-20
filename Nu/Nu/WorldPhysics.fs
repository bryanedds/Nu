// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module WorldPhysicsModule =

    /// The subsystem for the world's physics system.
    type [<ReferenceEquality>] PhysicsEngineSubsystem =
        private
            { SubsystemOrder : single
              PhysicsEngine : IPhysicsEngine }
            
        static member private handleBodyTransformMessage (message : BodyTransformMessage) (entity : Entity) world =
            let transform = entity.GetTransform world
            let transform2 =
                { transform with
                    // TODO: see if the following center-offsetting can be encapsulated within the Physics module!
                    Position = message.Position - transform.Size * 0.5f
                    Rotation = message.Rotation }
            if transform <> transform2
            then entity.SetTransform transform2 world
            else world
    
        static member private handleIntegrationMessage world integrationMessage =
            match World.getLiveness world with
            | Running ->
                match integrationMessage with
                | BodyTransformMessage bodyTransformMessage ->
                    let entity = bodyTransformMessage.SourceParticipant :?> Entity
                    if World.containsEntity entity world
                    then PhysicsEngineSubsystem.handleBodyTransformMessage bodyTransformMessage entity world
                    else world
                | BodyCollisionMessage bodyCollisionMessage ->
                    let source = bodyCollisionMessage.SourceParticipant :?> Entity
                    if World.containsEntity source world then
                        let collisionAddress = Events.Collision ->>- source.EntityAddress
                        let collisionData =
                            { Normal = bodyCollisionMessage.Normal
                              Speed = bodyCollisionMessage.Speed
                              Collidee = bodyCollisionMessage.SourceParticipant2 :?> Entity }
                        let eventTrace = EventTrace.record "World" "handleIntegrationMessage" EventTrace.empty
                        World.publish collisionData collisionAddress eventTrace Simulants.Game world
                    else world
            | Exiting -> world
    
        member this.BodyExists physicsId = this.PhysicsEngine.BodyExists physicsId
        member this.GetBodyContactNormals physicsId = this.PhysicsEngine.GetBodyContactNormals physicsId
        member this.GetBodyLinearVelocity physicsId = this.PhysicsEngine.GetBodyLinearVelocity physicsId
        member this.GetBodyToGroundContactNormals physicsId = this.PhysicsEngine.GetBodyToGroundContactNormals physicsId
        member this.GetOptBodyToGroundContactNormal physicsId = this.PhysicsEngine.GetOptBodyToGroundContactNormal physicsId
        member this.GetOptBodyToGroundContactTangent physicsId = this.PhysicsEngine.GetOptBodyToGroundContactTangent physicsId
        member this.IsBodyOnGround physicsId = this.PhysicsEngine.IsBodyOnGround physicsId
    
        interface World Subsystem with
            member this.SubsystemType = UpdateType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with PhysicsEngine = this.PhysicsEngine.ClearMessages () } :> World Subsystem
            member this.EnqueueMessage message = { this with PhysicsEngine = this.PhysicsEngine.EnqueueMessage (message :?> PhysicsMessage) } :> World Subsystem
                
            member this.ProcessMessages world =
                let tickRate = World.getTickRate world
                let (integrationMessages, physicsEngine) = this.PhysicsEngine.Integrate tickRate
                (integrationMessages :> obj, { this with PhysicsEngine = physicsEngine } :> World Subsystem, world)
    
            member this.ApplyResult (integrationMessages, world) =
                let integrationMessages = integrationMessages :?> IntegrationMessage list
                List.fold PhysicsEngineSubsystem.handleIntegrationMessage world integrationMessages
    
            member this.CleanUp world = (this :> World Subsystem, world)
    
        static member make subsystemOrder physicsEngine =
            { SubsystemOrder = subsystemOrder
              PhysicsEngine = physicsEngine }

    type World with

        /// Add a physics message to the world.
        static member addPhysicsMessage (message : PhysicsMessage) world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) Constants.Engine.PhysicsEngineSubsystemName world

        /// Check that the world contains a body with the given physics id?
        static member bodyExists physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.BodyExists physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Get the contact normals of the body with the given physics id.
        static member getBodyContactNormals physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.GetBodyContactNormals physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Get the linear velocity of the body with the given physics id.
        static member getBodyLinearVelocity physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.GetBodyLinearVelocity physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Get the contact normals where the body with the given physics id is touching the ground.
        static member getBodyToGroundContactNormals physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.GetBodyToGroundContactNormals physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
        static member getOptBodyToGroundContactNormal physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.GetOptBodyToGroundContactNormal physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
        static member getOptBodyToGroundContactTangent physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.GetOptBodyToGroundContactTangent physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Check that the body with the given physics id is on the ground.
        static member isBodyOnGround physicsId world =
            World.getSubsystemBy (fun (physicsEngine : PhysicsEngineSubsystem) -> physicsEngine.IsBodyOnGround physicsId) Constants.Engine.PhysicsEngineSubsystemName world

        /// Send a message to the physics system to create a physics body.
        static member createBody (entity : Entity) entityId bodyProperties world =
            let createBodyMessage = CreateBodyMessage { SourceParticipant = entity; SourceId = entityId; BodyProperties = bodyProperties }
            World.addPhysicsMessage createBodyMessage world

        /// Send a message to the physics system to create several physics bodies.
        static member createBodies (entity : Entity) entityId bodyPropertyList world =
            let createBodiesMessage = CreateBodiesMessage { SourceParticipant = entity; SourceId = entityId; BodyPropertyList = bodyPropertyList }
            World.addPhysicsMessage createBodiesMessage world

        /// Send a message to the physics system to destroy a physics body.
        static member destroyBody physicsId world =
            let destroyBodyMessage = DestroyBodyMessage { PhysicsId = physicsId }
            World.addPhysicsMessage destroyBodyMessage world

        /// Send a message to the physics system to destroy several physics bodies.
        static member destroyBodies physicsIds world =
            let destroyBodiesMessage = DestroyBodiesMessage { PhysicsIds = physicsIds }
            World.addPhysicsMessage destroyBodiesMessage world

        /// Send a message to the physics system to set the position of a body with the given physics id.
        static member setBodyPosition position physicsId world =
            let setBodyPositionMessage = SetBodyPositionMessage { PhysicsId = physicsId; Position = position }
            World.addPhysicsMessage setBodyPositionMessage world

        /// Send a message to the physics system to set the rotation of a body with the given physics id.
        static member setBodyRotation rotation physicsId world =
            let setBodyRotationMessage = SetBodyRotationMessage { PhysicsId = physicsId; Rotation = rotation }
            World.addPhysicsMessage setBodyRotationMessage world

        /// Send a message to the physics system to set the angular velocity of a body with the given physics id.
        static member setBodyAngularVelocity angularVelocity physicsId world =
            let setBodyAngularVelocityMessage = SetBodyAngularVelocityMessage { PhysicsId = physicsId; AngularVelocity = angularVelocity }
            World.addPhysicsMessage setBodyAngularVelocityMessage world

        /// Send a message to the physics system to apply angular impulse to a body with the given physics id.
        static member applyBodyAngularImpulse angularImpulse physicsId world =
            let applyBodyAngularImpulseMessage = ApplyBodyAngularImpulseMessage { PhysicsId = physicsId; AngularImpulse = angularImpulse }
            World.addPhysicsMessage applyBodyAngularImpulseMessage world

        /// Send a message to the physics system to set the linear velocity of a body with the given physics id.
        static member setBodyLinearVelocity linearVelocity physicsId world =
            let setBodyLinearVelocityMessage = SetBodyLinearVelocityMessage { PhysicsId = physicsId; LinearVelocity = linearVelocity }
            World.addPhysicsMessage setBodyLinearVelocityMessage world

        /// Send a message to the physics system to apply linear impulse to a body with the given physics id.
        static member applyBodyLinearImpulse linearImpulse physicsId world =
            let applyBodyLinearImpulseMessage = ApplyBodyLinearImpulseMessage { PhysicsId = physicsId; LinearImpulse = linearImpulse }
            World.addPhysicsMessage applyBodyLinearImpulseMessage world

        /// Send a message to the physics system to apply force to a body with the given physics id.
        static member applyBodyForce force physicsId world =
            let applyBodyForceMessage = ApplyBodyForceMessage { PhysicsId = physicsId; Force = force }
            World.addPhysicsMessage applyBodyForceMessage world

/// The subsystem for the world's physics system.
type PhysicsEngineSubsystem = WorldPhysicsModule.PhysicsEngineSubsystem
