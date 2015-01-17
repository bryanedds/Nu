// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2014.

namespace Nu
open System
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldPhysicsModule =

    /// The subsystem for the world's integrator.
    type [<ReferenceEquality>] IntegratorSubsystem =
        { SubsystemType : SubsystemType
          SubsystemOrder : single
          Integrator : IIntegrator }
        
        static member private handleBodyTransformMessage (message : BodyTransformMessage) (entityRep : EntityRep) world =
            // OPTIMIZATION: entity is not changed (avoiding a change entity event) if position and rotation haven't changed.
            if entityRep.GetPosition world <> message.Position || entityRep.GetRotation world <> message.Rotation then
                world |>
                    // TODO: see if the following center-offsetting can be encapsulated within the Physics module!
                    entityRep.SetPosition (message.Position - entityRep.GetSize world * 0.5f) |>
                    entityRep.SetRotation message.Rotation
            else world

        static member private handleIntegrationMessage world integrationMessage =
            match world.State.Liveness with
            | Running ->
                match integrationMessage with
                | BodyTransformMessage bodyTransformMessage ->
                    let entityAddress = atoea bodyTransformMessage.SourceAddress
                    if World.containsEntity entityAddress world then
                        let entityRep = { EntityAddress = entityAddress }
                        IntegratorSubsystem.handleBodyTransformMessage bodyTransformMessage entityRep world
                    else world
                | BodyCollisionMessage bodyCollisionMessage ->
                    match World.getOptEntity (atoea bodyCollisionMessage.SourceAddress) world with
                    | Some _ ->
                        let collisionAddress = CollisionEventAddress ->- bodyCollisionMessage.SourceAddress
                        let collisionData =
                            { Normal = bodyCollisionMessage.Normal
                              Speed = bodyCollisionMessage.Speed
                              Collidee = { EntityAddress = atoea bodyCollisionMessage.CollideeAddress }}
                        World.publish4 collisionData collisionAddress GameRep world
                    | None -> world
            | Exiting -> world

        member this.BodyExists physicsId = this.Integrator.BodyExists physicsId
        member this.GetBodyContactNormals physicsId = this.Integrator.GetBodyContactNormals physicsId
        member this.GetBodyLinearVelocity physicsId = this.Integrator.GetBodyLinearVelocity physicsId
        member this.GetBodyGroundContactNormals physicsId = this.Integrator.GetBodyGroundContactNormals physicsId
        member this.GetBodyOptGroundContactNormal physicsId = this.Integrator.GetBodyOptGroundContactNormal physicsId
        member this.GetBodyOptGroundContactTangent physicsId = this.Integrator.GetBodyOptGroundContactTangent physicsId
        member this.BodyOnGround physicsId = this.Integrator.BodyOnGround physicsId

        interface Subsystem with
            member this.SubsystemType = this.SubsystemType
            member this.SubsystemOrder = this.SubsystemOrder
            member this.ClearMessages () = { this with Integrator = this.Integrator.ClearMessages () } :> Subsystem
            member this.EnqueueMessage message = { this with Integrator = this.Integrator.EnqueueMessage (message :?> PhysicsMessage) } :> Subsystem
            
            member this.ProcessMessages _ =
                let (integrationMessages, integrator) = this.Integrator.Integrate ()
                (integrationMessages :> obj, { this with Integrator = integrator } :> Subsystem)

            member this.ApplyResult integrationMessages world =
                let integrationMessages = integrationMessages :?> IntegrationMessage list
                List.fold IntegratorSubsystem.handleIntegrationMessage world integrationMessages

            member this.CleanUp world = (this :> Subsystem, world)

    type World with

        /// Does the world contain the body with the given physics id?
        static member bodyExists physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.BodyExists physicsId) IntegratorSubsystemName world

        /// Get the contact normals of the body with the given physics id.
        static member getBodyContactNormals physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.GetBodyContactNormals physicsId) IntegratorSubsystemName world

        /// Get the linear velocity of the body with the given physics id.
        static member getBodyLinearVelocity physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.GetBodyLinearVelocity physicsId) IntegratorSubsystemName world

        /// Get the contact normals where the body with the given physics id is touching the ground.
        static member getBodyGroundContactNormals physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.GetBodyGroundContactNormals physicsId) IntegratorSubsystemName world

        /// Try to get a contact normal where the body with the given physics id is touching the ground.
        static member getBodyOptGroundContactNormal physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.GetBodyOptGroundContactNormal physicsId) IntegratorSubsystemName world

        /// Try to get a contact tangent where the body with the given physics id is touching the ground.
        static member getBodyOptGroundContactTangent physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.GetBodyOptGroundContactTangent physicsId) IntegratorSubsystemName world

        /// Query that the body with the give physics id is on the ground.
        static member bodyOnGround physicsId world =
            World.getSubsystemBy (fun (integrator : IntegratorSubsystem) -> integrator.BodyOnGround physicsId) IntegratorSubsystemName world

        /// Send a message to the physics system to create a physics body.
        static member createBody (entityAddress : Entity Address) entityId bodyProperties world =
            let createBodyMessage = CreateBodyMessage { SourceAddress = atooa entityAddress; SourceId = entityId; BodyProperties = bodyProperties }
            World.addPhysicsMessage createBodyMessage world

        /// Send a message to the physics system to create several physics bodies.
        static member createBodies (entityAddress : Entity Address) entityId bodyPropertyList world =
            let createBodiesMessage = CreateBodiesMessage { SourceAddress = atooa entityAddress; SourceId = entityId; BodyPropertyList = bodyPropertyList }
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