// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Concurrent
open System.Numerics
open Prime

/// A message to the physics system to create a body.
type CreateBodyMessage =
    { BodyId : BodyId
      BodyProperties : BodyProperties }

/// A message to the physics system to create multiple bodies.
type CreateBodiesMessage =
    { BodySource : Simulant
      BodiesProperties : BodyProperties list }

/// A message to the physics system to destroy a body.
type DestroyBodyMessage =
    { BodyId : BodyId }

/// A message to the physics system to destroy multiple bodies.
type DestroyBodiesMessage =
    { BodyIds : BodyId list }

/// A message to the physics system to create a joint.
type CreateBodyJointMessage =
    { BodyJointSource : Simulant
      BodyJointProperties : BodyJointProperties }

/// A message to the physics system to destroy a joint.
type DestroyBodyJointMessage =
    { BodyJointId : BodyJointId
      BodyJointTarget : BodyId
      BodyJointTarget2Opt : BodyId option }

/// A message to the physics system describing fluid emitter creation.
type CreateFluidEmitterMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticles : FluidParticle SArray
      FluidEmitterDescriptor : FluidEmitterDescriptor }

/// A message to the physics system describing fluid emitter destruction.
type DestroyFluidEmitterMessage =
    { FluidEmitterId : FluidEmitterId }

/// A message to the physics system to destroy a body.
type SetBodyEnabledMessage =
    { BodyId : BodyId
      Enabled : bool }

/// A message to the physics system to set body center.
type SetBodyCenterMessage =
    { BodyId : BodyId
      Center : Vector3 }

/// A message to the physics system to set the rotation of a body.
type SetBodyRotationMessage =
    { BodyId : BodyId
      Rotation : Quaternion }

/// A message to the physics system to set the linear velocity of a body.
type SetBodyLinearVelocityMessage =
    { BodyId : BodyId
      LinearVelocity : Vector3 }

/// A message to the physics system to set the angular velocity of a body.
type SetBodyAngularVelocityMessage =
    { BodyId : BodyId
      AngularVelocity : Vector3 }

/// A message to the physics system to set the forward input of a vehicle body.
type SetBodyVehicleForwardInputMessage =
    { BodyId : BodyId
      ForwardInput : single }

/// A message to the physics system to set the right input of a vehicle body.
type SetBodyVehicleRightInputMessage =
    { BodyId : BodyId
      RightInput : single }

/// A message to the physics system to set the brake input of a vehicle body.
type SetBodyVehicleBrakeInputMessage =
    { BodyId : BodyId
      BrakeInput : single }

/// A message to the physics system to set the brake input of a vehicle body.
type SetBodyVehicleHandBrakeInputMessage =
    { BodyId : BodyId
      HandBrakeInput : single }

/// A message to the physics system to set motor enabled state of a body joint.
type SetBodyJointMotorEnabledMessage =
    { BodyJointId : BodyJointId
      MotorEnabled : bool }

/// A message to the physics system to set motor speed of a body joint.
type SetBodyJointMotorSpeedMessage =
    { BodyJointId : BodyJointId
      MotorSpeed : single }

/// A message to the physics system to set target angle of a body joint.
type SetBodyJointTargetAngleMessage =
    { BodyJointId : BodyJointId
      TargetAngle : single }

/// A message to the physics system to apply a linear impulse to a body.
type ApplyBodyLinearImpulseMessage =
    { BodyId : BodyId
      LinearImpulse : Vector3
      OriginWorldOpt : Vector3 option }

/// A message to the physics system to apply an angular impulse to a body.
type ApplyBodyAngularImpulseMessage =
    { BodyId : BodyId
      AngularImpulse : Vector3 }

/// A message to the physics system to apply a force to a body.
type ApplyBodyForceMessage =
    { BodyId : BodyId
      Force : Vector3
      OriginWorldOpt : Vector3 option }

/// A message to the physics system to apply torque to a body.
type ApplyBodyTorqueMessage =
    { BodyId : BodyId
      Torque : Vector3 }

/// A message to the physics system to apply a jump motion to a body (KinematicCharacter only).
type JumpBodyMessage =
    { BodyId : BodyId
      CanJumpInAir : bool
      JumpSpeed : single }

/// A message from the physics system describing body pentration that took place.
type BodyPenetrationMessage =
    { BodyShapeSource : BodyShapeIndex
      BodyShapeSource2 : BodyShapeIndex
      Normal : Vector3 }

/// A message from the physics system describing body separation that took place.
type BodySeparationMessage =
    { BodyShapeSource : BodyShapeIndex
      BodyShapeSource2 : BodyShapeIndex }

/// A message from the physics system describing the updated transform of a body.
type BodyTransformMessage =
    { BodyId : BodyId
      Center : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      AngularVelocity : Vector3 }

/// A message from the physics system describing a body joint break.
type BodyJointBreakMessage =
    { BodyJointId : BodyJointId
      BreakingPoint : single
      BreakingOverflow : single }
      
/// A message from the physics system describing a fluid update.
type FluidEmitterMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticles : FluidParticle SArray
      FluidCollisions : FluidCollision ConcurrentBag }

/// A message to the physics system to update the description of a fluid emitter.
type UpdateFluidEmitterMessage =
    { FluidEmitterId : FluidEmitterId
      FluidEmitterDescriptor : FluidEmitterDescriptor }

/// A message to the physics system describing the assignment of fluid particles.
type SetFluidParticlesMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticles : FluidParticle SArray }
      
/// A message to the physics system describing fluid particle emission.
type EmitFluidParticlesMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticles : FluidParticle SArray }
      
/// A message to the physics system describing fluid particle mapping.
type MapFluidParticlesMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticleMapper : FluidParticle -> FluidParticle }

/// A message to the physics system describing fluid particle filter.
type FilterFluidParticlesMessage =
    { FluidEmitterId : FluidEmitterId
      FluidParticlePredicate : FluidParticle -> bool }

/// A message from the physics system.
type IntegrationMessage =
    | BodyPenetrationMessage of BodyPenetrationMessage
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage
    | BodyJointBreakMessage of BodyJointBreakMessage
    | FluidEmitterMessage of FluidEmitterMessage

/// A message to the physics system.
type PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateBodyJointMessage of CreateBodyJointMessage
    | DestroyBodyJointMessage of DestroyBodyJointMessage
    | CreateFluidEmitterMessage of CreateFluidEmitterMessage
    | DestroyFluidEmitterMessage of DestroyFluidEmitterMessage
    | SetBodyEnabledMessage of SetBodyEnabledMessage
    | SetBodyCenterMessage of SetBodyCenterMessage
    | SetBodyRotationMessage of SetBodyRotationMessage
    | SetBodyLinearVelocityMessage of SetBodyLinearVelocityMessage
    | SetBodyAngularVelocityMessage of SetBodyAngularVelocityMessage
    | SetBodyVehicleForwardInputMessage of SetBodyVehicleForwardInputMessage
    | SetBodyVehicleRightInputMessage of SetBodyVehicleRightInputMessage
    | SetBodyVehicleBrakeInputMessage of SetBodyVehicleBrakeInputMessage
    | SetBodyVehicleHandBrakeInputMessage of SetBodyVehicleHandBrakeInputMessage
    | SetBodyJointMotorEnabledMessage of SetBodyJointMotorEnabledMessage
    | SetBodyJointMotorSpeedMessage of SetBodyJointMotorSpeedMessage
    | SetBodyJointTargetAngleMessage of SetBodyJointTargetAngleMessage
    | JumpBodyMessage of JumpBodyMessage
    | UpdateFluidEmitterMessage of UpdateFluidEmitterMessage
    | SetFluidParticlesMessage of SetFluidParticlesMessage
    | MapFluidParticlesMessage of MapFluidParticlesMessage
    | FilterFluidParticlesMessage of FilterFluidParticlesMessage
    | ClearFluidParticlesMessage of FluidEmitterId
    | EmitFluidParticlesMessage of EmitFluidParticlesMessage
    | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
    | ApplyBodyAngularImpulseMessage of ApplyBodyAngularImpulseMessage
    | ApplyBodyForceMessage of ApplyBodyForceMessage
    | ApplyBodyTorqueMessage of ApplyBodyTorqueMessage
    | SetGravityMessage of Vector3

/// Marker interface for a physics-engine-specific rendering context.
type PhysicsEngineRenderContext = interface end

/// Represents a physics engine in Nu.
/// TODO: investigate if we'll ever have to handle enough physics or integration messages to necessitate the use of
/// SList instead of List.
type PhysicsEngine =

    /// Get the global gravity used in the physics engine.
    abstract Gravity : Vector3

    /// Get the default global gravity used for the physics engine.
    abstract GravityDefault : Vector3
    
    /// Check that the physics engine contain the body with the given body id.
    abstract GetBodyExists : bodyId : BodyId -> bool
    
    /// Get the contact normals of the body with the given body id.
    abstract GetBodyContactNormals : bodyId : BodyId -> Vector3 array
    
    /// Get the linear velocity of the body with the given body id.
    abstract GetBodyLinearVelocity : bodyId : BodyId -> Vector3
    
    /// Get the angular velocity of the body with the given body id.
    abstract GetBodyAngularVelocity : bodyId : BodyId -> Vector3
    
    /// Get the contact normals where the body with the given body id is touching the ground.
    abstract GetBodyToGroundContactNormals : bodyId : BodyId -> Vector3 array
    
    /// Get a contact normal where the body with the given body id is touching the ground (if one exists).
    abstract GetBodyToGroundContactNormalOpt : bodyId : BodyId -> Vector3 option
    
    /// Get a contact tangent where the body with the given body id is touching the ground (if one exists).
    abstract GetBodyToGroundContactTangentOpt : bodyId : BodyId -> Vector3 option
    
    /// Check that the body with the given body id is on the ground.
    abstract GetBodyGrounded : bodyId : BodyId -> bool
    
    /// Check that the body with the given body id is a sensor.
    abstract GetBodySensor : bodyId : BodyId -> bool
    
    /// Get the wheel speed framed in terms of the clutch (0.0f if not a wheeled vehicle).
    abstract GetBodyWheelSpeedAtClutch : bodyId : BodyId -> single
    
    /// Get the given wheel's model matrix (identity if not a wheeled vehicle or invalid wheel).
    abstract GetBodyWheelModelMatrix : wheelModelRight : Vector3 * wheelModelUp : Vector3 * wheelIndex : int * bodyId : BodyId -> Matrix4x4
    
    /// Get the given wheel's angular velocity (0.0f if not a wheeled vehicle or invalid wheel).
    abstract GetBodyWheelAngularVelocity : wheelIndex : int * bodyId : BodyId -> single
    
    /// Check that the physics engine contain the body with the given body id.
    abstract GetBodyJointExists : bodyJointId : BodyJointId -> bool

    /// Get the motor speed of the body joint with the given body joint id.
    abstract GetBodyJointMotorSpeed : bodyJointId : BodyJointId -> single

    /// Get the target angle of the body joint with the given body joint id.
    abstract GetBodyJointTargetAngle : bodyJointId : BodyJointId -> single
    
    /// Cast a ray into the physics bodies.
    abstract RayCast : ray : Ray3 * collisionMask : int * closestOnly : bool -> BodyIntersection array
    
    /// Cast a shape into the physics bodies.
    abstract ShapeCast : shape : BodyShape * transformOpt : Affine option * ray : Ray3 * collisionMask : int * closestOnly : bool -> BodyIntersection array

    /// Handle a physics message from an external source.
    abstract HandleMessage : message : PhysicsMessage -> unit
    
    /// Attempt to integrate the physics system one step.
    abstract TryIntegrate : delta : GameTime -> IntegrationMessage SArray option

    /// Attempt to render physics with the given physics-engine-specific render context.
    abstract TryRender : renderContext : PhysicsEngineRenderContext -> unit
    
    /// Clear the physics simulation, returning false if no physics objects existed to begin with. For internal use only.
    abstract ClearInternal : unit -> unit
    
    /// Handle physics clean up by freeing all created resources.
    abstract CleanUp : unit -> unit

/// The stub implementation of PhysicsEngine.
type [<ReferenceEquality>] StubPhysicsEngine =
    private { StubPhysicsEngine : unit }
    static member make () = { StubPhysicsEngine = () }
    interface PhysicsEngine with
        member physicsEngine.GravityDefault = v3Zero
        member physicsEngine.Gravity = v3Zero
        member physicsEngine.GetBodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyAngularVelocity _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormalOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactTangentOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyGrounded _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodySensor _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyWheelSpeedAtClutch _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyWheelModelMatrix (_, _, _, _) = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyWheelAngularVelocity (_, _) = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyJointExists _ = failwith "No body joints in StubPhysicsEngine"
        member physicsEngine.GetBodyJointMotorSpeed _ = failwith "No body joints in StubPhysicsEngine"
        member physicsEngine.GetBodyJointTargetAngle _ = failwith "No body joints in StubPhysicsEngine"
        member physicsEngine.RayCast (_, _, _) = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.ShapeCast (_, _, _, _, _) = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.HandleMessage _ = ()
        member physicsEngine.TryIntegrate _ = None
        member physicsEngine.TryRender _ = ()
        member physicsEngine.ClearInternal () = ()
        member physicsEngine.CleanUp () = ()