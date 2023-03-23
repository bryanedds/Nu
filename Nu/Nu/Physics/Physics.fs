// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// Identifies a target whose body can be found in the physics engine.
type [<CustomEquality; NoComparison>] PhysicsId =
    { SourceId : uint64
      CorrelationId : uint64 }

    /// The invalid physics id.
    static member InvalidId =
        { SourceId = 0UL; CorrelationId = 0UL }

    /// Hash a PhysicsId.
    static member hash pid =
        pid.SourceId.GetHashCode () ^^^ pid.CorrelationId.GetHashCode ()

    /// Equate PhysicsIds.
    static member equals pid pid2 =
        pid.SourceId.Equals pid2.SourceId &&
        pid.CorrelationId.Equals pid2.CorrelationId

    /// Make a PhysicsId for an external source.
    static member make sourceId =
        { SourceId = sourceId; CorrelationId = Gen.id64 }

    interface PhysicsId IEquatable with
        member this.Equals that =
            PhysicsId.equals this that

    override this.Equals that =
        match that with
        | :? PhysicsId as that -> PhysicsId.equals this that
        | _ -> false

    override this.GetHashCode () =
        PhysicsId.hash this

/// Store origination information about a simulant physics body.
type BodySourceInternal =
    { Simulant : Simulant
      BodyId : uint64 }

/// Store origination information about a simulant physics shape body.
type BodyShapeSourceInternal =
    { Simulant : Simulant
      BodyId : uint64
      ShapeId : uint64 }

/// Describes body shape-specific properties.
type BodyShapeProperties =
    { BodyShapeId : uint64
      FrictionOpt : single option
      RestitutionOpt : single option
      CollisionCategoriesOpt : int option
      CollisionMaskOpt : int option
      SensorOpt : bool option }

[<RequireQualifiedAccess>]
module BodyShapeProperties =

    let empty =
        { BodyShapeId = 0UL
          FrictionOpt = None
          RestitutionOpt = None
          CollisionCategoriesOpt = None
          CollisionMaskOpt = None
          SensorOpt = None }

/// Describes the substantial nature of a body in terms of mass or density.
[<Syntax
    ("Mass Density", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax);
     StructuralEquality; StructuralComparison; Struct>]
type Substance =
    | Mass of Mass : single
    | Density of Density : single

/// Describes the attributes of continuous collision detection.
type [<Struct>] Continuous =
    { MotionThreshold : single
      SweptSphereRadius : single }

/// Describe the form of collision detection to use.
[<Syntax
    ("DiscontinuousDetection ContinuousDetection", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax);
     StructuralEquality; NoComparison; Struct>]
type CollisionDetection =
    | Discontinuous
    | Continuous of Continuous

/// The shape of a physics body box.
type BodyBox =
    { Center : Vector3
      Size : Vector3
      PropertiesOpt : BodyShapeProperties option }
    static member toBox bodyBox =
        box3 (bodyBox.Center - bodyBox.Size * 0.5f) bodyBox.Size
    static member fromBox (box : Box3) =
        { Center = box.Center; Size = box.Size; PropertiesOpt = None }

/// The shape of a physics body sphere.
type BodySphere =
    { Center : Vector3
      Radius : single
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyCapsule =
    { Center : Vector3
      Height : single
      Radius : single
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyBoxRounded =
    { Center : Vector3
      Size : Vector3
      Radius : single
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body convex hull.
type BodyConvexHull =
    { Center : Vector3
      Vertices : Vector3 array
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body.
[<Syntax
    ("BodyEmpty BodyBox BodySphere BodyCapsule BodyConvexHull BodyShapes", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type BodyShape =
    | BodyEmpty
    | BodyBox of BodyBox
    | BodySphere of BodySphere
    | BodyCapsule of BodyCapsule
    | BodyBoxRounded of BodyBoxRounded
    | BodyConvexHull of BodyConvexHull
    | BodyShapes of BodyShape list

/// The type of a physics body; Static, Kinematic, or Dynamic.
[<Syntax
    ("Static Kinematic Dynamic", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax);
     StructuralEquality; StructuralComparison>]
type BodyType =
    | Static
    | Kinematic
    | Dynamic

/// The properties needed to describe the physical part of a body.
type BodyProperties =
    { BodyId : uint64
      Center : Vector3
      Rotation : Quaternion
      BodyType : BodyType
      BodyShape : BodyShape
      Sleeping : bool
      SleepingAllowed : bool
      Enabled : bool
      Friction : single
      Restitution : single
      LinearVelocity : Vector3
      LinearDamping : single
      AngularVelocity : Vector3
      AngularDamping : single
      AngularFactor : Vector3
      Substance : Substance
      GravityOverrideOpt : Vector3 option
      CollisionDetection : CollisionDetection
      CollisionCategories : int
      CollisionMask : int
      Bullet : bool
      Sensor : bool }

type JointAngle =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3
      Axis : Vector3
      Axis2 : Vector3
      AngleMin : single
      AngleMax : single
      Softness : single
      BiasFactor : single
      RelaxationFactor : single
      BreakImpulseThreshold : single }

type JointDistance =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3
      Length : single
      Frequency : single }

type JointFriction =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointGear =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointMotor =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPrismatic =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPulley =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRevolute =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRope =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointWheel =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

/// A joint on physics bodies.
[<Syntax
    ("JointAngle JointDistance JointFriction JointGear JointMotor JointPrismatic JointPulley JointRevolute JointRope JointWheel",
     "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax);
     StructuralEquality; NoComparison>]
type JointDevice =
    | JointEmpty
    | JointAngle of JointAngle
    | JointDistance of JointDistance
    | JointFriction of JointFriction
    | JointGear of JointGear
    | JointMotor of JointMotor
    | JointPrismatic of JointPrismatic
    | JointPulley of JointPulley
    | JointRevolute of JointRevolute
    | JointRope of JointRope
    | JointWheel of JointWheel

type JointProperties =
    { JointId : uint64
      JointDevice : JointDevice }

[<RequireQualifiedAccess>]
module JointProperties =

    let empty =
        { JointId = Gen.id64
          JointDevice = JointEmpty }

/// A message to the physics system to create a body.
type CreateBodyMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      BodyProperties : BodyProperties }

/// A message to the physics system to create multiple bodies.
type CreateBodiesMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      BodiesProperties : BodyProperties list }

/// A message to the physics system to destroy a body.
type DestroyBodyMessage =
    { SourceSimulant : Simulant
      PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple bodies.
type DestroyBodiesMessage =
    { SourceSimulant : Simulant
      PhysicsIds : PhysicsId list }

/// A message to the physics system to create a joint.
type CreateJointMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      JointProperties : JointProperties }

/// A message to the physics system to create multiple joints.
type CreateJointsMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      JointsProperties : JointProperties list }

/// A message to the physics system to destroy a joint.
type DestroyJointMessage =
    { SourceSimulant : Simulant
      PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple joints.
type DestroyJointsMessage =
    { SourceSimulant : Simulant
      PhysicsIds : PhysicsId list }

/// A message to the physics system to destroy a body.
type SetBodyEnabledMessage =
    { PhysicsId : PhysicsId
      Enabled : bool }

/// A message to the physics system to destroy a body.
type SetBodyCenterMessage =
    { PhysicsId : PhysicsId
      Center : Vector3 }

/// A message to the physics system to set the rotation of a body.
type SetBodyRotationMessage =
    { PhysicsId : PhysicsId
      Rotation : Quaternion }

/// A message to the physics system to set the linear velocity of a body.
type SetBodyLinearVelocityMessage =
    { PhysicsId : PhysicsId
      LinearVelocity : Vector3 }

/// A message to the physics system to apply a linear impulse to a body.
type ApplyBodyLinearImpulseMessage =
    { PhysicsId : PhysicsId
      LinearImpulse : Vector3
      Offset : Vector3 }

/// A message to the physics system to set the angular velocity of a body.
type SetBodyAngularVelocityMessage =
    { PhysicsId : PhysicsId
      AngularVelocity : Vector3 }

/// A message to the physics system to apply an angular impulse to a body.
type ApplyBodyAngularImpulseMessage =
    { PhysicsId : PhysicsId
      AngularImpulse : Vector3 }

/// A message to the physics system to apply a force to a body.
type ApplyBodyForceMessage =
    { PhysicsId : PhysicsId
      Force : Vector3
      Offset : Vector3 }

/// A message to the physics system to apply torque to a body.
type ApplyBodyTorqueMessage =
    { PhysicsId : PhysicsId
      Torque : Vector3 }

/// A message from the physics system describing a body collision that took place.
type BodyCollisionMessage =
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal
      Normal : Vector3
      Speed : single }

/// A message from the physics system describing a body separation that took place.
type BodySeparationMessage =
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal }

/// A message from the physics system describing the updated transform of a body.
type BodyTransformMessage =
    { BodySource : BodySourceInternal
      Center : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      AngularVelocity : Vector3 }

/// A message from the physics system.
type IntegrationMessage =
    | BodyCollisionMessage of BodyCollisionMessage
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage

/// A message to the physics system.
type PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateJointMessage of CreateJointMessage
    | CreateJointsMessage of CreateJointsMessage
    | DestroyJointMessage of DestroyJointMessage
    | DestroyJointsMessage of DestroyJointsMessage
    | SetBodyEnabledMessage of SetBodyEnabledMessage
    | SetBodyCenterMessage of SetBodyCenterMessage
    | SetBodyRotationMessage of SetBodyRotationMessage
    | SetBodyAngularVelocityMessage of SetBodyAngularVelocityMessage
    | ApplyBodyAngularImpulseMessage of ApplyBodyAngularImpulseMessage
    | SetBodyLinearVelocityMessage of SetBodyLinearVelocityMessage
    | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
    | ApplyBodyForceMessage of ApplyBodyForceMessage
    | ApplyBodyTorqueMessage of ApplyBodyTorqueMessage
    | SetGravityMessage of Vector3
    | RebuildPhysicsHackMessage

/// Represents a physics engine in Nu.
type PhysicsEngine =
    /// Check that the physics engine contain the body with the given physics id.
    abstract BodyExists : PhysicsId -> bool
    /// Get the contact normals of the body with the given physics id.
    abstract GetBodyContactNormals : PhysicsId -> Vector3 list
    /// Get the linear velocity of the body with the given physics id.
    abstract GetBodyLinearVelocity : PhysicsId -> Vector3
    /// Get the contact normals where the body with the given physics id is touching the ground.
    abstract GetBodyToGroundContactNormals : PhysicsId -> Vector3 list
    /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactNormalOpt : PhysicsId -> Vector3 option
    /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactTangentOpt : PhysicsId -> Vector3 option
    /// Check that the body with the given physics id is on the ground.
    abstract IsBodyOnGround : PhysicsId -> bool
    /// Pop all of the physics messages that have been enqueued.
    abstract PopMessages : unit -> PhysicsMessage UList * PhysicsEngine
    /// Clear all of the physics messages that have been enqueued.
    abstract ClearMessages : unit -> PhysicsEngine
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : PhysicsMessage -> PhysicsEngine
    /// Integrate the physics system one step.
    abstract Integrate : GameTime -> PhysicsMessage UList -> IntegrationMessage SegmentedArray

/// The mock implementation of PhysicsEngine.
type [<ReferenceEquality>] MockPhysicsEngine =
    private { MockPhysicsEngine : unit }
    static member make () = { MockPhysicsEngine = () }
    interface PhysicsEngine with
        member physicsEngine.BodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormals _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormalOpt _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactTangentOpt _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.IsBodyOnGround _ = failwith "No bodies in MockPhysicsEngine"
        member physicsEngine.PopMessages () = (UList.makeEmpty Functional, physicsEngine :> PhysicsEngine)
        member physicsEngine.ClearMessages () = physicsEngine :> PhysicsEngine
        member physicsEngine.EnqueueMessage _ = physicsEngine :> PhysicsEngine
        member physicsEngine.Integrate _ _ = SegmentedArray.empty

[<RequireQualifiedAccess>]
module Physics =

    /// Convert a category mask to a value that represents collision categories.
    /// Examples -
    ///     @ = -1
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either @ or a binary number!
    let categorizeCollisionMask categoryMask =
        match categoryMask with
        | "@" -> -1
        | _ -> Convert.ToInt32 (categoryMask, 2)

    /// Localize a body shape to a specific physics object.
    let rec localizeBodyShape (size : Vector3) (bodyShape : BodyShape) =
        match bodyShape with
        | BodyEmpty -> BodyEmpty
        | BodyBox bodyBox -> BodyBox { Center = Vector3.Multiply (bodyBox.Center, size); Size = Vector3.Multiply (size, bodyBox.Size); PropertiesOpt = bodyBox.PropertiesOpt }
        | BodySphere bodySphere -> BodySphere { Center = size.X * bodySphere.Center; Radius = size.X * bodySphere.Radius; PropertiesOpt = bodySphere.PropertiesOpt }
        | BodyCapsule bodyCapsule -> BodyCapsule { Center = size.Y * bodyCapsule.Center; Height = size.Y * bodyCapsule.Height; Radius = size.Y * bodyCapsule.Radius; PropertiesOpt = bodyCapsule.PropertiesOpt }
        | BodyBoxRounded bodyBoxRounded -> BodyBoxRounded { Center = size.Y * bodyBoxRounded.Center; Size = Vector3.Multiply (size, bodyBoxRounded.Size); Radius = size.X * bodyBoxRounded.Radius; PropertiesOpt = bodyBoxRounded.PropertiesOpt }
        | BodyConvexHull bodyConvexHull ->
            let vertices = Array.map (fun vertex -> vertex * size) bodyConvexHull.Vertices
            BodyConvexHull { Center = Vector3.Multiply (size, bodyConvexHull.Center); Vertices = vertices; PropertiesOpt = bodyConvexHull.PropertiesOpt }
        | BodyShapes bodyShapes ->
            let bodyShapes = List.map (localizeBodyShape size) bodyShapes
            BodyShapes bodyShapes