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
type [<NoComparison>] BodySourceInternal =
    { Simulant : Simulant
      BodyId : uint64 }

/// Store origination information about a simulant physics shape body.
type [<NoComparison>] BodyShapeSourceInternal =
    { Simulant : Simulant
      BodyId : uint64
      ShapeId : uint64 }

/// Describes body shape-specific properties.
type [<NoComparison>] BodyShapeProperties =
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

/// The shape of a physics body box.
type [<NoComparison>] BodyBox =
    { Extent : Vector3
      Center : Vector3
      PropertiesOpt : BodyShapeProperties option }
    static member toBox bodyBox =
        box3 (bodyBox.Center - bodyBox.Extent) (bodyBox.Extent * 2.0f)
    static member fromBox (box : Box3) =
        { Extent = box.Size * 0.5f; Center = box.Center; PropertiesOpt = None }

/// The shape of a physics body sphere.
type [<NoComparison>] BodySphere =
    { Radius : single
      Center : Vector3
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type [<NoComparison>] BodyCapsule =
    { Height : single
      Radius : single
      Center : Vector3
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type [<NoComparison>] BodyBoxRounded =
    { Extent : Vector3
      Radius : single
      Center : Vector3
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body polygon.
type [<NoComparison>] BodyPolygon =
    { Vertices : Vector3 array
      Center : Vector3
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body.
[<Syntax
    ("BodyEmpty BodyBox BodySphere BodyCapsule BodyPolygon BodyShapes", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type [<NoComparison>] BodyShape =
    | BodyEmpty
    | BodyBox of BodyBox
    | BodySphere of BodySphere
    | BodyCapsule of BodyCapsule
    | BodyBoxRounded of BodyBoxRounded
    | BodyPolygon of BodyPolygon
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
type [<NoComparison>] BodyProperties =
    { BodyId : uint64
      Position : Vector3
      Rotation : Quaternion
      BodyShape : BodyShape
      BodyType : BodyType
      Awake : bool
      Enabled : bool
      Density : single
      Friction : single
      Restitution : single
      LinearVelocity : Vector3
      LinearDamping : single
      AngularVelocity : Vector3
      AngularDamping : single
      FixedRotation : bool
      Inertia : single
      GravityScale : single
      CollisionCategories : int
      CollisionMask : int
      IgnoreCCD : bool
      Bullet : bool
      Sensor : bool }

[<RequireQualifiedAccess>]
module BodyProperties =

    let empty =
        { BodyId = 0UL
          Position = v3Zero
          Rotation = quatIdentity
          BodyShape = BodyEmpty
          BodyType = Dynamic
          Awake = true
          Enabled = true
          Density = Constants.Physics.DensityDefault
          Friction = 0.2f
          Restitution = 0.0f
          LinearVelocity = v3Zero
          LinearDamping = 0.0f
          AngularVelocity = v3Zero
          AngularDamping = 0.0f
          FixedRotation = false
          Inertia = 0.0f
          GravityScale = 1.0f
          CollisionCategories = 1
          CollisionMask = -1
          IgnoreCCD = false
          Bullet = false
          Sensor = false }

type [<NoComparison>] JointAngle =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3
      TargetAngle : single
      Softness : single }

type [<NoComparison>] JointDistance =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3
      Length : single
      Frequency : single }

type [<NoComparison>] JointFriction =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointGear =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointMotor =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointPrismatic =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointPulley =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointRevolute =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointRope =
    { TargetId : PhysicsId
      TargetId2 : PhysicsId
      Anchor : Vector3
      Anchor2 : Vector3 }

type [<NoComparison>] JointWheel =
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

type [<NoComparison>] JointProperties =
    { JointId : uint64
      JointDevice : JointDevice }

[<RequireQualifiedAccess>]
module JointProperties =

    let empty =
        { JointId = Gen.id64
          JointDevice = JointEmpty }

/// A message to the physics system to create a body.
type [<NoComparison>] CreateBodyMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      BodyProperties : BodyProperties }

/// A message to the physics system to create multiple bodies.
type [<NoComparison>] CreateBodiesMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      BodiesProperties : BodyProperties list }

/// A message to the physics system to destroy a body.
type [<NoComparison>] DestroyBodyMessage =
    { PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple bodies.
type [<NoComparison>] DestroyBodiesMessage =
    { PhysicsIds : PhysicsId list }

/// A message to the physics system to create a joint.
type [<NoComparison>] CreateJointMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      JointProperties : JointProperties }

/// A message to the physics system to create multiple joints.
type [<NoComparison>] CreateJointsMessage =
    { SourceSimulant : Simulant
      SourceId : uint64
      JointsProperties : JointProperties list }

/// A message to the physics system to destroy a joint.
type [<NoComparison>] DestroyJointMessage =
    { PhysicsId : PhysicsId }

/// A message to the physics system to destroy multiple joints.
type [<NoComparison>] DestroyJointsMessage =
    { PhysicsIds : PhysicsId list }

/// A message to the physics system to destroy a body.
type [<NoComparison>] SetBodyEnabledMessage =
    { PhysicsId : PhysicsId
      Enabled : bool }

/// A message to the physics system to destroy a body.
type [<NoComparison>] SetBodyPositionMessage =
    { PhysicsId : PhysicsId
      Position : Vector3 }

/// A message to the physics system to set the rotation of a body.
type [<NoComparison>] SetBodyRotationMessage =
    { PhysicsId : PhysicsId
      Rotation : Quaternion }

/// A message to the physics system to set the linear velocity of a body.
type [<NoComparison>] SetBodyLinearVelocityMessage =
    { PhysicsId : PhysicsId
      LinearVelocity : Vector3 }

/// A message to the physics system to apply a linear impulse to a body.
type [<NoComparison>] ApplyBodyLinearImpulseMessage =
    { PhysicsId : PhysicsId
      LinearImpulse : Vector3 }

/// A message to the physics system to set the angular velocity of a body.
type [<NoComparison>] SetBodyAngularVelocityMessage =
    { PhysicsId : PhysicsId
      AngularVelocity : Vector3 }

/// A message to the physics system to apply an angular impulse to a body.
type [<NoComparison>] ApplyBodyAngularImpulseMessage =
    { PhysicsId : PhysicsId
      AngularImpulse : Vector3 }

/// A message to the physics system to apply a force to a body.
type [<NoComparison>] ApplyBodyForceMessage =
    { PhysicsId : PhysicsId
      Force : Vector3 }

/// A message to the physics system to apply torque to a body.
type [<NoComparison>] ApplyBodyTorqueMessage =
    { PhysicsId : PhysicsId
      Torque : Vector3 }

/// A message from the physics system describing a body collision that took place.
type [<NoComparison>] BodyCollisionMessage =
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal
      Normal : Vector3
      Speed : single }

/// A message from the physics system describing a body separation that took place.
type [<NoComparison>] BodySeparationMessage =
    { BodyShapeSource : BodyShapeSourceInternal
      BodyShapeSource2 : BodyShapeSourceInternal }

/// A message from the physics system describing the updated transform of a body.
type [<NoComparison>] BodyTransformMessage =
    { BodySource : BodySourceInternal
      Position : Vector3
      Rotation : Quaternion
      LinearVelocity : Vector3
      AngularVelocity : Vector3 }

/// A message from the physics system.
type [<NoComparison>] IntegrationMessage =
    | BodyCollisionMessage of BodyCollisionMessage
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage

/// A message to the physics system.
type [<NoComparison>] PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateJointMessage of CreateJointMessage
    | CreateJointsMessage of CreateJointsMessage
    | DestroyJointMessage of DestroyJointMessage
    | DestroyJointsMessage of DestroyJointsMessage
    | SetBodyEnabledMessage of SetBodyEnabledMessage
    | SetBodyPositionMessage of SetBodyPositionMessage
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
    /// Integrate the physics system one frame.
    abstract Integrate : int64 -> PhysicsMessage UList -> IntegrationMessage SegmentedArray

/// The mock implementation of PhysicsEngine.
type [<ReferenceEquality; NoComparison>] MockPhysicsEngine =
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
    let rec localizeBodyShape (extent : Vector3) (bodyShape : BodyShape) =
        match bodyShape with
        | BodyEmpty -> BodyEmpty
        | BodyBox bodyBox -> BodyBox { Extent = Vector3.Multiply (extent, bodyBox.Extent); Center = Vector3.Multiply (extent, bodyBox.Center); PropertiesOpt = bodyBox.PropertiesOpt }
        | BodySphere bodySphere -> BodySphere { Radius = extent.X * bodySphere.Radius; Center = extent.X * bodySphere.Center; PropertiesOpt = bodySphere.PropertiesOpt }
        | BodyCapsule bodyCapsule -> BodyCapsule { Height = extent.Y * bodyCapsule.Height; Radius = extent.Y * bodyCapsule.Radius; Center = extent.Y * bodyCapsule.Center; PropertiesOpt = bodyCapsule.PropertiesOpt }
        | BodyBoxRounded bodyBoxRounded -> BodyBoxRounded { Extent = Vector3.Multiply (extent, bodyBoxRounded.Extent); Radius = extent.X * bodyBoxRounded.Radius; Center = extent.Y * bodyBoxRounded.Center; PropertiesOpt = bodyBoxRounded.PropertiesOpt }
        | BodyPolygon bodyPolygon ->
            let vertices = Array.map (fun vertex -> vertex * extent) bodyPolygon.Vertices
            BodyPolygon { Vertices = vertices; Center = Vector3.Multiply (extent, bodyPolygon.Center); PropertiesOpt = bodyPolygon.PropertiesOpt }
        | BodyShapes bodyShapes ->
            let bodyShapes = List.map (localizeBodyShape extent) bodyShapes
            BodyShapes bodyShapes