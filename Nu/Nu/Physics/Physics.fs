// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// Identifies a body that can be found in a physics engine.
type [<CustomEquality; NoComparison>] BodyId =
    { BodySource : Simulant
      BodyIndex : int }

    /// Hash a BodyId.
    static member hash pid =
        pid.BodySource.SimulantAddress.GetHashCode () ^^^
        pid.BodyIndex.GetHashCode ()

    /// Equate BodyIds.
    static member equals pid pid2 =
        Address.equals pid.BodySource.SimulantAddress pid2.BodySource.SimulantAddress &&
        pid.BodyIndex = pid2.BodyIndex

    interface BodyId IEquatable with
        member this.Equals that =
            BodyId.equals this that

    override this.Equals that =
        match that with
        | :? BodyId as that -> BodyId.equals this that
        | _ -> false

    override this.GetHashCode () =
        BodyId.hash this

/// Identifies a body shape in a physics engine.
and ShapeIndex =
    { BodyId : BodyId
      ShapeIndex : int }

/// Identifies a joint in a physics engine.
and JointId =
    { JointSource : Simulant
      JointIndex : int }

/// Describes body shape-specific properties.
type BodyShapeProperties =
    { ShapeIndex : int
      FrictionOpt : single option
      RestitutionOpt : single option
      CollisionCategoriesOpt : int option
      CollisionMaskOpt : int option
      SensorOpt : bool option }

[<RequireQualifiedAccess>]
module BodyShapeProperties =

    let empty =
        { ShapeIndex = 0
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

/// Describe the form of collision detection to use.
[<Syntax
    ("DiscontinuousDetection ContinuousDetection", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax);
     StructuralEquality; NoComparison; Struct>]
type CollisionDetection =
    | Discontinuous
    | Continuous of MotionThreshold : single * SweptSphereRadius : single

/// The shape of a physics body box.
type BodyBox =
    { Size : Vector3
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }
    static member ofBox3 (box : Box3) =
        { Size = box.Size; TransformOpt = Some (Matrix4x4.CreateTranslation box.Center); PropertiesOpt = None }

/// The shape of a physics body sphere.
type BodySphere =
    { Radius : single
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyCapsule =
    { Height : single
      Radius : single
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type BodyBoxRounded =
    { Size : Vector3
      Radius : single
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body convex hull.
type BodyConvexHull =
    { Vertices : Vector3 array
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body static model.
type BodyStaticModel =
    { StaticModel : StaticModel AssetTag
      TransformOpt : Matrix4x4 option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body static model surface.
type BodyStaticModelSurface =
    { SurfaceIndex : int
      StaticModel : StaticModel AssetTag
      TransformOpt : Matrix4x4 option
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
    | BodyStaticModel of BodyStaticModel
    | BodyStaticModelSurface of BodyStaticModelSurface
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
    { BodyIndex : int
      Center : Vector3
      Rotation : Quaternion
      BodyType : BodyType
      BodyShape : BodyShape
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
      GravityOverride : Vector3 option
      CollisionDetection : CollisionDetection
      CollisionCategories : int
      CollisionMask : int
      Sensor : bool }

type JointAngle =
    { TargetId : BodyId
      TargetId2 : BodyId
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
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3
      Length : single
      Frequency : single }

type JointFriction =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointGear =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointMotor =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPrismatic =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointPulley =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRevolute =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointRope =
    { TargetId : BodyId
      TargetId2 : BodyId
      Anchor : Vector3
      Anchor2 : Vector3 }

type JointWheel =
    { TargetId : BodyId
      TargetId2 : BodyId
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
    { JointIndex : int
      JointDevice : JointDevice }

[<RequireQualifiedAccess>]
module JointProperties =

    let empty =
        { JointIndex = 0
          JointDevice = JointEmpty }

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
type CreateJointMessage =
    { JointSource : Simulant
      JointProperties : JointProperties }

/// A message to the physics system to create multiple joints.
type CreateJointsMessage =
    { JointsSource : Simulant
      JointsProperties : JointProperties list }

/// A message to the physics system to destroy a joint.
type DestroyJointMessage =
    { JointId : JointId }

/// A message to the physics system to destroy multiple joints.
type DestroyJointsMessage =
    { JointIds : JointId list }

/// A message to the physics system to destroy a body.
type SetBodyEnabledMessage =
    { BodyId : BodyId
      Enabled : bool }

/// A message to the physics system to destroy a body.
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

/// A message to the physics system to apply a linear impulse to a body.
type ApplyBodyLinearImpulseMessage =
    { BodyId : BodyId
      LinearImpulse : Vector3
      Offset : Vector3 }

/// A message to the physics system to set the angular velocity of a body.
type SetBodyAngularVelocityMessage =
    { BodyId : BodyId
      AngularVelocity : Vector3 }

/// A message to the physics system to apply an angular impulse to a body.
type ApplyBodyAngularImpulseMessage =
    { BodyId : BodyId
      AngularImpulse : Vector3 }

/// A message to the physics system to apply a force to a body.
type ApplyBodyForceMessage =
    { BodyId : BodyId
      Force : Vector3
      Offset : Vector3 }

/// A message to the physics system to apply torque to a body.
type ApplyBodyTorqueMessage =
    { BodyId : BodyId
      Torque : Vector3 }

/// An internally used message to the physics system to set the observed state of a body.
type SetBodyObservableMessage =
    { BodyId : BodyId
      Observable : bool }

/// A message from the physics system describing a body collision that took place.
type BodyCollisionMessage =
    { BodyShapeSource : ShapeIndex
      BodyShapeSource2 : ShapeIndex
      Normal : Vector3 }

/// A message from the physics system describing a body separation that took place.
type BodySeparationMessage =
    { BodyShapeSource : ShapeIndex
      BodyShapeSource2 : ShapeIndex }

/// A message from the physics system describing the updated transform of a body.
type BodyTransformMessage =
    { BodyId : BodyId
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
    | SetBodyObservableMessage of SetBodyObservableMessage
    | SetGravityMessage of Vector3
    | ClearPhysicsMessageInternal

/// Represents a physics engine in Nu.
/// TODO: consider seeing if we can make message methods side-effect rather than functional.
type PhysicsEngine =
    /// Check that the physics engine contain the body with the given physics id.
    abstract GetBodyExists : BodyId -> bool
    /// Get the contact normals of the body with the given physics id.
    abstract GetBodyContactNormals : BodyId -> Vector3 list
    /// Get the linear velocity of the body with the given physics id.
    abstract GetBodyLinearVelocity : BodyId -> Vector3
    /// Get the contact normals where the body with the given physics id is touching the ground.
    abstract GetBodyToGroundContactNormals : BodyId -> Vector3 list
    /// Get a contact normal where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactNormalOpt : BodyId -> Vector3 option
    /// Get a contact tangent where the body with the given physics id is touching the ground (if one exists).
    abstract GetBodyToGroundContactTangentOpt : BodyId -> Vector3 option
    /// Check that the body with the given physics id is on the ground.
    abstract IsBodyOnGround : BodyId -> bool
    /// Pop all of the physics messages that have been enqueued.
    abstract PopMessages : unit -> PhysicsMessage UList * PhysicsEngine
    /// Clear all of the physics messages that have been enqueued.
    abstract ClearMessages : unit -> PhysicsEngine
    /// Enqueue a message from an external source.
    abstract EnqueueMessage : PhysicsMessage -> PhysicsEngine
    /// Integrate the physics system one step.
    abstract Integrate : GameTime -> PhysicsMessage UList -> IntegrationMessage SArray
    /// Handle physics clean up by freeing all created resources.
    abstract CleanUp : unit -> unit

/// The stub implementation of PhysicsEngine.
type [<ReferenceEquality>] StubPhysicsEngine =
    private { StubPhysicsEngine : unit }
    static member make () = { StubPhysicsEngine = () }
    interface PhysicsEngine with
        member physicsEngine.GetBodyExists _ = false
        member physicsEngine.GetBodyContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyLinearVelocity _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormals _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactNormalOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.GetBodyToGroundContactTangentOpt _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.IsBodyOnGround _ = failwith "No bodies in StubPhysicsEngine"
        member physicsEngine.PopMessages () = (UList.makeEmpty Functional, physicsEngine :> PhysicsEngine)
        member physicsEngine.ClearMessages () = physicsEngine :> PhysicsEngine
        member physicsEngine.EnqueueMessage _ = physicsEngine :> PhysicsEngine
        member physicsEngine.Integrate _ _ = SArray.empty
        member physicsEngine.CleanUp () = ()

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
        | Constants.Engine.WildcardName -> -1
        | _ -> Convert.ToInt32 (categoryMask, 2)

    /// Localize a body shape to a specific size.
    let rec localizeBodyShape (size : Vector3) bodyShape =
        let scaleTranslation (scalar : Vector3) (transformOpt : Matrix4x4 option) =
            match transformOpt with
            | Some transform ->
                let mutable transform = transform
                transform.Translation <- transform.Translation * scalar
                Some transform
            | None -> None
        match bodyShape with
        | BodyEmpty -> BodyEmpty
        | BodyBox bodyBox -> BodyBox { bodyBox with Size = Vector3.Multiply (size, bodyBox.Size); TransformOpt = scaleTranslation size bodyBox.TransformOpt }
        | BodySphere bodySphere -> BodySphere { bodySphere with Radius = size.X * bodySphere.Radius; TransformOpt = scaleTranslation size bodySphere.TransformOpt }
        | BodyCapsule bodyCapsule -> BodyCapsule { bodyCapsule with Height = size.Y * bodyCapsule.Height; Radius = size.Y * bodyCapsule.Radius; TransformOpt = scaleTranslation size bodyCapsule.TransformOpt }
        | BodyBoxRounded bodyBoxRounded -> BodyBoxRounded { bodyBoxRounded with Size = Vector3.Multiply (size, bodyBoxRounded.Size); Radius = size.X * bodyBoxRounded.Radius; TransformOpt = scaleTranslation size bodyBoxRounded.TransformOpt }
        | BodyConvexHull bodyConvexHull -> BodyConvexHull { bodyConvexHull with Vertices = Array.map (fun vertex -> size * vertex) bodyConvexHull.Vertices; TransformOpt = scaleTranslation size bodyConvexHull.TransformOpt }
        | BodyStaticModel _ as bodyStaticModel -> bodyStaticModel
        | BodyStaticModelSurface _ as bodyStaticModelSurface -> bodyStaticModelSurface
        | BodyShapes bodyShapes -> BodyShapes (List.map (localizeBodyShape size) bodyShapes)