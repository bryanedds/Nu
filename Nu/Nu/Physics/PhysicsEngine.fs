    // Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime

/// Identifies a body that can be found in a physics engine.
type [<CustomEquality; CustomComparison>] BodyId =
    { BodySource : Simulant
      BodyIndex : int }

    /// Hash a BodyId.
    static member hash bid =
        bid.BodySource.SimulantAddress.GetHashCode () ^^^
        bid.BodyIndex.GetHashCode ()

    /// Equate BodyIds.
    static member equals bid bid2 =
        if  refEq bid.BodySource.SimulantAddress bid2.BodySource.SimulantAddress || // OPTIMIZATION: first check ref equality.
            bid.BodySource.SimulantAddress.HashCode = bid2.BodySource.SimulantAddress.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
            String.equateManyBack bid.BodySource.SimulantAddress.Names bid2.BodySource.SimulantAddress.Names then // OPTIMIZATION: later names in an address tend to have higher variance.
            bid.BodyIndex = bid2.BodyIndex
        else false

    /// Compare BodyIds.
    static member compare bid bid2 =
        if  refEq bid.BodySource.SimulantAddress bid2.BodySource.SimulantAddress || // OPTIMIZATION: first check ref equality.
            bid.BodySource.SimulantAddress.HashCode = bid2.BodySource.SimulantAddress.HashCode && // OPTIMIZATION: check hash equality to bail as quickly as possible.
            String.equateManyBack bid.BodySource.SimulantAddress.Names bid2.BodySource.SimulantAddress.Names then // OPTIMIZATION: later names in an address tend to have higher variance.
            bid.BodyIndex.CompareTo bid2.BodyIndex
        else
            let result = String.compareMany bid.BodySource.SimulantAddress.Names bid2.BodySource.SimulantAddress.Names
            if result <> 0 then result
            else bid.BodyIndex.CompareTo bid2.BodyIndex

    interface BodyId IEquatable with
        member this.Equals that =
            BodyId.equals this that

    interface BodyId IComparable with
        member this.CompareTo that =
            BodyId.compare this that

    interface IComparable with
        member this.CompareTo that =
            match that with
            | :? BodyId as that -> BodyId.compare this that
            | _ -> -1

    override this.Equals that =
        match that with
        | :? BodyId as that -> BodyId.equals this that
        | _ -> false

    override this.GetHashCode () =
        BodyId.hash this

/// Identifies a body shape in a physics engine.
type BodyShapeIndex =
    { BodyId : BodyId
      BodyShapeIndex : int }

/// Describes body shape-specific properties.
type BodyShapeProperties =
    { BodyShapeIndex : int
      FrictionOpt : single option
      RestitutionOpt : single option
      CollisionCategoriesOpt : int option
      CollisionMaskOpt : int option
      SensorOpt : bool option }

    /// The empty body shape properties value.
    static member empty =
        { BodyShapeIndex = 0
          FrictionOpt = None
          RestitutionOpt = None
          CollisionCategoriesOpt = None
          CollisionMaskOpt = None
          SensorOpt = None }

    /// Check that the body shape properties are applicable for 3D physics.
    static member validateUtilization3d properties =
        properties.FrictionOpt.IsNone &&
        properties.RestitutionOpt.IsNone &&
        properties.CollisionCategoriesOpt.IsNone &&
        properties.CollisionMaskOpt.IsNone &&
        properties.SensorOpt.IsNone

/// Internal object that carries interstitial information between Nu and a physics engine.
type [<NoEquality; NoComparison>] BodyUserObject =
    { BodyId : BodyId
      Dispose : unit -> unit }

/// Describes the substantial nature of a body in terms of mass or density.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type Substance =

    /// Density is calculated from dividing constant mass by volume.
    | Mass of Mass : single

    /// Mass is calculated from multiplying constant density with volume (may be approximate).
    | Density of Density : single

/// Specifies a form of collision detection to use.
type CollisionDetection =

    /// Specifies discrete collision detection.
    /// This is the fastest form of collision detection, but fast-moving objects may tunnel through other
    /// objects without detecting a collision.
    | Discrete

    /// Specifies continuous collision detection.
    /// This form of collision detection is slower, but fast-moving objects will not tunnel through other
    /// objects without detecting a collision.
    | Continuous

/// Describes the physical profile of a complex body.
type Profile =

    /// A convex shape.
    /// This shape is defined by a body that forms a convex hull.
    /// Moderately efficient but accurate enough for many cases.
    | Convex

    /// A concave shape.
    /// This shape is defined by a body may form a concave hull.
    /// Least efficient but often more accurate.
    /// TODO: should this case be specified to require points to be formatted as groups of 3 to form triangles even for user-defined 2D and 3D engines?
    | Concave

    /// A simplified axis-aligned bounds.
    /// This shape is defined by a bounding box around a body.
    /// Most efficient but least accurate.
    | Bounds

/// The shape of a physics body box.
type BoxShape =
    { Size : Vector3
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }
    static member ofBox3 (box : Box3) =
        { Size = box.Size; TransformOpt = Some (Affine.makeTranslation box.Center); PropertiesOpt = None }

/// The shape of a physics body sphere.
type SphereShape =
    { Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body capsule.
type CapsuleShape =
    { Height : single
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body rounded box.
type BoxRoundedShape =
    { Size : Vector3
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }
      
/// The shape of a massless physics body in terms of one line segment.
/// Collision occurs at its side.
type EdgeShape =
    { Start : Vector3
      Stop : Vector3
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a massless physics body in terms of a free form sequence of line segments.
/// Collision occurs at its sides. When closed, the last link connects to the first.
/// It is expected that self-intersection does not occur.
/// It properly handles ghost collisions compared to multiple EdgeShapes: https://box2d.org/posts/2020/06/ghost-collisions/
/// Box2D and Aether.Physics2D call this a ChainShape, but it's not a physical chain.
type ContourShape =
    { Links : Vector3 array
      Closed : bool
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body defined by body-relative points.
type PointsShape =
    { Points : Vector3 array
      Profile : Profile
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body in terms of triangle faces.
type GeometryShape =
    { Vertices : Vector3 array
      Profile : Profile
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body in terms of a static model.
type StaticModelShape =
    { StaticModel : StaticModel AssetTag
      Profile : Profile
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body in terms of a static model surface.
type StaticModelSurfaceShape =
    { StaticModel : StaticModel AssetTag
      SurfaceIndex : int
      Profile : Profile
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body in terms of a terrain height map.
type TerrainShape =
    { Resolution : Vector2i
      Bounds : Box3
      HeightMap : HeightMap
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type BodyShape =
    | EmptyShape
    | BoxShape of BoxShape
    | SphereShape of SphereShape
    | CapsuleShape of CapsuleShape
    | BoxRoundedShape of BoxRoundedShape
    | EdgeShape of EdgeShape
    | ContourShape of ContourShape
    | PointsShape of PointsShape
    | GeometryShape of GeometryShape
    | StaticModelShape of StaticModelShape
    | StaticModelSurfaceShape of StaticModelSurfaceShape
    | TerrainShape of TerrainShape
    | BodyShapes of BodyShape list

    /// Get the shape's transform where it exists.
    member this.TransformOpt =
        match this with
        | EmptyShape -> None
        | BoxShape box -> box.TransformOpt
        | SphereShape sphere -> sphere.TransformOpt
        | CapsuleShape capsule -> capsule.TransformOpt
        | BoxRoundedShape boxRounded -> boxRounded.TransformOpt
        | EdgeShape edge -> edge.TransformOpt
        | ContourShape contour -> contour.TransformOpt
        | PointsShape points -> points.TransformOpt
        | GeometryShape geometry -> geometry.TransformOpt
        | StaticModelShape staticModel -> staticModel.TransformOpt
        | StaticModelSurfaceShape staticModelSurface -> staticModelSurface.TransformOpt
        | TerrainShape terrain -> terrain.TransformOpt
        | BodyShapes _ -> None

    /// Get the shape's properties where they exist.
    member this.PropertiesOpt =
        match this with
        | EmptyShape -> None
        | BoxShape box -> box.PropertiesOpt
        | SphereShape sphere -> sphere.PropertiesOpt
        | CapsuleShape capsule -> capsule.PropertiesOpt
        | BoxRoundedShape boxRounded -> boxRounded.PropertiesOpt
        | EdgeShape edge -> edge.PropertiesOpt
        | ContourShape contour -> contour.PropertiesOpt
        | PointsShape points -> points.PropertiesOpt
        | GeometryShape geometry -> geometry.PropertiesOpt
        | StaticModelShape staticModel -> staticModel.PropertiesOpt
        | StaticModelSurfaceShape staticModelSurface -> staticModelSurface.PropertiesOpt
        | TerrainShape terrain -> terrain.PropertiesOpt
        | BodyShapes _ -> None

    /// Whether a shape is considered a 'primitive', such as one that is entirely localized via
    /// Physics.localizePrimitiveBodyShape.
    member this.IsPrimitive =
        match this with
        | EmptyShape
        | BoxShape _
        | SphereShape _
        | CapsuleShape _
        | BoxRoundedShape _
        | EdgeShape _
        | ContourShape _
        | PointsShape _ -> true
        | GeometryShape _
        | StaticModelShape _
        | StaticModelSurfaceShape _
        | TerrainShape _ -> false
        | BodyShapes bodyShapes -> List.forall (fun (bodyShape : BodyShape) -> bodyShape.IsPrimitive) bodyShapes

    /// Check that a shape or any of its child shapes are sensors.
    member this.HasSensors =
        let isSensor =
            match this.PropertiesOpt with
            | Some properties ->
                match properties.SensorOpt with
                | Some sensor -> sensor
                | None -> false
            | None -> false
        if not isSensor then
            match this with
            | BodyShapes bodyShapes -> List.exists (fun (bodyShape : BodyShape) -> bodyShape.HasSensors) bodyShapes
            | _ -> false
        else true

/// Describes a physics body intersection, such as found from a ray cast.
type [<Struct>] BodyIntersection =
    { BodyShapeIntersected : BodyShapeIndex
      Progress : single
      Position : Vector3
      Normal : Vector3 }

    /// Make a ray intersesction value.
    static member make bodyShapeIntersected progress position normal =
        { BodyShapeIntersected = bodyShapeIntersected
          Progress = progress
          Position = position
          Normal = normal }

/// The type of a physics body.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.SimpleThresholdMax)>]
type BodyType =

    /// Immovable body that does not respond to forces or collisions.
    | Static

    /// Movable body that does not respond to forces or collisions, but can be moved kinematically by the user.
    | Kinematic

    /// Movable character body that does not respond to forces or collisions, but can be moved kinematically by the
    /// user. Character bodies can follow special physics rules that give them additional capabilities, such as walking
    /// up stairs and slopes.
    | KinematicCharacter

    /// Movable body that responds to forces and collisions.
    | Dynamic

    /// Movable character body that responds to forces and collisions. Character bodies can follow special physics
    /// rules that give them additional capabilities, such as walking up stairs and slopes.
    | DynamicCharacter

    /// Movable vehicle body that responds to forces and collisions and can be driven by vehicle-specific constraints.
    | Vehicle

    // Check that this body type is some sort of character.
    member this.IsCharacter =
        match this with
        | Static | Kinematic | Dynamic | Vehicle -> false
        | KinematicCharacter | DynamicCharacter -> true

/// The way in which an entity's motion is driven by a corresponding body.
type PhysicsMotion =

    /// When a body transform message comes in from physics subsystem, the associated entity's transform will be set by
    /// the game engine. When an entity's transform is set by the user, the associated body's transform message will be
    /// sent to physics engine.
    | SynchronizedMotion

    /// When a body transform message comes in from physics subsystem, the associated entity's transform will not be
    /// set by the game engine; instead an event will be published that can be handled manually. When an entity's
    /// transform is set by the user, nothing will be sent to the physics engine.
    | ManualMotion

/// The properties specific to the utilization of the character body types.
type [<SymbolicExpansion>] CharacterProperties =
    { CollisionPadding : single
      CollisionTolerance : single
      SlopeMax : single
      StairStepUp : Vector3
      StairStepDownStickToFloor : Vector3
      StairStepDownExtra : Vector3
      StairStepForwardTest : single
      StairStepForwardMin : single
      StairCosAngleForwardContact : single }

    /// The default character properties.
    static member defaultProperties =
        { CollisionPadding = 0.02f
          CollisionTolerance = 0.001f
          SlopeMax = Math.DegreesToRadians 45.0f
          StairStepUp = v3 0.0f 0.25f 0.0f
          StairStepDownStickToFloor = v3 0.0f -0.25f 0.0f
          StairStepDownExtra = v3Zero
          StairStepForwardTest = 0.15f
          StairStepForwardMin = 0.02f
          StairCosAngleForwardContact = cos (Math.DegreesToRadians 75.0f) }

/// The properties needed to describe the vehicle aspects of a body.
type VehicleProperties =
    | VehiclePropertiesAbsent
    | VehiclePropertiesAether
    | VehiclePropertiesJolt of JoltPhysicsSharp.VehicleConstraintSettings

/// The properties needed to describe the physical part of a body.
type BodyProperties =
    { Enabled : bool
      Center : Vector3
      Rotation : Quaternion
      Scale : Vector3
      BodyType : BodyType
      BodyShape : BodyShape
      SleepingAllowed : bool
      Friction : single
      Restitution : single
      LinearVelocity : Vector3
      LinearDamping : single
      AngularVelocity : Vector3
      AngularDamping : single
      AngularFactor : Vector3
      Substance : Substance
      GravityOverride : Vector3 option
      CharacterProperties : CharacterProperties
      VehicleProperties : VehicleProperties
      CollisionDetection : CollisionDetection
      CollisionCategories : int
      CollisionMask : int
      Sensor : bool
      Awake : bool
      BodyIndex : int }

    member this.HasSensors =
        this.Sensor || this.BodyShape.HasSensors

/// Identifies a joint in a physics engine.
type BodyJointId =
    { BodyJointSource : Simulant
      BodyJointIndex : int }

/// Allows users to create their own one-body 2D joints.
type OneBodyJoint2d =
    { CreateOneBodyJoint : (single -> single) -> (Vector3 -> nkast.Aether.Physics2D.Common.Vector2) -> nkast.Aether.Physics2D.Dynamics.Body -> nkast.Aether.Physics2D.Dynamics.Joints.Joint }

/// Allows users to create their own two-body 2D joints.
type TwoBodyJoint2d =
    { CreateTwoBodyJoint : (single -> single) -> (Vector3 -> nkast.Aether.Physics2D.Common.Vector2) -> nkast.Aether.Physics2D.Dynamics.Body -> nkast.Aether.Physics2D.Dynamics.Body -> nkast.Aether.Physics2D.Dynamics.Joints.Joint }

/// Allows users to create their own one-body 3D joints.
type OneBodyJoint3d =
    { CreateOneBodyJoint : JoltPhysicsSharp.Body -> JoltPhysicsSharp.Constraint }

/// Allows users to create their own two-body 3D joints.
type TwoBodyJoint3d =
    { CreateTwoBodyJoint : JoltPhysicsSharp.Body -> JoltPhysicsSharp.Body -> JoltPhysicsSharp.Constraint }

/// A joint on physics bodies.
/// Because physics joints don't generalize well across 2D and 3D - or even across different 3D physics engines, we're
/// currently only providing joint creation via the user-defined cases.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type BodyJoint =
    | EmptyJoint
    | OneBodyJoint2d of OneBodyJoint2d
    | TwoBodyJoint2d of TwoBodyJoint2d
    | OneBodyJoint3d of OneBodyJoint3d
    | TwoBodyJoint3d of TwoBodyJoint3d

/// Describes the universal properties of a body joint.
type BodyJointProperties =
    { BodyJoint : BodyJoint
      BodyJointTarget : BodyId
      BodyJointTarget2Opt : BodyId option
      BodyJointEnabled : bool
      BreakingPoint : single
      Broken : bool
      CollideConnected : bool
      BodyJointIndex : int }

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

/// A message from the physics system.
type IntegrationMessage =
    | BodyPenetrationMessage of BodyPenetrationMessage
    | BodySeparationMessage of BodySeparationMessage
    | BodyTransformMessage of BodyTransformMessage
    | BodyJointBreakMessage of BodyJointBreakMessage

/// A message to the physics system.
type PhysicsMessage =
    | CreateBodyMessage of CreateBodyMessage
    | CreateBodiesMessage of CreateBodiesMessage
    | DestroyBodyMessage of DestroyBodyMessage
    | DestroyBodiesMessage of DestroyBodiesMessage
    | CreateBodyJointMessage of CreateBodyJointMessage
    | DestroyBodyJointMessage of DestroyBodyJointMessage
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
    | ApplyBodyLinearImpulseMessage of ApplyBodyLinearImpulseMessage
    | ApplyBodyAngularImpulseMessage of ApplyBodyAngularImpulseMessage
    | ApplyBodyForceMessage of ApplyBodyForceMessage
    | ApplyBodyTorqueMessage of ApplyBodyTorqueMessage
    | JumpBodyMessage of JumpBodyMessage
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

/// Miscellaneous physics operations.
[<RequireQualifiedAccess>]
module Physics =

    /// Convert a category mask to a value that represents collision categories.
    /// Examples -
    ///     * = -1
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either * or a binary number!
    let categorizeCollisionMask categoryMask =
        match categoryMask with
        | Constants.Physics.CollisionWildcard -> -1
        | _ -> Convert.ToInt32 (categoryMask, 2)

    /// Localize a primitive body shape to a specific size, typically used in tile maps.
    /// Non-primitive shapes are unaffected as they should be scaled independently.
    let rec localizePrimitiveBodyShape (size : Vector3) bodyShape =
        let scaleTranslation (scalar : Vector3) (transformOpt : Affine option) =
            match transformOpt with
            | Some transform -> Some { transform with Translation = transform.Translation * scalar }
            | None -> None
        match bodyShape with
        | EmptyShape -> EmptyShape
        | BoxShape boxShape -> BoxShape { boxShape with Size = Vector3.Multiply (size, boxShape.Size); TransformOpt = scaleTranslation size boxShape.TransformOpt }
        | SphereShape sphereShape -> SphereShape { sphereShape with Radius = size.X * sphereShape.Radius; TransformOpt = scaleTranslation size sphereShape.TransformOpt }
        | CapsuleShape capsuleShape -> CapsuleShape { capsuleShape with Height = size.Y * capsuleShape.Height; Radius = size.Y * capsuleShape.Radius; TransformOpt = scaleTranslation size capsuleShape.TransformOpt }
        | BoxRoundedShape boxRoundedShape -> BoxRoundedShape { boxRoundedShape with Size = Vector3.Multiply (size, boxRoundedShape.Size); Radius = size.X * boxRoundedShape.Radius; TransformOpt = scaleTranslation size boxRoundedShape.TransformOpt }
        | EdgeShape edgeShape -> EdgeShape { edgeShape with Start = edgeShape.Start * size; Stop = edgeShape.Stop * size; TransformOpt = scaleTranslation size edgeShape.TransformOpt }
        | ContourShape contourShape -> ContourShape { contourShape with Links = Array.map (fun vertex -> size * vertex) contourShape.Links; TransformOpt = scaleTranslation size contourShape.TransformOpt }
        | PointsShape pointsShape -> PointsShape { pointsShape with Points = Array.map (fun vertex -> size * vertex) pointsShape.Points; TransformOpt = scaleTranslation size pointsShape.TransformOpt }
        | GeometryShape _ as geometryShape -> geometryShape
        | StaticModelShape _ as staticModelShape -> staticModelShape
        | StaticModelSurfaceShape _ as staticModelSurfaceShape -> staticModelSurfaceShape
        | TerrainShape _ as terrainShape -> terrainShape
        | BodyShapes bodyShapes -> BodyShapes (List.map (localizePrimitiveBodyShape size) bodyShapes)