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

/// Identifies a joint in a physics engine.
type BodyJointId =
    { BodyJointSource : Simulant
      BodyJointIndex : int }

/// Identifies a fluid particle emitter.
type FluidEmitterId =
    { FluidEmitterSource : Simulant }

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

/// Describes a particle used in a fluid simulation.
type [<Struct>] FluidParticle =
    { FluidParticlePosition : Vector3
      FluidParticleVelocity : Vector3
      GravityOverride : Vector3 voption }

/// Describes a collision between a fluid particle and a rigid body.
type [<Struct>] FluidCollision =
    { FluidCollider : FluidParticle
      FluidCollidee : BodyShapeIndex
      Nearest : Vector3
      Normal : Vector3 }

/// Describes a particle-based 2d fluid emitter.
type FluidEmitterDescriptor2d =
    { ParticleRadius : single
      ParticleScale : single
      ParticlesMax : int
      NeighborsMax : int
      CollisionTestsMax : int
      CellSize : single
      Enabled : bool
      Viscosity : single
      LinearDamping : single
      SimulationBounds : Box2
      GravityOverride : Vector2 option }

/// Describes a particle-based fluid emitter.
type FluidEmitterDescriptor =
    | FluidEmitterDescriptor2d of FluidEmitterDescriptor2d
    | FluidEmitterDescriptor3d

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