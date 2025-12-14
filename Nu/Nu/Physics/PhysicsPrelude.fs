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
      RollingResistanceOpt : single option
      TangentialSpeedOpt : single option
      CollisionGroupOpt : int option
      CollisionCategoriesOpt : uint64 option
      CollisionMaskOpt : uint64 option
      SensorOpt : bool option }

    /// The empty body shape properties value.
    static member val empty =
        { BodyShapeIndex = 0
          FrictionOpt = None
          RestitutionOpt = None
          RollingResistanceOpt = None
          TangentialSpeedOpt = None
          CollisionGroupOpt = None
          CollisionCategoriesOpt = None
          CollisionMaskOpt = None
          SensorOpt = None }

    /// Check that the body shape properties are applicable for Jolt physics.
    static member validateUtilizationJolt properties =
        properties.FrictionOpt.IsNone &&
        properties.RestitutionOpt.IsNone &&
        properties.RollingResistanceOpt.IsNone &&
        properties.TangentialSpeedOpt.IsNone &&
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

/// The shape of a physics body capsule, where the radius is extrinsic to the height.
type CapsuleShape =
    { Height : single
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a physics body rounded box, where the radius is intrinsic to the box size.
type BoxRoundedShape =
    { Size : Vector3
      Radius : single
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }
      
/// The shape of a massless physics body in terms of one line segment.
/// Collision occurs at both its sides.
type EdgeShape =
    { Start : Vector3
      Stop : Vector3
      TransformOpt : Affine option
      PropertiesOpt : BodyShapeProperties option }

/// The shape of a massless physics body in terms of at least 4 points, each consecutive pair of points forming a link.
/// Collision occurs one-sided at the right hand side of each link (a counter-clockwise winding order orients the normal outwards
/// and a clockwise winding order orients the normal inwards). When closed, an additional link is implied between the last link and
/// the first. Otherwise, the first link and the last link provide no collision and are used to overlap another contour shape at its
/// second or second-to-last link. It is assumed that self-intersection does not occur, there is no validation against this.
/// It properly handles ghost collisions compared to multiple EdgeShapes: https://box2d.org/posts/2020/06/ghost-collisions/
/// Box2D calls this a ChainShape, but it's not a physical chain - it's a chain of edges.
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

/// The shape of a physics body in terms of triangulated vertices that are not localized e.g. for tile sizes in a tile map.
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

/// The properties specific to the modelling a character body with discrete stair-stepping and slope physics, more commonly used in
/// man-made, structured or grid-like environments with explicit stairs and slopes.
type [<SymbolicExpansion>] CharacterStairSteppingProperties =
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
    static member val defaultProperties =
        { CollisionPadding = 0.02f
          CollisionTolerance = 0.001f
          SlopeMax = Math.DegreesToRadians 45.0f
          StairStepUp = v3 0.0f 0.25f 0.0f
          StairStepDownStickToFloor = v3 0.0f -0.25f 0.0f
          StairStepDownExtra = v3Zero
          StairStepForwardTest = 0.15f
          StairStepForwardMin = 0.02f
          StairCosAngleForwardContact = cos (Math.DegreesToRadians 75.0f) }

/// Describes what shape to cast from the bottom of the character cylinder to the ground for step detection.
/// Circle diameter scalar and segment width scalar are multiplied with character width (diameter for capsules).
/// Note that a scalar of 1 or higher may collide with vertical walls that the character only touches.
type PogoShape =
    | PogoPoint
    | PogoCircle of diameterScalar : single
    | PogoSegment of widthScalar : single

/// The properties specific to the modelling a character body with continuous ground contact using a spring-damper system,
/// more commonly used in organic or dynamic environments, with a more natural feeling on uneven surfaces.
/// PogoRestLengthScalar is a multiplier of capsule cylinder height (TODO: generalize this?).
type [<SymbolicExpansion>] CharacterPogoSpringProperties =
    { PogoRestLengthScalar : single
      PogoHertz : single
      PogoDampingRatio : single
      PogoShape : PogoShape
      AdditionalSoftCollisionMask : uint64 }

    /// The default character properties.
    static member defaultProperties =
        { PogoRestLengthScalar = 0.3f
          PogoHertz = 5.0f
          PogoDampingRatio = 0.8f
          PogoShape = PogoSegment 0.9f
          AdditionalSoftCollisionMask = 0UL }
          
/// The properties specific to the utilization of the character body types.
type CharacterProperties =
    | StairStepping of CharacterStairSteppingProperties
    | PogoSpring of CharacterPogoSpringProperties

/// The properties needed to describe the vehicle aspects of a body.
type VehicleProperties =
    | VehiclePropertiesAbsent
    | VehiclePropertiesBox2d
    | VehiclePropertiesJolt of JoltPhysicsSharp.VehicleConstraintSettings
    
/// Describes the gravitational property of a body.
type Gravity =
    | GravityWorld
    | GravityOverride of Vector3
    | GravityScale of single
    | GravityIgnore

    /// Compute local gravity based on the given world gravity.
    static member localize gravityWorld gravity =
        match gravity with
        | GravityWorld -> gravityWorld
        | GravityOverride gravity -> gravity
        | GravityScale scale -> gravityWorld * scale
        | GravityIgnore -> v3Zero

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
      RollingResistance : single
      LinearVelocity : Vector3
      LinearConveyorVelocity : Vector3
      LinearDamping : single
      AngularVelocity : Vector3
      AngularConveyorVelocity : Vector3
      AngularDamping : single
      AngularFactor : Vector3
      Substance : Substance
      Gravity : Gravity
      CharacterProperties : CharacterProperties
      CharacterSoftCollisionPushLimitOpt : single option
      VehicleProperties : VehicleProperties
      CollisionDetection : CollisionDetection
      CollisionGroup : int
      CollisionCategories : uint64
      CollisionMask : uint64
      Sensor : bool
      BodyIndex : int }

    member this.HasSensors =
        this.Sensor || this.BodyShape.HasSensors

/// Allows users to create their own two-body Aether joints.
type AetherBodyJoint =
    { CreateBodyJoint : (single -> single) -> (Vector3 -> nkast.Aether.Physics2D.Common.Vector2) -> nkast.Aether.Physics2D.Dynamics.Body -> nkast.Aether.Physics2D.Dynamics.Body -> nkast.Aether.Physics2D.Dynamics.Joints.Joint }

/// Allows users to create their own two-body Box2D.NET joints.
type Box2dNetBodyJoint =
    { CreateBodyJoint : (single -> single) -> (Vector3 -> Box2D.NET.B2Vec2) -> Box2D.NET.B2BodyId -> Box2D.NET.B2BodyId -> Box2D.NET.B2WorldId -> Box2D.NET.B2JointId }

/// Allows users to create their own two-body Jolt joints.
type JoltBodyJoint =
    { CreateBodyJoint : JoltPhysicsSharp.Body -> JoltPhysicsSharp.Body -> JoltPhysicsSharp.TwoBodyConstraint }

/// A joint on physics bodies.
/// Because physics joints don't generalize well across 2D and 3D - or even across different 3D physics engines, we're
/// currently only providing joint creation via the user-defined cases.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type BodyJoint =
    
    /// The empty joint.
    | EmptyJoint
    
    /// In Aether, all 2D joints must be between two bodies. Even for attaching a body to a fixed world position,
    /// the target position must be represented by a body but it can be without shapes. Therefore, one-body 2D joints do not exist.
    | AetherBodyJoint of AetherBodyJoint
    
    /// In Box2D.NET, all 2D joints must be between two bodies. Even for attaching a body to a fixed world position,
    /// the target position must be represented by a body but it can be without shapes. Therefore, one-body 2D joints do not exist.
    | Box2dNetBodyJoint of Box2dNetBodyJoint
    
    /// According to https://jrouwe.github.io/JoltPhysics/class_constraint.html, Jolt Physics constraints are either vehicle or two-body.
    /// Vehicle constraints are not represented as body joints in Nu. Therefore, one-body 3D joints also do not exist.
    | JoltBodyJoint of JoltBodyJoint

/// Describes the universal properties of a body joint.
type BodyJointProperties =
    { BodyJoint : BodyJoint
      BodyJointTarget : BodyId
      BodyJointTarget2 : BodyId
      BodyJointEnabled : bool
      BreakingPointOpt : single option
      Broken : bool
      CollideConnected : bool
      BodyJointIndex : int }

/// Describes a particle used in a fluid simulation.
type [<Struct>] FluidParticle =
    { FluidParticlePosition : Vector3
      FluidParticleVelocity : Vector3
      FluidParticleConfig : string }

/// Describes a collision between a fluid particle and a rigid body.
type [<Struct>] FluidCollision =
    { FluidCollider : FluidParticle
      FluidCollidee : BodyShapeIndex
      Nearest : Vector3
      Normal : Vector3 }

// NOTE: sfml-box2d-fluid is under rewrite, it is not recommended to tune for these parameters as a future update would change things up.
type [<SymbolicExpansion>] FluidParticleConfig = // see sfml-box2d-fluid: Game.cpp, Game::InitFluid
    { Radius : single
      Density : single
      Friction : single
      Restitution : single
      LinearDamping : single
      Impact : single
      ForceMultiplier : single
      ForceSurface : single
      ForceAdhesion : single
      ShearViscosity : single
      Viscosity : single
      ViscosityLeave : single
      MaxGetForce : single
      MaxForce : single
      MinDensity : single
      MaxDensity : single
      ForceDamping : single
      SurfaceWithOther : bool
      CollisionCategories : uint64
      CollisionMask : uint64
      Gravity : Gravity }
    // see sfml-box2d-fluid: GameObjects.h, struct ParticleConfig
    /// Use this as default
    static let scalingFactor = 640.0f / 2400.0f // HACK: sfml-box2d-fluid is in 2400×1350 while Nu is in 640×360, this scaling brings more appropriate behavior
    static member val waterProperties =
        { Radius = 4.f
          Density = 2.5f
          Friction = 0.0f
          Restitution = 0.01f
          LinearDamping = 0.f
          Impact = 5.f
          ForceMultiplier = 45000.f * scalingFactor
          ForceSurface = 50.f * scalingFactor
          ForceAdhesion = 0.f * scalingFactor
          ShearViscosity = 15.f
          Viscosity = 0.f
          ViscosityLeave = 0.f
          MaxGetForce = 1000.f * scalingFactor
          MaxForce = 187500.f * scalingFactor
          MinDensity = 0.25f
          MaxDensity = 3.f
          ForceDamping = 0.0025f * scalingFactor
          SurfaceWithOther = true
          CollisionCategories = Box2D.NET.B2Constants.B2_DEFAULT_CATEGORY_BITS
          CollisionMask = Box2D.NET.B2Constants.B2_DEFAULT_MASK_BITS
          Gravity = GravityWorld }
    static member val sandProperties =
        { FluidParticleConfig.waterProperties with
            // see sfml-box2d-fluid: Game.cpp, Game::LoadResources
            Density = 5.5f
            Friction = 0.75f
            Restitution = 0.025f
            // see sfml-box2d-fluid: Game.cpp, Game::InitFluid
            ForceMultiplier = 50000.f * scalingFactor
            ForceSurface = 50.f * scalingFactor
            Viscosity = 60.f * scalingFactor
            ViscosityLeave = 0.5f * scalingFactor
            ShearViscosity = 200.f }
    static member val gasProperties =
        { FluidParticleConfig.waterProperties with
            // see sfml-box2d-fluid: Game.cpp, Game::LoadResources
            Density = 0.5f
            Friction = 0.05f
            Restitution = 0.025f
            Gravity = GravityScale -0.25f
            // see sfml-box2d-fluid: Game.cpp, Game::InitFluid
            ForceMultiplier = 10000.f * scalingFactor
            ForceSurface = 10.f * scalingFactor / 10.0f // HACK: for some reason, extra scaling is needed here
            Viscosity = 20.f * scalingFactor / 10.0f
            MinDensity = 1.f
            MaxDensity = 1.f
            MaxForce = 43750.f * scalingFactor / 10.0f }
    static member val oilProperties =
        { FluidParticleConfig.waterProperties with
            // see sfml-box2d-fluid: Game.cpp, Game::LoadResources
            Density = 1.5f
            Friction = 0.f
            // see sfml-box2d-fluid: Game.cpp, Game::InitFluid
            ForceSurface = 1000.f * scalingFactor
            MaxDensity = 1.5f
            SurfaceWithOther = false }

/// Describes a particle-based 2d fluid emitter for Box2D.NET.
type FluidEmitterDescriptorBox2dNet =
    { Enabled : bool
      ParticlesMax : int
      CellSize : single
      Configs : Map<string, FluidParticleConfig>
      Gravity : Gravity
      SimulationBounds : Box2 }
    static member val defaultDescriptor =
        { Enabled = true
          ParticlesMax = 20000
          CellSize = 10.0f
          Configs = Map.ofList [("Water", FluidParticleConfig.waterProperties)
                                ("Sand", FluidParticleConfig.sandProperties)
                                ("Gas", FluidParticleConfig.gasProperties)
                                ("Oil", FluidParticleConfig.oilProperties)]
          Gravity = GravityWorld
          SimulationBounds = Box2 (-100.f, -100.f, 100.f, 100.f) }

/// Describes a particle-based 2d fluid emitter for Aether.
type FluidEmitterDescriptorAether =
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
      Gravity : Gravity
      Configs : Map<string, Gravity> } // No support for other properties

/// Describes a particle-based fluid emitter.
type FluidEmitterDescriptor =
    | FluidEmitterDescriptorAether of FluidEmitterDescriptorAether
    | FluidEmitterDescriptorBox2dNet of FluidEmitterDescriptorBox2dNet
    | FluidEmitterDescriptorJolt

/// Miscellaneous physics operations.
[<RequireQualifiedAccess>]
module Physics =

    /// Convert a category mask to a value that represents collision categories.
    /// Examples -
    ///     * = MaxValue
    ///     0 = 0
    ///     1 = 1
    ///     10 = 2
    ///     2 = ERROR - input must be either * or a binary number!
    let categorizeCollisionMask categoryMask =
        match categoryMask with
        | Constants.Physics.CollisionWildcard -> UInt64.MaxValue
        | _ -> Convert.ToUInt64 (categoryMask, 2)

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
        // NOTE: localization does not apply to 3D bodies.
        | StaticModelShape _ as staticModelShape -> staticModelShape
        | StaticModelSurfaceShape _ as staticModelSurfaceShape -> staticModelSurfaceShape
        | TerrainShape _ as terrainShape -> terrainShape
        | BodyShapes bodyShapes -> BodyShapes (List.map (localizePrimitiveBodyShape size) bodyShapes)