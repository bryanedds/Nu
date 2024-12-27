// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Linq
open System.Numerics
open System.Runtime.InteropServices
open JoltPhysicsSharp
open Prime

type [<ReferenceEquality>] PhysicsEngineJolt =
    private
        { PhysicsContext : PhysicsSystem }

    static member make () =

        let objectLayerNonMoving = 0us
        let broadPhaseLayerNonMoving = byte 0
        let objectLayerMoving = 1us
        let broadPhaseLayerMoving = byte 1

        // We use only 2 layers: one for non-moving objects and one for moving objects
        let objectLayerPairFilter = new ObjectLayerPairFilterTable (2u)
        objectLayerPairFilter.EnableCollision (objectLayerNonMoving, objectLayerMoving)
        objectLayerPairFilter.EnableCollision (objectLayerMoving, objectLayerMoving)

        // We use a 1-to-1 mapping between object layers and broadphase layers
        let broadPhaseLayerInterface = new BroadPhaseLayerInterfaceTable (2u, 2u)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (objectLayerNonMoving, broadPhaseLayerNonMoving)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (objectLayerMoving, broadPhaseLayerMoving)

        let objectVsBroadPhaseLayerFilter = new ObjectVsBroadPhaseLayerFilterTable (broadPhaseLayerInterface, 2u, objectLayerPairFilter, 2u)

        let mutable physicsSystemSettings = PhysicsSystemSettings ()
        physicsSystemSettings.ObjectLayerPairFilter <- objectLayerPairFilter
        physicsSystemSettings.BroadPhaseLayerInterface <- broadPhaseLayerInterface
        physicsSystemSettings.ObjectVsBroadPhaseLayerFilter <- objectVsBroadPhaseLayerFilter

        let physicsSystem = new PhysicsSystem (physicsSystemSettings)

        // ContactListener
        physicsSystem.OnContactValidate += OnContactValidate;
        physicsSystem.OnContactAdded += OnContactAdded;
        physicsSystem.OnContactPersisted += OnContactPersisted;
        physicsSystem.OnContactRemoved += OnContactRemoved;

        // BodyActivationListener
        physicsSystem.OnBodyActivated += OnBodyActivated;
        physicsSystem.OnBodyDeactivated += OnBodyDeactivated;

        { PhysicsContext = physicsContext }

    static member private handlePenetration physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) normal =
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; BodyShapeIndex = 0 }
              Normal = normal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private handleSeparation physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) =
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private attachBoxShape bodySource (bodyProperties : BodyProperties) (boxShape : Nu.BoxShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let halfExtent = boxShape.Size * 0.5f
        let boxShapeSettings = new BoxShapeSettings (&halfExtent)
        let center =
            match boxShape.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let shapeSettings =
            match boxShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (boxShapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (boxShapeSettings, &bodyProperties.Scale)
            | None -> boxShapeSettings
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyProperties.BodyIndex)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = boxShape.Size.X * boxShape.Size.Y * boxShape.Size.Z
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : Nu.SphereShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let sphereShapeSettings = new SphereShapeSettings (sphereShape.Radius)
        let center =
            match sphereShape.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let shapeSettings =
            match sphereShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (sphereShapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (sphereShapeSettings, &bodyProperties.Scale)
            | None -> sphereShapeSettings
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyProperties.BodyIndex)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = 4.0f / 3.0f * MathF.PI * pown sphereShape.Radius 3
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : Nu.CapsuleShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let capsuleShapeSettings = new CapsuleShapeSettings (capsuleShape.Height * 0.5f, capsuleShape.Radius)
        let center =
            match capsuleShape.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let capsuleSettings =
            match capsuleShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (capsuleShapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (capsuleShapeSettings, &bodyProperties.Scale)
            | None -> capsuleShapeSettings
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, capsuleSettings, uint bodyProperties.BodyIndex)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = MathF.PI * pown capsuleShape.Radius 2 * (4.0f / 3.0f * capsuleShape.Radius * capsuleShape.Height)
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : Nu.BoxRoundedShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
        let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
        PhysicsEngineJolt.attachBoxShape bodySource bodyProperties boxShape scShapeSettings masses

    static member private attachBodyConvexHullShape bodySource (bodyProperties : BodyProperties) (pointsShape : Nu.PointsShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        let unscaledPointsKey = UnscaledPointsKey.make pointsShape.Points
        let (optimized, unscaledPoints) =
            match physicsEngine.UnscaledPointsCached.TryGetValue unscaledPointsKey with
            | (true, unscaledVertices) -> (true, unscaledVertices)
            | (false, _) -> (false, pointsShape.Points)
        let unscaledPoints =
            if not optimized then
                let hull = new BulletSharp.ConvexHullShape (unscaledPoints) // TODO: find a way 
                hull.OptimizeConvexHull ()
                let unscaledPoints =
                    match hull.UnscaledPoints with
                    | null -> [|v3Zero|] // guarding against null
                    | unscaledPoints -> unscaledPoints |> Seq.map (fun p -> v3 p.X p.Y p.Z) |> Array.ofSeq
                physicsEngine.UnscaledPointsCached.Add (unscaledPointsKey, unscaledPoints)
                unscaledPoints
            else unscaledPoints
        let convexHullShapeSettings = new ConvexHullShapeSettings (unscaledPoints)
        let center =
            match pointsShape.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let (scale, convexHullShapeSettings) =
            match pointsShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                (shapeScale, (new ScaledShapeSettings (convexHullShapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale
                (shapeScale, new ScaledShapeSettings (convexHullShapeSettings, &shapeScale))
            | None -> (v3One, convexHullShapeSettings)
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, convexHullShapeSettings, uint bodyProperties.BodyIndex)
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let box = box3 v3Zero ((Box3.Enclose pointsShape.Points).Size * scale)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBodyShapes bodySource bodyProperties bodyShapes compoundShape masses physicsEngine =
        List.fold (fun centerMassInertiaDisposes bodyShape ->
            let masses' = PhysicsEngineJolt.attachBodyShape bodySource bodyProperties bodyShape compoundShape centerMassInertiaDisposes physicsEngine
            masses' @ masses)
            masses
            bodyShapes

    static member private attachBodyShape bodySource bodyProperties bodyShape scShapeSettings masses physicsEngine =
        match bodyShape with
        | EmptyShape -> masses
        | BoxShape boxShape -> PhysicsEngineJolt.attachBoxShape bodySource bodyProperties boxShape scShapeSettings masses
        | SphereShape sphereShape -> PhysicsEngineJolt.attachSphereShape bodySource bodyProperties sphereShape scShapeSettings masses
        | CapsuleShape capsuleShape -> PhysicsEngineJolt.attachCapsuleShape bodySource bodyProperties capsuleShape scShapeSettings masses
        | BoxRoundedShape boxRoundedShape -> PhysicsEngineJolt.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape scShapeSettings masses
        | PointsShape pointsShape -> PhysicsEngineJolt.attachBodyConvexHull bodySource bodyProperties pointsShape compoundShape centerMassInertiaDisposes physicsEngine
        //| GeometryShape geometryShape -> PhysicsEngineJolt.attachGeometryShape bodySource bodyProperties geometryShape compoundShape centerMassInertiaDisposes physicsEngine
        //| StaticModelShape staticModelShape -> PhysicsEngineJolt.attachStaticModelShape bodySource bodyProperties staticModelShape compoundShape centerMassInertiaDisposes physicsEngine
        //| StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngineJolt.attachStaticModelShapeSurface bodySource bodyProperties staticModelSurfaceShape compoundShape centerMassInertiaDisposes physicsEngine
        //| TerrainShape terrainShape -> PhysicsEngineJolt.attachTerrainShape bodySource bodyProperties terrainShape compoundShape centerMassInertiaDisposes
        | BodyShapes bodyShapes -> PhysicsEngineJolt.attachBodyShapes bodySource bodyProperties bodyShapes scShapeSettings masses physicsEngine

    static member private createBody (bodyId : BodyId) (bodyProperties : BodyProperties) physicsEngine =
        use scShapeSettings = new StaticCompoundShapeSettings ()
        let masses = PhysicsEngineJolt.attachBodyShape bodyId.BodySource bodyProperties bodyProperties.BodyShape scShapeSettings [] physicsEngine
        let mass = List.sum masses
        let motionType =
            match bodyProperties.BodyType with
            | Static -> MotionType.Static
            | Kinematic -> MotionType.Kinematic
            | KinematicCharacter -> MotionType.Kinematic // TODO: P0: implement character physics.
            | Dynamic -> MotionType.Dynamic
            | DynamicCharacter -> MotionType.Dynamic // TODO: P0: implement character physics.
        let mutable bodyCreationSettings = new BodyCreationSettings (scShapeSettings, &bodyProperties.Center, &bodyProperties.Rotation, motionType, uint16 bodyProperties.CollisionCategories)
        bodyCreationSettings.AllowSleeping <- bodyProperties.SleepingAllowed
        bodyCreationSettings.Friction <- bodyProperties.Friction
        bodyCreationSettings.Restitution <- bodyProperties.Restitution
        bodyCreationSettings.LinearVelocity <- bodyProperties.LinearVelocity
        bodyCreationSettings.LinearDamping <- bodyProperties.LinearDamping
        bodyCreationSettings.AngularVelocity <- bodyProperties.AngularVelocity
        bodyCreationSettings.AngularDamping <- bodyProperties.AngularDamping
        bodyCreationSettings.AllowedDOFs <- // TODO: P1: consider exposing linear factors if Aether physics also supports it.
            (if bodyProperties.AngularFactor.X <> 0.0f then AllowedDOFs.RotationX else enum<_> 0) |||
            (if bodyProperties.AngularFactor.Y <> 0.0f then AllowedDOFs.RotationY else enum<_> 0) |||
            (if bodyProperties.AngularFactor.Z <> 0.0f then AllowedDOFs.RotationZ else enum<_> 0)
        let massProperties = MassProperties ()
        massProperties.ScaleToMass mass
        bodyCreationSettings.MassPropertiesOverride <- massProperties
        bodyCreationSettings.GravityFactor <- // TODO: P0: implement individual gravity direction?
            match bodyProperties.GravityOverride with
            | Some gravity -> gravity.Magnitude
            | None -> 1.0f
        // TODO: P0: implement CharacterProperties.
        bodyCreationSettings.MotionQuality <-
            match bodyProperties.CollisionDetection with
            | Discontinuous -> MotionQuality.Discrete
            | Continuous (_, _) -> MotionQuality.LinearCast
        // TODO: P0: implement CollisionMask.
        bodyCreationSettings.IsSensor <- bodyProperties.Sensor
        let body = physicsEngine.PhysicsContext.BodyInterface.CreateBody bodyCreationSettings
        physicsEngine.PhysicsContext.BodyInterface.AddBody (&body, if bodyProperties.Enabled then Activation.Activate else Activation.DontActivate)