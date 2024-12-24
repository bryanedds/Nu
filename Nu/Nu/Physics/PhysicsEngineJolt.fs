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

    static member private attachBodyShapes tryGetAssetFilePath bodySource bodyProperties bodyShapes compoundShape centerMassInertiaDisposes physicsEngine =
        List.fold (fun centerMassInertiaDisposes bodyShape ->
            let centerMassInertiaDisposes' = PhysicsEngine3d.attachBodyShape tryGetAssetFilePath bodySource bodyProperties bodyShape compoundShape centerMassInertiaDisposes physicsEngine
            centerMassInertiaDisposes' @ centerMassInertiaDisposes)
            centerMassInertiaDisposes
            bodyShapes

    static member private attachBodyShape tryGetAssetFilePath bodySource bodyProperties bodyShape compoundShape centerMassInertiaDisposes physicsEngine =
        match bodyShape with
        | EmptyShape -> centerMassInertiaDisposes
        | BoxShape boxShape -> PhysicsEngine3d.attachBoxShape bodySource bodyProperties boxShape compoundShape centerMassInertiaDisposes
        | SphereShape sphereShape -> PhysicsEngine3d.attachSphereShape bodySource bodyProperties sphereShape compoundShape centerMassInertiaDisposes
        | CapsuleShape capsuleShape -> PhysicsEngine3d.attachCapsuleShape bodySource bodyProperties capsuleShape compoundShape centerMassInertiaDisposes
        | BoxRoundedShape boxRoundedShape -> PhysicsEngine3d.attachBoxRoundedShape bodySource bodyProperties boxRoundedShape compoundShape centerMassInertiaDisposes
        | PointsShape pointsShape -> PhysicsEngine3d.attachBodyConvexHull bodySource bodyProperties pointsShape compoundShape centerMassInertiaDisposes physicsEngine
        | GeometryShape geometryShape -> PhysicsEngine3d.attachGeometryShape bodySource bodyProperties geometryShape compoundShape centerMassInertiaDisposes physicsEngine
        | StaticModelShape staticModelShape -> PhysicsEngine3d.attachStaticModelShape bodySource bodyProperties staticModelShape compoundShape centerMassInertiaDisposes physicsEngine
        | StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngine3d.attachStaticModelShapeSurface bodySource bodyProperties staticModelSurfaceShape compoundShape centerMassInertiaDisposes physicsEngine
        | TerrainShape terrainShape -> PhysicsEngine3d.attachTerrainShape tryGetAssetFilePath bodySource bodyProperties terrainShape compoundShape centerMassInertiaDisposes
        | BodyShapes bodyShapes -> PhysicsEngine3d.attachBodyShapes tryGetAssetFilePath bodySource bodyProperties bodyShapes compoundShape centerMassInertiaDisposes physicsEngine

    static member private createBody (bodyId : BodyId) (bodyProperties : BodyProperties) physicsEngine =
        let shape = StaticCompoundShape 

        let mutable bodyCreationSettings = new BodyCreationSettings ()
        bodyCreationSettings.AllowSleeping <- bodyProperties.SleepingAllowed
        bodyCreationSettings.Position <- bodyProperties.Center
        bodyCreationSettings.Rotation <- bodyProperties.Rotation
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
        match bodyProperties.Substance with
        | Mass mass ->
            let massProperties = MassProperties ()
            massProperties.ScaleToMass mass
            bodyCreationSettings.MassPropertiesOverride <- massProperties
        | Density density ->
            let massProperties = MassProperties ()
            massProperties.SetMassAndInertiaOfSolidBox ((), density)
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
        bodyCreationSettings.ObjectLayer <- uint16 bodyProperties.CollisionCategories
        // TODO: P0: implement CollisionMask.
        bodyCreationSettings.IsSensor <- bodyProperties.Sensor
        let body = physicsEngine.PhysicsContext.BodyInterface.CreateBody bodyCreationSettings
        physicsEngine.PhysicsContext.BodyInterface.AddBody (&body, if bodyProperties.Enabled then Activation.Activate else Activation.DontActivate)
