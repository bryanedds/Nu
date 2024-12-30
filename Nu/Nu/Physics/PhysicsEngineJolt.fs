// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open JoltPhysicsSharp
open Prime

type [<Struct>] private ContactEvent =
    | ContactAdded of BodyID : BodyID * Body2ID : BodyID * Normal : Vector3
    | ContactRemoved of BodyID : BodyID * Body2ID : BodyID

type [<ReferenceEquality>] PhysicsEngineJolt =
    private
        { PhysicsContext : PhysicsSystem
          JobSystem : JobSystemThreadPool
          ContactLock : obj
          ContactEvents : ContactEvent HashSet
          UnscaledPointsCached : Dictionary<UnscaledPointsKey, Vector3 array>
          CollisionsGround : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          CollisionsAll : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          Bodies : Dictionary<BodyId, BodyID>
          BodyUserData : Dictionary<BodyID, BodyId>
          Constraints : Dictionary<BodyJointId, TwoBodyConstraint>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          IntegrationMessages : IntegrationMessage List }

    static member private handlePenetration (bodyId : BodyId) (body2Id : BodyId) (normal : Vector3) physicsEngine =

        //
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = normal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        //
        let theta = normal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match physicsEngine.CollisionsGround.TryGetValue bodyId with
            | (true, collisions) -> collisions.[body2Id] <- normal
            | (false, _) -> physicsEngine.CollisionsGround.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, normal)]
            
        //
        match physicsEngine.CollisionsAll.TryGetValue bodyId with
        | (true, collisions) -> collisions.[body2Id] <- normal
        | (false, _) -> physicsEngine.CollisionsAll.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, normal)]

        //
        let normal = -normal
        let theta = normal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match physicsEngine.CollisionsGround.TryGetValue body2Id with
            | (true, collisions) -> collisions.[bodyId] <- normal
            | (false, _) -> physicsEngine.CollisionsGround.[bodyId] <- dictPlus HashIdentity.Structural [bodyId, normal]

        //
        match physicsEngine.CollisionsAll.TryGetValue body2Id with
        | (true, collisions) -> collisions.[bodyId] <- normal
        | (false, _) -> physicsEngine.CollisionsAll.[bodyId] <- dictPlus HashIdentity.Structural [bodyId, normal]

    static member private handleSeparation (bodyId : BodyId) (body2Id : BodyId) physicsEngine =

        //
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        //
        match physicsEngine.CollisionsGround.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.CollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

        //
        match physicsEngine.CollisionsAll.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.CollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

        //
        match physicsEngine.CollisionsGround.TryGetValue body2Id with
        | (true, collisions) ->
            collisions.Remove bodyId |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.CollisionsGround.Remove body2Id |> ignore<bool>
        | (false, _) -> ()

        //
        match physicsEngine.CollisionsAll.TryGetValue body2Id with
        | (true, collisions) ->
            collisions.Remove bodyId |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.CollisionsGround.Remove body2Id |> ignore<bool>
        | (false, _) -> ()

    static member private attachBoxShape (bodyProperties : BodyProperties) (boxShape : Nu.BoxShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let halfExtent = boxShape.Size * 0.5f
        let shapeSettings = new BoxShapeSettings (&halfExtent)
        let center = match boxShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
        let shapeSettings =
            match boxShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match boxShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = boxShape.Size.X * boxShape.Size.Y * boxShape.Size.Z
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachSphereShape (bodyProperties : BodyProperties) (sphereShape : Nu.SphereShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let shapeSettings = new SphereShapeSettings (sphereShape.Radius)
        let center = match sphereShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
        let shapeSettings =
            match sphereShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match sphereShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = 4.0f / 3.0f * MathF.PI * pown sphereShape.Radius 3
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachCapsuleShape (bodyProperties : BodyProperties) (capsuleShape : Nu.CapsuleShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let shapeSettings = new CapsuleShapeSettings (capsuleShape.Height * 0.5f, capsuleShape.Radius)
        let center = match capsuleShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
        let shapeSettings =
            match capsuleShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match capsuleShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = MathF.PI * pown capsuleShape.Radius 2 * (4.0f / 3.0f * capsuleShape.Radius * capsuleShape.Height)
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBoxRoundedShape (bodyProperties : BodyProperties) (boxRoundedShape : Nu.BoxRoundedShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        Log.info "Rounded box not yet implemented via PhysicsEngineJolt; creating a normal box instead."
        let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
        PhysicsEngineJolt.attachBoxShape bodyProperties boxShape scShapeSettings masses

    static member private attachBodyConvexHullShape (bodyProperties : BodyProperties) (pointsShape : Nu.PointsShape) (scShapeSettings : StaticCompoundShapeSettings) masses (physicsEngine : PhysicsEngineJolt) =
        let unscaledPointsKey = UnscaledPointsKey.make pointsShape.Points
        let (optimized, unscaledPoints) =
            match physicsEngine.UnscaledPointsCached.TryGetValue unscaledPointsKey with
            | (true, unscaledVertices) -> (true, unscaledVertices)
            | (false, _) -> (false, pointsShape.Points)
        let unscaledPoints =
            if not optimized then
                let hull = new BulletSharp.ConvexHullShape (unscaledPoints) // TODO: P0: attempt to find a way to remove dependency on Bullet here.
                hull.OptimizeConvexHull ()
                let unscaledPoints =
                    match hull.UnscaledPoints with
                    | null -> [|v3Zero|] // guarding against null
                    | unscaledPoints -> unscaledPoints |> Seq.map (fun p -> v3 p.X p.Y p.Z) |> Array.ofSeq
                physicsEngine.UnscaledPointsCached.Add (unscaledPointsKey, unscaledPoints)
                unscaledPoints
            else unscaledPoints
        let shapeSettings = new ConvexHullShapeSettings (unscaledPoints)
        let center = match pointsShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
        let (scale, shapeSettings) =
            match pointsShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale
                (shapeScale, new ScaledShapeSettings (shapeSettings, &shapeScale))
            | None -> (v3One, shapeSettings)
        let bodyShapeId = match pointsShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
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

    static member private attachBodyBvhTriangles (bodyProperties : BodyProperties) (geometryShape : GeometryShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let triangles =
            geometryShape.Vertices |>
            Seq.chunkBySize 3 |>
            Seq.map (fun t -> Triangle (&t.[0], &t.[1], &t.[2])) |>
            Array.ofSeq
        let shapeSettings = new MeshShapeSettings (triangles)
        shapeSettings.Sanitize ()
        let center = match geometryShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
        let (scale, shapeSettings) =
            match geometryShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale
                (shapeScale, new ScaledShapeSettings (shapeSettings, &shapeScale))
            | None -> (v3One, shapeSettings)
        let bodyShapeId = match geometryShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let box = box3 v3Zero ((Box3.Enclose geometryShape.Vertices).Size * scale)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachGeometryShape (bodyProperties : BodyProperties) (geometryShape : GeometryShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        if geometryShape.Convex then
            let pointsShape = { Points = geometryShape.Vertices; TransformOpt = geometryShape.TransformOpt; PropertiesOpt = geometryShape.PropertiesOpt }
            PhysicsEngineJolt.attachBodyConvexHullShape bodyProperties pointsShape scShapeSettings masses physicsEngine
        else PhysicsEngineJolt.attachBodyBvhTriangles bodyProperties geometryShape scShapeSettings masses

    // TODO: add some error logging.
    static member private attachStaticModelShape (bodyProperties : BodyProperties) (staticModelShape : StaticModelShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        match Metadata.tryGetStaticModelMetadata staticModelShape.StaticModel with
        | ValueSome staticModel ->
            Seq.fold (fun centerMassInertiaDisposes i ->
                let surface = staticModel.Surfaces.[i]
                let transform =
                    match staticModelShape.TransformOpt with
                    | Some transform ->
                        Affine.make
                            (transform.Translation.Transform surface.SurfaceMatrix)
                            (transform.Rotation * surface.SurfaceMatrix.Rotation)
                            (transform.Scale.Transform surface.SurfaceMatrix)
                    | None -> Affine.makeFromMatrix surface.SurfaceMatrix
                let staticModelSurfaceShape = { StaticModel = staticModelShape.StaticModel; SurfaceIndex = i; Convex = staticModelShape.Convex; TransformOpt = Some transform; PropertiesOpt = staticModelShape.PropertiesOpt }
                match Metadata.tryGetStaticModelMetadata staticModelSurfaceShape.StaticModel with
                | ValueSome staticModel ->
                    if  staticModelSurfaceShape.SurfaceIndex > -1 &&
                        staticModelSurfaceShape.SurfaceIndex < staticModel.Surfaces.Length then
                        let geometry = staticModel.Surfaces.[staticModelSurfaceShape.SurfaceIndex].PhysicallyBasedGeometry
                        let geometryShape = { Vertices = geometry.Vertices; Convex = staticModelSurfaceShape.Convex; TransformOpt = staticModelSurfaceShape.TransformOpt; PropertiesOpt = staticModelSurfaceShape.PropertiesOpt }
                        PhysicsEngineJolt.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
                    else centerMassInertiaDisposes
                | ValueNone -> centerMassInertiaDisposes)
                masses
                [0 .. dec staticModel.Surfaces.Length]
        | ValueNone -> masses

    // TODO: add some error logging.
    static member private attachStaticModelShapeSurface (bodyProperties : BodyProperties) (staticModelSurfaceShape : StaticModelSurfaceShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        match Metadata.tryGetStaticModelMetadata staticModelSurfaceShape.StaticModel with
        | ValueSome staticModel ->
            if  staticModelSurfaceShape.SurfaceIndex > -1 &&
                staticModelSurfaceShape.SurfaceIndex < staticModel.Surfaces.Length then
                let surface = staticModel.Surfaces.[staticModelSurfaceShape.SurfaceIndex]
                let geometry = surface.PhysicallyBasedGeometry
                let geometryShape = { Vertices = geometry.Vertices; Convex = staticModelSurfaceShape.Convex; TransformOpt = staticModelSurfaceShape.TransformOpt; PropertiesOpt = staticModelSurfaceShape.PropertiesOpt }
                PhysicsEngineJolt.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
            else masses
        | ValueNone -> masses

    static member private attachTerrainShape (bodyProperties : BodyProperties) (terrainShape : TerrainShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        match HeightMap.tryGetMetadata Metadata.tryGetFilePath terrainShape.Bounds v2One terrainShape.HeightMap with
        | ValueSome heightMapMetadata ->
            if heightMapMetadata.Resolution = terrainShape.Resolution then
                if terrainShape.Resolution.X = terrainShape.Resolution.Y then
                    let heights = Array.zeroCreate heightMapMetadata.HeightsNormalized.Length
                    for i in 0 .. dec heightMapMetadata.HeightsNormalized.Length do
                        heights.[i] <- heightMapMetadata.HeightsNormalized.[i] * terrainShape.Bounds.Height
                    let center = match terrainShape.TransformOpt with Some transform -> transform.Translation | None -> v3Zero
                    let size = v3 terrainShape.Bounds.Width terrainShape.Bounds.Height terrainShape.Bounds.Depth
                    let size = match terrainShape.TransformOpt with Some transform -> transform.Scale * size | None -> size
                    let offset = size * -0.5f
                    let tileSize = v3 (size.X / single (dec terrainShape.Resolution.X)) (size.Y / terrainShape.Bounds.Height) (size.Z / single (dec terrainShape.Resolution.Y))
                    let shapeSettings = new HeightFieldShapeSettings (heights.AsSpan (), &offset, &tileSize, terrainShape.Resolution.X)
                    let bodyShapeId = match terrainShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
                    scShapeSettings.AddShape (&center, &bodyProperties.Rotation, shapeSettings, uint bodyShapeId)
                    0.0f :: masses // infinite mass
                else
                    Log.error ("Jolt Physics does not support non-square terrain resolution " + scstring terrainShape.Resolution + ".")
                    masses
            else
                Log.error ("Terrain shape resolution mismatch.")
                masses
        | ValueNone -> masses

    static member private attachBodyShapes bodyProperties bodyShapes scShapeSettings masses physicsEngine =
        List.fold (fun masses bodyShape ->
            let masses' = PhysicsEngineJolt.attachBodyShape bodyProperties bodyShape scShapeSettings masses physicsEngine
            masses' @ masses)
            masses
            bodyShapes

    static member private attachBodyShape bodyProperties bodyShape scShapeSettings masses physicsEngine =
        match bodyShape with
        | EmptyShape -> masses
        | BoxShape boxShape -> PhysicsEngineJolt.attachBoxShape bodyProperties boxShape scShapeSettings masses
        | SphereShape sphereShape -> PhysicsEngineJolt.attachSphereShape bodyProperties sphereShape scShapeSettings masses
        | CapsuleShape capsuleShape -> PhysicsEngineJolt.attachCapsuleShape bodyProperties capsuleShape scShapeSettings masses
        | BoxRoundedShape boxRoundedShape -> PhysicsEngineJolt.attachBoxRoundedShape bodyProperties boxRoundedShape scShapeSettings masses
        | PointsShape pointsShape -> PhysicsEngineJolt.attachBodyConvexHullShape bodyProperties pointsShape scShapeSettings masses physicsEngine
        | GeometryShape geometryShape -> PhysicsEngineJolt.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
        | StaticModelShape staticModelShape -> PhysicsEngineJolt.attachStaticModelShape bodyProperties staticModelShape scShapeSettings masses physicsEngine
        | StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngineJolt.attachStaticModelShapeSurface bodyProperties staticModelSurfaceShape scShapeSettings masses physicsEngine
        | TerrainShape terrainShape -> PhysicsEngineJolt.attachTerrainShape bodyProperties terrainShape scShapeSettings masses
        | BodyShapes bodyShapes -> PhysicsEngineJolt.attachBodyShapes bodyProperties bodyShapes scShapeSettings masses physicsEngine

    static member private createBody3 (bodyId : BodyId) (bodyProperties : BodyProperties) (physicsEngine : PhysicsEngineJolt) =
        use scShapeSettings = new StaticCompoundShapeSettings ()
        let masses = PhysicsEngineJolt.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettings [] physicsEngine
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
        bodyCreationSettings.AllowedDOFs <-
            (if bodyProperties.AngularFactor.X <> 0.0f then AllowedDOFs.RotationX else enum<_> 0) |||
            (if bodyProperties.AngularFactor.Y <> 0.0f then AllowedDOFs.RotationY else enum<_> 0) |||
            (if bodyProperties.AngularFactor.Z <> 0.0f then AllowedDOFs.RotationZ else enum<_> 0) |||
            AllowedDOFs.TranslationX ||| AllowedDOFs.TranslationY ||| AllowedDOFs.TranslationZ // TODO: P1: consider exposing linear factors if Aether physics also supports it.
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
        body.SetUserData (uint64 bodyId.BodyIndex)
        physicsEngine.PhysicsContext.BodyInterface.AddBody (&body, if bodyProperties.Enabled then Activation.Activate else Activation.DontActivate)
        physicsEngine.BodyUserData.Add (body.ID, bodyId)
        physicsEngine.Bodies.Add (bodyId, body.ID)

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // attempt to create body
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        PhysicsEngineJolt.createBody3 bodyId bodyProperties physicsEngine

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngineJolt.destroyBodyJointInternal bodyJointId physicsEngine
                PhysicsEngineJolt.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter (fun (bodyProperties : BodyProperties) ->
            let createBodyMessage =
                { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyProperties = bodyProperties }
            PhysicsEngineJolt.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngineJolt.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy body
        match physicsEngine.Bodies.TryGetValue bodyId with
        | (true, bodyID) ->
            physicsEngine.Bodies.Remove bodyId |> ignore<bool>
            physicsEngine.BodyUserData.Remove bodyID |> ignore<bool>
            physicsEngine.PhysicsContext.BodyInterface.RemoveAndDestroyBody &bodyID
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            PhysicsEngineJolt.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    // TODO: P0: test if we need to manually wake bodies when adding joints to them.
    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =
        match bodyJointProperties.BodyJoint with
        | EmptyJoint -> ()
        | _ ->
            let bodyId = bodyJointProperties.BodyJointTarget
            let body2Id = bodyJointProperties.BodyJointTarget2
            match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
            | ((true, bodyID), (true, body2ID)) ->
                let constrainOpt =
                    match bodyJointProperties.BodyJoint with
                    | EmptyJoint ->
                        failwithumf () // already checked
                    | AngleJoint hingeJoint ->
                        let mutable constraintSettings = Unchecked.defaultof<HingeConstraintSettings>
                        constraintSettings.Point1 <- hingeJoint.Anchor
                        constraintSettings.Point2 <- hingeJoint.Anchor2
                        let mutable bodyLockWrite = Unchecked.defaultof<_>
                        let mutable body2LockWrite = Unchecked.defaultof<_>
                        try physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&bodyID, &bodyLockWrite) // NOTE: assuming that jolt needs write capabilities for these.
                            physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&body2ID, &body2LockWrite) // TODO: P0: send support request to JoltPhysicsSharp maintainer to expose BodyLockMultiWrite so that we don't get a deadlock here if accessing from another thread!
                            let body = bodyLockWrite.Body
                            let body2 = body2LockWrite.Body
                            let constrain = constraintSettings.CreateConstraint (&body, &body2)
                            constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                            Some constrain
                        finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockWrite &bodyLockWrite
                    | DistanceJoint distanceJoint ->
                        let mutable constraintSettings = Unchecked.defaultof<DistanceConstraintSettings>
                        constraintSettings.Point1 <- distanceJoint.Anchor
                        constraintSettings.Point2 <- distanceJoint.Anchor2
                        constraintSettings.Space <- ConstraintSpace.LocalToBodyCOM
                        let mutable bodyLockWrite = Unchecked.defaultof<_>
                        let mutable body2LockWrite = Unchecked.defaultof<_>
                        try physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&bodyID, &bodyLockWrite) // NOTE: assuming that jolt needs write capabilities for these.
                            physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&body2ID, &body2LockWrite) // TODO: P0: send support request to JoltPhysicsSharp maintainer to expose BodyLockMultiWrite so that we don't get a deadlock here if accessing from another thread!
                            let body = bodyLockWrite.Body
                            let body2 = body2LockWrite.Body
                            let constrain = constraintSettings.CreateConstraint (&body, &body2)
                            constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                            Some constrain
                        finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockWrite &bodyLockWrite
                    | UserDefinedJoltJoint joltJoint ->
                        let mutable bodyLockWrite = Unchecked.defaultof<_>
                        let mutable body2LockWrite = Unchecked.defaultof<_>
                        try physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&bodyID, &bodyLockWrite) // NOTE: assuming that jolt needs write capabilities for these.
                            physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&body2ID, &body2LockWrite) // TODO: P0: send support request to JoltPhysicsSharp maintainer to expose BodyLockMultiWrite so that we don't get a deadlock here if accessing from another thread!
                            let body = bodyLockWrite.Body
                            let body2 = body2LockWrite.Body
                            let constrain = joltJoint.CreateBodyJoint body body2
                            constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                            Some constrain
                        finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockWrite &bodyLockWrite
                    | _ ->
                        Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for PhysicsEngine3d.")
                        None
                match constrainOpt with
                | Some constrain ->
                    physicsEngine.PhysicsContext.AddConstraint constrain
                    if physicsEngine.Constraints.TryAdd (bodyJointId, constrain)
                    then () // nothing to do
                    else Log.warn ("Could not add body joint for '" + scstring bodyJointId + "'.")
                | None -> ()
            | (_, _) -> ()

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =

        // log creation message
        for bodyTarget in [createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) -> messages.Add createBodyJointMessage
            | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        PhysicsEngineJolt.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine

    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.Constraints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.Constraints.Remove bodyJointId |> ignore
            physicsEngine.PhysicsContext.RemoveConstraint joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message
        for bodyTarget in [destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2] do
            match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
            | (true, messages) ->
                messages.RemoveAll (fun message ->
                    message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                    message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex) |>
                ignore<int>
            | (false, _) -> ()

        // attempt to destroy body joint
        PhysicsEngineJolt.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, bodyID) ->
            if setBodyEnabledMessage.Enabled
            then physicsEngine.PhysicsContext.BodyInterface.ActivateBody &bodyID
            else physicsEngine.PhysicsContext.BodyInterface.DeactivateBody &bodyID
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
        | (true, bodyID) ->
            physicsEngine.PhysicsContext.BodyInterface.SetPosition (&bodyID, &setBodyCenterMessage.Center, Activation.Activate) // force activation so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
        | (true, bodyID) ->
            physicsEngine.PhysicsContext.BodyInterface.SetRotation (&bodyID, &setBodyRotationMessage.Rotation, Activation.Activate) // force activation so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, bodyID) ->
            physicsEngine.PhysicsContext.BodyInterface.SetLinearVelocity (&bodyID, &setBodyLinearVelocityMessage.LinearVelocity)
        | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, bodyID) ->
            physicsEngine.PhysicsContext.BodyInterface.SetAngularVelocity (&bodyID, &setBodyAngularVelocityMessage.AngularVelocity)
        | (false, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, bodyID) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                let offset =
                    match applyBodyLinearImpulseMessage.OriginWorldOpt with
                    | Some originWorld -> physicsEngine.PhysicsContext.BodyInterface.GetPosition &bodyID - originWorld
                    | None -> v3Zero
                physicsEngine.PhysicsContext.BodyInterface.AddImpulse (&bodyID, &applyBodyLinearImpulseMessage.LinearImpulse, &offset)
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Jolt Physics.")
        | (false, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, bodyID) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddAngularImpulse (&bodyID, &applyBodyAngularImpulseMessage.AngularImpulse)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Jolt Physics.")
        | (false, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyForceMessage.BodyId with
        | (true, bodyID) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                let offset =
                    match applyBodyForceMessage.OriginWorldOpt with
                    | Some originWorld -> physicsEngine.PhysicsContext.BodyInterface.GetPosition &bodyID - originWorld
                    | None -> v3Zero
                physicsEngine.PhysicsContext.BodyInterface.AddForce (&bodyID, &applyBodyForceMessage.Force, &offset)
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Jolt Physics.")
        | (false, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, bodyID) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddTorque (&bodyID, &applyBodyTorqueMessage.Torque)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Jolt Physics.")
        | (false, _) -> ()

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        //match physicsEngine.KinematicCharacters.TryGetValue jumpBodyMessage.BodyId with
        //| (true, character) ->
        //    if jumpBodyMessage.CanJumpInAir || character.CharacterController.OnGround then
        //        character.CharacterController.JumpSpeed <- jumpBodyMessage.JumpSpeed
        //        character.CharacterController.Jump ()
        //| (false, _) -> ()
        ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngineJolt.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngineJolt.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngineJolt.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngineJolt.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> PhysicsEngineJolt.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> PhysicsEngineJolt.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngineJolt.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngineJolt.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngineJolt.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngineJolt.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngineJolt.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngineJolt.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngineJolt.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngineJolt.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngineJolt.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> PhysicsEngineJolt.jumpBody jumpBodyMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- gravity

    static member private createIntegrationMessages (physicsEngine : PhysicsEngineJolt) =

        let bodyInterface = physicsEngine.PhysicsContext.BodyInterface // OPTIMIZATION: cache property for efficiency.

        lock physicsEngine.ContactLock $ fun () ->
            for contactEvent in physicsEngine.ContactEvents do
                match contactEvent with
                | ContactAdded (bodyID, body2ID, normal) ->
                    let bodyId = physicsEngine.BodyUserData.[bodyID]
                    let body2Id = physicsEngine.BodyUserData.[body2ID]
                    PhysicsEngineJolt.handlePenetration bodyId body2Id normal physicsEngine
                    PhysicsEngineJolt.handlePenetration body2Id bodyId -normal physicsEngine
                | ContactRemoved (bodyID, body2ID) ->
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyId) ->
                        match physicsEngine.BodyUserData.TryGetValue body2ID with
                        | (true, body2Id) ->
                            PhysicsEngineJolt.handleSeparation bodyId body2Id physicsEngine
                            PhysicsEngineJolt.handleSeparation body2Id bodyId physicsEngine
                        | (false, _) -> ()
                    | (false, _) -> ()
            physicsEngine.ContactEvents.Clear ()

        for entry in physicsEngine.Bodies do
            let bodyId = entry.Key
            let bodyID = entry.Value
            if bodyInterface.IsActive &bodyID then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = bodyId
                          Center = bodyInterface.GetPosition &bodyID
                          Rotation = bodyInterface.GetRotation &bodyID
                          LinearVelocity = bodyInterface.GetLinearVelocity &bodyID
                          AngularVelocity = bodyInterface.GetAngularVelocity &bodyID }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

        (*
        // create collision entries
        let collisionsOld = physicsEngine.CollisionsFiltered
        physicsEngine.CollisionsFiltered <- SDictionary.make HashIdentity.Structural
        let numManifolds = physicsEngine.PhysicsContext.Dispatcher.NumManifolds
        for i in 0 .. dec numManifolds do

            // ensure at least ONE contact point is either intersecting or touching by checking distance.
            // this will filter out manifolds contacting only on the broadphase level according to -
            // https://github.com/timbeaudet/knowledge_base/blob/main/issues/bullet_contact_report_issue.md
            let manifold = physicsEngine.PhysicsContext.Dispatcher.GetManifoldByIndexInternal i
            let mutable intersecting = false
            let mutable j = 0
            while not intersecting && j < manifold.NumContacts do
                let pt = manifold.GetContactPoint j
                if pt.Distance <= 0.0f
                then intersecting <- true
                else j <- inc j
            if intersecting then

                // create non-ground collision entry if unfiltered
                let body0 = manifold.Body0
                let body1 = manifold.Body1
                let body0Source = (body0.UserObject :?> BodyUserObject).BodyId
                let body1Source = (body1.UserObject :?> BodyUserObject).BodyId
                let collisionKey = (body0Source, body1Source)
                let mutable normal = v3Zero
                let numContacts = manifold.NumContacts
                for j in 0 .. dec numContacts do
                    let contact = manifold.GetContactPoint j
                    normal <- normal - contact.NormalWorldOnB
                normal <- normal / single numContacts
                if  body0.UserIndex = 1 ||
                    body1.UserIndex = 1 then
                    physicsEngine.CollisionsFiltered.[collisionKey] <- normal // NOTE: incoming collision keys are not necessarily unique.

        // create gravitating body transform messages
        for bodyEntry in physicsEngine.BodiesGravitating do
            let (_, body) = bodyEntry.Value
            if body.IsActive then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = (body.UserObject :?> BodyUserObject).BodyId
                          Center = body.WorldTransform.Translation
                          Rotation = body.WorldTransform.Rotation
                          LinearVelocity = body.LinearVelocity
                          AngularVelocity = body.AngularVelocity }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

        // create kinematic character transform messages
        for characterEntry in physicsEngine.KinematicCharacters do
            let character = characterEntry.Value
            if character.Ghost.IsActive then
                let center = character.Ghost.WorldTransform.Translation
                let forward = character.Ghost.WorldTransform.Rotation.Forward
                let sign = if v3Up.Dot (forward.Cross character.Rotation.Forward) < 0.0f then 1.0f else -1.0f
                let angleBetweenOpt = forward.AngleBetween character.Rotation.Forward
                character.LinearVelocity <- center - character.Center
                character.AngularVelocity <- if Single.IsNaN angleBetweenOpt then v3Zero else v3 0.0f (angleBetweenOpt * sign) 0.0f
                character.Center <- center
                character.Rotation <- character.Ghost.WorldTransform.Rotation
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = (character.Ghost.UserObject :?> BodyUserObject).BodyId
                          Center = character.Center - character.CenterOffset
                          Rotation = character.Rotation
                          LinearVelocity = character.LinearVelocity
                          AngularVelocity = character.AngularVelocity }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage
        *)

    static member make (gravity : Vector3) =

        if not (Foundation.Init false) then
            Log.fail "Could not initialize Jolt Physics."

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
        physicsSystem.Gravity <- gravity

        let contactLock = obj ()
        let contactEvents = HashSet ()

        physicsSystem.add_OnContactValidate (fun _ _ _ _ _ ->
            lock contactLock $ fun () -> ValidateResult.AcceptContact) // TODO: P0: collision mask used here?

        physicsSystem.add_OnContactAdded (fun _ body body2 manifold _ ->
            let bodyID = body.ID
            let body2ID = body2.ID
            let normal = manifold.WorldSpaceNormal
            lock contactLock $ fun () -> contactEvents.Add (ContactAdded (bodyID, body2ID, normal)) |> ignore<bool>)

        physicsSystem.add_OnContactRemoved (fun _ subShapeIDPair ->
            let bodyID = subShapeIDPair.Body1ID
            let body2ID = subShapeIDPair.Body2ID
            lock contactLock $ fun () -> contactEvents.Add (ContactRemoved (bodyID, body2ID)) |> ignore<bool>)

        // TODO: P0: see if we need this to send awakeness to the engine.
        //physicsSystem.add_OnBodyActivated ???
        //physicsSystem.add_OnBodyDeactivated ???

        let mutable jobSystemConfig = JobSystemThreadPoolConfig ()
        jobSystemConfig.maxJobs <- uint Constants.Physics.Collision3dMaxJobs
        jobSystemConfig.maxBarriers <- uint Constants.Physics.Collision3dMaxBarriers
        jobSystemConfig.numThreads <- Constants.Physics.Collision3dNumThreads
        let jobSystem = new JobSystemThreadPool (&jobSystemConfig)

        { PhysicsContext = physicsSystem
          JobSystem = jobSystem
          ContactLock = contactLock
          ContactEvents = contactEvents
          UnscaledPointsCached = dictPlus UnscaledPointsKey.comparer []
          CollisionsGround = dictPlus HashIdentity.Structural []
          CollisionsAll = dictPlus HashIdentity.Structural []
          BodyUserData = dictPlus HashIdentity.Structural []
          Bodies = dictPlus HashIdentity.Structural []
          Constraints = dictPlus HashIdentity.Structural []
          CreateBodyJointMessages = dictPlus HashIdentity.Structural []
          IntegrationMessages = List () }

    static member cleanUp physicsEngine =
        physicsEngine.JobSystem.Dispose ()
        physicsEngine.PhysicsContext.Dispose ()
        Foundation.Shutdown ()

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            [|match physicsEngine.CollisionsAll.TryGetValue bodyId with
              | (true, collisions) -> for collision in collisions.Values do collision
              | (false, _) -> ()|]

        member physicsEngine.GetBodyLinearVelocity bodyId =
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, bodyID) -> physicsEngine.PhysicsContext.BodyInterface.GetLinearVelocity &bodyID
            | (false, _) -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyAngularVelocity bodyId =
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, bodyID) -> physicsEngine.PhysicsContext.BodyInterface.GetAngularVelocity &bodyID
            | (false, _) -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            match physicsEngine.CollisionsGround.TryGetValue bodyId with
            | (true, collisions) -> Array.ofSeq collisions.Values
            | (false, _) -> [||]

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            match groundNormals with
            | [||] -> None
            | _ ->
                let averageNormal = Array.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3.Cross (v3Forward, normal))
            | None -> None

        member physicsEngine.GetBodyGrounded bodyId =
            physicsEngine.CollisionsGround.ContainsKey bodyId

        member physicsEngine.RayCast (start, stop, collisionCategories, collisionMask, closestOnly) =
            //let mutable start = start
            //let mutable stop = stop
            //use rrc =
            //    if closestOnly
            //    then new ClosestRayResultCallback (&start, &stop) :> RayResultCallback
            //    else new AllHitsRayResultCallback (start, stop)
            //rrc.CollisionFilterGroup <- collisionCategories
            //rrc.CollisionFilterMask <- collisionMask
            //physicsEngine.PhysicsContext.RayTest (start, stop, rrc)
            //if rrc.HasHit then
            //    match rrc with
            //    | :? ClosestRayResultCallback as crrc ->
            //        [|  match crrc.CollisionObject.CollisionShape.UserObject with
            //            | :? BodyShapeIndex as shapeIndex ->
            //                BodyIntersection.make shapeIndex crrc.ClosestHitFraction crrc.HitPointWorld crrc.HitNormalWorld
            //            | _ -> failwithumf ()|]
            //    | :? AllHitsRayResultCallback as ahrrc ->
            //        [|for i in 0 .. dec ahrrc.CollisionObjects.Count do
            //            let collisionObject = ahrrc.CollisionObjects.[i]
            //            let hitPointWorld = ahrrc.HitPointWorld.[i]
            //            let hitNormalWorld = ahrrc.HitNormalWorld.[i]
            //            let hitFraction = ahrrc.HitFractions.[i]
            //            match collisionObject.CollisionShape.UserObject with
            //            | :? BodyShapeIndex as shapeIndex ->
            //                BodyIntersection.make shapeIndex hitFraction hitPointWorld hitNormalWorld
            //            | _ -> failwithumf ()|]
            //    | _ -> failwithumf ()
            //else [||]
            [||]

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngineJolt.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate stepTime =
            if not stepTime.IsZero then
                match physicsEngine.PhysicsContext.Update (stepTime.Seconds, Constants.Physics.Collision3dSteps, physicsEngine.JobSystem) with
                | PhysicsUpdateError.None ->
                    PhysicsEngineJolt.createIntegrationMessages physicsEngine
                    let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                    physicsEngine.IntegrationMessages.Clear ()
                    Some integrationMessages
                | error -> Log.warn ("Jolt Physics internal error: " + scstring error); None
            else None

        member physicsEngine.ClearInternal () =

            // compute whether the physics engine will be affected by this clear request
            let affected =
                physicsEngine.CollisionsGround.Count > 0 ||
                physicsEngine.Bodies.Count > 0 ||
                physicsEngine.Constraints.Count > 0 ||
                physicsEngine.CreateBodyJointMessages.Count > 0 ||
                physicsEngine.IntegrationMessages.Count > 0

            // clear collisions
            physicsEngine.CollisionsGround.Clear ()
            physicsEngine.CollisionsAll.Clear ()

            // clear intermediate maps
            physicsEngine.BodyUserData.Clear ()

            // destroy constraints
            for constrain in physicsEngine.Constraints.Values do
                physicsEngine.PhysicsContext.RemoveConstraint constrain
            physicsEngine.Constraints.Clear ()

            // destroy bodies
            for bodyID in physicsEngine.Bodies.Values do
                physicsEngine.PhysicsContext.BodyInterface.RemoveAndDestroyBody &bodyID
            physicsEngine.Bodies.Clear ()

            // clear joint creation messages
            physicsEngine.CreateBodyJointMessages.Clear ()

            // clear integration messages
            physicsEngine.IntegrationMessages.Clear ()

            // clear any in-flight contacts
            lock physicsEngine.ContactLock $ fun () -> physicsEngine.ContactEvents.Clear ()

            // fin
            affected

        member physicsEngine.CleanUp () =
            PhysicsEngineJolt.cleanUp physicsEngine