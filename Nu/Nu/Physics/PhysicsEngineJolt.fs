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
        { PhysicsContext : PhysicsSystem
          JobSystem : JobSystemThreadPool
          UnscaledPointsCached : Dictionary<UnscaledPointsKey, Vector3 array>
          CollisionsGround : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          ShapeSources : Dictionary<int, Simulant>
          BodySources : Dictionary<int, Simulant>
          IntegrationMessages : IntegrationMessage List }

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

        let mutable jobSystemConfig = JobSystemThreadPoolConfig ()
        jobSystemConfig.maxJobs <- uint Constants.Physics.Collision3dMaxJobs
        jobSystemConfig.maxBarriers <- uint Constants.Physics.Collision3dMaxBarriers
        jobSystemConfig.numThreads <- Constants.Physics.Collision3dNumThreads
        let jobSystem = new JobSystemThreadPool (&jobSystemConfig)

        let collisionsGround = dictPlus HashIdentity.Structural []
        let shapeSources = dictPlus HashIdentity.Structural []
        let bodySources = dictPlus<int, Simulant> HashIdentity.Structural []
        let integrationMessages = List ()

        physicsSystem.add_OnContactValidate (fun _ _ _ _ _ ->
            ValidateResult.AcceptContact) // TODO: collision mask used here?

        physicsSystem.add_OnContactAdded (fun _ body body2 manifold _ ->
            let bodyIndex = int (body.GetUserData ())
            let body2Index = int (body2.GetUserData ())
            let bodySource = bodySources.[bodyIndex]
            let body2Source = bodySources.[body2Index]
            let bodyId = { BodySource = bodySource; BodyIndex = bodyIndex }
            let body2Id = { BodySource = body2Source; BodyIndex = body2Index }
            PhysicsEngineJolt.handlePenetration bodyId body2Id manifold.WorldSpaceNormal collisionsGround integrationMessages
            PhysicsEngineJolt.handlePenetration body2Id bodyId -manifold.WorldSpaceNormal collisionsGround integrationMessages)

        physicsSystem.add_OnContactRemoved (fun _ subShapeIDPair ->
            let bodyIndex = let b1Id = subShapeIDPair.Body1ID in int (physicsSystem.BodyInterface.GetUserData &b1Id)
            let body2Index = let b2Id = subShapeIDPair.Body2ID in int (physicsSystem.BodyInterface.GetUserData &b2Id)
            let bodySource = bodySources.[bodyIndex]
            let body2Source = bodySources.[body2Index]
            let bodyId = { BodySource = bodySource; BodyIndex = bodyIndex }
            let body2Id = { BodySource = body2Source; BodyIndex = body2Index }
            PhysicsEngineJolt.handleSeparation bodyId body2Id collisionsGround integrationMessages
            PhysicsEngineJolt.handleSeparation body2Id bodyId collisionsGround integrationMessages)

        // TODO: P0: see if we need this to send awakeness to the engine.
        //physicsSystem.add_OnBodyActivated ???
        //physicsSystem.add_OnBodyDeactivated ???

        { PhysicsContext = physicsSystem
          JobSystem = jobSystem
          UnscaledPointsCached = dictPlus UnscaledPointsKey.comparer []
          CollisionsGround = collisionsGround
          ShapeSources = shapeSources
          BodySources = bodySources
          IntegrationMessages = integrationMessages }

    static member private handlePenetration (bodyId : BodyId) (body2Id : BodyId) (normal : Vector3) (collisionsGround : Dictionary<_, Dictionary<_, _>>) (integrationMessages : _ List) =

        //
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = normal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        integrationMessages.Add integrationMessage

        //
        let theta = normal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match collisionsGround.TryGetValue bodyId with
            | (true, collisions) -> collisions.Add (body2Id, normal)
            | (false, _) -> collisionsGround.Add (bodyId, dictPlus HashIdentity.Structural [(body2Id, normal)])
            
        //
        let normal = -normal
        let theta = normal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match collisionsGround.TryGetValue body2Id with
            | (true, collisions) -> collisions.Add (bodyId, normal)
            | (false, _) -> collisionsGround.Add (bodyId, dictPlus HashIdentity.Structural [bodyId, normal])

    static member private handleSeparation (bodyId : BodyId) (body2Id : BodyId) (collisionsGround : Dictionary<_, Dictionary<_, _>>) (integrationMessages : _ List) =

        //
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        integrationMessages.Add integrationMessage

        //
        match collisionsGround.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then collisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

        //
        match collisionsGround.TryGetValue body2Id with
        | (true, collisions) ->
            collisions.Remove bodyId |> ignore<bool>
            if collisions.Count = 0 then collisionsGround.Remove body2Id |> ignore<bool>
        | (false, _) -> ()

    static member private attachBoxShape bodySource (bodyProperties : BodyProperties) (boxShape : Nu.BoxShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
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

    static member private attachSphereShape bodySource (bodyProperties : BodyProperties) (sphereShape : Nu.SphereShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
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

    static member private attachCapsuleShape bodySource (bodyProperties : BodyProperties) (capsuleShape : Nu.CapsuleShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
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

    static member private attachBoxRoundedShape bodySource (bodyProperties : BodyProperties) (boxRoundedShape : Nu.BoxRoundedShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
        let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
        PhysicsEngineJolt.attachBoxShape bodySource bodyProperties boxShape scShapeSettings masses

    static member private attachBodyConvexHullShape bodySource (bodyProperties : BodyProperties) (pointsShape : Nu.PointsShape) (scShapeSettings : StaticCompoundShapeSettings) masses (physicsEngine : PhysicsEngineJolt) =
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

    static member private attachBodyBvhTriangles bodySource (bodyProperties : BodyProperties) (geometryShape : GeometryShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
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

    static member private attachGeometryShape bodySource (bodyProperties : BodyProperties) (geometryShape : GeometryShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        if geometryShape.Convex then
            let pointsShape = { Points = geometryShape.Vertices; TransformOpt = geometryShape.TransformOpt; PropertiesOpt = geometryShape.PropertiesOpt }
            PhysicsEngineJolt.attachBodyConvexHullShape bodySource bodyProperties pointsShape scShapeSettings masses physicsEngine
        else PhysicsEngineJolt.attachBodyBvhTriangles bodySource bodyProperties geometryShape scShapeSettings masses

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
        | PointsShape pointsShape -> PhysicsEngineJolt.attachBodyConvexHullShape bodySource bodyProperties pointsShape scShapeSettings masses physicsEngine
        | GeometryShape geometryShape -> PhysicsEngineJolt.attachGeometryShape bodySource bodyProperties geometryShape scShapeSettings masses physicsEngine
        //| StaticModelShape staticModelShape -> PhysicsEngineJolt.attachStaticModelShape bodySource bodyProperties staticModelShape compoundShape centerMassInertiaDisposes physicsEngine
        //| StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngineJolt.attachStaticModelShapeSurface bodySource bodyProperties staticModelSurfaceShape compoundShape centerMassInertiaDisposes physicsEngine
        //| TerrainShape terrainShape -> PhysicsEngineJolt.attachTerrainShape bodySource bodyProperties terrainShape compoundShape centerMassInertiaDisposes
        | BodyShapes bodyShapes -> PhysicsEngineJolt.attachBodyShapes bodySource bodyProperties bodyShapes scShapeSettings masses physicsEngine

    static member private createBody (bodyId : BodyId) (bodyProperties : BodyProperties) (physicsEngine : PhysicsEngineJolt) =
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

    static member private createIntegrationMessages (physicsEngine : PhysicsEngineJolt) =

        // create collision entries
        let collisionsOld = physicsEngine.CollisionsFiltered
        physicsEngine.CollisionsFiltered <- SDictionary.make HashIdentity.Structural
        physicsEngine.CollisionsGround.Clear ()
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

    static member private tryIntegrate stepTime physicsEngine =
        physicsEngine.PhysicsContext.Update (stepTime, Constants.Physics.Collision3dSteps, physicsEngine.JobSystem)

    member physicsEngine.TryIntegrate stepTime =
        match PhysicsEngineJolt.tryIntegrate stepTime physicsEngine with
        | PhysicsUpdateError.None ->
            PhysicsEngineJolt.createIntegrationMessages physicsEngine
            let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            Some integrationMessages
        | error ->
            Log.error ("Jolt Physics internal error: " + scstring error)
            None