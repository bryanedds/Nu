// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open JoltPhysicsSharp
open Prime

type [<Struct>] private BodyContactEvent =
    | BodyContactAdded of BodyID : BodyID * Body2ID : BodyID * ContactNormal : Vector3
    | BodyContactRemoved of BodyID : BodyID * Body2ID : BodyID

type [<Struct>] private CharacterContactEvent =
    | CharacterContactAdded of Character : CharacterVirtual * Character2Identifier : ValueEither<CharacterVirtual, BodyID> * SubShape2ID : SubShapeID * ContactPosition : Vector3 * ContactNormal : Vector3
    | CharacterContactRemoved of Character : CharacterVirtual * Character2Identifier : ValueEither<CharacterVirtual, BodyID> * SubShape2ID : SubShapeID

type [<Struct>] private CharacterContact =
    { CharacterIdentifier : ValueEither<CharacterVirtual, BodyID>
      ContactFresh : bool ref }

type [<Struct>] private CharacterUserData =
    { CharacterBodyId : BodyId
      CharacterContacts : Dictionary<SubShapeID, CharacterContact>
      CharacterProperties : CharacterProperties }

type [<ReferenceEquality>] PhysicsEngineJolt =
    private
        { PhysicsContext : PhysicsSystem
          JobSystem : JobSystemThreadPool
          UnscaledPointsCached : Dictionary<UnscaledPointsKey, Vector3 array>
          CharacterVsCharacterCollision : CharacterVsCharacterCollisionSimple
          CharacterContactLock : obj
          CharacterContactEvents : CharacterContactEvent HashSet
          CharacterCollisionsGround : Dictionary<CharacterVirtual, Dictionary<SubShapeID, Vector3>>
          CharacterCollisionsAll : Dictionary<CharacterVirtual, Dictionary<SubShapeID, Vector3>>
          CharacterUserData : Dictionary<CharacterVirtual, CharacterUserData>
          Characters : Dictionary<BodyId, CharacterVirtual>
          BodyContactLock : obj
          BodyContactEvents : BodyContactEvent HashSet
          BodyCollisionsGround : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          BodyCollisionsAll : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          BodyConstraints : Dictionary<BodyJointId, TwoBodyConstraint>
          BodyUserData : Dictionary<BodyID, BodyId>
          Bodies : Dictionary<BodyId, BodyID>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          IntegrationMessages : IntegrationMessage List }

    static member private handleBodyPenetration (bodyId : BodyId) (body2Id : BodyId) (contactNormal : Vector3) physicsEngine =

        //
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = contactNormal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        //
        let theta = contactNormal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match physicsEngine.BodyCollisionsGround.TryGetValue bodyId with
            | (true, collisions) -> collisions.[body2Id] <- contactNormal
            | (false, _) -> physicsEngine.BodyCollisionsGround.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, contactNormal)]
            
        //
        match physicsEngine.BodyCollisionsAll.TryGetValue bodyId with
        | (true, collisions) -> collisions.[body2Id] <- contactNormal
        | (false, _) -> physicsEngine.BodyCollisionsAll.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, contactNormal)]

    static member private handleBodySeparation (bodyId : BodyId) (body2Id : BodyId) physicsEngine =

        //
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        //
        match physicsEngine.BodyCollisionsGround.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.BodyCollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

        //
        match physicsEngine.BodyCollisionsAll.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.BodyCollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

    static member private handleCharacterPenetration (bodyId : BodyId) (body2Id : BodyId) (contactNormal : Vector3) physicsEngine =

        //
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = contactNormal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private handleCharacterSeparation (bodyId : BodyId) (body2Id : BodyId) physicsEngine =

        //
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private attachBoxShape (bodyProperties : BodyProperties) (boxShape : Nu.BoxShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let halfExtent = boxShape.Size * 0.5f
        let shapeSettings = new BoxShapeSettings (&halfExtent)
        let struct (center, rotation) =
            match boxShape.TransformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let shapeSettings =
            match boxShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match boxShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = boxShape.Size.X * boxShape.Size.Y * boxShape.Size.Z
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachSphereShape (bodyProperties : BodyProperties) (sphereShape : Nu.SphereShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let shapeSettings = new SphereShapeSettings (sphereShape.Radius)
        let struct (center, rotation) =
            match sphereShape.TransformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let shapeSettings =
            match sphereShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match sphereShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = 4.0f / 3.0f * MathF.PI * pown sphereShape.Radius 3
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachCapsuleShape (bodyProperties : BodyProperties) (capsuleShape : Nu.CapsuleShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let shapeSettings = new CapsuleShapeSettings (capsuleShape.Height * 0.5f, capsuleShape.Radius)
        let struct (center, rotation) =
            match capsuleShape.TransformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let shapeSettings =
            match capsuleShape.TransformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One -> new ScaledShapeSettings (shapeSettings, &bodyProperties.Scale)
            | None -> shapeSettings
        let bodyShapeId = match capsuleShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
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
                let hull = new BulletSharp.ConvexHullShape (unscaledPoints) // TODO: P1: attempt to find a way to remove dependency on Bullet here.
                hull.OptimizeConvexHull ()
                let unscaledPoints =
                    match hull.UnscaledPoints with
                    | null -> [|v3Zero|] // guarding against null
                    | unscaledPoints -> unscaledPoints |> Seq.map (fun p -> v3 p.X p.Y p.Z) |> Array.ofSeq
                physicsEngine.UnscaledPointsCached.Add (unscaledPointsKey, unscaledPoints)
                unscaledPoints
            else unscaledPoints
        let shapeSettings = new ConvexHullShapeSettings (unscaledPoints)
        let struct (center, rotation) =
            match pointsShape.TransformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
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
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
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
        let struct (center, rotation) =
            match geometryShape.TransformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
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
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
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
                    let struct (center, rotation) =
                        match terrainShape.TransformOpt with
                        | Some transform -> struct (transform.Translation, transform.Rotation)
                        | None -> (v3Zero, quatIdentity)
                    let size = v3 terrainShape.Bounds.Width terrainShape.Bounds.Height terrainShape.Bounds.Depth
                    let size = match terrainShape.TransformOpt with Some transform -> transform.Scale * size | None -> size
                    let offset = size * -0.5f
                    let tileSize = v3 (size.X / single (dec terrainShape.Resolution.X)) (size.Y / terrainShape.Bounds.Height) (size.Z / single (dec terrainShape.Resolution.Y))
                    let shapeSettings = new HeightFieldShapeSettings (heights.AsSpan (), &offset, &tileSize, terrainShape.Resolution.X)
                    let bodyShapeId = match terrainShape.PropertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
                    scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
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

        //
        use scShapeSettings = new StaticCompoundShapeSettings ()
        let masses = PhysicsEngineJolt.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettings [] physicsEngine
        let mass = List.sum masses
        let (motionType, isCharacter) =
            match bodyProperties.BodyType with
            | Static -> (MotionType.Static, false)
            | Kinematic -> (MotionType.Kinematic, false)
            | KinematicCharacter -> (MotionType.Kinematic, true)
            | Dynamic -> (MotionType.Dynamic, false)
            | DynamicCharacter -> (MotionType.Dynamic, true)
        if isCharacter then

            // character config
            let characterSettings = CharacterVirtualSettings ()
            characterSettings.CharacterPadding <- bodyProperties.CharacterProperties.CollisionPadding
            characterSettings.CollisionTolerance <- bodyProperties.CharacterProperties.CollisionTolerance
            characterSettings.EnhancedInternalEdgeRemoval <- true
            characterSettings.innerBodyLayer <- 1us // TODO: P0: use moving layer constant.
            characterSettings.Mass <- mass
            characterSettings.MaxSlopeAngle <- bodyProperties.CharacterProperties.SlopeMax
            characterSettings.Shape <- scShapeSettings.Create ()

            // inner shape config (must be set after Shape property)
            use scShapeSettingsInner = new StaticCompoundShapeSettings ()
            PhysicsEngineJolt.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettingsInner [] physicsEngine |> ignore<single list>
            characterSettings.InnerBodyShape <- scShapeSettingsInner.Create ()

            // create actual character
            let character = new CharacterVirtual (characterSettings, &bodyProperties.Center, &bodyProperties.Rotation, 0UL, physicsEngine.PhysicsContext)
            let innerBodyID = character.InnerBodyID
            physicsEngine.CharacterVsCharacterCollision.Add character
            character.SetCharacterVsCharacterCollision physicsEngine.CharacterVsCharacterCollision
            physicsEngine.BodyUserData.Add (innerBodyID, bodyId)
            physicsEngine.Bodies.Add (bodyId, innerBodyID)

            // set inner body physics properties
            // NOTE: dummied out since I don't think any of this does anything.
            //if bodyProperties.Enabled
            //then physicsEngine.PhysicsContext.BodyInterface.ActivateBody &innerBodyID
            //else physicsEngine.PhysicsContext.BodyInterface.DeactivateBody &innerBodyID
            //physicsEngine.PhysicsContext.BodyInterface.SetFriction (&innerBodyID, bodyProperties.Friction)
            //physicsEngine.PhysicsContext.BodyInterface.SetRestitution (&innerBodyID, bodyProperties.Restitution)
            //let motionQuality = match bodyProperties.CollisionDetection with Discontinuous -> MotionQuality.Discrete | Continuous (_, _) -> MotionQuality.LinearCast
            //physicsEngine.PhysicsContext.BodyInterface.SetMotionQuality (&innerBodyID, motionQuality)

            //
            character.add_OnContactValidate (fun _ _ _ ->
                lock physicsEngine.CharacterContactLock $ fun () ->
                    Bool8.True)

            //
            character.add_OnContactAdded (fun character body2ID subShape2ID contactPosition contactNormal _ ->
                let body2ID = body2ID
                let contactPosition = contactPosition
                let contactNormal = contactNormal
                lock physicsEngine.CharacterContactLock $ fun () ->
                    match physicsEngine.CharacterUserData.TryGetValue character with
                    | (true, characterUserData) ->
                        if not (characterUserData.CharacterContacts.ContainsKey subShape2ID) then

                            //
                            let theta = contactNormal.Dot Vector3.UnitY |> acos |> abs
                            if theta < Constants.Physics.GroundAngleMax then
                                match physicsEngine.CharacterCollisionsGround.TryGetValue character with
                                | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                                | (false, _) -> physicsEngine.CharacterCollisionsGround.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]

                            //
                            match physicsEngine.CharacterCollisionsAll.TryGetValue character with
                            | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                            | (false, _) -> physicsEngine.CharacterCollisionsAll.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]

                            //
                            let character2Identifier = ValueRight body2ID
                            characterUserData.CharacterContacts.Add (subShape2ID, { CharacterIdentifier = character2Identifier; ContactFresh = ref true })
                            
                            //
                            let contactPosition = v3 (single contactPosition.X) (single contactPosition.Y) (single contactPosition.Z)
                            physicsEngine.CharacterContactEvents.Add (CharacterContactAdded (character, character2Identifier, subShape2ID, contactPosition, contactNormal)) |> ignore<bool>

                    | (false, _) -> Log.warn "Potential logic error.")

            //
            character.add_OnCharacterContactValidate (fun _ _ _ ->
                lock physicsEngine.CharacterContactLock $ fun () ->
                    Bool8.True)

            //
            character.add_OnCharacterContactAdded (fun character character2 subShape2ID contactPosition contactNormal _ ->
                let contactPosition = contactPosition
                let contactNormal = contactNormal
                lock physicsEngine.CharacterContactLock $ fun () ->
                    match physicsEngine.CharacterUserData.TryGetValue character with
                    | (true, characterUserData) ->
                        if not (characterUserData.CharacterContacts.ContainsKey subShape2ID) then

                            //
                            let theta = contactNormal.Dot Vector3.UnitY |> acos |> abs
                            if theta < Constants.Physics.GroundAngleMax then
                                match physicsEngine.CharacterCollisionsGround.TryGetValue character with
                                | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                                | (false, _) -> physicsEngine.CharacterCollisionsGround.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]

                            //
                            match physicsEngine.CharacterCollisionsAll.TryGetValue character with
                            | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                            | (false, _) -> physicsEngine.CharacterCollisionsAll.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]

                            //
                            let character2Identifier = ValueLeft character2
                            characterUserData.CharacterContacts.Add (subShape2ID, { CharacterIdentifier = character2Identifier; ContactFresh = ref true })
                            
                            //
                            let contactPosition = v3 (single contactPosition.X) (single contactPosition.Y) (single contactPosition.Z)
                            physicsEngine.CharacterContactEvents.Add (CharacterContactAdded (character, character2Identifier, subShape2ID, contactPosition, contactNormal)) |> ignore<bool>

                    | (false, _) -> Log.warn "Potential logic error.")

            //
            let characterUserData = { CharacterBodyId = bodyId; CharacterContacts = dictPlus HashIdentity.Structural []; CharacterProperties = bodyProperties.CharacterProperties }
            physicsEngine.CharacterUserData.Add (character, characterUserData)
            physicsEngine.Characters.Add (bodyId, character)

        else

            //
            let layer = if bodyProperties.BodyType.IsStatic then 0us else 1us // TODO: P0: use layer constants.
            let mutable bodyCreationSettings = new BodyCreationSettings (scShapeSettings, &bodyProperties.Center, &bodyProperties.Rotation, motionType, layer)
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
            bodyCreationSettings.MotionQuality <-
                match bodyProperties.CollisionDetection with
                | Discontinuous -> MotionQuality.Discrete
                | Continuous (_, _) -> MotionQuality.LinearCast
            bodyCreationSettings.IsSensor <- bodyProperties.Sensor
            let body = physicsEngine.PhysicsContext.BodyInterface.CreateBody bodyCreationSettings
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

        // attempt to destroy character
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, character) ->
            let innerBodyID = character.InnerBodyID
            physicsEngine.Bodies.Remove bodyId |> ignore<bool>
            physicsEngine.BodyUserData.Remove character.InnerBodyID |> ignore<bool>
            physicsEngine.Characters.Remove bodyId |> ignore<bool>
            physicsEngine.CharacterUserData.Remove character |> ignore<bool>
            physicsEngine.CharacterVsCharacterCollision.Remove character
            physicsEngine.PhysicsContext.BodyInterface.RemoveAndDestroyBody &innerBodyID
            character.Dispose ()
        | (false, _) ->

            // otherwise, attempt to destroy non-character body
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
                        let constraintSettings = new HingeConstraintSettings ()
                        constraintSettings.Point1 <- hingeJoint.Anchor
                        constraintSettings.Point2 <- hingeJoint.Anchor2
                        use lockMultiWrite = physicsEngine.PhysicsContext.BodyLockInterface.LockMultiWrite ([|bodyID; body2ID|].AsSpan ()) // NOTE: assuming that jolt needs write capabilities for these.
                        let body = lockMultiWrite.GetBody 0u
                        let body2 = lockMultiWrite.GetBody 1u
                        let constrain = constraintSettings.CreateConstraint (&body, &body2)
                        constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                        Some constrain
                    | DistanceJoint distanceJoint ->
                        let constraintSettings = new DistanceConstraintSettings ()
                        constraintSettings.Point1 <- distanceJoint.Anchor
                        constraintSettings.Point2 <- distanceJoint.Anchor2
                        constraintSettings.Space <- ConstraintSpace.LocalToBodyCOM
                        use lockMultiWrite = physicsEngine.PhysicsContext.BodyLockInterface.LockMultiWrite ([|bodyID; body2ID|].AsSpan ()) // NOTE: assuming that jolt needs write capabilities for these.
                        let body = lockMultiWrite.GetBody 0u
                        let body2 = lockMultiWrite.GetBody 1u
                        let constrain = constraintSettings.CreateConstraint (&body, &body2)
                        constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                        Some constrain
                    | UserDefinedJoltJoint joltJoint ->
                        use lockMultiWrite = physicsEngine.PhysicsContext.BodyLockInterface.LockMultiWrite ([|bodyID; body2ID|].AsSpan ()) // NOTE: assuming that jolt needs write capabilities for these.
                        let body = lockMultiWrite.GetBody 0u
                        let body2 = lockMultiWrite.GetBody 1u
                        let constrain = joltJoint.CreateBodyJoint body body2
                        constrain.Enabled <- bodyJointProperties.BodyJointEnabled
                        Some constrain
                    | _ ->
                        Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for PhysicsEngine3d.")
                        None
                match constrainOpt with
                | Some constrain ->
                    physicsEngine.PhysicsContext.AddConstraint constrain
                    if physicsEngine.BodyConstraints.TryAdd (bodyJointId, constrain)
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
        match physicsEngine.BodyConstraints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.BodyConstraints.Remove bodyJointId |> ignore
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

    static member private tryGetBodyID bodyId physicsEngine =
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, character) -> ValueSome character.InnerBodyID
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, bodyID) -> ValueSome bodyID
            | (false, _) -> ValueNone

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID setBodyEnabledMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if setBodyEnabledMessage.Enabled
            then physicsEngine.PhysicsContext.BodyInterface.ActivateBody &bodyID
            else physicsEngine.PhysicsContext.BodyInterface.DeactivateBody &bodyID
        | ValueNone -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Characters.TryGetValue setBodyCenterMessage.BodyId with
        | (true, character) -> character.Position <- setBodyCenterMessage.Center
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue setBodyCenterMessage.BodyId with
            | (true, bodyID) ->
                physicsEngine.PhysicsContext.BodyInterface.SetPosition (&bodyID, &setBodyCenterMessage.Center, Activation.Activate) // force activation so that a transform message will be produced
            | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Characters.TryGetValue setBodyRotationMessage.BodyId with
        | (true, character) -> character.Rotation <- setBodyRotationMessage.Rotation
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue setBodyRotationMessage.BodyId with
            | (true, bodyID) ->
                physicsEngine.PhysicsContext.BodyInterface.SetRotation (&bodyID, &setBodyRotationMessage.Rotation, Activation.Activate) // force activation so that a transform message will be produced
            | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Characters.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, character) -> character.LinearVelocity <- setBodyLinearVelocityMessage.LinearVelocity
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue setBodyLinearVelocityMessage.BodyId with
            | (true, bodyID) ->
                physicsEngine.PhysicsContext.BodyInterface.SetLinearVelocity (&bodyID, &setBodyLinearVelocityMessage.LinearVelocity)
            | (false, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID setBodyAngularVelocityMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            physicsEngine.PhysicsContext.BodyInterface.SetAngularVelocity (&bodyID, &setBodyAngularVelocityMessage.AngularVelocity)
        | ValueNone -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID applyBodyLinearImpulseMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                let offset =
                    match applyBodyLinearImpulseMessage.OriginWorldOpt with
                    | Some originWorld -> physicsEngine.PhysicsContext.BodyInterface.GetPosition &bodyID - originWorld
                    | None -> v3Zero
                physicsEngine.PhysicsContext.BodyInterface.AddImpulse (&bodyID, &applyBodyLinearImpulseMessage.LinearImpulse, &offset)
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID applyBodyAngularImpulseMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddAngularImpulse (&bodyID, &applyBodyAngularImpulseMessage.AngularImpulse)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID applyBodyForceMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                let offset =
                    match applyBodyForceMessage.OriginWorldOpt with
                    | Some originWorld -> physicsEngine.PhysicsContext.BodyInterface.GetPosition &bodyID - originWorld
                    | None -> v3Zero
                physicsEngine.PhysicsContext.BodyInterface.AddForce (&bodyID, &applyBodyForceMessage.Force, &offset)
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match PhysicsEngineJolt.tryGetBodyID applyBodyTorqueMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddTorque (&bodyID, &applyBodyTorqueMessage.Torque)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

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

        lock physicsEngine.BodyContactLock $ fun () ->
            for contactEvent in physicsEngine.BodyContactEvents do
                match contactEvent with
                | BodyContactAdded (bodyID, body2ID, contactNormal) ->
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyId) ->
                        match physicsEngine.BodyUserData.TryGetValue body2ID with
                        | (true, body2Id) ->
                            PhysicsEngineJolt.handleBodyPenetration bodyId body2Id contactNormal physicsEngine
                            PhysicsEngineJolt.handleBodyPenetration body2Id bodyId -contactNormal physicsEngine
                        | (false, _) -> ()
                    | (false, _) -> ()
                | BodyContactRemoved (bodyID, body2ID) ->
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyId) ->
                        match physicsEngine.BodyUserData.TryGetValue body2ID with
                        | (true, body2Id) ->
                            PhysicsEngineJolt.handleBodySeparation bodyId body2Id physicsEngine
                            PhysicsEngineJolt.handleBodySeparation body2Id bodyId physicsEngine
                        | (false, _) -> ()
                    | (false, _) -> ()
            physicsEngine.BodyContactEvents.Clear ()

        lock physicsEngine.CharacterContactLock $ fun () ->
            for contactEvent in physicsEngine.CharacterContactEvents do
                match contactEvent with
                | CharacterContactAdded (character, character2Identifier, _, _, contactNormal) ->
                    let bodyId = (physicsEngine.CharacterUserData.[character]).CharacterBodyId
                    let body2IdOpt =
                        match character2Identifier with
                        | ValueLeft character ->
                            match physicsEngine.CharacterUserData.TryGetValue character with
                            | (true, characterUserData) -> ValueSome characterUserData.CharacterBodyId
                            | (false, _) -> ValueNone
                        | ValueRight body2ID ->
                            match physicsEngine.BodyUserData.TryGetValue body2ID with
                            | (true, bodyId) -> ValueSome bodyId
                            | (false, _) -> ValueNone
                    match body2IdOpt with
                    | ValueSome body2Id ->
                        PhysicsEngineJolt.handleCharacterPenetration bodyId body2Id contactNormal physicsEngine
                        PhysicsEngineJolt.handleCharacterPenetration body2Id bodyId -contactNormal physicsEngine
                    | ValueNone -> ()
                | CharacterContactRemoved (character, character2Identifier, _) ->
                    let bodyId = physicsEngine.CharacterUserData.[character].CharacterBodyId
                    let body2IdOpt =
                        match character2Identifier with
                        | ValueLeft character ->
                            match physicsEngine.CharacterUserData.TryGetValue character with
                            | (true, characterUserData) -> ValueSome characterUserData.CharacterBodyId
                            | (false, _) -> ValueNone
                        | ValueRight body2ID ->
                            match physicsEngine.BodyUserData.TryGetValue body2ID with
                            | (true, bodyId) -> ValueSome bodyId
                            | (false, _) -> ValueNone
                    match body2IdOpt with
                    | ValueSome body2Id ->
                        PhysicsEngineJolt.handleCharacterSeparation bodyId body2Id physicsEngine
                        PhysicsEngineJolt.handleCharacterSeparation body2Id bodyId physicsEngine
                    | ValueNone -> ()
            physicsEngine.CharacterContactEvents.Clear ()

        let bodyInterface = physicsEngine.PhysicsContext.BodyInterface // OPTIMIZATION: cache property for efficiency.
        for bodiesEntry in physicsEngine.Bodies do
            let bodyId = bodiesEntry.Key
            let bodyID = bodiesEntry.Value
            if bodyInterface.IsActive &bodyID then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = bodyId
                          Center = bodyInterface.GetPosition &bodyID
                          Rotation = bodyInterface.GetRotation &bodyID
                          LinearVelocity = bodyInterface.GetLinearVelocity &bodyID
                          AngularVelocity = bodyInterface.GetAngularVelocity &bodyID }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

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
            lock contactLock $ fun () -> ValidateResult.AcceptContact) // TODO: P0: use collision mask used here?

        physicsSystem.add_OnContactAdded (fun _ body body2 manifold _ ->
            let bodyID = body.ID
            let body2ID = body2.ID
            let contactNormal = manifold.WorldSpaceNormal
            lock contactLock $ fun () -> contactEvents.Add (BodyContactAdded (bodyID, body2ID, contactNormal)) |> ignore<bool>)

        physicsSystem.add_OnContactRemoved (fun _ subShapeIDPair ->
            let bodyID = subShapeIDPair.Body1ID
            let body2ID = subShapeIDPair.Body2ID
            lock contactLock $ fun () -> contactEvents.Add (BodyContactRemoved (bodyID, body2ID)) |> ignore<bool>)

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
          UnscaledPointsCached = dictPlus UnscaledPointsKey.comparer []
          CharacterVsCharacterCollision = new CharacterVsCharacterCollisionSimple ()
          CharacterContactLock = obj ()
          CharacterContactEvents = hashSetPlus HashIdentity.Structural []
          CharacterCollisionsGround = dictPlus HashIdentity.Structural []
          CharacterCollisionsAll = dictPlus HashIdentity.Structural []
          CharacterUserData = dictPlus HashIdentity.Structural []
          Characters = dictPlus HashIdentity.Structural []
          BodyContactLock = contactLock
          BodyContactEvents = contactEvents
          BodyCollisionsGround = dictPlus HashIdentity.Structural []
          BodyCollisionsAll = dictPlus HashIdentity.Structural []
          BodyConstraints = dictPlus HashIdentity.Structural []
          BodyUserData = dictPlus HashIdentity.Structural []
          Bodies = dictPlus HashIdentity.Structural []
          CreateBodyJointMessages = dictPlus HashIdentity.Structural []
          IntegrationMessages = List () }

    static member cleanUp physicsEngine =
        physicsEngine.JobSystem.Dispose ()
        physicsEngine.PhysicsContext.Dispose ()
        Foundation.Shutdown ()

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Characters.ContainsKey bodyId ||
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            [|match physicsEngine.Characters.TryGetValue bodyId with
              | (true, character) ->
                  match physicsEngine.CharacterCollisionsAll.TryGetValue character with
                  | (true, collisions) -> yield! collisions.Values
                  | (false, _) -> ()
              | (false, _) ->
                  match physicsEngine.BodyCollisionsAll.TryGetValue bodyId with
                  | (true, collisions) -> for collision in collisions.Values do collision
                  | (false, _) -> ()|]

        member physicsEngine.GetBodyLinearVelocity bodyId =
            match physicsEngine.Characters.TryGetValue bodyId with
            | (true, character) -> character.LinearVelocity
            | (false, _) ->
                match physicsEngine.Bodies.TryGetValue bodyId with
                | (true, bodyID) -> physicsEngine.PhysicsContext.BodyInterface.GetLinearVelocity &bodyID
                | (false, _) -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyAngularVelocity bodyId =
            match PhysicsEngineJolt.tryGetBodyID bodyId physicsEngine with
            | ValueSome bodyID -> physicsEngine.PhysicsContext.BodyInterface.GetAngularVelocity &bodyID
            | ValueNone -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            match physicsEngine.Characters.TryGetValue bodyId with
            | (true, character) ->
                match physicsEngine.CharacterCollisionsGround.TryGetValue character with
                | (true, collisions) -> Array.ofSeq collisions.Values
                | (false, _) -> [||]
            | (false, _) ->
                match physicsEngine.BodyCollisionsGround.TryGetValue bodyId with
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
            match physicsEngine.Characters.TryGetValue bodyId with
            | (true, character) -> physicsEngine.CharacterCollisionsGround.ContainsKey character
            | (false, _) -> physicsEngine.BodyCollisionsGround.ContainsKey bodyId

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

            // integrate only when time has passed
            if not stepTime.IsZero then

                // zero out character contacts
                lock physicsEngine.CharacterContactLock $ fun () ->
                    for characterUserData in physicsEngine.CharacterUserData.Values do
                        for characterContactEntry in characterUserData.CharacterContacts do
                            characterContactEntry.Value.ContactFresh.Value <- false

                // attempt to update
                match physicsEngine.PhysicsContext.Update (stepTime.Seconds, Constants.Physics.Collision3dSteps, physicsEngine.JobSystem) with
                | PhysicsUpdateError.None ->

                    // update characters
                    let characterLayer = 1us : ObjectLayer // TODO: P0: use layer constant.
                    for character in physicsEngine.Characters.Values do
                        let characterProperties = physicsEngine.CharacterUserData.[character].CharacterProperties
                        let mutable characterUpdateSettings =
                            ExtendedUpdateSettings
                                (WalkStairsStepUp = characterProperties.StairStepUp,
                                 StickToFloorStepDown = characterProperties.StairStepDownStickToFloor,
                                 WalkStairsStepDownExtra = characterProperties.StairStepDownExtra,
                                 WalkStairsStepForwardTest = characterProperties.StairStepForwardTest,
                                 WalkStairsMinStepForward = characterProperties.StairStepForwardMin,
                                 WalkStairsCosAngleForwardContact = characterProperties.StairCosAngleForwardContact)
                        character.LinearVelocity <-
                            character.LinearVelocity -
                            (character.LinearVelocity * (v3Dup characterProperties.TraversalDamping).WithY 0.0f * stepTime.Seconds)
                        if character.GroundState <> GroundState.OnGround then
                            let gravityForce = physicsEngine.PhysicsContext.Gravity * stepTime.Seconds
                            character.LinearVelocity <- character.LinearVelocity + gravityForce
                        character.ExtendedUpdate (stepTime.Seconds, characterUpdateSettings, &characterLayer, physicsEngine.PhysicsContext)
                        //character.Update (stepTime.Seconds, &characterLayer, physicsEngine.PhysicsContext)

                    // produce contact removed messages
                    lock physicsEngine.CharacterContactLock $ fun () ->
                        let contactKeysRemoved = List () // TODO: P0: cache this list.
                        for characterUserDataEntry in physicsEngine.CharacterUserData do
                            let character = characterUserDataEntry.Key
                            let characterUserData = characterUserDataEntry.Value
                            for characterContactEntry in characterUserData.CharacterContacts do
                                let subShape2ID = characterContactEntry.Key
                                let characterContact = characterContactEntry.Value
                                if not characterContact.ContactFresh.Value then

                                    //
                                    match physicsEngine.CharacterCollisionsGround.TryGetValue character with
                                    | (true, collisions) ->
                                        collisions.Remove subShape2ID |> ignore<bool>
                                        if collisions.Count = 0 then physicsEngine.CharacterCollisionsGround.Remove character |> ignore<bool>
                                    | (false, _) -> ()

                                    //
                                    match physicsEngine.CharacterCollisionsAll.TryGetValue character with
                                    | (true, collisions) ->
                                        collisions.Remove subShape2ID |> ignore<bool>
                                        if collisions.Count = 0 then physicsEngine.CharacterCollisionsGround.Remove character |> ignore<bool>
                                    | (false, _) -> ()

                                    //
                                    physicsEngine.CharacterContactEvents.Add (CharacterContactRemoved (character, characterContact.CharacterIdentifier, subShape2ID)) |> ignore<bool>

                                    //
                                    contactKeysRemoved.Add struct (character, subShape2ID)

                        //
                        for struct (character, contactKey) in contactKeysRemoved do
                            match physicsEngine.CharacterUserData.TryGetValue character with
                            | (true, characterUserData) -> characterUserData.CharacterContacts.Remove contactKey |> ignore<bool>
                            | (false, _) -> Log.warn "Potential logic error."

                    // create integration messages
                    PhysicsEngineJolt.createIntegrationMessages physicsEngine
                    let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                    physicsEngine.IntegrationMessages.Clear ()
                    Some integrationMessages

                // some manner of jolt error
                // TODO: P0: attempt to increase jolt pool sizes automatically when encountering a related error?
                | error -> Log.warn ("Jolt Physics internal error: " + scstring error); None

            // no time passed
            else None

        member physicsEngine.ClearInternal () =

            // compute whether the physics engine will be affected by this clear request
            let affected =
                physicsEngine.BodyConstraints.Count > 0 ||
                physicsEngine.Bodies.Count > 0 ||
                physicsEngine.CreateBodyJointMessages.Count > 0 ||
                physicsEngine.Characters.Count > 0 ||
                physicsEngine.IntegrationMessages.Count > 0

            // clear any in-flight character contacts
            lock physicsEngine.CharacterContactLock $ fun () ->
                physicsEngine.CharacterContactEvents.Clear ()

            // clear character collision tracking
            physicsEngine.CharacterCollisionsGround.Clear ()
            physicsEngine.CharacterCollisionsAll.Clear ()

            // destroy characters
            physicsEngine.CharacterUserData.Clear ()
            for character in physicsEngine.Characters.Values do
                let innerBodyID = character.InnerBodyID
                let innerBodyId = physicsEngine.BodyUserData.[innerBodyID]
                physicsEngine.CharacterVsCharacterCollision.Remove character
                physicsEngine.BodyUserData.Remove innerBodyID |> ignore<bool>
                physicsEngine.Bodies.Remove innerBodyId |> ignore<bool>
                physicsEngine.PhysicsContext.BodyInterface.RemoveAndDestroyBody &innerBodyID
                character.Dispose ()
            physicsEngine.Characters.Clear ()

            // clear any in-flight body contacts
            lock physicsEngine.BodyContactLock $ fun () ->
                physicsEngine.BodyContactEvents.Clear ()

            // clear body collision tracking
            physicsEngine.BodyCollisionsGround.Clear ()
            physicsEngine.BodyCollisionsAll.Clear ()

            // destroy constraints
            for constrain in physicsEngine.BodyConstraints.Values do
                physicsEngine.PhysicsContext.RemoveConstraint constrain
            physicsEngine.BodyConstraints.Clear ()

            // destroy bodies
            for bodyID in physicsEngine.Bodies.Values do
                physicsEngine.PhysicsContext.BodyInterface.RemoveAndDestroyBody &bodyID
            physicsEngine.BodyUserData.Clear ()
            physicsEngine.Bodies.Clear ()

            // clear body joint creation messages
            physicsEngine.CreateBodyJointMessages.Clear ()

            // clear integration messages
            physicsEngine.IntegrationMessages.Clear ()

            // fin
            affected

        member physicsEngine.CleanUp () =
            PhysicsEngineJolt.cleanUp physicsEngine