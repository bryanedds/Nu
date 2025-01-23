﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Linq
open System.Numerics
open JoltPhysicsSharp
open Prime

type [<Struct>] private CharacterContactEvent =
    | CharacterContactAdded of Character : CharacterVirtual * Character2Identifier : ValueEither<CharacterID, BodyID> * SubShape2ID : SubShapeID * ContactPosition : Vector3 * ContactNormal : Vector3
    | CharacterContactRemoved of Character : CharacterVirtual * Character2Identifier : ValueEither<CharacterID, BodyID> * SubShape2ID : SubShapeID

type [<Struct>] private CharacterUserData =
    { CharacterBodyId : BodyId
      CharacterCollisionCategories : int
      CharacterCollisionMask : int
      CharacterGravityOverride : Vector3 option
      CharacterProperties : CharacterProperties }

type [<Struct>] private BodyContactEvent =
    | BodyContactAdded of BodyID : BodyID * Body2ID : BodyID * ContactNormal : Vector3
    | BodyContactRemoved of BodyID : BodyID * Body2ID : BodyID

type [<Struct>] private BodyUserData =
    { BodyId : BodyId
      BodyCollisionCategories : int
      BodyCollisionMask : int
      BodyObserving : bool }

type [<Struct>] private BodyConstraintEvent =
    | BodyConstraintBreak of BodyJointId : BodyJointId * BreakingPoint : single * BreakingOverflow : single

type [<Struct>] private BodyConstraintUserData =
    { BreakingPoint : single }

type private BodyFilterLambda (predicateBodyID, predicateBody) =
    inherit BodyFilter ()
    override this.ShouldCollide bodyID = predicateBodyID bodyID
    override this.ShouldCollideLocked body = predicateBody body

type [<CustomEquality; NoComparison>] private UnscaledPointsKey =
    { HashCode : int
      Vertices : Vector3 array }

    static member hash chs =
        chs.HashCode

    static member equals left right =
        left.HashCode = right.HashCode && // TODO: ensure this isn't just a minor pessimization.
        Enumerable.SequenceEqual (left.Vertices, right.Vertices)

    static member comparer =
        HashIdentity.FromFunctions UnscaledPointsKey.hash UnscaledPointsKey.equals

    static member make (vertices : Vector3 array) =
        let hashCode =
            hash vertices.Length ^^^
            (if vertices.Length > 0 then vertices.[0].GetHashCode () else 0) ^^^
            (if vertices.Length > 0 then vertices.[vertices.Length / 2].GetHashCode () else 0) ^^^
            (if vertices.Length > 0 then vertices.[vertices.Length - 1].GetHashCode () else 0)
        { HashCode = hashCode
          Vertices = vertices }

    interface UnscaledPointsKey IEquatable with
        member this.Equals that =
            UnscaledPointsKey.equals this that

    override this.Equals that =
        match that with
        | :? UnscaledPointsKey as that -> UnscaledPointsKey.equals this that
        | _ -> false

    override this.GetHashCode () =
        this.HashCode

type [<ReferenceEquality>] PhysicsEngine3d =
    private
        { PhysicsContext : PhysicsSystem
          JobSystem : JobSystemThreadPool
          UnscaledPointsCache : Dictionary<UnscaledPointsKey, Vector3 array>
          CharacterVsCharacterCollision : CharacterVsCharacterCollisionSimple
          CharacterContactLock : obj
          CharacterContactEvents : CharacterContactEvent HashSet
          CharacterCollisions : Dictionary<CharacterVirtual, Dictionary<SubShapeID, Vector3>>
          CharacterUserData : Dictionary<CharacterID, CharacterUserData>
          Characters : Dictionary<BodyId, CharacterVirtual>
          BodyContactLock : obj
          BodyContactEvents : BodyContactEvent HashSet
          BodyCollisionsGround : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          BodyCollisionsAll : Dictionary<BodyId, Dictionary<BodyId, Vector3>>
          BodyUserData : Dictionary<BodyID, BodyUserData>
          Bodies : Dictionary<BodyId, BodyID>
          CreateBodyJointMessages : Dictionary<BodyId, CreateBodyJointMessage List>
          BodyConstraintEvents : BodyConstraintEvent List
          BodyConstraintUserData : Dictionary<BodyJointId, BodyConstraintUserData>
          BodyConstraints : Dictionary<BodyJointId, Constraint>
          IntegrationMessages : IntegrationMessage List }

    static member sanitizeScale scale =
        let scale' = Vector3.Max (scale, v3Dup 0.001f) // prevent having near zero or negative size
        if scale' <> scale then Log.warnOnce ("3D physics engine received scale too near or less than zero. Using " + scstring scale' + " instead.")
        scale'

    static member private handleBodyPenetration (bodyId : BodyId) (body2Id : BodyId) (contactNormal : Vector3) physicsEngine =

        // construct body penetration message
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = contactNormal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        // track body ground collisions
        let theta = contactNormal.Dot Vector3.UnitY |> acos |> abs
        if theta < Constants.Physics.GroundAngleMax then
            match physicsEngine.BodyCollisionsGround.TryGetValue bodyId with
            | (true, collisions) -> collisions.[body2Id] <- contactNormal
            | (false, _) -> physicsEngine.BodyCollisionsGround.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, contactNormal)]
            
        // track body collisions
        match physicsEngine.BodyCollisionsAll.TryGetValue bodyId with
        | (true, collisions) -> collisions.[body2Id] <- contactNormal
        | (false, _) -> physicsEngine.BodyCollisionsAll.[bodyId] <- dictPlus HashIdentity.Structural [(body2Id, contactNormal)]

    static member private handleBodySeparation (bodyId : BodyId) (body2Id : BodyId) physicsEngine =

        // construct body separation message
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        // track body ground collisions
        match physicsEngine.BodyCollisionsGround.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.BodyCollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

        // track body collisions
        match physicsEngine.BodyCollisionsAll.TryGetValue bodyId with
        | (true, collisions) ->
            collisions.Remove body2Id |> ignore<bool>
            if collisions.Count = 0 then physicsEngine.BodyCollisionsGround.Remove bodyId |> ignore<bool>
        | (false, _) -> ()

    static member private handleCharacterPenetration (bodyId : BodyId) (body2Id : BodyId) (contactNormal : Vector3) physicsEngine =
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = contactNormal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private handleCharacterSeparation (bodyId : BodyId) (body2Id : BodyId) physicsEngine =
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
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale)
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
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale)
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
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                new ScaledShapeSettings (shapeSettings, &shapeScale)
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
        Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
        let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
        PhysicsEngine3d.attachBoxShape bodyProperties boxShape scShapeSettings masses

    static member private attachBodyConvexHullShape (bodyProperties : BodyProperties) (pointsShape : Nu.PointsShape) (scShapeSettings : StaticCompoundShapeSettings) masses (physicsEngine : PhysicsEngine3d) =
        let unscaledPointsKey = UnscaledPointsKey.make pointsShape.Points
        let (optimized, unscaledPoints) =
            match physicsEngine.UnscaledPointsCache.TryGetValue unscaledPointsKey with
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
                physicsEngine.UnscaledPointsCache.Add (unscaledPointsKey, unscaledPoints)
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
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
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
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
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
            PhysicsEngine3d.attachBodyConvexHullShape bodyProperties pointsShape scShapeSettings masses physicsEngine
        else PhysicsEngine3d.attachBodyBvhTriangles bodyProperties geometryShape scShapeSettings masses

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
                        PhysicsEngine3d.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
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
                PhysicsEngine3d.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
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
            let masses' = PhysicsEngine3d.attachBodyShape bodyProperties bodyShape scShapeSettings masses physicsEngine
            masses' @ masses)
            masses
            bodyShapes

    static member private attachBodyShape bodyProperties bodyShape scShapeSettings masses physicsEngine =
        match bodyShape with
        | EmptyShape -> masses
        | BoxShape boxShape -> PhysicsEngine3d.attachBoxShape bodyProperties boxShape scShapeSettings masses
        | SphereShape sphereShape -> PhysicsEngine3d.attachSphereShape bodyProperties sphereShape scShapeSettings masses
        | CapsuleShape capsuleShape -> PhysicsEngine3d.attachCapsuleShape bodyProperties capsuleShape scShapeSettings masses
        | BoxRoundedShape boxRoundedShape -> PhysicsEngine3d.attachBoxRoundedShape bodyProperties boxRoundedShape scShapeSettings masses
        | PointsShape pointsShape -> PhysicsEngine3d.attachBodyConvexHullShape bodyProperties pointsShape scShapeSettings masses physicsEngine
        | GeometryShape geometryShape -> PhysicsEngine3d.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
        | StaticModelShape staticModelShape -> PhysicsEngine3d.attachStaticModelShape bodyProperties staticModelShape scShapeSettings masses physicsEngine
        | StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngine3d.attachStaticModelShapeSurface bodyProperties staticModelSurfaceShape scShapeSettings masses physicsEngine
        | TerrainShape terrainShape -> PhysicsEngine3d.attachTerrainShape bodyProperties terrainShape scShapeSettings masses
        | BodyShapes bodyShapes -> PhysicsEngine3d.attachBodyShapes bodyProperties bodyShapes scShapeSettings masses physicsEngine

    static member private createBody3 (bodyId : BodyId) (bodyProperties : BodyProperties) (physicsEngine : PhysicsEngine3d) =

        // create either a character or a non-character body
        use scShapeSettings = new StaticCompoundShapeSettings ()
        let masses = PhysicsEngine3d.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettings [] physicsEngine
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
            characterSettings.InnerBodyLayer <- Constants.Physics.ObjectLayerMoving
            characterSettings.Mass <- mass
            characterSettings.MaxSlopeAngle <- bodyProperties.CharacterProperties.SlopeMax
            characterSettings.Shape <- scShapeSettings.Create ()

            // inner shape config (must be set after Shape property)
            use scShapeSettingsInner = new StaticCompoundShapeSettings ()
            PhysicsEngine3d.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettingsInner [] physicsEngine |> ignore<single list>
            characterSettings.InnerBodyShape <- scShapeSettingsInner.Create ()

            // create actual character
            let character = new CharacterVirtual (characterSettings, &bodyProperties.Center, &bodyProperties.Rotation, 0UL, physicsEngine.PhysicsContext)
            let innerBodyID = character.InnerBodyID
            let bodyUserData =
                { BodyId = bodyId
                  BodyCollisionCategories = bodyProperties.CollisionCategories
                  BodyCollisionMask = bodyProperties.CollisionMask
                  BodyObserving = bodyProperties.ShouldObserve }
            physicsEngine.CharacterVsCharacterCollision.Add character
            character.SetCharacterVsCharacterCollision physicsEngine.CharacterVsCharacterCollision
            physicsEngine.BodyUserData.Add (innerBodyID, bodyUserData)
            physicsEngine.Bodies.Add (bodyId, innerBodyID)

            // set inner body physics properties
            // NOTE: dummied out since I don't think any of this does anything.
            //if bodyProperties.Enabled
            //then physicsEngine.PhysicsContext.BodyInterface.ActivateBody &innerBodyID
            //else physicsEngine.PhysicsContext.BodyInterface.DeactivateBody &innerBodyID
            //physicsEngine.PhysicsContext.BodyInterface.SetFriction (&innerBodyID, bodyProperties.Friction)
            //physicsEngine.PhysicsContext.BodyInterface.SetRestitution (&innerBodyID, bodyProperties.Restitution)
            //let motionQuality = match bodyProperties.CollisionDetection with Discontinuous -> MotionQuality.Discrete | Continuous -> MotionQuality.LinearCast
            //physicsEngine.PhysicsContext.BodyInterface.SetMotionQuality (&innerBodyID, motionQuality)

            // validate contact with category and mask
            character.add_OnContactValidate (fun character body2 _ ->
                let characterID = character.ID
                let body2ID = body2.ID
                lock physicsEngine.CharacterContactLock $ fun () ->
                    // TODO: P1: optimize collision mask and categories check with in-place body user data.
                    match physicsEngine.CharacterUserData.TryGetValue characterID with
                    | (true, characterUserData) ->
                        match physicsEngine.CharacterUserData.TryGetValue body2ID with
                        | (true, character2UserData) ->
                            if characterUserData.CharacterCollisionCategories &&& character2UserData.CharacterCollisionMask <> 0
                            then Bool8.True
                            else Bool8.False
                        | (false, _) -> Bool8.True
                    | (false, _) -> Bool8.True)

            // create character body contact add events
            character.add_OnContactAdded (fun character body2ID subShape2ID contactPosition contactNormal _ ->
                let body2ID = body2ID
                let contactPosition = contactPosition
                let contactNormal = contactNormal
                lock physicsEngine.CharacterContactLock $ fun () ->

                    // track character collision normals
                    match physicsEngine.CharacterCollisions.TryGetValue character with
                    | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                    | (false, _) -> physicsEngine.CharacterCollisions.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]

                    // create character contact add event
                    let character2Identifier = ValueRight body2ID
                    let contactPosition = v3 (single contactPosition.X) (single contactPosition.Y) (single contactPosition.Z)
                    physicsEngine.CharacterContactEvents.Add (CharacterContactAdded (character, character2Identifier, subShape2ID, contactPosition, contactNormal)) |> ignore<bool>)

            // create character body contact remove events
            character.add_OnContactRemoved (fun character body2ID subShape2ID ->
                let body2ID = body2ID
                lock physicsEngine.CharacterContactLock $ fun () ->

                    // track character collision normals
                    match physicsEngine.CharacterCollisions.TryGetValue character with
                    | (true, collisions) ->
                        collisions.Remove subShape2ID |> ignore<bool>
                        if collisions.Count = 0 then physicsEngine.CharacterCollisions.Remove character |> ignore<bool>
                    | (false, _) -> ()

                    // create character contact remove event
                    let character2Identifier = ValueRight body2ID
                    physicsEngine.CharacterContactEvents.Add (CharacterContactRemoved (character, character2Identifier, subShape2ID)) |> ignore<bool>)

            // validate contact with category and mask
            character.add_OnCharacterContactValidate (fun character character2 _ ->
                let characterID = character.ID
                let character2ID = character2.ID
                lock physicsEngine.CharacterContactLock $ fun () ->
                    // TODO: P1: optimize collision mask and categories check with in-place body user data.
                    match physicsEngine.CharacterUserData.TryGetValue characterID with
                    | (true, characterUserData) ->
                        match physicsEngine.CharacterUserData.TryGetValue character2ID with
                        | (true, character2UserData) ->
                            if characterUserData.CharacterCollisionCategories &&& character2UserData.CharacterCollisionMask <> 0
                            then Bool8.True
                            else Bool8.False
                        | (false, _) -> Bool8.True
                    | (false, _) -> Bool8.True)

            // create character contact add events
            character.add_OnCharacterContactAdded (fun character character2 subShape2ID contactPosition contactNormal _ ->
                let contactPosition = contactPosition
                let contactNormal = contactNormal
                lock physicsEngine.CharacterContactLock $ fun () ->

                    // track character collision normals
                    match physicsEngine.CharacterCollisions.TryGetValue character with
                    | (true, collisions) -> collisions.[subShape2ID] <- contactNormal
                    | (false, _) -> physicsEngine.CharacterCollisions.[character] <- dictPlus HashIdentity.Structural [(subShape2ID, contactNormal)]
                            
                    // create character contact add event
                    let character2Identifier = ValueLeft character2.ID
                    let contactPosition = v3 (single contactPosition.X) (single contactPosition.Y) (single contactPosition.Z)
                    physicsEngine.CharacterContactEvents.Add (CharacterContactAdded (character, character2Identifier, subShape2ID, contactPosition, contactNormal)) |> ignore<bool>)

            // create character contact remove events
            character.add_OnCharacterContactRemoved (fun character character2ID subShape2ID ->
                lock physicsEngine.CharacterContactLock $ fun () ->

                    // track character collision normals
                    match physicsEngine.CharacterCollisions.TryGetValue character with
                    | (true, collisions) ->
                        collisions.Remove subShape2ID |> ignore<bool>
                        if collisions.Count = 0 then physicsEngine.CharacterCollisions.Remove character |> ignore<bool>
                    | (false, _) -> ()

                    // create character contact remove event
                    let character2Identifier = ValueLeft character2ID
                    physicsEngine.CharacterContactEvents.Add (CharacterContactRemoved (character, character2Identifier, subShape2ID)) |> ignore<bool>)

            // bookkeep character
            let characterUserData =
                { CharacterBodyId = bodyId
                  CharacterCollisionCategories = bodyProperties.CollisionCategories
                  CharacterCollisionMask = bodyProperties.CollisionMask
                  CharacterGravityOverride = bodyProperties.GravityOverride
                  CharacterProperties = bodyProperties.CharacterProperties }
            physicsEngine.CharacterUserData.Add (character.ID, characterUserData)
            physicsEngine.Characters.Add (bodyId, character)

        else

            // configure and create non-character body
            let layer = if bodyProperties.BodyType.IsStatic then Constants.Physics.ObjectLayerNonMoving else Constants.Physics.ObjectLayerMoving
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
            bodyCreationSettings.GravityFactor <-
                match bodyProperties.GravityOverride with
                | Some gravity -> gravity.Magnitude
                | None -> 1.0f
            bodyCreationSettings.MotionQuality <-
                match bodyProperties.CollisionDetection with
                | Discontinuous -> MotionQuality.Discrete
                | Continuous -> MotionQuality.LinearCast
            bodyCreationSettings.IsSensor <- bodyProperties.Sensor
            let body = physicsEngine.PhysicsContext.BodyInterface.CreateBody bodyCreationSettings
            let bodyUserData =
                { BodyId = bodyId
                  BodyCollisionCategories = bodyProperties.CollisionCategories
                  BodyCollisionMask = bodyProperties.CollisionMask
                  BodyObserving = bodyProperties.ShouldObserve }
            physicsEngine.PhysicsContext.BodyInterface.AddBody (&body, if bodyProperties.Enabled then Activation.Activate else Activation.DontActivate)
            physicsEngine.BodyUserData.Add (body.ID, bodyUserData)
            physicsEngine.Bodies.Add (bodyId, body.ID)

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =

        // attempt to create body
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        PhysicsEngine3d.createBody3 bodyId bodyProperties physicsEngine

        // attempt to run any related body joint creation functions
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine3d.destroyBodyJointInternal bodyJointId physicsEngine
                PhysicsEngine3d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine
        | (false, _) -> ()

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter (fun (bodyProperties : BodyProperties) ->
            let createBodyMessage =
                { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyProperties = bodyProperties }
            PhysicsEngine3d.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =

        // attempt to run any related body joint destruction functions
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.CreateBodyJointMessages.TryGetValue bodyId with
        | (true, createBodyJointMessages) ->
            for createBodyJointMessage in createBodyJointMessages do
                let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
                PhysicsEngine3d.destroyBodyJointInternal bodyJointId physicsEngine
        | (false, _) -> ()

        // attempt to destroy character
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, character) ->
            let innerBodyID = character.InnerBodyID
            physicsEngine.Bodies.Remove bodyId |> ignore<bool>
            physicsEngine.BodyUserData.Remove character.InnerBodyID |> ignore<bool>
            physicsEngine.Characters.Remove bodyId |> ignore<bool>
            physicsEngine.CharacterUserData.Remove character.ID |> ignore<bool>
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
            PhysicsEngine3d.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createBodyJointInternal bodyJointProperties bodyJointId physicsEngine =

        // attempt to create joint
        let resultOpt =
            match bodyJointProperties.BodyJoint with
            | EmptyJoint ->
                None
            | OneBodyJoint2d _ | TwoBodyJoint2d _ ->
                Log.warn ("Joint type '" + getCaseName bodyJointProperties.BodyJoint + "' not implemented for PhysicsEngine3d.")
                None
            | OneBodyJoint3d oneBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                match physicsEngine.Bodies.TryGetValue bodyId with
                | (true, bodyID) ->
                    let mutable bodyLockWrite = BodyLockWrite ()
                    try physicsEngine.PhysicsContext.BodyLockInterface.LockWrite (&bodyID, &bodyLockWrite) // NOTE: assuming that jolt needs write capabilities for these.
                        let body = bodyLockWrite.Body
                        let joint = oneBodyJoint.CreateOneBodyJoint body
                        Some (joint, bodyID, None)
                    finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockWrite &bodyLockWrite
                | (false, _) -> None
            | TwoBodyJoint3d twoBodyJoint ->
                let bodyId = bodyJointProperties.BodyJointTarget
                let body2IdOpt = bodyJointProperties.BodyJointTarget2Opt
                match body2IdOpt with
                | Some body2Id ->
                    match (physicsEngine.Bodies.TryGetValue bodyId, physicsEngine.Bodies.TryGetValue body2Id) with
                    | ((true, bodyID), (true, body2ID)) ->
                        use lockMultiWrite = physicsEngine.PhysicsContext.BodyLockInterface.LockMultiWrite ([|bodyID; body2ID|].AsSpan ()) // NOTE: assuming that jolt needs write capabilities for these.
                        let body = lockMultiWrite.GetBody 0u
                        let body2 = lockMultiWrite.GetBody 1u
                        let joint = twoBodyJoint.CreateTwoBodyJoint body body2
                        Some (joint, bodyID, Some body2ID)
                    | _ -> None
                | None -> None

        // finalize joint creation and bookkeep it
        match resultOpt with
        | Some (constrain, bodyID, body2IDOpt) ->
            constrain.Enabled <- bodyJointProperties.BodyJointEnabled && not bodyJointProperties.Broken
            physicsEngine.PhysicsContext.BodyInterface.ActivateBody &bodyID // TODO: make sure we manually need to wake bodies acquiring constraints.
            match body2IDOpt with
            | Some body2ID -> physicsEngine.PhysicsContext.BodyInterface.ActivateBody &body2ID // TODO: make sure we manually need to wake bodies acquiring constraints.
            | None -> ()
            physicsEngine.PhysicsContext.AddConstraint constrain
            if physicsEngine.BodyConstraintUserData.TryAdd (bodyJointId, { BreakingPoint = bodyJointProperties.BreakingPoint })
            then physicsEngine.BodyConstraints.Add (bodyJointId, constrain)
            else Log.warn ("Could not add body joint for '" + scstring bodyJointId + "'.")
        | None -> ()

    static member private createBodyJoint (createBodyJointMessage : CreateBodyJointMessage) physicsEngine =

        // log creation message
        for bodyTargetOpt in [Some createBodyJointMessage.BodyJointProperties.BodyJointTarget; createBodyJointMessage.BodyJointProperties.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) -> messages.Add createBodyJointMessage
                | (false, _) -> physicsEngine.CreateBodyJointMessages.Add (bodyTarget, List [createBodyJointMessage])
            | None -> ()

        // attempt to add body joint
        let bodyJointId = { BodyJointSource = createBodyJointMessage.BodyJointSource; BodyJointIndex = createBodyJointMessage.BodyJointProperties.BodyJointIndex }
        PhysicsEngine3d.createBodyJointInternal createBodyJointMessage.BodyJointProperties bodyJointId physicsEngine

    static member private destroyBodyJointInternal (bodyJointId : BodyJointId) physicsEngine =
        match physicsEngine.BodyConstraints.TryGetValue bodyJointId with
        | (true, joint) ->
            physicsEngine.BodyConstraints.Remove bodyJointId |> ignore
            physicsEngine.BodyConstraintUserData.Remove bodyJointId |> ignore
            physicsEngine.PhysicsContext.RemoveConstraint joint
        | (false, _) -> ()

    static member private destroyBodyJoint (destroyBodyJointMessage : DestroyBodyJointMessage) physicsEngine =

        // unlog creation message
        for bodyTargetOpt in [Some destroyBodyJointMessage.BodyJointTarget; destroyBodyJointMessage.BodyJointTarget2Opt] do
            match bodyTargetOpt with
            | Some bodyTarget ->
                match physicsEngine.CreateBodyJointMessages.TryGetValue bodyTarget with
                | (true, messages) ->
                    messages.RemoveAll (fun message ->
                        message.BodyJointSource = destroyBodyJointMessage.BodyJointId.BodyJointSource &&
                        message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex) |>
                    ignore<int>
                | (false, _) -> ()
            | None -> ()

        // attempt to destroy body joint
        PhysicsEngine3d.destroyBodyJointInternal destroyBodyJointMessage.BodyJointId physicsEngine

    static member private tryGetBodyID bodyId physicsEngine =
        match physicsEngine.Characters.TryGetValue bodyId with
        | (true, character) -> ValueSome character.InnerBodyID
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, bodyID) -> ValueSome bodyID
            | (false, _) -> ValueNone

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match PhysicsEngine3d.tryGetBodyID setBodyEnabledMessage.BodyId physicsEngine with
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
        match PhysicsEngine3d.tryGetBodyID setBodyAngularVelocityMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            physicsEngine.PhysicsContext.BodyInterface.SetAngularVelocity (&bodyID, &setBodyAngularVelocityMessage.AngularVelocity)
        | ValueNone -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match PhysicsEngine3d.tryGetBodyID applyBodyLinearImpulseMessage.BodyId physicsEngine with
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
        match PhysicsEngine3d.tryGetBodyID applyBodyAngularImpulseMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddAngularImpulse (&bodyID, &applyBodyAngularImpulseMessage.AngularImpulse)
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match PhysicsEngine3d.tryGetBodyID applyBodyForceMessage.BodyId physicsEngine with
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
        match PhysicsEngine3d.tryGetBodyID applyBodyTorqueMessage.BodyId physicsEngine with
        | ValueSome bodyID ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.X)
            then physicsEngine.PhysicsContext.BodyInterface.AddTorque (&bodyID, &applyBodyTorqueMessage.Torque)
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Jolt Physics.")
        | ValueNone -> ()

    static member private jumpBody (jumpBodyMessage : JumpBodyMessage) physicsEngine =
        match physicsEngine.Characters.TryGetValue jumpBodyMessage.BodyId with
        | (true, character) ->
            if  jumpBodyMessage.CanJumpInAir ||
                character.GroundState = GroundState.OnGround then
                character.LinearVelocity <- character.LinearVelocity + v3Up * jumpBodyMessage.JumpSpeed
        | (false, _) ->
            match physicsEngine.Bodies.TryGetValue jumpBodyMessage.BodyId with
            | (true, bodyID) ->
                if  jumpBodyMessage.CanJumpInAir ||
                    physicsEngine.BodyCollisionsGround.ContainsKey jumpBodyMessage.BodyId then
                    let linearVelocity = physicsEngine.PhysicsContext.BodyInterface.GetLinearVelocity &bodyID + v3Up * jumpBodyMessage.JumpSpeed
                    physicsEngine.PhysicsContext.BodyInterface.SetLinearVelocity (&bodyID, &linearVelocity)
            | (false, _) -> ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine3d.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine3d.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine3d.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine3d.destroyBodies destroyBodiesMessage physicsEngine
        | CreateBodyJointMessage createBodyJointMessage -> PhysicsEngine3d.createBodyJoint createBodyJointMessage physicsEngine
        | DestroyBodyJointMessage destroyBodyJointMessage -> PhysicsEngine3d.destroyBodyJoint destroyBodyJointMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngine3d.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngine3d.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine3d.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine3d.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine3d.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine3d.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine3d.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine3d.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngine3d.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | JumpBodyMessage jumpBodyMessage -> PhysicsEngine3d.jumpBody jumpBodyMessage physicsEngine
        | SetGravityMessage gravity -> physicsEngine.PhysicsContext.Gravity <- gravity

    static member private createIntegrationMessages (physicsEngine : PhysicsEngine3d) =

        // handle character body collision events
        lock physicsEngine.CharacterContactLock $ fun () ->
            for contactEvent in physicsEngine.CharacterContactEvents do
                match contactEvent with
                | CharacterContactAdded (character, character2Identifier, _, _, contactNormal) ->
                    let bodyId = (physicsEngine.CharacterUserData.[character.ID]).CharacterBodyId
                    let body2IdOpt =
                        match character2Identifier with
                        | ValueLeft character2ID ->
                            match physicsEngine.CharacterUserData.TryGetValue character2ID with
                            | (true, characterUserData) -> ValueSome characterUserData.CharacterBodyId
                            | (false, _) -> ValueNone
                        | ValueRight body2ID ->
                            match physicsEngine.BodyUserData.TryGetValue body2ID with
                            | (true, bodyUserData) -> ValueSome bodyUserData.BodyId
                            | (false, _) -> ValueNone
                    match body2IdOpt with
                    | ValueSome body2Id ->
                        PhysicsEngine3d.handleCharacterPenetration bodyId body2Id contactNormal physicsEngine
                        PhysicsEngine3d.handleCharacterPenetration body2Id bodyId -contactNormal physicsEngine
                    | ValueNone -> ()
                | CharacterContactRemoved (character, character2Identifier, _) ->
                    let bodyId = physicsEngine.CharacterUserData.[character.ID].CharacterBodyId
                    let body2IdOpt =
                        match character2Identifier with
                        | ValueLeft character2ID ->
                            match physicsEngine.CharacterUserData.TryGetValue character2ID with
                            | (true, characterUserData) -> ValueSome characterUserData.CharacterBodyId
                            | (false, _) -> ValueNone
                        | ValueRight body2ID ->
                            match physicsEngine.BodyUserData.TryGetValue body2ID with
                            | (true, bodyUserData) -> ValueSome bodyUserData.BodyId
                            | (false, _) -> ValueNone
                    match body2IdOpt with
                    | ValueSome body2Id ->
                        PhysicsEngine3d.handleCharacterSeparation bodyId body2Id physicsEngine
                        PhysicsEngine3d.handleCharacterSeparation body2Id bodyId physicsEngine
                    | ValueNone -> ()
            physicsEngine.CharacterContactEvents.Clear ()

        // handle non-character body collision events
        lock physicsEngine.BodyContactLock $ fun () ->
            for contactEvent in physicsEngine.BodyContactEvents do
                match contactEvent with
                | BodyContactAdded (bodyID, body2ID, contactNormal) ->
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyUserData) ->
                        match physicsEngine.BodyUserData.TryGetValue body2ID with
                        | (true, body2UserData) ->
                            PhysicsEngine3d.handleBodyPenetration bodyUserData.BodyId body2UserData.BodyId contactNormal physicsEngine
                            PhysicsEngine3d.handleBodyPenetration body2UserData.BodyId bodyUserData.BodyId -contactNormal physicsEngine
                        | (false, _) -> ()
                    | (false, _) -> ()
                | BodyContactRemoved (bodyID, body2ID) ->
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyUserData) ->
                        match physicsEngine.BodyUserData.TryGetValue body2ID with
                        | (true, body2UserData) ->
                            PhysicsEngine3d.handleBodySeparation bodyUserData.BodyId body2UserData.BodyId physicsEngine
                            PhysicsEngine3d.handleBodySeparation body2UserData.BodyId bodyUserData.BodyId physicsEngine
                        | (false, _) -> ()
                    | (false, _) -> ()
            physicsEngine.BodyContactEvents.Clear ()

        // submit body joint break messages
        for bodyConstraintEvent in physicsEngine.BodyConstraintEvents do
            match bodyConstraintEvent with
            | BodyConstraintBreak (bodyJointId, breakingPoint, breakingOverflow) ->
                let bodyJointBreakMessage = BodyJointBreakMessage { BodyJointId = bodyJointId; BreakingPoint = breakingPoint; BreakingOverflow = breakingOverflow }
                physicsEngine.IntegrationMessages.Add bodyJointBreakMessage
        physicsEngine.BodyConstraintEvents.Clear ()

        // submit character body transform messages
        let bodyInterface = physicsEngine.PhysicsContext.BodyInterface // OPTIMIZATION: cache property for efficiency.
        for characterEntry in physicsEngine.Characters do
            let bodyId = characterEntry.Key
            let character = characterEntry.Value
            let innerBodyId = character.InnerBodyID
            let bodyTransformMessage =
                BodyTransformMessage
                    { BodyId = bodyId
                      Center = character.Position
                      Rotation = character.Rotation
                      LinearVelocity = character.LinearVelocity
                      AngularVelocity = bodyInterface.GetAngularVelocity &innerBodyId }
            physicsEngine.IntegrationMessages.Add bodyTransformMessage

        // submit non-character body transform messages
        for bodiesEntry in physicsEngine.Bodies do
            let bodyId = bodiesEntry.Key
            let bodyID = bodiesEntry.Value
            if  bodyInterface.IsActive &bodyID &&
                not (physicsEngine.Characters.ContainsKey bodyId) then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = bodyId
                          Center = bodyInterface.GetPosition &bodyID
                          Rotation = bodyInterface.GetRotation &bodyID
                          LinearVelocity = bodyInterface.GetLinearVelocity &bodyID
                          AngularVelocity = bodyInterface.GetAngularVelocity &bodyID }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    static member make (gravity : Vector3) =

        // initialize Jolt foundation layer
        if not (Foundation.Init false) then
            Log.fail "Could not initialize Jolt Physics."

        // setup multiphase physics pipeline.
        // we use only 2 layers: one for non-moving objects and one for moving objects.
        // we use a 1-to-1 mapping between object layers and broadphase layers.
        let objectLayerPairFilter = new ObjectLayerPairFilterTable (2u)
        objectLayerPairFilter.EnableCollision (Constants.Physics.ObjectLayerNonMoving, Constants.Physics.ObjectLayerMoving)
        objectLayerPairFilter.EnableCollision (Constants.Physics.ObjectLayerMoving, Constants.Physics.ObjectLayerMoving)
        let broadPhaseLayerInterface = new BroadPhaseLayerInterfaceTable (2u, 2u)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (Constants.Physics.ObjectLayerNonMoving, Constants.Physics.BroadPhaseLayerNonMoving)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (Constants.Physics.ObjectLayerMoving, Constants.Physics.BroadPhaseLayerMoving)
        let objectVsBroadPhaseLayerFilter = new ObjectVsBroadPhaseLayerFilterTable (broadPhaseLayerInterface, 2u, objectLayerPairFilter, 2u)

        // configure and create the Jolt physics system
        let mutable physicsSystemSettings = PhysicsSystemSettings ()
        physicsSystemSettings.ObjectLayerPairFilter <- objectLayerPairFilter
        physicsSystemSettings.BroadPhaseLayerInterface <- broadPhaseLayerInterface
        physicsSystemSettings.ObjectVsBroadPhaseLayerFilter <- objectVsBroadPhaseLayerFilter
        physicsSystemSettings.MaxBodies <- Constants.Physics.Collision3dBodiesMax
        physicsSystemSettings.MaxBodyPairs <- Constants.Physics.Collision3dBodyPairsMax
        physicsSystemSettings.MaxContactConstraints <- Constants.Physics.Collision3dContactConstraintsMax
        let physicsSystem = new PhysicsSystem (physicsSystemSettings)
        physicsSystem.Gravity <- gravity

        // create some physics engine bookkeeping fields that will be used by body contact handlers
        let bodyContactLock = obj ()
        let bodyContactEvents = HashSet ()
        let bodyUserData = dictPlus HashIdentity.Structural []

        // validate contact with category and mask
        physicsSystem.add_OnContactValidate (fun _ body body2 _ _ ->
            let bodyID = body.ID
            let body2ID = body2.ID
            lock bodyContactLock $ fun () ->
                // TODO: P1: optimize collision mask and categories check with in-place body user data.
                match bodyUserData.TryGetValue bodyID with
                | (true, bodyUserData_) ->
                    match bodyUserData.TryGetValue body2ID with
                    | (true, body2UserData) ->
                        if bodyUserData_.BodyCollisionCategories &&& body2UserData.BodyCollisionMask <> 0
                        then ValidateResult.AcceptContact
                        else ValidateResult.RejectContact
                    | (false, _) -> ValidateResult.AcceptContact
                | (false, _) -> ValidateResult.AcceptContact)

        // create body contact add event
        physicsSystem.add_OnContactAdded (fun _ body body2 manifold _ ->
            let bodyID = body.ID
            let body2ID = body2.ID
            let contactNormal = manifold.WorldSpaceNormal
            lock bodyContactLock $ fun () -> bodyContactEvents.Add (BodyContactAdded (bodyID, body2ID, contactNormal)) |> ignore<bool>)

        // create body contact remove event
        physicsSystem.add_OnContactRemoved (fun _ subShapeIDPair ->
            let bodyID = subShapeIDPair.Body1ID
            let body2ID = subShapeIDPair.Body2ID
            lock bodyContactLock $ fun () -> bodyContactEvents.Add (BodyContactRemoved (bodyID, body2ID)) |> ignore<bool>)

        // create thread job system
        let mutable jobSystemConfig = JobSystemThreadPoolConfig ()
        jobSystemConfig.maxJobs <- uint Constants.Physics.Collision3dJobsMax
        jobSystemConfig.maxBarriers <- uint Constants.Physics.Collision3dBarriersMax
        jobSystemConfig.numThreads <- Constants.Physics.Collision3dThreads
        let jobSystem = new JobSystemThreadPool (&jobSystemConfig)

        // make physics engine
        { PhysicsContext = physicsSystem
          JobSystem = jobSystem
          UnscaledPointsCache = dictPlus UnscaledPointsKey.comparer []
          CharacterVsCharacterCollision = new CharacterVsCharacterCollisionSimple ()
          CharacterContactLock = obj ()
          CharacterContactEvents = hashSetPlus HashIdentity.Structural []
          CharacterCollisions = dictPlus HashIdentity.Structural []
          CharacterUserData = dictPlus HashIdentity.Structural []
          Characters = dictPlus HashIdentity.Structural []
          BodyContactLock = bodyContactLock
          BodyContactEvents = bodyContactEvents
          BodyCollisionsGround = dictPlus HashIdentity.Structural []
          BodyCollisionsAll = dictPlus HashIdentity.Structural []
          BodyUserData = bodyUserData
          Bodies = dictPlus HashIdentity.Structural []
          CreateBodyJointMessages = dictPlus HashIdentity.Structural []
          BodyConstraintEvents = List ()
          BodyConstraintUserData = dictPlus HashIdentity.Structural []
          BodyConstraints = dictPlus HashIdentity.Structural []
          IntegrationMessages = List () }

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Characters.ContainsKey bodyId ||
            physicsEngine.Bodies.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            [|match physicsEngine.Characters.TryGetValue bodyId with
              | (true, character) ->
                  match physicsEngine.CharacterCollisions.TryGetValue character with
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
            match PhysicsEngine3d.tryGetBodyID bodyId physicsEngine with
            | ValueSome bodyID -> physicsEngine.PhysicsContext.BodyInterface.GetAngularVelocity &bodyID
            | ValueNone -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            match physicsEngine.Characters.TryGetValue bodyId with
            | (true, character) ->
                if character.GroundState = GroundState.OnGround
                then [|character.GroundNormal|]
                else [||]
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
            | (true, character) -> character.GroundState = GroundState.OnGround
            | (false, _) -> physicsEngine.BodyCollisionsGround.ContainsKey bodyId

        member physicsEngine.RayCast (segment, collisionMask, closestOnly) =
            let mutable ray = Ray ()
            ray.Position <- segment.A
            ray.Direction <- segment.Vector
            let bodyFilterID bodyID =
                match physicsEngine.BodyUserData.TryGetValue bodyID with
                | (true, bodyUserData) -> bodyUserData.BodyCollisionCategories &&& collisionMask <> 0
                | (false, _) -> false
            let bodyFilterInstance (body : Body) = bodyFilterID body.ID
            use bodyFilter = new BodyFilterLambda (bodyFilterID, bodyFilterInstance)
            let rayCastResults =
                if closestOnly then
                    let mutable rayCastResult = Unchecked.defaultof<RayCastResult>
                    physicsEngine.PhysicsContext.NarrowPhaseQuery.CastRay (&ray, &rayCastResult, null, null, bodyFilter) |> ignore<bool>
                    List [rayCastResult]
                else
                    let rayCastSettings = RayCastSettings ()
                    let collectorType = CollisionCollectorType.AllHitSorted // TODO: consider allowing for unsorted hits.
                    let rayCastResults = List ()
                    physicsEngine.PhysicsContext.NarrowPhaseQuery.CastRay (&ray, rayCastSettings, collectorType, rayCastResults, null, null, bodyFilter) |> ignore<bool>
                    rayCastResults
            [|for rayCastResult in rayCastResults do
                let bodyId = physicsEngine.BodyUserData.[rayCastResult.BodyID].BodyId
                let subShapeID = SubShapeID rayCastResult.subShapeID2
                let position = ray.Position + ray.Direction * rayCastResult.Fraction
                let mutable bodyLockRead = BodyLockRead ()
                let normal =
                    try physicsEngine.PhysicsContext.BodyLockInterface.LockRead (&rayCastResult.BodyID, &bodyLockRead)
                        if bodyLockRead.Succeeded
                        then bodyLockRead.Body.GetWorldSpaceSurfaceNormal (&subShapeID, &position)
                        else Log.warnOnce "Failed to find expected body."; v3Up
                    finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockRead &bodyLockRead
                let bodyShapeIndex = { BodyId = bodyId; BodyShapeIndex = Constants.Physics.InternalIndex } // TODO: P1: see if we can get the user-defined shape index.
                BodyIntersection.make bodyShapeIndex rayCastResult.Fraction position normal|]

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngine3d.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.TryIntegrate stepTime =

            // integrate only when time has passed
            if not stepTime.IsZero then

                // update non-character physics, logging on error (integration should still advance sim regardless of error)
                match physicsEngine.PhysicsContext.Update (stepTime.Seconds, Constants.Physics.Collision3dSteps, physicsEngine.JobSystem) with
                | PhysicsUpdateError.ManifoldCacheFull as error ->
                    Log.warnOnce
                        ("Jolt Physics internal error: " + scstring error + ". Consider increasing Constants.Physics." +
                         nameof Constants.Physics.Collision3dContactConstraintsMax + ".")
                | PhysicsUpdateError.BodyPairCacheFull as error ->
                    Log.warnOnce
                        ("Jolt Physics internal error: " + scstring error + ". Consider increasing Constants.Physics." +
                         nameof Constants.Physics.Collision3dBodyPairsMax + ".")
                | PhysicsUpdateError.ContactConstraintsFull as error ->
                    Log.warnOnce
                        ("Jolt Physics internal error: " + scstring error + ". Consider increasing Constants.Physics." +
                         nameof Constants.Physics.Collision3dContactConstraintsMax + ".")
                | _ -> ()

                // update characters
                let characterLayer = Constants.Physics.ObjectLayerMoving
                for character in physicsEngine.Characters.Values do
                    let characterUserData = physicsEngine.CharacterUserData.[character.ID]
                    let characterGravity = Option.defaultValue physicsEngine.PhysicsContext.Gravity characterUserData.CharacterGravityOverride
                    let characterProperties = characterUserData.CharacterProperties
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
                    character.LinearVelocity <-
                        if character.GroundState = GroundState.OnGround
                        then character.LinearVelocity.MapY (max 0.0f)
                        else character.LinearVelocity + characterGravity * stepTime.Seconds
                    character.ExtendedUpdate (stepTime.Seconds, characterUpdateSettings, &characterLayer, physicsEngine.PhysicsContext)

                // update constraints
                for bodyConstraintEntry in physicsEngine.BodyConstraints do
                    let bodyJointId = bodyConstraintEntry.Key
                    let constrain = bodyConstraintEntry.Value
                    let lambdaPositionOpt =
                        match constrain with
                        | :? HingeConstraint as constrain -> ValueSome constrain.TotalLambdaPosition.Magnitude
                        | :? DistanceConstraint as constrain -> ValueSome constrain.TotalLambdaPosition
                        | _ -> ValueNone
                    match lambdaPositionOpt with
                    | ValueSome lambdaPosition ->
                        let breakingPoint = physicsEngine.BodyConstraintUserData.[bodyJointId].BreakingPoint
                        let breakingDelta = lambdaPosition - breakingPoint
                        if breakingDelta >= 0.0f && constrain.Enabled then
                            physicsEngine.BodyConstraintEvents.Add (BodyConstraintBreak (bodyJointId, breakingPoint, breakingDelta))
                            constrain.Enabled <- false
                    | ValueNone -> ()

                // create integration messages
                PhysicsEngine3d.createIntegrationMessages physicsEngine
                let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
                physicsEngine.IntegrationMessages.Clear ()
                Some integrationMessages

            // no time passed
            else None

        member physicsEngine.ClearInternal () =

            // compute whether the physics engine will be affected by this clear request
            let affected =
                physicsEngine.Characters.Count > 0 ||
                physicsEngine.BodyConstraints.Count > 0 ||
                physicsEngine.Bodies.Count > 0 ||
                physicsEngine.CreateBodyJointMessages.Count > 0 ||
                physicsEngine.BodyConstraints.Count > 0 ||
                physicsEngine.IntegrationMessages.Count > 0

            // clear any in-flight character contacts
            lock physicsEngine.CharacterContactLock $ fun () ->
                physicsEngine.CharacterContactEvents.Clear ()

            // clear character collision tracking
            physicsEngine.CharacterCollisions.Clear ()

            // destroy characters
            physicsEngine.CharacterUserData.Clear ()
            for character in physicsEngine.Characters.Values do
                let innerBodyID = character.InnerBodyID
                let innerBodyId = physicsEngine.BodyUserData.[innerBodyID].BodyId
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

            // clear body joints
            physicsEngine.CreateBodyJointMessages.Clear ()
            physicsEngine.BodyConstraintEvents.Clear ()
            physicsEngine.BodyConstraintUserData.Clear ()
            physicsEngine.BodyConstraints.Clear ()

            // clear integration messages
            physicsEngine.IntegrationMessages.Clear ()

            // fin
            affected

        member physicsEngine.CleanUp () =
            physicsEngine.JobSystem.Dispose ()
            physicsEngine.PhysicsContext.Dispose ()
            Foundation.Shutdown ()