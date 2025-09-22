// Nu Game Engine.
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

type [<Struct; CustomEquality; NoComparison>] private BodyContactEvent =
    | BodyContactAdded of BodyID : BodyID * Body2ID : BodyID * ContactNormal : Vector3
    | BodyContactRemoved of BodyID : BodyID * Body2ID : BodyID
    override this.Equals (that : obj) =
        match that with
        | :? BodyContactEvent as that ->
            match this, that with
            | BodyContactAdded (bodyID, body2ID, contactNormal), BodyContactAdded (bodyID2, body2ID2, contactNormal2) ->
                bodyID.ID = bodyID2.ID && body2ID.ID = body2ID2.ID && contactNormal = contactNormal2
            | BodyContactRemoved (bodyID, body2ID), BodyContactRemoved (bodyID2, body2ID2) ->
                bodyID.ID = bodyID2.ID && body2ID.ID = body2ID2.ID
            | _ -> false
        | _ -> false
    override this.GetHashCode () =
        match this with
        | BodyContactAdded (bodyID, body2ID, contactNormal) ->
            int bodyID.ID ^^^ int body2ID.ID * 131 ^^^ contactNormal.GetHashCode ()
        | BodyContactRemoved (bodyID, body2ID) ->
            int bodyID.ID ^^^ int body2ID.ID * 131

type [<Struct>] private BodyUserData =
    { BodyId : BodyId
      BodyCollisionCategories : int
      BodyCollisionMask : int }

type [<Struct>] private BodyConstraintEvent =
    | BodyConstraintBreak of BodyJointId : BodyJointId * BreakingPoint : single * BreakingOverflow : single

type [<Struct>] private BodyConstraintUserData =
    { BreakingPoint : single }

type private BodyFilterLambda (predicateBodyID, predicateBody) =
    inherit BodyFilter ()
    override this.ShouldCollide bodyID = predicateBodyID bodyID
    override this.ShouldCollideLocked body = predicateBody body

type private BodyDrawFilterLambda (predicateBody) =
    inherit BodyDrawFilter ()
    override this.ShouldDraw body = predicateBody body

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

/// The 3d implementation of PhysicsEngineRenderContext in terms of Jolt Physics.
type PhysicsEngine3dRenderContext =
    { EyeCenter : Vector3
      EyeFrustum : Frustum
      DebugRenderer : DebugRenderer
      DrawSettings : DrawSettings }
    interface PhysicsEngineRenderContext

/// The 3d implementation of PhysicsEngine in terms of Jolt Physics.
and [<ReferenceEquality>] PhysicsEngine3d =
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
          VehicleConstraints : Dictionary<BodyId, VehicleConstraint>
          mutable BodyUnoptimizedCreationCount : int
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

    static member private sanitizeHeight (height : single) =
        let height' = max height 0.1f // prevent having near zero or negative height
        if height' <> height then Log.infoOnce ("3D physics engine received height too near or less than zero. Using " + scstring height' + " instead.")
        height'

    static member private sanitizeRadius (radius : single) =
        let radius' = max radius 0.1f // prevent having near zero or negative radius
        if radius' <> radius then Log.infoOnce ("3D physics engine received radius too near or less than zero. Using " + scstring radius' + " instead.")
        radius'

    static member private sanitizeExtent extent =
        let extent' = Vector3.Max (extent, v3Dup 0.1f) // prevent having near zero or negative extent
        if extent' <> extent then Log.infoOnce ("3D physics engine received extent too near or less than zero. Using " + scstring extent' + " instead.")
        extent'

    static member private sanitizeScale scale =
        let scale' = Vector3.Max (scale, v3Dup 0.0001f) // prevent having near zero or negative scale
        if scale' <> scale then Log.infoOnce ("3D physics engine received scale too near or less than zero. Using " + scstring scale' + " instead.")
        scale'

    static member private validateBodyShape (bodyShape : BodyShape) =
        match bodyShape.PropertiesOpt with
        | Some properties ->
            if not (BodyShapeProperties.validateUtilization3d properties) then
                Log.warnOnce "Invalid utilization of BodyShape.PropertiesOpt in PhysicsEngine3d. Only BodyShapeProperties.BodyShapeIndex can be utilized in the context of 3d physics."
        | None -> ()

    static member private handleBodyPenetration (bodyId : BodyId) (body2Id : BodyId) (contactNormal : Vector3) physicsEngine =

        // construct body penetration message
        let bodyPenetrationMessage =
            { BodyShapeSource = { BodyId = bodyId; BodyShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = body2Id; BodyShapeIndex = 0 }
              Normal = contactNormal }
        let integrationMessage = BodyPenetrationMessage bodyPenetrationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

        // track body ground collisions
        let theta = contactNormal.Dot Vector3.UnitY |> max -1.0f |> min 1.0f |> acos
        if theta <= Constants.Physics.GroundAngleMax && contactNormal.Y > 0.0f then
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

    static member private tryCreateShape (shape : BodyShape) =
        match shape with
        | EmptyShape ->
            None
        | BoxShape boxShape ->
            let extent = boxShape.Size |> PhysicsEngine3d.sanitizeExtent
            let halfExtent = extent * 0.5f
            let shapeSettings = new BoxShapeSettings (&halfExtent)
            let shape = new BoxShape (shapeSettings)
            shape :> ConvexShape |> Some
        | SphereShape sphereShape ->
            let radius = sphereShape.Radius |> PhysicsEngine3d.sanitizeRadius
            let shapeSettings = new SphereShapeSettings (radius)
            let shape = new SphereShape (shapeSettings)
            shape :> ConvexShape |> Some
        | CapsuleShape capsuleShape ->
            let height = capsuleShape.Height |> PhysicsEngine3d.sanitizeHeight
            let halfHeight = height * 0.5f
            let radius = capsuleShape.Radius |> PhysicsEngine3d.sanitizeRadius
            let shapeSettings = new CapsuleShapeSettings (halfHeight, radius)
            let shape = new CapsuleShape (shapeSettings)
            shape :> ConvexShape |> Some
        | BoxRoundedShape boxRoundedShape ->
            Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
            let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
            PhysicsEngine3d.tryCreateShape (Nu.BoxShape boxShape)
        | EdgeShape _ ->
            None
        | ContourShape _ ->
            None
        | PointsShape _ ->
            None // TODO: implement.
        | GeometryShape _ ->
            None // TODO: implement.
        | StaticModelShape _ ->
            None // TODO: implement?
        | StaticModelSurfaceShape _ ->
            None // TODO: implement?
        | TerrainShape _ ->
            None
        | BodyShapes _ ->
            None // TODO: implement?

    static member private attachBoxShape (bodyProperties : BodyProperties) (boxShape : Nu.BoxShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let extent = boxShape.Size |> PhysicsEngine3d.sanitizeExtent
        let halfExtent = extent * 0.5f
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
                let volume = extent.X * extent.Y * extent.Z
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachSphereShape (bodyProperties : BodyProperties) (sphereShape : Nu.SphereShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let radius = sphereShape.Radius |> PhysicsEngine3d.sanitizeRadius
        let shapeSettings = new SphereShapeSettings (radius)
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
                let volume = 4.0f / 3.0f * MathF.PI * pown radius 3
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachCapsuleShape (bodyProperties : BodyProperties) (capsuleShape : Nu.CapsuleShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        let height = capsuleShape.Height |> PhysicsEngine3d.sanitizeHeight
        let halfHeight = height * 0.5f
        let radius = capsuleShape.Radius |> PhysicsEngine3d.sanitizeRadius
        let shapeSettings = new CapsuleShapeSettings (halfHeight, radius)
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
                let volume = MathF.PI * pown radius 2 * (4.0f / 3.0f * radius * height)
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBoxRoundedShape (bodyProperties : BodyProperties) (boxRoundedShape : Nu.BoxRoundedShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
        let boxShape = { Size = boxRoundedShape.Size; TransformOpt = boxRoundedShape.TransformOpt; PropertiesOpt = boxRoundedShape.PropertiesOpt }
        PhysicsEngine3d.attachBoxShape bodyProperties boxShape scShapeSettings masses

    static member private attachEdgeShape (bodyProperties : BodyProperties) (edgeShape : Nu.EdgeShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        // TODO: implement this.
        Log.warnOnce "3D edge shapes are currently unsupported. Degrading to a convex points shape."
        PhysicsEngine3d.attachPointsShape bodyProperties { Points = [|edgeShape.Start; edgeShape.Stop|]; Profile = Convex; TransformOpt = edgeShape.TransformOpt; PropertiesOpt = edgeShape.PropertiesOpt } scShapeSettings masses

    static member private attachContourShape (bodyProperties : BodyProperties) (contourShape : Nu.ContourShape) (scShapeSettings : StaticCompoundShapeSettings) masses =
        // TODO: implement this. Untested AI attempt at implementation: https://github.com/bryanedds/Nu/pull/1113/commits/082ff7db1b05d691ebc6776ad32dd8965e7bbe4d#diff-7be7db6f2992557124644202960c26adb7192d0fb54ccacb3dcfc7b8d1a49deb
        Log.warnOnce "3D contour shapes are currently unsupported. Degrading to a convex points shape."
        PhysicsEngine3d.attachPointsShape bodyProperties { Points = contourShape.Links; Profile = Convex; TransformOpt = contourShape.TransformOpt; PropertiesOpt = contourShape.PropertiesOpt } scShapeSettings masses

    static member private attachBodyConvexHullShape (bodyProperties : BodyProperties) (points : Vector3 array) (transformOpt : Affine option) propertiesOpt (scShapeSettings : StaticCompoundShapeSettings) masses (physicsEngine : PhysicsEngine3d) =
        let unscaledPointsKey = UnscaledPointsKey.make points
        let (optimized, unscaledPoints) =
            match physicsEngine.UnscaledPointsCache.TryGetValue unscaledPointsKey with
            | (true, unscaledVertices) -> (true, unscaledVertices)
            | (false, _) -> (false, points)
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
            match transformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let (scale, shapeSettings) =
            match transformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, new ScaledShapeSettings (shapeSettings, &shapeScale))
            | None -> (v3One, shapeSettings)
        let bodyShapeId = match propertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let box = box3 v3Zero ((Box3.Enclose points).Size * scale)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBodyBvhTriangles (bodyProperties : BodyProperties) (vertices : Vector3 array) (transformOpt : Affine option) propertiesOpt (scShapeSettings : StaticCompoundShapeSettings) masses =
        let triangles =
            vertices
            |> Seq.chunkBySize 3
            |> Seq.map (fun t -> Triangle (&t.[0], &t.[1], &t.[2]))
            |> Array.ofSeq
        let shapeSettings = new MeshShapeSettings (triangles)
        shapeSettings.Sanitize ()
        let struct (center, rotation) =
            match transformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let (scale, shapeSettings) =
            match transformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, new ScaledShapeSettings (shapeSettings, &shapeScale))
            | None -> (v3One, shapeSettings)
        let bodyShapeId = match propertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let box = box3 v3Zero ((Box3.Enclose vertices).Size * scale)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachBodyBoundsShape (bodyProperties : BodyProperties) (points : Vector3 array) (transformOpt : Affine option) propertiesOpt (scShapeSettings : StaticCompoundShapeSettings) masses =
        let bounds = Box3.Enclose points
        let shapeSettings = new ConvexHullShapeSettings (bounds.Corners)
        let struct (center, rotation) =
            match transformOpt with
            | Some transform -> struct (transform.Translation, transform.Rotation)
            | None -> (v3Zero, quatIdentity)
        let (scale, shapeSettings) =
            match transformOpt with
            | Some transform ->
                let shapeScale = bodyProperties.Scale * transform.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, (new ScaledShapeSettings (shapeSettings, &shapeScale) : ShapeSettings))
            | None when bodyProperties.Scale <> v3One ->
                let shapeScale = bodyProperties.Scale |> PhysicsEngine3d.sanitizeScale
                (shapeScale, new ScaledShapeSettings (shapeSettings, &shapeScale))
            | None -> (v3One, shapeSettings)
        let bodyShapeId = match propertiesOpt with Some properties -> properties.BodyShapeIndex | None -> bodyProperties.BodyIndex
        scShapeSettings.AddShape (&center, &rotation, shapeSettings, uint bodyShapeId)
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let box = box3 v3Zero ((Box3.Enclose points).Size * scale)
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        mass :: masses

    static member private attachPointsShape (bodyProperties : BodyProperties) (pointsShape : PointsShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        match pointsShape.Profile with
        | Convex -> PhysicsEngine3d.attachBodyConvexHullShape bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt scShapeSettings masses physicsEngine
        | Concave ->
            Log.warnOnce "Creating body bvh triangles with PointsShape; PointsShape generally specifies individual points rather than triangulated vertices, so unintended behavior may arise."
            PhysicsEngine3d.attachBodyBvhTriangles bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt scShapeSettings masses
        | Bounds -> PhysicsEngine3d.attachBodyBoundsShape bodyProperties pointsShape.Points pointsShape.TransformOpt pointsShape.PropertiesOpt scShapeSettings masses

    static member private attachGeometryShape bodyProperties (geometryShape : GeometryShape) scShapeSettings masses physicsEngine =
        match geometryShape.Profile with
        | Convex -> PhysicsEngine3d.attachBodyConvexHullShape bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt scShapeSettings masses physicsEngine
        | Concave -> PhysicsEngine3d.attachBodyBvhTriangles bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt scShapeSettings masses
        | Bounds -> PhysicsEngine3d.attachBodyBoundsShape bodyProperties geometryShape.Vertices geometryShape.TransformOpt geometryShape.PropertiesOpt scShapeSettings masses

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
                let staticModelSurfaceShape = { StaticModel = staticModelShape.StaticModel; SurfaceIndex = i; Profile = staticModelShape.Profile; TransformOpt = Some transform; PropertiesOpt = staticModelShape.PropertiesOpt }
                match Metadata.tryGetStaticModelMetadata staticModelSurfaceShape.StaticModel with
                | ValueSome staticModel ->
                    if  staticModelSurfaceShape.SurfaceIndex > -1 &&
                        staticModelSurfaceShape.SurfaceIndex < staticModel.Surfaces.Length then
                        let geometry = staticModel.Surfaces.[staticModelSurfaceShape.SurfaceIndex].PhysicallyBasedGeometry
                        let transformOpt = staticModelSurfaceShape.TransformOpt
                        let propertiesOpt = staticModelSurfaceShape.PropertiesOpt
                        match staticModelSurfaceShape.Profile with
                        | Convex -> PhysicsEngine3d.attachBodyConvexHullShape bodyProperties geometry.Vertices transformOpt propertiesOpt scShapeSettings masses physicsEngine
                        | Concave -> PhysicsEngine3d.attachBodyBvhTriangles bodyProperties geometry.Triangles transformOpt propertiesOpt scShapeSettings masses
                        | Bounds -> PhysicsEngine3d.attachBodyBoundsShape bodyProperties geometry.Vertices transformOpt propertiesOpt scShapeSettings masses
                    else centerMassInertiaDisposes
                | ValueNone -> centerMassInertiaDisposes)
                masses
                [0 .. dec staticModel.Surfaces.Length]
        | ValueNone -> masses

    static member private attachStaticModelShapeSurface (bodyProperties : BodyProperties) (staticModelSurfaceShape : StaticModelSurfaceShape) (scShapeSettings : StaticCompoundShapeSettings) masses physicsEngine =
        match Metadata.tryGetStaticModelMetadata staticModelSurfaceShape.StaticModel with
        | ValueSome staticModel ->
            if  staticModelSurfaceShape.SurfaceIndex > -1 &&
                staticModelSurfaceShape.SurfaceIndex < staticModel.Surfaces.Length then
                let surface = staticModel.Surfaces.[staticModelSurfaceShape.SurfaceIndex]
                let geometry = surface.PhysicallyBasedGeometry
                let transformOpt = staticModelSurfaceShape.TransformOpt
                let propertiesOpt = staticModelSurfaceShape.PropertiesOpt
                match staticModelSurfaceShape.Profile with
                | Convex -> PhysicsEngine3d.attachBodyConvexHullShape bodyProperties geometry.Vertices transformOpt propertiesOpt scShapeSettings masses physicsEngine
                | Concave -> PhysicsEngine3d.attachBodyBvhTriangles bodyProperties geometry.Triangles transformOpt propertiesOpt scShapeSettings masses
                | Bounds -> PhysicsEngine3d.attachBodyBoundsShape bodyProperties geometry.Vertices transformOpt propertiesOpt scShapeSettings masses
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
        PhysicsEngine3d.validateBodyShape bodyShape
        match bodyShape with
        | EmptyShape -> masses
        | BoxShape boxShape -> PhysicsEngine3d.attachBoxShape bodyProperties boxShape scShapeSettings masses
        | SphereShape sphereShape -> PhysicsEngine3d.attachSphereShape bodyProperties sphereShape scShapeSettings masses
        | CapsuleShape capsuleShape -> PhysicsEngine3d.attachCapsuleShape bodyProperties capsuleShape scShapeSettings masses
        | BoxRoundedShape boxRoundedShape -> PhysicsEngine3d.attachBoxRoundedShape bodyProperties boxRoundedShape scShapeSettings masses
        | EdgeShape edgeShape -> PhysicsEngine3d.attachEdgeShape bodyProperties edgeShape scShapeSettings masses physicsEngine
        | ContourShape chainShape -> PhysicsEngine3d.attachContourShape bodyProperties chainShape scShapeSettings masses physicsEngine
        | PointsShape pointsShape -> PhysicsEngine3d.attachPointsShape bodyProperties pointsShape scShapeSettings masses physicsEngine
        | GeometryShape geometryShape -> PhysicsEngine3d.attachGeometryShape bodyProperties geometryShape scShapeSettings masses physicsEngine
        | StaticModelShape staticModelShape -> PhysicsEngine3d.attachStaticModelShape bodyProperties staticModelShape scShapeSettings masses physicsEngine
        | StaticModelSurfaceShape staticModelSurfaceShape -> PhysicsEngine3d.attachStaticModelShapeSurface bodyProperties staticModelSurfaceShape scShapeSettings masses physicsEngine
        | TerrainShape terrainShape -> PhysicsEngine3d.attachTerrainShape bodyProperties terrainShape scShapeSettings masses
        | BodyShapes bodyShapes -> PhysicsEngine3d.attachBodyShapes bodyProperties bodyShapes scShapeSettings masses physicsEngine

    static member private createBodyNonCharacter mass layer motionType (shapeSettings : ShapeSettings) (bodyId : BodyId) (bodyProperties : BodyProperties) (physicsEngine : PhysicsEngine3d) =

        // configure and create non-character body
        let mutable bodyCreationSettings = new BodyCreationSettings (shapeSettings, &bodyProperties.Center, &bodyProperties.Rotation, motionType, layer)
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
            | Discrete -> MotionQuality.Discrete
            | Continuous -> MotionQuality.LinearCast
        bodyCreationSettings.IsSensor <- bodyProperties.Sensor
        let body = physicsEngine.PhysicsContext.BodyInterface.CreateBody bodyCreationSettings
        let bodyUserData =
            { BodyId = bodyId
              BodyCollisionCategories = bodyProperties.CollisionCategories
              BodyCollisionMask = bodyProperties.CollisionMask }
        physicsEngine.PhysicsContext.BodyInterface.AddBody (&body, if bodyProperties.Enabled then Activation.Activate else Activation.DontActivate)
        physicsEngine.BodyUserData.Add (body.ID, bodyUserData)
        physicsEngine.Bodies.Add (bodyId, body.ID)
        (bodyId, body)

    static member private createBody3 (bodyId : BodyId) (bodyProperties : BodyProperties) (physicsEngine : PhysicsEngine3d) =

        // create either a character or a non-character body, ensuring we have at least one shape child in order to
        // avoid jolt error
        use scShapeSettings = new StaticCompoundShapeSettings ()
        let mass = PhysicsEngine3d.attachBodyShape bodyProperties bodyProperties.BodyShape scShapeSettings [] physicsEngine |> List.sum
        if scShapeSettings.NumSubShapes = 0u then
            let position = v3Zero
            let rotation = quatIdentity
            let centerOfMass = v3Zero
            scShapeSettings.AddShape (&position, &rotation, new EmptyShapeSettings (&centerOfMass))
        let layer =
            if bodyProperties.Enabled then
                if bodyProperties.BodyType.IsStatic
                then Constants.Physics.ObjectLayerNonMoving
                else Constants.Physics.ObjectLayerMoving
            else Constants.Physics.ObjectLayerDisabled
        let (motionType, representationType) =
            match bodyProperties.BodyType with
            | Static -> (MotionType.Static, Choice1Of3 ())
            | Kinematic -> (MotionType.Kinematic, Choice1Of3 ())
            | KinematicCharacter -> (MotionType.Kinematic, Choice2Of3 ())
            | Dynamic -> (MotionType.Dynamic, Choice1Of3 ())
            | DynamicCharacter -> (MotionType.Dynamic, Choice2Of3 ())
            | Vehicle ->
                match bodyProperties.VehicleProperties with
                | VehiclePropertiesJolt vehicleConstraintSettings -> (MotionType.Dynamic, Choice3Of3 vehicleConstraintSettings)
                | _ -> (MotionType.Dynamic, Choice1Of3 ())
        match representationType with
        | Choice1Of3 () ->

            // create body
            PhysicsEngine3d.createBodyNonCharacter mass layer motionType scShapeSettings bodyId bodyProperties physicsEngine |> ignore

        | Choice2Of3 () ->

            // character config
            let characterSettings = CharacterVirtualSettings ()
            characterSettings.CharacterPadding <- bodyProperties.CharacterProperties.CollisionPadding
            characterSettings.CollisionTolerance <- bodyProperties.CharacterProperties.CollisionTolerance
            characterSettings.EnhancedInternalEdgeRemoval <- true
            characterSettings.InnerBodyLayer <- layer
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
                  BodyCollisionMask = bodyProperties.CollisionMask }
            physicsEngine.CharacterVsCharacterCollision.Add character
            character.SetCharacterVsCharacterCollision physicsEngine.CharacterVsCharacterCollision
            physicsEngine.BodyUserData.Add (innerBodyID, bodyUserData)
            physicsEngine.Bodies.Add (bodyId, innerBodyID)

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

        | Choice3Of3 vehicleConstraintSettings ->

            // create vehicle offset COM shape
            let offset = v3Down * 1.25f // TODO: P1: expose this as parameter.
            let offsetComShapeSettings = new OffsetCenterOfMassShapeSettings (&offset, scShapeSettings)

            // create vehicle body
            let (bodyId, body) = PhysicsEngine3d.createBodyNonCharacter mass layer motionType offsetComShapeSettings bodyId bodyProperties physicsEngine
            
            // create vehicle constraint
            let vehicleConstraint = new VehicleConstraint (body, vehicleConstraintSettings)
            vehicleConstraint.SetVehicleCollisionTester (new VehicleCollisionTesterCastCylinder (layer, 1.0f))
            physicsEngine.VehicleConstraints.Add (bodyId, vehicleConstraint)
            physicsEngine.PhysicsContext.AddConstraint vehicleConstraint

            // register step listener
            physicsEngine.PhysicsContext.AddStepListener vehicleConstraint

        // HACK: optimize broad phase if we've taken in a lot of bodies.
        // NOTE: Might cause some intemittent run-time pauses when adding bodies.
        physicsEngine.BodyUnoptimizedCreationCount <- inc physicsEngine.BodyUnoptimizedCreationCount
        if physicsEngine.BodyUnoptimizedCreationCount = Constants.Physics.Collision3dBodyUnoptimizedCreationMax then
            physicsEngine.PhysicsContext.OptimizeBroadPhase ()
            physicsEngine.BodyUnoptimizedCreationCount <- 0

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

            // attempt to destroy wheeled vehicle controller
            match physicsEngine.VehicleConstraints.TryGetValue bodyId with
            | (true, vehicleConstraint) ->
                physicsEngine.PhysicsContext.RemoveStepListener vehicleConstraint
                physicsEngine.PhysicsContext.RemoveConstraint vehicleConstraint
                physicsEngine.VehicleConstraints.Remove bodyId |> ignore<bool>
                vehicleConstraint.Dispose ()
            | (false, _) -> ()

            // attempt to destroy non-character body
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
                        message.BodyJointProperties.BodyJointIndex = destroyBodyJointMessage.BodyJointId.BodyJointIndex)
                    |> ignore<int>
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
            let mutable layer =
                if setBodyEnabledMessage.Enabled then
                    if physicsEngine.PhysicsContext.BodyInterface.GetMotionType &bodyID = MotionType.Static
                    then Constants.Physics.ObjectLayerNonMoving
                    else Constants.Physics.ObjectLayerMoving
                else Constants.Physics.ObjectLayerDisabled
            physicsEngine.PhysicsContext.BodyInterface.SetObjectLayer (&bodyID, &layer)
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

    static member private setBodyVehicleForwardInput (setBodyVehicleForwardInputMessage : SetBodyVehicleForwardInputMessage) physicsEngine =
        match physicsEngine.VehicleConstraints.TryGetValue setBodyVehicleForwardInputMessage.BodyId with
        | (true, vehicleConstraint) ->
            let wheeledVehicleController = vehicleConstraint.GetController<WheeledVehicleController> ()
            wheeledVehicleController.ForwardInput <- setBodyVehicleForwardInputMessage.ForwardInput
        | (false, _) -> ()

    static member private setBodyVehicleRightInput (setBodyVehicleRightInputMessage : SetBodyVehicleRightInputMessage) physicsEngine =
        match physicsEngine.VehicleConstraints.TryGetValue setBodyVehicleRightInputMessage.BodyId with
        | (true, vehicleConstraint) ->
            let wheeledVehicleController = vehicleConstraint.GetController<WheeledVehicleController> ()
            wheeledVehicleController.RightInput <- setBodyVehicleRightInputMessage.RightInput
        | (false, _) -> ()

    static member private setBodyVehicleBrakeInput (setBodyVehicleBrakeInputMessage : SetBodyVehicleBrakeInputMessage) physicsEngine =
        match physicsEngine.VehicleConstraints.TryGetValue setBodyVehicleBrakeInputMessage.BodyId with
        | (true, vehicleConstraint) ->
            let wheeledVehicleController = vehicleConstraint.GetController<WheeledVehicleController> ()
            wheeledVehicleController.BrakeInput <- setBodyVehicleBrakeInputMessage.BrakeInput
        | (false, _) -> ()

    static member private setBodyVehicleHandBrakeInput (setBodyVehicleHandBrakeInputMessage : SetBodyVehicleHandBrakeInputMessage) physicsEngine =
        match physicsEngine.VehicleConstraints.TryGetValue setBodyVehicleHandBrakeInputMessage.BodyId with
        | (true, vehicleConstraint) ->
            let wheeledVehicleController = vehicleConstraint.GetController<WheeledVehicleController> ()
            wheeledVehicleController.HandBrakeInput <- setBodyVehicleHandBrakeInputMessage.HandBrakeInput
        | (false, _) -> ()

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
        | SetBodyVehicleForwardInputMessage setBodyVehicleForwardInputMessage -> PhysicsEngine3d.setBodyVehicleForwardInput setBodyVehicleForwardInputMessage physicsEngine
        | SetBodyVehicleRightInputMessage setBodyVehicleRightInputMessage -> PhysicsEngine3d.setBodyVehicleRightInput setBodyVehicleRightInputMessage physicsEngine
        | SetBodyVehicleBrakeInputMessage setBodyVehicleBrakeInputMessage -> PhysicsEngine3d.setBodyVehicleBrakeInput setBodyVehicleBrakeInputMessage physicsEngine
        | SetBodyVehicleHandBrakeInputMessage setBodyVehicleHandBrakeInputMessage -> PhysicsEngine3d.setBodyVehicleHandBrakeInput setBodyVehicleHandBrakeInputMessage physicsEngine
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
        // we use 3 layers: one for non-moving objects, one for moving objects, and one for disabled objects.
        // we use a 1-to-1 mapping between object layers and broadphase layers.
        let layerCount = 3u
        let objectLayerPairFilter = new ObjectLayerPairFilterTable (layerCount)
        objectLayerPairFilter.EnableCollision (Constants.Physics.ObjectLayerNonMoving, Constants.Physics.ObjectLayerMoving)
        objectLayerPairFilter.EnableCollision (Constants.Physics.ObjectLayerMoving, Constants.Physics.ObjectLayerMoving)
        let broadPhaseLayerInterface = new BroadPhaseLayerInterfaceTable (layerCount, layerCount)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (Constants.Physics.ObjectLayerNonMoving, Constants.Physics.BroadPhaseLayerNonMoving)
        broadPhaseLayerInterface.MapObjectToBroadPhaseLayer (Constants.Physics.ObjectLayerMoving, Constants.Physics.BroadPhaseLayerMoving)
        let objectVsBroadPhaseLayerFilter = new ObjectVsBroadPhaseLayerFilterTable (broadPhaseLayerInterface, layerCount, objectLayerPairFilter, layerCount)

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
        let bodyUserData = dictPlus (HashIdentity.FromFunctions (fun (bodyID : BodyID) -> int bodyID.ID) (fun (bodyID : BodyID) (bodyID2 : BodyID) -> bodyID.ID = bodyID2.ID)) []

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

        // create job system
        let jobSystem =
            // TODO: P1: expose this from the wrapper and then uncomment.
            //if Constants.Engine.RunSynchronously
            //then new JobSystemSingleThreaded ()
            //else
                let mutable jobSystemConfig = JobSystemThreadPoolConfig ()
                jobSystemConfig.maxJobs <- uint Constants.Physics.Collision3dJobsMax
                jobSystemConfig.maxBarriers <- uint Constants.Physics.Collision3dBarriersMax
                jobSystemConfig.numThreads <- Constants.Physics.Collision3dThreads
                new JobSystemThreadPool (&jobSystemConfig)

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
          VehicleConstraints = dictPlus HashIdentity.Structural []
          BodyUnoptimizedCreationCount = 0
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

        member physicsEngine.GravityDefault =
            Constants.Physics.GravityDefault

        member physicsEngine.Gravity =
            physicsEngine.PhysicsContext.Gravity

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

        member physicsEngine.GetBodySensor bodyId =
            match PhysicsEngine3d.tryGetBodyID bodyId physicsEngine with
            | ValueSome bodyID ->
                let mutable bodyLockRead = BodyLockRead ()
                try physicsEngine.PhysicsContext.BodyLockInterface.LockRead (&bodyID, &bodyLockRead)
                    if bodyLockRead.Succeeded
                    then bodyLockRead.Body.IsSensor
                    else Log.warnOnce "Failed to find expected body."; false
                finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockRead &bodyLockRead
            | ValueNone -> failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetWheelSpeedAtClutch bodyId =
            match physicsEngine.VehicleConstraints.TryGetValue bodyId with
            | (true, vehicleConstraint) ->
                let controller = vehicleConstraint.GetController<WheeledVehicleController> ()
                controller.WheelSpeedAtClutch
            | (false, _) -> 0.0f

        member physicsEngine.GetWheelModelMatrix (wheelModelRight, wheelModelUp, wheelIndex, bodyId) =
            match physicsEngine.VehicleConstraints.TryGetValue bodyId with
            | (true, vehicleConstraint) when wheelIndex >= 0 && vehicleConstraint.WheelsCount >= wheelIndex ->
                let mutable wheelModelRight = wheelModelRight
                let mutable wheelModelUp = wheelModelUp
                vehicleConstraint.GetWheelWorldTransform (wheelIndex, &wheelModelRight, &wheelModelUp)
            | (_, _) -> m4Identity

        member physicsEngine.GetWheelAngularVelocity (wheelIndex, bodyId) =
            match physicsEngine.VehicleConstraints.TryGetValue bodyId with
            | (true, vehicleConstraint) when wheelIndex >= 0 && vehicleConstraint.WheelsCount >= wheelIndex ->
                let wheel = vehicleConstraint.GetWheel<WheelWV> wheelIndex
                wheel.AngularVelocity
            | (_, _) -> 0.0f

        member physicsEngine.RayCast (ray, collisionMask, closestOnly) =
            let ray = new Ray (&ray.Origin, &ray.Direction)
            let bodyFilterID bodyID =
                match physicsEngine.BodyUserData.TryGetValue bodyID with
                | (true, bodyUserData) ->
                    let objectLayer = physicsEngine.PhysicsContext.BodyInterface.GetObjectLayer &bodyID
                    let bodyEnabled = objectLayer <> Constants.Physics.ObjectLayerDisabled
                    bodyEnabled && bodyUserData.BodyCollisionCategories &&& collisionMask <> 0
                | (false, _) -> false
            let bodyFilterInstance (body : Body) = bodyFilterID body.ID
            use bodyFilter = new BodyFilterLambda (bodyFilterID, bodyFilterInstance)
            let rayCastResults =
                if closestOnly then
                    let mutable rayCastResult = Unchecked.defaultof<RayCastResult>
                    physicsEngine.PhysicsContext.NarrowPhaseQuery.CastRay
                        (&ray, &rayCastResult, null, null, bodyFilter) |> ignore<bool>
                    List [rayCastResult]
                else
                    let rayCastSettings = RayCastSettings ()
                    let collectorType = CollisionCollectorType.AllHitSorted // TODO: consider allowing for unsorted hits.
                    let rayCastResults = List ()
                    physicsEngine.PhysicsContext.NarrowPhaseQuery.CastRay
                        (&ray, rayCastSettings, collectorType, rayCastResults, null, null, bodyFilter) |> ignore<bool>
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

        member physicsEngine.ShapeCast (shape, transformOpt, ray, collisionMask, closestOnly) =
            match PhysicsEngine3d.tryCreateShape shape with
            | Some shape ->
                let transformMatrix =
                    match transformOpt with
                    | Some transform ->
                        let mutable transform = transform
                        let transformMatrix = transform.Matrix
                        RMatrix4x4 &transformMatrix
                    | None ->
                        let mutable transform = m4Identity
                        RMatrix4x4 &transform
                let baseOffset = Double3 &ray.Origin
                let collectionType =
                    if closestOnly
                    then CollisionCollectorType.ClosestHit
                    else CollisionCollectorType.AllHitSorted
                let bodyFilterID bodyID =
                    match physicsEngine.BodyUserData.TryGetValue bodyID with
                    | (true, bodyUserData) ->
                        let objectLayer = physicsEngine.PhysicsContext.BodyInterface.GetObjectLayer &bodyID
                        let bodyEnabled = objectLayer <> Constants.Physics.ObjectLayerDisabled
                        bodyEnabled && bodyUserData.BodyCollisionCategories &&& collisionMask <> 0
                    | (false, _) -> false
                let bodyFilterInstance (body : Body) = bodyFilterID body.ID
                use bodyFilter = new BodyFilterLambda (bodyFilterID, bodyFilterInstance)
                let rayCastResults = List ()
                physicsEngine.PhysicsContext.NarrowPhaseQuery.CastShape
                    (shape, &transformMatrix, &ray.Direction, &baseOffset, collectionType, rayCastResults, null, null, bodyFilter, null) |> ignore<bool>
                [|for rayCastResult in rayCastResults do
                    let bodyID = rayCastResult.BodyID2 // second body since the first is the user-provided shape
                    let bodyId = physicsEngine.BodyUserData.[bodyID].BodyId
                    let subShapeID = rayCastResult.SubShapeID2
                    let position = ray.Origin + ray.Direction * rayCastResult.Fraction
                    let mutable bodyLockRead = BodyLockRead ()
                    let normal =
                        try physicsEngine.PhysicsContext.BodyLockInterface.LockRead (&bodyID, &bodyLockRead)
                            if bodyLockRead.Succeeded
                            then bodyLockRead.Body.GetWorldSpaceSurfaceNormal (&subShapeID, &position)
                            else Log.warnOnce "Failed to find expected body."; v3Up
                        finally physicsEngine.PhysicsContext.BodyLockInterface.UnlockRead &bodyLockRead
                    let bodyShapeIndex = { BodyId = bodyId; BodyShapeIndex = Constants.Physics.InternalIndex } // TODO: P1: see if we can get the user-defined shape index.
                    BodyIntersection.make bodyShapeIndex rayCastResult.Fraction position normal|]
            | None ->
                let shapeCaseName = getCaseName shape
                Log.warnOnce ("ShapeCast does not support shape type '" + shapeCaseName + "'.")
                [||]

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

        member physicsEngine.TryRender renderContext =
            match renderContext with
            | :? PhysicsEngine3dRenderContext as renderer ->
                let distanceMaxSquared =
                    Constants.Render.Body3dRenderDistanceMax *
                    Constants.Render.Body3dRenderDistanceMax
                use drawBodyFilter =
                    new BodyDrawFilterLambda (fun body ->
                        let bodyCenter = body.WorldSpaceBounds.Center
                        let bodyDistanceSquared = (bodyCenter - renderer.EyeCenter).MagnitudeSquared
                        body.Shape.Type <> ShapeType.HeightField && // NOTE: eliding terrain because without LOD, it's currently too expensive.
                        bodyDistanceSquared < distanceMaxSquared &&
                        renderer.EyeFrustum.Contains bodyCenter <> ContainmentType.Disjoint)
                let eyeCenter = renderer.EyeCenter
                renderer.DebugRenderer.SetCameraPosition &eyeCenter
                let drawSettings = renderer.DrawSettings
                physicsEngine.PhysicsContext.DrawBodies (&drawSettings, renderer.DebugRenderer, drawBodyFilter)
            | _ -> ()

        member physicsEngine.ClearInternal () =

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

            // clear vehicle step listeners and constraints
            for vehicleConstraint in physicsEngine.VehicleConstraints.Values do
                physicsEngine.PhysicsContext.RemoveStepListener vehicleConstraint
                physicsEngine.PhysicsContext.RemoveConstraint vehicleConstraint
            physicsEngine.VehicleConstraints.Clear ()

            // clear integration messages
            physicsEngine.IntegrationMessages.Clear ()

        member physicsEngine.CleanUp () =
            physicsEngine.JobSystem.Dispose ()
            physicsEngine.PhysicsContext.Dispose ()
            Foundation.Shutdown ()