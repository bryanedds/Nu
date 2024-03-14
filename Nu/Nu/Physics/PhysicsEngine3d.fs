// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open BulletSharp
open Prime

type [<CustomEquality; NoComparison>] private UnscaledPointsKey =
    { HashCode : int
      Vertices : Vector3 array }

    static member hash chs =
        chs.HashCode

    static member equals left right =
        left.HashCode = right.HashCode &&
        Linq.Enumerable.SequenceEqual (left.Vertices, right.Vertices)

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

/// The 3d implementation of PhysicsEngine in terms of Bullet Physics.
/// TODO: only record the collisions for bodies that have event subscriptions associated with them?
type [<ReferenceEquality>] PhysicsEngine3d =
    private
        { PhysicsContext : DynamicsWorld
          Constraints : Dictionary<JointId, TypedConstraint>
          NonStaticBodies : Dictionary<BodyId, Vector3 option * RigidBody>
          Bodies : Dictionary<BodyId, RigidBody>
          Ghosts : Dictionary<BodyId, Vector3 option * GhostObject>
          Objects : Dictionary<BodyId, CollisionObject>
          Actions : Dictionary<BodyId, IAction>
          CollisionsFiltered : Dictionary<BodyId * BodyId, Vector3>
          CollisionsGround : Dictionary<BodyId, Vector3 List>
          CollisionConfiguration : CollisionConfiguration
          CollisionDispatcher : Dispatcher
          BroadPhaseInterface : BroadphaseInterface
          ConstraintSolverPool : ConstraintSolverPoolMultiThreaded
          ConstraintSolver : ConstraintSolver
          TryGetAssetFilePath : AssetTag -> string option
          TryGetStaticModelMetadata : StaticModel AssetTag -> OpenGL.PhysicallyBased.PhysicallyBasedModel option
          UnscaledPointsCached : Dictionary<UnscaledPointsKey, Vector3 array>
          IntegrationMessages : IntegrationMessage List }

    static member private handleCollision physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) normal =
        let bodyCollisionMessage =
            { BodyShapeSource = { BodyId = bodyId; ShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; ShapeIndex = 0 }
              Normal = normal }
        let integrationMessage = BodyCollisionMessage bodyCollisionMessage
        physicsEngine.IntegrationMessages.Add integrationMessage
    
    static member private handleSeparation physicsEngine (bodyId : BodyId) (bodyId2 : BodyId) =
        let bodySeparationMessage =
            { BodyShapeSource = { BodyId = bodyId; ShapeIndex = 0 }
              BodyShapeSource2 = { BodyId = bodyId2; ShapeIndex = 0 }}
        let integrationMessage = BodySeparationMessage bodySeparationMessage
        physicsEngine.IntegrationMessages.Add integrationMessage

    static member private configureBodyShapeProperties (_ : BodyProperties) (_ : BodyShapeProperties option) (shape : CollisionShape) =
        shape.Margin <- Constants.Physics.Collision3dMargin

    static member private configureCollisionObjectProperties (bodyProperties : BodyProperties) (object : CollisionObject) =
        match bodyProperties.Enabled with
        | true -> object.ActivationState <- ActivationState.ActiveTag
        | false -> object.ActivationState <- ActivationState.DisableSimulation
        object.Friction <- bodyProperties.Friction
        object.Restitution <- bodyProperties.Restitution
        match bodyProperties.CollisionDetection with
        | Discontinuous ->
            object.CcdMotionThreshold <- 0.0f
            object.CcdSweptSphereRadius <- 0.0f
        | Continuous (motionThreshold, sweptSphereRadius) ->
            object.CcdMotionThreshold <- motionThreshold
            object.CcdSweptSphereRadius <- sweptSphereRadius
        match bodyProperties.BodyType with
        | Static ->
            object.CollisionFlags <- object.CollisionFlags ||| CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.KinematicObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.CharacterObject
        | Kinematic ->
            object.CollisionFlags <- object.CollisionFlags ||| CollisionFlags.KinematicObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.CharacterObject
        | KinematicCharacter ->
            object.CollisionFlags <- object.CollisionFlags ||| CollisionFlags.CharacterObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.KinematicObject
        | Dynamic ->
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.StaticObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.KinematicObject
            object.CollisionFlags <- object.CollisionFlags &&& ~~~CollisionFlags.CharacterObject

    static member private configureBodyProperties (bodyProperties : BodyProperties) (body : RigidBody) gravity =
        PhysicsEngine3d.configureCollisionObjectProperties bodyProperties body
        body.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center, bodyProperties.Rotation, v3One)
        if bodyProperties.SleepingAllowed // TODO: see if we can find a more reliable way to disable sleeping.
        then body.SetSleepingThresholds (Constants.Physics.SleepingThresholdLinear, Constants.Physics.SleepingThresholdAngular)
        else body.SetSleepingThresholds (0.0f, 0.0f)
        body.LinearVelocity <- bodyProperties.LinearVelocity
        body.LinearFactor <- if bodyProperties.BodyType = Static then v3Zero else v3One
        body.AngularVelocity <- bodyProperties.AngularVelocity
        body.AngularFactor <- if bodyProperties.BodyType = Static then v3Zero else bodyProperties.AngularFactor
        body.SetDamping (bodyProperties.LinearDamping, bodyProperties.AngularDamping)
        body.Gravity <- match bodyProperties.GravityOverride with Some gravityOverride -> gravityOverride | None -> gravity

    static member private attachBodyBox bodySource (bodyProperties : BodyProperties) (bodyBox : BodyBox) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        let box = new BoxShape (bodyBox.Size * 0.5f)
        PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodyBox.PropertiesOpt box
        box.LocalScaling <- bodyProperties.Scale
        box.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyBox.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodyBox.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = bodyBox.Size.X * bodyBox.Size.Y * bodyBox.Size.Z
                volume * density
            | Mass mass -> mass
        let inertia = box.CalculateLocalInertia mass
        compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, box)
        (center, mass, inertia, id) :: centerMassInertiaDisposes

    static member private attachBodySphere bodySource (bodyProperties : BodyProperties) (bodySphere : BodySphere) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        let sphere = new SphereShape (bodySphere.Radius)
        PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodySphere.PropertiesOpt sphere
        sphere.LocalScaling <- bodyProperties.Scale
        sphere.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodySphere.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodySphere.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = 4.0f / 3.0f * MathF.PI * pown bodySphere.Radius 3
                volume * density
            | Mass mass -> mass
        let inertia = sphere.CalculateLocalInertia mass
        compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, sphere)
        (center, mass, inertia, id) :: centerMassInertiaDisposes

    static member private attachBodyCapsule bodySource (bodyProperties : BodyProperties) (bodyCapsule : BodyCapsule) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        let capsule = new CapsuleShape (bodyCapsule.Radius, bodyCapsule.Height)
        PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodyCapsule.PropertiesOpt capsule
        capsule.LocalScaling <- bodyProperties.Scale
        capsule.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyCapsule.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        let center =
            match bodyCapsule.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = MathF.PI * pown bodyCapsule.Radius 2 * (4.0f / 3.0f * bodyCapsule.Radius * bodyCapsule.Height)
                volume * density
            | Mass mass -> mass
        let inertia = capsule.CalculateLocalInertia mass
        compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, capsule)
        (center, mass, inertia, id) :: centerMassInertiaDisposes

    static member private attachBodyBoxRounded bodySource (bodyProperties : BodyProperties) (bodyBoxRounded : BodyBoxRounded) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        Log.info "Rounded box not yet implemented via PhysicsEngine3d; creating a normal box instead."
        let bodyBox = { Size = bodyBoxRounded.Size; TransformOpt = bodyBoxRounded.TransformOpt; PropertiesOpt = bodyBoxRounded.PropertiesOpt }
        PhysicsEngine3d.attachBodyBox bodySource bodyProperties bodyBox compoundShape centerMassInertiaDisposes

    static member private attachBodyConvexHull bodySource (bodyProperties : BodyProperties) (bodyPoints : BodyPoints) (compoundShape : CompoundShape) centerMassInertiaDisposes physicsEngine =
        let unscaledPointsKey = UnscaledPointsKey.make bodyPoints.Points
        let (optimized, vertices) =
            match physicsEngine.UnscaledPointsCached.TryGetValue unscaledPointsKey with
            | (true, unscaledVertices) -> (true, unscaledVertices)
            | (false, _) -> (false, bodyPoints.Points)
        let hull = new ConvexHullShape (vertices)
        PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodyPoints.PropertiesOpt hull
        if not optimized then
            hull.OptimizeConvexHull ()
            let unscaledPoints = Array.ofSeq hull.UnscaledPoints
            physicsEngine.UnscaledPointsCached.Add (unscaledPointsKey, unscaledPoints)
        hull.LocalScaling <- bodyProperties.Scale
        hull.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyPoints.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation.
        let mutable min = v3Zero
        let mutable max = v3Zero
        hull.GetAabb (m4Identity, &min, &max)
        let center =
            match bodyPoints.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let box = box3 min max
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        let inertia = hull.CalculateLocalInertia mass
        compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, hull)
        (center, mass, inertia, id) :: centerMassInertiaDisposes

    static member private attachBodyBvhTriangles bodySource (bodyProperties : BodyProperties) (bodyGeometry : BodyGeometry) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        let vertexArray = new TriangleIndexVertexArray (Array.init bodyGeometry.Vertices.Length id, bodyGeometry.Vertices)
        let shape = new BvhTriangleMeshShape (vertexArray, true)
        shape.BuildOptimizedBvh ()
        PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodyGeometry.PropertiesOpt shape
        shape.LocalScaling <- bodyProperties.Scale
        shape.UserObject <-
            { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
              ShapeIndex = match bodyGeometry.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
        // NOTE: we approximate volume with the volume of a bounding box.
        // TODO: use a more accurate volume calculation?
        let mutable min = v3Zero
        let mutable max = v3Zero
        shape.GetAabb (m4Identity, &min, &max)
        let center =
            match bodyGeometry.TransformOpt with
            | Some transform -> transform.Translation
            | None -> v3Zero
        let box = box3 min max
        let mass =
            match bodyProperties.Substance with
            | Density density ->
                let volume = box.Width * box.Height * box.Depth
                volume * density
            | Mass mass -> mass
        let inertia = shape.CalculateLocalInertia mass
        compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, shape)
        (center, mass, inertia, id) :: centerMassInertiaDisposes

    static member private attachBodyGeometry bodySource (bodyProperties : BodyProperties) (bodyGeometry : BodyGeometry) (compoundShape : CompoundShape) centerMassInertiaDisposes physicsEngine =
        if bodyGeometry.Convex then
            let bodyPoints = { Points = bodyGeometry.Vertices; TransformOpt = bodyGeometry.TransformOpt; PropertiesOpt = bodyGeometry.PropertiesOpt }
            PhysicsEngine3d.attachBodyConvexHull bodySource bodyProperties bodyPoints compoundShape centerMassInertiaDisposes physicsEngine
        else PhysicsEngine3d.attachBodyBvhTriangles bodySource bodyProperties bodyGeometry compoundShape centerMassInertiaDisposes

    // TODO: add some error logging.
    static member private attachBodyStaticModel bodySource (bodyProperties : BodyProperties) (bodyStaticModel : BodyStaticModel) (compoundShape : CompoundShape) centerMassInertiaDisposes physicsEngine =
        match physicsEngine.TryGetStaticModelMetadata bodyStaticModel.StaticModel with
        | Some staticModel ->
            Seq.fold (fun centerMassInertiaDisposes i ->
                let surface = staticModel.Surfaces.[i]
                let transform =
                    match bodyStaticModel.TransformOpt with
                    | Some transform ->
                        Affine.make
                            (Vector3.Transform (transform.Translation, surface.SurfaceMatrix))
                            (Quaternion.CreateFromRotationMatrix (Matrix4x4.CreateFromQuaternion transform.Rotation * surface.SurfaceMatrix))
                            (Vector3.Transform (transform.Scale, surface.SurfaceMatrix))
                    | None ->
                        Affine.make
                            (Vector3.Transform (v3Zero, surface.SurfaceMatrix))
                            (Quaternion.CreateFromRotationMatrix (Matrix4x4.CreateFromQuaternion quatIdentity * surface.SurfaceMatrix))
                            (Vector3.Transform (v3One, surface.SurfaceMatrix))
                let bodyStaticModelSurface = { StaticModel = bodyStaticModel.StaticModel; SurfaceIndex = i; Convex = bodyStaticModel.Convex; TransformOpt = Some transform; PropertiesOpt = bodyStaticModel.PropertiesOpt }
                PhysicsEngine3d.attachBodyStaticModelSurface bodySource bodyProperties bodyStaticModelSurface compoundShape centerMassInertiaDisposes physicsEngine)
                centerMassInertiaDisposes
                [0 .. dec staticModel.Surfaces.Length]
        | None -> centerMassInertiaDisposes

    // TODO: add some error logging.
    static member private attachBodyStaticModelSurface bodySource (bodyProperties : BodyProperties) (bodyStaticModelSurface : BodyStaticModelSurface) (compoundShape : CompoundShape) centerMassInertiaDisposes physicsEngine =
        match physicsEngine.TryGetStaticModelMetadata bodyStaticModelSurface.StaticModel with
        | Some staticModel ->
            if  bodyStaticModelSurface.SurfaceIndex > -1 &&
                bodyStaticModelSurface.SurfaceIndex < staticModel.Surfaces.Length then
                let geometry = staticModel.Surfaces.[bodyStaticModelSurface.SurfaceIndex].PhysicallyBasedGeometry
                let bodyGeometry = { Vertices = geometry.Vertices; Convex = bodyStaticModelSurface.Convex; TransformOpt = bodyStaticModelSurface.TransformOpt; PropertiesOpt = bodyStaticModelSurface.PropertiesOpt }
                PhysicsEngine3d.attachBodyGeometry bodySource bodyProperties bodyGeometry compoundShape centerMassInertiaDisposes physicsEngine
            else centerMassInertiaDisposes
        | None -> centerMassInertiaDisposes

    static member private attachBodyTerrain tryGetAssetFilePath bodySource (bodyProperties : BodyProperties) (bodyTerrain : BodyTerrain) (compoundShape : CompoundShape) centerMassInertiaDisposes =
        let resolution = bodyTerrain.Resolution
        let bounds = bodyTerrain.Bounds
        match HeightMap.tryGetMetadata tryGetAssetFilePath bounds v2One bodyTerrain.HeightMap with
        | Some heightMapMetadata ->
            let heights = Array.zeroCreate heightMapMetadata.HeightsNormalized.Length
            for i in 0 .. dec heightMapMetadata.HeightsNormalized.Length do
                heights.[i] <- heightMapMetadata.HeightsNormalized.[i] * bounds.Height
            let handle = GCHandle.Alloc (heights, GCHandleType.Pinned)
            try let positionsPtr = handle.AddrOfPinnedObject ()
                let terrain = new HeightfieldTerrainShape (resolution.X, resolution.Y, positionsPtr, 1.0f, 0.0f, bounds.Height, 1, PhyScalarType.Single, false)
                terrain.LocalScaling <- v3 (bounds.Width / single (dec resolution.X)) 1.0f (bounds.Depth / single (dec resolution.Y))
                terrain.SetFlipTriangleWinding true // match terrain winding order - I think!
                PhysicsEngine3d.configureBodyShapeProperties bodyProperties bodyTerrain.PropertiesOpt terrain
                terrain.UserObject <-
                    { BodyId = { BodySource = bodySource; BodyIndex = bodyProperties.BodyIndex }
                      ShapeIndex = match bodyTerrain.PropertiesOpt with Some p -> p.ShapeIndex | None -> 0 }
                let center =
                    match bodyTerrain.TransformOpt with
                    | Some transform -> transform.Translation
                    | None -> v3Zero
                let mass = 0.0f // infinite mass
                let inertia = terrain.CalculateLocalInertia mass
                compoundShape.AddChildShape (Matrix4x4.CreateTranslation center, terrain)
                (center, mass, inertia, fun () -> handle.Free ()) :: centerMassInertiaDisposes
            with _ -> centerMassInertiaDisposes
        | None -> centerMassInertiaDisposes

    static member private attachBodyShapes tryGetAssetFilePath bodySource bodyProperties bodyShapes compoundShape centerMassInertiaDisposes physicsEngine =
        List.fold (fun centerMassInertiaDisposes bodyShape ->
            let centerMassInertiaDisposes' = PhysicsEngine3d.attachBodyShape tryGetAssetFilePath bodySource bodyProperties bodyShape compoundShape centerMassInertiaDisposes physicsEngine
            centerMassInertiaDisposes' @ centerMassInertiaDisposes)
            centerMassInertiaDisposes
            bodyShapes

    static member private attachBodyShape tryGetAssetFilePath bodySource bodyProperties bodyShape compoundShape centerMassInertiaDisposes physicsEngine =
        match bodyShape with
        | BodyEmpty -> centerMassInertiaDisposes
        | BodyBox bodyBox -> PhysicsEngine3d.attachBodyBox bodySource bodyProperties bodyBox compoundShape centerMassInertiaDisposes
        | BodySphere bodySphere -> PhysicsEngine3d.attachBodySphere bodySource bodyProperties bodySphere compoundShape centerMassInertiaDisposes
        | BodyCapsule bodyCapsule -> PhysicsEngine3d.attachBodyCapsule bodySource bodyProperties bodyCapsule compoundShape centerMassInertiaDisposes
        | BodyBoxRounded bodyBoxRounded -> PhysicsEngine3d.attachBodyBoxRounded bodySource bodyProperties bodyBoxRounded compoundShape centerMassInertiaDisposes
        | BodyPoints bodyPoints -> PhysicsEngine3d.attachBodyConvexHull bodySource bodyProperties bodyPoints compoundShape centerMassInertiaDisposes physicsEngine
        | BodyGeometry bodyGeometry -> PhysicsEngine3d.attachBodyGeometry bodySource bodyProperties bodyGeometry compoundShape centerMassInertiaDisposes physicsEngine
        | BodyStaticModel bodyStaticModel -> PhysicsEngine3d.attachBodyStaticModel bodySource bodyProperties bodyStaticModel compoundShape centerMassInertiaDisposes physicsEngine
        | BodyStaticModelSurface bodyStaticModelSurface -> PhysicsEngine3d.attachBodyStaticModelSurface bodySource bodyProperties bodyStaticModelSurface compoundShape centerMassInertiaDisposes physicsEngine
        | BodyTerrain bodyTerrain -> PhysicsEngine3d.attachBodyTerrain tryGetAssetFilePath bodySource bodyProperties bodyTerrain compoundShape centerMassInertiaDisposes
        | BodyShapes bodyShapes -> PhysicsEngine3d.attachBodyShapes tryGetAssetFilePath bodySource bodyProperties bodyShapes compoundShape centerMassInertiaDisposes physicsEngine

    static member private createBody3 attachBodyShape (bodyId : BodyId) (bodyProperties : BodyProperties) physicsEngine =
        let (compoundShape, centerMassInertiaDisposes) =
            let compoundShape = new CompoundShape ()
            let centerMassInertiaDisposes = attachBodyShape bodyProperties compoundShape []
            (compoundShape, centerMassInertiaDisposes)
        let shape =
            if  compoundShape.ChildList.Count = 1 &&
                (compoundShape.ChildList.[0].Transform = m4Identity ||
                 bodyProperties.BodyType = KinematicCharacter) then // attempt to strip compound shape
                let shape = compoundShape.ChildList.[0].ChildShape
                compoundShape.RemoveChildShape shape
                compoundShape.Dispose ()
                shape
            else compoundShape
        let (_, mass, inertia, disposer) =
            // TODO: make this more accurate by making each c weighted proportionately to its respective m.
            List.fold (fun (c, m, i, d) (c', m', i', d') -> (c + c', m + m', i + i', fun () -> d (); d' ())) (v3Zero, 0.0f, v3Zero, id) centerMassInertiaDisposes
        let userIndex = if bodyId.BodyIndex = Constants.Physics.InternalIndex then -1 else 1
        if bodyProperties.Sensor then
            let ghost = new GhostObject ()
            ghost.CollisionShape <- shape
            ghost.CollisionFlags <- ghost.CollisionFlags &&& ~~~CollisionFlags.NoContactResponse
            ghost.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center, bodyProperties.Rotation, bodyProperties.Scale)
            ghost.UserObject <- { BodyId = bodyId; Dispose = disposer }
            ghost.UserIndex <- userIndex
            PhysicsEngine3d.configureCollisionObjectProperties bodyProperties ghost
            physicsEngine.PhysicsContext.AddCollisionObject (ghost, bodyProperties.CollisionCategories, bodyProperties.CollisionMask)
            if physicsEngine.Ghosts.TryAdd (bodyId, (None, ghost))
            then physicsEngine.Objects.Add (bodyId, ghost)
            else Log.debug ("Could not add body for '" + scstring bodyId + "'.")
        elif bodyProperties.BodyType = KinematicCharacter then
            match shape with
            | :? ConvexShape as convexShape ->
                let shapeTransform =
                    match BodyShape.getTransformOpt bodyProperties.BodyShape with
                    | Some transform -> transform
                    | None -> Affine.Identity
                if shapeTransform.Rotation = quatIdentity && shapeTransform.Scale = v3One then
                    let ghostPairCallback = new GhostPairCallback ()
                    physicsEngine.PhysicsContext.Broadphase.OverlappingPairCache.SetInternalGhostPairCallback ghostPairCallback
                    let ghost = new PairCachingGhostObject ()
                    ghost.CollisionShape <- convexShape
                    ghost.CollisionFlags <- ghost.CollisionFlags &&& ~~~CollisionFlags.NoContactResponse
                    ghost.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center + shapeTransform.Translation, bodyProperties.Rotation, bodyProperties.Scale)
                    ghost.UserObject <- { BodyId = bodyId; Dispose = disposer }
                    ghost.UserIndex <- userIndex
                    PhysicsEngine3d.configureCollisionObjectProperties bodyProperties ghost
                    physicsEngine.PhysicsContext.AddCollisionObject (ghost, bodyProperties.CollisionCategories, bodyProperties.CollisionMask)
                    let offsetOpt = if shapeTransform.Translation <> v3Zero then Some shapeTransform.Translation else None
                    if physicsEngine.Ghosts.TryAdd (bodyId, (offsetOpt, ghost :> GhostObject))
                    then physicsEngine.Objects.Add (bodyId, ghost)
                    else Log.debug ("Could not add body for '" + scstring bodyId + "'.")
                    let mutable up = v3Up
                    let characterController = new KinematicCharacterController (ghost, convexShape, bodyProperties.StepHeight, &up)
                    physicsEngine.PhysicsContext.AddAction characterController
                    // TODO: P0: implement removal from Actions.

                else failwith "Locally rotated / scaled body shapes not supported for BodyType.Character."
            | _ -> failwith "Non-convex body shapes not supported for BodyType.Character."
        else
            let constructionInfo = new RigidBodyConstructionInfo (mass, new DefaultMotionState (), shape, inertia)
            let body = new RigidBody (constructionInfo)
            body.WorldTransform <- Matrix4x4.CreateFromTrs (bodyProperties.Center, bodyProperties.Rotation, bodyProperties.Scale)
            body.UserObject <- { BodyId = bodyId; Dispose = disposer }
            body.UserIndex <- userIndex
            PhysicsEngine3d.configureBodyProperties bodyProperties body physicsEngine.PhysicsContext.Gravity
            physicsEngine.PhysicsContext.AddRigidBody (body, bodyProperties.CollisionCategories, bodyProperties.CollisionMask)
            if physicsEngine.Bodies.TryAdd (bodyId, body) then
                if not body.IsStaticObject then physicsEngine.NonStaticBodies.Add (bodyId, (bodyProperties.GravityOverride, body))
                physicsEngine.Objects.Add (bodyId, body)
            else Log.debug ("Could not add body for '" + scstring bodyId + "'.")

    static member private createBody4 bodyShape (bodyId : BodyId) bodyProperties physicsEngine =
        PhysicsEngine3d.createBody3 (fun ps cs cmas ->
            PhysicsEngine3d.attachBodyShape physicsEngine.TryGetAssetFilePath bodyId.BodySource ps bodyShape cs cmas physicsEngine)
            bodyId bodyProperties physicsEngine

    static member private createBody (createBodyMessage : CreateBodyMessage) physicsEngine =
        let bodyId = createBodyMessage.BodyId
        let bodyProperties = createBodyMessage.BodyProperties
        PhysicsEngine3d.createBody4 bodyProperties.BodyShape bodyId bodyProperties physicsEngine

    static member private createBodies (createBodiesMessage : CreateBodiesMessage) physicsEngine =
        List.iter (fun (bodyProperties : BodyProperties) ->
            let createBodyMessage =
                { BodyId = { BodySource = createBodiesMessage.BodySource; BodyIndex = bodyProperties.BodyIndex }
                  BodyProperties = bodyProperties }
            PhysicsEngine3d.createBody createBodyMessage physicsEngine)
            createBodiesMessage.BodiesProperties

    static member private destroyBody (destroyBodyMessage : DestroyBodyMessage) physicsEngine =
        let bodyId = destroyBodyMessage.BodyId
        match physicsEngine.Objects.TryGetValue bodyId with
        | (true, object) ->
            match object with
            | :? RigidBody as body ->
                physicsEngine.Objects.Remove bodyId |> ignore
                physicsEngine.NonStaticBodies.Remove bodyId |> ignore
                physicsEngine.Bodies.Remove bodyId |> ignore
                physicsEngine.PhysicsContext.RemoveRigidBody body
                let userObject = body.UserObject :?> BodyUserObject
                userObject.Dispose ()
            | :? GhostObject as ghost ->
                physicsEngine.Objects.Remove bodyId |> ignore
                physicsEngine.Ghosts.Remove bodyId |> ignore
                physicsEngine.PhysicsContext.RemoveCollisionObject ghost
                let userObject = ghost.UserObject :?> BodyUserObject
                userObject.Dispose ()
            | _ -> ()
        | (false, _) -> ()

    static member private destroyBodies (destroyBodiesMessage : DestroyBodiesMessage) physicsEngine =
        List.iter (fun bodyId ->
            PhysicsEngine3d.destroyBody { BodyId = bodyId } physicsEngine)
            destroyBodiesMessage.BodyIds

    static member private createJoint (createJointMessage : CreateJointMessage) physicsEngine =
        let jointProperties = createJointMessage.JointProperties
        let jointId = { JointSource = createJointMessage.JointSource; JointIndex = jointProperties.JointIndex }
        match jointProperties.JointDevice with
        | JointEmpty -> ()
        | JointAngle jointAngle ->
            match (physicsEngine.Bodies.TryGetValue jointAngle.TargetId, physicsEngine.Bodies.TryGetValue jointAngle.TargetId2) with
            | ((true, body), (true, body2)) ->
                let hinge = new HingeConstraint (body, body2, jointAngle.Anchor, jointAngle.Anchor2, jointAngle.Axis, jointAngle.Axis2)
                hinge.SetLimit (jointAngle.AngleMin, jointAngle.AngleMax, jointAngle.Softness, jointAngle.BiasFactor, jointAngle.RelaxationFactor)
                hinge.BreakingImpulseThreshold <- jointAngle.BreakImpulseThreshold
                physicsEngine.PhysicsContext.AddConstraint (hinge, false)
                if physicsEngine.Constraints.TryAdd (jointId, hinge)
                then () // nothing to do
                else Log.debug ("Could not add joint via '" + scstring createJointMessage + "'.")
            | (_, _) -> Log.debug "Could not create a joint for one or more non-existent bodies."
        | _ -> failwithnie ()

    static member private createJoints (createJointsMessage : CreateJointsMessage) physicsEngine =
        List.iter (fun (jointProperties : JointProperties) ->
            let createJointMessage = { JointSource = createJointsMessage.JointsSource; JointProperties = jointProperties }
            PhysicsEngine3d.createJoint createJointMessage physicsEngine)
            createJointsMessage.JointsProperties

    static member private destroyJoint (destroyJointMessage : DestroyJointMessage) physicsEngine =
        match physicsEngine.Constraints.TryGetValue destroyJointMessage.JointId with
        | (true, contrain) ->
            physicsEngine.Constraints.Remove destroyJointMessage.JointId |> ignore
            physicsEngine.PhysicsContext.RemoveConstraint contrain
        | (false, _) -> ()

    static member private destroyJoints (destroyJointsMessage : DestroyJointsMessage) physicsEngine =
        List.iter (fun jointId ->
            PhysicsEngine3d.destroyJoint { JointId = jointId } physicsEngine)
            destroyJointsMessage.JointIds

    static member private setBodyEnabled (setBodyEnabledMessage : SetBodyEnabledMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyEnabledMessage.BodyId with
        | (true, object) ->
            object.ActivationState <-
                if setBodyEnabledMessage.Enabled
                then ActivationState.ActiveTag
                else ActivationState.DisableSimulation
        | (false, _) -> ()

    static member private setBodyCenter (setBodyCenterMessage : SetBodyCenterMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyCenterMessage.BodyId with
        | (true, object) ->
            let offsetOpt =
                match physicsEngine.Ghosts.TryGetValue setBodyCenterMessage.BodyId with
                | (true, (offsetOpt, _)) -> offsetOpt
                | (false, _) -> None
            let mutable transform = object.WorldTransform
            transform.Translation <- setBodyCenterMessage.Center + Option.defaultValue v3Zero offsetOpt
            object.WorldTransform <- transform
            object.Activate true // force activation so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyRotation (setBodyRotationMessage : SetBodyRotationMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyRotationMessage.BodyId with
        | (true, object) ->
            object.WorldTransform <- object.WorldTransform.SetRotation setBodyRotationMessage.Rotation
            object.Activate true // force activation so that a transform message will be produced
        | (false, _) -> ()

    static member private setBodyLinearVelocity (setBodyLinearVelocityMessage : SetBodyLinearVelocityMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyLinearVelocityMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            body.LinearVelocity <- setBodyLinearVelocityMessage.LinearVelocity
            body.Activate ()
        | (_, _) -> ()

    static member private setBodyAngularVelocity (setBodyAngularVelocityMessage : SetBodyAngularVelocityMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue setBodyAngularVelocityMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            body.AngularVelocity <- setBodyAngularVelocityMessage.AngularVelocity
            body.Activate ()
        | (_, _) -> ()

    static member private applyBodyLinearImpulse (applyBodyLinearImpulseMessage : ApplyBodyLinearImpulseMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyLinearImpulseMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            if not (Single.IsNaN applyBodyLinearImpulseMessage.LinearImpulse.X) then
                body.ApplyImpulse (applyBodyLinearImpulseMessage.LinearImpulse, applyBodyLinearImpulseMessage.Offset)
                body.Activate ()
            else Log.info ("Applying invalid linear impulse '" + scstring applyBodyLinearImpulseMessage.LinearImpulse + "'; this may destabilize Bullet.")
        | (_, _) -> ()

    static member private applyBodyAngularImpulse (applyBodyAngularImpulseMessage : ApplyBodyAngularImpulseMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyAngularImpulseMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            if not (Single.IsNaN applyBodyAngularImpulseMessage.AngularImpulse.X) then
                body.ApplyTorqueImpulse (applyBodyAngularImpulseMessage.AngularImpulse)
                body.Activate ()
            else Log.info ("Applying invalid angular impulse '" + scstring applyBodyAngularImpulseMessage.AngularImpulse + "'; this may destabilize Bullet.")
        | (_, _) -> ()

    static member private applyBodyForce (applyBodyForceMessage : ApplyBodyForceMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyForceMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            if not (Single.IsNaN applyBodyForceMessage.Force.X) then
                body.ApplyForce (applyBodyForceMessage.Force, applyBodyForceMessage.Offset)
                body.Activate ()
            else Log.info ("Applying invalid force '" + scstring applyBodyForceMessage.Force + "'; this may destabilize Bullet.")
        | (_, _) -> ()

    static member private applyBodyTorque (applyBodyTorqueMessage : ApplyBodyTorqueMessage) physicsEngine =
        match physicsEngine.Objects.TryGetValue applyBodyTorqueMessage.BodyId with
        | (true, (:? RigidBody as body)) ->
            if not (Single.IsNaN applyBodyTorqueMessage.Torque.X) then
                body.ApplyTorque applyBodyTorqueMessage.Torque
                body.Activate ()
            else Log.info ("Applying invalid torque '" + scstring applyBodyTorqueMessage.Torque + "'; this may destabilize Aether.")
        | (_, _) -> ()

    static member private setBodyObservable (setBodyObservableMessage : SetBodyObservableMessage) physicsEngine =
        match physicsEngine.Bodies.TryGetValue setBodyObservableMessage.BodyId with
        | (true, body) -> body.UserIndex <- if setBodyObservableMessage.Observable then 1 else -1
        | (false, _) ->
            match physicsEngine.Ghosts.TryGetValue setBodyObservableMessage.BodyId with
            | (true, (_, ghost)) -> ghost.UserIndex <- if setBodyObservableMessage.Observable then 1 else -1
            | (false, _) -> ()

    static member private handlePhysicsMessage physicsEngine physicsMessage =
        match physicsMessage with
        | CreateBodyMessage createBodyMessage -> PhysicsEngine3d.createBody createBodyMessage physicsEngine
        | CreateBodiesMessage createBodiesMessage -> PhysicsEngine3d.createBodies createBodiesMessage physicsEngine
        | DestroyBodyMessage destroyBodyMessage -> PhysicsEngine3d.destroyBody destroyBodyMessage physicsEngine
        | DestroyBodiesMessage destroyBodiesMessage -> PhysicsEngine3d.destroyBodies destroyBodiesMessage physicsEngine
        | CreateJointMessage createJointMessage -> PhysicsEngine3d.createJoint createJointMessage physicsEngine
        | CreateJointsMessage createJointsMessage -> PhysicsEngine3d.createJoints createJointsMessage physicsEngine
        | DestroyJointMessage destroyJointMessage -> PhysicsEngine3d.destroyJoint destroyJointMessage physicsEngine
        | DestroyJointsMessage destroyJointsMessage -> PhysicsEngine3d.destroyJoints destroyJointsMessage physicsEngine
        | SetBodyEnabledMessage setBodyEnabledMessage -> PhysicsEngine3d.setBodyEnabled setBodyEnabledMessage physicsEngine
        | SetBodyCenterMessage setBodyCenterMessage -> PhysicsEngine3d.setBodyCenter setBodyCenterMessage physicsEngine
        | SetBodyRotationMessage setBodyRotationMessage -> PhysicsEngine3d.setBodyRotation setBodyRotationMessage physicsEngine
        | SetBodyLinearVelocityMessage setBodyLinearVelocityMessage -> PhysicsEngine3d.setBodyLinearVelocity setBodyLinearVelocityMessage physicsEngine
        | SetBodyAngularVelocityMessage setBodyAngularVelocityMessage -> PhysicsEngine3d.setBodyAngularVelocity setBodyAngularVelocityMessage physicsEngine
        | ApplyBodyLinearImpulseMessage applyBodyLinearImpulseMessage -> PhysicsEngine3d.applyBodyLinearImpulse applyBodyLinearImpulseMessage physicsEngine
        | ApplyBodyAngularImpulseMessage applyBodyAngularImpulseMessage -> PhysicsEngine3d.applyBodyAngularImpulse applyBodyAngularImpulseMessage physicsEngine
        | ApplyBodyForceMessage applyBodyForceMessage -> PhysicsEngine3d.applyBodyForce applyBodyForceMessage physicsEngine
        | ApplyBodyTorqueMessage applyBodyTorqueMessage -> PhysicsEngine3d.applyBodyTorque applyBodyTorqueMessage physicsEngine
        | SetBodyObservableMessage setBodyObservableMessage -> PhysicsEngine3d.setBodyObservable setBodyObservableMessage physicsEngine
        | SetGravityMessage gravity ->

            // set gravity of all non-static bodies
            physicsEngine.PhysicsContext.Gravity <- gravity
            for bodyEntry in physicsEngine.NonStaticBodies do
                let (gravityOverride, body) = bodyEntry.Value
                match gravityOverride with
                | Some gravity -> body.Gravity <- gravity
                | None -> body.Gravity <- gravity

        | ClearPhysicsMessageInternal ->

            // collect body user objects as we proceed
            let bodyUserObjects = List ()

            // destroy constraints
            for constrain in physicsEngine.Constraints.Values do
                physicsEngine.PhysicsContext.RemoveConstraint constrain
            physicsEngine.Constraints.Clear ()

            // destroy bullet objects
            for object in physicsEngine.Objects.Values do
                bodyUserObjects.Add (object.UserObject :?> BodyUserObject)
            physicsEngine.Objects.Clear ()

            // destroy ghosts
            for (_, ghost) in physicsEngine.Ghosts.Values do
                bodyUserObjects.Add (ghost.UserObject :?> BodyUserObject)
                physicsEngine.PhysicsContext.RemoveCollisionObject ghost
            physicsEngine.Ghosts.Clear ()

            // clear non-static bodies
            physicsEngine.NonStaticBodies.Clear ()

            // destroy bodies
            for body in physicsEngine.Bodies.Values do
                physicsEngine.PhysicsContext.RemoveRigidBody body
            physicsEngine.Bodies.Clear ()

            // dispose body user objects
            for bodyUserObject in bodyUserObjects do
                bodyUserObject.Dispose ()

            // clear integration messages
            physicsEngine.IntegrationMessages.Clear ()

    static member private integrate stepTime physicsEngine =        
        match (Constants.GameTime.DesiredFrameRate, stepTime) with
        | (StaticFrameRate frameRate, UpdateTime frames) ->
            let physicsStepAmount = 1.0f / single frameRate * single frames
            if physicsStepAmount > 0.0f then
                let stepsTaken = physicsEngine.PhysicsContext.StepSimulation (physicsStepAmount, 16, 1.0f / single (frameRate * 2L))
                ignore stepsTaken
        | (DynamicFrameRate _, ClockTime physicsStepAmount) ->
            if physicsStepAmount > 0.0f then
                // The following line is what Bullet seems to recommend (https://pybullet.org/Bullet/phpBB3/viewtopic.php?t=2438) -
                //let stepsTaken = physicsEngine.PhysicsContext.StepSimulation (physicsStepAmount, 16, 1.0f / 120.0f)
                // However, the following line of code seems to give smoother results -
                let stepsTaken = physicsEngine.PhysicsContext.StepSimulation (physicsStepAmount, 2, physicsStepAmount / 2.0f - 0.0001f)
                ignore stepsTaken
        | (_, _) -> failwithumf ()

    static member private createIntegrationMessages physicsEngine =

        // create collision entries
        let collisionsOld = physicsEngine.CollisionsFiltered
        physicsEngine.CollisionsFiltered.Clear ()
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

                // create ground collision entry for body0 if needed
                normal <- -normal
                let theta = Vector3.Dot (normal, Vector3.UnitY) |> acos |> abs
                if theta < Constants.Physics.GroundAngleMax then
                    match physicsEngine.CollisionsGround.TryGetValue body0Source with
                    | (true, collisions) -> collisions.Add normal
                    | (false, _) -> physicsEngine.CollisionsGround.Add (body0Source, List [normal])

                // create ground collision entry for body1 if needed
                normal <- -normal
                let theta = -theta
                if theta < Constants.Physics.GroundAngleMax then
                    match physicsEngine.CollisionsGround.TryGetValue body1Source with
                    | (true, collisions) -> collisions.Add normal
                    | (false, _) -> physicsEngine.CollisionsGround.Add (body1Source, List [normal])

        // create collision messages
        for entry in physicsEngine.CollisionsFiltered do
            let (bodySourceA, bodySourceB) = entry.Key
            if not (collisionsOld.ContainsKey entry.Key) then
                PhysicsEngine3d.handleCollision physicsEngine bodySourceA bodySourceB entry.Value
                PhysicsEngine3d.handleCollision physicsEngine bodySourceB bodySourceA -entry.Value

        // create separation messages
        for entry in collisionsOld do
            let (bodySourceA, bodySourceB) = entry.Key
            if not (physicsEngine.CollisionsFiltered.ContainsKey entry.Key) then
                PhysicsEngine3d.handleSeparation physicsEngine bodySourceA bodySourceB
                PhysicsEngine3d.handleSeparation physicsEngine bodySourceB bodySourceA

        // create non-static transform messages
        for bodyEntry in physicsEngine.NonStaticBodies do
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

        // create ghost transform messages
        for ghostEntry in physicsEngine.Ghosts do
            let (offsetOpt, ghost) = ghostEntry.Value
            if ghost.IsActive then
                let bodyTransformMessage =
                    BodyTransformMessage
                        { BodyId = (ghost.UserObject :?> BodyUserObject).BodyId
                          Center = ghost.WorldTransform.Translation - Option.defaultValue v3Zero offsetOpt
                          Rotation = ghost.WorldTransform.Rotation
                          LinearVelocity = v3Zero
                          AngularVelocity = v3Zero }
                physicsEngine.IntegrationMessages.Add bodyTransformMessage

    static member make gravity tryGetAssetFilePath tryGetStaticModelMetadata =
        let taskScheduler = Threads.GetSequentialTaskScheduler () // NOTE: we're just using the non-threaded schedular since none of the others are available (perhaps because I didn't enable them when I previously built bullet).
        taskScheduler.NumThreads <- taskScheduler.MaxNumThreads
        Threads.TaskScheduler <- taskScheduler
        use collisionConfigurationInfo = new DefaultCollisionConstructionInfo (DefaultMaxPersistentManifoldPoolSize = 80000, DefaultMaxCollisionAlgorithmPoolSize = 80000)
        let collisionConfiguration = new DefaultCollisionConfiguration (collisionConfigurationInfo)
        let collisionDispatcher = new CollisionDispatcherMultiThreaded (collisionConfiguration)
        let broadPhaseInterface = new AxisSweep3 (v3Dup (Constants.Physics.AxisSweepBoundsSize3d * -0.5f), v3Dup Constants.Physics.AxisSweepBoundsSize3d) // NOTE: along with Constants.Physics.AllowedCcdPenetration3d, seems to keep characters from falling through terrain.
        let constraintSolverPool = new ConstraintSolverPoolMultiThreaded (Constants.Physics.ThreadCount)
        let constraintSolver = new SequentialImpulseConstraintSolverMultiThreaded ()
        let world = new DiscreteDynamicsWorldMultiThreaded (collisionDispatcher, broadPhaseInterface, constraintSolverPool, constraintSolver, collisionConfiguration)
        world.DispatchInfo.AllowedCcdPenetration <- Constants.Physics.AllowedCcdPenetration3d
        world.Gravity <- gravity
        let physicsEngine =
            { PhysicsContext = world
              Constraints = Dictionary HashIdentity.Structural
              NonStaticBodies = Dictionary HashIdentity.Structural
              Bodies = Dictionary HashIdentity.Structural
              Ghosts = Dictionary HashIdentity.Structural
              Objects = Dictionary HashIdentity.Structural
              Actions = Dictionary HashIdentity.Structural
              CollisionsFiltered = dictPlus HashIdentity.Structural []
              CollisionsGround = dictPlus HashIdentity.Structural []
              CollisionConfiguration = collisionConfiguration
              CollisionDispatcher = collisionDispatcher
              BroadPhaseInterface = broadPhaseInterface
              ConstraintSolverPool = constraintSolverPool
              ConstraintSolver = constraintSolver
              TryGetAssetFilePath = tryGetAssetFilePath
              TryGetStaticModelMetadata = tryGetStaticModelMetadata
              UnscaledPointsCached = dictPlus UnscaledPointsKey.comparer []
              IntegrationMessages = List () }
        physicsEngine

    static member cleanUp physicsEngine =
        physicsEngine.PhysicsContext.Dispose ()
        physicsEngine.ConstraintSolver.Dispose ()
        physicsEngine.ConstraintSolverPool.Dispose ()
        physicsEngine.BroadPhaseInterface.Dispose ()
        physicsEngine.CollisionDispatcher.Dispose ()
        physicsEngine.CollisionConfiguration.Dispose ()

    interface PhysicsEngine with

        member physicsEngine.GetBodyExists bodyId =
            physicsEngine.Objects.ContainsKey bodyId

        member physicsEngine.GetBodyContactNormals bodyId =
            [for collision in physicsEngine.CollisionsFiltered do
                let (body0, body1) = collision.Key
                if body0 = bodyId then -collision.Value
                elif body1 = bodyId then collision.Value]

        member physicsEngine.GetBodyLinearVelocity bodyId =
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, body) -> body.LinearVelocity
            | (false, _) ->
                if physicsEngine.Ghosts.ContainsKey bodyId then v3Zero
                else failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyAngularVelocity bodyId =
            match physicsEngine.Bodies.TryGetValue bodyId with
            | (true, body) -> body.AngularVelocity
            | (false, _) ->
                if physicsEngine.Ghosts.ContainsKey bodyId then v3Zero
                else failwith ("No body with BodyId = " + scstring bodyId + ".")

        member physicsEngine.GetBodyToGroundContactNormals bodyId =
            match physicsEngine.CollisionsGround.TryGetValue bodyId with
            | (true, collisions) -> List.ofSeq collisions
            | (false, _) -> []

        member physicsEngine.GetBodyToGroundContactNormalOpt bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            match groundNormals with
            | [] -> None
            | _ ->
                let averageNormal = List.reduce (fun normal normal2 -> (normal + normal2) * 0.5f) groundNormals
                Some averageNormal

        member physicsEngine.GetBodyToGroundContactTangentOpt bodyId =
            match (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormalOpt bodyId with
            | Some normal -> Some (Vector3.Cross (v3Forward, normal))
            | None -> None

        member physicsEngine.IsBodyOnGround bodyId =
            let groundNormals = (physicsEngine :> PhysicsEngine).GetBodyToGroundContactNormals bodyId
            List.notEmpty groundNormals

        member physicsEngine.HandleMessage physicsMessage =
            PhysicsEngine3d.handlePhysicsMessage physicsEngine physicsMessage

        member physicsEngine.Integrate stepTime =
            PhysicsEngine3d.integrate stepTime physicsEngine
            PhysicsEngine3d.createIntegrationMessages physicsEngine
            let integrationMessages = SArray.ofSeq physicsEngine.IntegrationMessages
            physicsEngine.IntegrationMessages.Clear ()
            integrationMessages

        member physicsEngine.CleanUp () =
            PhysicsEngine3d.cleanUp physicsEngine