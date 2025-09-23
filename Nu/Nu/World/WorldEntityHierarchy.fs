// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open ImGuiNET
open Prime
open Nu

[<AutoOpen>]
module WorldEntityHierarchyExtensions =

    type Entity with

        member internal this.GetSurfaceFreezable world =
            this.GetIs3d world &&
            this.GetStatic world &&
            not (this.Has<LightProbe3dFacet> world) &&
            not (this.Has<Light3dFacet> world) &&
            (this.GetChildren world |> Seq.forall (fun child -> child.GetSurfaceFreezable world))

        member internal this.GetBodyFreezableWhenSurfaceFreezable world =
            this.Has<RigidBodyFacet> world &&
            this.GetBodyType world = Static &&
            this.GetFriction world = Constants.Physics.FrictionDefault &&
            this.GetRestitution world = 0.0f &&
            not (this.GetSensor world)

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportEntityHierarchy presenceConferred staticModel surfaceMaterialsPopulated profile rigid (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | ValueSome staticModelMetadata ->
                let mutable i = 0 // using mutation due to imperative API
                staticModelMetadata.PhysicallyBasedHierarchy.Traverse (fun nodes ->
                    for node in nodes do
                        match node with
                        | OpenGL.PhysicallyBased.PhysicallyBasedNode names ->
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (names.Length > 0, names, group)
                                | Right entity -> (true, Array.append entity.Surnames names, entity.Group)
                            let mountOpt = if mountToParent then Some (Address.makeParent ()) else None
                            let child = World.createEntity<Entity3dDispatcher> mountOpt DefaultOverlay (Some surnames) group world
                            child.SetPresence presenceConferred world
                            child.SetStatic true world
                            child.AutoBounds world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLightProbe lightProbe ->
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (lightProbe.LightProbeNames.Length > 0, lightProbe.LightProbeNames, group)
                                | Right entity -> (true, Array.append entity.Surnames lightProbe.LightProbeNames, entity.Group)
                            let mountOpt = if mountToParent then Some (Address.makeParent ()) else None
                            let child = World.createEntity<LightProbe3dDispatcher> mountOpt DefaultOverlay (Some surnames) group world
                            child.SetProbeBounds lightProbe.LightProbeBounds world
                            child.SetPositionLocal lightProbe.LightProbeMatrix.Translation world
                            child.SetStatic true world
                            child.AutoBounds world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLight light ->
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (light.LightNames.Length > 0, light.LightNames, group)
                                | Right entity -> (true, Array.append entity.Surnames light.LightNames, entity.Group)
                            let mountOpt = if mountToParent then Some (Address.makeParent ()) else None
                            let child = World.createEntity<Light3dDispatcher> mountOpt DefaultOverlay (Some surnames) group world
                            child.SetColor light.LightColor world
                            child.SetLightType light.LightType world
                            let (position, rotation, world) =
                                let transform = light.LightMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, world)
                                else (transform.Translation, quatIdentity, world) // use translation, even from invalid transform
                            child.SetPositionLocal position world
                            child.SetRotationLocal rotation world
                            child.SetPresence presenceConferred world
                            child.SetStatic true world
                            child.AutoBounds world
                        | OpenGL.PhysicallyBased.PhysicallyBasedSurface surface ->
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (surface.SurfaceNames.Length > 0, surface.SurfaceNames, group)
                                | Right entity -> (true, Array.append entity.Surnames surface.SurfaceNames, entity.Group)
                            let mountOpt = if mountToParent then Some (Address.makeParent ()) else None
                            let child =
                                if rigid then
                                    let child = World.createEntity<RigidModelSurfaceDispatcher> mountOpt DefaultOverlay (Some surnames) group world
                                    let surfaceShape =
                                        match child.GetBodyShape world with
                                        | StaticModelSurfaceShape surfaceShape -> surfaceShape
                                        | _ -> failwithumf () // should always be surface shape by default
                                    // TODO: P1: consider implementing this so we can get local concavity from model surface itself.
                                    //let concave = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractConcave concave staticModelMetadata.SceneOpt surface
                                    let surfaceShape = { surfaceShape with Profile = profile }
                                    child.SetBodyShape (StaticModelSurfaceShape surfaceShape) world
                                    let navShape = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractNavShape StaticModelSurfaceNavShape staticModelMetadata.SceneOpt surface
                                    child.SetNavShape navShape world
                                    child
                                else World.createEntity<StaticModelSurfaceDispatcher> mountOpt DefaultOverlay (Some surnames) group world
                            let (position, rotation, scale, world) =
                                let transform = surface.SurfaceMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, scale, world)
                                else (transform.Translation, quatIdentity, transform.Scale, world) // use translation and scale, even from invalid transform
                            let presence = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractPresence presenceConferred staticModelMetadata.SceneOpt surface
                            let renderStyle = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractRenderStyle Deferred staticModelMetadata.SceneOpt surface
                            let ignoreLightMaps = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractIgnoreLightMaps Constants.Render.IgnoreLightMapsDefault staticModelMetadata.SceneOpt surface
                            let opaqueDistance = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractOpaqueDistance Constants.Render.OpaqueDistanceDefault staticModelMetadata.SceneOpt surface
                            let finenessOffset = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractFinenessOffset Constants.Render.FinenessOffsetDefault staticModelMetadata.SceneOpt surface
                            let scatterType = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractScatterType Constants.Render.ScatterTypeDefault staticModelMetadata.SceneOpt surface
                            let specularScalar = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractSpecularScalar Constants.Render.SpecularScalarDefault staticModelMetadata.SceneOpt surface
                            let refractiveIndex = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractRefractiveIndex Constants.Render.RefractiveIndexDefault staticModelMetadata.SceneOpt surface
                            child.SetPositionLocal position world
                            child.SetRotationLocal rotation world
                            child.SetScaleLocal scale world
                            child.SetPresence presence world
                            child.SetStatic true world
                            child.SetStaticModel staticModel world
                            child.SetSurfaceIndex i world
                            let properties =
                                { AlbedoOpt = ValueSome surface.SurfaceMaterialProperties.Albedo
                                  RoughnessOpt = ValueSome surface.SurfaceMaterialProperties.Roughness
                                  MetallicOpt = ValueSome surface.SurfaceMaterialProperties.Metallic
                                  AmbientOcclusionOpt = ValueSome surface.SurfaceMaterialProperties.AmbientOcclusion
                                  EmissionOpt = ValueSome surface.SurfaceMaterialProperties.Emission
                                  HeightOpt = ValueSome surface.SurfaceMaterialProperties.Height
                                  IgnoreLightMapsOpt = ValueSome ignoreLightMaps
                                  OpaqueDistanceOpt = ValueSome opaqueDistance
                                  FinenessOffsetOpt = ValueSome finenessOffset
                                  ScatterTypeOpt = ValueSome scatterType
                                  SpecularScalarOpt = ValueSome specularScalar
                                  RefractiveIndexOpt = ValueSome refractiveIndex }
                            child.SetMaterialProperties properties world
                            let material =
                                if surfaceMaterialsPopulated then
                                    { AlbedoImageOpt = Metadata.tryGetStaticModelAlbedoImage surface.SurfaceMaterialIndex staticModel
                                      RoughnessImageOpt = Metadata.tryGetStaticModelRoughnessImage surface.SurfaceMaterialIndex staticModel
                                      MetallicImageOpt = Metadata.tryGetStaticModelMetallicImage surface.SurfaceMaterialIndex staticModel
                                      AmbientOcclusionImageOpt = Metadata.tryGetStaticModelAmbientOcclusionImage surface.SurfaceMaterialIndex staticModel
                                      EmissionImageOpt = Metadata.tryGetStaticModelEmissionImage surface.SurfaceMaterialIndex staticModel
                                      NormalImageOpt = Metadata.tryGetStaticModelNormalImage surface.SurfaceMaterialIndex staticModel
                                      HeightImageOpt = Metadata.tryGetStaticModelHeightImage surface.SurfaceMaterialIndex staticModel
                                      SubdermalImageOpt = Metadata.tryGetStaticModelSubdermalImage surface.SurfaceMaterialIndex staticModel
                                      FinenessImageOpt = Metadata.tryGetStaticModelFinenessImage surface.SurfaceMaterialIndex staticModel
                                      ScatterImageOpt = Metadata.tryGetStaticModelScatterImage surface.SurfaceMaterialIndex staticModel
                                      TwoSidedOpt = Metadata.tryGetStaticModelTwoSided surface.SurfaceMaterialIndex staticModel
                                      ClippedOpt = Metadata.tryGetStaticModelClipped surface.SurfaceMaterialIndex staticModel }
                                else Material.empty
                            child.SetMaterial material world
                            child.SetRenderStyle renderStyle world
                            child.AutoBounds world
                            i <- inc i)
            | ValueNone -> ()

        /// Attempt to freeze an entity hierarchy where certain types of children's rendering functionality are baked
        /// into a manually renderable array.
        static member freezeEntityHierarchy surfaceMaterialsPopulated (parent : Entity) world =
            let mutable boundsOpt = Option<Box3>.None // using mutation because I was in a big hurry when I wrote this
            let frozenEntities = List ()
            let frozenPreBatches =
                Dictionary<
                    bool * Material * OpenGL.PhysicallyBased.PhysicallyBasedSurface * DepthTest * RenderType,
                    Guid * StaticModel AssetTag * int * (Matrix4x4 * bool * Presence * Box2 * MaterialProperties * Box3) List> ()
            let frozenShapes = List ()
            let rec getFrozenArtifacts (entity : Entity) =
                if entity <> parent then
                    if entity.GetSurfaceFreezable world then // NOTE: shouldn't matter in practice, but there are O(n^2) calls to GetFreezable implicated here.
                        if getType (entity.GetDispatcher world) = typeof<Entity3dDispatcher> then
                            frozenEntities.Add entity
                        elif entity.Has<StaticModelSurfaceFacet> world then
                            let mutable transform = entity.GetTransform world
                            let castShadow = transform.CastShadow
                            let affineMatrix = transform.AffineMatrix
                            let entityBounds = transform.Bounds3d
                            let presence = transform.Presence
                            let insetOpt = match entity.GetInsetOpt world with Some inset -> Some inset | None -> None // OPTIMIZATION: localize boxed value in memory.
                            let properties = entity.GetMaterialProperties world
                            let material = entity.GetMaterial world
                            let staticModel = entity.GetStaticModel world
                            let surfaceIndex = entity.GetSurfaceIndex world
                            let depthTest = entity.GetDepthTest world
                            let renderType = match entity.GetRenderStyle world with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                            boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine entityBounds) | None -> Some entityBounds
                            frozenEntities.Add entity
                            let metadata = Metadata.getStaticModelMetadata staticModel
                            let surface = metadata.Surfaces.[surfaceIndex]
                            let frozenKey = (material.Clipped, material, surface, depthTest, renderType)
                            let frozenValue = (affineMatrix, castShadow, presence, Option.defaultValue box2Zero insetOpt, properties, entityBounds)
                            match frozenPreBatches.TryGetValue frozenKey with
                            | (true, (_, _, _, preBatch)) -> preBatch.Add frozenValue
                            | (false, _) -> frozenPreBatches.Add (frozenKey, (Gen.id, staticModel, surfaceIndex, List [frozenValue]))
                            if entity.GetBodyFreezableWhenSurfaceFreezable world then
                                let affine = Affine.make (entity.GetPosition world) (entity.GetRotation world) (entity.GetScale world)
                                let navShape = entity.GetNavShape world
                                let bodyShape = entity.GetBodyShape world
                                frozenShapes.Add (entityBounds, affineMatrix, staticModel, surfaceIndex, navShape, affine, bodyShape)
                        elif
                            entity.Has<StaticModelFacet> world &&
                            (match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
                             | ValueSome metadata -> metadata.LightProbes.Length = 0 && metadata.Lights.Length = 0
                             | ValueNone -> false) then
                            let mutable transform = entity.GetTransform world
                            let castShadow = transform.CastShadow
                            let affineMatrix = transform.AffineMatrix
                            let insetOpt = match entity.GetInsetOpt world with Some inset -> Some inset | None -> None // OPTIMIZATION: localize boxed value in memory.
                            let properties = entity.GetMaterialProperties world
                            let staticModel = entity.GetStaticModel world
                            let clipped = entity.GetClipped world
                            let depthTest = entity.GetDepthTest world
                            let metadata = Metadata.getStaticModelMetadata (entity.GetStaticModel world)
                            let mutable surfaceIndex = 0
                            while surfaceIndex < metadata.Surfaces.Length do
                                let surface = metadata.Surfaces.[surfaceIndex]
                                let surfaceMatrix = if surface.SurfaceMatrixIsIdentity then affineMatrix else surface.SurfaceMatrix * affineMatrix
                                let surfaceBounds = surface.SurfaceBounds.Transform surfaceMatrix
                                let presence = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractPresence transform.Presence metadata.SceneOpt surface
                                let renderStyle = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractRenderStyle (entity.GetRenderStyle world) metadata.SceneOpt surface
                                let renderType = match renderStyle with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                                let ignoreLightMaps = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractIgnoreLightMaps properties.IgnoreLightMaps metadata.SceneOpt surface
                                let properties = if ignoreLightMaps <> properties.IgnoreLightMaps then { properties with IgnoreLightMapsOpt = ValueSome ignoreLightMaps } else properties
                                let finenessOffset = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractFinenessOffset properties.FinenessOffset metadata.SceneOpt surface
                                let properties = if finenessOffset <> properties.FinenessOffset then { properties with FinenessOffsetOpt = ValueSome finenessOffset } else properties
                                let scatterType = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractScatterType properties.ScatterType metadata.SceneOpt surface
                                let properties = if scatterType <> properties.ScatterType then { properties with ScatterTypeOpt = ValueSome scatterType } else properties
                                let material =
                                    if surfaceMaterialsPopulated then
                                        { AlbedoImageOpt = Metadata.tryGetStaticModelAlbedoImage surface.SurfaceMaterialIndex staticModel
                                          RoughnessImageOpt = Metadata.tryGetStaticModelRoughnessImage surface.SurfaceMaterialIndex staticModel
                                          MetallicImageOpt = Metadata.tryGetStaticModelMetallicImage surface.SurfaceMaterialIndex staticModel
                                          AmbientOcclusionImageOpt = Metadata.tryGetStaticModelAmbientOcclusionImage surface.SurfaceMaterialIndex staticModel
                                          EmissionImageOpt = Metadata.tryGetStaticModelEmissionImage surface.SurfaceMaterialIndex staticModel
                                          NormalImageOpt = Metadata.tryGetStaticModelNormalImage surface.SurfaceMaterialIndex staticModel
                                          HeightImageOpt = Metadata.tryGetStaticModelHeightImage surface.SurfaceMaterialIndex staticModel
                                          SubdermalImageOpt = Metadata.tryGetStaticModelSubdermalImage surface.SurfaceMaterialIndex staticModel
                                          FinenessImageOpt = Metadata.tryGetStaticModelFinenessImage surface.SurfaceMaterialIndex staticModel
                                          ScatterImageOpt = Metadata.tryGetStaticModelScatterImage surface.SurfaceMaterialIndex staticModel
                                          TwoSidedOpt = Metadata.tryGetStaticModelTwoSided surface.SurfaceMaterialIndex staticModel
                                          ClippedOpt = Metadata.tryGetStaticModelClipped surface.SurfaceMaterialIndex staticModel }
                                    else Material.empty
                                boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine surfaceBounds) | None -> Some surfaceBounds
                                let metadata = Metadata.getStaticModelMetadata staticModel
                                let surface = metadata.Surfaces.[surfaceIndex]
                                let frozenKey = (clipped, material, surface, depthTest, renderType)
                                let frozenValue = (affineMatrix, castShadow, presence, Option.defaultValue box2Zero insetOpt, properties, surfaceBounds)
                                match frozenPreBatches.TryGetValue frozenKey with
                                | (true, (_, _, _, preBatch)) -> preBatch.Add frozenValue
                                | (false, _) -> frozenPreBatches.Add (frozenKey, (Gen.id, staticModel, surfaceIndex, List [frozenValue]))
                                if entity.GetBodyFreezableWhenSurfaceFreezable world then
                                    let affine = Affine.make (entity.GetPosition world) (entity.GetRotation world) (entity.GetScale world)
                                    let navShape = entity.GetNavShape world
                                    let bodyShape = entity.GetBodyShape world
                                    frozenShapes.Add (surfaceBounds, affineMatrix, staticModel, surfaceIndex, navShape, affine, bodyShape)
                                surfaceIndex <- inc surfaceIndex
                            frozenEntities.Add entity
                for child in entity.GetChildren world do
                    getFrozenArtifacts child
            getFrozenArtifacts parent
            for entity in frozenEntities do
                entity.SetVisibleLocal false world
                if entity.GetBodyFreezableWhenSurfaceFreezable world then
                    entity.SetNavEnabled false world
                    entity.SetBodyFrozen true world
            match boundsOpt with
            | Some bounds ->
                if bounds.Size.Magnitude >= Constants.Engine.EnvironmentMagnitudeThreshold then
                    parent.SetPickable false world
                    Log.infoOnce "Presuming large frozen parent contains an environment due to total bounds of children and therfore setting it non-pickable."
                parent.SetSize bounds.Size world
                parent.SetOffset (bounds.Center - parent.GetPosition world) world
            | None ->
                parent.SetSize v3One world
                parent.SetOffset v3Zero world
            let frozenPreBatches =
                frozenPreBatches
                |> Seq.map (fun entry ->
                    let (clipped, material, _, depthTest, renderType) = entry.Key
                    let (preBatchId, staticModel, surfaceIndex, preBatch) = entry.Value
                    { PreBatchId = preBatchId
                      StaticModelSurfaces = Seq.toArray preBatch
                      Material = material
                      StaticModel = staticModel
                      SurfaceIndex = surfaceIndex
                      Clipped = clipped
                      DepthTest = depthTest
                      RenderType = renderType })
                |> Seq.toArray
            (frozenPreBatches, Array.ofSeq frozenShapes, world)

        /// Attempt to thaw an entity hierarchy where certain types of children's rendering functionality were baked
        /// into a manually renderable array.
        static member thawEntityHierarchy presenceConferred (parent : Entity) world =
            let rec showChildren (entity : Entity) =
                if entity <> parent then
                    entity.SetVisibleLocal true world
                    if entity.GetBodyFreezableWhenSurfaceFreezable world then
                        entity.SetNavEnabled true world
                        entity.SetBodyFrozen false world
                for child in entity.GetChildren world do
                    showChildren child
            showChildren parent
            parent.SetPresence presenceConferred world // just choosing a default...
            if (parent.GetSize world).Magnitude >= Constants.Engine.EnvironmentMagnitudeThreshold then
                parent.SetPickable true world
                Log.infoOnce "Presuming large thawed parent contains an environment due to total bounds of children and therfore setting it pickable."
            parent.SetSize v3One world
            parent.SetOffset v3Zero world

[<AutoOpen>]
module Permafreezer3dDispatcherExtensions =

    type Entity with
        member this.GetPermafrozenPreBatches world : StaticModelSurfacePreBatch array = this.Get (nameof this.PermafrozenPreBatches) world
        member this.SetPermafrozenPreBatches (value : StaticModelSurfacePreBatch array) world = this.Set (nameof this.PermafrozenPreBatches) value world
        member this.PermafrozenPreBatches = lens (nameof this.PermafrozenPreBatches) this this.GetPermafrozenPreBatches this.SetPermafrozenPreBatches
        member this.GetPermafrozenShapes world : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array = this.Get (nameof this.PermafrozenShapes) world
        member this.SetPermafrozenShapes (value : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array) world = this.Set (nameof this.PermafrozenShapes) value world
        member this.PermafrozenShapes = lens (nameof this.PermafrozenShapes) this this.GetPermafrozenShapes this.SetPermafrozenShapes
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred

        member internal this.RegisterFrozenShapesNav getFrozenShapes world =
            let mutable index = 0
            let frozenShapes = getFrozenShapes this world
            for (bounds, matrix, staticModel, surfaceIndex, navShape, _, _) in frozenShapes do
                let navId = { NavIndex = index; NavEntity = this }
                World.setNav3dBodyOpt (Some (bounds, matrix, staticModel, surfaceIndex, navShape)) navId world
                index <- inc index

        member internal this.RegisterFrozenShapesPhysics getFrozenShapes world =
            let mutable index = 0
            let frozenShapes = getFrozenShapes this world
            for (_, _, _, _, _, affine : Affine, bodyShape) in frozenShapes do
                let bodyId = { BodySource = this; BodyIndex = index }
                let bodyProperties =
                    { Enabled = true
                      Center = affine.Translation
                      Rotation = affine.Rotation
                      Scale = affine.Scale
                      BodyShape = bodyShape
                      BodyType = Static
                      SleepingAllowed = true
                      Friction = Constants.Physics.FrictionDefault
                      Restitution = 0.0f
                      LinearVelocity = v3Zero
                      LinearDamping = 0.0f
                      AngularVelocity = v3Zero
                      AngularDamping = 0.0f
                      AngularFactor = v3Zero
                      Substance = Mass 0.0f
                      GravityOverride = None
                      CharacterProperties = CharacterProperties.defaultProperties
                      VehicleProperties = VehiclePropertiesAbsent
                      CollisionDetection = Discrete
                      CollisionCategories = 1
                      CollisionMask = -1
                      Sensor = false
                      Awake = false
                      BodyIndex = index }
                if this.GetIs2d world
                then World.createBody2d bodyId bodyProperties world
                else World.createBody3d bodyId bodyProperties world
                index <- inc index

        member internal this.RegisterFrozenShapes getFrozenShapes world =
            this.RegisterFrozenShapesNav getFrozenShapes world
            this.RegisterFrozenShapesPhysics getFrozenShapes world

        member internal this.UnregisterFrozenShapesNav getFrozenShapes world =
            let mutable index = 0
            let frozenShapes = getFrozenShapes this world
            for _ in frozenShapes do
                let navId = { NavIndex = index; NavEntity = this }
                World.setNav3dBodyOpt None navId world
                index <- inc index

        member internal this.UnregisterFrozenShapesPhysics getFrozenShapes world =
            let mutable index = 0
            let frozenShapes = getFrozenShapes this world
            for _ in frozenShapes do
                let bodyId = { BodySource = this; BodyIndex = index }
                World.destroyBody3d bodyId world
                index <- inc index

        member internal this.UnregisterFrozenShapes getFrozenShapes world =
            this.UnregisterFrozenShapesNav getFrozenShapes world
            this.UnregisterFrozenShapesPhysics getFrozenShapes world

        member internal this.RenderFrozenPreBatches (bounds, presenceConferred, getFrozenPreBatches, renderPass, entity, world) =

            // compute intersection function based on render pass
            let intersects =
                let interiorOpt = ValueSome (World.getGameEye3dFrustumInterior Game world)
                let exterior = World.getGameEye3dFrustumExterior Game world
                let imposter = World.getGameEye3dFrustumImposter Game world
                let lightBoxOpt = ValueSome (World.getLight3dViewBox world)
                fun probe light presence (bounds : Box3) ->
                    match renderPass with
                    | LightMapPass (_, lightMapBounds) -> not probe && not light && lightMapBounds.Intersects bounds
                    | ShadowPass (_, _, _, _, frustum) -> not probe && not light && frustum.Intersects bounds
                    | ReflectionPass (_, _) -> false
                    | NormalPass -> Presence.intersects3d interiorOpt exterior imposter lightBoxOpt probe light presence bounds

            // render unculled surfaces
            if intersects false false presenceConferred bounds then
                let preBatches = getFrozenPreBatches entity world
                let message = RenderStaticModelSurfacePreBatches { StaticModelSurfacePreBatches = preBatches; RenderPass = renderPass }
                World.enqueueRenderMessage3d message world

/// Gives an entity the base behavior of a permafrozen hierarchy of potentially-rigid model surfaces.
type Permafreezer3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Properties =
        [define Entity.PermafrozenPreBatches [||]
         define Entity.PermafrozenShapes [||]
         define Entity.PresenceConferred Exterior]

    override this.Render (renderPass, entity, world) =
        let bounds = entity.GetBounds world
        let presenceConferred = entity.GetPresenceConferred world
        let getFrozenPreBatches = fun (entity : Entity) -> entity.GetPermafrozenPreBatches
        entity.RenderFrozenPreBatches (bounds, presenceConferred, getFrozenPreBatches, renderPass, entity, world)

    override this.RegisterPhysics (entity, world) =
        let getFrozenShapes = fun (entity : Entity) -> entity.GetPermafrozenShapes
        entity.RegisterFrozenShapesPhysics getFrozenShapes world

    override this.UnregisterPhysics (entity, world) =
        let getFrozenShapes = fun (entity : Entity) -> entity.GetPermafrozenShapes
        entity.UnregisterFrozenShapesPhysics getFrozenShapes world

    override this.RayCast (ray, entity, world) =
        if entity.GetPickable world then
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            [|Intersection.ofNullable intersectionOpt|]
        else [|Miss|]

[<AutoOpen>]
module Freezer3dFacetExtensions =

    type Entity with
        member this.GetFrozenPreBatches world : StaticModelSurfacePreBatch array = this.Get (nameof this.FrozenPreBatches) world
        member this.SetFrozenPreBatches (value : StaticModelSurfacePreBatch array) world = this.Set (nameof this.FrozenPreBatches) value world
        member this.FrozenPreBatches = lens (nameof this.FrozenPreBatches) this this.GetFrozenPreBatches this.SetFrozenPreBatches
        member this.GetFrozenShapes world : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array = this.Get (nameof this.FrozenShapes) world
        member this.SetFrozenShapes (value : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array) world = this.Set (nameof this.FrozenShapes) value world
        member this.FrozenShapes = lens (nameof this.FrozenShapes) this this.GetFrozenShapes this.SetFrozenShapes
        member this.GetFrozen world : bool = this.Get (nameof this.Frozen) world
        member this.SetFrozen (value : bool) world = this.Set (nameof this.Frozen) value world
        member this.Frozen = lens (nameof this.Frozen) this this.GetFrozen this.SetFrozen
        member this.GetSurfaceMaterialsPopulated world : bool = this.Get (nameof this.SurfaceMaterialsPopulated) world
        member this.SetSurfaceMaterialsPopulated (value : bool) world = this.Set (nameof this.SurfaceMaterialsPopulated) value world
        member this.SurfaceMaterialsPopulated = lens (nameof this.SurfaceMaterialsPopulated) this this.GetSurfaceMaterialsPopulated this.SetSurfaceMaterialsPopulated
        member this.GetIgnoreGlobalFreezerCommands world : bool = this.Get (nameof this.IgnoreGlobalFreezerCommands) world
        member this.SetIgnoreGlobalFreezerCommands (value : bool) world = this.Set (nameof this.IgnoreGlobalFreezerCommands) value world
        member this.IgnoreGlobalFreezerCommands = lens (nameof this.IgnoreGlobalFreezerCommands) this this.GetIgnoreGlobalFreezerCommands this.SetIgnoreGlobalFreezerCommands

        member internal this.UpdateFrozenHierarchy world =
            let getFrozenShapes = fun (entity : Entity) -> entity.GetFrozenShapes
            if this.GetFrozen world then
                let surfaceMaterialsPopulated = this.GetSurfaceMaterialsPopulated world
                let (frozenPreBatches, frozenShapes, world) = World.freezeEntityHierarchy surfaceMaterialsPopulated this world
                this.SetFrozenPreBatches frozenPreBatches world
                this.SetStatic true world
                if this.GetSelected world then this.UnregisterFrozenShapes getFrozenShapes world
                this.SetFrozenShapes frozenShapes world
                if this.GetSelected world then this.RegisterFrozenShapes getFrozenShapes world
            else
                if this.GetSelected world then this.UnregisterFrozenShapes getFrozenShapes world
                this.SetFrozenShapes [||] world
                this.SetStatic false world
                this.SetFrozenPreBatches [||] world
                World.thawEntityHierarchy (this.GetPresenceConferred world) this world

        /// Permanently freeze a freezer entity's descendents by freezing and then destroying them.
        member this.Permafreeze world =
            this.SetFrozen true world
            let descendents =
                this.GetDescendants world
                |> Array.ofSeq
                |> Array.sortBy (fun descendant -> descendant.Names.Length)
            for descendent in descendents do
                if descendent.GetExists world && descendent.GetSurfaceFreezable world then
                    World.destroyEntityImmediate descendent world

/// Gives an entity the ability to freeze hierarchies of 3D entities.
type Freezer3dFacet () =
    inherit Facet (false, false, false)

    static let handleUpdateFrozenHierarchy evt world =
        let entity = evt.Subscriber : Entity
        entity.UpdateFrozenHierarchy world
        Cascade

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel
         nonPersistent Entity.FrozenPreBatches [||]
         nonPersistent Entity.FrozenShapes [||]
         define Entity.Frozen false
         define Entity.PresenceConferred Exterior
         define Entity.SurfaceMaterialsPopulated false
         define Entity.IgnoreGlobalFreezerCommands false]

    override this.Register (entity, world) =
        entity.SetOffset v3Zero world
        World.defer (entity.UpdateFrozenHierarchy) entity world // children not loaded yet, so freeze at end of frame
        World.sense handleUpdateFrozenHierarchy (entity.ChangeEvent (nameof entity.Frozen)) entity (nameof Freezer3dFacet) world

    override this.Render (renderPass, entity, world) =
        let bounds = entity.GetBounds world
        let presenceConferred = entity.GetPresenceConferred world
        let getFrozenPreBatches = fun (entity : Entity) -> entity.GetFrozenPreBatches
        entity.RenderFrozenPreBatches (bounds, presenceConferred, getFrozenPreBatches, renderPass, entity, world)

    override this.RegisterPhysics (entity, world) =
        let getFrozenShapes = fun (entity : Entity) -> entity.GetFrozenShapes
        entity.RegisterFrozenShapesPhysics getFrozenShapes world

    override this.UnregisterPhysics (entity, world) =
        let getFrozenShapes = fun (entity : Entity) -> entity.GetFrozenShapes
        entity.UnregisterFrozenShapesPhysics getFrozenShapes world

    override this.RayCast (ray, entity, world) =
        if entity.GetPickable world then
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            [|Intersection.ofNullable intersectionOpt|]
        else [|Miss|]

    override this.Edit (op, entity, world) =
        match op with
        | AppendProperties append ->
            if ImGui.Button "Permafreeze" then
                append.EditContext.Snapshot Permafreeze world
                entity.Permafreeze world
                let frozenPreBatches = entity.GetFrozenPreBatches world
                let frozenShapes = entity.GetFrozenShapes world
                World.changeEntityDispatcher (nameof Permafreezer3dDispatcher) entity world
                entity.SetPermafrozenPreBatches frozenPreBatches world
                entity.SetPermafrozenShapes frozenShapes world
                let getFrozenShapes = fun (entity : Entity) -> entity.GetPermafrozenShapes
                entity.RegisterFrozenShapesPhysics getFrozenShapes world
        | _ -> ()

[<AutoOpen>]
module StaticModelHierarchyDispatcherExtensions =

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

/// Gives an entity the base behavior of hierarchy of indexed static models.
type StaticModelHierarchyDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static let updateLoadedHierarchy (entity : Entity) world =
        for child in entity.GetChildren world do
            World.destroyEntityImmediate child world
        World.tryImportEntityHierarchy
            (entity.GetPresenceConferred world)
            (entity.GetStaticModel world)
            (entity.GetSurfaceMaterialsPopulated world)
            Convex false (Right entity) world
        entity.UpdateFrozenHierarchy world

    static let handleUpdateLoadedHierarchy evt world =
        let entity = evt.Subscriber : Entity
        updateLoadedHierarchy entity world
        Cascade

    static member Facets =
        [typeof<Freezer3dFacet>]

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel
         define Entity.Loaded false]

    override this.Register (entity, world) =
        if not (entity.GetLoaded world) then
            updateLoadedHierarchy entity world
            entity.SetLoaded true world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.StaticModel)) entity world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.SurfaceMaterialsPopulated)) entity world

    override this.Edit (op, _, _) =
        match op with
        | ReplaceProperty replace ->
            if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded then
                replace.IndicateReplaced ()
        | _ -> ()

[<AutoOpen>]
module RigidModelHierarchyDispatcherExtensions =

    type Entity with
        member this.GetProfile world : Profile = this.Get (nameof this.Profile) world
        member this.SetProfile (value : Profile) world = this.Set (nameof this.Profile) value world
        member this.Profile = lens (nameof this.Profile) this this.GetProfile this.SetProfile

/// Gives an entity the base behavior of a hierarchy of indexed, physics-driven rigid models.
type RigidModelHierarchyDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static let updateLoadedHierarchy (entity : Entity) world =
        for child in entity.GetChildren world do
            World.destroyEntityImmediate child world
        World.tryImportEntityHierarchy
            (entity.GetPresenceConferred world)
            (entity.GetStaticModel world)
            (entity.GetSurfaceMaterialsPopulated world)
            (entity.GetProfile world)
            true (Right entity) world
        entity.UpdateFrozenHierarchy world

    static let handleUpdateLoadedHierarchy evt world =
        let entity = evt.Subscriber : Entity
        updateLoadedHierarchy entity world
        Cascade

    static member Facets =
        [typeof<Freezer3dFacet>]

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel
         define Entity.Profile Convex
         define Entity.Loaded false]

    override this.Register (entity, world) =
        if not (entity.GetLoaded world) then
            updateLoadedHierarchy entity world
            entity.SetLoaded true world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.StaticModel)) entity world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.SurfaceMaterialsPopulated)) entity world
        World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.Profile)) entity world

    override this.Edit (op, _, _) =
        match op with
        | ReplaceProperty replace ->
            if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded then
                replace.IndicateReplaced ()
        | _ -> ()