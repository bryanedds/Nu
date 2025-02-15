// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldEntityHierarchy =

    type Entity with

        /// Check whether an entity can be frozen by an ancestor with a FreezerFacet.
        member entity.GetFreezable world =
            entity.GetStatic world &&
            not (entity.Has<LightProbe3dFacet> world) &&
            not (entity.Has<Light3dFacet> world) &&
            (entity.GetChildren world |> Seq.forall (fun child -> child.GetFreezable world))

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportEntityHierarchy presenceConferred staticModel surfaceMaterialsPopulated rigid (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | ValueSome staticModelMetadata ->
                let mutable (world', i) = (world, 0) // using mutation due to imperative API
                staticModelMetadata.PhysicallyBasedHierarchy.Traverse (fun nodes ->
                    for node in nodes do
                        match node with
                        | OpenGL.PhysicallyBased.PhysicallyBasedNode names ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (names.Length > 0, names, group)
                                | Right entity -> (true, Array.append entity.Surnames names, entity.Group)
                            let (child, world) = World.createEntity<Entity3dDispatcher> DefaultOverlay (Some surnames) group world
                            let world = child.SetPresence presenceConferred world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.AutoBounds world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLightProbe lightProbe ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (lightProbe.LightProbeNames.Length > 0, lightProbe.LightProbeNames, group)
                                | Right entity -> (true, Array.append entity.Surnames lightProbe.LightProbeNames, entity.Group)
                            let (child, world) = World.createEntity<LightProbe3dDispatcher> DefaultOverlay (Some surnames) group world
                            let world = child.SetProbeBounds lightProbe.LightProbeBounds world
                            let world = child.SetPositionLocal lightProbe.LightProbeMatrix.Translation world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.AutoBounds world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedLight light ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (light.LightNames.Length > 0, light.LightNames, group)
                                | Right entity -> (true, Array.append entity.Surnames light.LightNames, entity.Group)
                            let (child, world) = World.createEntity<Light3dDispatcher> DefaultOverlay (Some surnames) group world
                            let world = child.SetColor light.LightColor world
                            let world = child.SetLightType light.LightType world
                            let (position, rotation, world) =
                                let transform = light.LightMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, world)
                                else (transform.Translation, quatIdentity, world) // use translation, even from invalid transform
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetPresence presenceConferred world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.AutoBounds world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedSurface surface ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (surface.SurfaceNames.Length > 0, surface.SurfaceNames, group)
                                | Right entity -> (true, Array.append entity.Surnames surface.SurfaceNames, entity.Group)
                            let (child, world) =
                                if rigid then
                                    let (child, world) = World.createEntity<RigidModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                                    let shape = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractNavShape BoundsNavShape staticModelMetadata.SceneOpt surface
                                    let world = child.SetNavShape shape world
                                    (child, world)
                                else World.createEntity<StaticModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
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
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetStaticModel staticModel world
                            let world = child.SetSurfaceIndex i world
                            let properties =
                                { AlbedoOpt = ValueSome surface.SurfaceMaterialProperties.Albedo
                                  RoughnessOpt = ValueSome surface.SurfaceMaterialProperties.Roughness
                                  MetallicOpt = ValueSome surface.SurfaceMaterialProperties.Metallic
                                  AmbientOcclusionOpt = ValueSome surface.SurfaceMaterialProperties.AmbientOcclusion
                                  EmissionOpt = ValueSome surface.SurfaceMaterialProperties.Emission
                                  HeightOpt = ValueSome surface.SurfaceMaterialProperties.Height
                                  IgnoreLightMapsOpt = ValueSome ignoreLightMaps
                                  OpaqueDistanceOpt = ValueSome opaqueDistance }
                            let world = child.SetMaterialProperties properties world
                            let material =
                                if surfaceMaterialsPopulated then
                                    { AlbedoImageOpt = Metadata.tryGetStaticModelAlbedoImage surface.SurfaceMaterialIndex staticModel
                                      RoughnessImageOpt = Metadata.tryGetStaticModelRoughnessImage surface.SurfaceMaterialIndex staticModel
                                      MetallicImageOpt = Metadata.tryGetStaticModelMetallicImage surface.SurfaceMaterialIndex staticModel
                                      AmbientOcclusionImageOpt = Metadata.tryGetStaticModelAmbientOcclusionImage surface.SurfaceMaterialIndex staticModel
                                      EmissionImageOpt = Metadata.tryGetStaticModelEmissionImage surface.SurfaceMaterialIndex staticModel
                                      NormalImageOpt = Metadata.tryGetStaticModelNormalImage surface.SurfaceMaterialIndex staticModel
                                      HeightImageOpt = Metadata.tryGetStaticModelHeightImage surface.SurfaceMaterialIndex staticModel
                                      TwoSidedOpt = Metadata.tryGetStaticModelTwoSided surface.SurfaceMaterialIndex staticModel }
                                else Material.empty
                            let world = child.SetMaterial material world
                            let world = child.SetRenderStyle renderStyle world
                            let world = child.AutoBounds world
                            world' <- world
                            i <- inc i)
                world'
            | ValueNone -> world

        /// Attempt to freeze an entity hierarchy where certain types of children's rendering functionality are baked
        /// into a manually renderable array.
        static member freezeEntityHierarchy surfaceMaterialsPopulated (parent : Entity) wtemp =
            let mutable (world, boundsOpt) = (wtemp, Option<Box3>.None) // using mutation because I was in a big hurry when I wrote this
            let frozenSurfaces = List ()
            let rec getFrozenArtifacts (entity : Entity) =
                if entity <> parent then
                    if entity.GetFreezable world then // NOTE: shouldn't matter in practice, but there are O(n^2) calls to GetFreezable implicated here.
                        if entity.Has<StaticModelSurfaceFacet> world then
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
                            let renderType = match entity.GetRenderStyle world with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                            let surface = { CastShadow = castShadow; ModelMatrix = affineMatrix; Presence = presence; InsetOpt = insetOpt; MaterialProperties = properties; Material = material; SurfaceIndex = surfaceIndex; StaticModel = staticModel; RenderType = renderType }
                            let frozenSurface = StructPair.make entityBounds surface
                            boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine entityBounds) | None -> Some entityBounds
                            world <- entity.SetVisibleLocal false world
                            frozenSurfaces.Add frozenSurface
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
                                let material =
                                    if surfaceMaterialsPopulated then
                                        { AlbedoImageOpt = Metadata.tryGetStaticModelAlbedoImage surface.SurfaceMaterialIndex staticModel
                                          RoughnessImageOpt = Metadata.tryGetStaticModelRoughnessImage surface.SurfaceMaterialIndex staticModel
                                          MetallicImageOpt = Metadata.tryGetStaticModelMetallicImage surface.SurfaceMaterialIndex staticModel
                                          AmbientOcclusionImageOpt = Metadata.tryGetStaticModelAmbientOcclusionImage surface.SurfaceMaterialIndex staticModel
                                          EmissionImageOpt = Metadata.tryGetStaticModelEmissionImage surface.SurfaceMaterialIndex staticModel
                                          NormalImageOpt = Metadata.tryGetStaticModelNormalImage surface.SurfaceMaterialIndex staticModel
                                          HeightImageOpt = Metadata.tryGetStaticModelHeightImage surface.SurfaceMaterialIndex staticModel
                                          TwoSidedOpt = Metadata.tryGetStaticModelTwoSided surface.SurfaceMaterialIndex staticModel }
                                    else Material.empty
                                let surface = { CastShadow = castShadow; ModelMatrix = surfaceMatrix; Presence = presence; InsetOpt = insetOpt; MaterialProperties = properties; Material = material; SurfaceIndex = surfaceIndex; StaticModel = staticModel; RenderType = renderType }
                                let frozenSurface = StructPair.make surfaceBounds surface                                
                                boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine surfaceBounds) | None -> Some surfaceBounds
                                world <- entity.SetVisibleLocal false world
                                frozenSurfaces.Add frozenSurface
                                surfaceIndex <- inc surfaceIndex
                            world <- entity.SetVisibleLocal false world
                for child in entity.GetChildren world do
                    getFrozenArtifacts child
            getFrozenArtifacts parent
            world <- parent.SetPresence Omnipresent world
            world <- parent.SetPickable false world
            match boundsOpt with
            | Some bounds ->
                world <- parent.SetSize bounds.Size world
                world <- parent.SetOffset (bounds.Center - parent.GetPosition world) world
            | None ->
                world <- parent.SetSize v3One world
                world <- parent.SetOffset v3Zero world
            (Array.ofSeq frozenSurfaces, world)

        /// Attempt to thaw an entity hierarchy where certain types of children's rendering functionality were baked
        /// into a manually renderable array.
        static member thawEntityHierarchy presenceConferred (parent : Entity) wtemp =
            let mutable world = wtemp
            let rec showChildren (entity : Entity) =
                if entity <> parent then
                    world <- entity.SetVisibleLocal true world
                for child in entity.GetChildren world do
                    showChildren child
            showChildren parent
            world <- parent.SetPresence presenceConferred world // just choosing a default...
            world <- parent.SetPickable true world
            world <- parent.SetSize v3One world
            world <- parent.SetOffset v3Zero world
            world

[<AutoOpen>]
module FreezerFacetModule =

    type Entity with
        member this.GetFrozenRenderStaticModelSurfaces world : StructPair<Box3, StaticModelSurfaceValue> array = this.Get (nameof this.FrozenRenderStaticModelSurfaces) world
        member this.SetFrozenRenderStaticModelSurfaces (value : StructPair<Box3, StaticModelSurfaceValue> array) world = this.Set (nameof this.FrozenRenderStaticModelSurfaces) value world
        member this.FrozenRenderStaticModelSurfaces = lens (nameof this.FrozenRenderStaticModelSurfaces) this this.GetFrozenRenderStaticModelSurfaces this.SetFrozenRenderStaticModelSurfaces
        member this.GetFrozen world : bool = this.Get (nameof this.Frozen) world
        member this.SetFrozen (value : bool) world = this.Set (nameof this.Frozen) value world
        member this.Frozen = lens (nameof this.Frozen) this this.GetFrozen this.SetFrozen
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred
        member this.GetSurfaceMaterialsPopulated world : bool = this.Get (nameof this.SurfaceMaterialsPopulated) world
        member this.SetSurfaceMaterialsPopulated (value : bool) world = this.Set (nameof this.SurfaceMaterialsPopulated) value world
        member this.SurfaceMaterialsPopulated = lens (nameof this.SurfaceMaterialsPopulated) this this.GetSurfaceMaterialsPopulated this.SetSurfaceMaterialsPopulated
        member this.UpdateFrozenHierarchy world =
            if this.GetFrozen world then
                let surfaceMaterialsPopulated = this.GetSurfaceMaterialsPopulated world
                let (frozenSurfaces, world) = World.freezeEntityHierarchy surfaceMaterialsPopulated this world
                let world = this.SetFrozenRenderStaticModelSurfaces frozenSurfaces world
                let world = this.SetStatic true world
                world
            else
                let world = this.SetStatic false world
                let world = this.SetFrozenRenderStaticModelSurfaces [||] world
                let world = World.thawEntityHierarchy (this.GetPresenceConferred world) this world
                world

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type FreezerFacet () =
        inherit Facet (false, false, false)

        static let handleUpdateFrozenHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = entity.UpdateFrozenHierarchy world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             nonPersistent Entity.FrozenRenderStaticModelSurfaces [||]
             define Entity.Frozen false
             define Entity.PresenceConferred Exterior
             define Entity.SurfaceMaterialsPopulated false]

        override this.Register (entity, world) =
            let world = entity.SetOffset v3Zero world
            let world = World.frame (entity.UpdateFrozenHierarchy) entity world // children not loaded yet, so freeze at end of frame
            let world = World.monitor handleUpdateFrozenHierarchy (entity.ChangeEvent (nameof entity.Frozen)) entity world
            world

        override this.Render (renderPass, entity, world) =

            // compute intersection function based on render pass
            let intersects =
                let interiorOpt = ValueSome (World.getGameEye3dFrustumInterior Game world)
                let exterior = World.getGameEye3dFrustumExterior Game world
                let imposter = World.getGameEye3dFrustumImposter Game world
                let lightBoxOpt = ValueSome (World.getLight3dBox world)
                fun probe light presence bounds ->
                    match renderPass with
                    | NormalPass -> Presence.intersects3d interiorOpt exterior imposter lightBoxOpt probe light presence bounds
                    | LightMapPass (_, lightMapBounds) -> not probe && not light && lightMapBounds.Intersects bounds
                    | ShadowPass (_, _, _, frustum) -> not probe && not light && frustum.Intersects bounds
                    | ReflectionPass (_, _) -> false

            // render unculled surfaces
            let bounds = entity.GetBounds world
            let presenceConferred = entity.GetPresenceConferred world
            if intersects false false presenceConferred bounds then
                let staticModelSurfaces = entity.GetFrozenRenderStaticModelSurfaces world
                for i in 0 .. dec staticModelSurfaces.Length do
                    let boundsAndSurface = &staticModelSurfaces.[i]
                    let bounds = &boundsAndSurface.Fst
                    let surface = &boundsAndSurface.Snd
                    if (not renderPass.IsShadowPass || surface.CastShadow) && intersects false false surface.Presence bounds then
                        World.renderStaticModelSurfaceFast (&surface.ModelMatrix, surface.CastShadow, surface.Presence, Option.toValueOption surface.InsetOpt, &surface.MaterialProperties, &surface.Material, surface.StaticModel, surface.SurfaceIndex, surface.RenderType, renderPass, world)

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type StaticModelHierarchyDispatcher () =
        inherit Entity3dDispatcher (false, false, false)

        static let updateLoadedHierarchy (entity : Entity) world =
            let world =
                Seq.fold (fun world child ->
                    World.destroyEntityImmediate child world)
                    world (entity.GetChildren world)
            let world =
                World.tryImportEntityHierarchy
                    (entity.GetPresenceConferred world)
                    (entity.GetStaticModel world)
                    (entity.GetSurfaceMaterialsPopulated world)
                    false (Right entity) world
            entity.UpdateFrozenHierarchy world

        static let handleUpdateLoadedHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = updateLoadedHierarchy entity world
            (Cascade, world)

        static member Facets =
            [typeof<FreezerFacet>]

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = updateLoadedHierarchy entity world
                    entity.SetLoaded true world
                else world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.SurfaceMaterialsPopulated)) entity world
            world

        override this.Edit (op, _, world) =
            match op with
            | ReplaceProperty replace ->
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded then replace.IndicateReplaced ()
                world
            | _ -> world

[<AutoOpen>]
module RigidModelHierarchyDispatcherModule =

    /// Gives an entity the base behavior of a hierarchy of indexed, physics-driven rigid models.
    type RigidModelHierarchyDispatcher () =
        inherit Entity3dDispatcher (true, false, false)

        static let updateLoadedHierarchy (entity : Entity) world =
            let world =
                Seq.fold (fun world child ->
                    World.destroyEntityImmediate child world)
                    world (entity.GetChildren world)
            let world =
                World.tryImportEntityHierarchy
                    (entity.GetPresenceConferred world)
                    (entity.GetStaticModel world)
                    (entity.GetSurfaceMaterialsPopulated world)
                    true (Right entity) world
            entity.UpdateFrozenHierarchy world

        static let handleUpdateLoadedHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = updateLoadedHierarchy entity world
            (Cascade, world)

        static member Facets =
            [typeof<FreezerFacet>]

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = updateLoadedHierarchy entity world
                    entity.SetLoaded true world
                else world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.SurfaceMaterialsPopulated)) entity world
            world

        override this.Edit (op, _, world) =
            match op with
            | ReplaceProperty replace ->
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded then replace.IndicateReplaced ()
                world
            | _ -> world