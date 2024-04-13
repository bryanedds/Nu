// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldEntityHierarchy =

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportEntityHierarchy presenceConferred staticModel surfaceMaterialsPopulated rigid (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
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
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let properties =
                                { AlbedoOpt = Some surface.SurfaceMaterialProperties.Albedo
                                  RoughnessOpt = Some surface.SurfaceMaterialProperties.Roughness
                                  MetallicOpt = Some surface.SurfaceMaterialProperties.Metallic
                                  AmbientOcclusionOpt = Some surface.SurfaceMaterialProperties.AmbientOcclusion
                                  EmissionOpt = Some surface.SurfaceMaterialProperties.Emission
                                  HeightOpt = Some surface.SurfaceMaterialProperties.Height
                                  IgnoreLightMapsOpt = Some ignoreLightMaps
                                  OpaqueDistanceOpt = Some opaqueDistance }
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
            | None -> world

        /// Attempt to freeze an entity hierarchy where certain types of children's rendering functionality are baked
        /// into a manually renderable array.
        static member freezeEntityHierarchy (parent : Entity) wtemp =
            let mutable (world, boundsOpt) = (wtemp, Option<Box3>.None) // using mutation because I was in a big hurry when I wrote this
            let rec getFrozenArtifacts (entity : Entity) =
                [|if entity <> parent then
                    if entity.Has<LightProbe3dFacet> world then
                        let id = entity.GetId world
                        let enabled = entity.GetEnabled world
                        let position = entity.GetPosition world
                        let bounds = entity.GetProbeBounds world
                        let stale = entity.GetProbeStale world
                        Choice1Of3 { LightProbeId = id; Enabled = enabled; Origin = position; Bounds = bounds; Stale = stale }
                        let probeBounds = entity.GetProbeBounds world
                        boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine probeBounds) | None -> Some probeBounds
                        world <- entity.SetVisibleLocal false world
                    if entity.Has<Light3dFacet> world then
                        if entity.GetEnabled world then
                            let lightId = entity.GetId world
                            let position = entity.GetPosition world
                            let rotation = entity.GetRotation world
                            let direction = rotation.Down
                            let color = entity.GetColor world
                            let brightness = entity.GetBrightness world
                            let attenuationLinear = entity.GetAttenuationLinear world
                            let attenuationQuadratic = entity.GetAttenuationQuadratic world
                            let lightCutoff = entity.GetLightCutoff world
                            let lightType = entity.GetLightType world
                            let desireShadows = entity.GetDesireShadows world
                            let presence = entity.GetPresence world
                            Choice2Of3 { LightId = lightId; Origin = position; Rotation = rotation; Direction = direction; Presence = presence; Color = color; Brightness = brightness; AttenuationLinear = attenuationLinear; AttenuationQuadratic = attenuationQuadratic; LightCutoff = lightCutoff; LightType = lightType; DesireShadows = desireShadows }
                            let lightCutoff = entity.GetLightCutoff world
                            let lightBounds = box3 (entity.GetPosition world - lightCutoff * v3One * 0.5f) (lightCutoff * v3One)
                            boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine lightBounds) | None -> Some lightBounds
                    if entity.Has<StaticModelSurfaceFacet> world then
                        let mutable transform = entity.GetTransform world
                        let absolute = transform.Absolute
                        let affineMatrix = transform.AffineMatrix
                        let entityBounds = transform.Bounds3d
                        let presence = transform.Presence
                        let insetOpt = match entity.GetInsetOpt world with Some inset -> Some inset | None -> None // OPTIMIZATION: localize boxed value in memory.
                        let properties = entity.GetMaterialProperties world
                        let material = entity.GetMaterial world
                        let staticModel = entity.GetStaticModel world
                        let surfaceIndex = entity.GetSurfaceIndex world
                        let renderType = match entity.GetRenderStyle world with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                        let surface = { Absolute = absolute; ModelMatrix = affineMatrix; Presence = presence; InsetOpt = insetOpt; MaterialProperties = properties; Material = material; SurfaceIndex = surfaceIndex; StaticModel = staticModel; RenderType = renderType }
                        Choice3Of3 (StructPair.make entityBounds surface)
                        boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine entityBounds) | None -> Some entityBounds
                        world <- entity.SetVisibleLocal false world
                    if entity <> parent then
                        world <- entity.SetVisibleLocal false world
                  for child in entity.GetChildren world do
                    yield! getFrozenArtifacts child|]
            let (frozenProbes, frozenLights, frozenSurfaces) = (List (), List (), List ())
            for artifact in getFrozenArtifacts parent do
                match artifact with
                | Choice1Of3 probe -> frozenProbes.Add probe
                | Choice2Of3 light -> frozenLights.Add light
                | Choice3Of3 surface -> frozenSurfaces.Add surface
            world <- parent.SetPresence Omnipresent world
            world <- parent.SetPickable false world
            match boundsOpt with
            | Some bounds ->
                world <- parent.SetSize bounds.Size world
                world <- parent.SetOffset (bounds.Center - parent.GetPosition world) world
            | None ->
                world <- parent.SetSize v3One world
                world <- parent.SetOffset v3Zero world
            (Array.ofSeq frozenProbes, Array.ofSeq frozenLights, Array.ofSeq frozenSurfaces, world)

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
        member this.GetFrozenRenderLightProbes3d world : LightProbe3dValue array = this.Get (nameof this.FrozenRenderLightProbes3d) world
        member this.SetFrozenRenderLightProbes3d (value : LightProbe3dValue array) world = this.Set (nameof this.FrozenRenderLightProbes3d) value world
        member this.FrozenRenderLightProbes3d = lens (nameof this.FrozenRenderLightProbes3d) this this.GetFrozenRenderLightProbes3d this.SetFrozenRenderLightProbes3d
        member this.GetFrozenRenderLights3d world : Light3dValue array = this.Get (nameof this.FrozenRenderLights3d) world
        member this.SetFrozenRenderLights3d (value : Light3dValue array) world = this.Set (nameof this.FrozenRenderLights3d) value world
        member this.FrozenRenderLights3d = lens (nameof this.FrozenRenderLights3d) this this.GetFrozenRenderLights3d this.SetFrozenRenderLights3d
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
                let (frozenProbes, frozenLights, frozenSurfaces, world) = World.freezeEntityHierarchy this world
                let world = this.SetFrozenRenderLightProbes3d frozenProbes world
                let world = this.SetFrozenRenderLights3d frozenLights world
                let world = this.SetFrozenRenderStaticModelSurfaces frozenSurfaces world
                let world = this.SetStatic true world
                world
            else
                let world = this.SetStatic false world
                let world = this.SetFrozenRenderLightProbes3d [||] world
                let world = this.SetFrozenRenderLights3d [||] world
                let world = this.SetFrozenRenderStaticModelSurfaces [||] world
                let world = World.thawEntityHierarchy (this.GetPresenceConferred world) this world
                world

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type FreezerFacet () =
        inherit Facet (false)

        static let handleUpdateFrozenHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = entity.UpdateFrozenHierarchy world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             nonPersistent Entity.FrozenRenderLightProbes3d [||]
             nonPersistent Entity.FrozenRenderLights3d [||]
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
                let interiorOpt = Some (World.getGameEye3dFrustumInterior Game world)
                let exterior = World.getGameEye3dFrustumExterior Game world
                let imposter = World.getGameEye3dFrustumImposter Game world
                let lightBoxOpt = Some (World.getLight3dBox world)
                fun probe light presence bounds ->
                    match renderPass with
                    | NormalPass -> Presence.intersects3d interiorOpt exterior imposter lightBoxOpt probe light presence bounds
                    | LightMapPass (_, lightMapBounds) -> not probe && not light && lightMapBounds.Intersects bounds
                    | ShadowPass (_, _, _, frustum) -> not probe && not light && frustum.Intersects bounds
                    | ReflectionPass (_, _) -> false

            // render all probes, clearing staleness when appropriate
            let probes = entity.GetFrozenRenderLightProbes3d world
            let world =
                List.fold (fun world i ->
                    let probe = &probes.[i]
                    let renderProbe = { LightProbeId = probe.LightProbeId; Enabled = probe.Enabled; Origin = probe.Origin; Bounds = probe.Bounds; RenderPass = renderPass }
                    World.enqueueRenderMessage3d (RenderLightProbe3d renderProbe) world
                    let world = if probe.Stale then World.requestLightMapRender world else world
                    probe.Stale <- false
                    world)
                    world [0 .. dec probes.Length]

            // render unculled lights
            let bounds = entity.GetBounds world
            let presenceConferred = entity.GetPresenceConferred world
            if intersects false true presenceConferred bounds then
                for light in entity.GetFrozenRenderLights3d world do
                    if intersects false true light.Presence bounds then
                        let renderLight = { LightId = light.LightId; Origin = light.Origin; Rotation = light.Rotation; Direction = light.Direction; Color = light.Color; Brightness = light.Brightness; AttenuationLinear = light.AttenuationLinear; AttenuationQuadratic = light.AttenuationQuadratic; LightCutoff = light.LightCutoff; LightType = light.LightType; DesireShadows = light.DesireShadows; RenderPass = renderPass }
                        World.enqueueRenderMessage3d (RenderLight3d renderLight) world

            // render unculled surfaces
            if intersects false false presenceConferred bounds then
                let staticModelSurfaces = entity.GetFrozenRenderStaticModelSurfaces world
                for i in 0 .. dec staticModelSurfaces.Length do
                    let boundsAndSurface = &staticModelSurfaces.[i]
                    let bounds = &boundsAndSurface.Fst
                    let surface = &boundsAndSurface.Snd
                    if intersects false false surface.Presence bounds then
                        World.renderStaticModelSurfaceFast (surface.Absolute, &surface.ModelMatrix, surface.Presence, Option.toValueOption surface.InsetOpt, &surface.MaterialProperties, &surface.Material, surface.StaticModel, surface.SurfaceIndex, surface.RenderType, renderPass, world)

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type StaticModelHierarchyDispatcher () =
        inherit Entity3dDispatcher (false)

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
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded
                then replace.IndicateReplaced world
                else world
            | _ -> world

[<AutoOpen>]
module RigidModelHierarchyDispatcherModule =

    /// Gives an entity the base behavior of a hierarchy of indexed, physics-driven rigid models.
    type RigidModelHierarchyDispatcher () =
        inherit Entity3dDispatcher (true)

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
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded
                then replace.IndicateReplaced world
                else world
            | _ -> world