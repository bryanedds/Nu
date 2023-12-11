// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WorldHierarchy =

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportHierarchy presenceConferred staticModel rigid (parent : Either<Group, Entity>) world =
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
                            let (child, world) = World.createEntity<EntityDispatcher3d> DefaultOverlay (Some surnames) group world
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
                            let (child, world) = World.createEntity<LightProbeDispatcher3d> DefaultOverlay (Some surnames) group world
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
                            let (child, world) = World.createEntity<LightDispatcher3d> DefaultOverlay (Some surnames) group world
                            let world = child.SetColor light.LightColor world
                            let world = child.SetLightType light.PhysicallyBasedLightType world
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
                                if rigid
                                then World.createEntity<RigidModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                                else World.createEntity<StaticModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                            let world = if rigid then child.SetBodyType Static world else world
                            let (position, rotation, scale, world) =
                                let transform = surface.SurfaceMatrix
                                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                                if Matrix4x4.Decompose (transform, &scale, &rotation, &position)
                                then (position, rotation, scale, world)
                                else (transform.Translation, quatIdentity, transform.Scale, world) // use translation and scale, even from invalid transform
                            let renderStyle = match surface.RenderStyleOpt with Some rs -> rs | None -> Deferred
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetPresence presenceConferred world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let materialProperties =
                                { AlbedoOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Albedo
                                  RoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Roughness
                                  MetallicOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Metallic
                                  AmbientOcclusionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.AmbientOcclusion
                                  EmissionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Emission
                                  HeightOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Height
                                  InvertRoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.InvertRoughness }
                            let world = child.SetMaterialProperties materialProperties world
                            let world = child.SetRenderStyle renderStyle world
                            let world = child.AutoBounds world
                            world' <- world
                            i <- inc i)
                world'
            | None -> world

        ///
        static member freezeHierarchy (parent : Entity) wtemp =
            let mutable (world, boundsOpt) = (wtemp, Option<Box3>.None) // using mutation because I was in a big hurry when I wrote this
            let rec getFrozenArtifacts (entity : Entity) =
                [|if entity <> parent then
                    if entity.Has<LightProbeFacet3d> world then
                        let id = entity.GetId world
                        let enabled = entity.GetEnabled world
                        let position = entity.GetPosition world
                        let bounds = entity.GetProbeBounds world
                        let stale = entity.GetProbeStalePrevious world
                        Choice1Of3 { LightProbeId = id; Enabled = enabled; Origin = position; Bounds = bounds; Stale = stale }
                        let probeBounds = entity.GetProbeBounds world
                        boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine probeBounds) | None -> Some probeBounds
                        world <- entity.SetVisibleLocal false world
                    if entity.Has<LightFacet3d> world then
                        if entity.GetEnabled world then
                            let position = entity.GetPosition world
                            let rotation = entity.GetRotation world
                            let color = entity.GetColor world
                            let brightness = entity.GetBrightness world
                            let attenuationLinear = entity.GetAttenuationLinear world
                            let attenuationQuadratic = entity.GetAttenuationQuadratic world
                            let lightCutoff = entity.GetLightCutoff world
                            let lightType = entity.GetLightType world
                            Choice2Of3 { Origin = position; Direction = Vector3.Transform (v3Up, rotation); Color = color; Brightness = brightness; AttenuationLinear = attenuationLinear; AttenuationQuadratic = attenuationQuadratic; LightCutoff = lightCutoff; LightType = lightType }
                            let lightCutoff = entity.GetLightCutoff world
                            let lightBounds = box3 (entity.GetPosition world - lightCutoff * v3One * 0.5f) (lightCutoff * v3One)
                            boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine lightBounds) | None -> Some lightBounds
                    if entity.Has<StaticModelSurfaceFacet> world then
                        let mutable transform = entity.GetTransform world
                        let absolute = transform.Absolute
                        let affineMatrix = transform.AffineMatrix
                        let insetOpt = match entity.GetInsetOpt world with Some inset -> Some inset | None -> None // OPTIMIZATION: localize boxed value in memory.
                        let properties = entity.GetMaterialProperties world
                        let renderType = match entity.GetRenderStyle world with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                        let staticModel = entity.GetStaticModel world
                        let surfaceIndex = entity.GetSurfaceIndex world
                        Choice3Of3 { Absolute = absolute; ModelMatrix = affineMatrix; InsetOpt = insetOpt; MaterialProperties = properties; RenderType = renderType; SurfaceIndex = surfaceIndex; StaticModel = staticModel }
                        boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine (entity.GetBounds world)) | None -> Some (entity.GetBounds world)
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

        ///
        static member thawHierarchy presenceConferred (parent : Entity) wtemp =
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
module FreezeFacetModule =

    type Entity with
        member this.GetFrozenRenderLightProbe3ds world : RenderLightProbe3d array = this.Get (nameof this.FrozenRenderLightProbe3ds) world
        member this.SetFrozenRenderLightProbe3ds (value : RenderLightProbe3d array) world = this.Set (nameof this.FrozenRenderLightProbe3ds) value world
        member this.FrozenRenderLightProbe3ds = lens (nameof this.FrozenRenderLightProbe3ds) this this.GetFrozenRenderLightProbe3ds this.SetFrozenRenderLightProbe3ds
        member this.GetFrozenRenderLight3ds world : RenderLight3d array = this.Get (nameof this.FrozenRenderLight3ds) world
        member this.SetFrozenRenderLight3ds (value : RenderLight3d array) world = this.Set (nameof this.FrozenRenderLight3ds) value world
        member this.FrozenRenderLight3ds = lens (nameof this.FrozenRenderLight3ds) this this.GetFrozenRenderLight3ds this.SetFrozenRenderLight3ds
        member this.GetFrozenRenderStaticModelSurfaces world : RenderStaticModelSurface array = this.Get (nameof this.FrozenRenderStaticModelSurfaces) world
        member this.SetFrozenRenderStaticModelSurfaces (value : RenderStaticModelSurface array) world = this.Set (nameof this.FrozenRenderStaticModelSurfaces) value world
        member this.FrozenRenderStaticModelSurfaces = lens (nameof this.FrozenRenderStaticModelSurfaces) this this.GetFrozenRenderStaticModelSurfaces this.SetFrozenRenderStaticModelSurfaces
        member this.GetFrozen world : bool = this.Get (nameof this.Frozen) world
        member this.SetFrozen (value : bool) world = this.Set (nameof this.Frozen) value world
        member this.Frozen = lens (nameof this.Frozen) this this.GetFrozen this.SetFrozen
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred
        member this.UpdateFrozenHierarchy world =
            if this.GetFrozen world then
                let (frozenProbes, frozenLights, frozenSurfaces, world) = World.freezeHierarchy this world
                let world = this.SetFrozenRenderLightProbe3ds frozenProbes world
                let world = this.SetFrozenRenderLight3ds frozenLights world
                let world = this.SetFrozenRenderStaticModelSurfaces frozenSurfaces world
                world
            else
                let world = this.SetFrozenRenderLightProbe3ds [||] world
                let world = this.SetFrozenRenderLight3ds [||] world
                let world = this.SetFrozenRenderStaticModelSurfaces [||] world
                let world = World.thawHierarchy (this.GetPresenceConferred world) this world
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
             nonPersistent Entity.FrozenRenderLightProbe3ds [||]
             nonPersistent Entity.FrozenRenderLight3ds [||]
             nonPersistent Entity.FrozenRenderStaticModelSurfaces [||]
             define Entity.Frozen false
             define Entity.PresenceConferred Exposed]

        override this.Register (entity, world) =
            let world = entity.SetOffset v3Zero world
            let world = World.frame (entity.UpdateFrozenHierarchy) entity world // children not loaded yet, so freeze at end of frame
            let world = World.monitor handleUpdateFrozenHierarchy (entity.ChangeEvent (nameof entity.Frozen)) entity world
            world

        override this.Render (entity, world) =

            // render probes, clearing staleness when appropriate
            let bounds = entity.GetBounds world
            let presenceConferred = entity.GetPresenceConferred world
            if World.boundsInView3d true false presenceConferred bounds world then
                let probes = entity.GetFrozenRenderLightProbe3ds world
                for i in 0 .. dec probes.Length do
                    let probe = probes.[i]
                    World.enqueueRenderMessage3d (RenderLightProbe3d probe) world
                    if probe.Stale then probes.[i] <- { probe with Stale = false }

            // render lights
            if World.boundsInView3d false true presenceConferred bounds world then
                for light in entity.GetFrozenRenderLight3ds world do
                    World.enqueueRenderMessage3d (RenderLight3d light) world

            // render surfaces
            if World.boundsInView3d false false presenceConferred bounds world then
                for message in entity.GetFrozenRenderStaticModelSurfaces world do
                    World.renderStaticModelSurfaceFast (message.Absolute, &message.ModelMatrix, Option.toValueOption message.InsetOpt, &message.MaterialProperties, message.RenderType, message.StaticModel, message.SurfaceIndex, world)

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (false)

        static let updateLoadedHierarchy (entity : Entity) world =
            let world =
                Seq.fold (fun world child ->
                    World.destroyEntityImmediate child world)
                    world (entity.GetChildren world)
            let world =
                World.tryImportHierarchy
                    (entity.GetPresenceConferred world) (entity.GetStaticModel world) false (Right entity) world
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
        inherit EntityDispatcher3d (true)

        static let updateLoadedHierarchy (entity : Entity) world =
            let world =
                Seq.fold (fun world child ->
                    World.destroyEntityImmediate child world)
                    world (entity.GetChildren world)
            let world =
                World.tryImportHierarchy
                    (entity.GetPresenceConferred world) (entity.GetStaticModel world) true (Right entity) world
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
            world

        override this.Edit (op, _, world) =
            match op with
            | ReplaceProperty replace ->
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded
                then replace.IndicateReplaced world
                else world
            | _ -> world