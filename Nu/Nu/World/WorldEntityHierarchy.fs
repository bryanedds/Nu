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
module WorldEntityHierarchy =

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
                                    let surfaceShape =
                                        match child.GetBodyShape world with
                                        | StaticModelSurfaceShape surfaceShape -> surfaceShape
                                        | _ -> failwithumf () // should always be surface shape by default
                                    // TODO: P1: consider implementing this so we can get local concavity from model surface itself.
                                    //let concave = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractConcave concave staticModelMetadata.SceneOpt surface
                                    let surfaceShape = { surfaceShape with Profile = profile }
                                    let world = child.SetBodyShape (StaticModelSurfaceShape surfaceShape) world
                                    let navShape = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractNavShape StaticModelSurfaceNavShape staticModelMetadata.SceneOpt surface
                                    let world = child.SetNavShape navShape world
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
                            let finenessOffset = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractFinenessOffset Constants.Render.FinenessOffsetDefault staticModelMetadata.SceneOpt surface
                            let scatterType = OpenGL.PhysicallyBased.PhysicallyBasedSurfaceFns.extractScatterType Constants.Render.ScatterTypeDefault staticModelMetadata.SceneOpt surface
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
                                  OpaqueDistanceOpt = ValueSome opaqueDistance
                                  FinenessOffsetOpt = ValueSome finenessOffset
                                  ScatterTypeOpt = ValueSome scatterType }
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
                                      SubdermalImageOpt = Metadata.tryGetStaticModelSubdermalImage surface.SurfaceMaterialIndex staticModel
                                      FinenessImageOpt = Metadata.tryGetStaticModelFinenessImage surface.SurfaceMaterialIndex staticModel
                                      ScatterImageOpt = Metadata.tryGetStaticModelScatterImage surface.SurfaceMaterialIndex staticModel
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
            let frozenEntities = List ()
            let frozenSurfaces = List ()
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
                            let surface = { CastShadow = castShadow; ModelMatrix = affineMatrix; Presence = presence; InsetOpt = insetOpt; MaterialProperties = properties; Material = material; SurfaceIndex = surfaceIndex; StaticModel = staticModel; DepthTest = depthTest; RenderType = renderType }
                            let frozenSurface = StructPair.make entityBounds surface
                            boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine entityBounds) | None -> Some entityBounds
                            frozenEntities.Add entity
                            frozenSurfaces.Add frozenSurface
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
                                          TwoSidedOpt = Metadata.tryGetStaticModelTwoSided surface.SurfaceMaterialIndex staticModel }
                                    else Material.empty
                                let surface = { CastShadow = castShadow; ModelMatrix = surfaceMatrix; Presence = presence; InsetOpt = insetOpt; MaterialProperties = properties; Material = material; SurfaceIndex = surfaceIndex; StaticModel = staticModel; DepthTest = depthTest; RenderType = renderType }
                                let frozenSurface = StructPair.make surfaceBounds surface                                
                                boundsOpt <- match boundsOpt with Some bounds -> Some (bounds.Combine surfaceBounds) | None -> Some surfaceBounds
                                frozenSurfaces.Add frozenSurface
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
                world <- entity.SetVisibleLocal false world
                if entity.GetBodyFreezableWhenSurfaceFreezable world then
                    world <- entity.SetNavEnabled false world
                    world <- entity.SetBodyEnabled false world
            match boundsOpt with
            | Some bounds ->
                if bounds.Size.Magnitude >= Constants.Engine.EnvironmentMagnitudeThreshold then
                    world <- parent.SetPickable false world
                    Log.infoOnce "Presuming large frozen parent contains an environment due to total bounds of children and therfore setting it non-pickable."
                world <- parent.SetSize bounds.Size world
                world <- parent.SetOffset (bounds.Center - parent.GetPosition world) world
            | None ->
                world <- parent.SetSize v3One world
                world <- parent.SetOffset v3Zero world
            (Array.ofSeq frozenSurfaces, Array.ofSeq frozenShapes, world)

        /// Attempt to thaw an entity hierarchy where certain types of children's rendering functionality were baked
        /// into a manually renderable array.
        static member thawEntityHierarchy presenceConferred (parent : Entity) wtemp =
            let mutable world = wtemp
            let rec showChildren (entity : Entity) =
                if entity <> parent then
                    world <- entity.SetVisibleLocal true world
                    if entity.GetBodyFreezableWhenSurfaceFreezable world then
                        world <- entity.SetNavEnabled true world
                        world <- entity.SetBodyEnabled true world
                for child in entity.GetChildren world do
                    showChildren child
            showChildren parent
            world <- parent.SetPresence presenceConferred world // just choosing a default...
            if (parent.GetSize world).Magnitude >= Constants.Engine.EnvironmentMagnitudeThreshold then
                world <- parent.SetPickable true world
                Log.infoOnce "Presuming large thawed parent contains an environment due to total bounds of children and therfore setting it pickable."
            world <- parent.SetSize v3One world
            world <- parent.SetOffset v3Zero world
            world

[<AutoOpen>]
module Freezer3dFacetModule =

    type Entity with
        member this.GetFrozenSurfaces world : StructPair<Box3, StaticModelSurfaceValue> array = this.Get (nameof this.FrozenSurfaces) world
        member this.SetFrozenSurfaces (value : StructPair<Box3, StaticModelSurfaceValue> array) world = this.Set (nameof this.FrozenSurfaces) value world
        member this.FrozenSurfaces = lens (nameof this.FrozenSurfaces) this this.GetFrozenSurfaces this.SetFrozenSurfaces
        member this.GetFrozenShapes world : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array = this.Get (nameof this.FrozenShapes) world
        member this.SetFrozenShapes (value : (Box3 * Matrix4x4 * StaticModel AssetTag * int * NavShape * Affine * BodyShape) array) world = this.Set (nameof this.FrozenShapes) value world
        member this.FrozenShapes = lens (nameof this.FrozenShapes) this this.GetFrozenShapes this.SetFrozenShapes
        member this.GetFrozen world : bool = this.Get (nameof this.Frozen) world
        member this.SetFrozen (value : bool) world = this.Set (nameof this.Frozen) value world
        member this.Frozen = lens (nameof this.Frozen) this this.GetFrozen this.SetFrozen
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred
        member this.GetSurfaceMaterialsPopulated world : bool = this.Get (nameof this.SurfaceMaterialsPopulated) world
        member this.SetSurfaceMaterialsPopulated (value : bool) world = this.Set (nameof this.SurfaceMaterialsPopulated) value world
        member this.SurfaceMaterialsPopulated = lens (nameof this.SurfaceMaterialsPopulated) this this.GetSurfaceMaterialsPopulated this.SetSurfaceMaterialsPopulated

        member internal this.RegisterFrozenShapesNav world =
            Array.foldi (fun index world (bounds, matrix, staticModel, surfaceIndex, navShape, _, _) ->
                let navId = { NavIndex = index; NavEntity = this }
                World.setNav3dBodyOpt (Some (bounds, matrix, staticModel, surfaceIndex, navShape)) navId world)
                world (this.GetFrozenShapes world)

        member internal this.RegisterFrozenShapesPhysics world =
            Array.foldi (fun index world (_, _, _, _, _, affine : Affine, bodyShape) ->
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
                      CollisionDetection = Discontinuous
                      CollisionCategories = 1
                      CollisionMask = -1
                      Sensor = false
                      Awake = false
                      BodyIndex = index }
                World.createBody (this.GetIs2d world) bodyId bodyProperties world)
                world (this.GetFrozenShapes world)

        member internal this.RegisterFrozenShapes world =
            let world = this.RegisterFrozenShapesNav world
            let world = this.RegisterFrozenShapesPhysics world
            world

        member internal this.UnregisterFrozenShapesNav world =
            Array.foldi (fun index world _ ->
                let navId = { NavIndex = index; NavEntity = this }
                World.setNav3dBodyOpt None navId world)
                world (this.GetFrozenShapes world)

        member internal this.UnregisterFrozenShapesPhysics world =
            Array.foldi (fun index world _ ->
                let bodyId = { BodySource = this; BodyIndex = index }
                World.destroyBody false bodyId world)
                world (this.GetFrozenShapes world)

        member internal this.UnregisterFrozenShapes world =
            let world = this.UnregisterFrozenShapesNav world
            let world = this.UnregisterFrozenShapesPhysics world
            world

        member internal this.UpdateFrozenHierarchy world =
            if this.GetFrozen world then
                let surfaceMaterialsPopulated = this.GetSurfaceMaterialsPopulated world
                let (frozenSurfaces, frozenShapes, world) = World.freezeEntityHierarchy surfaceMaterialsPopulated this world
                let world = this.SetFrozenSurfaces frozenSurfaces world
                let world = this.SetStatic true world
                let world = if this.GetSelected world then this.UnregisterFrozenShapes world else world
                let world = this.SetFrozenShapes frozenShapes world
                let world = if this.GetSelected world then this.RegisterFrozenShapes world else world
                world
            else
                let world = if this.GetSelected world then this.UnregisterFrozenShapes world else world
                let world = this.SetFrozenShapes [||] world
                let world = this.SetStatic false world
                let world = this.SetFrozenSurfaces [||] world
                let world = World.thawEntityHierarchy (this.GetPresenceConferred world) this world
                world

        /// Permanently freeze a freezer entity's descendents by freezing and then destroying them.
        member this.Permafreeze world =
            let world = this.SetFrozen true world
            let descendents =
                this.GetDescendants world |>
                Array.ofSeq |>
                Array.sortBy (fun descendant -> descendant.Names.Length)
            Array.fold (fun world (descendent : Entity) ->
                if descendent.GetExists world && descendent.GetSurfaceFreezable world
                then World.destroyEntityImmediate descendent world
                else world)
                world descendents

        /// Permanently split a freezer entity's descendents into more spatially-coherent freezers.
        member this.Permasplit world =
            let splitParent =
                match this.Parent with
                | :? Group as group -> group / (this.Name + "Split")
                | :? Entity as entity -> entity / (this.Name + "Split")
                | _ -> failwithumf ()
            let descendents =
                this.GetDescendants world |>
                Array.ofSeq |>
                Array.sortByDescending (fun descendant -> descendant.Names.Length)
            if splitParent.GetExists world then
                Log.error ("Failed to permasplit due to already existing entity '" + scstring splitParent + ".")
                world
            elif Array.exists (fun (descendent : Entity) -> descendent.GetProtected world) descendents then
                Log.error "Failed to permasplit due to protected entity in existing hierarchy."
                world
            else
                let frozen = this.GetFrozen world
                let presence = this.GetPresence world
                let presenceConferred = this.GetPresenceConferred world
                let surfaceMaterialsPopulated = this.GetSurfaceMaterialsPopulated world
                let world = if not frozen then this.SetFrozen true world else world // ensure we're frozen so we get the total bounds from the entity
                let offset = -(this.GetBounds world).Min // use offset to bring div ops into positive space
                let world = this.SetFrozen false world
                let world = World.createEntity DefaultOverlay (Some splitParent.Surnames) splitParent.Group world |> snd
                let splitSize = Constants.Engine.OctnodeSize
                let splits = dictPlus HashIdentity.Structural []
                let world =
                    Array.fold (fun world (descendent : Entity) ->
                        if  descendent.GetExists world &&
                            descendent.GetSurfaceFreezable world &&
                            getType (descendent.GetDispatcher world) <> typeof<Entity3dDispatcher> then
                            let bounds = descendent.GetBounds world
                            let divs = (bounds.Center + offset) / splitSize
                            let evens = v3 (divs.X |> int |> single) (divs.Y |> int |> single) (divs.Z |> int |> single)
                            let splitKey = evens * splitSize - offset
                            let (split, world) =
                                match splits.TryGetValue splitKey with
                                | (true, split : Entity) -> (split, world)
                                | (false, _) ->
                                    let dispatcher = this.GetDispatcher world
                                    let dispatacherName = getTypeName dispatcher
                                    let (split, world) = World.createEntity6 false dispatacherName DefaultOverlay (Some (Array.append splitParent.Surnames [|scstring splitKey|])) this.Group world
                                    let world = split.SetMountOpt (Some (Relation.makeParent ())) world
                                    let world = split.SetStaticModel (AssetTag.makeEmpty ()) world
                                    let world = split.SetPositionLocal splitKey world
                                    let world = split.SetPresence presence world
                                    let world = split.SetPresenceConferred presenceConferred world
                                    let world = split.SetSurfaceMaterialsPopulated surfaceMaterialsPopulated world
                                    let world = split.SetPickable false world
                                    splits.Add (splitKey, split)
                                    (split, world)
                            let descendent' =
                                if descendent.Has<StaticModelSurfaceFacet> world && descendent.Name.StartsWith "Geometry"
                                then split / descendent.Parent.Name // probably generic geometry imported from another engine's scene, so using a likely more descriptive parent name
                                else split / descendent.Name
                            let descendent' =
                                if descendent'.GetExists world
                                then split / (descendent'.Name + Gen.name)
                                else descendent'
                            let world = World.renameEntityImmediate descendent descendent' world
                            world
                        else world)
                        world descendents
                let world = Seq.fold (fun world (split : Entity) -> split.SetFrozen frozen world) world splits.Values
                let world = World.destroyEntityImmediate this world
                let world = World.renameEntityImmediate splitParent this world
                world

    /// Gives an entity the ability to freeze hierarchies of 3D entities.
    type Freezer3dFacet () =
        inherit Facet (false, false, false)

        static let handleUpdateFrozenHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = entity.UpdateFrozenHierarchy world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             nonPersistent Entity.FrozenSurfaces [||]
             nonPersistent Entity.FrozenShapes [||]
             define Entity.Frozen false
             define Entity.PresenceConferred Exterior
             define Entity.SurfaceMaterialsPopulated false]

        override this.Register (entity, world) =
            let world = entity.SetOffset v3Zero world
            let world = World.defer (entity.UpdateFrozenHierarchy) entity world // children not loaded yet, so freeze at end of frame
            let world = World.sense handleUpdateFrozenHierarchy (entity.ChangeEvent (nameof entity.Frozen)) entity (nameof Freezer3dFacet) world
            world

        override this.Render (renderPass, entity, world) =

            // compute intersection function based on render pass
            let intersects =
                let interiorOpt = ValueSome (World.getGameEye3dFrustumInterior Game world)
                let exterior = World.getGameEye3dFrustumExterior Game world
                let imposter = World.getGameEye3dFrustumImposter Game world
                let lightBoxOpt = ValueSome (World.getLight3dViewBox world)
                fun probe light presence bounds ->
                    match renderPass with
                    | NormalPass -> Presence.intersects3d interiorOpt exterior imposter lightBoxOpt probe light presence bounds
                    | LightMapPass (_, lightMapBounds) -> not probe && not light && lightMapBounds.Intersects bounds
                    | ShadowPass (_, _, _, _, frustum) -> not probe && not light && frustum.Intersects bounds
                    | ReflectionPass (_, _) -> false

            // render unculled surfaces
            let bounds = entity.GetBounds world
            let presenceConferred = entity.GetPresenceConferred world
            if intersects false false presenceConferred bounds then
                let surfaces = entity.GetFrozenSurfaces world
                for i in 0 .. dec surfaces.Length do
                    let boundsAndSurface = &surfaces.[i]
                    let bounds = &boundsAndSurface.Fst
                    let surface = &boundsAndSurface.Snd
                    if (not renderPass.IsShadowPass || surface.CastShadow) && intersects false false surface.Presence bounds then
                        World.renderStaticModelSurfaceFast (&surface.ModelMatrix, surface.CastShadow, surface.Presence, Option.toValueOption surface.InsetOpt, &surface.MaterialProperties, &surface.Material, surface.StaticModel, surface.SurfaceIndex, surface.DepthTest, surface.RenderType, renderPass, world)

        override this.RegisterPhysics (entity, world) =
            entity.RegisterFrozenShapesPhysics world

        override this.UnregisterPhysics (entity, world) =
            entity.UnregisterFrozenShapesPhysics world

        override this.RayCast (ray, entity, world) =
            if entity.GetPickable world then
                let intersectionOpt = ray.Intersects (entity.GetBounds world)
                [|Intersection.ofNullable intersectionOpt|]
            else [|Miss|]

        override this.Edit (op, entity, world) =
            match op with
            | AppendProperties append ->
                let world =
                    if ImGui.Button "Permafreeze" then
                        let world = append.EditContext.Snapshot Permafreeze world
                        entity.Permafreeze world
                    else world
                let world =
                    if ImGui.Button "Permasplit" then
                        let world = append.EditContext.Snapshot Permasplit world
                        entity.Permasplit world
                    else world
                world
            | _ -> world

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
                    Convex false (Right entity) world
            entity.UpdateFrozenHierarchy world

        static let handleUpdateLoadedHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = updateLoadedHierarchy entity world
            (Cascade, world)

        static member Facets =
            [typeof<Freezer3dFacet>]

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

    type Entity with
        member this.GetProfile world : Profile = this.Get (nameof this.Profile) world
        member this.SetProfile (value : Profile) world = this.Set (nameof this.Profile) value world
        member this.Profile = lens (nameof this.Profile) this this.GetProfile this.SetProfile

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
                    (entity.GetProfile world)
                    true (Right entity) world
            entity.UpdateFrozenHierarchy world

        static let handleUpdateLoadedHierarchy evt world =
            let entity = evt.Subscriber : Entity
            let world = updateLoadedHierarchy entity world
            (Cascade, world)

        static member Facets =
            [typeof<Freezer3dFacet>]

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.Profile Convex
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
            let world = World.monitor handleUpdateLoadedHierarchy (entity.ChangeEvent (nameof entity.Profile)) entity world
            world

        override this.Edit (op, _, world) =
            match op with
            | ReplaceProperty replace ->
                if replace.PropertyDescriptor.PropertyName = nameof Entity.Loaded then replace.IndicateReplaced ()
                world
            | _ -> world