// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module EntityDispatcherModule =

    /// A 2d entity dispatcher.
    type EntityDispatcher2d (centered, physical) =
        inherit EntityDispatcher (true, centered, physical)

        new (physical) =
            EntityDispatcher2d (Constants.Engine.EntityCentered2dDefault, physical)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize2dDefault
             define Entity.Centered Constants.Engine.EntityCentered2dDefault]

    /// A gui entity dispatcher.
    type GuiDispatcher () =
        inherit EntityDispatcher (true, Constants.Engine.EntityCenteredGuiDefault, false)

        static member Facets =
            [typeof<LayoutFacet>]

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.Centered Constants.Engine.EntityCenteredGuiDefault
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Presence Omnipresent
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

    /// A 3d entity dispatcher.
    type EntityDispatcher3d (centered, physical) =
        inherit EntityDispatcher (false, centered, physical)

        new (physical) =
            EntityDispatcher3d (Constants.Engine.EntityCentered3dDefault, physical)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize3dDefault
             define Entity.Centered Constants.Engine.EntityCentered3dDefault]

        override this.RayCast (ray, entity, world) =
            if Array.isEmpty (entity.GetFacets world) then
                let intersectionOpt = ray.Intersects (entity.GetBounds world)
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            else base.RayCast (ray, entity, world)

    /// A vui dispatcher (gui in 3d).
    and [<AbstractClass>] VuiDispatcher () =
        inherit EntityDispatcher (false, Constants.Engine.EntityCenteredVuiDefault, false)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySizeVuiDefault
             define Entity.Centered Constants.Engine.EntityCenteredVuiDefault]

[<AutoOpen>]
module StaticSpriteDispatcherModule =

    /// Gives an entity the base behavior of a static sprite.
    type StaticSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Box
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

[<AutoOpen>]
module AnimatedSpriteDispatcherModule =

    /// Gives an entity the base behavior of an animated sprite.
    type AnimatedSpriteDispatcher () =
        inherit EntityDispatcher2d (false)

        static member Facets =
            [typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AnimationSheet Assets.Default.Block // TODO: use proper animated sheet.
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

[<AutoOpen>]
module TextDispatcherModule =

    /// Gives an entity the base behavior of a gui text control.
    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>
             typeof<BackdroppableFacet>]

        static member Properties =
            [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

        override this.GetQuickSize (entity, world) =
            match entity.GetBackdropImageOpt world with
            | Some image ->
                match Metadata.tryGetTextureSizeF image with
                | Some size -> size.V3
                | None -> Constants.Engine.EntitySizeGuiDefault
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module LabelDispatcherModule =

    /// Gives an entity the base behavior of a gui label.
    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<LabelFacet>]

[<AutoOpen>]
module ButtonDispatcherModule =

    /// Gives an entity the base behavior of a gui button.
    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>
             typeof<ButtonFacet>]

[<AutoOpen>]
module ToggleButtonDispatcherModule =

    /// Gives an entity the base behavior of gui toggle button.
    type ToggleButtonDispatcher () =
        inherit GuiDispatcher ()
        
        static member Facets =
            [typeof<TextFacet>
             typeof<ToggleButtonFacet>]

[<AutoOpen>]
module RadioButtonDispatcherModule =

    /// Gives an entity the base behavior of a gui radio button.
    type RadioButtonDispatcher () =
        inherit GuiDispatcher ()
        
        static member Facets =
            [typeof<TextFacet>
             typeof<RadioButtonFacet>]

[<AutoOpen>]
module FillBarDispatcherModule =

    /// Gives an entity the base behavior of gui fill bar.
    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        static member Facets =
            [typeof<FillBarFacet>]

[<AutoOpen>]
module FeelerDispatcherModule =

    /// Gives an entity the base behavior of gui feeler (an invisible control that only takes mouse input).
    type FeelerDispatcher () =
        inherit GuiDispatcher ()
        
        static member Facets =
            [typeof<FeelerFacet>]

[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
        member this.GetStartUpdateTime world : int64 = this.Get (nameof this.StartUpdateTime) world
        member this.SetStartUpdateTime (value : int64) world = this.Set (nameof this.StartUpdateTime) value world
        member this.StartUpdateTime = lens (nameof this.StartUpdateTime) this this.GetStartUpdateTime this.SetStartUpdateTime
        member this.GetStartDateTime world : DateTimeOffset = this.Get (nameof this.StartDateTime) world
        member this.SetStartDateTime (value : DateTimeOffset) world = this.Set (nameof this.StartDateTime) value world
        member this.StartDateTime = lens (nameof this.StartDateTime) this this.GetStartDateTime this.SetStartDateTime

    /// Gives an entity the base behavior of a gui FPS counter.
    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTimeOffset.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 5.0 then
                let world = entity.SetStartUpdateTime world.UpdateTime world
                entity.SetStartDateTime currentDateTime world
            else world

        static member Properties =
            [nonPersistent Entity.StartUpdateTime 0L
             nonPersistent Entity.StartDateTime DateTimeOffset.UtcNow]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let world = resetIntermittent entity world
                let startDateTime = entity.GetStartDateTime world
                let currentDateTime = DateTimeOffset.UtcNow
                let elapsedDateTime = currentDateTime - startDateTime
                let time = double (world.UpdateTime - entity.GetStartUpdateTime world)
                let frames = time / elapsedDateTime.TotalSeconds
                if not (Double.IsNaN frames) then
                    let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                    entity.SetText framesStr world
                else world
            else world

[<AutoOpen>]
module BasicStaticSpriteEmitterDispatcherModule =

    /// Gives an entity the base behavior of basic static sprite emitter.
    type BasicStaticSpriteEmitterDispatcher () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<BasicStaticSpriteEmitterFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher2dModule =

    /// Gives an entity the base behavior of a 2d effect.
    type EffectDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<EffectFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

[<AutoOpen>]
module BlockDispatcher2dModule =

    /// Gives an entity the base behavior of a rigid 2d block using static physics.
    type BlockDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage Assets.Default.Block]

[<AutoOpen>]
module BoxDispatcher2dModule =

    /// Gives an entity the base behavior of a rigid 2d box using dynamic physics.
    type BoxDispatcher2d () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Box]

[<AutoOpen>]
module SideViewCharacterDispatcherModule =

    type Entity with
        member this.GetSideViewCharacterIdleImage world : Image AssetTag = this.Get (nameof this.SideViewCharacterIdleImage) world
        member this.SetSideViewCharacterIdleImage (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterIdleImage) value world
        member this.SideViewCharacterIdleImage = lens (nameof this.SideViewCharacterIdleImage) this this.GetSideViewCharacterIdleImage this.SetSideViewCharacterIdleImage
        member this.GetSideViewCharacterJumpImage world : Image AssetTag = this.Get (nameof this.SideViewCharacterJumpImage) world
        member this.SetSideViewCharacterJumpImage (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterJumpImage) value world
        member this.SideViewCharacterJumpImage = lens (nameof this.SideViewCharacterJumpImage) this this.GetSideViewCharacterJumpImage this.SetSideViewCharacterJumpImage
        member this.GetSideViewCharacterWalkSheet world : Image AssetTag = this.Get (nameof this.SideViewCharacterWalkSheet) world
        member this.SetSideViewCharacterWalkSheet (value : Image AssetTag) world = this.Set (nameof this.SideViewCharacterWalkSheet) value world
        member this.SideViewCharacterWalkSheet = lens (nameof this.SideViewCharacterWalkSheet) this this.GetSideViewCharacterWalkSheet this.SetSideViewCharacterWalkSheet
        member this.GetSideViewCharacterFacingLeft world : bool = this.Get (nameof this.SideViewCharacterFacingLeft) world
        member this.SetSideViewCharacterFacingLeft (value : bool) world = this.Set (nameof this.SideViewCharacterFacingLeft) value world
        member this.SideViewCharacterFacingLeft = lens (nameof this.SideViewCharacterFacingLeft) this this.GetSideViewCharacterFacingLeft this.SetSideViewCharacterFacingLeft

    /// Gives an entity the base behavior of 2d physics-driven character in a platformer.
    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher2d (true)

        static let computeWalkCelInset time delay (celSize : Vector2) (celRun : int) =
            let compressedTime =
                match (time, delay) with
                | (UpdateTime time, UpdateTime delay) -> time / delay
                | (ClockTime time, ClockTime delay) -> time / delay |> int64
                | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
            let frame = compressedTime % int64 celRun
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            box2 offset celSize

        static member Facets =
            [typeof<RigidBodyFacet>]

        static member Properties =
            [define Entity.CelSize (v2 28.0f 28.0f)
             define Entity.CelRun 8
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AngularFactor v3Zero
             define Entity.GravityOverride (Some (Constants.Physics.Gravity2dDefault * 3.0f))
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
             define Entity.SideViewCharacterIdleImage Assets.Default.SideViewCharacterIdleImage
             define Entity.SideViewCharacterJumpImage Assets.Default.SideViewCharacterJumpImage
             define Entity.SideViewCharacterWalkSheet Assets.Default.SideViewCharacterWalkImage
             define Entity.SideViewCharacterFacingLeft false]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                // we have to use a bit of hackery to remember whether the character is facing left or
                // right when there is no velocity
                let facingLeft = entity.GetSideViewCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity (entity.GetBodyId world) world
                if facingLeft && velocity.X > 1.0f then entity.SetSideViewCharacterFacingLeft false world
                elif not facingLeft && velocity.X < -1.0f then entity.SetSideViewCharacterFacingLeft true world
                else world
            else world

        override this.Render (entity, world) =
            let bodyId = entity.GetBodyId world
            let facingLeft = entity.GetSideViewCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity bodyId world
            let celSize = entity.GetCelSize world
            let celRun = entity.GetCelRun world
            let animationDelay = entity.GetAnimationDelay world
            let mutable transform = entity.GetTransform world
            let struct (insetOpt, image) =
                if not (World.getBodyGrounded bodyId world) then
                    let image = entity.GetSideViewCharacterJumpImage world
                    struct (ValueNone, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = entity.GetSideViewCharacterIdleImage world
                    struct (ValueNone, image)
                else
                    let image = entity.GetSideViewCharacterWalkSheet world
                    struct (ValueSome (computeWalkCelInset world.GameTime animationDelay celSize celRun), image)
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = AssetTag.generalize image
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = insetOpt
                          Image = image
                          Color = Color.One
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = if facingLeft then FlipH else FlipNone }}
                world

[<AutoOpen>]
module TileMapDispatcherModule =

    /// Gives an entity the base behavior of an asset-defined tile map.
    type TileMapDispatcher () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<TileMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap]

[<AutoOpen>]
module TmxMapDispatcherModule =

    /// Gives an entity the base behavior of a user-defined tile map.
    type TmxMapDispatcher () =
        inherit EntityDispatcher2d (true)

        static member Facets =
            [typeof<TmxMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())]

[<AutoOpen>]
module SkyBoxDispatcherModule =

    /// Gives an entity the base behavior of sky box.
    type SkyBoxDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<SkyBoxFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

[<AutoOpen>]
module LightProbeDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d light probe.
    type LightProbeDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightProbeFacet3d>]

        static member Properties =
            [define Entity.LightProbe true
             define Entity.Presence Omnipresent
             define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
             define Entity.ProbeStale false]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module LightDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d light.
    type LightDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightFacet3d>]

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness Constants.Render.BrightnessDefault
             define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
             define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
             define Entity.LightType PointLight]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardDispatcherModule =

    /// Gives an entity the base behavior of a static billboard.
    type StaticBillboardDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticBillboardFacet>]

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.MetallicImage Assets.Default.MaterialMetallic
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.EmissionImage Assets.Default.MaterialEmission
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.HeightImage Assets.Default.MaterialHeight
             define Entity.TextureMinFilterOpt None
             define Entity.TextureMagFilterOpt None
             define Entity.RenderStyle Deferred]

// TODO: AnimatedBillboardDispatcher.

[<AutoOpen>]
module StaticModelDispatcherModule =

    /// Gives an entity the base behavior of a static model.
    type StaticModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module RigidModelDispatcherModule =

    /// Gives an entity the base behavior of physics-driven rigid model.
    type RigidModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let bodyShape = entity.GetBodyShape world
            let staticModel = entity.GetStaticModel world
            if (match bodyShape with BodyStaticModel body -> body.StaticModel <> staticModel | _ -> false) then
                let bodyStaticModel = { StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                let world = entity.SetBodyShape (BodyStaticModel bodyStaticModel) world
                (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.BodyShape (BodyStaticModel { StaticModel = Assets.Default.StaticModel; TransformOpt = None; PropertiesOpt = None })
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

[<AutoOpen>]
module StaticModelSurfaceDispatcherModule =

    /// Gives an entity the base behavior of an indexed static model.
    type StaticModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module RigidModelSurfaceDispatcherModule =

    /// Gives an entity the base behavior of an indexed, physics-driven rigid model.
    type RigidModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let bodyShape = entity.GetBodyShape world
            let surfaceIndex = entity.GetSurfaceIndex world
            let staticModel = entity.GetStaticModel world
            if (match bodyShape with BodyStaticModelSurface body -> body.SurfaceIndex <> surfaceIndex || body.StaticModel <> staticModel | _ -> false) then
                let bodyStaticModel = { SurfaceIndex = surfaceIndex; StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                let world = entity.SetBodyShape (BodyStaticModelSurface bodyStaticModel) world
                (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.BodyShape (BodyStaticModelSurface { SurfaceIndex = 0; StaticModel = Assets.Default.StaticModel; TransformOpt = None; PropertiesOpt = None })
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.SurfaceIndex)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type World with

        /// Attempt to import a static model hierarchy below the target entity.
        static member tryImportHierarchy rigid presence staticModel (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                let mutable (world', i) = (world, 0) // using mutation due to imperative API
                staticModelMetadata.PhysicallyBasedStaticHierarchy.Traverse (fun nodes ->
                    for node in nodes do
                        match node with
                        | OpenGL.PhysicallyBased.PhysicallyBasedNode names ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (names.Length > 0, names, group)
                                | Right entity -> (true, Array.append entity.Surnames names, entity.Group)
                            let (child, world) = World.createEntity<EntityDispatcher3d> DefaultOverlay (Some surnames) group world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
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
                            let world = child.QuickSize world
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
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
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
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetPresence presence world
                            let world = child.SetStatic true world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let materialProperties =
                                { AlbedoOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Albedo
                                  MetallicOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Metallic
                                  RoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Roughness
                                  AmbientOcclusionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.AmbientOcclusion
                                  EmissionOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Emission
                                  HeightOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.Height
                                  InvertRoughnessOpt = ValueSome surface.SurfaceMaterial.MaterialProperties.InvertRoughness }
                            let world = child.SetMaterialProperties materialProperties world
                            let world = child.QuickSize world
                            world' <- world
                            i <- inc i)
                world'
            | None -> world

    type Entity with
        member this.GetPresenceConferred world : Presence = this.Get (nameof this.PresenceConferred) world
        member this.SetPresenceConferred (value : Presence) world = this.Set (nameof this.PresenceConferred) value world
        member this.PresenceConferred = lens (nameof this.PresenceConferred) this this.GetPresenceConferred this.SetPresenceConferred
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    /// Gives an entity the base behavior of hierarchy of indexed static models.
    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportHierarchy false (entity.GetPresenceConferred world) (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.PresenceConferred Exposed
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportHierarchy false (entity.GetPresence world) (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world

[<AutoOpen>]
module RigidModelHierarchyDispatcherModule =

    /// Gives an entity the base behavior of a hierarchy of indexed, physics-driven rigid models.
    type RigidModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportHierarchy true (entity.GetPresenceConferred world) (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.PresenceConferred Exposed
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportHierarchy true (entity.GetPresence world) (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.PresenceConferred)) entity world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world

[<AutoOpen>]
module BasicStaticBillboardEmitterDispatcherModule =

    /// Gives an entity the base behavior of basic static billboard emitter.
    type BasicStaticBillboardEmitterDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<BasicStaticBillboardEmitterFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher3dModule =

    /// Gives an entity the base behavior of a 3d effect.
    type EffectDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<EffectFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

[<AutoOpen>]
module BlockDispatcher3dModule =

    /// Gives an entity the base behavior of a rigid 3d block using static physics.
    type BlockDispatcher3d () =
        inherit EntityDispatcher3d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticModel Assets.Default.StaticModel]

[<AutoOpen>]
module BoxDispatcher3dModule =

    /// Gives an entity the base behavior of a rigid 3d box using dynamic physics.
    type BoxDispatcher3d () =
        inherit EntityDispatcher3d (true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.BodyType Dynamic
             define Entity.StaticModel Assets.Default.StaticModel]

[<AutoOpen>]
module TerrainDispatcher3dModule =

    /// Gives an entity the base behavior of a rigid 3d terrain.
    type TerrainDispatcher () =
        inherit EntityDispatcher3d (true)

        static member Facets =
            [typeof<TerrainFacet>]

        static member Properties =
            [define Entity.Size (v3 1024.0f 128.0f 1024.0f)
             define Entity.Presence Omnipresent]