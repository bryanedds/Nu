// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// A 2d entity dispatcher.
type Entity2dDispatcher (physical, lightProbe, light) =
    inherit EntityDispatcher (true, physical, lightProbe, light)

    static member Properties =
        [define Entity.Size Constants.Engine.Entity2dSizeDefault]

/// A gui entity dispatcher.
type GuiDispatcher () =
    inherit EntityDispatcher (true, false, false, false)

    static member Facets =
        [typeof<LayoutFacet>]

    static member Properties =
        [define Entity.Absolute true
         define Entity.Size Constants.Engine.EntityGuiSizeDefault
         define Entity.Presence Omnipresent
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Layout Manual
         define Entity.LayoutMargin v2Zero
         define Entity.LayoutOrder 0
         define Entity.DockType DockCenter
         define Entity.GridPosition v2iZero]

/// A 3d entity dispatcher.
type Entity3dDispatcher (physical, lightProbe, light) =
    inherit EntityDispatcher (false, physical, lightProbe, light)

    static member Properties =
        [define Entity.Size Constants.Engine.Entity3dSizeDefault]

/// A vui dispatcher (gui in 3d).
type VuiDispatcher () =
    inherit EntityDispatcher (false, false, false, false)

    static member Properties =
        [define Entity.Size Constants.Engine.EntityVuiSizeDefault]

/// Gives an entity the base behavior of a static sprite.
type StaticSpriteDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static member Facets =
        [typeof<StaticSpriteFacet>]

/// Gives an entity the base behavior of an animated sprite.
type AnimatedSpriteDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static member Facets =
        [typeof<AnimatedSpriteFacet>]

/// Gives an entity the base behavior of a gui text control.
type TextDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<TextFacet>]

/// Gives an entity the base behavior of a gui label.
type LabelDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<TextFacet>]

    static member Properties =
        [define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

/// Gives an entity the base behavior of a gui button.
type ButtonDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<TextFacet>
         typeof<ButtonFacet>]

/// Gives an entity the base behavior of gui toggle button.
type ToggleButtonDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<TextFacet>
         typeof<ToggleButtonFacet>]

/// Gives an entity the base behavior of a gui radio button.
type RadioButtonDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<TextFacet>
         typeof<RadioButtonFacet>]

/// Gives an entity the base behavior of gui fill bar.
type FillBarDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<FillBarFacet>]

/// Gives an entity the base behavior of gui feeler (an invisible control that only takes mouse input).
type FeelerDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<FeelerFacet>]

/// Gives an entity the base behavior of a gui text box control.
type TextBoxDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>
         typeof<TextBoxFacet>]

    static member Properties =
        [define Entity.BackdropImageOpt (Some Assets.Default.Label)]

[<AutoOpen>]
module FpsDispatcherExtensions =
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
        let currentDateTime = DateTimeOffset.Now
        let elapsedDateTime = currentDateTime - startDateTime
        if elapsedDateTime.TotalSeconds >= 5.0 then
            entity.SetStartUpdateTime world.UpdateTime world
            entity.SetStartDateTime currentDateTime world

    static member Properties =
        [nonPersistent Entity.StartUpdateTime 0L
         nonPersistent Entity.StartDateTime DateTimeOffset.Now]

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            resetIntermittent entity world
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTimeOffset.Now
            let elapsedDateTime = currentDateTime - startDateTime
            let time = double (world.UpdateTime - entity.GetStartUpdateTime world)
            let frames = time / elapsedDateTime.TotalSeconds
            if not (Double.IsNaN frames) then
                let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                entity.SetText framesStr world

/// Gives an entity the base behavior of a gui panel.
type PanelDispatcher () =
    inherit GuiDispatcher ()

    static let handlePanelLeftDown evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then Resolve
            else Cascade
        else Cascade

    static member Facets =
        [typeof<BackdroppableFacet>]

    static member Properties =
        [define Entity.Size (v3 Constants.Engine.EntityGuiSizeDefault.X Constants.Engine.EntityGuiSizeDefault.X 0.0f)
         define Entity.BackdropImageOpt (Some Assets.Default.Panel)]

    override this.Register (entity, world) =
        World.monitor handlePanelLeftDown Nu.Game.Handle.MouseLeftDownEvent entity world

[<AutoOpen>]
module CursorDispatcherExtensions =
    type Entity with
        member this.GetCursorType world : CursorType = this.Get (nameof this.CursorType) world
        member this.SetCursorType (value : CursorType) world = this.Set (nameof this.CursorType) value world
        member this.CursorType = lens (nameof this.CursorType) this this.GetCursorType this.SetCursorType

/// Gives an entity the base behavior of a cursor.
type CursorDispatcher () =
    inherit GuiDispatcher ()

    static member Properties =
        [define Entity.CursorType DefaultCursor]

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let absolute = entity.GetAbsolute world
            let position = World.getMousePosition2dWorld absolute world
            entity.SetPosition position.V3 world
            World.setCursorType (entity.GetCursorType world) world
            World.setCursorVisible (entity.GetVisible world) world

/// Gives an entity the base behavior of basic static sprite emitter.
type BasicStaticSpriteEmitterDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<BasicStaticSpriteEmitterFacet>]

/// Gives an entity the base behavior of a 2d effect.
type Effect2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<EffectFacet>]

    static member Properties =
        [define Entity.EffectDescriptor (scvalue "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

/// Gives an entity the base behavior of a rigid 2d block using Static physics.
type Block2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

/// Gives an entity the base behavior of a rigid 2d box using Dynamic physics.
type Box2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType Dynamic
         define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })]

/// Gives an entity the base behavior of a rigid 2d sphere using Static physics.
type Sphere2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.BodyShape (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.StaticImage Assets.Default.Ball]

/// Gives an entity the base behavior of a rigid 2d ball using Dynamic physics.
type Ball2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType Dynamic
         define Entity.BodyShape (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.StaticImage Assets.Default.Ball]

[<AutoOpen>]
module Character2dDispatcherExtensions =
    type Entity with
        member this.GetCharacter2dIdleImage world : Image AssetTag = this.Get (nameof this.Character2dIdleImage) world
        member this.SetCharacter2dIdleImage (value : Image AssetTag) world = this.Set (nameof this.Character2dIdleImage) value world
        member this.Character2dIdleImage = lens (nameof this.Character2dIdleImage) this this.GetCharacter2dIdleImage this.SetCharacter2dIdleImage
        member this.GetCharacter2dJumpImage world : Image AssetTag = this.Get (nameof this.Character2dJumpImage) world
        member this.SetCharacter2dJumpImage (value : Image AssetTag) world = this.Set (nameof this.Character2dJumpImage) value world
        member this.Character2dJumpImage = lens (nameof this.Character2dJumpImage) this this.GetCharacter2dJumpImage this.SetCharacter2dJumpImage
        member this.GetCharacter2dWalkSheet world : Image AssetTag = this.Get (nameof this.Character2dWalkSheet) world
        member this.SetCharacter2dWalkSheet (value : Image AssetTag) world = this.Set (nameof this.Character2dWalkSheet) value world
        member this.Character2dWalkSheet = lens (nameof this.Character2dWalkSheet) this this.GetCharacter2dWalkSheet this.SetCharacter2dWalkSheet
        member this.GetCharacter2dFacingLeft world : bool = this.Get (nameof this.Character2dFacingLeft) world
        member this.SetCharacter2dFacingLeft (value : bool) world = this.Set (nameof this.Character2dFacingLeft) value world
        member this.Character2dFacingLeft = lens (nameof this.Character2dFacingLeft) this this.GetCharacter2dFacingLeft this.SetCharacter2dFacingLeft

/// Gives an entity the base behavior of 2d physics-driven character in a platformer.
type Character2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static let computeWalkCelInset time delay (celSize : Vector2) (celRun : int) =
        let compressedTime =
            match (time, delay) with
            | (UpdateTime time, UpdateTime delay) -> time / delay
            | (TickTime time, TickTime delay) -> time / delay
            | (_, _) -> failwith "Cannot operate on incompatible GameTime values."
        let frame = compressedTime % int64 celRun
        let i = single (frame % 3L)
        let j = single (frame / 3L)
        let offset = v2 (i * celSize.X) (j * celSize.Y) 
        box2 offset celSize

    static let propagateGravityToRotation (entity : Entity) world =
        // set rotation to gravity rotated anticlockwise by 90 degrees
        let gravity = entity.GetGravityOverride world |> Option.defaultValue (World.getGravity2d world) |> _.V2
        if gravity <> v2Zero then entity.SetRotation (Quaternion.CreateLookAt2d (gravity.Rotate MathF.PI_OVER_2)) world
        Cascade

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.CelSize (v2 28.0f 28.0f)
         define Entity.CelRun 8
         define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
         define Entity.BodyType Dynamic
         define Entity.AngularFactor v3Zero
         define Entity.SleepingAllowed true
         define Entity.GravityOverride (Some (Constants.Physics.GravityDefault * Constants.Engine.Meter2d * 3.0f))
         define Entity.BodyShape (CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Character2dIdleImage Assets.Default.Character2dIdle
         define Entity.Character2dJumpImage Assets.Default.Character2dJump
         define Entity.Character2dWalkSheet Assets.Default.Character2dWalk
         define Entity.Character2dFacingLeft false]

    override this.Register (entity, world) =
        World.monitor (_.Subscriber >> propagateGravityToRotation) Game.Gravity2dChangeEvent entity world
        World.monitor (_.Subscriber >> propagateGravityToRotation) entity.GravityOverride.ChangeEvent entity world
        propagateGravityToRotation entity world |> ignore

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            // we have to use a bit of hackery to remember whether the character is facing left or
            // right when there is no velocity
            let facingLeft = entity.GetCharacter2dFacingLeft world
            let velocity = (World.getBodyLinearVelocity (entity.GetBodyId world) world).Transform (entity.GetRotation world).Inverted
            if facingLeft && velocity.X > 1.0f then entity.SetCharacter2dFacingLeft false world
            elif not facingLeft && velocity.X < -1.0f then entity.SetCharacter2dFacingLeft true world

    override this.Render (_, entity, world) =
        let bodyId = entity.GetBodyId world
        let facingLeft = entity.GetCharacter2dFacingLeft world
        let celSize = entity.GetCelSize world
        let celRun = entity.GetCelRun world
        let animationDelay = entity.GetAnimationDelay world
        let mutable transform = entity.GetTransform world
        let velocity = entity.GetLinearVelocity(world).Transform transform.Rotation.Inverted
        let struct (insetOpt, image) =
            if not (World.getBodyGrounded bodyId world) then
                let image = entity.GetCharacter2dJumpImage world
                struct (ValueNone, image)
            elif velocity.X < 5.0f && velocity.X > -5.0f then
                let image = entity.GetCharacter2dIdleImage world
                struct (ValueNone, image)
            else
                let image = entity.GetCharacter2dWalkSheet world
                struct (ValueSome (computeWalkCelInset world.GameTime animationDelay celSize celRun), image)
        World.enqueueLayeredOperation2d
            { Elevation = transform.Elevation
              Horizon = transform.Horizon
              AssetTag = image
              RenderOperation2d =
                RenderSprite
                    { Transform = transform
                      InsetOpt = insetOpt
                      ClipOpt = ValueNone
                      Image = image
                      Color = Color.One
                      Blend = Transparent
                      Emission = Color.Zero
                      Flip = if facingLeft then FlipH else FlipNone }}
            world

/// Gives an entity the base behavior of a physics-driven 2d joint.
type BodyJoint2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<BodyJointFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.BodyJoint2dSizeDefault v3Zero

[<AutoOpen>]
module FluidEmitterDispatcherExtensions =
    type Entity with
        
        /// When set to a color, the simulation will render the spatial grid cells for debugging or visualization.
        member this.GetFluidParticleCellColor world : Color option = this.Get (nameof Entity.FluidParticleCellColor) world
        member this.SetFluidParticleCellColor (value : Color option) world = this.Set (nameof Entity.FluidParticleCellColor) value world
        member this.FluidParticleCellColor = lens (nameof Entity.FluidParticleCellColor) this this.GetFluidParticleCellColor this.SetFluidParticleCellColor

        /// The size of the particle image - when None, uses Entity.FluidParticleRadius * Entity.FluidSimulationMeter.
        member this.GetFluidParticleImageSizeOverride world : Vector2 option = this.Get (nameof Entity.FluidParticleImageSizeOverride) world
        member this.SetFluidParticleImageSizeOverride (value : Vector2 option) world = this.Set (nameof Entity.FluidParticleImageSizeOverride) value world
        member this.FluidParticleImageSizeOverride = lens (nameof Entity.FluidParticleImageSizeOverride) this this.GetFluidParticleImageSizeOverride this.SetFluidParticleImageSizeOverride

/// Gives an entity the base behavior of fluid emission.
type FluidEmitter2dDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<FluidEmitter2dFacet>]

    static member Properties =
        [define Entity.FluidParticleImageSizeOverride None
         define Entity.FluidParticleCellColor None
         define Entity.InsetOpt None
         define Entity.ClipOpt None
         define Entity.StaticImage Assets.Default.Fluid
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    override this.Render (_, emitter, world) =

        // collect sim properties
        let particleRadius = emitter.GetFluidParticleRadius world
        let cellSize = particleRadius * emitter.GetFluidCellRatio world
        let drawCells = emitter.GetFluidParticleCellColor world
        let cellPositions = SHashSet.make HashIdentity.Structural
        let staticImage = emitter.GetStaticImage world
        let insetOpt = match emitter.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = emitter.GetClipOpt world |> Option.toValueOption
        let color = emitter.GetColor world
        let blend = emitter.GetBlend world
        let emission = emitter.GetEmission world
        let flip = emitter.GetFlip world
        let drawnSize = emitter.GetFluidParticleImageSizeOverride world |> Option.defaultValue (v2Dup particleRadius)

        // render particles
        let mutable transform = Transform.makeIntuitive false v3Zero v3One v3Zero drawnSize.V3 v3Zero (emitter.GetElevation world)
        for particle in emitter.GetFluidParticles world do
            transform.Position <- particle.FluidParticlePosition
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
            if drawCells.IsSome then cellPositions.Add (FluidEmitter2d.positionToCell cellSize particle.FluidParticlePosition.V2) |> ignore

        // render cells when desired
        match drawCells with
        | Some color ->
            transform.Elevation <- transform.Elevation - 1f
            transform.Size <- v3Dup cellSize
            let staticImage = Assets.Default.White
            for cell in cellPositions do
                let box = FluidEmitter2d.cellToBox cellSize cell
                transform.Position <- box.Center.V3
                World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

        | None -> ()

/// Gives an entity the base behavior of an asset-defined tile map.
type TileMapDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<TileMapFacet>]

/// Gives an entity the base behavior of a user-defined tile map.
type TmxMapDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static member Facets =
        [typeof<TmxMapFacet>]

/// Gives an entity the base behavior of a Spine skeleton.
/// NOTE: Spine skeletons are inherently imperative and therefore currently unsupported by undo / redo.
type SpineSkeletonDispatcher () =
    inherit Entity2dDispatcher (false, false, false)

    static member Facets =
        [typeof<SpineSkeletonFacet>]

/// Gives an entity the base behavior of sky box.
type SkyBoxDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<SkyBoxFacet>]

    override this.PresenceOverride =
        ValueSome Omnipresent

[<AutoOpen>]
module Lighting3dConfigDispatcherExtensions =
    type Entity with
        member this.GetLighting3dConfig world : Lighting3dConfig = this.Get (nameof this.Lighting3dConfig) world
        member this.SetLighting3dConfig (value : Lighting3dConfig) world = this.Set (nameof this.Lighting3dConfig) value world
        member this.Lighting3dConfig = lens (nameof this.Lighting3dConfig) this this.GetLighting3dConfig this.SetLighting3dConfig

/// Gives an entity the base behavior of a 3d lighting configuration.
type Lighting3dConfigDispatcher () =
    inherit Entity3dDispatcher (false, false, false)
    
    static member Properties =
        [define Entity.Presence Omnipresent
         define Entity.Lighting3dConfig Lighting3dConfig.defaultConfig]

    override this.Render (_, entity, world) =
        let config = entity.GetLighting3dConfig world
        World.enqueueRenderMessage3d (ConfigureLighting3d config) world

/// Gives an entity the base behavior of a 3d light probe.
type LightProbe3dDispatcher () =
    inherit Entity3dDispatcher (false, true, false)

    static member Facets =
        [typeof<LightProbe3dFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero

/// Gives an entity the base behavior of a 3d light.
type Light3dDispatcher () =
    inherit Entity3dDispatcher (false, false, true)

    static member Facets =
        [typeof<Light3dFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero

/// Gives an entity the base behavior of a static billboard.
type StaticBillboardDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<StaticBillboardFacet>]

/// Gives an entity the base behavior of an animated billboard.
type AnimatedBillboardDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<AnimatedBillboardFacet>]

/// Gives an entity the base behavior of a static model.
type StaticModelDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<StaticModelFacet>]

/// Gives an entity the base behavior of an animated model.
type AnimatedModelDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<AnimatedModelFacet>]

/// Gives an entity the base behavior of physics-driven sensor model.
type SensorModelDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        match bodyShape with
        | StaticModelShape staticModelShape ->
            if staticModelShape.StaticModel <> staticModel then
                let staticModelShape =
                    { StaticModel = staticModel
                      Profile = staticModelShape.Profile
                      TransformOpt = staticModelShape.TransformOpt
                      PropertiesOpt = staticModelShape.PropertiesOpt }
                entity.SetBodyShape (StaticModelShape staticModelShape) world
        | _ -> ()
        Cascade

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>]

    static member Properties =
        [define Entity.Visible false
         define Entity.BodyShape (StaticModelShape { StaticModel = Assets.Default.StaticModel; Profile = Convex; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.NavShape EmptyNavShape]

    override this.Register (entity, world) =
        World.monitor updateBodyShape entity.StaticModel.ChangeEvent entity world
        World.monitor updateBodyShape entity.BodyShape.ChangeEvent entity world

/// Gives an entity the base behavior of physics-driven rigid model.
type RigidModelDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        match bodyShape with
        | StaticModelShape staticModelShape ->
            if staticModelShape.StaticModel <> staticModel then
                let staticModelShape =
                    { StaticModel = staticModel
                      Profile = staticModelShape.Profile
                      TransformOpt = staticModelShape.TransformOpt
                      PropertiesOpt = staticModelShape.PropertiesOpt }
                entity.SetBodyShape (StaticModelShape staticModelShape) world
        | _ -> ()
        Cascade

    static let updateNavShape evt world =
        let entity = evt.Subscriber : Entity
        match entity.GetBodyType world with
        | Static -> entity.SetNavShape BoundsNavShape world
        | Kinematic | KinematicCharacter | Dynamic | DynamicCharacter | Vehicle -> entity.SetNavShape NavShape.EmptyNavShape world
        Cascade

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (StaticModelShape { StaticModel = Assets.Default.StaticModel; Profile = Convex; TransformOpt = None; PropertiesOpt = None })
         define Entity.NavShape StaticModelNavShape]

    override this.Register (entity, world) =
        World.monitor updateBodyShape entity.StaticModel.ChangeEvent entity world
        World.monitor updateBodyShape entity.BodyShape.ChangeEvent entity world
        World.monitor updateNavShape entity.BodyType.ChangeEvent entity world

/// Gives an entity the base behavior of an indexed static model.
type StaticModelSurfaceDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<StaticModelSurfaceFacet>]

/// Gives an entity the base behavior of an indexed, physics-driven sensor model.
type SensorModelSurfaceDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        let surfaceIndex = entity.GetSurfaceIndex world
        match bodyShape with
        | StaticModelSurfaceShape staticModelSurfaceShape ->
            if staticModelSurfaceShape.StaticModel <> staticModel || staticModelSurfaceShape.SurfaceIndex <> surfaceIndex then
                let staticModelShape =
                    { StaticModel = staticModel
                      SurfaceIndex = surfaceIndex
                      Profile = staticModelSurfaceShape.Profile
                      TransformOpt = staticModelSurfaceShape.TransformOpt
                      PropertiesOpt = staticModelSurfaceShape.PropertiesOpt }
                entity.SetBodyShape (StaticModelSurfaceShape staticModelShape) world
        | _ -> ()
        Cascade

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelSurfaceFacet>]

    static member Properties =
        [define Entity.Visible false
         define Entity.BodyShape (StaticModelSurfaceShape { StaticModel = Assets.Default.StaticModel; SurfaceIndex = 0; Profile = Convex; TransformOpt = None; PropertiesOpt = None })
         define Entity.Sensor true
         define Entity.NavShape EmptyNavShape]

    override this.Register (entity, world) =
        World.monitor updateBodyShape entity.StaticModel.ChangeEvent entity world
        World.monitor updateBodyShape entity.SurfaceIndex.ChangeEvent entity world

/// Gives an entity the base behavior of an indexed, physics-driven rigid model.
type RigidModelSurfaceDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        let surfaceIndex = entity.GetSurfaceIndex world
        match bodyShape with
        | StaticModelSurfaceShape staticModelSurfaceShape ->
            if staticModelSurfaceShape.StaticModel <> staticModel || staticModelSurfaceShape.SurfaceIndex <> surfaceIndex then
                let staticModelShape =
                    { StaticModel = staticModel
                      SurfaceIndex = surfaceIndex
                      Profile = staticModelSurfaceShape.Profile
                      TransformOpt = staticModelSurfaceShape.TransformOpt
                      PropertiesOpt = staticModelSurfaceShape.PropertiesOpt }
                entity.SetBodyShape (StaticModelSurfaceShape staticModelShape) world
        | _ -> ()
        Cascade

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelSurfaceFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (StaticModelSurfaceShape { StaticModel = Assets.Default.StaticModel; SurfaceIndex = 0; Profile = Convex; TransformOpt = None; PropertiesOpt = None })
         define Entity.NavShape StaticModelSurfaceNavShape]

    override this.Register (entity, world) =
        World.monitor updateBodyShape entity.StaticModel.ChangeEvent entity world
        World.monitor updateBodyShape entity.SurfaceIndex.ChangeEvent entity world
        World.monitor updateBodyShape entity.BodyShape.ChangeEvent entity world

/// Gives an entity the base behavior of basic static billboard emitter.
type BasicStaticBillboardEmitterDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<BasicStaticBillboardEmitterFacet>]

/// Gives an entity the base behavior of a 3d effect.
type Effect3dDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<EffectFacet>]

    static member Properties =
        [define Entity.EffectDescriptor (scvalue "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[Billboard [Resource Default MaterialAlbedo] [Resource Default MaterialRoughness] [Resource Default MaterialMetallic] [Resource Default MaterialAmbientOcclusion] [Resource Default MaterialEmission] [Resource Default MaterialNormal] [Resource Default MaterialHeightMap] True True [] Nil]]]]]")]

/// Gives an entity the base behavior of a rigid 3d block using Static physics.
type Block3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

/// Gives an entity the base behavior of a rigid 3d box using Dynamic physics.
type Box3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType Dynamic]

/// Gives an entity the base behavior of a rigid 3d sphere using Static physics.
type Sphere3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.StaticModel Assets.Default.BallModel]

/// Gives an entity the base behavior of a rigid 3d ball using Dynamic physics.
type Ball3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType Dynamic
         define Entity.BodyShape (SphereShape { Radius = 0.5f; TransformOpt = None; PropertiesOpt = None })
         define Entity.StaticModel Assets.Default.BallModel]

/// Gives an entity the base behavior of a 3d character.
type Character3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<AnimatedModelFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })]

    override this.Update (entity, world) =
        let rotation = entity.GetRotation world
        let linearVelocity = entity.GetLinearVelocity world
        let angularVelocity = entity.GetAngularVelocity world
        let forwardness = linearVelocity.Dot rotation.Forward
        let backness = linearVelocity.Dot -rotation.Forward
        let rightness = linearVelocity.Dot rotation.Right
        let leftness = linearVelocity.Dot -rotation.Right
        let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 0.5f else 0.0f
        let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 0.5f else 0.0f
        let animations =
            [Animation.make GameTime.zero None "Idle" Loop 1.0f 1.0f None]
        let animations =
            if forwardness >= 0.01f then Animation.make GameTime.zero None "WalkForward" Loop 1.0f (max 0.025f forwardness) None :: animations
            elif backness >= 0.01f then Animation.make GameTime.zero None "WalkBack" Loop 1.0f (max 0.025f backness) None :: animations
            else animations
        let animations =
            if rightness >= 0.01f then Animation.make GameTime.zero None "WalkRight" Loop 1.0f (max 0.025f rightness) None :: animations
            elif leftness >= 0.01f then Animation.make GameTime.zero None "WalkLeft" Loop 1.0f (max 0.025f leftness) None :: animations
            else animations
        let animations =
            if turnRightness >= 0.01f then Animation.make GameTime.zero None "TurnRight" Loop 1.0f (max 0.025f turnRightness) None :: animations
            elif turnLeftness >= 0.01f then Animation.make GameTime.zero None "TurnLeft" Loop 1.0f (max 0.025f turnLeftness) None :: animations
            else animations
        entity.SetAnimations (List.toArray animations) world

/// Gives an entity the base behavior of a physics-driven 3d joint.
type BodyJoint3dDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<BodyJointFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.BodyJoint3dSizeDefault v3Zero

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        [|Intersection.ofNullable intersectionOpt|]

/// Gives an entity the base behavior of a rigid 3d terrain.
type TerrainDispatcher () =
    inherit Entity3dDispatcher (true, false, false)

    static member Facets =
        [typeof<TerrainFacet>]

[<AutoOpen>]
module Nav3dConfigDispatcherExtensions =
    type Entity with
        member this.GetNav3dConfig world : Nav3dConfig = this.Get (nameof this.Nav3dConfig) world
        member this.SetNav3dConfig (value : Nav3dConfig) world = this.Set (nameof this.Nav3dConfig) value world
        member this.Nav3dConfig = lens (nameof this.Nav3dConfig) this this.GetNav3dConfig this.SetNav3dConfig

/// Gives an entity the base behavior of a navigation mesh.
type Nav3dConfigDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static let propagateNav3dConfig (entity : Entity) world =
        let config = entity.GetNav3dConfig world
        World.setNav3dConfig config entity.Screen world

    static member Properties =
        [define Entity.Nav3dConfig Nav3dConfig.defaultConfig]

    override this.Register (entity, world) =
        World.monitor (fun _ world -> propagateNav3dConfig entity world; Cascade) (entity.ChangeEvent (nameof entity.Nav3dConfig)) entity world

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->
            let nav3d = World.getScreenNav3d entity.Screen world
            match nav3d.Nav3dMeshOpt with
            | Some (_, nbrData, _, _) ->

                // edge color compute lambda
                let computeEdgeColor (edge : Segment3) =
                    let middleY = edge.A.Y + edge.B.Y * 0.5f
                    let height = Math.Lerp (0.0f, 1.0f, (middleY - nbrData.NavEdgesMinY) / (nbrData.NavEdgesMaxY - nbrData.NavEdgesMinY))
                    Color (1.0f, 1.0f - height, height, 1.0f)

                // draw edges and points
                World.imGuiSegments3dPlus nbrData.NavInteriorEdges 1.0f computeEdgeColor world
                World.imGuiSegments3dPlus nbrData.NavExteriorEdges 1.0f computeEdgeColor world

            | None -> ()
        | _ -> ()

/// Enables common operations on 3D entities that intersect this entity's bounds.
/// TODO: P1: implement EditAreaDispatcher for 2D entities.
type EditVolumeDispatcher () =
    inherit Entity3dDispatcher (false, false, false)

    static member Facets =
        [typeof<EditVolumeFacet>]