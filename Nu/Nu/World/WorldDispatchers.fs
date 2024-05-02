// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// A 2d entity dispatcher.
type Entity2dDispatcher (perimeterCentered, physical) =
    inherit EntityDispatcher (true, perimeterCentered, physical)

    new (physical) =
        Entity2dDispatcher (Constants.Engine.Entity2dPerimeterCenteredDefault, physical)

    static member Properties =
        [define Entity.Size Constants.Engine.Entity2dSizeDefault
         define Entity.PerimeterCentered Constants.Engine.Entity2dPerimeterCenteredDefault]

/// A gui entity dispatcher.
type GuiDispatcher () =
    inherit EntityDispatcher (true, Constants.Engine.EntityGuiPerimeterCenteredDefault, false)

    static member Facets =
        [typeof<LayoutFacet>]

    static member Properties =
        [define Entity.Size Constants.Engine.EntityGuiSizeDefault
         define Entity.PerimeterCentered Constants.Engine.EntityGuiPerimeterCenteredDefault
         define Entity.Absolute true
         define Entity.Presence Omnipresent
         define Entity.DisabledColor Constants.Gui.DisabledColor]

/// A 3d entity dispatcher.
type Entity3dDispatcher (physical) =
    inherit EntityDispatcher (false, true, physical)

    new (physical) =
        Entity3dDispatcher (physical)

    static member Properties =
        [define Entity.Size Constants.Engine.Entity3dSizeDefault]

    override this.RayCast (ray, entity, world) =
        if Array.isEmpty (entity.GetFacets world) then
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]
        else base.RayCast (ray, entity, world)

/// A vui dispatcher (gui in 3d).
type VuiDispatcher () =
    inherit EntityDispatcher (false, true, false)

    static member Properties =
        [define Entity.Size Constants.Engine.EntityVuiSizeDefault]

/// Gives an entity the base behavior of a static sprite.
type StaticSpriteDispatcher () =
    inherit Entity2dDispatcher (false)

    static member Facets =
        [typeof<StaticSpriteFacet>]

/// Gives an entity the base behavior of an animated sprite.
type AnimatedSpriteDispatcher () =
    inherit Entity2dDispatcher (false)

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
            let world = entity.SetStartUpdateTime world.UpdateTime world
            entity.SetStartDateTime currentDateTime world
        else world

    static member Properties =
        [nonPersistent Entity.StartUpdateTime 0L
         nonPersistent Entity.StartDateTime DateTimeOffset.Now]

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let world = resetIntermittent entity world
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTimeOffset.Now
            let elapsedDateTime = currentDateTime - startDateTime
            let time = double (world.UpdateTime - entity.GetStartUpdateTime world)
            let frames = time / elapsedDateTime.TotalSeconds
            if not (Double.IsNaN frames) then
                let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                entity.SetText framesStr world
            else world
        else world

/// Gives an entity the base behavior of a gui panel.
type PanelDispatcher () =
    inherit GuiDispatcher ()

    static member Facets =
        [typeof<BackdroppableFacet>]

    static member Properties =
        [define Entity.BackdropImageOpt (Some Assets.Default.Panel)]

/// Gives an entity the base behavior of basic static sprite emitter.
type BasicStaticSpriteEmitterDispatcher () =
    inherit Entity2dDispatcher (true, false)

    static member Facets =
        [typeof<BasicStaticSpriteEmitterFacet>]

    static member Properties =
        [define Entity.PerimeterCentered true]

/// Gives an entity the base behavior of a 2d effect.
type Effect2dDispatcher () =
    inherit Entity2dDispatcher (true, false)

    static member Facets =
        [typeof<EffectFacet>]

    static member Properties =
        [define Entity.PerimeterCentered true
         define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

/// Gives an entity the base behavior of a rigid 2d block using static physics.
type Block2dDispatcher () =
    inherit Entity2dDispatcher (true)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.Static true
         define Entity.Color Color.Gray]

/// Gives an entity the base behavior of a rigid 2d box using dynamic physics.
type Box2dDispatcher () =
    inherit Entity2dDispatcher (true)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticSpriteFacet>]

    static member Properties =
        [define Entity.BodyType Dynamic]

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
    inherit Entity2dDispatcher (true)

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
         define Entity.BodyType Dynamic
         define Entity.SleepingAllowed true
         define Entity.GravityOverride (Some (Constants.Physics.GravityDefault * Constants.Engine.Meter2d * 3.0f))
         define Entity.BodyShape (CapsuleShape { Height = 0.5f; Radius = 0.25f; TransformOpt = None; PropertiesOpt = None })
         define Entity.Character2dIdleImage Assets.Default.Character2dIdle
         define Entity.Character2dJumpImage Assets.Default.Character2dJump
         define Entity.Character2dWalkSheet Assets.Default.Character2dWalk
         define Entity.Character2dFacingLeft false]

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            // we have to use a bit of hackery to remember whether the character is facing left or
            // right when there is no velocity
            let facingLeft = entity.GetCharacter2dFacingLeft world
            let velocity = World.getBodyLinearVelocity (entity.GetBodyId world) world
            if facingLeft && velocity.X > 1.0f then entity.SetCharacter2dFacingLeft false world
            elif not facingLeft && velocity.X < -1.0f then entity.SetCharacter2dFacingLeft true world
            else world
        else world

    override this.Render (_, entity, world) =
        let bodyId = entity.GetBodyId world
        let facingLeft = entity.GetCharacter2dFacingLeft world
        let velocity = entity.GetLinearVelocity world
        let celSize = entity.GetCelSize world
        let celRun = entity.GetCelRun world
        let animationDelay = entity.GetAnimationDelay world
        let mutable transform = entity.GetTransform world
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
                      Image = image
                      Color = Color.One
                      Blend = Transparent
                      Emission = Color.Zero
                      Flip = if facingLeft then FlipH else FlipNone }}
            world

/// Gives an entity the base behavior of a physics-driven 2d joint.
type BodyJoint2dDispatcher () =
    inherit Entity2dDispatcher (true)

    static member Facets =
        [typeof<BodyJointFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.BodyJoint2dSizeDefault v3Zero

/// Gives an entity the base behavior of an asset-defined tile map.
type TileMapDispatcher () =
    inherit Entity2dDispatcher (true)

    static member Facets =
        [typeof<TileMapFacet>]

/// Gives an entity the base behavior of a user-defined tile map.
type TmxMapDispatcher () =
    inherit Entity2dDispatcher (true)

    static member Facets =
        [typeof<TmxMapFacet>]

/// Gives an entity the base behavior of sky box.
type SkyBoxDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<SkyBoxFacet>]

[<AutoOpen>]
module Lighting3dConfigDispatcherExtensions =
    type Entity with
        member this.GetLightingConfig world : LightingConfig = this.Get (nameof this.LightingConfig) world
        member this.SetLightingConfig (value : LightingConfig) world = this.Set (nameof this.LightingConfig) value world
        member this.LightingConfig = lens (nameof this.LightingConfig) this this.GetLightingConfig this.SetLightingConfig

type Lighting3dConfigDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Properties =
        [define Entity.LightingConfig LightingConfig.defaultConfig
         define Entity.Presence Omnipresent
         define Entity.AlwaysUpdate true]

    override this.Update (entity, world) =
        let lightingConfig = entity.GetLightingConfig world
        World.enqueueRenderMessage3d (ConfigureLighting lightingConfig) world
        world

/// Gives an entity the base behavior of a 3d light probe.
type LightProbe3dDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<LightProbe3dFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero

/// Gives an entity the base behavior of a 3d light.
type Light3dDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<Light3dFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero

/// Gives an entity the base behavior of a static billboard.
type StaticBillboardDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<StaticBillboardFacet>]

/// Gives an entity the base behavior of a static model.
type StaticModelDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<StaticModelFacet>]

/// Gives an entity the base behavior of an animated model.
type AnimatedModelDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<AnimatedModelFacet>]

/// Gives an entity the base behavior of physics-driven rigid model.
type RigidModelDispatcher () =
    inherit Entity3dDispatcher (true)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        if (match bodyShape with StaticModelShape staticModelShape -> staticModelShape.StaticModel <> staticModel | _ -> false) then
            let staticModelShape = { StaticModel = staticModel; Convex = true; TransformOpt = None; PropertiesOpt = None }
            let world = entity.SetBodyShape (StaticModelShape staticModelShape) world
            (Cascade, world)
        else (Cascade, world)

    static let updateNavShape evt world =
        let entity = evt.Subscriber : Entity
        let world =
            match entity.GetBodyType world with
            | Static -> entity.SetNavShape BoundsNavShape world
            | Kinematic | KinematicCharacter | Dynamic | DynamicCharacter -> entity.SetNavShape NavShape.EmptyNavShape world
        (Cascade, world)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (StaticModelShape { StaticModel = Assets.Default.StaticModel; Convex = true; TransformOpt = None; PropertiesOpt = None })]

    override this.Register (entity, world) =
        let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
        let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
        let world = World.monitor updateNavShape (entity.GetChangeEvent (nameof entity.BodyType)) entity world
        world

/// Gives an entity the base behavior of an indexed static model.
type StaticModelSurfaceDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<StaticModelSurfaceFacet>]

/// Gives an entity the base behavior of an indexed, physics-driven rigid model.
type RigidModelSurfaceDispatcher () =
    inherit Entity3dDispatcher (true)

    static let updateBodyShape evt world =
        let entity = evt.Subscriber : Entity
        let bodyShape = entity.GetBodyShape world
        let staticModel = entity.GetStaticModel world
        let surfaceIndex = entity.GetSurfaceIndex world
        if (match bodyShape with StaticModelSurfaceShape staticModelSurfaceShape -> staticModelSurfaceShape.StaticModel <> staticModel || staticModelSurfaceShape.SurfaceIndex <> surfaceIndex | _ -> false) then
            let staticModelShape = { StaticModel = staticModel; SurfaceIndex = surfaceIndex; Convex = true; TransformOpt = None; PropertiesOpt = None }
            let world = entity.SetBodyShape (StaticModelSurfaceShape staticModelShape) world
            (Cascade, world)
        else (Cascade, world)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelSurfaceFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyShape (StaticModelSurfaceShape { StaticModel = Assets.Default.StaticModel; SurfaceIndex = 0; Convex = true; TransformOpt = None; PropertiesOpt = None })]

    override this.Register (entity, world) =
        let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
        let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.SurfaceIndex)) entity world
        let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
        world

/// Gives an entity the base behavior of basic static billboard emitter.
type BasicStaticBillboardEmitterDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<BasicStaticBillboardEmitterFacet>]

/// Gives an entity the base behavior of a 3d effect.
type Effect3dDispatcher () =
    inherit Entity3dDispatcher (false)

    static member Facets =
        [typeof<EffectFacet>]

    static member Properties =
        [define Entity.EffectDescriptor (scvalue<Effects.EffectDescriptor> "[[EffectName Effect] [LifeTimeOpt None] [Definitions []] [Content [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]]")]

/// Gives an entity the base behavior of a rigid 3d block using static physics.
type Block3dDispatcher () =
    inherit Entity3dDispatcher (true)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.Static true]

/// Gives an entity the base behavior of a rigid 3d box using dynamic physics.
type Box3dDispatcher () =
    inherit Entity3dDispatcher (true)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<StaticModelFacet>
         typeof<NavBodyFacet>]

    static member Properties =
        [define Entity.BodyType Dynamic]

[<AutoOpen>]
module Character3dDispatcherExtensions =
    type Entity with
        member this.GetLinearVelocityPrevious world : Vector3 = this.Get (nameof this.LinearVelocityPrevious) world
        member this.SetLinearVelocityPrevious (value : Vector3) world = this.Set (nameof this.LinearVelocityPrevious) value world
        member this.LinearVelocityPrevious = lens (nameof this.LinearVelocityPrevious) this this.GetLinearVelocityPrevious this.SetLinearVelocityPrevious
        member this.GetAngularVelocityPrevious world : Vector3 = this.Get (nameof this.AngularVelocityPrevious) world
        member this.SetAngularVelocityPrevious (value : Vector3) world = this.Set (nameof this.AngularVelocityPrevious) value world
        member this.AngularVelocityPrevious = lens (nameof this.AngularVelocityPrevious) this this.GetAngularVelocityPrevious this.SetAngularVelocityPrevious

/// Gives an entity the base behavior of a 3d character.
type Character3dDispatcher () =
    inherit Entity3dDispatcher (true)

    static member Facets =
        [typeof<AnimatedModelFacet>
         typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.BodyType KinematicCharacter
         define Entity.BodyShape (CapsuleShape { Height = 1.0f; Radius = 0.35f; TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f)); PropertiesOpt = None })
         define Entity.LinearVelocityPrevious v3Zero
         define Entity.AngularVelocityPrevious v3Zero]
         
    override this.Update (entity, world) =
        let rotation = entity.GetRotation world
        let linearVelocity = entity.GetLinearVelocity world
        let linearVelocityPrevious = entity.GetLinearVelocityPrevious world
        let linearVelocityAvg = (linearVelocity + linearVelocityPrevious) * 0.5f
        let angularVelocity = entity.GetAngularVelocity world
        let angularVelocityPrevious = entity.GetAngularVelocityPrevious world
        let angularVelocityAvg = (angularVelocity + angularVelocityPrevious) * 0.5f
        let forwardness = (Vector3.Dot (linearVelocityAvg * 32.0f, rotation.Forward))
        let backness = (Vector3.Dot (linearVelocityAvg * 32.0f, -rotation.Forward))
        let rightness = (Vector3.Dot (linearVelocityAvg * 32.0f, rotation.Right))
        let leftness = (Vector3.Dot (linearVelocityAvg * 32.0f, -rotation.Right))
        let turnRightness = if angularVelocity.Y < 0.0f then -angularVelocity.Y * 48.0f else 0.0f
        let turnLeftness = if angularVelocity.Y > 0.0f then angularVelocity.Y * 48.0f else 0.0f
        let animations =
            [Animation.make 0L None "Armature|Idle" Loop 1.0f 0.5f None]
        let animations =
            if forwardness >= 0.01f then Animation.make 0L None "Armature|WalkForward" Loop 1.0f (max 0.025f forwardness) None :: animations
            elif backness >= 0.01f then Animation.make 0L None "Armature|WalkBack" Loop 1.0f (max 0.025f backness) None :: animations
            else animations
        let animations =
            if rightness >= 0.01f then Animation.make 0L None "Armature|WalkRight" Loop 1.0f (max 0.025f rightness) None :: animations
            elif leftness >= 0.01f then Animation.make 0L None "Armature|WalkLeft" Loop 1.0f (max 0.025f leftness) None :: animations
            else animations
        let animations =
            if turnRightness >= 0.01f then Animation.make 0L None "Armature|TurnRight" Loop 1.0f (max 0.025f turnRightness) None :: animations
            elif turnLeftness >= 0.01f then Animation.make 0L None "Armature|TurnLeft" Loop 1.0f (max 0.025f turnLeftness) None :: animations
            else animations
        let world = entity.SetAnimations (List.toArray animations) world
        let world = entity.SetLinearVelocityPrevious linearVelocityAvg world
        let world = entity.SetAngularVelocityPrevious angularVelocityAvg world
        world

/// Gives an entity the base behavior of a physics-driven 3d joint.
type BodyJoint3dDispatcher () =
    inherit Entity3dDispatcher (true)

    static member Facets =
        [typeof<BodyJointFacet>]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.BodyJoint3dSizeDefault v3Zero

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        if intersectionOpt.HasValue then [|intersectionOpt.Value|]
        else [||]

/// Gives an entity the base behavior of a rigid 3d terrain.
type TerrainDispatcher () =
    inherit Entity3dDispatcher (true)

    static member Facets =
        [typeof<TerrainFacet>]

[<AutoOpen>]
module Nav3dConfigDispatcherExtensions =
    type Entity with
        member this.GetNav3dConfig world : Nav3dConfig = this.Get (nameof this.Nav3dConfig) world
        member this.SetNav3dConfig (value : Nav3dConfig) world = this.Set (nameof this.Nav3dConfig) value world
        member this.Nav3dConfig = lens (nameof this.Nav3dConfig) this this.GetNav3dConfig this.SetNav3dConfig

/// Augments an entity with a navigation mesh.
type Nav3dConfigDispatcher () =
    inherit Entity3dDispatcher (false)

    static let propagateNav3dConfig (entity : Entity) world =
        let config = entity.GetNav3dConfig world
        World.setNav3dConfig config entity.Screen world

    static member Properties =
        [define Entity.Nav3dConfig Nav3dConfig.defaultConfig]

    override this.Register (entity, world) =
        World.monitor (fun _ world -> (Cascade, propagateNav3dConfig entity world)) (entity.ChangeEvent (nameof entity.Nav3dConfig)) entity world

    override this.Edit (op, entity, world) =
        match op with
        | OverlayViewport _ ->
            let nav3d = World.getScreenNav3d entity.Screen world
            match nav3d.Nav3dMeshOpt with
            | Some (nbrData, _, _) ->
            
                // edge color compute lambda
                let computeEdgeColor (edge : struct (Vector3 * Vector3)) =
                    let middleY = (fst' edge).Y + (snd' edge).Y * 0.5f
                    let height = Math.Lerp (0.0f, 1.0f, (middleY - nbrData.NavEdgesMinY) / (nbrData.NavEdgesMaxY - nbrData.NavEdgesMinY))
                    Color (1.0f, 1.0f - height, height, 1.0f)

                // point color compute lambda
                let computePointColor (point : Vector3) =
                    let height = Math.Lerp (0.0f, 1.0f, (point.Y - nbrData.NavPointsMinY) / (nbrData.NavPointsMaxY - nbrData.NavPointsMinY))
                    Color (1.0f, 1.0f - height, height, 1.0f)

                // draw edges and points
                World.imGuiSegments3dPlus false nbrData.NavInteriorEdges 1.0f computeEdgeColor world
                World.imGuiSegments3dPlus false nbrData.NavExteriorEdges 1.0f computeEdgeColor world
                World.imGuiCircles3dPlus false nbrData.NavPoints 2.5f true computePointColor world
                world

            | None -> world
        | _ -> world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.unimportant