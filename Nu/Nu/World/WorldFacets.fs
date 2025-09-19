// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open ImGuiNET
open TiledSharp
open Prime
open Nu
open Nu.Particles

/// Declaratively exposes simulant lenses and events.
[<AutoOpen>]
module Declarative =

    /// The ubiquitous Game simulant.
    let Game = Game.Handle

    /// Declaratively exposes Screen lenses and events.
    let Screen = Unchecked.defaultof<Screen>

    /// Declaratively exposes Group lenses and events.
    let Group = Unchecked.defaultof<Group>

    /// Declaratively exposes Entity lenses and events.
    let Entity = Unchecked.defaultof<Entity>

[<AutoOpen>]
module StaticSpriteFacetExtensions =
    type Entity with
        member this.GetInsetOpt world : Box2 option = this.Get (nameof this.InsetOpt) world
        member this.SetInsetOpt (value : Box2 option) world = this.Set (nameof this.InsetOpt) value world
        member this.InsetOpt = lens (nameof this.InsetOpt) this this.GetInsetOpt this.SetInsetOpt
        member this.GetClipOpt world : Box2 option = this.Get (nameof this.ClipOpt) world
        member this.SetClipOpt (value : Box2 option) world = this.Set (nameof this.ClipOpt) value world
        member this.ClipOpt = lens (nameof this.ClipOpt) this this.GetClipOpt this.SetClipOpt
        member this.GetStaticImage world : Image AssetTag = this.Get (nameof this.StaticImage) world
        member this.SetStaticImage (value : Image AssetTag) world = this.Set (nameof this.StaticImage) value world
        member this.StaticImage = lens (nameof this.StaticImage) this this.GetStaticImage this.SetStaticImage
        member this.GetColor world : Color = this.Get (nameof this.Color) world
        member this.SetColor (value : Color) world = this.Set (nameof this.Color) value world
        member this.Color = lens (nameof this.Color) this this.GetColor this.SetColor
        member this.GetBlend world : Blend = this.Get (nameof this.Blend) world
        member this.SetBlend (value : Blend) world = this.Set (nameof this.Blend) value world
        member this.Blend = lens (nameof this.Blend) this this.GetBlend this.SetBlend
        member this.GetEmission world : Color = this.Get (nameof this.Emission) world
        member this.SetEmission (value : Color) world = this.Set (nameof this.Emission) value world
        member this.Emission = lens (nameof this.Emission) this this.GetEmission this.SetEmission
        member this.GetFlip world : Flip = this.Get (nameof this.Flip) world
        member this.SetFlip (value : Flip) world = this.Set (nameof this.Flip) value world
        member this.Flip = lens (nameof this.Flip) this this.GetFlip this.SetFlip

/// Augments an entity with a static sprite.
type StaticSpriteFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InsetOpt None
         define Entity.ClipOpt None
         define Entity.StaticImage Assets.Default.StaticSprite
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let staticImage = entity.GetStaticImage world
        let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = entity.GetClipOpt world |> Option.toValueOption
        let color = entity.GetColor world
        let blend = entity.GetBlend world
        let emission = entity.GetEmission world
        let flip = entity.GetFlip world
        World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.Entity2dSizeDefault v3Zero

[<AutoOpen>]
module AnimatedSpriteFacetExtensions =
    type Entity with
        member this.GetStartTime world : GameTime = this.Get (nameof this.StartTime) world
        member this.SetStartTime (value : GameTime) world = this.Set (nameof this.StartTime) value world
        member this.StartTime = lens (nameof this.StartTime) this this.GetStartTime this.SetStartTime
        member this.GetCelSize world : Vector2 = this.Get (nameof this.CelSize) world
        member this.SetCelSize (value : Vector2) world = this.Set (nameof this.CelSize) value world
        member this.CelSize = lens (nameof this.CelSize) this this.GetCelSize this.SetCelSize
        member this.GetCelCount world : int = this.Get (nameof this.CelCount) world
        member this.SetCelCount (value : int) world = this.Set (nameof this.CelCount) value world
        member this.CelCount = lens (nameof this.CelCount) this this.GetCelCount this.SetCelCount
        member this.GetCelRun world : int = this.Get (nameof this.CelRun) world
        member this.SetCelRun (value : int) world = this.Set (nameof this.CelRun) value world
        member this.CelRun = lens (nameof this.CelRun) this this.GetCelRun this.SetCelRun
        member this.GetAnimationStride world : int = this.Get (nameof this.AnimationStride) world
        member this.SetAnimationStride (value : int) world = this.Set (nameof this.AnimationStride) value world
        member this.AnimationStride = lens (nameof this.AnimationStride) this this.GetAnimationStride this.SetAnimationStride
        member this.GetAnimationDelay world : GameTime = this.Get (nameof this.AnimationDelay) world
        member this.SetAnimationDelay (value : GameTime) world = this.Set (nameof this.AnimationDelay) value world
        member this.AnimationDelay = lens (nameof this.AnimationDelay) this this.GetAnimationDelay this.SetAnimationDelay
        member this.GetAnimationSheet world : Image AssetTag = this.Get (nameof this.AnimationSheet) world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.Set (nameof this.AnimationSheet) value world
        member this.AnimationSheet = lens (nameof this.AnimationSheet) this this.GetAnimationSheet this.SetAnimationSheet

/// Augments an entity with an animated sprite.
type AnimatedSpriteFacet () =
    inherit Facet (false, false, false)

    static let getSpriteInsetOpt (entity : Entity) world =
        let startTime = entity.GetStartTime world
        let celCount = entity.GetCelCount world
        let celRun = entity.GetCelRun world
        if celCount <> 0 && celRun <> 0 then
            let localTime = world.GameTime - startTime
            let cel = int (localTime / entity.GetAnimationDelay world) % celCount * entity.GetAnimationStride world
            let celSize = entity.GetCelSize world
            let celI = cel % celRun
            let celJ = cel / celRun
            let celX = single celI * celSize.X
            let celY = single celJ * celSize.Y
            let inset = box2 (v2 celX celY) celSize
            Some inset
        else None

    static member Properties =
        [define Entity.StartTime GameTime.zero
         define Entity.CelSize (Vector2 (32.0f, 32.0f))
         define Entity.CelCount 16
         define Entity.CelRun 4
         define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
         define Entity.AnimationStride 1
         define Entity.AnimationSheet Assets.Default.AnimatedSprite
         define Entity.ClipOpt None
         define Entity.Color Color.One
         define Entity.Blend Transparent
         define Entity.Emission Color.Zero
         define Entity.Flip FlipNone]

    override this.Update (entity, world) =
        if not (entity.GetEnabled world) then
            entity.StartTime.Map ((+) world.GameDelta) world

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let animationSheet = entity.GetAnimationSheet world
        let insetOpt = match getSpriteInsetOpt entity world with Some inset -> ValueSome inset | None -> ValueNone
        let clipOpt = entity.GetClipOpt world |> Option.toValueOption
        let color = entity.GetColor world
        let blend = entity.GetBlend world
        let emission = entity.GetEmission world
        let flip = entity.GetFlip world
        World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, animationSheet, &transform, &insetOpt, &clipOpt, animationSheet, &color, blend, &emission, flip, world)

    override this.GetAttributesInferred (entity, world) =
        AttributesInferred.important (entity.GetCelSize world).V3 v3Zero

[<AutoOpen>]
module BasicStaticSpriteEmitterFacetExtensions =
    type Entity with
        member this.GetSelfDestruct world : bool = this.Get (nameof this.SelfDestruct) world
        member this.SetSelfDestruct (value : bool) world = this.Set (nameof this.SelfDestruct) value world
        member this.SelfDestruct = lens (nameof this.SelfDestruct) this this.GetSelfDestruct this.SetSelfDestruct
        member this.GetEmitterGravity world : Vector3 = this.Get (nameof this.EmitterGravity) world
        member this.SetEmitterGravity (value : Vector3) world = this.Set (nameof this.EmitterGravity) value world
        member this.EmitterGravity = lens (nameof this.EmitterGravity) this this.GetEmitterGravity this.SetEmitterGravity
        member this.GetEmitterImage world : Image AssetTag = this.Get (nameof this.EmitterImage) world
        member this.SetEmitterImage (value : Image AssetTag) world = this.Set (nameof this.EmitterImage) value world
        member this.EmitterImage = lens (nameof this.EmitterImage) this this.GetEmitterImage this.SetEmitterImage
        member this.GetEmitterBlend world : Blend = this.Get (nameof this.EmitterBlend) world
        member this.SetEmitterBlend (value : Blend) world = this.Set (nameof this.EmitterBlend) value world
        member this.EmitterBlend = lens (nameof this.EmitterBlend) this this.GetEmitterBlend this.SetEmitterBlend
        member this.GetEmitterClipOpt world : Box2 option = this.Get (nameof this.EmitterClipOpt) world
        member this.SetEmitterClipOpt (value : Box2 option) world = this.Set (nameof this.EmitterClipOpt) value world
        member this.EmitterClipOpt = lens (nameof this.EmitterClipOpt) this this.GetEmitterClipOpt this.SetEmitterClipOpt
        member this.GetEmitterLifeTimeOpt world : GameTime = this.Get (nameof this.EmitterLifeTimeOpt) world
        member this.SetEmitterLifeTimeOpt (value : GameTime) world = this.Set (nameof this.EmitterLifeTimeOpt) value world
        member this.EmitterLifeTimeOpt = lens (nameof this.EmitterLifeTimeOpt) this this.GetEmitterLifeTimeOpt this.SetEmitterLifeTimeOpt
        member this.GetParticleLifeTimeMaxOpt world : GameTime = this.Get (nameof this.ParticleLifeTimeMaxOpt) world
        member this.SetParticleLifeTimeMaxOpt (value : GameTime) world = this.Set (nameof this.ParticleLifeTimeMaxOpt) value world
        member this.ParticleLifeTimeMaxOpt = lens (nameof this.ParticleLifeTimeMaxOpt) this this.GetParticleLifeTimeMaxOpt this.SetParticleLifeTimeMaxOpt
        member this.GetParticleRate world : single = this.Get (nameof this.ParticleRate) world
        member this.SetParticleRate (value : single) world = this.Set (nameof this.ParticleRate) value world
        member this.ParticleRate = lens (nameof this.ParticleRate) this this.GetParticleRate this.SetParticleRate
        member this.GetParticleMax world : int = this.Get (nameof this.ParticleMax) world
        member this.SetParticleMax (value : int) world = this.Set (nameof this.ParticleMax) value world
        member this.ParticleMax = lens (nameof this.ParticleMax) this this.GetParticleMax this.SetParticleMax
        member this.GetBasicParticleSeed world : Particles.BasicParticle = this.Get (nameof this.BasicParticleSeed) world
        member this.SetBasicParticleSeed (value : Particles.BasicParticle) world = this.Set (nameof this.BasicParticleSeed) value world
        member this.BasicParticleSeed = lens (nameof this.BasicParticleSeed) this this.GetBasicParticleSeed this.SetBasicParticleSeed
        member this.GetEmitterConstraint world : Particles.Constraint = this.Get (nameof this.EmitterConstraint) world
        member this.SetEmitterConstraint (value : Particles.Constraint) world = this.Set (nameof this.EmitterConstraint) value world
        member this.EmitterConstraint = lens (nameof this.EmitterConstraint) this this.GetEmitterConstraint this.SetEmitterConstraint
        member this.GetEmitterStyle world : string = this.Get (nameof this.EmitterStyle) world
        member this.SetEmitterStyle (value : string) world = this.Set (nameof this.EmitterStyle) value world
        member this.EmitterStyle = lens (nameof this.EmitterStyle) this this.GetEmitterStyle this.SetEmitterStyle
        member this.GetParticleSystem world : Particles.ParticleSystem = this.Get (nameof this.ParticleSystem) world
        member this.SetParticleSystem (value : Particles.ParticleSystem) world = this.Set (nameof this.ParticleSystem) value world
        member this.ParticleSystem = lens (nameof this.ParticleSystem) this this.GetParticleSystem this.SetParticleSystem

/// Augments an entity with a basic static sprite emitter.
type BasicStaticSpriteEmitterFacet () =
    inherit Facet (false, false, false)

    static let tryMakeEmitter (entity : Entity) (world : World) =
        World.tryMakeEmitter
            world.GameTime
            (entity.GetEmitterLifeTimeOpt world)
            (entity.GetParticleLifeTimeMaxOpt world)
            (entity.GetParticleRate world)
            (entity.GetParticleMax world)
            (entity.GetEmitterStyle world)
            world
        |> Option.map cast<Particles.BasicStaticSpriteEmitter>

    static let makeEmitter entity world =
        match tryMakeEmitter entity world with
        | Some emitter ->
            let mutable transform = entity.GetTransform world
            { emitter with
                Body =
                    { Position = transform.Position
                      Scale = transform.Scale
                      Angles = transform.Angles
                      LinearVelocity = v3Zero
                      AngularVelocity = v3Zero
                      Restitution = Constants.Particles.RestitutionDefault }
                Elevation = transform.Elevation
                Absolute = transform.Absolute
                ClipOpt = entity.GetEmitterClipOpt world
                Blend = entity.GetEmitterBlend world
                Image = entity.GetEmitterImage world
                ParticleSeed = entity.GetBasicParticleSeed world
                Constraint = entity.GetEmitterConstraint world }
        | None ->
            Particles.BasicStaticSpriteEmitter.makeEmpty
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)

    static let mapParticleSystem mapper (entity : Entity) world =
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = mapper particleSystem
        entity.SetParticleSystem particleSystem world

    static let mapEmitter mapper (entity : Entity) world =
        mapParticleSystem (fun particleSystem ->
            match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                let emitter = mapper emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem)
            entity world

    static let rec processOutput output entity world =
        match output with
        | Particles.OutputEmitter (name, emitter) -> mapParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
        | Particles.Outputs outputs -> for output in outputs do processOutput output entity world

    static let handleEmitterBlendChange evt world =
        let emitterBlend = evt.Data.Value :?> Blend
        mapEmitter (fun emitter -> if emitter.Blend <> emitterBlend then { emitter with Blend = emitterBlend } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterClipOptChange evt world =
        let emitterClipOpt = evt.Data.Value :?> Box2 option
        mapEmitter (fun emitter -> if emitter.ClipOpt <> emitterClipOpt then { emitter with ClipOpt = emitterClipOpt } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterImageChange evt world =
        let emitterImage = evt.Data.Value :?> Image AssetTag
        mapEmitter (fun emitter -> if assetNeq emitter.Image emitterImage then { emitter with Image = emitterImage } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterLifeTimeOptChange evt world =
        let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
        mapEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
        Cascade

    static let handleParticleLifeTimeMaxOptChange evt world =
        let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
        mapEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
        Cascade

    static let handleParticleRateChange evt world =
        let particleRate = evt.Data.Value :?> single
        mapEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
        Cascade

    static let handleParticleMaxChange evt world =
        let particleMax = evt.Data.Value :?> int
        mapEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticSpriteEmitter.resize particleMax emitter else emitter) evt.Subscriber world
        Cascade

    static let handleBasicParticleSeedChange evt world =
        let particleSeed = evt.Data.Value :?> Particles.BasicParticle
        mapEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterConstraintChange evt world =
        let emitterConstraint = evt.Data.Value :?> Particles.Constraint
        mapEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterStyleChange evt world =
        let entity = evt.Subscriber : Entity
        let emitter = makeEmitter entity world
        mapEmitter (constant emitter) entity world
        Cascade

    static let handlePositionChange evt world =
        let entity = evt.Subscriber : Entity
        let particleSystem = entity.GetParticleSystem world
        let particleSystem =
            match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                let position = entity.GetPosition world
                let emitter =
                    if v3Neq emitter.Body.Position position
                    then { emitter with Body = { emitter.Body with Position = position }}
                    else emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem
        entity.SetParticleSystem particleSystem world
        Cascade

    static let handleRotationChange evt world =
        let entity = evt.Subscriber : Entity
        let particleSystem = entity.GetParticleSystem world
        let particleSystem =
            match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                let angles = entity.GetAngles world
                let emitter =
                    if v3Neq emitter.Body.Angles angles
                    then { emitter with Body = { emitter.Body with Angles = angles }}
                    else emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem
        entity.SetParticleSystem particleSystem world
        Cascade

    static member Properties =
        [define Entity.SelfDestruct false
         define Entity.EmitterBlend Transparent
         define Entity.EmitterClipOpt None
         define Entity.EmitterImage Assets.Default.Image
         define Entity.EmitterLifeTimeOpt GameTime.zero
         define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
         define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
         define Entity.ParticleMax 60
         define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = Constants.Engine.Particle2dSizeDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
         define Entity.EmitterConstraint Particles.Constraint.empty
         define Entity.EmitterStyle "BasicStaticSpriteEmitter"
         nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

    override this.Register (entity, world) =
        let emitter = makeEmitter entity world
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
        entity.SetParticleSystem particleSystem world
        World.sense handlePositionChange entity.Position.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleRotationChange entity.Rotation.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterBlendChange entity.EmitterBlend.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterClipOptChange entity.EmitterClipOpt.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterImageChange entity.EmitterImage.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterLifeTimeOptChange entity.EmitterLifeTimeOpt.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleParticleLifeTimeMaxOptChange entity.ParticleLifeTimeMaxOpt.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleParticleRateChange entity.ParticleRate.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleParticleMaxChange entity.ParticleMax.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleBasicParticleSeedChange entity.BasicParticleSeed.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterConstraintChange entity.EmitterConstraint.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world
        World.sense handleEmitterStyleChange entity.EmitterStyle.ChangeEvent entity (nameof BasicStaticSpriteEmitterFacet) world

    override this.Unregister (entity, world) =
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = { particleSystem with Emitters = Map.remove typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters }
        entity.SetParticleSystem particleSystem world

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let delta = world.GameDelta
            let time = world.GameTime
            let particleSystem = entity.GetParticleSystem world
            let (particleSystem, output) = Particles.ParticleSystem.run delta time particleSystem
            entity.SetParticleSystem particleSystem world
            processOutput output entity world

    override this.Render (_, entity, world) =
        let time = world.GameTime
        let particleSystem = entity.GetParticleSystem world
        let particlesMessages =
            particleSystem
            |> Particles.ParticleSystem.toParticlesDescriptors time
            |> List.map (fun descriptor ->
                match descriptor with
                | Particles.SpriteParticlesDescriptor descriptor ->
                    Some
                        { Elevation = descriptor.Elevation
                          Horizon = descriptor.Horizon
                          AssetTag = descriptor.Image
                          RenderOperation2d = RenderSpriteParticles descriptor }
                | _ -> None)
            |> List.definitize
        World.enqueueLayeredOperations2d particlesMessages world

[<AutoOpen>]
module TextFacetExtensions =
    type Entity with
        member this.GetText world : string = this.Get (nameof this.Text) world
        member this.SetText (value : string) world = this.Set (nameof this.Text) value world
        member this.Text = lens (nameof this.Text) this this.GetText this.SetText
        member this.GetFont world : Font AssetTag = this.Get (nameof this.Font) world
        member this.SetFont (value : Font AssetTag) world = this.Set (nameof this.Font) value world
        member this.Font = lens (nameof this.Font) this this.GetFont this.SetFont
        member this.GetFontSizing world : int option = this.Get (nameof this.FontSizing) world
        member this.SetFontSizing (value : int option) world = this.Set (nameof this.FontSizing) value world
        member this.FontSizing = lens (nameof this.FontSizing) this this.GetFontSizing this.SetFontSizing
        member this.GetFontStyling world : FontStyle Set = this.Get (nameof this.FontStyling) world
        member this.SetFontStyling (value : FontStyle Set) world = this.Set (nameof this.FontStyling) value world
        member this.FontStyling = lens (nameof this.FontStyling) this this.GetFontStyling this.SetFontStyling
        member this.GetJustification world : Justification = this.Get (nameof this.Justification) world
        member this.SetJustification (value : Justification) world = this.Set (nameof this.Justification) value world
        member this.Justification = lens (nameof this.Justification) this this.GetJustification this.SetJustification
        member this.GetTextMargin world : Vector2 = this.Get (nameof this.TextMargin) world
        member this.SetTextMargin (value : Vector2) world = this.Set (nameof this.TextMargin) value world
        member this.TextMargin = lens (nameof this.TextMargin) this this.GetTextMargin this.SetTextMargin
        member this.GetTextColor world : Color = this.Get (nameof this.TextColor) world
        member this.SetTextColor (value : Color) world = this.Set (nameof this.TextColor) value world
        member this.TextColor = lens (nameof this.TextColor) this this.GetTextColor this.SetTextColor
        member this.GetTextColorDisabled world : Color = this.Get (nameof this.TextColorDisabled) world
        member this.SetTextColorDisabled (value : Color) world = this.Set (nameof this.TextColorDisabled) value world
        member this.TextColorDisabled = lens (nameof this.TextColorDisabled) this this.GetTextColorDisabled this.SetTextColorDisabled
        member this.GetTextOffset world : Vector2 = this.Get (nameof this.TextOffset) world
        member this.SetTextOffset (value : Vector2) world = this.Set (nameof this.TextOffset) value world
        member this.TextOffset = lens (nameof this.TextOffset) this this.GetTextOffset this.SetTextOffset
        member this.GetTextShift world : single = this.Get (nameof this.TextShift) world
        member this.SetTextShift (value : single) world = this.Set (nameof this.TextShift) value world
        member this.TextShift = lens (nameof this.TextShift) this this.GetTextShift this.SetTextShift

/// Augments an entity with text.
type TextFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.Text ""
         define Entity.Font Assets.Default.Font
         define Entity.FontSizing None
         define Entity.FontStyling Set.empty
         define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
         define Entity.TextMargin v2Zero
         define Entity.TextColor Color.White
         define Entity.TextColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.TextOffset v2Zero
         define Entity.TextShift Constants.Gui.TextShiftDefault]

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let absolute = transform.Absolute
        let perimeter = transform.Perimeter
        let offset = (entity.GetTextOffset world).V3
        let elevation = transform.Elevation
        let shift = entity.GetTextShift world
        let clipOpt = ValueSome transform.Bounds2d.Box2
        let justification = entity.GetJustification world
        let margin = (entity.GetTextMargin world).V3
        let color = if transform.Enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
        let font = entity.GetFont world
        let fontSizing = entity.GetFontSizing world
        let fontStyling = entity.GetFontStyling world
        let text = entity.GetText world
        World.renderGuiText absolute perimeter offset elevation shift clipOpt justification None margin color font fontSizing fontStyling text world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module BackdroppableFacetExtensions =
    type Entity with
        member this.GetColorDisabled world : Color = this.Get (nameof this.ColorDisabled) world
        member this.SetColorDisabled (value : Color) world = this.Set (nameof this.ColorDisabled) value world
        member this.ColorDisabled = lens (nameof this.ColorDisabled) this this.GetColorDisabled this.SetColorDisabled
        member this.GetSliceMargin world : Vector2 = this.Get (nameof this.SliceMargin) world
        member this.SetSliceMargin (value : Vector2) world = this.Set (nameof this.SliceMargin) value world
        member this.SliceMargin = lens (nameof this.SliceMargin) this this.GetSliceMargin this.SetSliceMargin
        member this.GetBackdropImageOpt world : Image AssetTag option = this.Get (nameof this.BackdropImageOpt) world
        member this.SetBackdropImageOpt (value : Image AssetTag option) world = this.Set (nameof this.BackdropImageOpt) value world
        member this.BackdropImageOpt = lens (nameof this.BackdropImageOpt) this this.GetBackdropImageOpt this.SetBackdropImageOpt

/// Augments an entity with optional backdrop behavior.
type BackdroppableFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.Color Color.One
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.BackdropImageOpt None]

    override this.Render (_, entity, world) =
        match entity.GetBackdropImageOpt world with
        | Some spriteImage ->
            let mutable transform = entity.GetTransform world
            let sliceMargin = entity.GetSliceMargin world
            let color = if transform.Enabled then entity.GetColor world else entity.GetColorDisabled world
            World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin spriteImage transform.Offset transform.Elevation color world
        | None -> ()

    override this.GetAttributesInferred (entity, world) =
        match entity.GetBackdropImageOpt world with
        | Some backdropImage ->
            match Metadata.tryGetTextureSizeF backdropImage with
            | ValueSome size -> AttributesInferred.important size.V3 v3Zero
            | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero
        | None -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module ButtonFacetExtensions =
    type Entity with
        member this.GetDown world : bool = this.Get (nameof this.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof this.Down) value world
        member this.Down = lens (nameof this.Down) this this.GetDown this.SetDown
        member this.GetDownOffset world : Vector2 = this.Get (nameof this.DownOffset) world
        member this.SetDownOffset (value : Vector2) world = this.Set (nameof this.DownOffset) value world
        member this.DownOffset = lens (nameof this.DownOffset) this this.GetDownOffset this.SetDownOffset
        member this.GetUpImage world : Image AssetTag = this.Get (nameof this.UpImage) world
        member this.SetUpImage (value : Image AssetTag) world = this.Set (nameof this.UpImage) value world
        member this.UpImage = lens (nameof this.UpImage) this this.GetUpImage this.SetUpImage
        member this.GetDownImage world : Image AssetTag = this.Get (nameof this.DownImage) world
        member this.SetDownImage (value : Image AssetTag) world = this.Set (nameof this.DownImage) value world
        member this.DownImage = lens (nameof this.DownImage) this this.GetDownImage this.SetDownImage
        member this.GetClickSoundOpt world : Sound AssetTag option = this.Get (nameof this.ClickSoundOpt) world
        member this.SetClickSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ClickSoundOpt) value world
        member this.ClickSoundOpt = lens (nameof this.ClickSoundOpt) this this.GetClickSoundOpt this.SetClickSoundOpt
        member this.GetClickSoundVolume world : single = this.Get (nameof this.ClickSoundVolume) world
        member this.SetClickSoundVolume (value : single) world = this.Set (nameof this.ClickSoundVolume) value world
        member this.ClickSoundVolume = lens (nameof this.ClickSoundVolume) this this.GetClickSoundVolume this.SetClickSoundVolume
        member this.UpEvent = Events.UpEvent --> this
        member this.DownEvent = Events.DownEvent --> this
        member this.ClickEvent = Events.ClickEvent --> this

/// Augments an entity with button behavior.
type ButtonFacet () =
    inherit Facet (false, false, false)

    static let handleMouseLeftDown evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled then
                    entity.SetDown true world
                    entity.TrySet (nameof Entity.TextOffset) (entity.GetDownOffset world) world |> ignore
                    let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftDown" "" EventTrace.empty
                    World.publishPlus () entity.DownEvent eventTrace entity true false world
                    Resolve
                else Resolve
            else Cascade
        else Cascade

    static let handleMouseLeftUp evt world =
        let entity = evt.Subscriber : Entity
        let wasDown = entity.GetDown world
        entity.SetDown false world
        entity.TrySet (nameof Entity.TextOffset) v2Zero world |> ignore
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled && wasDown then
                    let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Up" EventTrace.empty
                    World.publishPlus () entity.UpEvent eventTrace entity true false world
                    let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Click" EventTrace.empty
                    World.publishPlus () entity.ClickEvent eventTrace entity true false world
                    match entity.GetClickSoundOpt world with
                    | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                    | None -> ()
                    Resolve
                else Cascade
            else Cascade
        else Cascade

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Down false
         define Entity.DownOffset v2Zero
         define Entity.UpImage Assets.Default.ButtonUp
         define Entity.DownImage Assets.Default.ButtonDown
         define Entity.ClickSoundOpt (Some Assets.Default.Sound)
         define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

    override this.Register (entity, world) =
        World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ButtonFacet) world
        World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ButtonFacet) world

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let sliceMargin = entity.GetSliceMargin world
        let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
        let color = if transform.Enabled then Color.One else entity.GetColorDisabled world
        World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin spriteImage transform.Offset transform.Elevation color world

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module ToggleButtonFacetExtensions =
    type Entity with
        member this.GetToggled world : bool = this.Get (nameof this.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof this.Toggled) value world
        member this.Toggled = lens (nameof this.Toggled) this this.GetToggled this.SetToggled
        member this.GetToggledOffset world : Vector2 = this.Get (nameof this.ToggledOffset) world
        member this.SetToggledOffset (value : Vector2) world = this.Set (nameof this.ToggledOffset) value world
        member this.ToggledOffset = lens (nameof this.ToggledOffset) this this.GetToggledOffset this.SetToggledOffset
        member this.GetPushed world : bool = this.Get (nameof this.Pushed) world
        member this.SetPushed (value : bool) world = this.Set (nameof this.Pushed) value world
        member this.Pushed = lens (nameof this.Pushed) this this.GetPushed this.SetPushed
        member this.GetPushedOffset world : Vector2 = this.Get (nameof this.PushedOffset) world
        member this.SetPushedOffset (value : Vector2) world = this.Set (nameof this.PushedOffset) value world
        member this.PushedOffset = lens (nameof this.PushedOffset) this this.GetPushedOffset this.SetPushedOffset
        member this.GetUntoggledImage world : Image AssetTag = this.Get (nameof this.UntoggledImage) world
        member this.SetUntoggledImage (value : Image AssetTag) world = this.Set (nameof this.UntoggledImage) value world
        member this.UntoggledImage = lens (nameof this.UntoggledImage) this this.GetUntoggledImage this.SetUntoggledImage
        member this.GetToggledImage world : Image AssetTag = this.Get (nameof this.ToggledImage) world
        member this.SetToggledImage (value : Image AssetTag) world = this.Set (nameof this.ToggledImage) value world
        member this.ToggledImage = lens (nameof this.ToggledImage) this this.GetToggledImage this.SetToggledImage
        member this.GetToggleSoundOpt world : Sound AssetTag option = this.Get (nameof this.ToggleSoundOpt) world
        member this.SetToggleSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ToggleSoundOpt) value world
        member this.ToggleSoundOpt = lens (nameof this.ToggleSoundOpt) this this.GetToggleSoundOpt this.SetToggleSoundOpt
        member this.GetToggleSoundVolume world : single = this.Get (nameof this.ToggleSoundVolume) world
        member this.SetToggleSoundVolume (value : single) world = this.Set (nameof this.ToggleSoundVolume) value world
        member this.ToggleSoundVolume = lens (nameof this.ToggleSoundVolume) this this.GetToggleSoundVolume this.SetToggleSoundVolume
        member this.ToggleEvent = Events.ToggleEvent --> this
        member this.ToggledEvent = Events.ToggledEvent --> this
        member this.UntoggledEvent = Events.UntoggledEvent --> this

/// Augments an entity with toggle button behavior.
type ToggleButtonFacet () =
    inherit Facet (false, false, false)
    
    static let handleMouseLeftDown evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled then
                    entity.SetPushed true world
                    Resolve
                else Resolve
            else Cascade
        else Cascade

    static let handleMouseLeftUp evt world =
        let entity = evt.Subscriber : Entity
        let wasPushed = entity.GetPushed world
        if wasPushed then entity.SetPushed false world
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled && wasPushed then
                    entity.SetToggled (not (entity.GetToggled world)) world
                    let toggled = entity.GetToggled world
                    let eventAddress = if toggled then entity.ToggledEvent else entity.UntoggledEvent
                    let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "" EventTrace.empty
                    World.publishPlus () eventAddress eventTrace entity true false world
                    let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "Toggle" EventTrace.empty
                    World.publishPlus toggled entity.ToggleEvent eventTrace entity true false world
                    match entity.GetToggleSoundOpt world with
                    | Some toggleSound -> World.playSound (entity.GetToggleSoundVolume world) toggleSound world
                    | None -> ()
                    Resolve
                else Cascade
            else Cascade
        else Cascade

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Toggled false
         define Entity.ToggledOffset v2Zero
         define Entity.Pushed false
         define Entity.PushedOffset v2Zero
         define Entity.UntoggledImage Assets.Default.ButtonUp
         define Entity.ToggledImage Assets.Default.ButtonDown
         define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
         define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

    override this.Register (entity, world) =
        World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ToggleButtonFacet) world
        World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ToggleButtonFacet) world

    override this.Update (entity, world) =
        let textOffset =
            if entity.GetPushed world then entity.GetPushedOffset world
            elif entity.GetToggled world then entity.GetToggledOffset world
            else v2Zero
        entity.TrySet (nameof Entity.TextOffset) textOffset world |> ignore

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let sliceMargin = entity.GetSliceMargin world
        let spriteImage =
            if entity.GetToggled world || entity.GetPushed world
            then entity.GetToggledImage world
            else entity.GetUntoggledImage world
        let color = if transform.Enabled then Color.One else entity.GetColorDisabled world
        World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin spriteImage transform.Offset transform.Elevation color world

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetUntoggledImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module RadioButtonFacetExtensions =
    type Entity with
        member this.GetDialed world : bool = this.Get (nameof this.Dialed) world
        member this.SetDialed (value : bool) world = this.Set (nameof this.Dialed) value world
        member this.Dialed = lens (nameof this.Dialed) this this.GetDialed this.SetDialed
        member this.GetDialedOffset world : Vector2 = this.Get (nameof this.DialedOffset) world
        member this.SetDialedOffset (value : Vector2) world = this.Set (nameof this.DialedOffset) value world
        member this.DialedOffset = lens (nameof this.DialedOffset) this this.GetDialedOffset this.SetDialedOffset
        member this.GetUndialedImage world : Image AssetTag = this.Get (nameof this.UndialedImage) world
        member this.SetUndialedImage (value : Image AssetTag) world = this.Set (nameof this.UndialedImage) value world
        member this.UndialedImage = lens (nameof this.UndialedImage) this this.GetUndialedImage this.SetUndialedImage
        member this.GetDialedImage world : Image AssetTag = this.Get (nameof this.DialedImage) world
        member this.SetDialedImage (value : Image AssetTag) world = this.Set (nameof this.DialedImage) value world
        member this.DialedImage = lens (nameof this.DialedImage) this this.GetDialedImage this.SetDialedImage
        member this.GetDialSoundOpt world : Sound AssetTag option = this.Get (nameof this.DialSoundOpt) world
        member this.SetDialSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.DialSoundOpt) value world
        member this.DialSoundOpt = lens (nameof this.DialSoundOpt) this this.GetDialSoundOpt this.SetDialSoundOpt
        member this.GetDialSoundVolume world : single = this.Get (nameof this.DialSoundVolume) world
        member this.SetDialSoundVolume (value : single) world = this.Set (nameof this.DialSoundVolume) value world
        member this.DialSoundVolume = lens (nameof this.DialSoundVolume) this this.GetDialSoundVolume this.SetDialSoundVolume
        member this.DialEvent = Events.DialEvent --> this
        member this.DialedEvent = Events.DialedEvent --> this
        member this.UndialedEvent = Events.UndialedEvent --> this

/// Augments an entity with radio button behavior.
type RadioButtonFacet () =
    inherit Facet (false, false, false)

    static let handleMouseLeftDown evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled then
                    entity.SetPushed true world
                    Resolve
                else Resolve
            else Cascade
        else Cascade

    static let handleMouseLeftUp evt world =
        let entity = evt.Subscriber : Entity
        let wasPushed = entity.GetPushed world
        if wasPushed then entity.SetPushed false world
        let wasDialed = entity.GetDialed world
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled && wasPushed && not wasDialed then
                    entity.SetDialed true world
                    let dialed = entity.GetDialed world
                    let eventAddress = if dialed then entity.DialedEvent else entity.UndialedEvent
                    let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "" EventTrace.empty
                    World.publishPlus () eventAddress eventTrace entity true false world
                    let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "Dial" EventTrace.empty
                    World.publishPlus dialed entity.DialEvent eventTrace entity true false world
                    match entity.GetDialSoundOpt world with
                    | Some dialSound -> World.playSound (entity.GetDialSoundVolume world) dialSound world
                    | None -> ()
                    Resolve
                else Cascade
            else Cascade
        else Cascade

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Dialed false
         define Entity.DialedOffset v2Zero
         define Entity.Pushed false
         define Entity.PushedOffset v2Zero
         define Entity.UndialedImage Assets.Default.ButtonUp
         define Entity.DialedImage Assets.Default.ButtonDown
         define Entity.DialSoundOpt (Some Assets.Default.Sound)
         define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

    override this.Register (entity, world) =
        World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof RadioButtonFacet) world
        World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof RadioButtonFacet) world

    override this.Update (entity, world) =
        let textOffset =
            if entity.GetPushed world then entity.GetPushedOffset world
            elif entity.GetDialed world then entity.GetDialedOffset world
            else v2Zero
        entity.TrySet (nameof Entity.TextOffset) textOffset world |> ignore

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let sliceMargin = entity.GetSliceMargin world
        let spriteImage =
            if entity.GetDialed world || entity.GetPushed world
            then entity.GetDialedImage world
            else entity.GetUndialedImage world
        let color = if transform.Enabled then Color.One else entity.GetColorDisabled world
        World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin spriteImage transform.Offset transform.Elevation color world

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module FillBarFacetExtensions =
    type Entity with
        member this.GetFill world : single = this.Get (nameof this.Fill) world
        member this.SetFill (value : single) world = this.Set (nameof this.Fill) value world
        member this.Fill = lens (nameof this.Fill) this this.GetFill this.SetFill
        member this.GetFillInset world : single = this.Get (nameof this.FillInset) world
        member this.SetFillInset (value : single) world = this.Set (nameof this.FillInset) value world
        member this.FillInset = lens (nameof this.FillInset) this this.GetFillInset this.SetFillInset
        member this.GetFillColor world : Color = this.Get (nameof this.FillColor) world
        member this.SetFillColor (value : Color) world = this.Set (nameof this.FillColor) value world
        member this.FillColor = lens (nameof this.FillColor) this this.GetFillColor this.SetFillColor
        member this.GetFillImage world : Image AssetTag = this.Get (nameof this.FillImage) world
        member this.SetFillImage (value : Image AssetTag) world = this.Set (nameof this.FillImage) value world
        member this.FillImage = lens (nameof this.FillImage) this this.GetFillImage this.SetFillImage
        member this.GetBorderColor world : Color = this.Get (nameof this.BorderColor) world
        member this.SetBorderColor (value : Color) world = this.Set (nameof this.BorderColor) value world
        member this.BorderColor = lens (nameof this.BorderColor) this this.GetBorderColor this.SetBorderColor
        member this.GetBorderImage world : Image AssetTag = this.Get (nameof this.BorderImage) world
        member this.SetBorderImage (value : Image AssetTag) world = this.Set (nameof this.BorderImage) value world
        member this.BorderImage = lens (nameof this.BorderImage) this this.GetBorderImage this.SetBorderImage

/// Augments an entity with fill bar behavior.
type FillBarFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.SliceMargin Constants.Gui.SliceMarginDefault
         define Entity.ColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.Fill 0.0f
         define Entity.FillInset 0.0f
         define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
         define Entity.FillImage Assets.Default.White
         define Entity.BorderColor (Color (1.0f, 1.0f, 1.0f, 1.0f))
         define Entity.BorderImage Assets.Default.Border]

    override this.Render (_, entity, world) =

        // border sprite
        let mutable transform = entity.GetTransform world
        let sliceMargin = entity.GetSliceMargin world
        let elevation = transform.Elevation + 0.5f
        let color = if transform.Enabled then Color.White else entity.GetColorDisabled world
        let borderImageColor = entity.GetBorderColor world * color
        let borderImage = entity.GetBorderImage world
        World.renderGuiSpriteSliced transform.Absolute transform.Perimeter sliceMargin borderImage transform.Offset elevation borderImageColor world

        // fill sprite
        let fillSize = transform.Perimeter.Size
        let fillInset = fillSize.X * entity.GetFillInset world * 0.5f
        let fillWidth = (fillSize.X - fillInset * 2.0f) * (entity.GetFill world |> min 1.0f |> max 0.0f)
        let fillPosition = transform.Perimeter.Left + v3 (fillWidth * 0.5f) 0.0f 0.0f + v3 fillInset 0.0f 0.0f
        let fillHeight = fillSize.Y - fillInset * 2.0f
        let fillSize = v3 fillWidth fillHeight 0.0f
        let fillPerimeter = box3 (fillPosition - fillSize * 0.5f) fillSize
        let fillImageColor = entity.GetFillColor world * color
        let fillImage = entity.GetFillImage world
        World.renderGuiSpriteSliced transform.Absolute fillPerimeter sliceMargin fillImage transform.Offset transform.Elevation fillImageColor world

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
        | ValueSome size -> AttributesInferred.important size.V3 v3Zero
        | ValueNone -> AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module FeelerFacetExtensions =
    type Entity with
        member this.GetTouched world : bool = this.Get (nameof this.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof this.Touched) value world
        member this.Touched = lens (nameof this.Touched) this this.GetTouched this.SetTouched
        member this.TouchEvent = Events.TouchEvent --> this
        member this.TouchingEvent = Events.TouchingEvent --> this
        member this.UntouchEvent = Events.UntouchEvent --> this

/// Augments an entity with feeler behavior, acting as little invisible pane that produces touch events in response
/// to mouse input.
type FeelerFacet () =
    inherit Facet (false, false, false)

    static let handleMouseLeftDown evt world =
        let entity = evt.Subscriber : Entity
        let data = evt.Data : MouseButtonData
        if entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled then
                    entity.SetTouched true world
                    let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                    World.publishPlus data.Position entity.TouchEvent eventTrace entity true false world
                    Resolve
                else Resolve
            else Cascade
        else Cascade

    static let handleMouseLeftUp evt world =
        let entity = evt.Subscriber : Entity
        let data = evt.Data : MouseButtonData
        let wasTouched = entity.GetTouched world
        entity.SetTouched false world
        if entity.GetVisible world then
            if entity.GetEnabled world && wasTouched then
                let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                World.publishPlus data.Position entity.UntouchEvent eventTrace entity true false world
                Resolve
            else Cascade
        else Cascade

    static let handleIncoming evt world =
        let entity = evt.Subscriber : Entity
        if  MouseState.isButtonDown MouseLeft &&
            entity.GetVisible world &&
            entity.GetEnabled world then
            let mousePosition = MouseState.getPosition ()
            entity.SetTouched true world
            let eventTrace = EventTrace.debug "FeelerFacet" "handleIncoming" "" EventTrace.empty
            World.publishPlus mousePosition entity.TouchEvent eventTrace entity true false world
            Resolve
        else Cascade

    static let handleOutgoing evt world =
        let entity = evt.Subscriber : Entity
        entity.SetTouched false world
        Cascade

    static member Properties =
        [define Entity.Touched false]

    override this.Register (entity, world) =
        World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof FeelerFacet) world
        World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof FeelerFacet) world
        World.sense handleIncoming entity.Screen.IncomingFinishEvent entity (nameof FeelerFacet) world
        World.sense handleOutgoing entity.Screen.OutgoingStartEvent entity (nameof FeelerFacet) world

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            if entity.GetTouched world then
                let mousePosition = World.getMousePosition world
                let eventTrace = EventTrace.debug "FeelerFacet" "Update" "" EventTrace.empty
                World.publishPlus mousePosition entity.TouchingEvent eventTrace entity true false world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module TextBoxFacetExtensions =
    type Entity with
        member this.GetTextCapacity world : int = this.Get (nameof this.TextCapacity) world
        member this.SetTextCapacity (value : int) world = this.Set (nameof this.TextCapacity) value world
        member this.TextCapacity = lens (nameof this.TextCapacity) this this.GetTextCapacity this.SetTextCapacity
        member this.GetFocused world : bool = this.Get (nameof this.Focused) world
        member this.SetFocused (value : bool) world = this.Set (nameof this.Focused) value world
        member this.Focused = lens (nameof this.Focused) this this.GetFocused this.SetFocused
        member this.GetCursor world : int = this.Get (nameof this.Cursor) world
        member this.SetCursor (value : int) world = this.Set (nameof this.Cursor) value world
        member this.Cursor = lens (nameof this.Cursor) this this.GetCursor this.SetCursor
        member this.TextEditEvent = Events.TextEditEvent --> this
        member this.FocusEvent = Events.FocusEvent --> this

/// Augments an entity with text box behavior.
type TextBoxFacet () =
    inherit Facet (false, false, false)

    static let handleMouseLeftDown evt (world : World) =
        let entity = evt.Subscriber : Entity
        if world.Advancing && entity.GetVisible world then
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
            let mousePositionWorld = World.getMousePosition2dWorld transform.Absolute world
            if perimeter.Intersects mousePositionWorld then
                if transform.Enabled && not (entity.GetFocused world) then
                    let eventTrace = EventTrace.debug "TextBoxFacet" "handleMouseLeftDown" "" EventTrace.empty
                    World.publishPlus () entity.FocusEvent eventTrace entity true false world
                    Resolve
                else Resolve
            else Cascade
        else Cascade

    static let handleKeyboardKeyChange evt (world : World) =
        let entity = evt.Subscriber : Entity
        let data = evt.Data : KeyboardKeyData
        let cursor = entity.GetCursor world
        let text = entity.GetText world
        if  world.Advancing &&
            entity.GetVisible world &&
            entity.GetEnabled world &&
            entity.GetFocused world &&
            text.Length < entity.GetTextCapacity world then
            if data.Down then
                if data.KeyboardKey = KeyboardKey.Left then 
                    if cursor > 0 then
                        let cursor = dec cursor
                        entity.SetCursor cursor world
                        let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                        World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
                elif data.KeyboardKey = KeyboardKey.Right then
                    if cursor < text.Length then
                        let cursor = inc cursor
                        let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                        World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
                elif data.KeyboardKey = KeyboardKey.Home || data.KeyboardKey = KeyboardKey.Up then
                    let cursor = 0
                    entity.SetCursor cursor world
                    let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                    World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
                elif data.KeyboardKey = KeyboardKey.End || data.KeyboardKey = KeyboardKey.Down then
                    let cursor = text.Length
                    entity.SetCursor cursor world
                    let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                    World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
                elif data.KeyboardKey = KeyboardKey.Backspace then
                    if cursor > 0 && text.Length > 0 then
                        let text = String.take (dec cursor) text + String.skip cursor text
                        let cursor = dec cursor
                        entity.SetText text world
                        entity.SetCursor cursor world
                        let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                        World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
                elif data.KeyboardKey = KeyboardKey.Delete then
                    let text = entity.GetText world
                    if cursor >= 0 && cursor < text.Length then
                        let text = String.take cursor text + String.skip (inc cursor) text
                        entity.SetText text world
                        let eventTrace = EventTrace.debug "TextBoxFacet" "handleKeyboardKeyChange" "" EventTrace.empty
                        World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
            Resolve
        else Cascade

    static let handleTextInput evt (world : World) =
        let entity = evt.Subscriber : Entity
        let cursor = entity.GetCursor world
        let text = entity.GetText world
        if  world.Advancing &&
            entity.GetVisible world &&
            entity.GetEnabled world &&
            entity.GetFocused world &&
            text.Length < entity.GetTextCapacity world then
            let text =
                if cursor < 0 || cursor >= text.Length
                then text + string evt.Data.TextInput
                else String.take cursor text + string evt.Data.TextInput + String.skip cursor text
            let cursor = inc cursor
            entity.SetText text world
            if cursor >= 0 then entity.SetCursor cursor world
            let eventTrace = EventTrace.debug "TextBoxFacet" "handleTextInput" "" EventTrace.empty
            World.publishPlus { Text = text; Cursor = cursor } entity.TextEditEvent eventTrace entity true false world
            Resolve
        else Cascade

    static member Properties =
        [define Entity.Text ""
         define Entity.Font Assets.Default.Font
         define Entity.FontSizing None
         define Entity.FontStyling Set.empty
         define Entity.TextMargin (v2 2.0f 0.0f)
         define Entity.TextColor Color.White
         define Entity.TextColorDisabled Constants.Gui.ColorDisabledDefault
         define Entity.TextOffset v2Zero
         define Entity.TextShift Constants.Gui.TextShiftDefault
         define Entity.TextCapacity 14
         define Entity.Focused false
         nonPersistent Entity.Cursor 0]

    override this.Register (entity, world) =
        World.sense handleMouseLeftDown Game.MouseLeftDownEvent entity (nameof TextBoxFacet) world
        World.sense handleKeyboardKeyChange Game.KeyboardKeyChangeEvent entity (nameof TextBoxFacet) world
        World.sense handleTextInput Game.TextInputEvent entity (nameof TextBoxFacet) world
        entity.SetCursor (entity.GetText world).Length world

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let absolute = transform.Absolute
        let enabled = transform.Enabled
        let perimeter = transform.Perimeter
        let offset = (entity.GetTextOffset world).V3
        let elevation = transform.Elevation
        let shift = entity.GetTextShift world
        let clipOpt = ValueSome transform.Bounds2d.Box2
        let justification = Justified (JustifyLeft, JustifyMiddle)
        let focused = entity.GetFocused world
        let cursorOpt = if enabled && focused then Some (entity.GetCursor world) else None
        let margin = (entity.GetTextMargin world).V3
        let color = if enabled then entity.GetTextColor world else entity.GetTextColorDisabled world
        let font = entity.GetFont world
        let fontSizing = entity.GetFontSizing world
        let fontStyling = entity.GetFontStyling world
        let text = entity.GetText world
        World.renderGuiText absolute perimeter offset elevation shift clipOpt justification cursorOpt margin color font fontSizing fontStyling text world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important Constants.Engine.EntityGuiSizeDefault v3Zero

[<AutoOpen>]
module EffectFacetExtensions =
    type Entity with
        member this.GetRunMode world : RunMode = this.Get (nameof this.RunMode) world
        member this.SetRunMode (value : RunMode) world = this.Set (nameof this.RunMode) value world
        member this.RunMode = lens (nameof this.RunMode) this this.GetRunMode this.SetRunMode
        member this.GetEffectSymbolOpt world : Symbol AssetTag option = this.Get (nameof this.EffectSymbolOpt) world
        member this.SetEffectSymbolOpt (value : Symbol AssetTag option) world = this.Set (nameof this.EffectSymbolOpt) value world
        member this.EffectSymbolOpt = lens (nameof this.EffectSymbolOpt) this this.GetEffectSymbolOpt this.SetEffectSymbolOpt
        member this.GetEffectStartTimeOpt world : GameTime option = this.Get (nameof this.EffectStartTimeOpt) world
        member this.SetEffectStartTimeOpt (value : GameTime option) world = this.Set (nameof this.EffectStartTimeOpt) value world
        member this.EffectStartTimeOpt = lens (nameof this.EffectStartTimeOpt) this this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get (nameof this.EffectDefinitions) world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.Set (nameof this.EffectDefinitions) value world
        member this.EffectDefinitions = lens (nameof this.EffectDefinitions) this this.GetEffectDefinitions this.SetEffectDefinitions
        member this.GetEffectDescriptor world : Effects.EffectDescriptor = this.Get (nameof this.EffectDescriptor) world
        /// When RunMode is set to RunEarly, call this AFTER setting the rest of the entity's properties. This
        /// is because setting the effect descriptin in RunEarly mode will immediately run the first frame of the
        /// effect due to a semantic limitation in Nu.
        member this.SetEffectDescriptor (value : Effects.EffectDescriptor) world = this.Set (nameof this.EffectDescriptor) value world
        member this.EffectDescriptor = lens (nameof this.EffectDescriptor) this this.GetEffectDescriptor this.SetEffectDescriptor
        member this.GetEffectOffset world : Vector3 = this.Get (nameof this.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof this.EffectOffset) value world
        member this.EffectOffset = lens (nameof this.EffectOffset) this this.GetEffectOffset this.SetEffectOffset
        member this.GetEffectCastShadow world : bool = this.Get (nameof this.EffectCastShadow) world
        member this.SetEffectCastShadow (value : bool) world = this.Set (nameof this.EffectCastShadow) value world
        member this.EffectCastShadow = lens (nameof this.EffectCastShadow) this this.GetEffectCastShadow this.SetEffectCastShadow
        member this.GetEffectClipOpt world : Box2 option = this.Get (nameof this.EffectClipOpt) world
        member this.SetEffectClipOpt (value : Box2 option) world = this.Set (nameof this.EffectClipOpt) value world
        member this.EffectClipOpt = lens (nameof this.EffectClipOpt) this this.GetEffectClipOpt this.SetEffectClipOpt
        member this.GetEffectShadowOffset world : single = this.Get (nameof this.EffectShadowOffset) world
        member this.SetEffectShadowOffset (value : single) world = this.Set (nameof this.EffectShadowOffset) value world
        member this.EffectShadowOffset = lens (nameof this.EffectShadowOffset) this this.GetEffectShadowOffset this.SetEffectShadowOffset
        member this.GetEffectRenderType world : RenderType = this.Get (nameof this.EffectRenderType) world
        member this.SetEffectRenderType (value : RenderType) world = this.Set (nameof this.EffectRenderType) value world
        member this.EffectRenderType = lens (nameof this.EffectRenderType) this this.GetEffectRenderType this.SetEffectRenderType
        member this.GetEffectHistoryMax world : int = this.Get (nameof this.EffectHistoryMax) world
        member this.SetEffectHistoryMax (value : int) world = this.Set (nameof this.EffectHistoryMax) value world
        member this.EffectHistoryMax = lens (nameof this.EffectHistoryMax) this this.GetEffectHistoryMax this.SetEffectHistoryMax
        member this.GetEffectHistory world : Effects.Slice Deque = this.Get (nameof this.EffectHistory) world
        member this.EffectHistory = lensReadOnly (nameof this.EffectHistory) this this.GetEffectHistory
        member this.GetEffectTagTokens world : Map<string, Effects.Slice> = this.Get (nameof this.EffectTagTokens) world
        member this.SetEffectTagTokens (value : Map<string, Effects.Slice>) world = this.Set (nameof this.EffectTagTokens) value world
        member this.EffectTagTokens = lens (nameof this.EffectTagTokens) this this.GetEffectTagTokens this.SetEffectTagTokens
        member this.GetEffectDataToken world : DataToken = this.Get (nameof this.EffectDataToken) world
        member this.SetEffectDataToken (value : DataToken) world = this.Set (nameof this.EffectDataToken) value world
        member this.EffectDataToken = lens (nameof this.EffectDataToken) this this.GetEffectDataToken this.SetEffectDataToken

/// Augments an entity with an effect.
type EffectFacet () =
    inherit Facet (false, false, false)

    static let setEffect effectSymbolOpt (entity : Entity) world =
        match effectSymbolOpt with
        | Some effectSymbol ->
            let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
            match World.assetTagToValueOpt<Effects.EffectDescriptor> effectSymbol symbolLoadMetadata world with
            | Some effect -> entity.SetEffectDescriptor effect world
            | None -> ()
        | None -> ()

    static let run (entity : Entity) world =

        // make effect
        let effect =
            Effect.makePlus
                (match entity.GetEffectStartTimeOpt world with Some effectStartTime -> effectStartTime | None -> GameTime.zero)
                (entity.GetEffectOffset world)
                (entity.GetTransform world)
                (entity.GetEffectClipOpt world)
                (entity.GetEffectShadowOffset world)
                (entity.GetEffectRenderType world)
                (entity.GetParticleSystem world)
                (entity.GetEffectHistoryMax world)
                (entity.GetEffectHistory world)
                (entity.GetEffectDefinitions world)
                (entity.GetEffectDescriptor world)

        // run effect, optionally destroying upon exhaustion
        let (alive, effect, dataToken) = Effect.run effect world
        entity.SetParticleSystem effect.ParticleSystem world
        entity.SetEffectTagTokens effect.TagTokens world
        entity.SetEffectDataToken dataToken world
        if not alive && entity.GetSelfDestruct world then
            World.destroyEntity entity world

    static let handleEffectDescriptorChange evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetEnabled world then
            match entity.GetRunMode world with
            | RunEarly -> run entity world
            | _ -> ()
        Cascade

    static let handleEffectsChange evt world =
        let entity = evt.Subscriber : Entity
        setEffect (entity.GetEffectSymbolOpt world) entity world
        Cascade

    static let handleAssetsReload evt world =
        let entity = evt.Subscriber : Entity
        setEffect (entity.GetEffectSymbolOpt world) entity world
        Cascade

    static let handlePreUpdate evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetEnabled world then
            match entity.GetRunMode world with
            | RunEarly -> run entity world
            | _ -> ()
        Cascade

    static let handlePostUpdate evt world =
        let entity = evt.Subscriber : Entity
        if entity.GetEnabled world then
            match entity.GetRunMode world with
            | RunLate -> run entity world
            | _ -> ()
        Cascade

    static member Properties =
        [define Entity.ParticleSystem Particles.ParticleSystem.empty
         define Entity.SelfDestruct false
         define Entity.RunMode RunLate
         define Entity.EffectSymbolOpt None
         define Entity.EffectStartTimeOpt None
         define Entity.EffectDefinitions Map.empty
         define Entity.EffectDescriptor Effects.EffectDescriptor.empty
         define Entity.EffectOffset v3Zero
         define Entity.EffectCastShadow true
         define Entity.EffectClipOpt None
         define Entity.EffectShadowOffset Constants.Engine.ParticleShadowOffsetDefault
         define Entity.EffectRenderType (ForwardRenderType (0.0f, 0.0f))
         define Entity.EffectHistoryMax Constants.Effects.EffectHistoryMaxDefault
         variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))
         nonPersistent Entity.EffectTagTokens Map.empty
         nonPersistent Entity.EffectDataToken DataToken.empty]

    override this.Register (entity, world) =
        let effectStartTime = Option.defaultValue world.GameTime (entity.GetEffectStartTimeOpt world)
        entity.SetEffectStartTimeOpt (Some effectStartTime) world
        World.sense handleEffectDescriptorChange entity.EffectDescriptor.ChangeEvent entity (nameof EffectFacet) world
        World.sense handleEffectsChange entity.EffectSymbolOpt.ChangeEvent entity (nameof EffectFacet) world
        World.sense handleAssetsReload Nu.Game.Handle.AssetsReloadEvent entity (nameof EffectFacet) world
        World.sense handlePreUpdate entity.Group.PreUpdateEvent entity (nameof EffectFacet) world
        World.sense handlePostUpdate entity.Group.PostUpdateEvent entity (nameof EffectFacet) world

    override this.Render (renderPass, entity, world) =

        // ensure rendering is applicable for this pass
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && entity.GetEffectCastShadow world
        if not renderPass.IsShadowPass || castShadow then

            // render effect data token
            let time = world.GameTime
            let dataToken = entity.GetEffectDataToken world
            World.renderDataToken renderPass dataToken world

            // render particles
            let presence = entity.GetPresence world
            let particleSystem = entity.GetParticleSystem world
            let descriptors = ParticleSystem.toParticlesDescriptors time particleSystem
            for descriptor in descriptors do
                match descriptor with
                | SpriteParticlesDescriptor descriptor ->
                    let message =
                        { Elevation = descriptor.Elevation
                          Horizon = descriptor.Horizon
                          AssetTag = descriptor.Image
                          RenderOperation2d = RenderSpriteParticles descriptor }
                    World.enqueueLayeredOperation2d message world
                | BillboardParticlesDescriptor descriptor ->
                    let message =
                        RenderBillboardParticles
                            { CastShadow = castShadow
                              Presence = presence
                              MaterialProperties = descriptor.MaterialProperties
                              Material = descriptor.Material
                              ShadowOffset = descriptor.ShadowOffset
                              Particles = descriptor.Particles
                              DepthTest =  LessThanOrEqualTest
                              RenderType = descriptor.RenderType
                              RenderPass = renderPass }
                    World.enqueueRenderMessage3d message world

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        [|Intersection.ofNullable intersectionOpt|]

[<AutoOpen>]
module RigidBodyFacetExtensions =
    type Entity with
        member this.GetBodyEnabled world : bool = this.Get (nameof this.BodyEnabled) world
        member this.SetBodyEnabled (value : bool) world = this.Set (nameof this.BodyEnabled) value world
        member this.BodyEnabled = lens (nameof this.BodyEnabled) this this.GetBodyEnabled this.SetBodyEnabled
        member this.GetBodyFrozen world : bool = this.Get (nameof this.BodyFrozen) world
        member this.SetBodyFrozen (value : bool) world = this.Set (nameof this.BodyFrozen) value world
        member this.BodyFrozen = lens (nameof this.BodyFrozen) this this.GetBodyFrozen this.SetBodyFrozen
        member this.GetBodyType world : BodyType = this.Get (nameof this.BodyType) world
        member this.SetBodyType (value : BodyType) world = this.Set (nameof this.BodyType) value world
        member this.BodyType = lens (nameof this.BodyType) this this.GetBodyType this.SetBodyType
        member this.GetBodyShape world : BodyShape = this.Get (nameof this.BodyShape) world
        member this.SetBodyShape (value : BodyShape) world = this.Set (nameof this.BodyShape) value world
        member this.BodyShape = lens (nameof this.BodyShape) this this.GetBodyShape this.SetBodyShape
        member this.GetSleepingAllowed world : bool = this.Get (nameof this.SleepingAllowed) world
        member this.SetSleepingAllowed (value : bool) world = this.Set (nameof this.SleepingAllowed) value world
        member this.SleepingAllowed = lens (nameof this.SleepingAllowed) this this.GetSleepingAllowed this.SetSleepingAllowed
        member this.GetFriction world : single = this.Get (nameof this.Friction) world
        member this.SetFriction (value : single) world = this.Set (nameof this.Friction) value world
        member this.Friction = lens (nameof this.Friction) this this.GetFriction this.SetFriction
        member this.GetRestitution world : single = this.Get (nameof this.Restitution) world
        member this.SetRestitution (value : single) world = this.Set (nameof this.Restitution) value world
        member this.Restitution = lens (nameof this.Restitution) this this.GetRestitution this.SetRestitution
        member this.GetLinearVelocity world : Vector3 = this.Get (nameof this.LinearVelocity) world
        member this.SetLinearVelocity (value : Vector3) world = this.Set (nameof this.LinearVelocity) value world
        member this.LinearVelocity = lens (nameof this.LinearVelocity) this this.GetLinearVelocity this.SetLinearVelocity
        member this.GetLinearDamping world : single = this.Get (nameof this.LinearDamping) world
        member this.SetLinearDamping (value : single) world = this.Set (nameof this.LinearDamping) value world
        member this.LinearDamping = lens (nameof this.LinearDamping) this this.GetLinearDamping this.SetLinearDamping
        member this.GetAngularVelocity world : Vector3 = this.Get (nameof this.AngularVelocity) world
        member this.SetAngularVelocity (value : Vector3) world = this.Set (nameof this.AngularVelocity) value world
        member this.AngularVelocity = lens (nameof this.AngularVelocity) this this.GetAngularVelocity this.SetAngularVelocity
        member this.GetAngularDamping world : single = this.Get (nameof this.AngularDamping) world
        member this.SetAngularDamping (value : single) world = this.Set (nameof this.AngularDamping) value world
        member this.AngularDamping = lens (nameof this.AngularDamping) this this.GetAngularDamping this.SetAngularDamping
        member this.GetAngularFactor world : Vector3 = this.Get (nameof this.AngularFactor) world
        member this.SetAngularFactor (value : Vector3) world = this.Set (nameof this.AngularFactor) value world
        member this.AngularFactor = lens (nameof this.AngularFactor) this this.GetAngularFactor this.SetAngularFactor
        member this.GetSubstance world : Substance = this.Get (nameof this.Substance) world
        member this.SetSubstance (value : Substance) world = this.Set (nameof this.Substance) value world
        member this.Substance = lens (nameof this.Substance) this this.GetSubstance this.SetSubstance
        member this.GetGravityOverride world : Vector3 option = this.Get (nameof this.GravityOverride) world
        member this.SetGravityOverride (value : Vector3 option) world = this.Set (nameof this.GravityOverride) value world
        member this.GravityOverride = lens (nameof this.GravityOverride) this this.GetGravityOverride this.SetGravityOverride
        member this.GetCharacterProperties world : CharacterProperties = this.Get (nameof this.CharacterProperties) world
        member this.SetCharacterProperties (value : CharacterProperties) world = this.Set (nameof this.CharacterProperties) value world
        member this.CharacterProperties = lens (nameof this.CharacterProperties) this this.GetCharacterProperties this.SetCharacterProperties
        member this.GetVehicleProperties world : VehicleProperties = this.Get (nameof this.VehicleProperties) world
        member this.SetVehicleProperties (value : VehicleProperties) world = this.Set (nameof this.VehicleProperties) value world
        member this.VehicleProperties = lens (nameof this.VehicleProperties) this this.GetVehicleProperties this.SetVehicleProperties
        member this.GetCollisionDetection world : CollisionDetection = this.Get (nameof this.CollisionDetection) world
        member this.SetCollisionDetection (value : CollisionDetection) world = this.Set (nameof this.CollisionDetection) value world
        member this.CollisionDetection = lens (nameof this.CollisionDetection) this this.GetCollisionDetection this.SetCollisionDetection
        member this.GetCollisionCategories world : string = this.Get (nameof this.CollisionCategories) world
        member this.SetCollisionCategories (value : string) world = this.Set (nameof this.CollisionCategories) value world
        member this.CollisionCategories = lens (nameof this.CollisionCategories) this this.GetCollisionCategories this.SetCollisionCategories
        member this.GetCollisionMask world : string = this.Get (nameof this.CollisionMask) world
        member this.SetCollisionMask (value : string) world = this.Set (nameof this.CollisionMask) value world
        member this.CollisionMask = lens (nameof this.CollisionMask) this this.GetCollisionMask this.SetCollisionMask
        member this.GetPhysicsMotion world : PhysicsMotion = this.Get (nameof this.PhysicsMotion) world
        member this.SetPhysicsMotion (value : PhysicsMotion) world = this.Set (nameof this.PhysicsMotion) value world
        member this.PhysicsMotion = lens (nameof this.PhysicsMotion) this this.GetPhysicsMotion this.SetPhysicsMotion
        member this.GetSensor world : bool = this.Get (nameof this.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof this.Sensor) value world
        member this.Sensor = lens (nameof this.Sensor) this this.GetSensor this.SetSensor
        member this.GetAwakeTimeStamp world : int64 = this.Get (nameof this.AwakeTimeStamp) world
        member this.SetAwakeTimeStamp (value : int64) world = this.Set (nameof this.AwakeTimeStamp) value world
        member this.AwakeTimeStamp = lens (nameof this.AwakeTimeStamp) this this.GetAwakeTimeStamp this.SetAwakeTimeStamp
        member this.GetAwake world : bool = this.Get (nameof this.Awake) world
        member this.Awake = lensReadOnly (nameof this.Awake) this this.GetAwake
        member this.GetBodyId world : BodyId = this.Get (nameof this.BodyId) world
        member this.BodyId = lensReadOnly (nameof this.BodyId) this this.GetBodyId

/// Augments an entity with a physics-driven rigid body.
type RigidBodyFacet () =
    inherit Facet (true, false, false)

    static let getBodyShape (entity : Entity) world =
        let scalar = entity.GetScale world * entity.GetSize world
        let bodyShape = entity.GetBodyShape world
        if entity.GetIs2d world
        then World.localizePrimitiveBodyShape scalar bodyShape
        else bodyShape // NOTE: localization does not apply to 3D bodies.

    static let propagatePhysicsCenter (entity : Entity) (_ : Event<ChangeData, Entity>) world =
        if entity.GetPhysicsMotion world <> ManualMotion then
            let bodyId = entity.GetBodyId world
            let center = if entity.GetIs2d world then entity.GetPerimeterCenter world else entity.GetPosition world
            World.setBodyCenter center bodyId world
            Cascade
        else Cascade

    static let propagatePhysicsRotation (entity : Entity) (evt : Event<ChangeData, Entity>) world =
        if entity.GetPhysicsMotion world <> ManualMotion then
            let bodyId = entity.GetBodyId world
            let rotation = evt.Data.Value :?> Quaternion
            World.setBodyRotation rotation bodyId world
            Cascade
        else Cascade

    static let propagatePhysicsLinearVelocity (entity : Entity) (evt : Event<ChangeData, Entity>) world =
        if entity.GetPhysicsMotion world <> ManualMotion then
            let bodyId = entity.GetBodyId world
            let linearVelocity = evt.Data.Value :?> Vector3
            World.setBodyLinearVelocity linearVelocity bodyId world
            Cascade
        else Cascade

    static let propagatePhysicsAngularVelocity (entity : Entity) (evt : Event<ChangeData, Entity>) world =
        if entity.GetPhysicsMotion world <> ManualMotion then
            let bodyId = entity.GetBodyId world
            let angularVelocity = evt.Data.Value :?> Vector3
            World.setBodyAngularVelocity angularVelocity bodyId world
            Cascade
        else Cascade

    static let propagatePhysicsAffected (entity : Entity) (evt : Event<ChangeData, Entity>) world =
        if evt.Data.Name = nameof Entity.BodyType && not (evt.Data.Value :?> BodyType).IsStatic then entity.SetStatic false world
        entity.PropagatePhysics world
        Cascade

    static let createVehiclePropertiesAether () =
        VehiclePropertiesAether

    static let createVehiclePropertiesJolt () =

        let createWheelSettingsWV front position =
            let settings = new JoltPhysicsSharp.WheelSettingsWV ()
            settings.Position <- position
            settings.WheelForward <- v3Forward
            if front then
                settings.MaxBrakeTorque <- 0.0f
                settings.MaxHandBrakeTorque <- 4000.0f
            else
                settings.MaxBrakeTorque <- 4000.0f
                settings.MaxHandBrakeTorque <- 4000.0f
                settings.MaxSteerAngle <- 0.0f
            settings

        // vehicle controller config
        let mutable differential = JoltPhysicsSharp.VehicleDifferentialSettings (LeftWheel = 0, RightWheel = 1)
        let wheeledVehicleControllerSettings = new JoltPhysicsSharp.WheeledVehicleControllerSettings ()
        wheeledVehicleControllerSettings.DifferentialsCount <- 1
        wheeledVehicleControllerSettings.SetDifferential (0, differential)

        // vehicle wheels config
        let wheelSettings =
            [|for i in 0 .. dec 4 do
                let position =
                    match i with
                    | 0 -> v3 -0.8f -0.3f -3.0f // front left
                    | 1 -> v3 0.8f -0.3f -3.0f // front right
                    | 2 -> v3 -0.8f -0.3f 1.5f // back left
                    | 3 -> v3 0.8f -0.3f 1.5f // back right
                    | _ -> failwithumf ()
                createWheelSettingsWV (i < 2) position :> JoltPhysicsSharp.WheelSettings|]

        // vehicle constraint config
        let vehicleConstraintSettings = new JoltPhysicsSharp.VehicleConstraintSettings ()
        vehicleConstraintSettings.Forward <- v3Forward
        vehicleConstraintSettings.Wheels <- wheelSettings
        vehicleConstraintSettings.Controller <- wheeledVehicleControllerSettings

        // fin
        VehiclePropertiesJolt vehicleConstraintSettings

    static member Properties =
        [define Entity.BodyEnabled true
         define Entity.BodyFrozen false
         define Entity.BodyType Static
         define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.SleepingAllowed true
         define Entity.Friction Constants.Physics.FrictionDefault
         define Entity.Restitution 0.0f
         define Entity.LinearVelocity v3Zero
         define Entity.LinearDamping 0.0f
         define Entity.AngularVelocity v3Zero
         define Entity.AngularDamping Constants.Physics.AngularDampingDefault
         define Entity.AngularFactor v3One
         define Entity.Substance (Mass 1.0f)
         define Entity.GravityOverride None
         define Entity.CharacterProperties CharacterProperties.defaultProperties
         nonPersistent Entity.VehicleProperties VehiclePropertiesAbsent
         define Entity.CollisionDetection Discrete
         define Entity.CollisionCategories "1"
         define Entity.CollisionMask Constants.Physics.CollisionWildcard
         define Entity.PhysicsMotion SynchronizedMotion
         define Entity.Sensor false
         nonPersistent Entity.AwakeTimeStamp 0L
         computed Entity.Awake (fun (entity : Entity) world -> entity.GetAwakeTimeStamp world = world.UpdateTime) None
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = Constants.Physics.InternalIndex }) None]

    override this.Register (entity, world) =

        // OPTIMIZATION: using manual unsubscription in order to use less live objects for subscriptions.
        // OPTIMIZATION: share lambdas to reduce live object count.
        // OPTIMIZATION: using special BodyPropertiesAffecting change event to reduce subscription count.
        let subIds = Array.init 5 (fun _ -> Gen.id64)
        World.subscribePlus subIds.[0] (propagatePhysicsCenter entity) (entity.ChangeEvent (nameof entity.Transform)) entity world |> ignore
        World.subscribePlus subIds.[1] (propagatePhysicsRotation entity) (entity.ChangeEvent (nameof entity.Rotation)) entity world |> ignore
        World.subscribePlus subIds.[2] (propagatePhysicsLinearVelocity entity) (entity.ChangeEvent (nameof entity.LinearVelocity)) entity world |> ignore
        World.subscribePlus subIds.[3] (propagatePhysicsAngularVelocity entity) (entity.ChangeEvent (nameof entity.AngularVelocity)) entity world |> ignore
        World.subscribePlus subIds.[4] (propagatePhysicsAffected entity) (entity.ChangeEvent "BodyPropertiesAffecting") entity world |> ignore
        let unsubscribe = fun world ->
            for subId in subIds do
                World.unsubscribe subId world
        let callback = fun evt world ->
            if  Set.contains (nameof RigidBodyFacet) (evt.Data.Previous :?> string Set) &&
                not (Set.contains (nameof RigidBodyFacet) (evt.Data.Value :?> string Set)) then
                unsubscribe world
                Cascade
            else Cascade
        let callback2 = fun _ world ->
            unsubscribe world
            Cascade
        World.sense callback entity.FacetNames.ChangeEvent entity (nameof RigidBodyFacet) world
        World.sense callback2 entity.UnregisteringEvent entity (nameof RigidBodyFacet) world
        entity.SetAwakeTimeStamp world.UpdateTime world

    override this.RegisterPhysics (entity, world) =
        let frozen = entity.GetBodyFrozen world
        if not frozen then
            let is2d = entity.GetIs2d world
            let bodyId = entity.GetBodyId world
            let mutable transform = entity.GetTransform world
            let vehicleProperties =
                match entity.GetBodyType world with
                | Vehicle ->
                    match entity.GetVehicleProperties world with
                    | VehiclePropertiesAbsent -> if is2d then createVehiclePropertiesAether () else createVehiclePropertiesJolt ()
                    | _ as properties -> properties
                | _ -> VehiclePropertiesAbsent
            let bodyProperties =
                { Enabled = entity.GetBodyEnabled world
                  Center = if entity.GetIs2d world then transform.PerimeterCenter else transform.Position
                  Rotation = transform.Rotation
                  Scale = transform.Scale
                  BodyShape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  SleepingAllowed = entity.GetSleepingAllowed world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  AngularFactor = entity.GetAngularFactor world
                  Substance = entity.GetSubstance world
                  GravityOverride = entity.GetGravityOverride world
                  CharacterProperties = entity.GetCharacterProperties world
                  VehicleProperties = vehicleProperties
                  CollisionDetection = entity.GetCollisionDetection world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Sensor = entity.GetSensor world
                  Awake = entity.GetAwake world
                  BodyIndex = bodyId.BodyIndex }
            if entity.GetIs2d world
            then World.createBody2d bodyId bodyProperties world
            else World.createBody3d bodyId bodyProperties world

    override this.UnregisterPhysics (entity, world) =
        let frozen = entity.GetBodyFrozen world
        if not frozen then
            if entity.GetIs2d world
            then World.destroyBody2d (entity.GetBodyId world) world
            else World.destroyBody3d (entity.GetBodyId world) world

    override this.Edit (op, entity, world) =
        match (op, entity.GetBodyType world) with
        | (ViewportOverlay _, Vehicle) ->
            let bodyId = entity.GetBodyId world
            for i in 0 .. dec 4 do
                let wheelModelMatrix = World.getBodyWheelModelMatrix v3Right v3Up i bodyId world
                World.imGuiCircle3d wheelModelMatrix.Translation 5.0f false Color.Yellow world
        | (_, _) -> ()

[<AutoOpen>]
module BodyJointFacetExtensions =
    type Entity with
        member this.GetBodyJoint world : BodyJoint = this.Get (nameof this.BodyJoint) world
        member this.SetBodyJoint (value : BodyJoint) world = this.Set (nameof this.BodyJoint) value world
        member this.BodyJoint = lens (nameof this.BodyJoint) this this.GetBodyJoint this.SetBodyJoint
        member this.GetBodyJointTarget world : Entity Address = this.Get (nameof this.BodyJointTarget) world
        member this.SetBodyJointTarget (value : Entity Address) world = this.Set (nameof this.BodyJointTarget) value world
        member this.BodyJointTarget = lens (nameof this.BodyJointTarget) this this.GetBodyJointTarget this.SetBodyJointTarget
        member this.GetBodyJointTarget2Opt world : Entity Address option = this.Get (nameof this.BodyJointTarget2Opt) world
        member this.SetBodyJointTarget2Opt (value : Entity Address option) world = this.Set (nameof this.BodyJointTarget2Opt) value world
        member this.BodyJointTarget2Opt = lens (nameof this.BodyJointTarget2Opt) this this.GetBodyJointTarget2Opt this.SetBodyJointTarget2Opt
        member this.GetBodyJointEnabled world : bool = this.Get (nameof this.BodyJointEnabled) world
        member this.SetBodyJointEnabled (value : bool) world = this.Set (nameof this.BodyJointEnabled) value world
        member this.BodyJointEnabled = lens (nameof this.BodyJointEnabled) this this.GetBodyJointEnabled this.SetBodyJointEnabled
        member this.GetBreakingPoint world : single = this.Get (nameof this.BreakingPoint) world
        member this.SetBreakingPoint (value : single) world = this.Set (nameof this.BreakingPoint) value world
        member this.BreakingPoint = lens (nameof this.BreakingPoint) this this.GetBreakingPoint this.SetBreakingPoint
        member this.GetBroken world : bool = this.Get (nameof this.Broken) world
        member this.SetBroken (value : bool) world = this.Set (nameof this.Broken) value world
        member this.Broken = lens (nameof this.Broken) this this.GetBroken this.SetBroken
        member this.GetCollideConnected world : bool = this.Get (nameof this.CollideConnected) world
        member this.SetCollideConnected (value : bool) world = this.Set (nameof this.CollideConnected) value world
        member this.CollideConnected = lens (nameof this.CollideConnected) this this.GetCollideConnected this.SetCollideConnected
        member this.GetBodyJointId world : BodyJointId = this.Get (nameof this.BodyJointId) world
        member this.BodyJointId = lensReadOnly (nameof this.BodyJointId) this this.GetBodyJointId
        member this.BodyJointBreakEvent = Events.BodyJointBreakEvent --> this

/// Augments an entity with a physics-driven joint.
type BodyJointFacet () =
    inherit Facet (true, false, false)

    static let tryGetBodyTargetIds (entity : Entity) world =
        match tryResolve (entity.GetBodyJointTarget world) entity with
        | Some targetEntity ->
            let targetId = { BodySource = targetEntity; BodyIndex = Constants.Physics.InternalIndex }
            match entity.GetBodyJointTarget2Opt world with
            | Some target2 ->
                match tryResolve target2 entity with
                | Some target2Entity ->
                    let target2Id = { BodySource = target2Entity; BodyIndex = Constants.Physics.InternalIndex }
                    Some (targetId, Some target2Id)
                | None -> None
            | None -> Some (targetId, None)
        | None -> None

    static member Properties =
        [define Entity.BodyJoint EmptyJoint
         define Entity.BodyJointTarget (Address.makeParent ())
         define Entity.BodyJointTarget2Opt None
         define Entity.BodyJointEnabled true
         define Entity.BreakingPoint Constants.Physics.BreakingPointDefault
         define Entity.Broken false
         define Entity.CollideConnected true
         computed Entity.BodyJointId (fun (entity : Entity) _ -> { BodyJointSource = entity; BodyJointIndex = Constants.Physics.InternalIndex }) None]

    override this.Register (entity, world) =
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyJoint)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyJointTarget)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyJointTarget2Opt)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyJointEnabled)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BreakingPoint)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Broken)) entity (nameof BodyJointFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollideConnected)) entity (nameof BodyJointFacet) world

    override this.RegisterPhysics (entity, world) =
        match tryGetBodyTargetIds entity world with
        | Some (targetId, target2IdOpt) ->
            let bodyJointProperties =
                { BodyJoint = entity.GetBodyJoint world
                  BodyJointTarget = targetId
                  BodyJointTarget2Opt = target2IdOpt
                  BodyJointEnabled = entity.GetBodyJointEnabled world
                  BreakingPoint = entity.GetBreakingPoint world
                  Broken = entity.GetBroken world
                  CollideConnected = entity.GetCollideConnected world
                  BodyJointIndex = (entity.GetBodyJointId world).BodyJointIndex }
            if entity.GetIs2d world
            then World.createBodyJoint2d entity bodyJointProperties world
            else World.createBodyJoint3d entity bodyJointProperties world
        | None -> ()

    override this.UnregisterPhysics (entity, world) =
        match tryGetBodyTargetIds entity world with
        | Some (targetId, target2IdOpt) ->
            if entity.GetIs2d world
            then World.destroyBodyJoint2d targetId target2IdOpt (entity.GetBodyJointId world) world
            else World.destroyBodyJoint3d targetId target2IdOpt (entity.GetBodyJointId world) world
        | None -> ()

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.unimportant

[<AutoOpen>]
module TileMapFacetExtensions =
    type Entity with
        member this.GetTileLayerClearance world : single = this.Get (nameof this.TileLayerClearance) world
        member this.SetTileLayerClearance (value : single) world = this.Set (nameof this.TileLayerClearance) value world
        member this.TileLayerClearance = lens (nameof this.TileLayerClearance) this this.GetTileLayerClearance this.SetTileLayerClearance
        member this.GetTileSizeDivisor world : int = this.Get (nameof this.TileSizeDivisor) world
        member this.SetTileSizeDivisor (value : int) world = this.Set (nameof this.TileSizeDivisor) value world
        member this.TileSizeDivisor = lens (nameof this.TileSizeDivisor) this this.GetTileSizeDivisor this.SetTileSizeDivisor
        member this.GetTileIndexOffset world : int = this.Get (nameof this.TileIndexOffset) world
        member this.SetTileIndexOffset (value : int) world = this.Set (nameof this.TileIndexOffset) value world
        member this.TileIndexOffset = lens (nameof this.TileIndexOffset) this this.GetTileIndexOffset this.SetTileIndexOffset
        member this.GetTileIndexOffsetRange world : int * int = this.Get (nameof this.TileIndexOffsetRange) world
        member this.SetTileIndexOffsetRange (value : int * int) world = this.Set (nameof this.TileIndexOffsetRange) value world
        member this.TileIndexOffsetRange = lens (nameof this.TileIndexOffsetRange) this this.GetTileIndexOffsetRange this.SetTileIndexOffsetRange
        member this.GetTileMap world : TileMap AssetTag = this.Get (nameof this.TileMap) world
        member this.SetTileMap (value : TileMap AssetTag) world = this.Set (nameof this.TileMap) value world
        member this.TileMap = lens (nameof this.TileMap) this this.GetTileMap this.SetTileMap

/// Augments an entity with a asset-defined tile map.
type TileMapFacet () =
    inherit Facet (true, false, false)

    static member Properties =
        [define Entity.BodyEnabled true
         define Entity.Friction 0.0f
         define Entity.Restitution 0.0f
         define Entity.CollisionDetection Discrete
         define Entity.CollisionCategories "1"
         define Entity.CollisionMask Constants.Physics.CollisionWildcard
         define Entity.PhysicsMotion SynchronizedMotion
         define Entity.ClipOpt None
         define Entity.Color Color.One
         define Entity.Emission Color.Zero
         define Entity.TileLayerClearance 2.0f
         define Entity.TileSizeDivisor 1
         define Entity.TileIndexOffset 0
         define Entity.TileIndexOffsetRange (0, 0)
         define Entity.TileMap Assets.Default.TileMap
         nonPersistent Entity.AwakeTimeStamp 0L
         computed Entity.Awake (fun (entity : Entity) world -> entity.GetAwakeTimeStamp world = world.UpdateTime) None
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

    override this.Register (entity, world) =
        entity.AutoBounds world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionDetection)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.TileSizeDivisor)) entity (nameof TileMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.TileMap)) entity (nameof TileMapFacet) world
        World.sense (fun _ world ->
            let attributes = entity.GetAttributesInferred world
            let mutable transform = entity.GetTransform world
            transform.Size <- attributes.SizeInferred
            transform.Offset <- attributes.OffsetInferred
            entity.SetTransformWithoutEvent transform world
            entity.PropagatePhysics world
            Cascade)
            (entity.ChangeEvent (nameof entity.TileMap))
            entity
            (nameof TileMapFacet)
            world

    override this.RegisterPhysics (entity, world) =
        match TmxMap.tryGetTileMap (entity.GetTileMap world) with
        | Some tileMap ->
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
            let tileMapPosition = perimeterUnscaled.Min.V2
            let tileSizeDivisor = entity.GetTileSizeDivisor world
            let tileMapDescriptor = TmxMap.getDescriptor tileMapPosition tileSizeDivisor tileMap
            let bodyProperties =
                TmxMap.getBodyProperties
                    transform.Enabled
                    (entity.GetFriction world)
                    (entity.GetRestitution world)
                    (entity.GetCollisionDetection world)
                    (entity.GetCollisionCategories world)
                    (entity.GetCollisionMask world)
                    (entity.GetBodyId world).BodyIndex
                    tileMapDescriptor
            if entity.GetIs2d world
            then World.createBody2d (entity.GetBodyId world) bodyProperties world
            else World.createBody3d (entity.GetBodyId world) bodyProperties world
        | None -> ()

    override this.UnregisterPhysics (entity, world) =
        if entity.GetIs2d world
        then World.destroyBody2d (entity.GetBodyId world) world
        else World.destroyBody3d (entity.GetBodyId world) world

    override this.Render (_, entity, world) =
        let tileMapAsset = entity.GetTileMap world
        match TmxMap.tryGetTileMap tileMapAsset with
        | Some tileMap ->
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
            let viewBounds = World.getViewBounds2dRelative world
            let tileMapMessages =
                TmxMap.getLayeredMessages2d
                    world.GameTime
                    transform.Absolute
                    viewBounds
                    perimeterUnscaled.Min.V2
                    transform.Elevation
                    (entity.GetClipOpt world |> Option.toValueOption)
                    (entity.GetColor world)
                    (entity.GetEmission world)
                    (entity.GetTileLayerClearance world)
                    (entity.GetTileSizeDivisor world)
                    (entity.GetTileIndexOffset world)
                    (entity.GetTileIndexOffsetRange world)
                    tileMapAsset.PackageName
                    tileMap
            World.enqueueLayeredOperations2d tileMapMessages world
        | None -> ()

    override this.GetAttributesInferred (entity, world) =
        match TmxMap.tryGetTileMap (entity.GetTileMap world) with
        | Some tileMap -> TmxMap.getAttributesInferred (entity.GetTileSizeDivisor world) tileMap
        | None -> AttributesInferred.important Constants.Engine.Entity2dSizeDefault v3Zero

[<AutoOpen>]
module TmxMapFacetExtensions =
    type Entity with
        member this.GetTmxMap world : TmxMap = this.Get (nameof this.TmxMap) world
        member this.SetTmxMap (value : TmxMap) world = this.Set (nameof this.TmxMap) value world
        member this.TmxMap = lens (nameof this.TmxMap) this this.GetTmxMap this.SetTmxMap

/// Augments an entity with a user-defined tile map.
type TmxMapFacet () =
    inherit Facet (true, false, false)

    static member Properties =
        [define Entity.BodyEnabled true
         define Entity.Friction 0.0f
         define Entity.Restitution 0.0f
         define Entity.CollisionDetection Discrete
         define Entity.CollisionCategories "1"
         define Entity.CollisionMask Constants.Physics.CollisionWildcard
         define Entity.PhysicsMotion SynchronizedMotion
         define Entity.ClipOpt None
         define Entity.Color Color.One
         define Entity.Emission Color.Zero
         define Entity.TileLayerClearance 2.0f
         define Entity.TileSizeDivisor 1
         define Entity.TileIndexOffset 0
         define Entity.TileIndexOffsetRange (0, 0)
         nonPersistent Entity.TmxMap (TmxMap.makeDefault ())
         nonPersistent Entity.AwakeTimeStamp 0L
         computed Entity.Awake (fun (entity : Entity) world -> entity.GetAwakeTimeStamp world = world.UpdateTime) None
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

    override this.Register (entity, world) =
        entity.AutoBounds world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionDetection)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.TileSizeDivisor)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.TmxMap)) entity (nameof TmxMapFacet) world
        World.sense (fun _ world ->
            let attributes = entity.GetAttributesInferred world
            let mutable transform = entity.GetTransform world
            transform.Size <- attributes.SizeInferred
            transform.Offset <- attributes.OffsetInferred
            entity.SetTransformWithoutEvent transform world
            entity.PropagatePhysics world
            Cascade)
            (entity.ChangeEvent (nameof entity.TmxMap))
            entity
            (nameof TmxMapFacet)
            world

    override this.RegisterPhysics (entity, world) =
        let mutable transform = entity.GetTransform world
        let perimeterUnscaled = transform.PerimeterUnscaled // tmx map currently ignores rotation and scale
        let tileSizeDivisor = entity.GetTileSizeDivisor world
        let tmxMap = entity.GetTmxMap world
        let tmxMapPosition = perimeterUnscaled.Min.V2
        let tmxMapDescriptor = TmxMap.getDescriptor tmxMapPosition tileSizeDivisor tmxMap
        let bodyProperties =
            TmxMap.getBodyProperties
                transform.Enabled
                (entity.GetFriction world)
                (entity.GetRestitution world)
                (entity.GetCollisionDetection world)
                (entity.GetCollisionCategories world)
                (entity.GetCollisionMask world)
                (entity.GetBodyId world).BodyIndex
                tmxMapDescriptor
        if entity.GetIs2d world
        then World.createBody2d (entity.GetBodyId world) bodyProperties world
        else World.createBody3d (entity.GetBodyId world) bodyProperties world

    override this.UnregisterPhysics (entity, world) =
        if entity.GetIs2d world
        then World.destroyBody2d (entity.GetBodyId world) world
        else World.destroyBody3d (entity.GetBodyId world) world

    override this.Render (_, entity, world) =
        let mutable transform = entity.GetTransform world
        let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
        let viewBounds = World.getViewBounds2dRelative world
        let tmxMap = entity.GetTmxMap world
        let tmxPackage = if tmxMap.TmxDirectory = "" then Assets.Default.PackageName else PathF.GetFileName tmxMap.TmxDirectory // really folder name, but whatever...
        let tmxMapMessages =
            TmxMap.getLayeredMessages2d
                world.GameTime
                transform.Absolute
                viewBounds
                perimeterUnscaled.Min.V2
                transform.Elevation
                (entity.GetClipOpt world |> Option.toValueOption)
                (entity.GetColor world)
                (entity.GetEmission world)
                (entity.GetTileLayerClearance world)
                (entity.GetTileSizeDivisor world)
                (entity.GetTileIndexOffset world)
                (entity.GetTileIndexOffsetRange world)
                tmxPackage
                tmxMap
        World.enqueueLayeredOperations2d tmxMapMessages world

    override this.GetAttributesInferred (entity, world) =
        let tmxMap = entity.GetTmxMap world
        TmxMap.getAttributesInferred (entity.GetTileSizeDivisor world) tmxMap

[<AutoOpen>]
module SpineSkeletonExtensions =
    type Entity with
        member this.GetSpineSkeleton world : SpineSkeleton AssetTag = this.Get (nameof this.SpineSkeleton) world
        member this.SetSpineSkeleton (value : SpineSkeleton AssetTag) world = this.Set (nameof this.SpineSkeleton) value world
        member this.SpineSkeleton = lens (nameof this.SpineSkeleton) this this.GetSpineSkeleton this.SetSpineSkeleton
        member this.GetSpineSkeletonStateOpt world : SpineSkeletonState option = this.Get (nameof this.SpineSkeletonStateOpt) world
        member this.SetSpineSkeletonStateOpt (value : SpineSkeletonState option) world = this.Set (nameof this.SpineSkeletonStateOpt) value world
        member this.SpineSkeletonStateOpt = lens (nameof this.SpineSkeletonStateOpt) this this.GetSpineSkeletonStateOpt this.SetSpineSkeletonStateOpt
        member this.GetSpineAnimations world : SpineAnimation array = this.Get (nameof this.SpineAnimations) world
        member this.SetSpineAnimations (value : SpineAnimation array) world = this.Set (nameof this.SpineAnimations) value world
        member this.SpineAnimations = lens (nameof this.SpineAnimations) this this.GetSpineAnimations this.SetSpineAnimations
        member this.GetSpineAnimationSpeed world : single = this.Get (nameof this.SpineAnimationSpeed) world
        member this.SetSpineAnimationSpeed (value : single) world = this.Set (nameof this.SpineAnimationSpeed) value world
        member this.SpineAnimationSpeed = lens (nameof this.SpineAnimationSpeed) this this.GetSpineAnimationSpeed this.SetSpineAnimationSpeed
        member this.GetSpineAnimationMix world : single = this.Get (nameof this.SpineAnimationMix) world
        member this.SetSpineAnimationMix (value : single) world = this.Set (nameof this.SpineAnimationMix) value world
        member this.SpineAnimationMix = lens (nameof this.SpineAnimationMix) this this.GetSpineAnimationMix this.SetSpineAnimationMix
        member this.SpineSkeletonAnimationTriggerEvent = Events.SpineSkeletonAnimationTriggerEvent --> this

/// Augments an entity with Spine skeleton content.
/// NOTE: SpineSkeleteState fields are inherently imperative and therefore currently unsupported by undo / redo.
type SpineSkeletonFacet () =
    inherit Facet (false, false, false)

    static let getOrTryCreateSpineSkeletonState (entity : Entity) world =
        let spineSkeleton = entity.GetSpineSkeleton world
        match entity.GetSpineSkeletonStateOpt world with
        | None ->
            match Metadata.tryGetSpineSkeletonMetadata spineSkeleton with
            | ValueSome metadata ->
                let startTime = entity.GetStartTime world
                let localTime = world.GameTime - startTime
                let spineSkeletonInstance = Spine.Skeleton metadata.SpineSkeletonData
                spineSkeletonInstance.Time <- localTime.Seconds
                let spineAnimationStateData = Spine.AnimationStateData spineSkeletonInstance.Data
                spineAnimationStateData.DefaultMix <- entity.GetSpineAnimationMix world
                let spineAnimationState = Spine.AnimationState spineAnimationStateData
                let spineAnimations = entity.GetSpineAnimations world
                spineAnimationState.ClearTracks ()
                let mutable i = 0
                for spineAnimation in spineAnimations do
                    if notNull (spineAnimationState.Data.SkeletonData.FindAnimation spineAnimation.SpineAnimationName) then
                        spineAnimationState.SetAnimation (i, spineAnimation.SpineAnimationName, spineAnimation.SpineAnimationPlayback = Loop) |> ignore<Spine.TrackEntry>
                        i <- inc i
                let color = entity.GetColor world
                spineSkeletonInstance.R <- color.R
                spineSkeletonInstance.G <- color.G
                spineSkeletonInstance.B <- color.B
                spineSkeletonInstance.A <- color.A
                let spineSkeletonState = { SpineSkeleton = spineSkeletonInstance; SpineAnimationState = spineAnimationState }
                entity.SetSpineSkeletonStateOpt (Some spineSkeletonState) world
                Some spineSkeletonState
            | ValueNone -> None
        | Some spineSkeletonState -> Some spineSkeletonState

    static let handleAnimationChange evt world =
        let entity = evt.Subscriber : Entity
        entity.SetSpineSkeletonStateOpt None world
        getOrTryCreateSpineSkeletonState entity world |> ignore<SpineSkeletonState option>
        Cascade

    static member Properties =
        [define Entity.AlwaysUpdate true
         define Entity.StartTime GameTime.zero
         define Entity.Color Color.White
         define Entity.Flip FlipNone
         define Entity.SpineSkeleton Assets.Default.SpineSkeleton
         nonPersistent Entity.SpineSkeletonStateOpt None
         define Entity.SpineAnimations [|{ SpineAnimationName = "idle"; SpineAnimationPlayback = Loop }|]
         define Entity.SpineAnimationSpeed 1.0f
         define Entity.SpineAnimationMix 0.2f]

    override this.Register (entity, world) =
        World.sense handleAnimationChange entity.StartTime.ChangeEvent entity (nameof SpineSkeletonFacet) world
        World.sense handleAnimationChange entity.SpineSkeleton.ChangeEvent entity (nameof SpineSkeletonFacet) world
        World.sense handleAnimationChange entity.SpineAnimations.ChangeEvent entity (nameof SpineSkeletonFacet) world
        World.sense handleAnimationChange entity.SpineAnimationMix.ChangeEvent entity (nameof SpineSkeletonFacet) world
        entity.SetStartTime world.GameTime world

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let gameDelta = world.GameDelta
            match getOrTryCreateSpineSkeletonState entity world with
            | Some spineSkeletonState ->
                let startTrackArgs = List ()
                let interruptTrackArgs = List ()
                let completeTrackArgs = List ()
                let endTrackArgs = List ()
                let eventTrackArgs = List ()
                let startDelegate = Spine.AnimationState.TrackEntryDelegate startTrackArgs.Add
                let interruptDelegate = Spine.AnimationState.TrackEntryDelegate interruptTrackArgs.Add
                let completeDelegate = Spine.AnimationState.TrackEntryDelegate completeTrackArgs.Add
                let endDelegate = Spine.AnimationState.TrackEntryDelegate endTrackArgs.Add
                let eventDelegate = Spine.AnimationState.TrackEntryEventDelegate (fun entry event -> eventTrackArgs.Add (entry, event))
                spineSkeletonState.SpineAnimationState.add_Start startDelegate
                spineSkeletonState.SpineAnimationState.add_Interrupt interruptDelegate
                spineSkeletonState.SpineAnimationState.add_Complete completeDelegate
                spineSkeletonState.SpineAnimationState.add_End endDelegate
                spineSkeletonState.SpineAnimationState.add_Event eventDelegate
                let color = entity.GetColor world
                spineSkeletonState.SpineSkeleton.R <- color.R
                spineSkeletonState.SpineSkeleton.G <- color.G
                spineSkeletonState.SpineSkeleton.B <- color.B
                spineSkeletonState.SpineSkeleton.A <- color.A
                let struct (scaleX, scaleY) =
                    match entity.GetFlip world with
                    | FlipNone -> struct (1.0f, 1.0f)
                    | FlipH -> struct (-1.0f, 1.0f)
                    | FlipV -> struct (1.0f, -1.0f)
                    | FlipHV -> struct (-1.0f, -1.0f)
                spineSkeletonState.SpineSkeleton.ScaleX <- scaleX
                spineSkeletonState.SpineSkeleton.ScaleY <- scaleY
                spineSkeletonState.SpineAnimationState.TimeScale <- entity.GetSpineAnimationSpeed world
                spineSkeletonState.SpineSkeleton.Update gameDelta.Seconds
                spineSkeletonState.SpineAnimationState.Update gameDelta.Seconds
                spineSkeletonState.SpineAnimationState.Apply spineSkeletonState.SpineSkeleton |> ignore<bool>
                spineSkeletonState.SpineSkeleton.UpdateWorldTransform Spine.Skeleton.Physics.Update
                spineSkeletonState.SpineAnimationState.remove_Start startDelegate
                spineSkeletonState.SpineAnimationState.remove_Interrupt interruptDelegate
                spineSkeletonState.SpineAnimationState.remove_Complete completeDelegate
                spineSkeletonState.SpineAnimationState.remove_End endDelegate
                spineSkeletonState.SpineAnimationState.remove_Event eventDelegate
                for arg in startTrackArgs do World.publishUnsorted (SpineSkeletonAnimationStartData arg) entity.SpineSkeletonAnimationTriggerEvent entity world
                for arg in interruptTrackArgs do World.publishUnsorted (SpineSkeletonAnimationInterruptData arg) entity.SpineSkeletonAnimationTriggerEvent entity world
                for arg in completeTrackArgs do World.publishUnsorted (SpineSkeletonAnimationCompleteData arg) entity.SpineSkeletonAnimationTriggerEvent entity world
                for arg in endTrackArgs do World.publishUnsorted (SpineSkeletonAnimationEndData arg) entity.SpineSkeletonAnimationTriggerEvent entity world
                for arg in eventTrackArgs do World.publishUnsorted (SpineSkeletonAnimationEventData arg) entity.SpineSkeletonAnimationTriggerEvent entity world
            | None -> ()
        else entity.StartTime.Map ((+) world.GameDelta) world

    override this.Render (_, entity, world) =
        let spineSkeleton = entity.GetSpineSkeleton world
        match entity.GetSpineSkeletonStateOpt world with
        | Some spineSkeletonState ->
            let mutable transform = entity.GetTransform world
            let spineSkeletonId = entity.GetId world
            let spineSkeletonClone = Spine.Skeleton spineSkeletonState.SpineSkeleton // NOTE: this is where the bulk of this entity's allocations are coming from.
            let renderSpineSkeleton = RenderSpineSkeleton { Transform = transform; SpineSkeletonId = spineSkeletonId; SpineSkeletonClone = spineSkeletonClone }
            let renderOperation = LayeredOperation2d { Elevation = transform.Elevation; Horizon = transform.Horizon; AssetTag = spineSkeleton; RenderOperation2d = renderSpineSkeleton }
            World.enqueueRenderMessage2d renderOperation world
        | None -> ()

    override this.GetAttributesInferred (entity, world) =
        match getOrTryCreateSpineSkeletonState entity world with
        | Some spineSkeletonState ->

            // update skeleton so we can take some actual metrics
            spineSkeletonState.SpineAnimationState.Apply spineSkeletonState.SpineSkeleton |> ignore<bool>
            spineSkeletonState.SpineSkeleton.UpdateWorldTransform Spine.Skeleton.Physics.Update
            let mutable (minX, minY, maxX, maxY) = (Single.MaxValue, Single.MaxValue, Single.MinValue, Single.MinValue)

            // compute bounds
            // NOTE: this uses a simplistic algorithm that merely makes a very loose approximation of the bounds since
            // SkeletonBounds doesn't work in our test case.
            // TODO: P1: improve the accuracy of this algorithm.
            for slot in spineSkeletonState.SpineSkeleton.Slots do
                if slot.Bone.Active then
                    minX <- min minX slot.Bone.AX
                    minY <- min minY slot.Bone.AY
                    maxX <- max maxX slot.Bone.AX
                    maxY <- max maxY slot.Bone.AY
            let skeletonSize = v3 (maxX - minX) (maxY - minY) 0.0f
            let skeletonOffset = v3 ((skeletonSize.X * 0.5f - maxX) / skeletonSize.X * 0.5f) ((skeletonSize.Y * 0.5f - maxY) / skeletonSize.Y * 0.5f) 0.0f
            AttributesInferred.important skeletonSize skeletonOffset

        | None -> base.GetAttributesInferred (entity, world)

[<AutoOpen>]
module LayoutFacetExtensions =
    type Entity with
        member this.GetLayout world : Layout = this.Get (nameof this.Layout) world
        member this.SetLayout (value : Layout) world = this.Set (nameof this.Layout) value world
        member this.Layout = lens (nameof this.Layout) this this.GetLayout this.SetLayout
        member this.GetLayoutMargin world : Vector2 = this.Get (nameof this.LayoutMargin) world
        member this.SetLayoutMargin (value : Vector2) world = this.Set (nameof this.LayoutMargin) value world
        member this.LayoutMargin = lens (nameof this.LayoutMargin) this this.GetLayoutMargin this.SetLayoutMargin
        member this.GetLayoutOrder world : int = this.Get (nameof this.LayoutOrder) world
        member this.SetLayoutOrder (value : int) world = this.Set (nameof this.LayoutOrder) value world
        member this.LayoutOrder = lens (nameof this.LayoutOrder) this this.GetLayoutOrder this.SetLayoutOrder
        member this.GetDockType world : DockType = this.Get (nameof this.DockType) world
        member this.SetDockType (value : DockType) world = this.Set (nameof this.DockType) value world
        member this.DockType = lens (nameof this.DockType) this this.GetDockType this.SetDockType
        member this.GetGridPosition world : Vector2i = this.Get (nameof this.GridPosition) world
        member this.SetGridPosition (value : Vector2i) world = this.Set (nameof this.GridPosition) value world
        member this.GridPosition = lens (nameof this.GridPosition) this this.GetGridPosition this.SetGridPosition

/// Augments an entity with the capability to perform layout transformations on its children.
type LayoutFacet () =
    inherit Facet (false, false, false)

    static let rec flowRightward
        reentry leftX (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
        let childPerimeter = child.GetPerimeter world // gui currently ignores rotation
        let childHalfWidth = childPerimeter.Width * 0.5f
        let childHalfHeight = childPerimeter.Height * 0.5f
        let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
        let childRightX = childCenter.X + childHalfWidth + margin.X
        offsetX <- childCenter.X + childHalfWidth
        if childRightX > leftX + wrapLimit then
            offsetX <- leftX
            offsetY <- offsetY + -margin.Y + -maximum
            maximum <- 0.0f
            if not reentry
            then flowRightward true leftX margin wrapLimit &offsetX &offsetY &maximum child world
            else child.SetPerimeterCenterLocal childCenter.V3 world
        else child.SetPerimeterCenterLocal childCenter.V3 world
        if childPerimeter.Height > maximum then maximum <- childPerimeter.Height

    static let rec flowDownward
        reentry topY (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
        let childPerimeter = child.GetPerimeter world // gui currently ignores rotation
        let childHalfWidth = childPerimeter.Width * 0.5f
        let childHalfHeight = childPerimeter.Height * 0.5f
        let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
        let childBottomY = childCenter.Y + -childHalfHeight + -margin.Y
        offsetY <- childCenter.Y + -childHalfHeight
        if childBottomY < topY + -wrapLimit then
            offsetX <- offsetX + margin.X + maximum
            offsetY <- topY
            maximum <- 0.0f
            if not reentry
            then flowDownward true topY margin wrapLimit &offsetX &offsetY &maximum child world
            else child.SetPerimeterCenterLocal childCenter.V3 world
        else child.SetPerimeterCenterLocal childCenter.V3 world
        if childPerimeter.Width > maximum then maximum <- childPerimeter.Width

    static let flowLayout (perimeter : Box2) margin flowDirection flowLimit children world =
        let leftX = perimeter.Width * -0.5f
        let topY = perimeter.Height * 0.5f
        let mutable offsetX = leftX
        let mutable offsetY = topY
        let mutable maximum = 0.0f
        match flowDirection with
        | FlowRightward ->
            let wrapLimit =
                match flowLimit with
                | FlowParent -> perimeter.Width
                | FlowUnlimited -> Single.MaxValue
                | FlowTo limit -> limit
            for child in children do
                flowRightward false leftX margin wrapLimit &offsetX &offsetY &maximum child world
        | FlowDownward ->
            let wrapLimit =
                match flowLimit with
                | FlowParent -> perimeter.Height
                | FlowUnlimited -> Single.MaxValue
                | FlowTo limit -> limit
            for child in children do
                flowDownward false topY margin wrapLimit &offsetX &offsetY &maximum child world
        | FlowLeftward ->
            Log.warnOnce "FlowLeftward not yet implemented." // TODO: P1: implement.
        | FlowUpward ->
            Log.warnOnce "FlowUpward not yet implemented." // TODO: P1: implement.

    static let dockLayout (perimeter : Box2) margin (margins : Vector4) (children : Entity array) world =
        let perimeterWidthHalf = perimeter.Width * 0.5f
        let perimeterHeightHalf = perimeter.Height * 0.5f
        for child in children do
            if child.Has<LayoutFacet> world then
                match child.GetDockType world with
                | DockCenter ->
                    let size =
                        v2
                            (perimeter.Width - margins.X - margins.Z)
                            (perimeter.Height - margins.Y - margins.W) -
                        margin
                    let position =
                        v2
                            ((margins.X - margins.Z) * 0.5f)
                            ((margins.Y - margins.W) * 0.5f)
                    child.SetPositionLocal position.V3 world
                    child.SetSize size.V3 world
                | DockTop ->
                    let size = v2 perimeter.Width margins.Y - margin
                    let position = v2 0.0f (-perimeterHeightHalf + margins.Y * 0.5f)
                    child.SetPositionLocal position.V3 world
                    child.SetSize size.V3 world
                | DockRight ->
                    let size = v2 margins.Z (perimeter.Height - margins.Y - margins.W) - margin
                    let position = v2 (perimeterWidthHalf - margins.Z * 0.5f) ((margins.Y - margins.W) * 0.5f)
                    child.SetPositionLocal position.V3 world
                    child.SetSize size.V3 world
                | DockBottom ->
                    let size = v2 perimeter.Width margins.W - margin
                    let position = v2 0.0f (perimeterHeightHalf - margins.Z * 0.5f)
                    child.SetPositionLocal position.V3 world
                    child.SetSize size.V3 world
                | DockLeft ->
                    let size = v2 margins.X (perimeter.Height - margins.Y - margins.W) - margin
                    let position = v2 (-perimeterWidthHalf + margins.X * 0.5f) ((margins.Y - margins.W) * 0.5f)
                    child.SetPositionLocal position.V3 world
                    child.SetSize size.V3 world

    static let gridLayout (perimeter : Box2) margin (dims : Vector2i) flowDirectionOpt resizeChildren (children : Entity array) world =
        let perimeterWidthHalf = perimeter.Width * 0.5f
        let perimeterHeightHalf = perimeter.Height * 0.5f
        let cellSize = v2 (perimeter.Width / single dims.X) (perimeter.Height / single dims.Y)
        let cellWidthHalf = cellSize.X * 0.5f
        let cellHeightHalf = cellSize.Y * 0.5f
        let childSize = cellSize - margin
        let mutable n = 0
        for child in children do
            let (i, j) =
                match flowDirectionOpt with
                | Some flowDirection ->
                    match flowDirection with
                    | FlowRightward -> (n % dims.Y, n / dims.Y)
                    | FlowDownward -> (n / dims.Y, n % dims.Y)
                    | FlowLeftward -> (dec dims.Y - n % dims.Y, dec dims.Y - n / dims.Y)
                    | FlowUpward -> (dec dims.Y - n / dims.Y, dec dims.Y - n % dims.Y)
                | None ->
                    if child.Has<LayoutFacet> world then
                        let gridPosition = child.GetGridPosition world
                        (gridPosition.X, gridPosition.Y)
                    else (0, 0)
            let childPosition =
                v2
                    (-perimeterWidthHalf + single i * cellSize.X + cellWidthHalf)
                    (perimeterHeightHalf - single j * cellSize.Y - cellHeightHalf)
            child.SetPositionLocal childPosition.V3 world
            if resizeChildren then child.SetSize childSize.V3 world
            n <- inc n

    static let performLayout (entity : Entity) world =
        match entity.GetLayout world with
        | Manual -> () // OPTIMIZATION: early exit.
        | layout ->
            let children =
                World.getEntityMounters entity world
                |> Array.ofSeq // array for sorting
                |> Array.map (fun child ->
                    let layoutOrder =
                        if child.Has<LayoutFacet> world
                        then child.GetLayoutOrder world
                        else 0
                    let order = child.GetOrder world
                    (layoutOrder, order, child))
                |> Array.sortBy ab_
                |> Array.map __c
            let perimeter = (entity.GetPerimeter world).Box2 // gui currently ignores rotation
            let margin = entity.GetLayoutMargin world
            match layout with
            | Flow (flowDirection, flowLimit) ->
                flowLayout perimeter margin flowDirection flowLimit children world
            | Dock (margins, percentageBased, resizeChildren) ->
                ignore (percentageBased, resizeChildren) // TODO: P1: implement using these values.
                dockLayout perimeter margin margins children world
            | Grid (dims, flowDirectionOpt, resizeChildren) ->
                gridLayout perimeter margin dims flowDirectionOpt resizeChildren children world
            | Manual -> ()

    static let performLayoutPlus (entity : Entity) world =
        let mutable top = entity
        let mutable currentOpt = Some top
        while currentOpt.IsSome do
            match top.GetMountOpt world with
            | Some mount ->
                let mountAddress = Address.resolve mount top.EntityAddress
                if mountAddress.Names.Length > 3 then
                    let mountee = Nu.Entity mountAddress
                    if  mountee.GetExists world &&
                        mountee.Has<LayoutFacet> world then
                        match mountee.GetLayout world with
                        | Flow _ | Grid (_, Some _, _) -> top <- mountee; currentOpt <- Some top
                        | Dock _ | Grid (_, None, _) | Manual -> currentOpt <- None
                    else currentOpt <- None
                else currentOpt <- None
            | None -> currentOpt <- None
        performLayout top world

    static let handleLayout evt world =
        let entity = evt.Subscriber : Entity
        performLayout entity world
        Cascade

    static let handleLayoutPlus evt world =
        let entity = evt.Subscriber : Entity
        performLayoutPlus entity world
        Cascade

    static let handleMount evt world =
        let entity = evt.Subscriber : Entity
        let mounter = evt.Data.Mounter
        let orderChangeUnsub = World.sensePlus (fun _ world -> performLayout entity world; Cascade) (Entity.Order.ChangeEvent --> mounter) entity (nameof LayoutFacet)
        let layoutOrderChangeUnsub = World.sensePlus (fun _ world -> performLayout entity world; Cascade) (Entity.LayoutOrder.ChangeEvent --> mounter) entity (nameof LayoutFacet)
        let dockTypeChangeUnsub = World.sensePlus (fun _ world -> performLayout entity world; Cascade) (Entity.DockType.ChangeEvent --> mounter) entity (nameof LayoutFacet)
        let gridPositionChangeUnsub = World.sensePlus (fun _ world -> performLayout entity world; Cascade) (Entity.GridPosition.ChangeEvent --> mounter) entity (nameof LayoutFacet)
        World.sense (fun evt world ->
            if evt.Data.Mounter = mounter then
                orderChangeUnsub world |> ignore<World -> unit>
                layoutOrderChangeUnsub world |> ignore<World -> unit>
                dockTypeChangeUnsub world |> ignore<World -> unit>
                gridPositionChangeUnsub world |> ignore<World -> unit>
                performLayout entity world
            Cascade)
            entity.UnmountEvent
            entity
            (nameof LayoutFacet)
            world
        performLayout entity world
        Cascade

    static member Properties =
        [define Entity.Layout Manual
         define Entity.LayoutMargin v2Zero
         define Entity.LayoutOrder 0
         define Entity.DockType DockCenter
         define Entity.GridPosition v2iZero]

    override this.Register (entity, world) =
        performLayout entity world
        World.sense handleMount entity.MountEvent entity (nameof LayoutFacet) world
        World.sense handleLayoutPlus entity.Size.ChangeEvent entity (nameof LayoutFacet) world
        World.sense handleLayout entity.Perimeter.ChangeEvent entity (nameof LayoutFacet) world
        World.sense handleLayout entity.Layout.ChangeEvent entity (nameof LayoutFacet) world
        World.sense handleLayout entity.LayoutMargin.ChangeEvent entity (nameof LayoutFacet) world
        World.sense handleLayoutPlus entity.LayoutOrder.ChangeEvent entity (nameof LayoutFacet) world

[<AutoOpen>]
module SkyBoxFacetExtensions =
    type Entity with
        member this.GetAmbientColor world : Color = this.Get (nameof this.AmbientColor) world
        member this.SetAmbientColor (value : Color) world = this.Set (nameof this.AmbientColor) value world
        member this.AmbientColor = lens (nameof this.AmbientColor) this this.GetAmbientColor this.SetAmbientColor
        member this.GetAmbientBrightness world : single = this.Get (nameof this.AmbientBrightness) world
        member this.SetAmbientBrightness (value : single) world = this.Set (nameof this.AmbientBrightness) value world
        member this.AmbientBrightness = lens (nameof this.AmbientBrightness) this this.GetAmbientBrightness this.SetAmbientBrightness
        member this.GetBrightness world : single = this.Get (nameof this.Brightness) world
        member this.SetBrightness (value : single) world = this.Set (nameof this.Brightness) value world
        member this.Brightness = lens (nameof this.Brightness) this this.GetBrightness this.SetBrightness
        member this.GetCubeMap world : CubeMap AssetTag = this.Get (nameof this.CubeMap) world
        member this.SetCubeMap (value : CubeMap AssetTag) world = this.Set (nameof this.CubeMap) value world
        member this.CubeMap = lens (nameof this.CubeMap) this this.GetCubeMap this.SetCubeMap

/// Augments an entity with sky box.
type SkyBoxFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.Presence Omnipresent
         define Entity.Static true
         define Entity.AmbientColor Color.White
         define Entity.AmbientBrightness 0.5f
         define Entity.Color Color.White
         define Entity.Brightness 1.0f
         define Entity.CubeMap Assets.Default.SkyBoxMap]

    override this.Render (renderPass, entity, world) =
        World.enqueueRenderMessage3d
            (RenderSkyBox
                { AmbientColor = entity.GetAmbientColor world
                  AmbientBrightness = entity.GetAmbientBrightness world
                  CubeMapColor = entity.GetColor world
                  CubeMapBrightness = entity.GetBrightness world
                  CubeMap = entity.GetCubeMap world
                  RenderPass = renderPass })
            world

[<AutoOpen>]
module LightProbe3dFacetExtensions =
    type Entity with

        member this.GetProbeBounds world : Box3 = this.Get (nameof this.ProbeBounds) world
        member this.SetProbeBounds (value : Box3) world = this.Set (nameof this.ProbeBounds) value world
        member this.ProbeBounds = lens (nameof this.ProbeBounds) this this.GetProbeBounds this.SetProbeBounds
        member this.GetProbeStale world : bool = this.Get (nameof this.ProbeStale) world
        member this.SetProbeStale (value : bool) world = this.Set (nameof this.ProbeStale) value world
        member this.ProbeStale = lens (nameof this.ProbeStale) this this.GetProbeStale this.SetProbeStale

        /// Reset probe bounds around current probe position.
        member this.ResetProbeBounds world =
            let bounds =
                box3
                    (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + this.GetPosition world)
                    (v3Dup Constants.Render.LightProbeSizeDefault)
            this.SetProbeBounds bounds world

        /// Center probe position within its current bounds.
        member this.RecenterInProbeBounds world =
            let probeBounds = this.GetProbeBounds world
            if Option.isSome (this.GetMountOpt world)
            then this.SetPositionLocal probeBounds.Center world
            else this.SetPosition probeBounds.Center world

/// Augments an entity with a 3d light probe.
type LightProbe3dFacet () =
    inherit Facet (false, true, false)

    static let handleProbeVisibleChange (evt : Event<ChangeData, Entity>) world =
        let entity = evt.Subscriber
        if evt.Data.Value :?> bool && entity.Group.GetVisible world then entity.SetProbeStale true world
        Cascade

    static let handleProbeStaleChange (evt : Event<ChangeData, Entity>) world =
        if evt.Data.Value :?> bool then World.requestLightMapRender world
        Cascade

    static member Properties =
        [define Entity.Size (v3Dup 0.25f)
         define Entity.Presence Omnipresent
         define Entity.LightProbe true
         define Entity.Static true
         define Entity.AmbientColor Color.White
         define Entity.AmbientBrightness 0.5f
         define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
         nonPersistent Entity.ProbeStale false]

    override this.Register (entity, world) =
        World.sense handleProbeVisibleChange entity.Group.Visible.ChangeEvent entity (nameof LightProbe3dFacet) world
        World.sense handleProbeVisibleChange entity.Visible.ChangeEvent entity (nameof LightProbe3dFacet) world
        World.sense handleProbeStaleChange entity.ProbeStale.ChangeEvent entity (nameof LightProbe3dFacet) world
        entity.SetProbeStale true world

    override this.Render (renderPass, entity, world) =
        let id = entity.GetId world
        let enabled = entity.GetEnabled world
        let position = entity.GetPosition world
        let ambientColor = entity.GetAmbientColor world
        let ambientBrightness = entity.GetAmbientBrightness world
        let bounds = entity.GetProbeBounds world
        World.enqueueRenderMessage3d (RenderLightProbe3d { LightProbeId = id; Enabled = enabled; Origin = position; AmbientColor = ambientColor; AmbientBrightness = ambientBrightness; Bounds = bounds; RenderPass = renderPass }) world

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        [|Intersection.ofNullable intersectionOpt|]

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.important (v3Dup 0.25f) v3Zero

    override this.Edit (op, entity, world) =
        match op with
        | AppendProperties append ->
            if ImGui.Button "Rerender Light Map" then entity.SetProbeStale true world // this isn't undoable
            if ImGui.Button "Recenter in Probe Bounds" then
                append.EditContext.Snapshot RencenterInProbeBounds world
                let probeBounds = entity.GetProbeBounds world
                if Option.isSome (entity.GetMountOpt world)
                then entity.SetPositionLocal probeBounds.Center world
                else entity.SetPosition probeBounds.Center world
            if ImGui.Button "Reset Probe Bounds" then
                append.EditContext.Snapshot ResetProbeBounds world
                entity.ResetProbeBounds world
        | _ -> ()

[<AutoOpen>]
module Light3dFacetExtensions =
    type Entity with
        member this.GetAttenuationLinear world : single = this.Get (nameof this.AttenuationLinear) world
        member this.SetAttenuationLinear (value : single) world = this.Set (nameof this.AttenuationLinear) value world
        member this.AttenuationLinear = lens (nameof this.AttenuationLinear) this this.GetAttenuationLinear this.SetAttenuationLinear
        member this.GetAttenuationQuadratic world : single = this.Get (nameof this.AttenuationQuadratic) world
        member this.SetAttenuationQuadratic (value : single) world = this.Set (nameof this.AttenuationQuadratic) value world
        member this.AttenuationQuadratic = lens (nameof this.AttenuationQuadratic) this this.GetAttenuationQuadratic this.SetAttenuationQuadratic
        member this.GetAutoAttenuate world : bool = this.Get (nameof this.AutoAttenuate) world
        member this.SetAutoAttenuate (value : bool) world = this.Set (nameof this.AutoAttenuate) value world
        member this.AutoAttenuate = lens (nameof this.AutoAttenuate) this this.GetAutoAttenuate this.SetAutoAttenuate
        member this.GetLightCutoff world : single = this.Get (nameof this.LightCutoff) world
        member this.SetLightCutoff (value : single) world = this.Set (nameof this.LightCutoff) value world
        member this.LightCutoff = lens (nameof this.LightCutoff) this this.GetLightCutoff this.SetLightCutoff
        member this.GetLightType world : LightType = this.Get (nameof this.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof this.LightType) value world
        member this.LightType = lens (nameof this.LightType) this this.GetLightType this.SetLightType
        member this.GetDesireShadows world : bool = this.Get (nameof this.DesireShadows) world
        member this.SetDesireShadows (value : bool) world = this.Set (nameof this.DesireShadows) value world
        member this.DesireShadows = lens (nameof this.DesireShadows) this this.GetDesireShadows this.SetDesireShadows
        member this.GetDesireFog world : bool = this.Get (nameof this.DesireFog) world
        member this.SetDesireFog (value : bool) world = this.Set (nameof this.DesireFog) value world
        member this.DesireFog = lens (nameof this.DesireFog) this this.GetDesireFog this.SetDesireFog

        member this.ComputeShadowView world =
            match this.GetLightType world with
            | PointLight ->
                Matrix4x4.CreateTranslation (-this.GetPosition world)
            | SpotLight (_, _) ->
                let shadowOrigin = this.GetPosition world
                let shadowRotation = this.GetRotation world
                let shadowForward = shadowRotation.Down
                let shadowUp = shadowForward.OrthonormalUp
                let shadowView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + shadowForward, shadowUp)
                shadowView
            | DirectionalLight | CascadedLight ->
                let shadowOrigin = this.GetPosition world
                let shadowRotation = this.GetRotation world
                let shadowForward = shadowRotation.Down
                let shadowUp = shadowForward.OrthonormalUp
                let shadowView = Matrix4x4.CreateLookAt (shadowOrigin, shadowOrigin + shadowForward, shadowUp)
                shadowView

        member this.ComputeShadowProjection world =
            match this.GetLightType world with
            | PointLight ->
                let shadowCutoff = max (this.GetLightCutoff world) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
                Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)
            | SpotLight (_, coneOuter) ->
                let shadowFov = max (min coneOuter Constants.Render.ShadowFovMax) 0.01f
                let shadowCutoff = max (this.GetLightCutoff world) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
                Matrix4x4.CreatePerspectiveFieldOfView (shadowFov, 1.0f, Constants.Render.NearPlaneDistanceInterior, shadowCutoff)
            | DirectionalLight | CascadedLight ->
                let shadowCutoff = max (this.GetLightCutoff world) (Constants.Render.NearPlaneDistanceInterior * 2.0f)
                Matrix4x4.CreateOrthographic (shadowCutoff * 2.0f, shadowCutoff * 2.0f, -shadowCutoff, shadowCutoff)

        member this.ComputeShadowFrustum world =
            let shadowView = this.ComputeShadowView world
            let shadowProjection = this.ComputeShadowProjection world
            Frustum (shadowView * shadowProjection)

/// Augments an entity with a 3d light.
type Light3dFacet () =
    inherit Facet (false, false, true)

    static let handleLightingChange evt world =
        let entity = evt.Subscriber : Entity
        let brightness = entity.GetBrightness world
        let lightCutoff = entity.GetLightCutoff world
        let world =
            if entity.GetAutoAttenuate world then
                entity.SetAttenuationLinear (1.0f / (brightness * lightCutoff)) world
                entity.SetAttenuationQuadratic (1.0f / (brightness * lightCutoff * lightCutoff)) world
                world
            else world
        let size = v3Dup (lightCutoff * 2.0f)
        entity.SetSize size world
        Cascade

    static member Properties =
        [define Entity.Size (v3Dup (Constants.Render.LightCutoffDefault * 2.0f))
         define Entity.Static true
         define Entity.Light true
         define Entity.Color Color.White
         define Entity.Brightness Constants.Render.BrightnessDefault
         define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
         define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
         define Entity.AutoAttenuate true
         define Entity.LightCutoff Constants.Render.LightCutoffDefault
         define Entity.LightType PointLight
         define Entity.DesireShadows false
         define Entity.DesireFog false]

    override this.Register (entity, world) =
        World.sense handleLightingChange entity.Brightness.ChangeEvent entity (nameof Light3dFacet) world
        World.sense handleLightingChange entity.LightCutoff.ChangeEvent entity (nameof Light3dFacet) world

    override this.Render (renderPass, entity, world) =
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
        let desireFog = entity.GetDesireFog world
        let bounds = entity.GetBounds world
        World.enqueueRenderMessage3d
            (RenderLight3d
                { LightId = lightId
                  Origin = position
                  Rotation = rotation
                  Direction = direction
                  Color = color
                  Brightness = brightness
                  AttenuationLinear = attenuationLinear
                  AttenuationQuadratic = attenuationQuadratic
                  LightCutoff = lightCutoff
                  LightType = lightType
                  DesireShadows = desireShadows
                  DesireFog = desireFog
                  Bounds = bounds
                  RenderPass = renderPass })
            world

    override this.RayCast (ray, entity, world) =
        let boundsSize = v3Dup 0.25f
        let bounds = box3 (entity.GetPosition world - boundsSize * 0.5f) boundsSize
        let intersectionOpt = ray.Intersects bounds
        [|Intersection.ofNullable intersectionOpt|]

    override this.GetAttributesInferred (entity, world) =
        let lightCutoff = entity.GetLightCutoff world
        let size = v3Dup (lightCutoff * 2.0f)
        AttributesInferred.important size v3Zero

    override this.Edit (op, entity, world) =
        match op with
        | AppendProperties ap ->
            if ImGuiNET.ImGui.Button "Normalize Attenutation" then
                ap.EditContext.Snapshot NormalizeAttenuation world
                let brightness = entity.GetBrightness world
                let lightCutoff = entity.GetLightCutoff world
                entity.SetAttenuationLinear (1.0f / (brightness * lightCutoff)) world
                entity.SetAttenuationQuadratic (1.0f / (brightness * lightCutoff * lightCutoff)) world
        | ViewportOverlay _ ->
            let shadowFrustum = entity.ComputeShadowFrustum world
            for segment in shadowFrustum.Segments do
                World.imGuiSegment3d segment 1.0f Color.Yellow world
        | _ -> ()

[<AutoOpen>]
module StaticBillboardFacetExtensions =
    type Entity with
        member this.GetMaterialProperties world : MaterialProperties = this.Get (nameof this.MaterialProperties) world
        member this.SetMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.MaterialProperties) value world
        member this.MaterialProperties = lens (nameof this.MaterialProperties) this this.GetMaterialProperties this.SetMaterialProperties
        member this.GetMaterial world : Material = this.Get (nameof this.Material) world
        member this.SetMaterial (value : Material) world = this.Set (nameof this.Material) value world
        member this.Material = lens (nameof this.Material) this this.GetMaterial this.SetMaterial
        member this.GetDepthTest world : DepthTest = this.Get (nameof this.DepthTest) world
        member this.SetDepthTest (value : DepthTest) world = this.Set (nameof this.DepthTest) value world
        member this.DepthTest = lens (nameof this.DepthTest) this this.GetDepthTest this.SetDepthTest
        member this.GetRenderStyle world : RenderStyle = this.Get (nameof this.RenderStyle) world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set (nameof this.RenderStyle) value world
        member this.RenderStyle = lens (nameof this.RenderStyle) this this.GetRenderStyle this.SetRenderStyle
        member this.GetShadowOffset world : single = this.Get (nameof this.ShadowOffset) world
        member this.SetShadowOffset (value : single) world = this.Set (nameof this.ShadowOffset) value world
        member this.ShadowOffset = lens (nameof this.ShadowOffset) this this.GetShadowOffset this.SetShadowOffset
        member this.GetOrientUp world : bool = this.Get(nameof this.OrientUp) world
        member this.SetOrientUp (value : bool) world = this.Set(nameof this.OrientUp) value world
        member this.OrientUp = lens (nameof this.OrientUp) this this.GetOrientUp this.SetOrientUp
        member this.GetPlanar world : bool = this.Get(nameof this.Planar) world
        member this.SetPlanar (value : bool) world = this.Set(nameof this.Planar) value world
        member this.Planar = lens (nameof this.Planar) this this.GetPlanar this.SetPlanar

/// Augments an entity with a static billboard.
type StaticBillboardFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InsetOpt None
         define Entity.MaterialProperties MaterialProperties.defaultProperties
         define Entity.Material Material.defaultMaterial
         define Entity.DepthTest LessThanOrEqualTest
         define Entity.RenderStyle Deferred
         define Entity.ShadowOffset Constants.Engine.BillboardShadowOffsetDefault
         define Entity.OrientUp true
         define Entity.Planar true]

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let shadowOffset = entity.GetShadowOffset world
            let depthTest = entity.GetDepthTest world
            let orientUp = entity.GetOrientUp world
            let planar = entity.GetPlanar world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.enqueueRenderMessage3d
                (RenderBillboard
                    { CastShadow = castShadow; Presence = presence; ModelMatrix = affineMatrix; InsetOpt = insetOpt; OrientUp = orientUp; Planar = planar;
                      MaterialProperties = properties; Material = material; ShadowOffset = shadowOffset; DepthTest = depthTest; RenderType = renderType; RenderPass = renderPass })
                world

    override this.RayCast (ray, entity, world) =
        // TODO: P1: intersect against oriented quad rather than bounds.
        let bounds = entity.GetBounds world
        let intersectionOpt = ray.Intersects bounds
        [|Intersection.ofNullable intersectionOpt|]

/// Augments an entity with an animated billboard.
type AnimatedBillboardFacet () =
    inherit Facet (false, false, false)

    static let getSpriteInsetOpt (entity : Entity) world =
        let startTime = entity.GetStartTime world
        let celCount = entity.GetCelCount world
        let celRun = entity.GetCelRun world
        if celCount <> 0 && celRun <> 0 then
            let localTime = world.GameTime - startTime
            let cel = int (localTime / entity.GetAnimationDelay world) % celCount * entity.GetAnimationStride world
            let celSize = entity.GetCelSize world
            let celI = cel % celRun
            let celJ = cel / celRun
            let celX = single celI * celSize.X
            let celY = single celJ * celSize.Y
            let inset = box2 (v2 celX celY) celSize
            Some inset
        else None

    static member Properties =
        [define Entity.StartTime GameTime.zero
         define Entity.CelSize (Vector2 (32.0f, 32.0f))
         define Entity.CelCount 16
         define Entity.CelRun 4
         define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
         define Entity.AnimationStride 1
         define Entity.MaterialProperties MaterialProperties.defaultProperties
         define Entity.Material Material.defaultMaterial
         define Entity.DepthTest LessThanOrEqualTest
         define Entity.RenderStyle Deferred
         define Entity.ShadowOffset Constants.Engine.BillboardShadowOffsetDefault
         define Entity.OrientUp true
         define Entity.Planar true]

    override this.Update (entity, world) =
        if not (entity.GetEnabled world) then
            entity.StartTime.Map ((+) world.GameDelta) world

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = getSpriteInsetOpt entity world
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let shadowOffset = entity.GetShadowOffset world
            let depthTest = entity.GetDepthTest world
            let orientUp = entity.GetOrientUp world
            let planar = entity.GetPlanar world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.enqueueRenderMessage3d
                (RenderBillboard
                    { CastShadow = castShadow; Presence = presence; ModelMatrix = affineMatrix; InsetOpt = insetOpt; OrientUp = orientUp; Planar = planar;
                      MaterialProperties = properties; Material = material; ShadowOffset = shadowOffset; DepthTest = depthTest; RenderType = renderType; RenderPass = renderPass })
                world

    override this.RayCast (ray, entity, world) =
        // TODO: P1: intersect against oriented quad rather than bounds.
        let bounds = entity.GetBounds world
        let intersectionOpt = ray.Intersects bounds
        [|Intersection.ofNullable intersectionOpt|]

[<AutoOpen>]
module BasicStaticBillboardEmitterFacetExtensions =
    type Entity with
        member this.GetEmitterMaterialProperties world : MaterialProperties = this.Get (nameof this.EmitterMaterialProperties) world
        member this.SetEmitterMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.EmitterMaterialProperties) value world
        member this.EmitterMaterialProperties = lens (nameof this.EmitterMaterialProperties) this this.GetEmitterMaterialProperties this.SetEmitterMaterialProperties
        member this.GetEmitterMaterial world : Material = this.Get (nameof this.EmitterMaterial) world
        member this.SetEmitterMaterial (value : Material) world = this.Set (nameof this.EmitterMaterial) value world
        member this.EmitterMaterial = lens (nameof this.EmitterMaterial) this this.GetEmitterMaterial this.SetEmitterMaterial
        member this.GetEmitterCastShadow world : bool = this.Get (nameof this.EmitterCastShadow) world
        member this.SetEmitterCastShadow (value : bool) world = this.Set (nameof this.EmitterCastShadow) value world
        member this.EmitterCastShadow = lens (nameof this.EmitterCastShadow) this this.GetEmitterCastShadow this.SetEmitterCastShadow
        member this.GetEmitterShadowOffset world : single = this.Get (nameof this.EmitterShadowOffset) world
        member this.SetEmitterShadowOffset (value : single) world = this.Set (nameof this.EmitterShadowOffset) value world
        member this.EmitterShadowOffset = lens (nameof this.EmitterShadowOffset) this this.GetEmitterShadowOffset this.SetEmitterShadowOffset
        member this.GetEmitterRenderStyle world : RenderStyle = this.Get (nameof this.EmitterRenderStyle) world
        member this.SetEmitterRenderStyle (value : RenderStyle) world = this.Set (nameof this.EmitterRenderStyle) value world
        member this.EmitterRenderStyle = lens (nameof this.EmitterRenderStyle) this this.GetEmitterRenderStyle this.SetEmitterRenderStyle

/// Augments an entity with basic static billboard emitter.
type BasicStaticBillboardEmitterFacet () =
    inherit Facet (false, false, false)

    static let tryMakeEmitter (entity : Entity) (world : World) =
        World.tryMakeEmitter
            world.GameTime
            (entity.GetEmitterLifeTimeOpt world)
            (entity.GetParticleLifeTimeMaxOpt world)
            (entity.GetParticleRate world)
            (entity.GetParticleMax world)
            (entity.GetEmitterStyle world)
            world
        |> Option.map cast<Particles.BasicStaticBillboardEmitter>

    static let makeEmitter entity world =
        match tryMakeEmitter entity world with
        | Some emitter ->
            let mutable transform = entity.GetTransform world
            let renderType = match entity.GetEmitterRenderStyle world with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            { emitter with
                Body =
                    { Position = transform.Position
                      Scale = transform.Scale
                      Angles = transform.Angles
                      LinearVelocity = v3Zero
                      AngularVelocity = v3Zero
                      Restitution = Constants.Particles.RestitutionDefault }
                Absolute = transform.Absolute
                Material = entity.GetEmitterMaterial world
                ParticleSeed = entity.GetBasicParticleSeed world
                Constraint = entity.GetEmitterConstraint world
                RenderType = renderType }
        | None ->
            Particles.BasicStaticBillboardEmitter.makeEmpty
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)

    static let mapParticleSystem mapper (entity : Entity) world =
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = mapper particleSystem
        entity.SetParticleSystem particleSystem world

    static let mapEmitter mapper (entity : Entity) world =
        mapParticleSystem (fun particleSystem ->
            match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                let emitter = mapper emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem)
            entity world

    static let rec processOutput output entity world =
        match output with
        | Particles.OutputEmitter (name, emitter) -> mapParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
        | Particles.Outputs outputs -> for output in outputs do processOutput output entity world

    static let handleEmitterMaterialPropertiesChange evt world =
        let emitterMaterialProperties = evt.Data.Value :?> MaterialProperties
        mapEmitter (fun emitter -> if emitter.MaterialProperties <> emitterMaterialProperties then { emitter with MaterialProperties = emitterMaterialProperties } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterMaterialChange evt world =
        let emitterMaterial = evt.Data.Value :?> Material
        mapEmitter (fun emitter -> if emitter.Material <> emitterMaterial then { emitter with Material = emitterMaterial } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterShadowOffsetChange evt world =
        let emitterShadowOffset = evt.Data.Value :?> single
        mapEmitter (fun emitter -> if emitter.ShadowOffset <> emitterShadowOffset then { emitter with ShadowOffset = emitterShadowOffset } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterRenderStyleChange evt world =
        let emitterRenderStyle = evt.Data.Value :?> RenderStyle
        let emitterRenderType = match emitterRenderStyle with Deferred -> DeferredRenderType | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
        mapEmitter (fun emitter -> if emitter.RenderType <> emitterRenderType then { emitter with RenderType = emitterRenderType } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterLifeTimeOptChange evt world =
        let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
        mapEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
        Cascade

    static let handleParticleLifeTimeMaxOptChange evt world =
        let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
        mapEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
        Cascade

    static let handleParticleRateChange evt world =
        let particleRate = evt.Data.Value :?> single
        mapEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
        Cascade

    static let handleParticleMaxChange evt world =
        let particleMax = evt.Data.Value :?> int
        mapEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticBillboardEmitter.resize particleMax emitter else emitter) evt.Subscriber world
        Cascade

    static let handleBasicParticleSeedChange evt world =
        let particleSeed = evt.Data.Value :?> Particles.BasicParticle
        mapEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterConstraintChange evt world =
        let emitterConstraint = evt.Data.Value :?> Particles.Constraint
        mapEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
        Cascade

    static let handleEmitterStyleChange evt world =
        let entity = evt.Subscriber : Entity
        let emitter = makeEmitter entity world
        mapEmitter (constant emitter) entity world
        Cascade

    static let handlePositionChange evt world =
        let entity = evt.Subscriber : Entity
        let particleSystem = entity.GetParticleSystem world
        let particleSystem =
            match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                let position = entity.GetPosition world
                let emitter =
                    if v3Neq emitter.Body.Position position
                    then { emitter with Body = { emitter.Body with Position = position }}
                    else emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem
        entity.SetParticleSystem particleSystem world
        Cascade

    static let handleRotationChange evt world =
        let entity = evt.Subscriber : Entity
        let particleSystem = entity.GetParticleSystem world
        let particleSystem =
            match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
            | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                let angles = entity.GetAngles world
                let emitter =
                    if v3Neq emitter.Body.Angles angles
                    then { emitter with Body = { emitter.Body with Angles = angles }}
                    else emitter
                { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            | _ -> particleSystem
        entity.SetParticleSystem particleSystem world
        Cascade

    static member Properties =
        [define Entity.SelfDestruct false
         define Entity.EmitterMaterialProperties MaterialProperties.defaultProperties
         define Entity.EmitterMaterial Material.defaultMaterial
         define Entity.EmitterLifeTimeOpt GameTime.zero
         define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
         define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
         define Entity.ParticleMax 60
         define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = v3Dup 0.25f; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
         define Entity.EmitterConstraint Particles.Constraint.empty
         define Entity.EmitterStyle "BasicStaticBillboardEmitter"
         define Entity.EmitterRenderStyle Deferred
         define Entity.EmitterCastShadow true
         define Entity.EmitterShadowOffset Constants.Engine.ParticleShadowOffsetDefault
         nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

    override this.Register (entity, world) =
        let emitter = makeEmitter entity world
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
        entity.SetParticleSystem particleSystem world
        World.sense handlePositionChange entity.Position.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleRotationChange entity.Rotation.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterMaterialPropertiesChange entity.EmitterMaterialProperties.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterMaterialChange entity.EmitterMaterial.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterShadowOffsetChange entity.EmitterShadowOffset.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterRenderStyleChange entity.EmitterRenderStyle.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterLifeTimeOptChange entity.EmitterLifeTimeOpt.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleParticleLifeTimeMaxOptChange entity.ParticleLifeTimeMaxOpt.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleParticleRateChange entity.ParticleRate.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleParticleMaxChange entity.ParticleMax.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleBasicParticleSeedChange entity.BasicParticleSeed.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterConstraintChange entity.EmitterConstraint.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world
        World.sense handleEmitterStyleChange entity.EmitterStyle.ChangeEvent entity (nameof BasicStaticBillboardEmitterFacet) world

    override this.Unregister (entity, world) =
        let particleSystem = entity.GetParticleSystem world
        let particleSystem = { particleSystem with Emitters = Map.remove typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters }
        entity.SetParticleSystem particleSystem world

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let delta = world.GameDelta
            let time = world.GameTime
            let particleSystem = entity.GetParticleSystem world
            let (particleSystem, output) = Particles.ParticleSystem.run delta time particleSystem
            entity.SetParticleSystem particleSystem world
            processOutput output entity world

    override this.Render (renderPass, entity, world) =
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && entity.GetEmitterCastShadow world
        if not renderPass.IsShadowPass || castShadow then
            let time = world.GameTime
            let presence = entity.GetPresence world
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem
                |> Particles.ParticleSystem.toParticlesDescriptors time
                |> List.map (fun descriptor ->
                    match descriptor with
                    | Particles.BillboardParticlesDescriptor descriptor ->
                        let emitterProperties = entity.GetEmitterMaterialProperties world
                        let properties =
                            { AlbedoOpt = match emitterProperties.AlbedoOpt with ValueSome albedo -> ValueSome albedo | ValueNone -> descriptor.MaterialProperties.AlbedoOpt
                              RoughnessOpt = match emitterProperties.RoughnessOpt with ValueSome roughness -> ValueSome roughness | ValueNone -> descriptor.MaterialProperties.RoughnessOpt
                              MetallicOpt = match emitterProperties.MetallicOpt with ValueSome metallic -> ValueSome metallic | ValueNone -> descriptor.MaterialProperties.MetallicOpt
                              AmbientOcclusionOpt = match emitterProperties.AmbientOcclusionOpt with ValueSome ambientOcclusion -> ValueSome ambientOcclusion | ValueNone -> descriptor.MaterialProperties.AmbientOcclusionOpt
                              EmissionOpt = match emitterProperties.EmissionOpt with ValueSome emission -> ValueSome emission | ValueNone -> descriptor.MaterialProperties.EmissionOpt
                              HeightOpt = match emitterProperties.HeightOpt with ValueSome height -> ValueSome height | ValueNone -> descriptor.MaterialProperties.HeightOpt
                              IgnoreLightMapsOpt = match emitterProperties.IgnoreLightMapsOpt with ValueSome ignoreLightMaps -> ValueSome ignoreLightMaps | ValueNone -> descriptor.MaterialProperties.IgnoreLightMapsOpt
                              OpaqueDistanceOpt = ValueNone
                              FinenessOffsetOpt = match emitterProperties.FinenessOffsetOpt with ValueSome finenessOffset -> ValueSome finenessOffset | ValueNone -> descriptor.MaterialProperties.FinenessOffsetOpt
                              ScatterTypeOpt = match emitterProperties.ScatterTypeOpt with ValueSome scatterType -> ValueSome scatterType | ValueNone -> descriptor.MaterialProperties.ScatterTypeOpt
                              SpecularScalarOpt = match emitterProperties.SpecularScalarOpt with ValueSome specularScalar -> ValueSome specularScalar | ValueNone -> descriptor.MaterialProperties.SpecularScalarOpt
                              RefractiveIndexOpt = match emitterProperties.RefractiveIndexOpt with ValueSome refractiveIndex -> ValueSome refractiveIndex | ValueNone -> descriptor.MaterialProperties.RefractiveIndexOpt }
                        let emitterMaterial = entity.GetEmitterMaterial world
                        let material =
                            { AlbedoImageOpt = match emitterMaterial.AlbedoImageOpt with ValueSome albedoImage -> ValueSome albedoImage | ValueNone -> descriptor.Material.AlbedoImageOpt
                              RoughnessImageOpt = match emitterMaterial.RoughnessImageOpt with ValueSome roughnessImage -> ValueSome roughnessImage | ValueNone -> descriptor.Material.RoughnessImageOpt
                              MetallicImageOpt = match emitterMaterial.MetallicImageOpt with ValueSome metallicImage -> ValueSome metallicImage | ValueNone -> descriptor.Material.MetallicImageOpt
                              AmbientOcclusionImageOpt = match emitterMaterial.AmbientOcclusionImageOpt with ValueSome ambientOcclusionImage -> ValueSome ambientOcclusionImage | ValueNone -> descriptor.Material.AmbientOcclusionImageOpt
                              EmissionImageOpt = match emitterMaterial.EmissionImageOpt with ValueSome emissionImage -> ValueSome emissionImage | ValueNone -> descriptor.Material.EmissionImageOpt
                              NormalImageOpt = match emitterMaterial.NormalImageOpt with ValueSome normalImage -> ValueSome normalImage | ValueNone -> descriptor.Material.NormalImageOpt
                              HeightImageOpt = match emitterMaterial.HeightImageOpt with ValueSome heightImage -> ValueSome heightImage | ValueNone -> descriptor.Material.HeightImageOpt
                              SubdermalImageOpt = match emitterMaterial.SubdermalImageOpt with ValueSome subdermalImage -> ValueSome subdermalImage | ValueNone -> descriptor.Material.SubdermalImageOpt
                              FinenessImageOpt = match emitterMaterial.FinenessImageOpt with ValueSome finenessImage -> ValueSome finenessImage | ValueNone -> descriptor.Material.FinenessImageOpt
                              ScatterImageOpt = match emitterMaterial.ScatterImageOpt with ValueSome scatterImage -> ValueSome scatterImage | ValueNone -> descriptor.Material.ScatterImageOpt
                              TwoSidedOpt = match emitterMaterial.TwoSidedOpt with ValueSome twoSided -> ValueSome twoSided | ValueNone -> descriptor.Material.TwoSidedOpt
                              ClippedOpt = match emitterMaterial.ClippedOpt with ValueSome clipped -> ValueSome clipped | ValueNone -> descriptor.Material.ClippedOpt }
                        Some
                            (RenderBillboardParticles
                                { CastShadow = castShadow
                                  Presence = presence
                                  MaterialProperties = properties
                                  Material = material
                                  ShadowOffset = descriptor.ShadowOffset
                                  Particles = descriptor.Particles
                                  DepthTest =  LessThanOrEqualTest
                                  RenderType = descriptor.RenderType
                                  RenderPass = renderPass })
                    | _ -> None)
                |> List.definitize
            World.enqueueRenderMessages3d particlesMessages world

    override this.RayCast (ray, entity, world) =
        let intersectionOpt = ray.Intersects (entity.GetBounds world)
        [|Intersection.ofNullable intersectionOpt|]

[<AutoOpen>]
module StaticModelFacetExtensions =
    type Entity with
        member this.GetClipped world : bool = this.Get (nameof this.Clipped) world
        member this.SetClipped (value : bool) world = this.Set (nameof this.Clipped) value world
        member this.Clipped = lens (nameof this.Clipped) this this.GetClipped this.SetClipped
        member this.GetStaticModel world : StaticModel AssetTag = this.Get (nameof this.StaticModel) world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set (nameof this.StaticModel) value world
        member this.StaticModel = lens (nameof this.StaticModel) this this.GetStaticModel this.SetStaticModel

/// Augments an entity with a static model.
type StaticModelFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InsetOpt None
         define Entity.MaterialProperties MaterialProperties.empty
         define Entity.Clipped false
         define Entity.DepthTest LessThanOrEqualTest
         define Entity.RenderStyle Deferred
         define Entity.StaticModel Assets.Default.StaticModel]

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = ValueOption.ofOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let staticModel = entity.GetStaticModel world
            let clipped = entity.GetClipped world
            let depthTest = entity.GetDepthTest world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.renderStaticModelFast (&affineMatrix, castShadow, presence, insetOpt, &properties, staticModel, clipped, depthTest, renderType, renderPass, world)

    override this.GetAttributesInferred (entity, world) =
        let staticModel = entity.GetStaticModel world
        match Metadata.tryGetStaticModelMetadata staticModel with
        | ValueSome staticModelMetadata ->
            let bounds = staticModelMetadata.Bounds
            AttributesInferred.important bounds.Size bounds.Center
        | ValueNone -> base.GetAttributesInferred (entity, world)

    override this.RayCast (ray, entity, world) =
        let affineMatrix = entity.GetAffineMatrix world
        let inverseMatrix = Matrix4x4.Invert affineMatrix |> snd
        let rayEntity = ray.Transform inverseMatrix
        match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
        | ValueSome staticModelMetadata ->
            let intersectionses =
                Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                    let geometry = surface.PhysicallyBasedGeometry
                    let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                    let raySurface = rayEntity.Transform inverse
                    let boundsIntersectionOpt = raySurface.Intersects geometry.Bounds
                    if boundsIntersectionOpt.HasValue then
                        let intersections = raySurface.Intersects (geometry.Indices, geometry.Vertices)
                        if Seq.notEmpty intersections then
                            intersections
                            |> Seq.map snd'
                            |> Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity)
                            |> Seq.map (fun pointEntity -> pointEntity.Transform affineMatrix)
                            |> Seq.map (fun point -> Hit (point - ray.Origin).Magnitude)
                            |> Seq.toArray
                        else [|Miss|]
                    else [|Miss|])
                    staticModelMetadata.Surfaces
            Array.concat intersectionses
        | ValueNone -> [|Miss|]

[<AutoOpen>]
module StaticModelSurfaceFacetExtensions =
    type Entity with
        member this.GetSurfaceIndex world : int = this.Get (nameof this.SurfaceIndex) world
        member this.SetSurfaceIndex (value : int) world = this.Set (nameof this.SurfaceIndex) value world
        member this.SurfaceIndex = lens (nameof this.SurfaceIndex) this this.GetSurfaceIndex this.SetSurfaceIndex

/// Augments an entity with an indexed static model surface.
type StaticModelSurfaceFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InsetOpt None
         define Entity.MaterialProperties MaterialProperties.defaultProperties
         define Entity.Material Material.empty
         define Entity.DepthTest LessThanOrEqualTest
         define Entity.RenderStyle Deferred
         define Entity.StaticModel Assets.Default.StaticModel
         define Entity.SurfaceIndex 0]

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let staticModel = entity.GetStaticModel world
            let surfaceIndex = entity.GetSurfaceIndex world
            let depthTest = entity.GetDepthTest world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.renderStaticModelSurfaceFast (&affineMatrix, castShadow, presence, insetOpt, &properties, &material, staticModel, surfaceIndex, depthTest, renderType, renderPass, world)

    override this.GetAttributesInferred (entity, world) =
        match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
        | ValueSome staticModelMetadata ->
            let surfaceIndex = entity.GetSurfaceIndex world
            if surfaceIndex > -1 && surfaceIndex < staticModelMetadata.Surfaces.Length then
                let bounds = staticModelMetadata.Surfaces.[surfaceIndex].SurfaceBounds
                AttributesInferred.important bounds.Size bounds.Center
            else base.GetAttributesInferred (entity, world)
        | ValueNone -> base.GetAttributesInferred (entity, world)

    override this.RayCast (ray, entity, world) =
        let rayEntity = ray.Transform (Matrix4x4.Invert (entity.GetAffineMatrix world) |> snd)
        match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
        | ValueSome staticModelMetadata ->
            let surfaceIndex = entity.GetSurfaceIndex world
            if surfaceIndex < staticModelMetadata.Surfaces.Length then
                let surface = staticModelMetadata.Surfaces.[surfaceIndex]
                let geometry = surface.PhysicallyBasedGeometry
                let boundsIntersectionOpt = rayEntity.Intersects geometry.Bounds
                if boundsIntersectionOpt.HasValue then
                    let intersections = Array.ofSeq (rayEntity.Intersects (geometry.Indices, geometry.Vertices))
                    if Array.notEmpty intersections
                    then Array.map (snd' >> Hit) intersections
                    else [|Miss|]
                else [|Miss|]
            else [|Miss|]
        | ValueNone -> [|Miss|]

[<AutoOpen>]
module StaticModelSurfaceFacetExtensions2 =

    type Entity with

        /// Attempt to get the currently assigned albedo image, looking it up from metadata when unassigned.
        member this.TryGetAlbedoImage world =
            if this.Has<StaticModelSurfaceFacet> world then
                let material = this.GetMaterial world
                match material.AlbedoImageOpt with
                | ValueSome _ as albedoImageOpt -> albedoImageOpt
                | ValueNone ->
                    let staticModel = this.GetStaticModel world
                    match Metadata.tryGetStaticModelMetadata staticModel with
                    | ValueSome metadata ->
                        let surfaceIndex = this.GetSurfaceIndex world
                        let surface = metadata.Surfaces.[surfaceIndex]
                        match Metadata.tryGetStaticModelAlbedoImage surface.SurfaceMaterialIndex staticModel with
                        | ValueSome _ as albedoImageOpt -> albedoImageOpt
                        | ValueNone -> ValueNone
                    | ValueNone -> ValueNone
            else ValueNone

        /// Attempt to get the currently assigned albedo image asset name, looking it up from metadata when unassigned.
        /// Useful in editor because oftentimes the only useful identifying information about a static model surface is
        /// its material assets, particularly its albedo image like so -
        /// <code>
        /// for entity in World.getEntities SelectedGroup world do
        ///     match entity.TryGetAlbedoImageAssetName world with
        ///     | ValueSome name when name.Contains "Glass" ->
        ///         entity.SetRenderStyle (Forward (0.0f, 0.0f)) world
        ///     | _ -> ()
        /// </code>
        member this.TryGetAlbedoImageAssetName world =
            match this.TryGetAlbedoImage world with
            | ValueSome albedoImage -> ValueSome albedoImage.AssetName
            | ValueNone -> ValueNone

[<AutoOpen>]
module AnimatedModelFacetExtensions =
    type Entity with

        member this.GetAnimations world : Animation array = this.Get (nameof this.Animations) world
        member this.SetAnimations (value : Animation array) world = this.Set (nameof this.Animations) value world
        member this.Animations = lens (nameof this.Animations) this this.GetAnimations this.SetAnimations
        member this.GetAnimatedModel world : AnimatedModel AssetTag = this.Get (nameof this.AnimatedModel) world
        member this.SetAnimatedModel (value : AnimatedModel AssetTag) world = this.Set (nameof this.AnimatedModel) value world
        member this.AnimatedModel = lens (nameof this.AnimatedModel) this this.GetAnimatedModel this.SetAnimatedModel
        member this.GetSubsortOffsets world : Map<int, single> = this.Get (nameof this.SubsortOffsets) world
        member this.SetSubsortOffsets (value : Map<int, single>) world = this.Set (nameof this.SubsortOffsets) value world
        member this.SubsortOffsets = lens (nameof this.SubsortOffsets) this this.GetSubsortOffsets this.SetSubsortOffsets
        member this.GetDualRenderedSurfaceIndices world : int Set = this.Get (nameof this.DualRenderedSurfaceIndices) world
        member this.SetDualRenderedSurfaceIndices (value : int Set) world = this.Set (nameof this.DualRenderedSurfaceIndices) value world
        member this.DualRenderedSurfaceIndices = lens (nameof this.DualRenderedSurfaceIndices) this this.GetDualRenderedSurfaceIndices this.SetDualRenderedSurfaceIndices
        member this.GetBoneIdsOpt world : Dictionary<string, int> option = this.Get (nameof this.BoneIdsOpt) world
        member this.SetBoneIdsOpt (value : Dictionary<string, int> option) world = this.Set (nameof this.BoneIdsOpt) value world
        member this.BoneIdsOpt = lens (nameof this.BoneIdsOpt) this this.GetBoneIdsOpt this.SetBoneIdsOpt
        member this.GetBoneOffsetsOpt world : Matrix4x4 array option = this.Get (nameof this.BoneOffsetsOpt) world
        member this.SetBoneOffsetsOpt (value : Matrix4x4 array option) world = this.Set (nameof this.BoneOffsetsOpt) value world
        member this.BoneOffsetsOpt = lens (nameof this.BoneOffsetsOpt) this this.GetBoneOffsetsOpt this.SetBoneOffsetsOpt
        member this.GetBoneTransformsOpt world : Matrix4x4 array option = this.Get (nameof this.BoneTransformsOpt) world
        member this.SetBoneTransformsOpt (value : Matrix4x4 array option) world = this.Set (nameof this.BoneTransformsOpt) value world
        member this.BoneTransformsOpt = lens (nameof this.BoneTransformsOpt) this this.GetBoneTransformsOpt this.SetBoneTransformsOpt
        member this.GetUseJobGraph world : bool = this.Get (nameof this.UseJobGraph) world
        member this.SetUseJobGraph (value : bool) world = this.Set (nameof this.UseJobGraph) value world
        member this.UseJobGraph = lens (nameof this.UseJobGraph) this this.GetUseJobGraph this.SetUseJobGraph

        /// Set the bone transforms via a fast path.
        /// OPTIMIZATION: this function sets these properties without comparison or events. Unfortunately, F# forces
        /// array value equality on us. Because this function elides change detection and thus the equality check, it
        /// runs much faster than setting the BoneOffsets and BoneTransform properties normally.
        member this.SetBoneTransformsFast boneIds boneOffsets boneTransforms world =
            let entityState = World.getEntityState this world
            let entityState = EntityState.setProperty (nameof Entity.BoneIdsOpt) { PropertyType = typeof<Dictionary<string, int> option>; PropertyValue = Some boneIds } entityState
            let entityState = EntityState.setProperty (nameof Entity.BoneOffsetsOpt) { PropertyType = typeof<Matrix4x4 array option>; PropertyValue = Some boneOffsets } entityState
            let entityState = EntityState.setProperty (nameof Entity.BoneTransformsOpt) { PropertyType = typeof<Matrix4x4 array option>; PropertyValue = Some boneTransforms } entityState
            World.setEntityState entityState this world

        /// Attempt to get the bone ids, offsets, and transforms from an entity that supports boned models.
        member this.TryGetBoneTransformByName boneName world =
            match this.GetBoneIdsOpt world with
            | Some ids ->
                match ids.TryGetValue boneName with
                | (true, boneIndex) -> this.TryGetBoneTransformByIndex boneIndex world
                | (false, _) -> None
            | None -> None

        /// Attempt to get the bone ids, offsets, and transforms from an entity that supports boned models.
        member this.TryGetBoneTransformByIndex boneIndex world =
            match (this.GetBoneOffsetsOpt world, this.GetBoneTransformsOpt world) with
            | (Some offsets, Some transforms) ->
                let transform =
                    offsets.[boneIndex].Inverted *
                    transforms.[boneIndex] *
                    this.GetAffineMatrix world
                Some transform
            | (_, _) -> None

        /// Attempt to compute the bone transforms (and related data) using the given time and animation data.
        member this.TryComputeBoneTransforms time animations (sceneOpt : Assimp.Scene option) =
            match sceneOpt with
            | Some scene when scene.Meshes.Count > 0 ->
                let (boneIds, boneOffsets, boneTransforms) = scene.ComputeBoneTransforms (time, animations, scene.Meshes.[0])
                Some (boneIds, boneOffsets, boneTransforms)
            | Some _ | None -> None

        /// TODO: document this!
        member this.AnimateBones (world : World) =
            let time = world.GameTime
            let animations = this.GetAnimations world
            let animatedModel = this.GetAnimatedModel world
            let sceneOpt = match Metadata.tryGetAnimatedModelMetadata animatedModel with ValueSome model -> model.SceneOpt | ValueNone -> None
            match this.TryComputeBoneTransforms time animations sceneOpt with
            | Some (boneIds, boneOffsets, boneTransforms) -> this.SetBoneTransformsFast boneIds boneOffsets boneTransforms world
            | None -> ()

/// Augments an entity with an animated model.
type AnimatedModelFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InsetOpt None
         define Entity.MaterialProperties MaterialProperties.empty
         define Entity.Animations [|{ StartTime = GameTime.zero; LifeTimeOpt = None; Name = ""; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None }|]
         define Entity.AnimatedModel Assets.Default.AnimatedModel
         define Entity.SubsortOffsets Map.empty
         define Entity.DualRenderedSurfaceIndices Set.empty
         define Entity.DepthTest LessThanOrEqualTest
         define Entity.RenderStyle Deferred
         nonPersistent Entity.BoneIdsOpt None
         nonPersistent Entity.BoneOffsetsOpt None
         nonPersistent Entity.BoneTransformsOpt None
         define Entity.UseJobGraph true]

    override this.Register (entity, world) =
        entity.AnimateBones world
        World.sense
            (fun evt world ->
                let playBox = fst' (World.getPlayBounds3d world)
                let outsidePlayBounds = entity.GetPresence world <> Omnipresent && not (entity.GetAlwaysUpdate world) && not (playBox.Intersects (evt.Subscriber.GetBounds world))
                let disabled = not (entity.GetEnabled world)
                let notUpdating = world.Halted || outsidePlayBounds || disabled
                if notUpdating then evt.Subscriber.AnimateBones world
                Cascade)
            (entity.ChangeEvent (nameof entity.Animations)) entity (nameof AnimatedModelFacet) world
        World.sense (fun evt world -> evt.Subscriber.AnimateBones world; Cascade) (entity.ChangeEvent (nameof entity.AnimatedModel)) entity (nameof AnimatedModelFacet) world
        World.sense (fun evt world -> evt.Subscriber.AnimateBones world; Cascade) Game.AssetsReloadEvent entity (nameof AnimatedModelFacet) world

    override this.Update (entity, world) =
        if entity.GetEnabled world then
            let time = world.GameTime
            let animations = entity.GetAnimations world
            let animatedModel = entity.GetAnimatedModel world
            let sceneOpt = match Metadata.tryGetAnimatedModelMetadata animatedModel with ValueSome model -> model.SceneOpt | ValueNone -> None
            let resultOpt =
                if entity.GetUseJobGraph world then
                    let resultOpt =
                        match World.tryAwaitJob (world.DateTime + TimeSpan.FromSeconds 0.001) (entity, nameof AnimatedModelFacet) world with
                        | Some (JobCompletion (_, _, (:? ((Dictionary<string, int> * Matrix4x4 array * Matrix4x4 array) option) as boneOffsetsAndTransformsOpt))) -> boneOffsetsAndTransformsOpt
                        | _ -> None
                    let job = Job.make (entity, nameof AnimatedModelFacet) (fun () -> entity.TryComputeBoneTransforms time animations sceneOpt)
                    World.enqueueJob 1.0f job world
                    resultOpt
                else entity.TryComputeBoneTransforms time animations sceneOpt
            match resultOpt with
            | Some (boneIds, boneOffsets, boneTransforms) -> entity.SetBoneTransformsFast boneIds boneOffsets boneTransforms world
            | None -> ()
        else
            let animations =
                Array.map (fun (animation : Animation) ->
                    { animation with StartTime = animation.StartTime + world.GameDelta })
                    (entity.GetAnimations world)
            entity.SetAnimations animations world

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let animatedModel = entity.GetAnimatedModel world
            let subsortOffsets = entity.GetSubsortOffsets world
            let drsIndices = entity.GetDualRenderedSurfaceIndices world
            let depthTest = entity.GetDepthTest world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            match entity.GetBoneTransformsOpt world with
            | Some boneTransforms -> World.renderAnimatedModelFast (&affineMatrix, castShadow, presence, insetOpt, &properties, boneTransforms, animatedModel, subsortOffsets, drsIndices, depthTest, renderType, renderPass, world)
            | None -> ()

    override this.GetAttributesInferred (entity, world) =
        let animatedModel = entity.GetAnimatedModel world
        match Metadata.tryGetAnimatedModelMetadata animatedModel with
        | ValueSome animatedModelMetadata ->
            let bounds = animatedModelMetadata.Bounds
            AttributesInferred.important bounds.Size bounds.Center
        | ValueNone -> base.GetAttributesInferred (entity, world)

    override this.RayCast (ray, entity, world) =
        let affineMatrix = entity.GetAffineMatrix world
        let inverseMatrix = Matrix4x4.Invert affineMatrix |> snd
        let rayEntity = ray.Transform inverseMatrix
        match Metadata.tryGetAnimatedModelMetadata (entity.GetAnimatedModel world) with
        | ValueSome animatedModelMetadata ->
            let intersectionses =
                Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                    let geometry = surface.PhysicallyBasedGeometry
                    let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                    let raySurface = rayEntity.Transform inverse
                    let boundsIntersectionOpt = raySurface.Intersects geometry.Bounds
                    if boundsIntersectionOpt.HasValue then
                        let intersections = raySurface.Intersects (geometry.Indices, geometry.Vertices)
                        if Seq.notEmpty intersections then
                            intersections
                            |> Seq.map snd'
                            |> Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity)
                            |> Seq.map (fun pointEntity -> pointEntity.Transform affineMatrix)
                            |> Seq.map (fun point -> Hit (point - ray.Origin).Magnitude)
                            |> Seq.toArray
                        else [|Miss|]
                    else [|Miss|])
                    animatedModelMetadata.Surfaces
            Array.concat intersectionses
        | ValueNone -> [|Miss|]

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->
            match (entity.GetBoneOffsetsOpt world, entity.GetBoneTransformsOpt world) with
            | (Some offsets, Some transforms) ->
                let affineMatrix = entity.GetAffineMatrix world
                for i in 0 .. dec offsets.Length do
                    let offset = offsets.[i]
                    let transform = transforms.[i]
                    World.imGuiCircle3d (offset.Inverted * transform * affineMatrix).Translation 2.0f false Color.Yellow world
            | (_, _) -> ()
        | _ -> ()

[<AutoOpen>]
module TerrainFacetExtensions =
    type Entity with

        member this.GetTerrainMaterialProperties world : TerrainMaterialProperties = this.Get (nameof this.TerrainMaterialProperties) world
        member this.SetTerrainMaterialProperties (value : TerrainMaterialProperties) world = this.Set (nameof this.TerrainMaterialProperties) value world
        member this.TerrainMaterialProperties = lens (nameof this.TerrainMaterialProperties) this this.GetTerrainMaterialProperties this.SetTerrainMaterialProperties
        member this.GetTerrainMaterial world : TerrainMaterial = this.Get (nameof this.TerrainMaterial) world
        member this.SetTerrainMaterial (value : TerrainMaterial) world = this.Set (nameof this.TerrainMaterial) value world
        member this.TerrainMaterial = lens (nameof this.TerrainMaterial) this this.GetTerrainMaterial this.SetTerrainMaterial
        member this.GetTintImageOpt world : Image AssetTag option = this.Get (nameof this.TintImageOpt) world
        member this.SetTintImageOpt (value : Image AssetTag option) world = this.Set (nameof this.TintImageOpt) value world
        member this.TintImageOpt = lens (nameof this.TintImageOpt) this this.GetTintImageOpt this.SetTintImageOpt
        member this.GetNormalImageOpt world : Image AssetTag option = this.Get (nameof this.NormalImageOpt) world
        member this.SetNormalImageOpt (value : Image AssetTag option) world = this.Set (nameof this.NormalImageOpt) value world
        member this.NormalImageOpt = lens (nameof this.NormalImageOpt) this this.GetNormalImageOpt this.SetNormalImageOpt
        member this.GetTiles world : Vector2 = this.Get (nameof this.Tiles) world
        member this.SetTiles (value : Vector2) world = this.Set (nameof this.Tiles) value world
        member this.Tiles = lens (nameof this.Tiles) this this.GetTiles this.SetTiles
        member this.GetHeightMap world : HeightMap = this.Get (nameof this.HeightMap) world
        member this.SetHeightMap (value : HeightMap) world = this.Set (nameof this.HeightMap) value world
        member this.HeightMap = lens (nameof this.HeightMap) this this.GetHeightMap this.SetHeightMap
        member this.GetPatches world : Vector2i = this.Get (nameof this.Patches) world
        member this.SetPatches (value : Vector2i) world = this.Set (nameof this.Patches) value world
        member this.Patches = lens (nameof this.Patches) this this.GetPatches this.SetPatches

        /// Attempt to get the resolution of the terrain.
        member this.TryGetTerrainResolution world =
            match this.GetHeightMap world with
            | ImageHeightMap map ->
                match Metadata.tryGetTextureSize map with
                | ValueSome textureSize -> Some textureSize
                | ValueNone -> None
            | RawHeightMap map -> Some map.Resolution

        /// Attempt to get the size of each terrain quad.
        member this.TryGetTerrainQuadSize world =
            let bounds = this.GetBounds world
            match this.TryGetTerrainResolution world with
            | Some resolution -> Some (v2 (bounds.Size.X / single (dec resolution.X)) (bounds.Size.Z / single (dec resolution.Y)))
            | None -> None

/// Augments an entity with a rigid 3d terrain.
type TerrainFacet () =
    inherit Facet (true, false, false)

    static member Properties =
        [define Entity.Size (v3 512.0f 128.0f 512.0f)
         define Entity.Presence Omnipresent
         define Entity.Static true
         define Entity.AlwaysRender true
         define Entity.BodyEnabled true
         define Entity.Friction Constants.Physics.FrictionDefault
         define Entity.Restitution 0.0f
         define Entity.CollisionDetection Discrete
         define Entity.CollisionCategories "1"
         define Entity.CollisionMask Constants.Physics.CollisionWildcard
         define Entity.InsetOpt None
         define Entity.TerrainMaterialProperties TerrainMaterialProperties.defaultProperties
         define Entity.TerrainMaterial
            (BlendMaterial
                { TerrainLayers =
                    [|{ AlbedoImage = Assets.Default.TerrainLayer0Albedo
                        RoughnessImage = Assets.Default.TerrainLayer0Roughness
                        AmbientOcclusionImage = Assets.Default.TerrainLayer0AmbientOcclusion
                        NormalImage = Assets.Default.TerrainLayer0Normal
                        HeightImage = Assets.Default.TerrainLayer0Height }
                      { AlbedoImage = Assets.Default.TerrainLayer1Albedo
                        RoughnessImage = Assets.Default.TerrainLayer1Roughness
                        AmbientOcclusionImage = Assets.Default.TerrainLayer1AmbientOcclusion
                        NormalImage = Assets.Default.TerrainLayer1Normal
                        HeightImage = Assets.Default.TerrainLayer1Height }|]
                  BlendMap =
                      RedsMap
                        [|Assets.Default.TerrainLayer0Blend
                          Assets.Default.TerrainLayer1Blend|]})
         define Entity.TintImageOpt None
         define Entity.NormalImageOpt None
         define Entity.Tiles (v2 256.0f 256.0f)
         define Entity.HeightMap (RawHeightMap { Resolution = v2i 513 513; RawFormat = RawUInt16 LittleEndian; RawAsset = Assets.Default.HeightMap })
         define Entity.Patches (v2i 2 2) // NOTE: terrain patches don't appear to be a great optimization nowadays.
         nonPersistent Entity.AwakeTimeStamp 0L
         computed Entity.Awake (fun (entity : Entity) world -> entity.GetAwakeTimeStamp world = world.UpdateTime) None
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

    override this.Register (entity, world) =
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.HeightMap)) entity (nameof TerrainFacet) world
        World.sense (fun _ world -> entity.PropagatePhysics world; Cascade) (entity.ChangeEvent (nameof entity.Patches)) entity (nameof TerrainFacet) world
        entity.SetAwakeTimeStamp world.UpdateTime world

    override this.RegisterPhysics (entity, world) =
        match entity.TryGetTerrainResolution world with
        | Some resolution ->
            let bodyId = entity.GetBodyId world
            let mutable transform = entity.GetTransform world
            let terrainShape =
                { Resolution = resolution
                  Bounds = transform.Bounds3d
                  HeightMap = entity.GetHeightMap world
                  TransformOpt = None
                  PropertiesOpt = None }
            let bodyProperties =
                { Enabled = entity.GetBodyEnabled world
                  Center = if entity.GetIs2d world then transform.PerimeterCenter else transform.Position
                  Rotation = transform.Rotation
                  Scale = transform.Scale
                  BodyShape = TerrainShape terrainShape
                  BodyType = Static
                  SleepingAllowed = true
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  LinearVelocity = v3Zero
                  LinearDamping = 0.0f
                  AngularVelocity = v3Zero
                  AngularDamping = 0.0f
                  AngularFactor = v3Zero
                  Substance = Mass 0.0f
                  GravityOverride = None
                  CharacterProperties = CharacterProperties.defaultProperties
                  VehicleProperties = VehiclePropertiesAbsent
                  CollisionDetection = entity.GetCollisionDetection world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Sensor = false
                  Awake = entity.GetAwake world
                  BodyIndex = bodyId.BodyIndex }
            World.createBody3d bodyId bodyProperties world
        | None -> ()

    override this.UnregisterPhysics (entity, world) =
        World.destroyBody3d (entity.GetBodyId world) world

    override this.Render (renderPass, entity, world) =
        let mutable transform = entity.GetTransform world
        let castShadow = (World.getRenderer3dConfig world).LightShadowingEnabled && transform.CastShadow
        if not renderPass.IsShadowPass || castShadow then
            let terrainDescriptor =
                { Bounds = transform.Bounds3d
                  CastShadow = castShadow
                  InsetOpt = entity.GetInsetOpt world
                  MaterialProperties = entity.GetTerrainMaterialProperties world
                  Material = entity.GetTerrainMaterial world
                  TintImageOpt = entity.GetTintImageOpt world
                  NormalImageOpt = entity.GetNormalImageOpt world
                  Tiles = entity.GetTiles world
                  HeightMap = entity.GetHeightMap world
                  Patches = entity.GetPatches world }
            World.enqueueRenderMessage3d
                (RenderTerrain
                    { Visible = transform.Visible
                      TerrainDescriptor = terrainDescriptor
                      RenderPass = renderPass })
                world

    override this.GetAttributesInferred (entity, world) =
        match entity.TryGetTerrainResolution world with
        | Some resolution -> AttributesInferred.important (v3 (single (dec resolution.X)) 128.0f (single (dec resolution.Y))) v3Zero
        | None -> AttributesInferred.important (v3 512.0f 128.0f 512.0f) v3Zero

    override this.RayCast (_, _, _) =
        [|Miss|]

/// Enables common operations on 3D entities that intersect this entity's bounds.
/// TODO: P1: implement EditAreaFacet for 2D entities.
type EditVolumeFacet () =
    inherit Facet (false, false, false)

    /// Check whether an entity can be frozen by an ancestor with a FreezerFacet.
    static let rec getEntityParentable (entity : Entity) parent world =
        let presence = entity.GetPresence world
        entity <> parent &&
        not (entity.GetProtected world) &&
        not presence.IsOmnipresent &&
        (entity.GetChildren world |> Seq.forall (fun child -> getEntityParentable child parent world))

    static let getIntersectedEntities (entity : Entity) world =
        let bounds = entity.GetBounds world
        World.getEntities3dInBounds bounds (hashSetPlus HashIdentity.Structural []) world
        |> Seq.filter (fun intersected -> getEntityParentable intersected entity world)
        |> Seq.toArray
        |> Array.sortBy _.Names.Length

    override this.Edit (op, entity, world) =

        match op with
        | AppendProperties append ->

            // category and indentation
            ImGui.Text "Volume Operations"
            ImGui.Indent ()

            // parent intersected
            if ImGui.Button "Parent Intersected" then
                append.EditContext.Snapshot (VolumeEdit "Parent Intersected") world
                let intersecteds = getIntersectedEntities entity world
                for intersected in intersecteds do
                    if intersected.GetExists world then
                        let intersected' =
                            if intersected.Has<StaticModelSurfaceFacet> world && intersected.Name.StartsWith "Geometry"
                            then entity / intersected.Parent.Name // probably generic geometry imported from another engine's scene, so using a likely more descriptive parent name
                            else entity / intersected.Name
                        let intersected' =
                            if intersected'.GetExists world
                            then entity / (intersected'.Name + Gen.name)
                            else intersected'
                        World.renameEntityImmediate intersected intersected' world

            // unparent intersected
            if ImGui.Button "Unparent Intersected" then
                append.EditContext.Snapshot (VolumeEdit "Unparent Intersected") world
                let bounds = entity.GetBounds world
                let children =
                    entity.GetChildren world
                    |> Seq.filter (fun child -> bounds.Intersects (child.GetBounds world))
                    |> Array.ofSeq
                for child in children do
                    if child.GetExists world then
                        let child' = child.Names |> Array.take (entity.Names.Length - 1) |> Array.add child.Name |> rtoa |> Nu.Entity
                        let child' =
                            if child'.GetExists world
                            then entity / (child'.Name + Gen.name)
                            else child'
                        World.renameEntityImmediate child child' world

            // delete intersected
            if ImGui.Button "Delete Intersected" then
                append.EditContext.Snapshot (VolumeEdit "Delete Intersected") world
                let intersecteds = getIntersectedEntities entity world
                for intersected in intersecteds do
                    if intersected.GetExists world then
                        World.destroyEntity intersected world

            // end of category
            ImGui.Unindent ()

        | ViewportOverlay viewportOverlay ->
            if entity.GetOffset world = v3Zero then
                if world.DateTime.Millisecond < 500 then
                    for intersected in getIntersectedEntities entity world do
                        let bounds = intersected.GetBounds world
                        World.imGuiCircle3d bounds.Center 5.0f false Color.Orange world
                let (manipulationResult, bounds) =
                    World.imGuiEditBox3d viewportOverlay.EditContext.SnapDrag (entity.GetBounds world) world
                match manipulationResult with
                | ImGuiEditActive started ->
                    if started then viewportOverlay.EditContext.Snapshot (ChangeProperty (None, nameof Entity.Bounds)) world
                    match entity.TryGetMountee world with
                    | Some mountee ->
                        let positionMountee = mountee.GetPosition world
                        entity.SetPositionLocal (positionMountee - bounds.Center) world
                        entity.SetSize (bounds.Size / entity.GetScale world) world
                    | None ->
                        entity.SetPosition bounds.Center world
                        entity.SetSize (bounds.Size / entity.GetScale world) world
                | ImGuiEditInactive -> ()
            else
                // TODO: P1: see if we can implement this properly instead of schmoing out.
                Log.warnOnce "Bounds adjustment currently not implemented for entities with non-zero offset."

        | _ -> ()

    override this.RayCast (_, _, _) =
        [|Miss|]

[<AutoOpen>]
module TraversalInterpolatedFacetExtensions =
    type Entity with
        member this.GetPositionHistory world : Vector3 FQueue = this.Get (nameof this.PositionHistory) world
        member this.SetPositionHistory (value : Vector3 FQueue) world = this.Set (nameof this.PositionHistory) value world
        member this.PositionHistory = lens (nameof this.PositionHistory) this this.GetPositionHistory this.SetPositionHistory
        member this.GetRotationHistory world : Quaternion FQueue = this.Get (nameof this.RotationHistory) world
        member this.SetRotationHistory (value : Quaternion FQueue) world = this.Set (nameof this.RotationHistory) value world
        member this.RotationHistory = lens (nameof this.RotationHistory) this this.GetRotationHistory this.SetRotationHistory
        member this.GetLinearVelocityHistory world : Vector3 FQueue = this.Get (nameof this.LinearVelocityHistory) world
        member this.SetLinearVelocityHistory (value : Vector3 FQueue) world = this.Set (nameof this.LinearVelocityHistory) value world
        member this.LinearVelocityHistory = lens (nameof this.LinearVelocityHistory) this this.GetLinearVelocityHistory this.SetLinearVelocityHistory
        member this.GetAngularVelocityHistory world : Vector3 FQueue = this.Get (nameof this.AngularVelocityHistory) world
        member this.SetAngularVelocityHistory (value : Vector3 FQueue) world = this.Set (nameof this.AngularVelocityHistory) value world
        member this.AngularVelocityHistory = lens (nameof this.AngularVelocityHistory) this this.GetAngularVelocityHistory this.SetAngularVelocityHistory
        member this.GetTraversalHistoryMax world : int = this.Get (nameof this.TraversalHistoryMax) world
        member this.SetTraversalHistoryMax (value : int) world = this.Set (nameof this.TraversalHistoryMax) value world
        member this.TraversalHistoryMax = lens (nameof this.TraversalHistoryMax) this this.GetTraversalHistoryMax this.SetTraversalHistoryMax

        member this.GetPositionInterpolated world =
            let position = this.GetPosition world
            let positionHistory = this.GetPositionHistory world
            if FQueue.notEmpty positionHistory then
                let positions = FQueue.conj position positionHistory
                Seq.sum positions / single positions.Length
            else position

        member this.GetRotationInterpolated world =
            let rotation = this.GetRotation world
            let rotationHistory = this.GetRotationHistory world
            if FQueue.notEmpty rotationHistory then
                let rotations = FQueue.conj rotation rotationHistory
                if rotations.Length > 1 then
                    let unnormalized = Quaternion.Slerp (Seq.head rotations, Seq.last rotations, 0.5f) // HACK: we just interpolate the first and last rotations...
                    unnormalized.Normalized
                else rotation
            else rotation

        member this.GetLinearVelocityInterpolated world =
            let linearVelocity = this.GetLinearVelocity world
            let linearVelocityHistory = this.GetLinearVelocityHistory world
            if FQueue.notEmpty linearVelocityHistory then
                let linearVelocities = FQueue.conj linearVelocity linearVelocityHistory
                Seq.sum linearVelocities / single linearVelocities.Length
            else linearVelocity

        member this.GetAngularVelocityInterpolated world =
            let angularVelocity = this.GetAngularVelocity world
            let angularVelocityHistory = this.GetAngularVelocityHistory world
            if FQueue.notEmpty angularVelocityHistory then
                let angularVelocities = FQueue.conj angularVelocity angularVelocityHistory
                Seq.sum angularVelocities / single angularVelocities.Length
            else angularVelocity
            
/// Tracks interpolated values typically used for traversal.
/// TODO: P1: make this GameTime-based rather than frame-based!
type TraversalInterpolatedFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [nonPersistent Entity.PositionHistory FQueue.empty
         nonPersistent Entity.RotationHistory FQueue.empty
         nonPersistent Entity.LinearVelocityHistory FQueue.empty
         nonPersistent Entity.AngularVelocityHistory FQueue.empty
         define Entity.TraversalHistoryMax 4]

    override this.Update (entity, world) =

        // process history for the frame
        let historyMax = entity.GetTraversalHistoryMax world
        entity.PositionHistory.Map (fun history -> (if history.Length >= historyMax then FQueue.tail history else history) |> FQueue.conj (entity.GetPosition world)) world
        entity.RotationHistory.Map (fun history -> (if history.Length >= historyMax then FQueue.tail history else history) |> FQueue.conj (entity.GetRotation world)) world
        entity.LinearVelocityHistory.Map (fun history -> (if history.Length >= historyMax then FQueue.tail history else history) |> FQueue.conj (entity.GetLinearVelocity world)) world
        entity.AngularVelocityHistory.Map (fun history -> (if history.Length >= historyMax then FQueue.tail history else history) |> FQueue.conj (entity.GetAngularVelocity world)) world

    override this.Edit (op, entity, world) =

        // ensure position history isn't stale when editing
        match op with
        | ViewportOverlay _ when world.Halted ->
            let position = entity.GetPosition world
            let positionHistory = FQueue.singleton position
            entity.SetPositionHistory positionHistory world
        | _ -> ()

[<AutoOpen>]
module NavBodyFacetExtensions =
    type Entity with
        member this.GetNavShape world : NavShape = this.Get (nameof this.NavShape) world
        member this.SetNavShape (value : NavShape) world = this.Set (nameof this.NavShape) value world
        member this.NavShape = lens (nameof this.NavShape) this this.GetNavShape this.SetNavShape
        member this.GetNavEnabled world : bool = this.Get (nameof this.NavEnabled) world
        member this.SetNavEnabled (value : bool) world = this.Set (nameof this.NavEnabled) value world
        member this.NavEnabled = lens (nameof this.NavEnabled) this this.GetNavEnabled this.SetNavEnabled

/// Augments an entity with a 3d navigation body.
type NavBodyFacet () =
    inherit Facet (false, false, false)

    static let propagateNavBody (entity : Entity) world =
        let navId = { NavIndex = -1; NavEntity = entity }
        match entity.GetNavShape world with
        | NavShape.EmptyNavShape ->
            if entity.GetIs2d world
            then () // TODO: implement for 2d navigation when it's available.
            else World.setNav3dBodyOpt None navId world
        | shape ->
            if entity.GetIs2d world
            then () // TODO: implement for 2d navigation when it's available.
            else
                if entity.GetNavEnabled world then
                    let bounds = entity.GetBounds world
                    let affineMatrix = entity.GetAffineMatrix world
                    let staticModel = entity.GetStaticModel world
                    let surfaceIndex = entity.GetSurfaceIndex world
                    World.setNav3dBodyOpt (Some (bounds, affineMatrix, staticModel, surfaceIndex, shape)) navId world
                else World.setNav3dBodyOpt None navId world

    static member Properties =
        [define Entity.StaticModel Assets.Default.StaticModel
         define Entity.SurfaceIndex 0
         define Entity.NavShape BoundsNavShape
         define Entity.NavEnabled true]

    override this.Register (entity, world) =

        // OPTIMIZATION: conditionally subscribe to bounds change event.
        let subId = Gen.id64
        let subscribe world =
            World.subscribePlus subId (fun _ world -> propagateNavBody entity world; Cascade) (entity.ChangeEvent (nameof entity.Bounds)) entity world |> ignore
        let unsubscribe world =
            World.unsubscribe subId world
        let callback evt world =
            let entity = evt.Subscriber : Entity
            let previous = evt.Data.Previous :?> NavShape
            let value = evt.Data.Value :?> NavShape
            let navEnabled = entity.GetNavEnabled world
            if not previous.IsEmptyNavShape || navEnabled then unsubscribe world
            if not value.IsEmptyNavShape || navEnabled then subscribe world
            propagateNavBody entity world
            Cascade
        let callback2 evt world =
            let entity = evt.Subscriber : Entity
            let previous = evt.Data.Previous :?> bool
            let value = evt.Data.Value :?> bool
            let navShape = entity.GetNavShape world
            if not navShape.IsEmptyNavShape || previous then unsubscribe world
            if not navShape.IsEmptyNavShape || value then subscribe world
            propagateNavBody entity world
            Cascade
        let callback3 evt world =
            if  Set.contains (nameof NavBodyFacet) (evt.Data.Previous :?> string Set) &&
                Set.contains (nameof NavBodyFacet) (evt.Data.Value :?> string Set) |> not then
                unsubscribe world
            Cascade
        let callback4 _ world = unsubscribe world; Cascade
        match entity.GetNavShape world with
        | NavShape.EmptyNavShape -> ()
        | _ -> subscribe world
        World.sense callback (entity.ChangeEvent (nameof entity.NavShape)) entity (nameof NavBodyFacet) world
        World.sense callback2 (entity.ChangeEvent (nameof entity.NavEnabled)) entity (nameof NavBodyFacet) world
        World.sense callback3 entity.FacetNames.ChangeEvent entity (nameof NavBodyFacet) world
        World.sense callback4 entity.UnregisteringEvent entity (nameof NavBodyFacet) world

        // unconditional registration behavior
        let callbackPnb evt world = propagateNavBody evt.Subscriber world; Cascade
        World.sense callbackPnb (entity.ChangeEvent (nameof entity.StaticModel)) entity (nameof NavBodyFacet) world
        World.sense callbackPnb (entity.ChangeEvent (nameof entity.SurfaceIndex)) entity (nameof NavBodyFacet) world
        propagateNavBody entity world

    override this.Unregister (entity, world) =
        let navId = { NavIndex = -1; NavEntity = entity }
        if entity.GetIs2d world
        then () // TODO: implement for 2d navigation when it's available.
        else World.setNav3dBodyOpt None navId world

    override this.GetAttributesInferred (_, _) =
        AttributesInferred.unimportant

[<AutoOpen>]
module FollowerFacetExtensions =
    type Entity with
        member this.GetFollowing world : bool = this.Get (nameof this.Following) world
        member this.SetFollowing (value : bool) world = this.Set (nameof this.Following) value world
        member this.Following = lens (nameof this.Following) this this.GetFollowing this.SetFollowing
        member this.GetFollowMoveSpeed world : single = this.Get (nameof this.FollowMoveSpeed) world
        member this.SetFollowMoveSpeed (value : single) world = this.Set (nameof this.FollowMoveSpeed) value world
        member this.FollowMoveSpeed = lens (nameof this.FollowMoveSpeed) this this.GetFollowMoveSpeed this.SetFollowMoveSpeed
        member this.GetFollowTurnSpeed world : single = this.Get (nameof this.FollowTurnSpeed) world
        member this.SetFollowTurnSpeed (value : single) world = this.Set (nameof this.FollowTurnSpeed) value world
        member this.FollowTurnSpeed = lens (nameof this.FollowTurnSpeed) this this.GetFollowTurnSpeed this.SetFollowTurnSpeed
        member this.GetFollowDistanceMinOpt world : single option = this.Get (nameof this.FollowDistanceMinOpt) world
        member this.SetFollowDistanceMinOpt (value : single option) world = this.Set (nameof this.FollowDistanceMinOpt) value world
        member this.FollowDistanceMinOpt = lens (nameof this.FollowDistanceMinOpt) this this.GetFollowDistanceMinOpt this.SetFollowDistanceMinOpt
        member this.GetFollowDistanceMaxOpt world : single option = this.Get (nameof this.FollowDistanceMaxOpt) world
        member this.SetFollowDistanceMaxOpt (value : single option) world = this.Set (nameof this.FollowDistanceMaxOpt) value world
        member this.FollowDistanceMaxOpt = lens (nameof this.FollowDistanceMaxOpt) this this.GetFollowDistanceMaxOpt this.SetFollowDistanceMaxOpt
        member this.GetFollowTargetOpt world : Entity option = this.Get (nameof this.FollowTargetOpt) world
        member this.SetFollowTargetOpt (value : Entity option) world = this.Set (nameof this.FollowTargetOpt) value world
        member this.FollowTargetOpt = lens (nameof this.FollowTargetOpt) this this.GetFollowTargetOpt this.SetFollowTargetOpt

/// Enables an entity to follow another entity (currently for 3D entities only).
type FollowerFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.Following true
         define Entity.FollowMoveSpeed 1.0f
         define Entity.FollowTurnSpeed 3.0f
         define Entity.FollowDistanceMinOpt None
         define Entity.FollowDistanceMaxOpt None
         define Entity.FollowTargetOpt None]

    override this.Update (entity, world) =
        if entity.GetFollowing world then
            let targetOpt = entity.GetFollowTargetOpt world
            match targetOpt with
            | Some target when target.GetExists world ->
                let moveSpeed = entity.GetFollowMoveSpeed world
                let turnSpeed = entity.GetFollowTurnSpeed world
                let distanceMinOpt = entity.GetFollowDistanceMinOpt world
                let distanceMaxOpt = entity.GetFollowDistanceMaxOpt world
                let position = entity.GetPosition world
                let destination = target.GetPosition world
                let distance = (destination - position).Magnitude
                let rotation = entity.GetRotation world
                if  (distanceMinOpt.IsNone || distance > distanceMinOpt.Value) &&
                    (distanceMaxOpt.IsNone || distance <= distanceMaxOpt.Value) then
                    if entity.GetIs2d world then
                        // TODO: implement for 2d navigation when it's available.
                        ()
                    else
                        // TODO: consider doing an offset physics ray cast to align nav position with near
                        // ground. Additionally, consider removing the CellHeight offset in the above query so
                        // that we don't need to do an offset here at all.
                        let followOutput = World.nav3dFollow distanceMinOpt distanceMaxOpt moveSpeed turnSpeed position rotation destination entity.Screen world
                        let hasLinearVelocity =
                            match entity.TryGetProperty (nameof entity.LinearVelocity) world with
                            | Some property -> property.PropertyType = typeof<Vector3>
                            | None -> false
                        let hasAngularVelocity =
                            match entity.TryGetProperty (nameof entity.AngularVelocity) world with
                            | Some property -> property.PropertyType = typeof<Vector3>
                            | None -> false
                        if hasLinearVelocity && hasAngularVelocity then
                            entity.SetLinearVelocity followOutput.NavLinearVelocity world
                            entity.SetAngularVelocity followOutput.NavAngularVelocity world
                            entity.SetRotation followOutput.NavRotation world
                        else
                            entity.SetPosition followOutput.NavPosition world
                            entity.SetRotation followOutput.NavRotation world
            | _ -> ()