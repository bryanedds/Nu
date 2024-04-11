// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

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

    /// The global Game simulant.
    let Game = Game.Handle

    /// Declaratively exposes Screen lenses and events.
    let Screen = Unchecked.defaultof<Screen>

    /// Declaratively exposes Group lenses and events.
    let Group = Unchecked.defaultof<Group>

    /// Declaratively exposes Entity lenses and events.
    let Entity = Unchecked.defaultof<Entity>

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with
        member this.GetInsetOpt world : Box2 option = this.Get (nameof this.InsetOpt) world
        member this.SetInsetOpt (value : Box2 option) world = this.Set (nameof this.InsetOpt) value world
        member this.InsetOpt = lens (nameof this.InsetOpt) this this.GetInsetOpt this.SetInsetOpt
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
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.StaticImage Assets.Default.Box
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let staticImage = entity.GetStaticImage world
            let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
            let color = entity.GetColor world
            let blend = entity.GetBlend world
            let emission = entity.GetEmission world
            let flip = entity.GetFlip world
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, staticImage, &color, blend, &emission, flip, world)

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySize2dDefault v3Zero

[<AutoOpen>]
module AnimatedSpriteFacetModule =

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
        inherit Facet (false)

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
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let animationSheet = entity.GetAnimationSheet world
            let insetOpt = match getSpriteInsetOpt entity world with Some inset -> ValueSome inset | None -> ValueNone
            let color = entity.GetColor world
            let blend = entity.GetBlend world
            let emission = entity.GetEmission world
            let flip = entity.GetFlip world
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, animationSheet, &transform, &insetOpt, animationSheet, &color, blend, &emission, flip, world)

        override this.GetAttributesInferred (entity, world) =
            AttributesInferred.important (entity.GetCelSize world).V3 v3Zero

[<AutoOpen>]
module BasicStaticSpriteEmitterFacetModule =

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
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) (world : World) =
            World.tryMakeEmitter
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)
                (entity.GetEmitterStyle world)
                world |>
            Option.map cast<Particles.BasicStaticSpriteEmitter>

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
            let world = entity.SetParticleSystem particleSystem world
            world

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
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterBlendChange evt world =
            let emitterBlend = evt.Data.Value :?> Blend
            let world = mapEmitter (fun emitter -> if emitter.Blend <> emitterBlend then { emitter with Blend = emitterBlend } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterImageChange evt world =
            let emitterImage = evt.Data.Value :?> Image AssetTag
            let world = mapEmitter (fun emitter -> if assetNeq emitter.Image emitterImage then { emitter with Image = emitterImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = mapEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = mapEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = mapEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = mapEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticSpriteEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = mapEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = mapEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = mapEmitter (constant emitter) entity world
            (Cascade, world)

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
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

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
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.EmitterBlend Transparent
             define Entity.EmitterImage Assets.Default.Image
             define Entity.EmitterLifeTimeOpt GameTime.zero
             define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
             define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = Constants.Engine.ParticleSize2dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicStaticSpriteEmitter"
             nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.sense handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterBlendChange (entity.GetChangeEvent (nameof entity.EmitterBlend)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterImageChange (entity.GetChangeEvent (nameof entity.EmitterImage)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity (nameof BasicStaticSpriteEmitterFacet) world
            world

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
                let world = entity.SetParticleSystem particleSystem world
                processOutput output entity world
            else world

        override this.Render (_, entity, world) =
            let time = world.GameTime
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                Particles.ParticleSystem.toParticlesDescriptors time |>
                List.map (fun descriptor ->
                    match descriptor with
                    | Particles.SpriteParticlesDescriptor descriptor ->
                        Some
                            { Elevation = descriptor.Elevation
                              Horizon = descriptor.Horizon
                              AssetTag = descriptor.Image
                              RenderOperation2d = RenderSpriteParticles descriptor }
                    | _ -> None) |>
                List.definitize
            World.enqueueLayeredOperations2d particlesMessages world

[<AutoOpen>]
module TextFacetModule =

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
        member this.GetTextDisabledColor world : Color = this.Get (nameof this.TextDisabledColor) world
        member this.SetTextDisabledColor (value : Color) world = this.Set (nameof this.TextDisabledColor) value world
        member this.TextDisabledColor = lens (nameof this.TextDisabledColor) this this.GetTextDisabledColor this.SetTextDisabledColor
        member this.GetTextOffset world : Vector2 = this.Get (nameof this.TextOffset) world
        member this.SetTextOffset (value : Vector2) world = this.Set (nameof this.TextOffset) value world
        member this.TextOffset = lens (nameof this.TextOffset) this this.GetTextOffset this.SetTextOffset
        member this.GetTextShift world : single = this.Get (nameof this.TextShift) world
        member this.SetTextShift (value : single) world = this.Set (nameof this.TextShift) value world
        member this.TextShift = lens (nameof this.TextShift) this this.GetTextShift this.SetTextShift

    /// Augments an entity with text.
    type TextFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Text ""
             define Entity.Font Assets.Default.Font
             define Entity.FontSizing None
             define Entity.FontStyling Set.empty
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.TextMargin v2Zero
             define Entity.TextColor Color.Black
             define Entity.TextDisabledColor (Color (0.25f, 0.25f, 0.25f, 0.75f))
             define Entity.TextOffset v2Zero
             define Entity.TextShift 0.5f]

        override this.Render (_, entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon
                let mutable textTransform = Transform.makeDefault false // centered-ness and offset are already baked into perimeter
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3
                let shift = entity.GetTextShift world
                textTransform.Position <- perimeter.Min + margin + offset
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + shift
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world
                let fontSizing = entity.GetFontSizing world
                let fontStyling = entity.GetFontStyling world
                World.enqueueLayeredOperation2d
                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = font
                      RenderOperation2d =
                        RenderText
                            { Transform = textTransform
                              Text = text
                              Font = font
                              FontSizing = fontSizing
                              FontStyling = fontStyling
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                              Justification = entity.GetJustification world }}
                    world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module BackdroppableFacetModule =

    type Entity with
        member this.GetDisabledColor world : Color = this.Get (nameof this.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof this.DisabledColor) value world
        member this.DisabledColor = lens (nameof this.DisabledColor) this this.GetDisabledColor this.SetDisabledColor
        member this.GetBackdropImageOpt world : Image AssetTag option = this.Get (nameof this.BackdropImageOpt) world
        member this.SetBackdropImageOpt (value : Image AssetTag option) world = this.Set (nameof this.BackdropImageOpt) value world
        member this.BackdropImageOpt = lens (nameof this.BackdropImageOpt) this this.GetBackdropImageOpt this.SetBackdropImageOpt

    /// Augments an entity with optional backdrop behavior.
    type BackdroppableFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.BackdropImageOpt None]

        override this.Render (_, entity, world) =
            match entity.GetBackdropImageOpt world with
            | Some spriteImage ->
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.PerimeterCentered
                World.enqueueLayeredOperation2d
                    { Elevation = spriteTransform.Elevation
                      Horizon = spriteTransform.Horizon
                      AssetTag = spriteImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = spriteTransform
                              InsetOpt = ValueNone
                              Image = spriteImage
                              Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> ()

        override this.GetAttributesInferred (entity, world) =
            match entity.GetBackdropImageOpt world with
            | Some image ->
                match Metadata.tryGetTextureSizeF image with
                | Some size -> AttributesInferred.important size.V3 v3Zero
                | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module LabelFacetModule =

    type Entity with
        member this.GetLabelImage world : Image AssetTag = this.Get (nameof this.LabelImage) world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set (nameof this.LabelImage) value world
        member this.LabelImage = lens (nameof this.LabelImage) this this.GetLabelImage this.SetLabelImage

    /// Augments an entity with label behavior.
    type LabelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.LabelImage Assets.Default.Label]

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.PerimeterCentered // gui currently ignore rotation
            let spriteImage = entity.GetLabelImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetLabelImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module ButtonFacetModule =

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
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled then
                        let world = entity.SetDown true world
                        let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) (entity.GetDownOffset world) world
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus () entity.DownEvent eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasDown = entity.GetDown world
            let world = entity.SetDown false world
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) v2Zero world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled && wasDown then
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publishPlus () entity.UpEvent eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publishPlus () entity.ClickEvent eventTrace entity true false world
                        let world =
                            match entity.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.Down false
             define Entity.DownOffset v2Zero
             define Entity.UpImage Assets.Default.ButtonUp
             define Entity.DownImage Assets.Default.ButtonDown
             define Entity.ClickSoundOpt (Some Assets.Default.Sound)
             define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ButtonFacet) world
            world

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.PerimeterCentered // gui currently ignore rotation
            let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module ToggleButtonFacetModule =

    type Entity with
        member this.GetToggled world : bool = this.Get (nameof this.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof this.Toggled) value world
        member this.Toggled = lens (nameof this.Toggled) this this.GetToggled this.SetToggled
        member this.GetToggledOffset world : Vector2 = this.Get (nameof this.ToggledOffset) world
        member this.SetToggledOffset (value : Vector2) world = this.Set (nameof this.ToggledOffset) value world
        member this.ToggledOffset = lens (nameof this.ToggledOffset) this this.GetToggledOffset this.SetToggledOffset
        member this.GetPressed world : bool = this.Get (nameof this.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof this.Pressed) value world
        member this.Pressed = lens (nameof this.Pressed) this this.GetPressed this.SetPressed
        member this.GetPressedOffset world : Vector2 = this.Get (nameof this.PressedOffset) world
        member this.SetPressedOffset (value : Vector2) world = this.Set (nameof this.PressedOffset) value world
        member this.PressedOffset = lens (nameof this.PressedOffset) this this.GetPressedOffset this.SetPressedOffset
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
        inherit Facet (false)
        
        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled && wasPressed then
                        let world = entity.SetToggled (not (entity.GetToggled world)) world
                        let toggled = entity.GetToggled world
                        let eventAddress = if toggled then entity.ToggledEvent else entity.UntoggledEvent
                        let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () eventAddress eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publishPlus toggled entity.ToggleEvent eventTrace entity true false world
                        let world =
                            match entity.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound (entity.GetToggleSoundVolume world) toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.Toggled false
             define Entity.ToggledOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedOffset v2Zero
             define Entity.UntoggledImage Assets.Default.ButtonUp
             define Entity.ToggledImage Assets.Default.ButtonDown
             define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
             define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ToggleButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ToggleButtonFacet) world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedOffset world
                elif entity.GetToggled world then entity.GetToggledOffset world
                else v2Zero
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) textOffset world
            world

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.PerimeterCentered // gui currently ignores rotation
            let spriteImage =
                if entity.GetToggled world || entity.GetPressed world
                then entity.GetToggledImage world
                else entity.GetUntoggledImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUntoggledImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module RadioButtonFacetModule =

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
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            let wasDialed = entity.GetDialed world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled && wasPressed && not wasDialed then
                        let world = entity.SetDialed true world
                        let dialed = entity.GetDialed world
                        let eventAddress = if dialed then entity.DialedEvent else entity.UndialedEvent
                        let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () eventAddress eventTrace entity true false world
                        let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "Dial" EventTrace.empty
                        let world = World.publishPlus dialed entity.DialEvent eventTrace entity true false world
                        let world =
                            match entity.GetDialSoundOpt world with
                            | Some dialSound -> World.playSound (entity.GetDialSoundVolume world) dialSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.Dialed false
             define Entity.DialedOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedOffset v2Zero
             define Entity.UndialedImage Assets.Default.ButtonUp
             define Entity.DialedImage Assets.Default.ButtonDown
             define Entity.DialSoundOpt (Some Assets.Default.Sound)
             define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof RadioButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof RadioButtonFacet) world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedOffset world
                elif entity.GetDialed world then entity.GetDialedOffset world
                else v2Zero
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) textOffset world
            world

        override this.Render (_, entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.PerimeterCentered // gui currently ignores rotation
            let spriteImage =
                if entity.GetDialed world || entity.GetPressed world
                then entity.GetDialedImage world
                else entity.GetUndialedImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module FillBarFacetModule =

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
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor Constants.Gui.DisabledColor
             define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.White
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Border]

        override this.Render (_, entity, world) =

            // border sprite
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter // gui currently ignores rotation
            let horizon = transform.Horizon
            let mutable borderTransform = Transform.makeDefault transform.PerimeterCentered
            borderTransform.Position <- perimeter.Min
            borderTransform.Size <- perimeter.Size
            borderTransform.Offset <- transform.Offset
            borderTransform.Elevation <- transform.Elevation + 0.5f
            borderTransform.Absolute <- transform.Absolute
            let color = if transform.Enabled then Color.White else entity.GetDisabledColor world
            let borderImageColor = entity.GetBorderColor world * color
            let borderImage = entity.GetBorderImage world
            World.enqueueLayeredOperation2d
                { Elevation = borderTransform.Elevation
                  Horizon = horizon
                  AssetTag = borderImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = borderTransform
                          InsetOpt = ValueNone
                          Image = borderImage
                          Color = borderImageColor
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

            // fill sprite
            let fillSize = perimeter.Size
            let fillInset = fillSize.X * entity.GetFillInset world * 0.5f
            let fillPosition = perimeter.Min + v3 fillInset fillInset 0.0f
            let fillWidth = (fillSize.X - fillInset * 2.0f) * entity.GetFill world
            let fillHeight = fillSize.Y - fillInset * 2.0f
            let fillSize = v3 fillWidth fillHeight 0.0f
            let mutable fillTransform = Transform.makeDefault transform.PerimeterCentered
            fillTransform.Position <- fillPosition
            fillTransform.Size <- fillSize
            fillTransform.Offset <- transform.Offset
            fillTransform.Elevation <- transform.Elevation
            fillTransform.Absolute <- transform.Absolute
            let fillImageColor = entity.GetFillColor world * color
            let fillImage = entity.GetFillImage world
            World.enqueueLayeredOperation2d
                { Elevation = fillTransform.Elevation
                  Horizon = horizon
                  AssetTag = fillImage
                  RenderOperation2d =
                      RenderSprite
                          { Transform = fillTransform
                            InsetOpt = ValueNone
                            Image = fillImage
                            Color = fillImageColor
                            Blend = Transparent
                            Emission = Color.Zero
                            Flip = FlipNone }}
                world

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
            | Some size -> AttributesInferred.important size.V3 v3Zero
            | None -> AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module FeelerFacetModule =

    type Entity with
        member this.GetTouched world : bool = this.Get (nameof this.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof this.Touched) value world
        member this.Touched = lens (nameof this.Touched) this this.GetTouched this.SetTouched
        member this.TouchEvent = Events.TouchEvent --> this
        member this.TouchingEvent = Events.TouchingEvent --> this
        member this.UntouchEvent = Events.UntouchEvent --> this

    /// Augments an entity with feeler behavior.
    type FeelerFacet () =
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2 // gui currently ignores rotation
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then
                    if transform.Enabled then
                        let world = entity.SetTouched true world
                        let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus data.Position entity.TouchEvent eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasTouched = entity.GetTouched world
            let world = entity.SetTouched false world
            if entity.GetVisible world then
                if entity.GetEnabled world && wasTouched then
                    let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                    let world = World.publishPlus data.Position entity.UntouchEvent eventTrace entity true false world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleIncoming evt world =
            let entity = evt.Subscriber : Entity
            if  MouseState.isButtonDown MouseLeft &&
                entity.GetVisible world &&
                entity.GetEnabled world then
                let mousePosition = MouseState.getPosition ()
                let world = entity.SetTouched true world
                let eventTrace = EventTrace.debug "FeelerFacet" "handleIncoming" "" EventTrace.empty
                let world = World.publishPlus mousePosition entity.TouchEvent eventTrace entity true false world
                (Resolve, world)
            else (Cascade, world)

        static let handleOutgoing evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, entity.SetTouched false world)

        static member Properties =
            [define Entity.Touched false]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof FeelerFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof FeelerFacet) world
            let world = World.sense handleIncoming entity.Screen.IncomingFinishEvent entity (nameof FeelerFacet) world
            let world = World.sense handleOutgoing entity.Screen.OutgoingStartEvent entity (nameof FeelerFacet) world
            world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                if entity.GetTouched world then
                    let mousePosition = World.getMousePosition world
                    let eventTrace = EventTrace.debug "FeelerFacet" "Update" "" EventTrace.empty
                    let world = World.publishPlus mousePosition entity.TouchingEvent eventTrace entity true false world
                    world
                else world
            else world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important Constants.Engine.EntitySizeGuiDefault v3Zero

[<AutoOpen>]
module EffectFacetModule =

    /// The timing with which an effect should be evaluated in a frame.
    type RunMode =
        | RunEarly
        | RunLate

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
        member this.GetEffectPerimeterCentered world : bool = this.Get (nameof this.EffectPerimeterCentered) world
        member this.SetEffectPerimeterCentered (value : bool) world = this.Set (nameof this.EffectPerimeterCentered) value world
        member this.EffectPerimeterCentered = lens (nameof this.EffectPerimeterCentered) this this.GetEffectPerimeterCentered this.SetEffectPerimeterCentered
        member this.GetEffectOffset world : Vector3 = this.Get (nameof this.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof this.EffectOffset) value world
        member this.EffectOffset = lens (nameof this.EffectOffset) this this.GetEffectOffset this.SetEffectOffset
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
        inherit Facet (false)

        static let setEffect effectSymbolOpt (entity : Entity) world =
            match effectSymbolOpt with
            | Some effectSymbol ->
                let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
                match World.assetTagToValueOpt<Effects.EffectDescriptor> effectSymbol symbolLoadMetadata world with
                | Some effect -> entity.SetEffectDescriptor effect world
                | None -> world
            | None -> world

        static let run (entity : Entity) world =

            // make effect
            let effect =
                Effect.makePlus
                    (match entity.GetEffectStartTimeOpt world with Some effectStartTime -> effectStartTime | None -> GameTime.zero)
                    (entity.GetEffectPerimeterCentered world)
                    (entity.GetEffectOffset world)
                    (entity.GetTransform world)
                    (entity.GetEffectRenderType world)
                    (entity.GetParticleSystem world)
                    (entity.GetEffectHistoryMax world)
                    (entity.GetEffectHistory world)
                    (entity.GetEffectDefinitions world)
                    (entity.GetEffectDescriptor world)

            // run effect, optionally destroying upon exhaustion
            let (liveness, effect, dataToken) = Effect.run effect world
            let world = entity.SetParticleSystem effect.ParticleSystem world
            let world = entity.SetEffectTagTokens effect.TagTokens world
            let world = entity.SetEffectDataToken dataToken world
            if liveness = Dead && entity.GetSelfDestruct world
            then World.destroyEntity entity world
            else world

        static let handleEffectDescriptorChange evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunEarly -> run entity world
                    | _ -> world
                else world
            (Cascade, world)

        static let handleEffectsChange evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

        static let handlePreUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunEarly -> run entity world
                    | _ -> world
                else world
            (Cascade, world)

        static let handlePostUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunLate -> run entity world
                    | _ -> world
                else world
            (Cascade, world)

        static member Properties =
            [define Entity.ParticleSystem Particles.ParticleSystem.empty
             define Entity.SelfDestruct false
             define Entity.RunMode RunLate
             define Entity.EffectSymbolOpt None
             define Entity.EffectStartTimeOpt None
             define Entity.EffectPerimeterCentered true
             define Entity.EffectDefinitions Map.empty
             define Entity.EffectDescriptor Effects.EffectDescriptor.empty
             define Entity.EffectOffset v3Zero
             define Entity.EffectRenderType (ForwardRenderType (0.0f, 0.0f))
             define Entity.EffectHistoryMax Constants.Effects.EffectHistoryMaxDefault
             variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))
             nonPersistent Entity.EffectTagTokens Map.empty
             nonPersistent Entity.EffectDataToken DataToken.empty]

        override this.Register (entity, world) =
            let effectStartTime = Option.defaultValue world.GameTime (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.sense handleEffectDescriptorChange (entity.GetChangeEvent (nameof entity.EffectDescriptor)) entity (nameof EffectFacet) world
            let world = World.sense handleEffectsChange (entity.GetChangeEvent (nameof entity.EffectSymbolOpt)) entity (nameof EffectFacet) world
            let world = World.sense handleAssetsReload Nu.Game.Handle.AssetsReloadEvent entity (nameof EffectFacet) world
            let world = World.sense handlePreUpdate entity.Group.PreUpdateEvent entity (nameof EffectFacet) world
            let world = World.sense handlePostUpdate entity.Group.PostUpdateEvent entity (nameof EffectFacet) world
            world

        override this.Render (renderPass, entity, world) =

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
                            { Absolute = descriptor.Absolute
                              Presence = presence
                              MaterialProperties = descriptor.MaterialProperties
                              Material = descriptor.Material
                              Particles = descriptor.Particles
                              RenderType = descriptor.RenderType
                              RenderPass = renderPass }
                    World.enqueueRenderMessage3d message world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
        member this.GetBodyEnabled world : bool = this.Get (nameof this.BodyEnabled) world
        member this.SetBodyEnabled (value : bool) world = this.Set (nameof this.BodyEnabled) value world
        member this.BodyEnabled = lens (nameof this.BodyEnabled) this this.GetBodyEnabled this.SetBodyEnabled
        member this.GetBodyType world : BodyType = this.Get (nameof this.BodyType) world
        member this.SetBodyType (value : BodyType) world = this.Set (nameof this.BodyType) value world
        member this.BodyType = lens (nameof this.BodyType) this this.GetBodyType this.SetBodyType
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
        member this.GetCollisionDetection world : CollisionDetection = this.Get (nameof this.CollisionDetection) world
        member this.SetCollisionDetection (value : CollisionDetection) world = this.Set (nameof this.CollisionDetection) value world
        member this.CollisionDetection = lens (nameof this.CollisionDetection) this this.GetCollisionDetection this.SetCollisionDetection
        member this.GetCollisionCategories world : string = this.Get (nameof this.CollisionCategories) world
        member this.SetCollisionCategories (value : string) world = this.Set (nameof this.CollisionCategories) value world
        member this.CollisionCategories = lens (nameof this.CollisionCategories) this this.GetCollisionCategories this.SetCollisionCategories
        member this.GetCollisionMask world : string = this.Get (nameof this.CollisionMask) world
        member this.SetCollisionMask (value : string) world = this.Set (nameof this.CollisionMask) value world
        member this.CollisionMask = lens (nameof this.CollisionMask) this this.GetCollisionMask this.SetCollisionMask
        member this.GetBodyShape world : BodyShape = this.Get (nameof this.BodyShape) world
        member this.SetBodyShape (value : BodyShape) world = this.Set (nameof this.BodyShape) value world
        member this.BodyShape = lens (nameof this.BodyShape) this this.GetBodyShape this.SetBodyShape
        member this.GetPhysicsMotion world : PhysicsMotion = this.Get (nameof this.PhysicsMotion) world
        member this.SetPhysicsMotion (value : PhysicsMotion) world = this.Set (nameof this.PhysicsMotion) value world
        member this.PhysicsMotion = lens (nameof this.PhysicsMotion) this this.GetPhysicsMotion this.SetPhysicsMotion
        member this.GetSensor world : bool = this.Get (nameof this.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof this.Sensor) value world
        member this.Sensor = lens (nameof this.Sensor) this this.GetSensor this.SetSensor
        member this.GetObservable world : bool = this.Get (nameof this.Observable) world
        member this.SetObservable (value : bool) world = this.Set (nameof this.Observable) value world
        member this.Observable = lens (nameof this.Observable) this this.GetObservable this.SetObservable
        member this.GetBodyId world : BodyId = this.Get (nameof this.BodyId) world
        member this.BodyId = lensReadOnly (nameof this.BodyId) this this.GetBodyId
        member this.BodyCollisionEvent = Events.BodyCollisionEvent --> this
        member this.BodySeparationExplicitEvent = Events.BodySeparationExplicitEvent --> this
        member this.BodySeparationImplicitEvent = Events.BodySeparationImplicitEvent --> Game
        member this.BodyTransformEvent = Events.BodyTransformEvent --> this

    /// Augments an entity with a physics-driven rigid body.
    type RigidBodyFacet () =
        inherit Facet (true)

        static let getBodyShape (entity : Entity) world =
            let scalar = entity.GetScale world * entity.GetSize world
            let bodyShape = entity.GetBodyShape world
            if entity.GetIs2d world
            then World.localizeBodyShape scalar bodyShape
            else bodyShape

        static let propagatePhysicsPosition (entity : Entity) (evt : Event<ChangeData, Entity>) world =
            if entity.GetPhysicsMotion world <> ManualMotion then
                let bodyId = entity.GetBodyId world
                let position = evt.Data.Value :?> Vector3
                (Cascade, World.setBodyCenter position bodyId world)
            else (Cascade, world)

        static let propagatePhysicsRotation (entity : Entity) (evt : Event<ChangeData, Entity>) world =
            if entity.GetPhysicsMotion world <> ManualMotion then
                let bodyId = entity.GetBodyId world
                let rotation = evt.Data.Value :?> Quaternion
                (Cascade, World.setBodyRotation rotation bodyId world)
            else (Cascade, world)

        static let propagatePhysicsLinearVelocity (entity : Entity) (evt : Event<ChangeData, Entity>) world =
            if entity.GetPhysicsMotion world <> ManualMotion then
                let bodyId = entity.GetBodyId world
                let linearVelocity = evt.Data.Value :?> Vector3
                (Cascade, World.setBodyLinearVelocity linearVelocity bodyId world)
            else (Cascade, world)

        static let propagatePhysicsAngularVelocity (entity : Entity) (evt : Event<ChangeData, Entity>) world =
            if entity.GetPhysicsMotion world <> ManualMotion then
                let bodyId = entity.GetBodyId world
                let angularVelocity = evt.Data.Value :?> Vector3
                (Cascade, World.setBodyAngularVelocity angularVelocity bodyId world)
            else (Cascade, world)

        static let propagatePhysics (entity : Entity) (_ : Event<ChangeData, Entity>) world =
            let world = entity.PropagatePhysics world
            (Cascade, world)

        static member Properties =
            [define Entity.BodyEnabled true
             define Entity.BodyType Static
             define Entity.SleepingAllowed true
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.LinearVelocity v3Zero
             define Entity.LinearDamping 0.0f // leave this up to friction by default
             define Entity.AngularVelocity v3Zero
             define Entity.AngularDamping 0.2f
             define Entity.AngularFactor v3One
             define Entity.Substance (Mass 1.0f)
             define Entity.GravityOverride None
             define Entity.CharacterProperties CharacterProperties.defaultProperties
             define Entity.CollisionDetection Discontinuous
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
             define Entity.PhysicsMotion SynchronizedMotion
             define Entity.Sensor false
             define Entity.Observable false
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =

            // OPTIMIZATION: using manual unsubscription in order to use less live objects for subscriptions.
            // OPTIMIZATION: share lambdas to reduce live object count.
            let subIds = Array.init 26 (fun _ -> makeGuid ())
            let world = World.subscribePlus subIds.[0] (propagatePhysicsPosition entity) (entity.ChangeEvent (nameof entity.Position)) entity world |> snd
            let world = World.subscribePlus subIds.[1] (propagatePhysicsRotation entity) (entity.ChangeEvent (nameof entity.Rotation)) entity world |> snd
            let world = World.subscribePlus subIds.[2] (propagatePhysicsLinearVelocity entity) (entity.ChangeEvent (nameof entity.LinearVelocity)) entity world |> snd
            let world = World.subscribePlus subIds.[3] (propagatePhysicsAngularVelocity entity) (entity.ChangeEvent (nameof entity.AngularVelocity)) entity world |> snd
            let world = World.subscribePlus subIds.[4] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Scale)) entity world |> snd
            let world = World.subscribePlus subIds.[5] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Offset)) entity world |> snd
            let world = World.subscribePlus subIds.[6] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Size)) entity world |> snd
            let world = World.subscribePlus subIds.[7] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.PerimeterCentered)) entity world |> snd
            let world = World.subscribePlus subIds.[8] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world |> snd
            let world = World.subscribePlus subIds.[9] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.BodyType)) entity world |> snd
            let world = World.subscribePlus subIds.[10] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.SleepingAllowed)) entity world |> snd
            let world = World.subscribePlus subIds.[11] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Friction)) entity world |> snd
            let world = World.subscribePlus subIds.[12] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Restitution)) entity world |> snd
            let world = World.subscribePlus subIds.[13] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.LinearDamping)) entity world |> snd
            let world = World.subscribePlus subIds.[14] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.AngularDamping)) entity world |> snd
            let world = World.subscribePlus subIds.[15] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.AngularFactor)) entity world |> snd
            let world = World.subscribePlus subIds.[16] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Substance)) entity world |> snd
            let world = World.subscribePlus subIds.[17] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.GravityOverride)) entity world |> snd
            let world = World.subscribePlus subIds.[18] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.CharacterProperties)) entity world |> snd
            let world = World.subscribePlus subIds.[19] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.CollisionDetection)) entity world |> snd
            let world = World.subscribePlus subIds.[10] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world |> snd
            let world = World.subscribePlus subIds.[21] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world |> snd
            let world = World.subscribePlus subIds.[23] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.BodyShape)) entity world |> snd
            let world = World.subscribePlus subIds.[24] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Sensor)) entity world |> snd
            let world = World.subscribePlus subIds.[25] (propagatePhysics entity) (entity.ChangeEvent (nameof entity.Observable)) entity world |> snd
            let unsubscribe = fun world ->
                Array.fold (fun world subId -> World.unsubscribe subId world) world subIds
            let callback = fun evt world ->
                if  Set.contains (nameof RigidBodyFacet) (evt.Data.Previous :?> string Set) &&
                    not (Set.contains (nameof RigidBodyFacet) (evt.Data.Value :?> string Set)) then
                    (Cascade, unsubscribe world)
                else (Cascade, world)
            let callback2 = fun _ world ->
                (Cascade, unsubscribe world)
            let world = World.sense callback entity.FacetNames.ChangeEvent entity (nameof RigidBodyFacet) world
            let world = World.sense callback2 entity.UnregisteringEvent entity (nameof RigidBodyFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let bodyProperties =
                { BodyIndex = (entity.GetBodyId world).BodyIndex
                  Center = if entity.GetIs2d world then transform.PerimeterCenter else transform.Position
                  Rotation = transform.Rotation
                  Scale = transform.Scale
                  BodyShape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  SleepingAllowed = entity.GetSleepingAllowed world
                  Enabled = entity.GetBodyEnabled world
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
                  CollisionDetection = entity.GetCollisionDetection world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Sensor = entity.GetSensor world
                  Observable = entity.GetObservable world }
            World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

[<AutoOpen>]
module BodyJointFacetModule =

    type Entity with
        member this.GetBodyJoint world : BodyJoint = this.Get (nameof this.BodyJoint) world
        member this.SetBodyJoint (value : BodyJoint) world = this.Set (nameof this.BodyJoint) value world
        member this.BodyJoint = lens (nameof this.BodyJoint) this this.GetBodyJoint this.SetBodyJoint
        member this.GetBodyJointId world : BodyJointId = this.Get (nameof this.BodyJointId) world
        member this.BodyJointId = lensReadOnly (nameof this.BodyJointId) this this.GetBodyJointId

    /// Augments an entity with a physics-driven joint.
    type BodyJointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.BodyJoint EmptyJoint
             computed Entity.BodyJointId (fun (entity : Entity) _ -> { BodyJointSource = entity; BodyJointIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof BodyJointFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyJoint)) entity (nameof BodyJointFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            let bodyJointProperties =
                { BodyJointIndex = (entity.GetBodyJointId world).BodyJointIndex
                  BodyJoint = (entity.GetBodyJoint world) }
            World.createBodyJoint (entity.GetIs2d world) entity bodyJointProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBodyJoint (entity.GetIs2d world) (entity.GetBodyJointId world) world

[<AutoOpen>]
module TileMapFacetModule =

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
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.Observable false
             define Entity.PhysicsMotion SynchronizedMotion
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileSizeDivisor 1
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.AutoBounds world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Observable)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TileSizeDivisor)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TileMap)) entity (nameof TileMapFacet) world
            let world =
                World.sense (fun _ world ->
                    let attributes = entity.GetAttributesInferred world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- attributes.SizeInferred
                    transform.Offset <- attributes.OffsetInferred
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TileMap))
                    entity
                    (nameof TileMapFacet)
                    world
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
                        (entity.GetCollisionCategories world)
                        (entity.GetCollisionMask world)
                        (entity.GetObservable world)
                        (entity.GetBodyId world).BodyIndex
                        tileMapDescriptor
                World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

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
            | None -> AttributesInferred.important Constants.Engine.EntitySize2dDefault v3Zero

[<AutoOpen>]
module TmxMapFacetModule =

    type Entity with
        member this.GetTmxMap world : TmxMap = this.Get (nameof this.TmxMap) world
        member this.SetTmxMap (value : TmxMap) world = this.Set (nameof this.TmxMap) value world
        member this.TmxMap = lens (nameof this.TmxMap) this this.GetTmxMap this.SetTmxMap

    /// Augments an entity with a user-defined tile map.
    type TmxMapFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.Observable false
             define Entity.PhysicsMotion SynchronizedMotion
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileSizeDivisor 1
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.AutoBounds world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Observable)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TileSizeDivisor)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TmxMap)) entity (nameof TmxMapFacet) world
            let world =
                World.sense (fun _ world ->
                    let attributes = entity.GetAttributesInferred world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- attributes.SizeInferred
                    transform.Offset <- attributes.OffsetInferred
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TmxMap))
                    entity
                    (nameof TmxMapFacet)
                    world
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
                    (entity.GetCollisionCategories world)
                    (entity.GetCollisionMask world)
                    (entity.GetObservable world)
                    (entity.GetBodyId world).BodyIndex
                    tmxMapDescriptor
            World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

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
module LayoutFacetModule =

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
        inherit Facet (false)

        static let rec flowRightward
            reentry leftX (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world // gui currently ignores rotation
            let childHalfWidth = childPerimeter.Width * 0.5f
            let childHalfHeight = childPerimeter.Height * 0.5f
            let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
            let childRightX = childCenter.X + childHalfWidth + margin.X
            offsetX <- childCenter.X + childHalfWidth
            let world =
                if childRightX >= leftX + wrapLimit then
                    offsetX <- leftX
                    offsetY <- offsetY + -margin.Y + -maximum
                    maximum <- 0.0f
                    if not reentry
                    then flowRightward true leftX margin wrapLimit &offsetX &offsetY &maximum child world
                    else child.SetPerimeterCenterLocal childCenter.V3 world
                else child.SetPerimeterCenterLocal childCenter.V3 world
            if childPerimeter.Height > maximum then maximum <- childPerimeter.Height
            world

        static let rec flowDownward
            reentry topY (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world // gui currently ignores rotation
            let childHalfWidth = childPerimeter.Width * 0.5f
            let childHalfHeight = childPerimeter.Height * 0.5f
            let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
            let childBottomY = childCenter.Y + -childHalfHeight + -margin.Y
            offsetY <- childCenter.Y + -childHalfHeight
            let world =
                if childBottomY <= topY + -wrapLimit then
                    offsetX <- offsetX + margin.X + maximum
                    offsetY <- topY
                    maximum <- 0.0f
                    if not reentry
                    then flowDownward true topY margin wrapLimit &offsetX &offsetY &maximum child world
                    else child.SetPerimeterCenterLocal childCenter.V3 world
                else child.SetPerimeterCenterLocal childCenter.V3 world
            if childPerimeter.Width > maximum then maximum <- childPerimeter.Width
            world

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
                    | FlowTo flowLimit -> flowLimit
                Array.fold (fun world child ->
                    flowRightward false leftX margin wrapLimit &offsetX &offsetY &maximum child world)
                    world children
            | FlowDownward ->
                let wrapLimit =
                    match flowLimit with
                    | FlowParent -> perimeter.Height
                    | FlowUnlimited -> Single.MaxValue
                    | FlowTo flowLimit -> flowLimit
                Array.fold (fun world child ->
                    flowDownward false topY margin wrapLimit &offsetX &offsetY &maximum child world)
                    world children
            | FlowLeftward -> world
            | FlowUpward -> world

        static let dockLayout (perimeter : Box2) margin (margins : Vector4) children world =
            let perimeterWidthHalf = perimeter.Width * 0.5f
            let perimeterHeightHalf = perimeter.Height * 0.5f
            Array.fold (fun world (child : Entity) ->
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
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockTop ->
                        let size = v2 perimeter.Width margins.W - margin
                        let position = v2 0.0f (perimeterHeightHalf - margins.Z * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockRight ->
                        let size = v2 margins.Z (perimeter.Height - margins.Y - margins.W) - margin
                        let position = v2 (perimeterWidthHalf - margins.Z * 0.5f) ((margins.Y - margins.W) * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockBottom ->
                        let size = v2 perimeter.Width margins.Y - margin
                        let position = v2 0.0f (-perimeterHeightHalf + margins.Y * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockLeft ->
                        let size = v2 margins.X (perimeter.Height - margins.Y - margins.W) - margin
                        let position = v2 (-perimeterWidthHalf + margins.X * 0.5f) ((margins.Y - margins.W) * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                else world)
                world children

        static let gridLayout (perimeter : Box2) margin (dims : Vector2i) flowDirectionOpt resizeChildren children world =
            let perimeterWidthHalf = perimeter.Width * 0.5f
            let perimeterHeightHalf = perimeter.Height * 0.5f
            let cellSize = v2 (perimeter.Width / single dims.X) (perimeter.Height / single dims.Y)
            let cellWidthHalf = cellSize.X * 0.5f
            let cellHeightHalf = cellSize.Y * 0.5f
            let childSize = cellSize - margin
            Array.foldi (fun n world (child : Entity) ->
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
                let world = child.SetPositionLocal childPosition.V3 world
                if resizeChildren
                then child.SetSize childSize.V3 world
                else world)
                world children

        static let performLayout (entity : Entity) world =
            if entity.GetPerimeterCentered world then // NOTE: layouts only supported for centered entities.
                match entity.GetLayout world with
                | Manual -> world // OPTIMIZATION: early exit.
                | layout ->
                    let children =
                        World.getEntityMounters entity world |>
                        Array.ofSeq |>
                        Array.map (fun child ->
                            let layoutOrder =
                                if child.Has<LayoutFacet> world
                                then child.GetLayoutOrder world
                                else 0
                            let order = child.GetOrder world
                            (layoutOrder, order, child)) |>
                        Array.sortBy ab_ |>
                        Array.map __c
                    let perimeter = (entity.GetPerimeter world).Box2 // gui currently ignores rotation
                    let margin = entity.GetLayoutMargin world
                    let world =
                        match layout with
                        | Flow (flowDirection, flowLimit) ->
                            flowLayout perimeter margin flowDirection flowLimit children world
                        | Dock (margins, percentageBased, resizeChildren) ->
                            ignore (percentageBased, resizeChildren) // TODO: P1: implement using these values.
                            dockLayout perimeter margin margins children world
                        | Grid (dims, flowDirectionOpt, resizeChildren) ->
                            gridLayout perimeter margin dims flowDirectionOpt resizeChildren children world
                        | Manual -> world
                    world
            else world

        static let handleLayout evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, performLayout entity world)

        static let handleMount evt world =
            let entity = evt.Subscriber : Entity
            let mounter = evt.Data.Mounter
            let (orderChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.Order.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (layoutOrderChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.LayoutOrder.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (dockTypeChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.DockType.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (gridPositionChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.GridPosition.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let world =
                World.sense (fun evt world ->
                    let world =
                        if evt.Data.Mounter = mounter then
                            let world = world |> orderChangeUnsub |> layoutOrderChangeUnsub |> dockTypeChangeUnsub |> gridPositionChangeUnsub
                            performLayout entity world
                        else world
                    (Cascade, world))
                    entity.UnmountEvent
                    entity
                    (nameof LayoutFacet)
                    world
            let world = performLayout entity world
            (Cascade, world)

        static member Properties =
            [define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

        override this.Register (entity, world) =
            let world = performLayout entity world
            let world = World.sense handleMount entity.MountEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.Transform.ChangeEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.Layout.ChangeEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.LayoutMargin.ChangeEvent entity (nameof LayoutFacet) world
            world

[<AutoOpen>]
module SkyBoxFacetModule =

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
        inherit Facet (false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.Static true
             define Entity.AmbientColor Color.White
             define Entity.AmbientBrightness 1.0f
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
module LightProbe3dFacetModule =

    type Entity with
        member this.GetProbeBounds world : Box3 = this.Get (nameof this.ProbeBounds) world
        member this.SetProbeBounds (value : Box3) world = this.Set (nameof this.ProbeBounds) value world
        member this.ProbeBounds = lens (nameof this.ProbeBounds) this this.GetProbeBounds this.SetProbeBounds
        member this.GetProbeStale world : bool = this.Get (nameof this.ProbeStale) world
        member this.SetProbeStale (value : bool) world = this.Set (nameof this.ProbeStale) value world
        member this.ProbeStale = lens (nameof this.ProbeStale) this this.GetProbeStale this.SetProbeStale

        member this.ResetProbeBounds world =
            let bounds =
                box3
                    (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + this.GetPosition world)
                    (v3Dup Constants.Render.LightProbeSizeDefault)
            this.SetProbeBounds bounds world

        member this.RecenterInProbeBounds world =
            let probeBounds = this.GetProbeBounds world
            if Option.isSome (this.GetMountOpt world)
            then this.SetPositionLocal probeBounds.Center world
            else this.SetPosition probeBounds.Center world

    /// Augments an entity with a 3d light probe.
    type LightProbe3dFacet () =
        inherit Facet (false)

        static let handleProbeStaleChange (evt : Event<ChangeData, Entity>) world =
            let world =
                if evt.Data.Value :?> bool
                then World.requestLightMapRender world
                else world
            (Cascade, world)

        static member Properties =
            [define Entity.Size (v3Dup 0.25f)
             define Entity.LightProbe true
             define Entity.Presence Omnipresent
             define Entity.Static true
             define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
             nonPersistent Entity.ProbeStale false]

        override this.Register (entity, world) =
            let world = World.sense handleProbeStaleChange (entity.GetChangeEvent (nameof entity.ProbeStale)) entity (nameof LightProbe3dFacet) world
            entity.SetProbeStale true world

        override this.Render (renderPass, entity, world) =
            let id = entity.GetId world
            let enabled = entity.GetEnabled world
            let position = entity.GetPosition world
            let bounds = entity.GetProbeBounds world
            World.enqueueRenderMessage3d (RenderLightProbe3d { LightProbeId = id; Enabled = enabled; Origin = position; Bounds = bounds; RenderPass = renderPass }) world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important (v3Dup 0.25f) v3Zero

        override this.Edit (op, entity, world) =
            match op with
            | AppendProperties append ->
                let world =
                    if ImGui.Button "Rerender Light Map" then
                        let world = append.Snapshot world
                        entity.SetProbeStale true world
                    else world
                let world =
                    if ImGui.Button "Recenter in Probe Bounds" then
                        let world = append.Snapshot world
                        let probeBounds = entity.GetProbeBounds world
                        if Option.isSome (entity.GetMountOpt world)
                        then entity.SetPositionLocal probeBounds.Center world
                        else entity.SetPosition probeBounds.Center world
                    else world
                let world =
                    if ImGui.Button "Reset Probe Bounds" then
                        let world = append.Snapshot world
                        entity.ResetProbeBounds world
                    else world
                world
            | _ -> world

[<AutoOpen>]
module Light3dFacetModule =

    type Entity with
        member this.GetAttenuationLinear world : single = this.Get (nameof this.AttenuationLinear) world
        member this.SetAttenuationLinear (value : single) world = this.Set (nameof this.AttenuationLinear) value world
        member this.AttenuationLinear = lens (nameof this.AttenuationLinear) this this.GetAttenuationLinear this.SetAttenuationLinear
        member this.GetAttenuationQuadratic world : single = this.Get (nameof this.AttenuationQuadratic) world
        member this.SetAttenuationQuadratic (value : single) world = this.Set (nameof this.AttenuationQuadratic) value world
        member this.AttenuationQuadratic = lens (nameof this.AttenuationQuadratic) this this.GetAttenuationQuadratic this.SetAttenuationQuadratic
        member this.GetLightCutoff world : single = this.Get (nameof this.LightCutoff) world
        member this.SetLightCutoff (value : single) world = this.Set (nameof this.LightCutoff) value world
        member this.LightCutoff = lens (nameof this.LightCutoff) this this.GetLightCutoff this.SetLightCutoff
        member this.GetLightType world : LightType = this.Get (nameof this.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof this.LightType) value world
        member this.LightType = lens (nameof this.LightType) this this.GetLightType this.SetLightType
        member this.GetDesireShadows world : bool = this.Get (nameof this.DesireShadows) world
        member this.SetDesireShadows (value : bool) world = this.Set (nameof this.DesireShadows) value world
        member this.DesireShadows = lens (nameof this.DesireShadows) this this.GetDesireShadows this.SetDesireShadows

    /// Augments an entity with a 3d light.
    type Light3dFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Size (v3Dup 0.25f)
             define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness Constants.Render.BrightnessDefault
             define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
             define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
             define Entity.LightCutoff Constants.Render.LightCutoffDefault
             define Entity.LightType PointLight
             define Entity.DesireShadows false]

        override this.Render (renderPass, entity, world) =
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
                          RenderPass = renderPass })
                    world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.important (v3Dup 0.25f) v3Zero

        override this.Edit (op, entity, world) =
            match op with
            | AppendProperties _ ->
                if ImGuiNET.ImGui.Button "Normalize Attenutation" then
                    let brightness = entity.GetBrightness world
                    let lightCutoff = entity.GetLightCutoff world
                    let world = entity.SetAttenuationLinear (1.0f / (brightness * lightCutoff)) world
                    let world = entity.SetAttenuationQuadratic (1.0f / (brightness * lightCutoff * lightCutoff)) world
                    world
                else world
            | _ -> world

[<AutoOpen>]
module LightingConfigFacetModule =

    type Entity with
        member this.GetLightingConfig world : LightingConfig = this.Get (nameof this.LightingConfig) world
        member this.SetLightingConfig (value : LightingConfig) world = this.Set (nameof this.LightingConfig) value world
        member this.LightingConfig = lens (nameof this.LightingConfig) this this.GetLightingConfig this.SetLightingConfig

    type LightingConfigFacet () =
        inherit Facet (false)

        static let configureLighting (entity : Entity) world =
            let lightingConfig = entity.GetLightingConfig world
            World.enqueueRenderMessage3d (ConfigureLighting lightingConfig) world

        static member Properties =
            [define Entity.LightingConfig LightingConfig.defaultConfig]

        override this.Register (entity, world) =
            let world = World.sense (fun evt world -> configureLighting evt.Subscriber world; (Cascade, world)) (entity.ChangeEvent (nameof entity.LightingConfig)) entity (nameof LightingConfigFacet) world
            configureLighting entity world
            world

[<AutoOpen>]
module StaticBillboardFacetModule =

    type Entity with
        // OPTIMIZATION: override allows surface properties to be fetched with a single look-up.
        member this.GetMaterialProperties world : MaterialProperties = this.Get (nameof this.MaterialProperties) world
        member this.SetMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.MaterialProperties) value world
        member this.MaterialProperties = lens (nameof this.MaterialProperties) this this.GetMaterialProperties this.SetMaterialProperties
        member this.GetMaterial world : Material = this.Get (nameof this.Material) world
        member this.SetMaterial (value : Material) world = this.Set (nameof this.Material) value world
        member this.Material = lens (nameof this.Material) this this.GetMaterial this.SetMaterial
        member this.GetRenderStyle world : RenderStyle = this.Get (nameof this.RenderStyle) world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set (nameof this.RenderStyle) value world
        member this.RenderStyle = lens (nameof this.RenderStyle) this this.GetRenderStyle this.SetRenderStyle

    /// Augments an entity with a static billboard.
    type StaticBillboardFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.Material Material.defaultMaterial
             define Entity.RenderStyle Deferred]

        override this.Render (renderPass, entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.enqueueRenderMessage3d
                (RenderBillboard
                    { Absolute = absolute; Presence = presence; ModelMatrix = affineMatrix; InsetOpt = insetOpt
                      MaterialProperties = properties; Material = material; RenderType = renderType; RenderPass = renderPass })
                world

        override this.RayCast (ray, entity, world) =
            // TODO: P1: intersect against oriented quad rather than bounds.
            let bounds = entity.GetBounds world
            let intersectionOpt = ray.Intersects bounds
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

[<AutoOpen>]
module BasicStaticBillboardEmitterFacetModule =

    type Entity with

        member this.GetEmitterMaterialProperties world : MaterialProperties = this.Get (nameof this.EmitterMaterialProperties) world
        member this.SetEmitterMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.EmitterMaterialProperties) value world
        member this.EmitterMaterialProperties = lens (nameof this.EmitterMaterialProperties) this this.GetEmitterMaterialProperties this.SetEmitterMaterialProperties
        member this.GetEmitterMaterial world : Material = this.Get (nameof this.EmitterMaterial) world
        member this.SetEmitterMaterial (value : Material) world = this.Set (nameof this.EmitterMaterial) value world
        member this.EmitterMaterial = lens (nameof this.EmitterMaterial) this this.GetEmitterMaterial this.SetEmitterMaterial
        member this.GetEmitterRenderType world : RenderType = this.Get (nameof this.EmitterRenderType) world
        member this.SetEmitterRenderType (value : RenderType) world = this.Set (nameof this.EmitterRenderType) value world
        member this.EmitterRenderType = lens (nameof this.EmitterRenderType) this this.GetEmitterRenderType this.SetEmitterRenderType

    /// Augments an entity with basic static billboard emitter.
    type BasicStaticBillboardEmitterFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) (world : World) =
            World.tryMakeEmitter
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)
                (entity.GetEmitterStyle world)
                world |>
            Option.map cast<Particles.BasicStaticBillboardEmitter>

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
                    Absolute = transform.Absolute
                    Material = entity.GetEmitterMaterial world
                    ParticleSeed = entity.GetBasicParticleSeed world
                    Constraint = entity.GetEmitterConstraint world }
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
            let world = entity.SetParticleSystem particleSystem world
            world

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
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterMaterialPropertiesChange evt world =
            let emitterMaterialProperties = evt.Data.Value :?> MaterialProperties
            let world = mapEmitter (fun emitter -> if emitter.MaterialProperties <> emitterMaterialProperties then { emitter with MaterialProperties = emitterMaterialProperties } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMaterialChange evt world =
            let emitterMaterial = evt.Data.Value :?> Material
            let world = mapEmitter (fun emitter -> if emitter.Material <> emitterMaterial then { emitter with Material = emitterMaterial } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterRenderTypeChange evt world =
            let emitterRenderType = evt.Data.Value :?> RenderType
            let world = mapEmitter (fun emitter -> if emitter.RenderType <> emitterRenderType then { emitter with RenderType = emitterRenderType } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = mapEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = mapEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = mapEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = mapEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticBillboardEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = mapEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = mapEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = mapEmitter (constant emitter) entity world
            (Cascade, world)

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
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

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
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

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
             nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.sense handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMaterialPropertiesChange (entity.GetChangeEvent (nameof entity.EmitterMaterialProperties)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMaterialChange (entity.GetChangeEvent (nameof entity.EmitterMaterial)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterRenderTypeChange (entity.GetChangeEvent (nameof entity.EmitterRenderType)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity (nameof BasicStaticBillboardEmitterFacet) world
            world

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
                let world = entity.SetParticleSystem particleSystem world
                processOutput output entity world
            else world

        override this.Render (renderPass, entity, world) =
            let time = world.GameTime
            let presence = entity.GetPresence world
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                Particles.ParticleSystem.toParticlesDescriptors time |>
                List.map (fun descriptor ->
                    match descriptor with
                    | Particles.BillboardParticlesDescriptor descriptor ->
                        let emitterProperties = entity.GetEmitterMaterialProperties world
                        let properties =
                            { AlbedoOpt = match emitterProperties.AlbedoOpt with Some albedo -> Some albedo | None -> descriptor.MaterialProperties.AlbedoOpt
                              RoughnessOpt = match emitterProperties.RoughnessOpt with Some roughness -> Some roughness | None -> descriptor.MaterialProperties.RoughnessOpt
                              MetallicOpt = match emitterProperties.MetallicOpt with Some metallic -> Some metallic | None -> descriptor.MaterialProperties.MetallicOpt
                              AmbientOcclusionOpt = match emitterProperties.AmbientOcclusionOpt with Some ambientOcclusion -> Some ambientOcclusion | None -> descriptor.MaterialProperties.AmbientOcclusionOpt
                              EmissionOpt = match emitterProperties.EmissionOpt with Some emission -> Some emission | None -> descriptor.MaterialProperties.EmissionOpt
                              HeightOpt = match emitterProperties.HeightOpt with Some height -> Some height | None -> descriptor.MaterialProperties.HeightOpt
                              IgnoreLightMapsOpt = match emitterProperties.IgnoreLightMapsOpt with Some ignoreLightMaps -> Some ignoreLightMaps | None -> descriptor.MaterialProperties.IgnoreLightMapsOpt
                              OpaqueDistanceOpt = None }
                        let emitterMaterial = entity.GetEmitterMaterial world
                        let material =
                            { AlbedoImageOpt = match emitterMaterial.AlbedoImageOpt with Some albedoImage -> Some albedoImage | None -> descriptor.Material.AlbedoImageOpt
                              RoughnessImageOpt = match emitterMaterial.RoughnessImageOpt with Some roughnessImage -> Some roughnessImage | None -> descriptor.Material.RoughnessImageOpt
                              MetallicImageOpt = match emitterMaterial.MetallicImageOpt with Some metallicImage -> Some metallicImage | None -> descriptor.Material.MetallicImageOpt
                              AmbientOcclusionImageOpt = match emitterMaterial.AmbientOcclusionImageOpt with Some ambientOcclusionImage -> Some ambientOcclusionImage | None -> descriptor.Material.AmbientOcclusionImageOpt
                              EmissionImageOpt = match emitterMaterial.EmissionImageOpt with Some emissionImage -> Some emissionImage | None -> descriptor.Material.EmissionImageOpt
                              NormalImageOpt = match emitterMaterial.NormalImageOpt with Some normalImage -> Some normalImage | None -> descriptor.Material.NormalImageOpt
                              HeightImageOpt = match emitterMaterial.HeightImageOpt with Some heightImage -> Some heightImage | None -> descriptor.Material.HeightImageOpt
                              TwoSidedOpt = match emitterMaterial.TwoSidedOpt with Some twoSided -> Some twoSided | None -> descriptor.Material.TwoSidedOpt }
                        Some
                            (RenderBillboardParticles
                                { Absolute = descriptor.Absolute
                                  Presence = presence
                                  MaterialProperties = properties
                                  Material = material
                                  Particles = descriptor.Particles
                                  RenderType = descriptor.RenderType
                                  RenderPass = renderPass })
                    | _ -> None) |>
                List.definitize
            World.enqueueRenderMessages3d particlesMessages world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

[<AutoOpen>]
module StaticModelFacetModule =

    type Entity with

        member this.GetStaticModel world : StaticModel AssetTag = this.Get (nameof this.StaticModel) world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set (nameof this.StaticModel) value world
        member this.StaticModel = lens (nameof this.StaticModel) this this.GetStaticModel this.SetStaticModel

    /// Augments an entity with a static model.
    type StaticModelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.empty
             define Entity.RenderStyle Deferred
             define Entity.StaticModel Assets.Default.StaticModel]

        override this.Render (renderPass, entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = ValueOption.ofOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let staticModel = entity.GetStaticModel world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.renderStaticModelFast (absolute, &affineMatrix, presence, insetOpt, &properties, staticModel, renderType, renderPass, world)

        override this.GetAttributesInferred (entity, world) =
            let staticModel = entity.GetStaticModel world
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                let bounds = staticModelMetadata.Bounds
                AttributesInferred.important bounds.Size bounds.Center
            | None -> base.GetAttributesInferred (entity, world)

        override this.RayCast (ray, entity, world) =
            let affineMatrix = entity.GetAffineMatrix world
            let inverseMatrix = Matrix4x4.Invert affineMatrix |> snd
            let rayEntity = ray.Transform inverseMatrix
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface = rayEntity.Transform inverse
                        let boundsIntersectionOpt = raySurface.Intersects geometry.Bounds
                        if boundsIntersectionOpt.HasValue then
                            raySurface.Intersects (geometry.Indices, geometry.Vertices) |>
                            Seq.map snd' |>
                            Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity) |>
                            Seq.map (fun pointEntity -> Vector3.Transform (pointEntity, affineMatrix)) |>
                            Seq.map (fun point -> (point - ray.Origin).Magnitude) |>
                            Seq.toArray
                        else [||])
                        staticModelMetadata.Surfaces
                Array.concat intersectionses
            | None -> [||]

[<AutoOpen>]
module StaticModelSurfaceFacetModule =

    type Entity with
        member this.GetSurfaceIndex world : int = this.Get (nameof this.SurfaceIndex) world
        member this.SetSurfaceIndex (value : int) world = this.Set (nameof this.SurfaceIndex) value world
        member this.SurfaceIndex = lens (nameof this.SurfaceIndex) this this.GetSurfaceIndex this.SetSurfaceIndex

    /// Augments an entity with an indexed static model surface.
    type StaticModelSurfaceFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.empty
             define Entity.Material Material.empty
             define Entity.RenderStyle Deferred
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.SurfaceIndex 0]

        override this.Render (renderPass, entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let material = entity.GetMaterial world
            let staticModel = entity.GetStaticModel world
            let surfaceIndex = entity.GetSurfaceIndex world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.renderStaticModelSurfaceFast (absolute, &affineMatrix, presence, insetOpt, &properties, &material, staticModel, surfaceIndex, renderType, renderPass, world)

        override this.GetAttributesInferred (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex > -1 && surfaceIndex < staticModelMetadata.Surfaces.Length then
                    let bounds = staticModelMetadata.Surfaces.[surfaceIndex].SurfaceBounds
                    AttributesInferred.important bounds.Size bounds.Center
                else base.GetAttributesInferred (entity, world)
            | None -> base.GetAttributesInferred (entity, world)

        override this.RayCast (ray, entity, world) =
            let rayEntity = ray.Transform (Matrix4x4.Invert (entity.GetAffineMatrix world) |> snd)
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex < staticModelMetadata.Surfaces.Length then
                    let surface = staticModelMetadata.Surfaces.[surfaceIndex]
                    let geometry = surface.PhysicallyBasedGeometry
                    let boundsIntersectionOpt = rayEntity.Intersects geometry.Bounds
                    if boundsIntersectionOpt.HasValue then
                        let intersections = rayEntity.Intersects (geometry.Indices, geometry.Vertices)
                        intersections |> Seq.map snd' |> Seq.toArray
                    else [||]
                else [||]
            | None -> [||]

[<AutoOpen>]
module AnimatedModelFacetModule =

    type Entity with

        member this.GetAnimations world : Animation array = this.Get (nameof this.Animations) world
        member this.SetAnimations (value : Animation array) world = this.Set (nameof this.Animations) value world
        member this.Animations = lens (nameof this.Animations) this this.GetAnimations this.SetAnimations
        member this.GetAnimatedModel world : AnimatedModel AssetTag = this.Get (nameof this.AnimatedModel) world
        member this.SetAnimatedModel (value : AnimatedModel AssetTag) world = this.Set (nameof this.AnimatedModel) value world
        member this.AnimatedModel = lens (nameof this.AnimatedModel) this this.GetAnimatedModel this.SetAnimatedModel
        member this.GetBoneIdsOpt world : Dictionary<string, int> option = this.Get (nameof this.BoneIdsOpt) world
        member this.SetBoneIdsOpt (value : Dictionary<string, int> option) world = this.Set (nameof this.BoneIdsOpt) value world
        member this.BoneIdsOpt = lens (nameof this.BoneIdsOpt) this this.GetBoneIdsOpt this.SetBoneIdsOpt
        member this.GetBoneOffsetsOpt world : Matrix4x4 array option = this.Get (nameof this.BoneOffsetsOpt) world
        member this.SetBoneOffsetsOpt (value : Matrix4x4 array option) world = this.Set (nameof this.BoneOffsetsOpt) value world
        member this.BoneOffsetsOpt = lens (nameof this.BoneOffsetsOpt) this this.GetBoneOffsetsOpt this.SetBoneOffsetsOpt
        member this.GetBoneTransformsOpt world : Matrix4x4 array option = this.Get (nameof this.BoneTransformsOpt) world
        member this.SetBoneTransformsOpt (value : Matrix4x4 array option) world = this.Set (nameof this.BoneTransformsOpt) value world
        member this.BoneTransformsOpt = lens (nameof this.BoneTransformsOpt) this this.GetBoneTransformsOpt this.SetBoneTransformsOpt

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

    /// Augments an entity with an animated model.
    type AnimatedModelFacet () =
        inherit Facet (false)

        static let tryComputeBoneTransforms time animations (sceneOpt : Assimp.Scene option) =
            match sceneOpt with
            | Some scene when scene.Meshes.Count > 0 ->
                let (boneIds, boneOffsets, boneTransforms) = scene.ComputeBoneTransforms (time, animations, scene.Meshes.[0])
                Some (boneIds, boneOffsets, boneTransforms)
            | Some _ | None -> None

        static let tryAnimateBones (entity : Entity) (world : World) =
            let time = world.GameTime
            let animations = entity.GetAnimations world
            let animatedModel = entity.GetAnimatedModel world
            let sceneOpt = match Metadata.tryGetAnimatedModelMetadata animatedModel with Some model -> model.SceneOpt | None -> None
            match tryComputeBoneTransforms time animations sceneOpt with
            | Some (boneIds, boneOffsets, boneTransforms) ->
                let world = entity.SetBoneIdsOpt (Some boneIds) world
                let world = entity.SetBoneOffsetsOpt (Some boneOffsets) world
                let world = entity.SetBoneTransformsOpt (Some boneTransforms) world
                world
            | None -> world

        static member Properties =
            [define Entity.StartTime GameTime.zero
             define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.empty
             define Entity.Animations [|{ StartTime = GameTime.zero; LifeTimeOpt = None; Name = "Armature"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None }|]
             define Entity.AnimatedModel Assets.Default.AnimatedModel
             nonPersistent Entity.BoneIdsOpt None
             nonPersistent Entity.BoneOffsetsOpt None
             nonPersistent Entity.BoneTransformsOpt None]

        override this.Register (entity, world) =
            let world = tryAnimateBones entity world
            let world =
                World.sense
                    (fun evt world -> (Cascade, if world.Halted then tryAnimateBones evt.Subscriber world else world))
                    (entity.ChangeEvent (nameof entity.Animations)) entity (nameof AnimatedModelFacet) world
            let world =
                World.sense
                    (fun evt world -> (Cascade, tryAnimateBones evt.Subscriber world))
                    (entity.ChangeEvent (nameof entity.AnimatedModel)) entity (nameof AnimatedModelFacet) world
            world

        override this.Update (entity, world) =
            let time = world.GameTime
            let animations = entity.GetAnimations world
            let animatedModel = entity.GetAnimatedModel world
            let sceneOpt = match Metadata.tryGetAnimatedModelMetadata animatedModel with Some model -> model.SceneOpt | None -> None
            let resultOpt =
                match World.tryAwaitJob (world.DateTime + TimeSpan.FromSeconds 0.001) (entity, nameof AnimatedModelFacet) world with
                | Some (JobCompletion (_, _, (:? ((Dictionary<string, int> * Matrix4x4 array * Matrix4x4 array) option) as boneOffsetsAndTransformsOpt))) -> boneOffsetsAndTransformsOpt
                | _ -> None
            let world =
                match resultOpt with
                | Some (boneIds, boneOffsets, boneTransforms) ->
                    let world = entity.SetBoneIdsOpt (Some boneIds) world
                    let world = entity.SetBoneOffsetsOpt (Some boneOffsets) world
                    let world = entity.SetBoneTransformsOpt (Some boneTransforms) world
                    world
                | None -> world
            let job = Job.make (entity, nameof AnimatedModelFacet) (fun () -> tryComputeBoneTransforms time animations sceneOpt)
            World.enqueueJob 1.0f job world
            world

        override this.Render (renderPass, entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let animatedModel = entity.GetAnimatedModel world
            match entity.GetBoneTransformsOpt world with
            | Some boneTransforms -> World.renderAnimatedModelFast (absolute, &affineMatrix, presence, insetOpt, &properties, boneTransforms, animatedModel, renderPass, world)
            | None -> ()

        override this.GetAttributesInferred (entity, world) =
            let animatedModel = entity.GetAnimatedModel world
            match Metadata.tryGetAnimatedModelMetadata animatedModel with
            | Some animatedModelMetadata ->
                let bounds = animatedModelMetadata.Bounds
                AttributesInferred.important bounds.Size bounds.Center
            | None -> base.GetAttributesInferred (entity, world)

        override this.RayCast (ray, entity, world) =
            let affineMatrix = entity.GetAffineMatrix world
            let inverseMatrix = Matrix4x4.Invert affineMatrix |> snd
            let rayEntity = ray.Transform inverseMatrix
            match Metadata.tryGetAnimatedModelMetadata (entity.GetAnimatedModel world) with
            | Some animatedModelMetadata ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface = rayEntity.Transform inverse
                        let boundsIntersectionOpt = raySurface.Intersects geometry.Bounds
                        if boundsIntersectionOpt.HasValue then
                            raySurface.Intersects (geometry.Indices, geometry.Vertices) |>
                            Seq.map snd' |>
                            Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity) |>
                            Seq.map (fun pointEntity -> Vector3.Transform (pointEntity, affineMatrix)) |>
                            Seq.map (fun point -> (point - ray.Origin).Magnitude) |>
                            Seq.toArray
                        else [||])
                        animatedModelMetadata.Surfaces
                Array.concat intersectionses
            | None -> [||]

        override this.Edit (op, entity, world) =
            match op with
            | OverlayViewport _ ->
                match (entity.GetBoneOffsetsOpt world, entity.GetBoneTransformsOpt world) with
                | (Some offsets, Some transforms) ->
                    let affineMatrix = entity.GetAffineMatrix world
                    for i in 0 .. dec offsets.Length do
                        let offset = offsets.[i]
                        let transform = transforms.[i]
                        World.imGuiCircle3d false (offset.Inverted * transform * affineMatrix).Translation 2.0f false Color.Yellow world
                    world
                | (_, _) -> world
            | _ -> world

[<AutoOpen>]
module TerrainFacetModule =

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
        member this.GetSegments world : Vector2i = this.Get (nameof this.Segments) world
        member this.SetSegments (value : Vector2i) world = this.Set (nameof this.Segments) value world
        member this.Segments = lens (nameof this.Segments) this this.GetSegments this.SetSegments

        member this.TryGetTerrainResolution world =
            match this.GetHeightMap world with
            | ImageHeightMap map ->
                match Metadata.tryGetTextureSize map with
                | Some textureSize -> Some textureSize
                | None -> None
            | RawHeightMap map -> Some map.Resolution

        member this.TryGetTerrainQuadSize world =
            let bounds = this.GetBounds world
            match this.TryGetTerrainResolution world with
            | Some resolution -> Some (v2 (bounds.Size.X / single (dec resolution.X)) (bounds.Size.Z / single (dec resolution.Y)))
            | None -> None

    /// Augments an entity with a rigid 3d terrain.
    type TerrainFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Size (v3 512.0f 128.0f 512.0f)
             define Entity.Presence Omnipresent
             define Entity.Static true
             define Entity.AlwaysRender true
             define Entity.BodyEnabled true
             define Entity.Friction 0.5f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.InsetOpt None
             define Entity.TerrainMaterialProperties TerrainMaterialProperties.defaultProperties
             define Entity.TerrainMaterial
                (BlendMaterial
                    { TerrainLayers =
                        [|{ AlbedoImage = Assets.Default.TerrainLayerAlbedo
                            RoughnessImage = Assets.Default.TerrainLayerRoughness
                            AmbientOcclusionImage = Assets.Default.TerrainLayerAmbientOcclusion
                            NormalImage = Assets.Default.TerrainLayerNormal
                            HeightImage = Assets.Default.TerrainLayerHeight }
                          { AlbedoImage = Assets.Default.TerrainLayer2Albedo
                            RoughnessImage = Assets.Default.TerrainLayer2Roughness
                            AmbientOcclusionImage = Assets.Default.TerrainLayer2AmbientOcclusion
                            NormalImage = Assets.Default.TerrainLayer2Normal
                            HeightImage = Assets.Default.TerrainLayer2Height }|]
                      BlendMap =
                          RedsMap
                            [|Assets.Default.TerrainLayerBlend
                              Assets.Default.TerrainLayer2Blend|]})
             define Entity.TintImageOpt None
             define Entity.NormalImageOpt None
             define Entity.Tiles (v2 256.0f 256.0f)
             define Entity.HeightMap (RawHeightMap { Resolution = v2i 513 513; RawFormat = RawUInt16 LittleEndian; RawAsset = Assets.Default.HeightMap })
             define Entity.Segments v2iOne
             define Entity.Observable false
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.HeightMap)) entity (nameof TerrainFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            match entity.TryGetTerrainResolution world with
            | Some resolution ->
                let mutable transform = entity.GetTransform world
                let terrainShape =
                    { Resolution = resolution
                      Bounds = transform.Bounds3d
                      HeightMap = entity.GetHeightMap world
                      TransformOpt = None
                      PropertiesOpt = None }
                let bodyProperties =
                    { BodyIndex = (entity.GetBodyId world).BodyIndex
                      Center = if entity.GetIs2d world then transform.PerimeterCenter else transform.Position
                      Rotation = transform.Rotation
                      Scale = transform.Scale
                      BodyShape = TerrainShape terrainShape
                      BodyType = Static
                      SleepingAllowed = true
                      Enabled = entity.GetBodyEnabled world
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
                      CollisionDetection = Discontinuous
                      CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                      CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                      Sensor = false
                      Observable = entity.GetObservable world }
                World.createBody false (entity.GetBodyId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody false (entity.GetBodyId world) world

        override this.Render (renderPass, entity, world) =
            let mutable transform = entity.GetTransform world
            let terrainDescriptor =
                { Bounds = transform.Bounds3d
                  InsetOpt = entity.GetInsetOpt world
                  MaterialProperties = entity.GetTerrainMaterialProperties world
                  Material = entity.GetTerrainMaterial world
                  TintImageOpt = entity.GetTintImageOpt world
                  NormalImageOpt = entity.GetNormalImageOpt world
                  Tiles = entity.GetTiles world
                  HeightMap = entity.GetHeightMap world
                  Segments = entity.GetSegments world }
            World.enqueueRenderMessage3d
                (RenderTerrain
                    { Absolute = transform.Absolute
                      Visible = transform.Visible
                      TerrainDescriptor = terrainDescriptor
                      RenderPass = renderPass })
                world

        override this.GetAttributesInferred (entity, world) =
            match entity.TryGetTerrainResolution world with
            | Some resolution -> AttributesInferred.important (v3 (single (dec resolution.X)) 128.0f (single (dec resolution.Y))) v3Zero
            | None -> AttributesInferred.important (v3 512.0f 128.0f 512.0f) v3Zero

[<AutoOpen>]
module NavBodyFacetModule =

    type Entity with
        member this.GetNavShape world : NavShape = this.Get (nameof this.NavShape) world
        member this.SetNavShape (value : NavShape) world = this.Set (nameof this.NavShape) value world
        member this.NavShape = lens (nameof this.NavShape) this this.GetNavShape this.SetNavShape

    /// Augments an entity with a 3d navigation body.
    type NavBodyFacet () =
        inherit Facet (false)

        static let propagateNavBody (entity : Entity) world =
            match entity.GetNavShape world with
            | NavShape.EmptyNavShape ->
                if entity.GetIs2d world
                then world // TODO: implement for 2d navigation when it's available.
                else World.setNav3dBodyOpt None entity world
            | shape ->
                if entity.GetIs2d world
                then world // TODO: implement for 2d navigation when it's available.
                else
                    let bounds = entity.GetBounds world
                    let affineMatrix = entity.GetAffineMatrix world
                    let staticModel = entity.GetStaticModel world
                    let surfaceIndex = entity.GetSurfaceIndex world
                    World.setNav3dBodyOpt (Some (bounds, affineMatrix, staticModel, surfaceIndex, shape)) entity world

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.SurfaceIndex 0
             define Entity.NavShape BoundsNavShape]

        override this.Register (entity, world) =

            // OPTIMIZATION: conditionally subscribe to transform change event.
            let subId = Gen.id
            let subscribe world =
                World.subscribePlus subId (fun _ world -> (Cascade, propagateNavBody entity world)) (entity.ChangeEvent (nameof entity.Bounds)) entity world |> snd
            let unsubscribe world =
                World.unsubscribe subId world
            let callback evt world =
                let entity = evt.Subscriber : Entity
                let previous = evt.Data.Previous :?> NavShape
                let shape = evt.Data.Value :?> NavShape
                let world = match previous with NavShape.EmptyNavShape -> world | _ -> unsubscribe world
                let world = match shape with NavShape.EmptyNavShape -> world | _ -> subscribe world
                let world = propagateNavBody entity world
                (Cascade, world)
            let callback2 evt world =
                if  Set.contains (nameof NavBodyFacet) (evt.Data.Previous :?> string Set) &&
                    not (Set.contains (nameof NavBodyFacet) (evt.Data.Value :?> string Set)) then
                    (Cascade, unsubscribe world)
                else (Cascade, world)
            let callback3 _ world = (Cascade, unsubscribe world)
            let world = match entity.GetNavShape world with NavShape.EmptyNavShape -> world | _ -> subscribe world
            let world = World.sense callback (entity.ChangeEvent (nameof entity.NavShape)) entity (nameof NavBodyFacet) world
            let world = World.sense callback2 entity.FacetNames.ChangeEvent entity (nameof NavBodyFacet) world
            let world = World.sense callback3 entity.UnregisteringEvent entity (nameof NavBodyFacet) world

            // unconditional registration behavior
            let world = World.sense (fun _ world -> (Cascade, propagateNavBody entity world)) (entity.ChangeEvent (nameof entity.StaticModel)) entity (nameof NavBodyFacet) world
            let world = World.sense (fun _ world -> (Cascade, propagateNavBody entity world)) (entity.ChangeEvent (nameof entity.SurfaceIndex)) entity (nameof NavBodyFacet) world
            propagateNavBody entity world

        override this.Unregister (entity, world) =
            if entity.GetIs2d world
            then world // TODO: implement for 2d navigation when it's available.
            else World.setNav3dBodyOpt None entity world

        override this.GetAttributesInferred (_, _) =
            AttributesInferred.unimportant

[<AutoOpen>]
module Nav3dConfigFacetModule =

    type Entity with
        member this.GetNav3dConfig world : Nav3dConfig = this.Get (nameof this.Nav3dConfig) world
        member this.SetNav3dConfig (value : Nav3dConfig) world = this.Set (nameof this.Nav3dConfig) value world
        member this.Nav3dConfig = lens (nameof this.Nav3dConfig) this this.GetNav3dConfig this.SetNav3dConfig

    /// Augments an entity with a navigation mesh.
    type Nav3dConfigFacet () =
        inherit Facet (false)

        static let propagateNav3dConfig (entity : Entity) world =
            let config = entity.GetNav3dConfig world
            World.setNav3dConfig config entity.Screen world

        static member Properties =
            [define Entity.Nav3dConfig Nav3dConfig.defaultConfig]

        override this.Register (entity, world) =
            World.sense (fun _ world -> (Cascade, propagateNav3dConfig entity world)) (entity.ChangeEvent (nameof entity.Nav3dConfig)) entity (nameof Nav3dConfigFacet) world

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

[<AutoOpen>]
module FollowerFacetModule =

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

    type FollowerFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Following true
             define Entity.FollowMoveSpeed 2.0f
             define Entity.FollowTurnSpeed 3.0f
             define Entity.FollowDistanceMinOpt None
             define Entity.FollowDistanceMaxOpt None
             define Entity.FollowTargetOpt None]

        override this.Update (entity, world) =
            let following = entity.GetFollowing world
            if following then
                let targetOpt = entity.GetFollowTargetOpt world
                match targetOpt with
                | Some target when target.Exists world ->
                    let moveSpeed = entity.GetFollowMoveSpeed world * (let gd = world.GameDelta in gd.Seconds)
                    let turnSpeed = entity.GetFollowTurnSpeed world * (let gd = world.GameDelta in gd.Seconds)
                    let distanceMinOpt = entity.GetFollowDistanceMinOpt world
                    let distanceMaxOpt = entity.GetFollowDistanceMaxOpt world
                    let position = entity.GetPosition world
                    let destination = target.GetPosition world
                    let distance = (destination - position).Magnitude
                    let rotation = entity.GetRotation world
                    if  (distanceMinOpt.IsNone || distance > distanceMinOpt.Value) &&
                        (distanceMaxOpt.IsNone || distance <= distanceMaxOpt.Value) then
                        if entity.GetIs2d world
                        then world // TODO: implement for 2d navigation when it's available.
                        else
                            // TODO: consider doing an offset physics ray cast to align nav position with near
                            // ground. Additionally, consider removing the CellHeight offset in the above query so
                            // that we don't need to do an offset here at all.
                            let followOutput = World.nav3dFollow distanceMinOpt distanceMaxOpt moveSpeed turnSpeed position rotation destination entity.Screen world
                            let world = entity.SetPosition followOutput.NavPosition world
                            let world = entity.SetRotation followOutput.NavRotation world
                            let world = entity.SetLinearVelocity followOutput.NavLinearVelocity world
                            let world = entity.SetAngularVelocity followOutput.NavAngularVelocity world
                            world
                    else world
                | _ -> world
            else world