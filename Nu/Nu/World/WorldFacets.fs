// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open TiledSharp
open Prime
open Nu

/// Declaratively exposes simulant lenses and events.
module Declarative =

    /// Declaratively exposes Game lenses and events.
    let Game = Unchecked.defaultof<Game>

    /// Declaratively exposes Screen lenses and events.
    let Screen = Unchecked.defaultof<Screen>

    /// Declaratively exposes Group lenses and events.
    let Group = Unchecked.defaultof<Group>

    /// Declaratively exposes Entity lenses and events.
    let Entity = Unchecked.defaultof<Entity>

open Declarative

[<AutoOpen>]
module ScriptFacetModule =

    type Entity with
        member this.GetScriptOpt world : Symbol AssetTag option = this.Get (nameof this.ScriptOpt) world
        member this.SetScriptOpt (value : Symbol AssetTag option) world = this.Set (nameof this.ScriptOpt) value world
        member this.ScriptOpt = lens (nameof this.ScriptOpt) this this.GetScriptOpt this.SetScriptOpt
        member this.GetScript world : Scripting.Expr array = this.Get (nameof this.Script) world
        member this.SetScript (value : Scripting.Expr array) world = this.Set (nameof this.Script) value world
        member this.Script = lens (nameof this.Script) this this.GetScript this.SetScript
        member internal this.GetScriptUnsubscriptions world : Unsubscription list = this.Get (nameof this.ScriptUnsubscriptions) world
        member internal this.SetScriptUnsubscriptions (value : Unsubscription list) world = this.Set (nameof this.ScriptUnsubscriptions) value world
        member internal this.ScriptUnsubscriptions = lens (nameof this.ScriptUnsubscriptions) this this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions
        member this.GetRegisterScript world : Scripting.Expr = this.Get (nameof this.RegisterScript) world
        member this.SetRegisterScript (value : Scripting.Expr) world = this.Set (nameof this.RegisterScript) value world
        member this.RegisterScript = lens (nameof this.RegisterScript) this this.GetRegisterScript this.SetRegisterScript
        member this.GetUnregisterScript world : Scripting.Expr = this.Get (nameof this.UnregisterScript) world
        member this.SetUnregisterScript (value : Scripting.Expr) world = this.Set (nameof this.UnregisterScript) value world
        member this.UnregisterScript = lens (nameof this.UnregisterScript) this this.GetUnregisterScript this.SetUnregisterScript
#if !DISABLE_ENTITY_PRE_UPDATE
        member this.GetPreUpdateScript world : Scripting.Expr = this.Get (nameof this.PreUpdateScript) world
        member this.SetPreUpdateScript (value : Scripting.Expr) world = this.Set (nameof this.PreUpdateScript) value world
        member this.PreUpdateScript = lens (nameof this.PreUpdateScript) this this.GetPreUpdateScript this.SetPreUpdateScript
#endif
        member this.GetUpdateScript world : Scripting.Expr = this.Get (nameof this.UpdateScript) world
        member this.SetUpdateScript (value : Scripting.Expr) world = this.Set (nameof this.UpdateScript) value world
        member this.UpdateScript = lens (nameof this.UpdateScript) this this.GetUpdateScript this.SetUpdateScript
#if !DISABLE_ENTITY_POST_UPDATE
        member this.GetPostUpdateScript world : Scripting.Expr = this.Get (nameof this.PostUpdateScript) world
        member this.SetPostUpdateScript (value : Scripting.Expr) world = this.Set (nameof this.PostUpdateScript) value world
        member this.PostUpdateScript = lens (nameof this.PostUpdateScript) this this.GetPostUpdateScript this.SetPostUpdateScript
#endif
        member this.GetRenderScript world : Scripting.Expr = this.Get (nameof this.RenderScript) world
        member this.SetRenderScript (value : Scripting.Expr) world = this.Set (nameof this.RenderScript) value world
        member this.RenderScript = lens (nameof this.RenderScript) this this.GetRenderScript this.SetRenderScript

    type ScriptFacet () =
        inherit Facet (false)

        static let handleScriptChange evt world =
            let entity = evt.Subscriber : Entity
            let script = entity.GetScript world
            let scriptFrame = Scripting.DeclarationFrame StringComparer.Ordinal
            let world = World.setEntityScriptFrame scriptFrame entity world |> snd'
            let world = evalManyWithLogging script scriptFrame entity world |> snd'
            (Cascade, world)

        static let handleRegisterScriptChange evt world =
            let entity = evt.Subscriber : Entity
            let world = World.unregisterEntity entity world
            let world = World.registerEntity entity world
            (Cascade, world)

        static member Properties =
            [define Entity.ScriptOpt None
             define Entity.Script [||]
             define Entity.ScriptUnsubscriptions []
             define Entity.RegisterScript Scripting.Unit
             define Entity.UnregisterScript Scripting.Unit
#if !DISABLE_ENTITY_PRE_UPDATE
             define Entity.PreUpdateScript Scripting.Unit
#endif
             define Entity.UpdateScript Scripting.Unit
#if !DISABLE_ENTITY_POST_UPDATE
             define Entity.PostUpdateScript Scripting.Unit
#endif
             define Entity.RenderScript Scripting.Unit]

        override this.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetRegisterScript world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChange (entity.GetChangeEvent (nameof entity.ScriptFrame)) entity world
            let world = World.monitor handleRegisterScriptChange (entity.GetChangeEvent (nameof entity.RegisterScript)) entity world
            world

        override this.Unregister (entity, world) =
            World.evalWithLogging (entity.GetUnregisterScript world) (entity.GetScriptFrame world) entity world |> snd'

#if !DISABLE_ENTITY_PRE_UPDATE
        override this.PreUpdate (entity, world) =
            World.evalWithLogging (entity.GetPreUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'
#endif

        override this.Update (entity, world) =
            World.evalWithLogging (entity.GetUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'

#if !DISABLE_ENTITY_POST_UPDATE
        override this.PostUpdate (entity, world) =
            World.evalWithLogging (entity.GetPostUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'
#endif

        override this.Render (entity, world) =
            World.evalWithLogging (entity.GetRenderScript world) (entity.GetScriptFrame world) entity world |> snd'

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with
        member this.GetStaticImage world : Image AssetTag = this.Get (nameof this.StaticImage) world
        member this.SetStaticImage (value : Image AssetTag) world = this.Set (nameof this.StaticImage) value world
        member this.StaticImage = lens (nameof this.StaticImage) this this.GetStaticImage this.SetStaticImage
        member this.GetInsetOpt world : Box2 option = this.Get (nameof this.InsetOpt) world
        member this.SetInsetOpt (value : Box2 option) world = this.Set (nameof this.InsetOpt) value world
        member this.InsetOpt = lens (nameof this.InsetOpt) this this.GetInsetOpt this.SetInsetOpt
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

    type StaticSpriteFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.StaticImage Assets.Default.Box
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let staticImage = entity.GetStaticImage world
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = AssetTag.generalize staticImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
                          Image = staticImage
                          Color = entity.GetColor world
                          Blend = entity.GetBlend world
                          Emission = entity.GetEmission world
                          Flip = entity.GetFlip world }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
        member this.GetCelSize world : Vector2 = this.Get (nameof this.CelSize) world
        member this.SetCelSize (value : Vector2) world = this.Set (nameof this.CelSize) value world
        member this.CelSize = lens (nameof this.CelSize) this this.GetCelSize this.SetCelSize
        member this.GetCelRun world : int = this.Get (nameof this.CelRun) world
        member this.SetCelRun (value : int) world = this.Set (nameof this.CelRun) value world
        member this.CelRun = lens (nameof this.CelRun) this this.GetCelRun this.SetCelRun
        member this.GetCelCount world : int = this.Get (nameof this.CelCount) world
        member this.SetCelCount (value : int) world = this.Set (nameof this.CelCount) value world
        member this.CelCount = lens (nameof this.CelCount) this this.GetCelCount this.SetCelCount
        member this.GetAnimationDelay world : GameTime = this.Get (nameof this.AnimationDelay) world
        member this.SetAnimationDelay (value : GameTime) world = this.Set (nameof this.AnimationDelay) value world
        member this.AnimationDelay = lens (nameof this.AnimationDelay) this this.GetAnimationDelay this.SetAnimationDelay
        member this.GetAnimationSheet world : Image AssetTag = this.Get (nameof this.AnimationSheet) world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.Set (nameof this.AnimationSheet) value world
        member this.AnimationSheet = lens (nameof this.AnimationSheet) this this.GetAnimationSheet this.SetAnimationSheet

    type AnimatedSpriteFacet () =
        inherit Facet (false)

        static let getSpriteInsetOpt (entity : Entity) world =
            let celCount = entity.GetCelCount world
            let celRun = entity.GetCelRun world
            if celCount <> 0 && celRun <> 0 then
                let cel =
                    match entity.GetAnimationDelay world with
                    | UpdateTime delay -> int (world.UpdateTime / delay) % celCount
                    | ClockTime delay -> int (world.ClockTime / delay) % celCount
                let celSize = entity.GetCelSize world
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = single celI * celSize.X
                let celY = single celJ * celSize.Y
                let inset = box2 (v2 celX celY) celSize
                Some inset
            else None

        static member Properties =
            [define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelRun 4
             define Entity.CelCount 16
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AnimationSheet Assets.Default.Block
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let animationSheet = entity.GetAnimationSheet world
            World.enqueueLayeredOperation2d
                { Elevation = transform.Elevation
                  Horizon = transform.Horizon
                  AssetTag = AssetTag.generalize animationSheet
                  RenderOperation2d =
                    RenderSprite
                        { Transform = transform
                          InsetOpt = match getSpriteInsetOpt entity world with Some inset -> ValueSome inset | None -> ValueNone
                          Image = animationSheet
                          Color = entity.GetColor world
                          Blend = entity.GetBlend world
                          Emission = entity.GetEmission world
                          Flip = entity.GetFlip world }}
                world

        override this.GetQuickSize (entity, world) =
            (entity.GetCelSize world).V3

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

    type BasicStaticSpriteEmitterFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) world =
            World.tryMakeEmitter
                (World.getGameTime world)
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
                    (World.getGameTime world)
                    (entity.GetEmitterLifeTimeOpt world)
                    (entity.GetParticleLifeTimeMaxOpt world)
                    (entity.GetParticleRate world)
                    (entity.GetParticleMax world)

        static let updateParticleSystem updater (entity : Entity) world =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = updater particleSystem
            let world = entity.SetParticleSystem particleSystem world
            world

        static let updateEmitter updater (entity : Entity) world =
            updateParticleSystem (fun particleSystem ->
                match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                    let emitter = updater emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem)
                entity world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterBlendChange evt world =
            let emitterBlend = evt.Data.Value :?> Blend
            let world = updateEmitter (fun emitter -> if emitter.Blend <> emitterBlend then { emitter with Blend = emitterBlend } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterImageChange evt world =
            let emitterImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.Image emitterImage then { emitter with Image = emitterImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = updateEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = updateEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticSpriteEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = updateEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
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
            let world = World.monitor handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity world
            let world = World.monitor handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity world
            let world = World.monitor handleEmitterBlendChange (entity.GetChangeEvent (nameof entity.EmitterBlend)) entity world
            let world = World.monitor handleEmitterImageChange (entity.GetChangeEvent (nameof entity.EmitterImage)) entity world
            let world = World.monitor handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity world
            let world = World.monitor handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity world
            let world = World.monitor handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity world
            let world = World.monitor handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity world
            let world = World.monitor handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity world
            let world = World.monitor handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity world
            let world = World.monitor handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity world
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

        override this.Render (entity, world) =
            let time = World.getGameTime world
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
                              AssetTag = AssetTag.generalize descriptor.Image
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

    type TextFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Text ""
             define Entity.Font Assets.Default.Font
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.TextMargin v2Zero
             define Entity.TextColor Color.Black
             define Entity.TextDisabledColor (Color (0.25f, 0.25f, 0.25f, 0.75f))
             define Entity.TextOffset v2Zero]

        override this.Render (entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon
                let mutable textTransform = Transform.makeDefault false // centered-ness and offset is already baked into perimeterUnscaled
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3
                textTransform.Position <- perimeter.Min + margin + offset
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + 0.5f // lift text above parent
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world
                World.enqueueLayeredOperation2d
                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize font
                      RenderOperation2d =
                        RenderText
                            { Transform = textTransform
                              Text = text
                              Font = font
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                              Justification = entity.GetJustification world }}
                    world
            else world

[<AutoOpen>]
module EffectFacetModule =

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
        member this.GetEffectCentered world : bool = this.Get (nameof this.EffectCentered) world
        member this.SetEffectCentered (value : bool) world = this.Set (nameof this.EffectCentered) value world
        member this.EffectCentered = lens (nameof this.EffectCentered) this this.GetEffectCentered this.SetEffectCentered
        member this.GetEffectOffset world : Vector3 = this.Get (nameof this.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof this.EffectOffset) value world
        member this.EffectOffset = lens (nameof this.EffectOffset) this this.GetEffectOffset this.SetEffectOffset
        member this.GetEffectRenderType world : RenderType = this.Get (nameof this.EffectRenderType) world
        member this.SetEffectRenderType (value : RenderType) world = this.Set (nameof this.EffectRenderType) value world
        member this.EffectRenderType = lens (nameof this.EffectRenderType) this this.GetEffectRenderType this.SetEffectRenderType
        member this.GetEffectHistoryMax world : int = this.Get (nameof this.EffectHistoryMax) world
        member this.SetEffectHistoryMax (value : int) world = this.Set (nameof this.EffectHistoryMax) value world
        member this.EffectHistoryMax = lens (nameof this.EffectHistoryMax) this this.GetEffectHistoryMax this.SetEffectHistoryMax
        member this.GetEffectHistory world : Effects.Slice Nito.Collections.Deque = this.Get (nameof this.EffectHistory) world
        member this.EffectHistory = lensReadOnly (nameof this.EffectHistory) this this.GetEffectHistory
        member this.GetEffectTags world : Map<string, Effects.Slice> = this.Get (nameof this.EffectTags) world
        member this.SetEffectTags (value : Map<string, Effects.Slice>) world = this.Set (nameof this.EffectTags) value world
        member this.EffectTags = lens (nameof this.EffectTags) this this.GetEffectTags this.SetEffectTags

        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetEffectStartTimeOpt world with
            | Some effectStartTime -> effectStartTime
            | None -> GameTime.zero

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
                    (entity.GetEffectStartTime world)
                    (entity.GetEffectCentered world)
                    (entity.GetEffectOffset world)
                    (entity.GetTransform world)
                    (entity.GetEffectRenderType world)
                    (entity.GetParticleSystem world)
                    (entity.GetEffectHistoryMax world)
                    (entity.GetEffectHistory world)
                    (entity.GetEffectDefinitions world)
                    (entity.GetEffectDescriptor world)

            // run effect, optionally destroying upon exhaustion
            let (liveness, effect, world) = Effect.run effect world
            let world = entity.SetParticleSystem effect.ParticleSystem world
            let world = entity.SetEffectTags effect.Tags world
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

#if DISABLE_ENTITY_PRE_UPDATE
        static let handlePreUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunEarly -> run entity world
                    | _ -> world
                else world
            (Cascade, world)
#endif

#if DISABLE_ENTITY_POST_UPDATE
        static let handlePostUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunLate -> run entity world
                    | _ -> world
                else world
            (Cascade, world)
#endif

        static member Properties =
            [define Entity.ParticleSystem Particles.ParticleSystem.empty
             define Entity.SelfDestruct false
             define Entity.RunMode RunLate
             define Entity.EffectSymbolOpt None
             define Entity.EffectStartTimeOpt None
             define Entity.EffectDefinitions Map.empty
             define Entity.EffectDescriptor Effects.EffectDescriptor.empty
             define Entity.EffectCentered true
             define Entity.EffectOffset v3Zero
             define Entity.EffectRenderType (ForwardRenderType (0.0f, 0.0f))
             define Entity.EffectHistoryMax Constants.Effects.EffectHistoryMaxDefault
             variable Entity.EffectHistory (fun _ -> Nito.Collections.Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))
             nonPersistent Entity.EffectTags Map.empty]

        override this.Register (entity, world) =
            let effectStartTime = Option.defaultValue (World.getGameTime world) (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.monitor handleEffectDescriptorChange (entity.GetChangeEvent (nameof entity.EffectDescriptor)) entity world
            let world = World.monitor handleEffectsChange (entity.GetChangeEvent (nameof entity.EffectSymbolOpt)) entity world
            let world = World.monitor handleAssetsReload Events.AssetsReload entity world
#if DISABLE_ENTITY_PRE_UPDATE
            let world = World.monitor handlePreUpdate entity.Group.PreUpdateEvent entity world
#endif
#if DISABLE_ENTITY_POST_UPDATE
            let world = World.monitor handlePostUpdate entity.Group.PostUpdateEvent entity world
#endif
            world

#if !DISABLE_ENTITY_PRE_UPDATE
        override this.PreUpdate (entity, world) =
            if entity.GetEnabled world && entity.GetRunMode world = RunEarly
            then run entity world
            else world
#endif

#if !DISABLE_ENTITY_POST_UPDATE
        override this.PostUpdate (entity, world) =
            if entity.GetEnabled world && entity.GetRunMode world = RunLate
            then run entity world
            else world
#endif

        override this.RayCast (ray, entity, world) =
            if entity.GetIs3d world then
                let intersectionOpt = ray.Intersects (entity.GetBounds world)
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
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
        member this.GetBullet world : bool = this.Get (nameof this.Bullet) world
        member this.SetBullet (value : bool) world = this.Set (nameof this.Bullet) value world
        member this.Bullet = lens (nameof this.Bullet) this this.GetBullet this.SetBullet
        member this.GetSensor world : bool = this.Get (nameof this.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof this.Sensor) value world
        member this.Sensor = lens (nameof this.Sensor) this this.GetSensor this.SetSensor
        member this.GetModelDriven world : bool = this.Get (nameof this.ModelDriven) world
        member this.SetModelDriven (value : bool) world = this.Set (nameof this.ModelDriven) value world
        member this.ModelDriven = lens (nameof this.ModelDriven) this this.GetModelDriven this.SetModelDriven
        member this.GetBodyId world : BodyId = this.Get (nameof this.BodyId) world
        member this.BodyId = lensReadOnly (nameof this.BodyId) this this.GetBodyId
        member this.BodyCollisionEvent = Events.BodyCollision --> this
        member this.BodySeparationImplicitEvent = Events.BodySeparationImplicit
        member this.BodySeparationExplicitEvent = Events.BodySeparationExplicit --> this
        member this.BodyTransformEvent = Events.BodyTransform --> this

    type RigidBodyFacet () =
        inherit Facet (true)

        static let getBodyShape (entity : Entity) world =
            let scalar = entity.GetScale world * entity.GetSize world
            let bodyShape = entity.GetBodyShape world
            World.localizeBodyShape scalar bodyShape world

        static member Properties =
            [define Entity.BodyEnabled true
             define Entity.BodyType Dynamic
             define Entity.SleepingAllowed true
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.LinearVelocity v3Zero
             define Entity.LinearDamping 0.0f
             define Entity.AngularVelocity v3Zero
             define Entity.AngularDamping 0.0f
             define Entity.AngularFactor v3One
             define Entity.Substance (Density 1.0f)
             define Entity.GravityOverride None
             define Entity.CollisionDetection Discontinuous
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.BodyShape (BodyBox { Size = v3One; TransformOpt = None; PropertiesOpt = None })
             define Entity.Bullet false
             define Entity.Sensor false
             define Entity.ModelDriven false
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Position)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Scale)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Rotation)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Offset)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Size)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Centered)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyType)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.SleepingAllowed)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.LinearVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.LinearDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularFactor)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Substance)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.GravityOverride)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionDetection)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyShape)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Bullet)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Sensor)) entity world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let bodyProperties =
                { BodyIndex = (entity.GetBodyId world).BodyIndex
                  Center = transform.Center
                  Rotation = transform.Rotation
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
                  CollisionDetection = entity.GetCollisionDetection world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Bullet = entity.GetBullet world
                  Sensor = entity.GetSensor world }
            let world = World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world
            let world = World.updateBodyObservable false entity world
            world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

[<AutoOpen>]
module JointFacetModule =

    type Entity with
        member this.GetJointDevice world : JointDevice = this.Get (nameof this.JointDevice) world
        member this.SetJointDevice (value : JointDevice) world = this.Set (nameof this.JointDevice) value world
        member this.JointDevice = lens (nameof this.JointDevice) this this.GetJointDevice this.SetJointDevice
        member this.GetJointId world : JointId = this.Get (nameof this.JointId) world
        member this.JointId = lensReadOnly (nameof this.JointId) this this.GetJointId

    type JointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.JointDevice JointEmpty
             computed Entity.JointId (fun (entity : Entity) _ -> { JointSource = entity; JointIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.JointDevice)) entity world
            world

        override this.RegisterPhysics (entity, world) =
            let jointProperties =
                { JointIndex = (entity.GetJointId world).JointIndex
                  JointDevice = (entity.GetJointDevice world) }
            World.createJoint (entity.GetIs2d world) entity jointProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyJoint (entity.GetIs2d world) (entity.GetJointId world) world

[<AutoOpen>]
module TileMapFacetModule =

    type Entity with
        member this.GetTileLayerClearance world : single = this.Get (nameof this.TileLayerClearance) world
        member this.SetTileLayerClearance (value : single) world = this.Set (nameof this.TileLayerClearance) value world
        member this.TileLayerClearance = lens (nameof this.TileLayerClearance) this this.GetTileLayerClearance this.SetTileLayerClearance
        member this.GetTileIndexOffset world : int = this.Get (nameof this.TileIndexOffset) world
        member this.SetTileIndexOffset (value : int) world = this.Set (nameof this.TileIndexOffset) value world
        member this.TileIndexOffset = lens (nameof this.TileIndexOffset) this this.GetTileIndexOffset this.SetTileIndexOffset
        member this.GetTileIndexOffsetRange world : int * int = this.Get (nameof this.TileIndexOffsetRange) world
        member this.SetTileIndexOffsetRange (value : int * int) world = this.Set (nameof this.TileIndexOffsetRange) value world
        member this.TileIndexOffsetRange = lens (nameof this.TileIndexOffsetRange) this this.GetTileIndexOffsetRange this.SetTileIndexOffsetRange
        member this.GetTileMap world : TileMap AssetTag = this.Get (nameof this.TileMap) world
        member this.SetTileMap (value : TileMap AssetTag) world = this.Set (nameof this.TileMap) value world
        member this.TileMap = lens (nameof this.TileMap) this this.GetTileMap this.SetTileMap

    type TileMapFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TileMap)) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TileMap))
                    entity
                    world
            world

        override this.RegisterPhysics (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) with
            | Some tileMap ->
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
                let tileMapPosition = perimeterUnscaled.Min.V2
                let tileMapDescriptor = TmxMap.getDescriptor tileMapPosition tileMap
                let bodyProperties =
                    TmxMap.getBodyProperties
                        transform.Enabled
                        (entity.GetFriction world)
                        (entity.GetRestitution world)
                        (entity.GetCollisionCategories world)
                        (entity.GetCollisionMask world)
                        (entity.GetBodyId world).BodyIndex
                        tileMapDescriptor
                World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

        override this.Render (entity, world) =
            let tileMapAsset = entity.GetTileMap world
            match TmxMap.tryGetTileMap tileMapAsset with
            | Some tileMap ->
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
                let viewBounds = World.getViewBounds2d world
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
                        (entity.GetTileIndexOffset world)
                        (entity.GetTileIndexOffsetRange world)
                        tileMapAsset.PackageName
                        tileMap
                World.enqueueLayeredOperations2d tileMapMessages world
            | None -> world

        override this.GetQuickSize (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) with
            | Some tileMap -> TmxMap.getQuickSize tileMap
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module TmxMapFacetModule =

    type Entity with
        member this.GetTmxMap world : TmxMap = this.Get (nameof this.TmxMap) world
        member this.SetTmxMap (value : TmxMap) world = this.Set (nameof this.TmxMap) value world
        member this.TmxMap = lens (nameof this.TmxMap) this this.GetTmxMap this.SetTmxMap

    type TmxMapFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TmxMap (TmxMap.makeDefault ())
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TmxMap)) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TmxMap))
                    entity
                    world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tmx map currently ignores rotation and scale
            let tmxMap = entity.GetTmxMap world
            let tmxMapPosition = perimeterUnscaled.Min.V2
            let tmxMapDescriptor = TmxMap.getDescriptor tmxMapPosition tmxMap
            let bodyProperties =
                TmxMap.getBodyProperties
                    transform.Enabled
                    (entity.GetFriction world)
                    (entity.GetRestitution world)
                    (entity.GetCollisionCategories world)
                    (entity.GetCollisionMask world)
                    (entity.GetBodyId world).BodyIndex
                    tmxMapDescriptor
            World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
            let viewBounds = World.getViewBounds2d world
            let tmxMap = entity.GetTmxMap world
            let tmxPackage = if tmxMap.TmxDirectory = "" then Assets.Default.PackageName else Path.GetFileName tmxMap.TmxDirectory // really folder name, but whatever...
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
                    (entity.GetTileIndexOffset world)
                    (entity.GetTileIndexOffsetRange world)
                    tmxPackage
                    tmxMap
            World.enqueueLayeredOperations2d tmxMapMessages world

        override this.GetQuickSize (entity, world) =
            let tmxMap = entity.GetTmxMap world
            TmxMap.getQuickSize tmxMap

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

    type LayoutFacet () =
        inherit Facet (false)

        static let rec flowRightward
            reentry leftX (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world
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
                    else child.SetCenterLocal childCenter.V3 world
                else child.SetCenterLocal childCenter.V3 world
            if childPerimeter.Height > maximum then maximum <- childPerimeter.Height
            world

        static let rec flowDownward
            reentry topY (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world
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
                    else child.SetCenterLocal childCenter.V3 world
                else child.SetCenterLocal childCenter.V3 world
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
            if entity.GetCentered world then // NOTE: layouts only supported for centered entities.
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
                    let perimeter = (entity.GetPerimeter world).Box2
                    let margin = entity.GetLayoutMargin world
                    let world =
                        match layout with
                        | Flow (flowDirection, flowLimit) ->
                            flowLayout perimeter margin flowDirection flowLimit children world
                        | Dock (margins, percentageBased, resizeChildren) ->
                            ignore (percentageBased, resizeChildren) // TODO: implement using these values.
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
            let (orderChangeUnsub, world) = World.monitorPlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.Order.ChangeEvent --> mounter) entity world
            let (layoutOrderChangeUnsub, world) = World.monitorPlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.LayoutOrder.ChangeEvent --> mounter) entity world
            let (dockTypeChangeUnsub, world) = World.monitorPlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.DockType.ChangeEvent --> mounter) entity world
            let (gridPositionChangeUnsub, world) = World.monitorPlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.GridPosition.ChangeEvent --> mounter) entity world
            let world =
                World.monitor (fun evt world ->
                    let world =
                        if evt.Data.Mounter = mounter then
                            let world = world |> orderChangeUnsub |> layoutOrderChangeUnsub |> dockTypeChangeUnsub |> gridPositionChangeUnsub
                            performLayout entity world
                        else world
                    (Cascade, world))
                    (Events.Unmount --> entity)
                    entity
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
            let world = World.monitor handleMount (Events.Mount --> entity) entity world
            let world = World.monitor handleLayout entity.Transform.ChangeEvent entity world
            let world = World.monitor handleLayout entity.Layout.ChangeEvent entity world
            let world = World.monitor handleLayout entity.LayoutMargin.ChangeEvent entity world
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

    type SkyBoxFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.AmbientColor Color.White
             define Entity.AmbientBrightness 1.0f
             define Entity.Color Color.White
             define Entity.Brightness 1.0f
             define Entity.CubeMap Assets.Default.SkyBoxMap]

        override this.Render (entity, world) =
            World.enqueueRenderMessage3d
                (RenderSkyBox
                    { AmbientColor = entity.GetAmbientColor world
                      AmbientBrightness = entity.GetAmbientBrightness world
                      CubeMapColor = entity.GetColor world
                      CubeMapBrightness = entity.GetBrightness world
                      CubeMap = entity.GetCubeMap world })
                world

[<AutoOpen>]
module LightProbeFacet3dModule =

    type Entity with
        member this.GetProbeBounds world : Box3 = this.Get (nameof this.ProbeBounds) world
        member this.SetProbeBounds (value : Box3) world = this.Set (nameof this.ProbeBounds) value world
        member this.ProbeBounds = lens (nameof this.ProbeBounds) this this.GetProbeBounds this.SetProbeBounds
        member this.GetProbeStale world : bool = this.Get (nameof this.ProbeStale) world
        member this.SetProbeStale (value : bool) world = this.Set (nameof this.ProbeStale) value world
        member this.ProbeStale = lens (nameof this.ProbeStale) this this.GetProbeStale this.SetProbeStale

    type LightProbeFacet3d () =
        inherit Facet (false)

        static let handleProbeStaleChange (evt : Event<ChangeData, Entity>) world =
            let world =
                if evt.Data.Value :?> bool
                then World.requestUnculledRender world
                else world
            (Cascade, world)

        static member Properties =
            [define Entity.Light true
             define Entity.Presence Omnipresent
             define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
             define Entity.ProbeStale false]

        override this.Register (entity, world) =
            let world = World.monitor handleProbeStaleChange (entity.GetChangeEvent (nameof entity.ProbeStale)) entity world
            entity.SetProbeStale true world
            
        override this.Render (entity, world) =
            let id = entity.GetId world
            let enabled = entity.GetEnabled world
            let position = entity.GetPosition world
            let bounds = entity.GetProbeBounds world
            let stale = entity.GetProbeStale world
            let world = if stale then entity.SetProbeStale false world else world
            World.enqueueRenderMessage3d (RenderLightProbe3d { LightProbeId = id; Enabled = enabled; Origin = position; Bounds = bounds; Stale = stale }) world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.TryGetHighlightBounds (entity, world) =
            Some (entity.GetBounds world)

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module LightFacet3dModule =

    type Entity with
        member this.GetAttenuationLinear world : single = this.Get (nameof this.AttenuationLinear) world
        member this.SetAttenuationLinear (value : single) world = this.Set (nameof this.AttenuationLinear) value world
        member this.AttenuationLinear = lens (nameof this.AttenuationLinear) this this.GetAttenuationLinear this.SetAttenuationLinear
        member this.GetAttenuationQuadratic world : single = this.Get (nameof this.AttenuationQuadratic) world
        member this.SetAttenuationQuadratic (value : single) world = this.Set (nameof this.AttenuationQuadratic) value world
        member this.AttenuationQuadratic = lens (nameof this.AttenuationQuadratic) this this.GetAttenuationQuadratic this.SetAttenuationQuadratic
        member this.GetCutoff world : single = this.Get (nameof this.Cutoff) world
        member this.SetCutoff (value : single) world = this.Set (nameof this.Cutoff) value world
        member this.Cutoff = lens (nameof this.Cutoff) this this.GetCutoff this.SetCutoff
        member this.GetLightType world : LightType = this.Get (nameof this.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof this.LightType) value world
        member this.LightType = lens (nameof this.LightType) this this.GetLightType this.SetLightType

    type LightFacet3d () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness Constants.Render.BrightnessDefault
             define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
             define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
             define Entity.Cutoff Constants.Render.CutoffDefault
             define Entity.LightType PointLight]

        override this.Render (entity, world) =
            if entity.GetEnabled world then
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let color = entity.GetColor world
                let brightness = entity.GetBrightness world
                let attenuationLinear = entity.GetAttenuationLinear world
                let attenuationQuadratic = entity.GetAttenuationQuadratic world
                let cutoff = entity.GetCutoff world
                let lightType = entity.GetLightType world
                World.enqueueRenderMessage3d
                    (RenderLight3d
                        { Origin = position
                          Direction = Vector3.Transform (v3Up, rotation)
                          Color = color
                          Brightness = brightness
                          AttenuationLinear = attenuationLinear
                          AttenuationQuadratic = attenuationQuadratic
                          Cutoff = cutoff
                          LightType = lightType })
                    world
            else world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.TryGetHighlightBounds (entity, world) =
            Some (entity.GetBounds world)

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardFacetModule =

    type [<StructuralEquality; StructuralComparison>] RenderStyle =
        | Deferred
        | Forward of Subsort : single * Sort : single

    type Entity with
        // OPTIMIZATION: override allows surface properties to be fetched with a single look-up.
        member this.GetMaterialProperties world : MaterialProperties = this.Get (nameof this.MaterialProperties) world
        member this.SetMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.MaterialProperties) value world
        member this.MaterialProperties = lens (nameof this.MaterialProperties) this this.GetMaterialProperties this.SetMaterialProperties
        member this.GetAlbedoImage world : Image AssetTag = this.Get (nameof this.AlbedoImage) world
        member this.SetAlbedoImage (value : Image AssetTag) world = this.Set (nameof this.AlbedoImage) value world
        member this.AlbedoImage = lens (nameof this.AlbedoImage) this this.GetAlbedoImage this.SetAlbedoImage
        member this.GetMetallicImage world : Image AssetTag = this.Get (nameof this.MetallicImage) world
        member this.SetMetallicImage (value : Image AssetTag) world = this.Set (nameof this.MetallicImage) value world
        member this.MetallicImage = lens (nameof this.MetallicImage) this this.GetMetallicImage this.SetMetallicImage
        member this.GetRoughnessImage world : Image AssetTag = this.Get (nameof this.RoughnessImage) world
        member this.SetRoughnessImage (value : Image AssetTag) world = this.Set (nameof this.RoughnessImage) value world
        member this.RoughnessImage = lens (nameof this.RoughnessImage) this this.GetRoughnessImage this.SetRoughnessImage
        member this.GetAmbientOcclusionImage world : Image AssetTag = this.Get (nameof this.AmbientOcclusionImage) world
        member this.SetAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof this.AmbientOcclusionImage) value world
        member this.AmbientOcclusionImage = lens (nameof this.AmbientOcclusionImage) this this.GetAmbientOcclusionImage this.SetAmbientOcclusionImage
        member this.GetEmissionImage world : Image AssetTag = this.Get (nameof this.EmissionImage) world
        member this.SetEmissionImage (value : Image AssetTag) world = this.Set (nameof this.EmissionImage) value world
        member this.EmissionImage = lens (nameof this.EmissionImage) this this.GetEmissionImage this.SetEmissionImage
        member this.GetNormalImage world : Image AssetTag = this.Get (nameof this.NormalImage) world
        member this.SetNormalImage (value : Image AssetTag) world = this.Set (nameof this.NormalImage) value world
        member this.NormalImage = lens (nameof this.NormalImage) this this.GetNormalImage this.SetNormalImage
        member this.GetHeightImage world : Image AssetTag = this.Get (nameof this.HeightImage) world
        member this.SetHeightImage (value : Image AssetTag) world = this.Set (nameof this.HeightImage) value world
        member this.HeightImage = lens (nameof this.HeightImage) this this.GetHeightImage this.SetHeightImage
        member this.GetTextureMinFilterOpt world : OpenGL.TextureMinFilter option = this.Get (nameof this.TextureMinFilterOpt) world
        member this.SetTextureMinFilterOpt (value : OpenGL.TextureMinFilter option) world = this.Set (nameof this.TextureMinFilterOpt) value world
        member this.TextureMinFilterOpt = lens (nameof this.TextureMinFilterOpt) this this.GetTextureMinFilterOpt this.SetTextureMinFilterOpt
        member this.GetTextureMagFilterOpt world : OpenGL.TextureMagFilter option = this.Get (nameof this.TextureMagFilterOpt) world
        member this.SetTextureMagFilterOpt (value : OpenGL.TextureMagFilter option) world = this.Set (nameof this.TextureMagFilterOpt) value world
        member this.TextureMagFilterOpt = lens (nameof this.TextureMagFilterOpt) this this.GetTextureMagFilterOpt this.SetTextureMagFilterOpt
        member this.GetRenderStyle world : RenderStyle = this.Get (nameof this.RenderStyle) world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set (nameof this.RenderStyle) value world
        member this.RenderStyle = lens (nameof this.RenderStyle) this this.GetRenderStyle this.SetRenderStyle

    type StaticBillboardFacet () =
        inherit Facet (false)

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

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let albedoImage = entity.GetAlbedoImage world
            let metallicImage = entity.GetMetallicImage world
            let roughnessImage = entity.GetRoughnessImage world
            let ambientOcclusionImage = entity.GetAmbientOcclusionImage world
            let emissionImage = entity.GetEmissionImage world
            let normalImage = entity.GetNormalImage world
            let heightImage = entity.GetHeightImage world
            let minFilterOpt = entity.GetTextureMinFilterOpt world
            let magFilterOpt = entity.GetTextureMagFilterOpt world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.enqueueRenderMessage3d
                (RenderBillboard
                    { Absolute = absolute; ModelMatrix = affineMatrix; InsetOpt = insetOpt; MaterialProperties = properties
                      AlbedoImage = albedoImage; MetallicImage = metallicImage; RoughnessImage = roughnessImage; AmbientOcclusionImage = ambientOcclusionImage; EmissionImage = emissionImage; NormalImage = normalImage; HeightImage = heightImage
                      MinFilterOpt = minFilterOpt; MagFilterOpt = magFilterOpt; RenderType = renderType })
                world

        override this.GetQuickSize (_, _) =
            v3 1.0f 1.0f 1.0f

        override this.RayCast (ray, entity, world) =
            // TODO: intersect against oriented quad rather than box.
            match this.TryGetHighlightBounds (entity, world) with
            | Some bounds ->
                let intersectionOpt = ray.Intersects bounds
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            let bounds = entity.GetBounds world
            Some bounds

[<AutoOpen>]
module BasicStaticBillboardEmitterFacetModule =

    type Entity with

        member this.GetEmitterMaterialProperties world : MaterialProperties = this.Get (nameof this.EmitterMaterialProperties) world
        member this.SetEmitterMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.EmitterMaterialProperties) value world
        member this.EmitterMaterialProperties = lens (nameof this.EmitterMaterialProperties) this this.GetEmitterMaterialProperties this.SetEmitterMaterialProperties
        member this.GetEmitterAlbedoImage world : Image AssetTag = this.Get (nameof this.EmitterAlbedoImage) world
        member this.SetEmitterAlbedoImage (value : Image AssetTag) world = this.Set (nameof this.EmitterAlbedoImage) value world
        member this.EmitterAlbedoImage = lens (nameof this.EmitterAlbedoImage) this this.GetEmitterAlbedoImage this.SetEmitterAlbedoImage
        member this.GetEmitterMetallicImage world : Image AssetTag = this.Get (nameof this.EmitterMetallicImage) world
        member this.SetEmitterMetallicImage (value : Image AssetTag) world = this.Set (nameof this.EmitterMetallicImage) value world
        member this.EmitterMetallicImage = lens (nameof this.EmitterMetallicImage) this this.GetEmitterMetallicImage this.SetEmitterMetallicImage
        member this.GetEmitterRoughnessImage world : Image AssetTag = this.Get (nameof this.EmitterRoughnessImage) world
        member this.SetEmitterRoughnessImage (value : Image AssetTag) world = this.Set (nameof this.EmitterRoughnessImage) value world
        member this.EmitterRoughnessImage = lens (nameof this.EmitterRoughnessImage) this this.GetEmitterRoughnessImage this.SetEmitterRoughnessImage
        member this.GetEmitterAmbientOcclusionImage world : Image AssetTag = this.Get (nameof this.EmitterAmbientOcclusionImage) world
        member this.SetEmitterAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof this.EmitterAmbientOcclusionImage) value world
        member this.EmitterAmbientOcclusionImage = lens (nameof this.EmitterAmbientOcclusionImage) this this.GetEmitterAmbientOcclusionImage this.SetEmitterAmbientOcclusionImage
        member this.GetEmitterEmissionImage world : Image AssetTag = this.Get (nameof this.EmitterEmissionImage) world
        member this.SetEmitterEmissionImage (value : Image AssetTag) world = this.Set (nameof this.EmitterEmissionImage) value world
        member this.EmitterEmissionImage = lens (nameof this.EmitterEmissionImage) this this.GetEmitterEmissionImage this.SetEmitterEmissionImage
        member this.GetEmitterNormalImage world : Image AssetTag = this.Get (nameof this.EmitterNormalImage) world
        member this.SetEmitterNormalImage (value : Image AssetTag) world = this.Set (nameof this.EmitterNormalImage) value world
        member this.EmitterNormalImage = lens (nameof this.EmitterNormalImage) this this.GetEmitterNormalImage this.SetEmitterNormalImage
        member this.GetEmitterHeightImage world : Image AssetTag = this.Get (nameof this.EmitterHeightImage) world
        member this.SetEmitterHeightImage (value : Image AssetTag) world = this.Set (nameof this.EmitterHeightImage) value world
        member this.EmitterHeightImage = lens (nameof this.EmitterHeightImage) this this.GetEmitterHeightImage this.SetEmitterHeightImage
        member this.GetEmitterMinFilterOpt world : OpenGL.TextureMinFilter option = this.Get (nameof this.EmitterMinFilterOpt) world
        member this.SetEmitterMinFilterOpt (value : OpenGL.TextureMinFilter option) world = this.Set (nameof this.EmitterMinFilterOpt) value world
        member this.EmitterMinFilterOpt = lens (nameof this.EmitterMinFilterOpt) this this.GetEmitterMinFilterOpt this.SetEmitterMinFilterOpt
        member this.GetEmitterMagFilterOpt world : OpenGL.TextureMagFilter option = this.Get (nameof this.EmitterMagFilterOpt) world
        member this.SetEmitterMagFilterOpt (value : OpenGL.TextureMagFilter option) world = this.Set (nameof this.EmitterMagFilterOpt) value world
        member this.EmitterMagFilterOpt = lens (nameof this.EmitterMagFilterOpt) this this.GetEmitterMagFilterOpt this.SetEmitterMagFilterOpt
        member this.GetEmitterRenderType world : RenderType = this.Get (nameof this.EmitterRenderType) world
        member this.SetEmitterRenderType (value : RenderType) world = this.Set (nameof this.EmitterRenderType) value world
        member this.EmitterRenderType = lens (nameof this.EmitterRenderType) this this.GetEmitterRenderType this.SetEmitterRenderType

    type BasicStaticBillboardEmitterFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) world =
            World.tryMakeEmitter
                (World.getGameTime world)
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
                    AlbedoImage = entity.GetEmitterAlbedoImage world
                    MetallicImage = entity.GetEmitterMetallicImage world
                    RoughnessImage = entity.GetEmitterRoughnessImage world
                    AmbientOcclusionImage = entity.GetEmitterAmbientOcclusionImage world
                    EmissionImage = entity.GetEmitterEmissionImage world
                    NormalImage = entity.GetEmitterNormalImage world
                    HeightImage = entity.GetEmitterHeightImage world
                    ParticleSeed = entity.GetBasicParticleSeed world
                    Constraint = entity.GetEmitterConstraint world }
            | None ->
                Particles.BasicStaticBillboardEmitter.makeEmpty
                    (World.getGameTime world)
                    (entity.GetEmitterLifeTimeOpt world)
                    (entity.GetParticleLifeTimeMaxOpt world)
                    (entity.GetParticleRate world)
                    (entity.GetParticleMax world)

        static let updateParticleSystem updater (entity : Entity) world =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = updater particleSystem
            let world = entity.SetParticleSystem particleSystem world
            world

        static let updateEmitter updater (entity : Entity) world =
            updateParticleSystem (fun particleSystem ->
                match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                    let emitter = updater emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem)
                entity world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterMaterialPropertiesChange evt world =
            let emitterMaterialProperties = evt.Data.Value :?> MaterialProperties
            let world = updateEmitter (fun emitter -> if emitter.MaterialProperties <> emitterMaterialProperties then { emitter with MaterialProperties = emitterMaterialProperties } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterAlbedoImageChange evt world =
            let emitterAlbedoImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.AlbedoImage emitterAlbedoImage then { emitter with AlbedoImage = emitterAlbedoImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMetallicImageChange evt world =
            let emitterMetallicImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.MetallicImage emitterMetallicImage then { emitter with MetallicImage = emitterMetallicImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterRoughnessImageChange evt world =
            let emitterRoughnessImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.RoughnessImage emitterRoughnessImage then { emitter with RoughnessImage = emitterRoughnessImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterAmbientOcclusionImageChange evt world =
            let emitterAmbientOcclusionImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.AmbientOcclusionImage emitterAmbientOcclusionImage then { emitter with AmbientOcclusionImage = emitterAmbientOcclusionImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterEmissionImageChange evt world =
            let emitterEmissionImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.EmissionImage emitterEmissionImage then { emitter with EmissionImage = emitterEmissionImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterNormalImageChange evt world =
            let emitterNormalImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.NormalImage emitterNormalImage then { emitter with NormalImage = emitterNormalImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterHeightImageChange evt world =
            let emitterHeightImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.HeightImage emitterHeightImage then { emitter with HeightImage = emitterHeightImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMinFilterOptChange evt world =
            let emitterMinFilterOpt = evt.Data.Value :?> OpenGL.TextureMinFilter option
            let world = updateEmitter (fun emitter -> if emitter.MinFilterOpt <> emitterMinFilterOpt then { emitter with MinFilterOpt = emitterMinFilterOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMagFilterOptChange evt world =
            let emitterMagFilterOpt = evt.Data.Value :?> OpenGL.TextureMagFilter option
            let world = updateEmitter (fun emitter -> if emitter.MagFilterOpt <> emitterMagFilterOpt then { emitter with MagFilterOpt = emitterMagFilterOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterRenderTypeChange evt world =
            let emitterRenderType = evt.Data.Value :?> RenderType
            let world = updateEmitter (fun emitter -> if emitter.RenderType <> emitterRenderType then { emitter with RenderType = emitterRenderType } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = updateEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = updateEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticBillboardEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = updateEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
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
             define Entity.EmitterAlbedoImage Assets.Default.MaterialAlbedo
             define Entity.EmitterMetallicImage Assets.Default.MaterialMetallic
             define Entity.EmitterRoughnessImage Assets.Default.MaterialRoughness
             define Entity.EmitterAmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.EmitterEmissionImage Assets.Default.MaterialEmission
             define Entity.EmitterNormalImage Assets.Default.MaterialNormal
             define Entity.EmitterHeightImage Assets.Default.MaterialHeight
             define Entity.EmitterMinFilterOpt None
             define Entity.EmitterMagFilterOpt None
             define Entity.EmitterLifeTimeOpt GameTime.zero
             define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
             define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = Constants.Engine.ParticleSize3dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicStaticBillboardEmitter"
             nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.monitor handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity world
            let world = World.monitor handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity world
            let world = World.monitor handleEmitterMaterialPropertiesChange (entity.GetChangeEvent (nameof entity.EmitterMaterialProperties)) entity world
            let world = World.monitor handleEmitterAlbedoImageChange (entity.GetChangeEvent (nameof entity.EmitterAlbedoImage)) entity world
            let world = World.monitor handleEmitterMetallicImageChange (entity.GetChangeEvent (nameof entity.EmitterMetallicImage)) entity world
            let world = World.monitor handleEmitterRoughnessImageChange (entity.GetChangeEvent (nameof entity.EmitterRoughnessImage)) entity world
            let world = World.monitor handleEmitterAmbientOcclusionImageChange (entity.GetChangeEvent (nameof entity.EmitterAmbientOcclusionImage)) entity world
            let world = World.monitor handleEmitterEmissionImageChange (entity.GetChangeEvent (nameof entity.EmitterEmissionImage)) entity world
            let world = World.monitor handleEmitterNormalImageChange (entity.GetChangeEvent (nameof entity.EmitterNormalImage)) entity world
            let world = World.monitor handleEmitterHeightImageChange (entity.GetChangeEvent (nameof entity.EmitterHeightImage)) entity world
            let world = World.monitor handleEmitterMinFilterOptChange (entity.GetChangeEvent (nameof entity.EmitterMinFilterOpt)) entity world
            let world = World.monitor handleEmitterMagFilterOptChange (entity.GetChangeEvent (nameof entity.EmitterMagFilterOpt)) entity world
            let world = World.monitor handleEmitterRenderTypeChange (entity.GetChangeEvent (nameof entity.EmitterRenderType)) entity world
            let world = World.monitor handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity world
            let world = World.monitor handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity world
            let world = World.monitor handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity world
            let world = World.monitor handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity world
            let world = World.monitor handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity world
            let world = World.monitor handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity world
            let world = World.monitor handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity world
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

        override this.Render (entity, world) =
            let time = World.getGameTime world
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                Particles.ParticleSystem.toParticlesDescriptors time |>
                List.map (fun descriptor ->
                    match descriptor with
                    | Particles.BillboardParticlesDescriptor descriptor ->
                        let emitterProperties = entity.GetEmitterMaterialProperties world
                        let materialProperties =
                            { AlbedoOpt = match emitterProperties.AlbedoOpt with ValueSome albedo -> ValueSome albedo | ValueNone -> descriptor.MaterialProperties.AlbedoOpt
                              MetallicOpt = match emitterProperties.MetallicOpt with ValueSome metallic -> ValueSome metallic | ValueNone -> descriptor.MaterialProperties.MetallicOpt
                              RoughnessOpt = match emitterProperties.RoughnessOpt with ValueSome roughness -> ValueSome roughness | ValueNone -> descriptor.MaterialProperties.RoughnessOpt
                              AmbientOcclusionOpt = match emitterProperties.AmbientOcclusionOpt with ValueSome ambientOcclusion -> ValueSome ambientOcclusion | ValueNone -> descriptor.MaterialProperties.AmbientOcclusionOpt
                              EmissionOpt = match emitterProperties.EmissionOpt with ValueSome emission -> ValueSome emission | ValueNone -> descriptor.MaterialProperties.EmissionOpt
                              HeightOpt = match emitterProperties.HeightOpt with ValueSome height -> ValueSome height | ValueNone -> descriptor.MaterialProperties.HeightOpt
                              InvertRoughnessOpt = match emitterProperties.InvertRoughnessOpt with ValueSome invertRoughness -> ValueSome invertRoughness | ValueNone -> descriptor.MaterialProperties.InvertRoughnessOpt }
                        Some
                            (RenderBillboardParticles
                                { Absolute = descriptor.Absolute
                                  MaterialProperties = materialProperties
                                  AlbedoImage = descriptor.AlbedoImage
                                  MetallicImage = descriptor.MetallicImage
                                  RoughnessImage = descriptor.RoughnessImage
                                  AmbientOcclusionImage = descriptor.AmbientOcclusionImage
                                  EmissionImage = descriptor.EmissionImage
                                  NormalImage = descriptor.NormalImage
                                  HeightImage = descriptor.HeightImage
                                  MinFilterOpt = descriptor.MinFilterOpt
                                  MagFilterOpt = descriptor.MagFilterOpt
                                  RenderType = descriptor.RenderType
                                  Particles = descriptor.Particles })
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

    type StaticModelFacet () =
        inherit Facet (false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let staticModel = entity.GetStaticModel world
            match entity.TryGetProperty (nameof Entity.BodyShape) world with
            | Some property when property.PropertyType = typeof<BodyShape> ->
                let bodyShape = property.PropertyValue :?> BodyShape
                if (match bodyShape with BodyStaticModel body -> body.StaticModel <> staticModel | _ -> false) then
                    let bodyStaticModel = { StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                    let world = entity.SetBodyShape (BodyStaticModel bodyStaticModel) world
                    (Cascade, world)
                else (Cascade, world)
            | _ -> (Cascade, world)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.RenderStyle Deferred
             define Entity.StaticModel Assets.Default.StaticModel]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let presence = transform.Presence
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            let staticModel = entity.GetStaticModel world
            World.enqueueRenderMessage3d
                (RenderStaticModel
                    { Absolute = absolute
                      ModelMatrix = affineMatrix
                      Presence = presence
                      InsetOpt = insetOpt
                      MaterialProperties = properties
                      RenderType = renderType
                      StaticModel = staticModel }) world

        override this.GetQuickSize (entity, world) =
            let staticModel = entity.GetStaticModel world
            let staticModelMetadata = Metadata.getStaticModelMetadata staticModel
            let bounds = staticModelMetadata.Bounds
            let boundsExtended = bounds.Combine bounds.Mirror
            boundsExtended.Size

        override this.RayCast (ray, entity, world) =
            let affineMatrix = entity.GetAffineMatrix world
            let inverseMatrix = Matrix4x4.Invert affineMatrix |> snd
            let rayEntity = ray.Transform inverseMatrix
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModel ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface = rayEntity.Transform inverse
                        let mutable bounds = geometry.Bounds
                        let boundsIntersectionOpt = raySurface.Intersects bounds
                        if boundsIntersectionOpt.HasValue then
                            raySurface.Intersects (geometry.Indices, geometry.Vertices) |>
                            Seq.map snd' |>
                            Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity) |>
                            Seq.map (fun pointEntity -> Vector3.Transform (pointEntity, affineMatrix)) |>
                            Seq.map (fun point -> (point - ray.Origin).Magnitude) |>
                            Seq.toArray
                        else [||])
                        staticModel.Surfaces
                Array.concat intersectionses
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModel ->
                let mutable boundsOpt = None
                for surface in staticModel.Surfaces do
                    let bounds2 = surface.SurfaceBounds.Transform surface.SurfaceMatrix
                    match boundsOpt with
                    | Some (bounds : Box3) -> boundsOpt <- Some (bounds.Combine bounds2)
                    | None -> boundsOpt <- Some bounds2
                match boundsOpt with
                | Some bounds -> Some (bounds.Transform (entity.GetAffineMatrix world))
                | None -> None
            | None -> None

[<AutoOpen>]
module StaticModelSurfaceFacetModule =

    type Entity with
        member this.GetSurfaceIndex world : int = this.Get (nameof this.SurfaceIndex) world
        member this.SetSurfaceIndex (value : int) world = this.Set (nameof this.SurfaceIndex) value world
        member this.SurfaceIndex = lens (nameof this.SurfaceIndex) this this.GetSurfaceIndex this.SetSurfaceIndex

    type StaticModelSurfaceFacet () =
        inherit Facet (false)

        static let updateBodyShape evt world =
            let entity = evt.Subscriber : Entity
            let surfaceIndex = entity.GetSurfaceIndex world
            let staticModel = entity.GetStaticModel world
            match entity.TryGetProperty (nameof Entity.BodyShape) world with
            | Some property when property.PropertyType = typeof<BodyShape> ->
                let bodyShape = property.PropertyValue :?> BodyShape
                if (match bodyShape with BodyStaticModelSurface body -> body.SurfaceIndex <> surfaceIndex || body.StaticModel <> staticModel | _ -> false) then
                    let bodyStaticModel = { SurfaceIndex = surfaceIndex; StaticModel = staticModel; TransformOpt = None; PropertiesOpt = None }
                    let world = entity.SetBodyShape (BodyStaticModelSurface bodyStaticModel) world
                    (Cascade, world)
                else (Cascade, world)
            | _ -> (Cascade, world)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

        override this.Register (entity, world) =
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.SurfaceIndex)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.StaticModel)) entity world
            let world = World.monitor updateBodyShape (entity.GetChangeEvent (nameof entity.BodyShape)) entity world
            world

        override this.Render (entity, world) =
            match entity.GetSurfaceIndex world with
            | -1 -> world
            | surfaceIndex ->
                let mutable transform = entity.GetTransform world
                let absolute = transform.Absolute
                let affineMatrix = transform.AffineMatrix
                let insetOpt = entity.GetInsetOpt world
                let properties = entity.GetMaterialProperties world
                let renderType =
                    match entity.GetRenderStyle world with
                    | Deferred -> DeferredRenderType
                    | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                let staticModel = entity.GetStaticModel world
                World.enqueueRenderMessage3d
                    (RenderStaticModelSurface
                        { Absolute = absolute
                          ModelMatrix = affineMatrix
                          InsetOpt = insetOpt
                          MaterialProperties = properties
                          RenderType = renderType
                          StaticModel = staticModel
                          SurfaceIndex = surfaceIndex })
                    world

        override this.GetQuickSize (entity, world) =
            let staticModel = Metadata.getStaticModelMetadata (entity.GetStaticModel world)
            let surfaceIndex = entity.GetSurfaceIndex world
            if surfaceIndex > -1 && surfaceIndex < staticModel.Surfaces.Length then
                let bounds = staticModel.Surfaces.[surfaceIndex].SurfaceBounds
                let boundsExtended = bounds.Combine bounds.Mirror
                boundsExtended.Size
            else Constants.Engine.EntitySize3dDefault

        override this.RayCast (ray, entity, world) =
            let rayEntity = ray.Transform (Matrix4x4.Invert (entity.GetAffineMatrix world) |> snd)
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModel ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex < staticModel.Surfaces.Length then
                    let surface = staticModel.Surfaces.[surfaceIndex]
                    let geometry = surface.PhysicallyBasedGeometry
                    let mutable bounds = geometry.Bounds
                    let boundsIntersectionOpt = rayEntity.Intersects bounds
                    if boundsIntersectionOpt.HasValue then
                        let intersections = rayEntity.Intersects (geometry.Indices, geometry.Vertices)
                        intersections |> Seq.map snd' |> Seq.toArray
                    else [||]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModel ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex < staticModel.Surfaces.Length then
                    let surface = staticModel.Surfaces.[surfaceIndex]
                    let bounds = surface.PhysicallyBasedGeometry.Bounds
                    Some (bounds.Transform (entity.GetAffineMatrix world))
                else None
            | None -> None