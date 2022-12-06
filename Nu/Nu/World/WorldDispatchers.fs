// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open TiledSharp
open Prime
open Nu

module Declarative =

    let Game = Unchecked.defaultof<Game>
    let Screen = Unchecked.defaultof<Screen>
    let Group = Unchecked.defaultof<Group>
    let Entity = Unchecked.defaultof<Entity>

open Declarative

[<AutoOpen>]
module DeclarativeOperators2 =

    type World with

        static member internal renderView view world =
            match view with
            | Render2d (elevation, horizon, assetTag, descriptor) ->
                let message = { Elevation = elevation; Horizon = horizon; AssetTag = AssetTag.generalize assetTag; RenderDescriptor2d = descriptor }
                World.enqueueRenderLayeredMessage2d message world
            | Render3d renderMessage -> World.enqueueRenderMessage3d renderMessage world; world
            | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
            | PlaySong (fadeIn, fadeOut, volume, start, assetTag) -> World.playSong fadeIn fadeOut volume start assetTag world
            | FadeOutSong fade -> World.fadeOutSong fade world
            | StopSong -> World.stopSong world
            | SpawnEmitter (_, _) -> world
            | Tag _ -> world
            | Views views -> Array.fold (fun world view -> World.renderView view world) world views
            | SegmentedViews views -> SegmentedArray.fold (fun world view -> World.renderView view world) world views

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

        static let handleScriptChanged evt world =
            let entity = evt.Subscriber : Entity
            let script = entity.GetScript world
            let scriptFrame = Scripting.DeclarationFrame StringComparer.Ordinal
            let world = World.setEntityScriptFrame scriptFrame entity world |> snd'
            let world = evalManyWithLogging script scriptFrame entity world |> snd'
            (Cascade, world)

        static let handleRegisterScriptChanged evt world =
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
             define Entity.UpdateScript Scripting.Unit
#if !DISABLE_ENTITY_POST_UPDATE
             define Entity.PostUpdateScript Scripting.Unit
#endif
             define Entity.RenderScript Scripting.Unit]

        override this.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetRegisterScript world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChanged (entity.GetChangeEvent (nameof entity.ScriptFrame)) entity world
            let world = World.monitor handleRegisterScriptChanged (entity.GetChangeEvent (nameof entity.RegisterScript)) entity world
            world

        override this.Unregister (entity, world) =
            World.evalWithLogging (entity.GetUnregisterScript world) (entity.GetScriptFrame world) entity world |> snd'

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
        member this.GetGlow world : Color = this.Get (nameof this.Glow) world
        member this.SetGlow (value : Color) world = this.Set (nameof this.Glow) value world
        member this.Glow = lens (nameof this.Glow) this this.GetGlow this.SetGlow
        member this.GetFlip world : Flip = this.Get (nameof this.Flip) world
        member this.SetFlip (value : Flip) world = this.Set (nameof this.Flip) value world
        member this.Flip = lens (nameof this.Flip) this this.GetFlip this.SetFlip

    type StaticSpriteFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Glow Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter
            let staticImage = entity.GetStaticImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = transform.Elevation
                  Horizon = perimeter.Position.Y
                  AssetTag = AssetTag.generalize staticImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
                          Image = staticImage
                          Color = entity.GetColor world
                          Blend = entity.GetBlend world
                          Glow = entity.GetGlow world
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
        member this.GetAnimationDelay world : int64 = this.Get (nameof this.AnimationDelay) world
        member this.SetAnimationDelay (value : int64) world = this.Set (nameof this.AnimationDelay) value world
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
                let cel = int (World.getUpdateTime world / entity.GetAnimationDelay world) % celCount
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
             define Entity.AnimationDelay 4L
             define Entity.AnimationSheet Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Glow Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter
            let animationSheet = entity.GetAnimationSheet world
            World.enqueueRenderLayeredMessage2d
                { Elevation = transform.Elevation
                  Horizon = perimeter.Position.Y
                  AssetTag = AssetTag.generalize animationSheet
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = match getSpriteInsetOpt entity world with Some inset -> ValueSome inset | None -> ValueNone
                          Image = animationSheet
                          Color = entity.GetColor world
                          Blend = entity.GetBlend world
                          Glow = entity.GetGlow world
                          Flip = entity.GetFlip world }}
                world

        override this.GetQuickSize (entity, world) =
            (entity.GetCelSize world).V3

[<AutoOpen>]
module TextFacetModule =

    type Entity with
        member this.GetText world : string = this.Get (nameof this.Text) world
        member this.SetText (value : string) world = this.Set (nameof this.Text) value world
        member this.Text = lens (nameof this.Text) this this.GetText this.SetText
        member this.GetFont world : Font AssetTag = this.Get (nameof this.Font) world
        member this.SetFont (value : Font AssetTag) world = this.Set (nameof this.Font) value world
        member this.Font = lens (nameof this.Font) this this.GetFont this.SetFont
        member this.GetMargins world : Vector3 = this.Get (nameof this.Margins) world
        member this.SetMargins (value : Vector3) world = this.Set (nameof this.Margins) value world
        member this.Margins = lens (nameof this.Margins) this this.GetMargins this.SetMargins
        member this.GetJustification world : Justification = this.Get (nameof this.Justification) world
        member this.SetJustification (value : Justification) world = this.Set (nameof this.Justification) value world
        member this.Justification = lens (nameof this.Justification) this this.GetJustification this.SetJustification
        member this.GetTextColor world : Color = this.Get (nameof this.TextColor) world
        member this.SetTextColor (value : Color) world = this.Set (nameof this.TextColor) value world
        member this.TextColor = lens (nameof this.TextColor) this this.GetTextColor this.SetTextColor
        member this.GetTextDisabledColor world : Color = this.Get (nameof this.TextDisabledColor) world
        member this.SetTextDisabledColor (value : Color) world = this.Set (nameof this.TextDisabledColor) value world
        member this.TextDisabledColor = lens (nameof this.TextDisabledColor) this this.GetTextDisabledColor this.SetTextDisabledColor
        member this.GetTextOffset world : Vector3 = this.Get (nameof this.TextOffset) world
        member this.SetTextOffset (value : Vector3) world = this.Set (nameof this.TextOffset) value world
        member this.TextOffset = lens (nameof this.TextOffset) this this.GetTextOffset this.SetTextOffset

    type TextFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Text ""
             define Entity.Font Assets.Default.Font
             define Entity.Margins v3Zero
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.TextColor Color.Black
             define Entity.TextDisabledColor (Color (0.25f, 0.25f, 0.25f, 0.75f))
             define Entity.TextOffset v3Zero]

        override this.Render (entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // gui currently ignores rotation and scale
                let horizon = perimeterUnscaled.Position.Y
                let mutable textTransform = Transform.makeDefault transform.Centered
                textTransform.Position <- perimeterUnscaled.Position + entity.GetMargins world + entity.GetTextOffset world
                textTransform.Size <- perimeterUnscaled.Size - entity.GetMargins world * 2.0f
                textTransform.Offset <- transform.Offset
                textTransform.Elevation <- transform.Elevation + 0.5f // lift text above parent
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world
                World.enqueueRenderLayeredMessage2d
                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize font
                      RenderDescriptor2d =
                        TextDescriptor
                            { Transform = textTransform
                              Text = text
                              Font = font
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                              Justification = entity.GetJustification world }}
                    world
            else world

[<AutoOpen>]
module BasicEmitter2dFacetModule =

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
        member this.GetEmitterLifeTimeOpt world : int64 = this.Get (nameof this.EmitterLifeTimeOpt) world
        member this.SetEmitterLifeTimeOpt (value : int64) world = this.Set (nameof this.EmitterLifeTimeOpt) value world
        member this.EmitterLifeTimeOpt = lens (nameof this.EmitterLifeTimeOpt) this this.GetEmitterLifeTimeOpt this.SetEmitterLifeTimeOpt
        member this.GetParticleLifeTimeMaxOpt world : int64 = this.Get (nameof this.ParticleLifeTimeMaxOpt) world
        member this.SetParticleLifeTimeMaxOpt (value : int64) world = this.Set (nameof this.ParticleLifeTimeMaxOpt) value world
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
        member this.GetParticleSystem world : ParticleSystem = this.Get (nameof this.ParticleSystem) world
        member this.SetParticleSystem (value : ParticleSystem) world = this.Set (nameof this.ParticleSystem) value world
        member this.ParticleSystem = lens (nameof this.ParticleSystem) this this.GetParticleSystem this.SetParticleSystem

    type BasicEmitter2dFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) world =
            World.tryMakeEmitter
                (World.getUpdateTime world)
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)
                (entity.GetEmitterStyle world)
                world |>
            Option.map cast<Particles.BasicEmitter>

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
                Particles.BasicEmitter2d.makeEmpty
                    (World.getUpdateTime world)
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
                match Map.tryFind typeof<Particles.BasicEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicEmitter as emitter) ->
                    let emitter = updater emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem)
                entity world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SegmentedArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterBlendChanged evt world =
            let emitterBlend = evt.Data.Value :?> Blend
            let world = updateEmitter (fun emitter -> if emitter.Blend <> emitterBlend then { emitter with Blend = emitterBlend } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterImageChanged evt world =
            let emitterImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.Image emitterImage then { emitter with Image = emitterImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChanged evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> int64
            let world = updateEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChanged evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> int64
            let world = updateEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChanged evt world =
            let particleRate = evt.Data.Value :?> single
            let world = updateEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChanged evt world =
            let particleMax = evt.Data.Value :?> int
            let world = updateEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicEmitter2d.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChanged evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = updateEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChanged evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChanged evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
            (Cascade, world)

        static let handlePositionChanged evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicEmitter as emitter) ->
                    let position = entity.GetPosition world
                    let emitter =
                        if v3Neq emitter.Body.Position position
                        then { emitter with Body = { emitter.Body with Position = position }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static let handleRotationChanged evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicEmitter as emitter) ->
                    let angles = entity.GetAngles world
                    let emitter =
                        if v3Neq emitter.Body.Angles angles
                        then { emitter with Body = { emitter.Body with Angles = angles }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.EmitterBlend Transparent
             define Entity.EmitterImage Assets.Default.Image
             define Entity.EmitterLifeTimeOpt 0L
             define Entity.ParticleLifeTimeMaxOpt 60L
             define Entity.ParticleRate 1.0f
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make 0L 60L; Body = Particles.Body.defaultBody2d; Size = Constants.Engine.ParticleSize2dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Glow = Color.Zero; Flip = FlipNone }
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicEmitter2d"
             nonPersistent Entity.ParticleSystem ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.monitor handlePositionChanged (entity.GetChangeEvent (nameof entity.Position)) entity world
            let world = World.monitor handleRotationChanged (entity.GetChangeEvent (nameof entity.Rotation)) entity world
            let world = World.monitor handleEmitterBlendChanged (entity.GetChangeEvent (nameof entity.EmitterBlend)) entity world
            let world = World.monitor handleEmitterImageChanged (entity.GetChangeEvent (nameof entity.EmitterImage)) entity world
            let world = World.monitor handleEmitterLifeTimeOptChanged (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity world
            let world = World.monitor handleParticleLifeTimeMaxOptChanged (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity world
            let world = World.monitor handleParticleRateChanged (entity.GetChangeEvent (nameof entity.ParticleRate)) entity world
            let world = World.monitor handleParticleMaxChanged (entity.GetChangeEvent (nameof entity.ParticleMax)) entity world
            let world = World.monitor handleBasicParticleSeedChanged (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity world
            let world = World.monitor handleEmitterConstraintChanged (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity world
            let world = World.monitor handleEmitterStyleChanged (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity world
            world

        override this.Unregister (entity, world) =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.remove typeof<Particles.BasicEmitter>.Name particleSystem.Emitters }
            entity.SetParticleSystem particleSystem world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let time = World.getUpdateTime world
                let particleSystem = entity.GetParticleSystem world
                let (particleSystem, output) = ParticleSystem.run time particleSystem
                let world = entity.SetParticleSystem particleSystem world
                processOutput output entity world
            else world

        override this.Render (entity, world) =
            let time = World.getUpdateTime world
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                ParticleSystem.toParticlesDescriptors time |>
                List.map (fun (descriptor : ParticlesDescriptor) ->
                    { Elevation = descriptor.Elevation
                      Horizon = descriptor.Horizon
                      AssetTag = AssetTag.generalize descriptor.Image
                      RenderDescriptor2d = ParticlesDescriptor descriptor })
            World.enqueueRenderLayeredMessages2d particlesMessages world

[<AutoOpen>]
module Effect2dFacetModule =

    type EffectTags =
        Map<string, Effects.Slice>

    type Entity with

        member this.GetEffectSymbolOpt world : Symbol AssetTag option = this.Get (nameof this.EffectSymbolOpt) world
        member this.SetEffectSymbolOpt (value : Symbol AssetTag option) world = this.Set (nameof this.EffectSymbolOpt) value world
        member this.EffectSymbolOpt = lens (nameof this.EffectSymbolOpt) this this.GetEffectSymbolOpt this.SetEffectSymbolOpt
        member this.GetEffectStartTimeOpt world : int64 option = this.Get (nameof this.EffectStartTimeOpt) world
        member this.SetEffectStartTimeOpt (value : int64 option) world = this.Set (nameof this.EffectStartTimeOpt) value world
        member this.EffectStartTimeOpt = lens (nameof this.EffectStartTimeOpt) this this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get (nameof this.EffectDefinitions) world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.Set (nameof this.EffectDefinitions) value world
        member this.EffectDefinitions = lens (nameof this.EffectDefinitions) this this.GetEffectDefinitions this.SetEffectDefinitions
        member this.GetEffect world : Effect = this.Get (nameof this.Effect) world
        member this.SetEffect (value : Effect) world = this.Set (nameof this.Effect) value world
        member this.Effect = lens (nameof this.Effect) this this.GetEffect this.SetEffect
        member this.GetEffectOffset world : Vector3 = this.Get (nameof this.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof this.EffectOffset) value world
        member this.EffectOffset = lens (nameof this.EffectOffset) this this.GetEffectOffset this.SetEffectOffset
        member this.GetEffectCentered world : bool = this.Get (nameof this.EffectCentered) world
        member this.SetEffectCentered (value : bool) world = this.Set (nameof this.EffectCentered) value world
        member this.EffectCentered = lens (nameof this.EffectCentered) this this.GetEffectCentered this.SetEffectCentered
        member this.GetEffectTags world : EffectTags = this.Get (nameof this.EffectTags) world
        member private this.SetEffectTags (value : EffectTags) world = this.Set (nameof this.EffectTags) value world
        member this.EffectTags = lensReadOnly (nameof this.EffectTags) this this.GetEffectTags
        member this.GetEffectHistoryMax world : int = this.Get (nameof this.EffectHistoryMax) world
        member this.SetEffectHistoryMax (value : int) world = this.Set (nameof this.EffectHistoryMax) value world
        member this.EffectHistoryMax = lens (nameof this.EffectHistoryMax) this this.GetEffectHistoryMax this.SetEffectHistoryMax
        member this.GetEffectHistory world : Effects.Slice Nito.Collections.Deque = this.Get (nameof this.EffectHistory) world
        member private this.SetEffectHistory (value : Effects.Slice Nito.Collections.Deque) world = this.Set (nameof this.EffectHistory) value world
        member this.EffectHistory = lensReadOnly (nameof this.EffectHistory) this this.GetEffectHistory

        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetEffectStartTimeOpt world with
            | Some effectStartTime -> effectStartTime
            | None -> 0L

        /// The time relative to the start of the effect.
        member this.GetEffectTime world =
            let startTime = this.GetEffectStartTime world
            let time = World.getUpdateTime world
            time - startTime

    type Effect2dFacet () =
        inherit Facet (false)

        static let updateParticleSystem updater (entity : Entity) world =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = updater particleSystem
            let world = entity.SetParticleSystem particleSystem world
            world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SegmentedArray.fold (fun world output -> processOutput output entity world) world outputs

        static let setEffect effectSymbolOpt (entity : Entity) world =
            match effectSymbolOpt with
            | Some effectSymbol ->
                let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
                match World.assetTagToValueOpt<Effect> effectSymbol symbolLoadMetadata world with
                | Some effect -> entity.SetEffect effect world
                | None -> world
            | None -> world

        static let handleEffectsChanged evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.EffectSymbolOpt None
             define Entity.EffectStartTimeOpt None
             define Entity.EffectDefinitions Map.empty
             define Entity.Effect Effect.empty
             define Entity.EffectOffset v3Zero
             define Entity.EffectCentered true
             nonPersistent Entity.EffectTags Map.empty
             define Entity.EffectHistoryMax Constants.Effects.EffectHistoryMaxDefault
             define Entity.ParticleSystem ParticleSystem.empty
             variable Entity.EffectHistory (fun _ -> Nito.Collections.Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let time = World.getUpdateTime world
                let effect = entity.GetEffect world
                let particleSystem = entity.GetParticleSystem world
                let (particleSystem, output) = ParticleSystem.run time particleSystem
                let world = entity.SetParticleSystem particleSystem world
                let world = processOutput output entity world
                match (entity.GetSelfDestruct world, effect.LifeTimeOpt) with
                | (true, Some lifetime) ->
                    let effectTime = entity.GetEffectTime world
                    if  effectTime >= dec lifetime && // NOTE: dec keeps effect from rendering past the last frame when it is created mid-frame
                        (match ParticleSystem.getLiveness time particleSystem with Live -> false | Dead -> true) then
                        World.destroyEntity entity world
                    else world
                | (_, _) -> world
            else world

        override this.Render (entity, world) =

            // set up effect system to evaluate effect
            let time = World.getUpdateTime world
            let world = entity.SetEffectTags Map.empty world
            let mutable transform = entity.GetTransform world
            let effect = entity.GetEffect world
            let effectTime = entity.GetEffectTime world
            let effectAbsolute = entity.GetAbsolute world
            let effectSlice =
                { Effects.Position = transform.Position
                  Effects.Scale = transform.Scale
                  Effects.Angles = transform.Angles
                  Effects.Elevation = transform.Elevation
                  Effects.Offset = entity.GetEffectOffset world
                  Effects.Size = transform.Size
                  Effects.Inset = Box2.Zero
                  Effects.Color = Color.One
                  Effects.Blend = Transparent
                  Effects.Glow = Color.Zero
                  Effects.Flip = FlipNone
                  Effects.Volume = Constants.Audio.SoundVolumeDefault
                  Effects.Enabled = true
                  Effects.Centered = entity.GetEffectCentered world }
            let effectHistory = entity.GetEffectHistory world
            let effectDefinitions = entity.GetEffectDefinitions world
            let effectSystem = EffectSystem.make effectAbsolute effectTime effectDefinitions

            // evaluate effect with effect system
            let (view, _) = EffectSystem.eval effect effectSlice effectHistory effectSystem

            // render effect view
            let world = World.renderView view world

            // convert view to array for storing tags and spawning emitters
            let views = View.toSeq view

            // store tags
            let tags =
                views |>
                Seq.choose (function Tag (name, value) -> Some (name, value :?> Effects.Slice) | _ -> None) |>
                Map.ofSeq
            let world = entity.SetEffectTags tags world

            // spawn emitters
            let particleSystem =
                views |>
                Seq.choose (function SpawnEmitter (name, descriptor) -> Some (name, descriptor) | _ -> None) |>
                Seq.choose (fun (name : string, descriptor : EmitterDescriptor) ->
                    match descriptor with
                    | :? Particles.BasicEmitterDescriptor as descriptor ->
                        match World.tryMakeEmitter time descriptor.LifeTimeOpt descriptor.ParticleLifeTimeMaxOpt descriptor.ParticleRate descriptor.ParticleMax descriptor.Style world with
                        | Some (:? Particles.BasicEmitter as emitter) ->
                            let emitter =
                                { emitter with
                                    Body = descriptor.Body
                                    Blend = descriptor.Blend
                                    Image = descriptor.Image
                                    ParticleSeed = descriptor.ParticleSeed
                                    Constraint = descriptor.Constraint }
                            Some (name, emitter)
                        | _ -> None
                    | _ -> None) |>
                Seq.fold (fun particleSystem (name, emitter) ->
                    ParticleSystem.add name emitter particleSystem)
                    (entity.GetParticleSystem world)

            // render particles
            let particlesMessages =
                particleSystem |>
                ParticleSystem.toParticlesDescriptors time |>
                List.map (fun (descriptor : ParticlesDescriptor) ->
                    { Elevation = descriptor.Elevation
                      Horizon = descriptor.Horizon
                      AssetTag = AssetTag.generalize descriptor.Image
                      RenderDescriptor2d = ParticlesDescriptor descriptor })
            let world = World.enqueueRenderLayeredMessages2d particlesMessages world

            // update effect history in-place
            effectHistory.AddToFront effectSlice
            if effectHistory.Count > entity.GetEffectHistoryMax world then effectHistory.RemoveFromBack () |> ignore
            world

        override this.Register (entity, world) =
            let effectStartTime = Option.defaultValue (World.getUpdateTime world) (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.monitor handleEffectsChanged (entity.GetChangeEvent (nameof entity.EffectSymbolOpt)) entity world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
        member this.GetBodyEnabled world : bool = this.Get (nameof this.BodyEnabled) world
        member this.SetBodyEnabled (value : bool) world = this.Set (nameof this.BodyEnabled) value world
        member this.BodyEnabled = lens (nameof this.BodyEnabled) this this.GetBodyEnabled this.SetBodyEnabled
        member this.GetBodyType world : BodyType = this.Get (nameof this.BodyType) world
        member this.SetBodyType (value : BodyType) world = this.Set (nameof this.BodyType) value world
        member this.BodyType = lens (nameof this.BodyType) this this.GetBodyType this.SetBodyType
        member this.GetAwake world : bool = this.Get (nameof this.Awake) world
        member this.SetAwake (value : bool) world = this.Set (nameof this.Awake) value world
        member this.Awake = lens (nameof this.Awake) this this.GetAwake this.SetAwake
        member this.GetDensity world : single = this.Get (nameof this.Density) world
        member this.SetDensity (value : single) world = this.Set (nameof this.Density) value world
        member this.Density = lens (nameof this.Density) this this.GetDensity this.SetDensity
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
        member this.GetFixedRotation world : bool = this.Get (nameof this.FixedRotation) world
        member this.SetFixedRotation (value : bool) world = this.Set (nameof this.FixedRotation) value world
        member this.FixedRotation = lens (nameof this.FixedRotation) this this.GetFixedRotation this.SetFixedRotation
        member this.GetInertia world : single = this.Get (nameof this.Inertia) world
        member this.SetInertia (value : single) world = this.Set (nameof this.Inertia) value world
        member this.Inertia = lens (nameof this.Inertia) this this.GetInertia this.SetInertia
        member this.GetGravityScale world : single = this.Get (nameof this.GravityScale) world
        member this.SetGravityScale (value : single) world = this.Set (nameof this.GravityScale) value world
        member this.GravityScale = lens (nameof this.GravityScale) this this.GetGravityScale this.SetGravityScale
        member this.GetCollisionCategories world : string = this.Get (nameof this.CollisionCategories) world
        member this.SetCollisionCategories (value : string) world = this.Set (nameof this.CollisionCategories) value world
        member this.CollisionCategories = lens (nameof this.CollisionCategories) this this.GetCollisionCategories this.SetCollisionCategories
        member this.GetCollisionMask world : string = this.Get (nameof this.CollisionMask) world
        member this.SetCollisionMask (value : string) world = this.Set (nameof this.CollisionMask) value world
        member this.CollisionMask = lens (nameof this.CollisionMask) this this.GetCollisionMask this.SetCollisionMask
        member this.GetBodyShape world : BodyShape = this.Get (nameof this.BodyShape) world
        member this.SetBodyShape (value : BodyShape) world = this.Set (nameof this.BodyShape) value world
        member this.BodyShape = lens (nameof this.BodyShape) this this.GetBodyShape this.SetBodyShape
        member this.GetIgnoreCCD world : bool = this.Get (nameof this.IgnoreCCD) world
        member this.SetIgnoreCCD (value : bool) world = this.Set (nameof this.IgnoreCCD) value world
        member this.IgnoreCCD = lens (nameof this.IgnoreCCD) this this.GetIgnoreCCD this.SetIgnoreCCD
        member this.GetBullet world : bool = this.Get (nameof this.Bullet) world
        member this.SetBullet (value : bool) world = this.Set (nameof this.Bullet) value world
        member this.Bullet = lens (nameof this.Bullet) this this.GetBullet this.SetBullet
        member this.GetSensor world : bool = this.Get (nameof this.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof this.Sensor) value world
        member this.Sensor = lens (nameof this.Sensor) this this.GetSensor this.SetSensor
        member this.GetPhysicsId world : PhysicsId = this.Get (nameof this.PhysicsId) world
        member this.PhysicsId = lensReadOnly (nameof this.PhysicsId) this this.GetPhysicsId
        member this.BodyCollisionEvent = Events.BodyCollision --> this
        member this.BodySeparationEvent = Events.BodySeparation --> this
        member this.BodyTransformEvent = Events.BodyTransform --> this

    type RigidBodyFacet () =
        inherit Facet (true)

        static let getBodyShape (entity : Entity) world =
            World.localizeBodyShape (entity.GetScale world * entity.GetSize world) (entity.GetBodyShape world) world

        static member Properties =
            [define Entity.BodyEnabled true
             define Entity.BodyType Dynamic
             define Entity.Awake true
             define Entity.Density Constants.Physics.DensityDefault
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.LinearVelocity v3Zero
             define Entity.LinearDamping 0.0f
             define Entity.AngularVelocity v3Zero
             define Entity.AngularDamping 0.0f
             define Entity.FixedRotation false
             define Entity.Inertia 0.0f
             define Entity.GravityScale 1.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.BodyShape (BodyBox { Extent = v3 0.5f 0.5f 0.0f; Center = v3Zero; PropertiesOpt = None })
             define Entity.IgnoreCCD false
             define Entity.Bullet false
             define Entity.Sensor false
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = 0UL }) None]

        override this.Register (entity, world) =
            // TODO: check if would should include monitoring for Offset and Centered changes.
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Position)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Scale)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Rotation)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyType)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Awake)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Density)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.LinearVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.LinearDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.FixedRotation)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Inertia)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.GravityScale)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyShape)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.IgnoreCCD)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Bullet)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Sensor)) entity world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let bodyProperties =
                { BodyId = (entity.GetPhysicsId world).CorrelationId
                  Position = transform.Center
                  Rotation = transform.Rotation
                  BodyShape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  Awake = entity.GetAwake world
                  Enabled = entity.GetBodyEnabled world
                  Density = entity.GetDensity world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  FixedRotation = entity.GetFixedRotation world
                  Inertia = entity.GetInertia world
                  GravityScale = entity.GetGravityScale world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  IgnoreCCD = entity.GetIgnoreCCD world
                  Bullet = entity.GetBullet world
                  Sensor = entity.GetSensor world }
            World.createBody entity (entity.GetId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

[<AutoOpen>]
module JointFacetModule =

    type Entity with
        member this.GetJointDevice world : JointDevice = this.Get (nameof this.JointDevice) world
        member this.SetJointDevice (value : JointDevice) world = this.Set (nameof this.JointDevice) value world
        member this.JointDevice = lens (nameof this.JointDevice) this this.GetJointDevice this.SetJointDevice

    type JointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.JointDevice JointEmpty
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = 0UL }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.JointDevice)) entity world
            world

        override this.RegisterPhysics (entity, world) =
            let jointProperties =
                { JointId = (entity.GetPhysicsId world).CorrelationId
                  JointDevice = (entity.GetJointDevice world) }
            World.createJoint entity (entity.GetId world) jointProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyJoint (entity.GetPhysicsId world) world

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
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = 0UL }) None]

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
                let tileMapPosition = perimeterUnscaled.Position.V2
                let tileMapDescriptor = TmxMap.getDescriptor tileMapPosition tileMap
                let bodyProperties =
                    TmxMap.getBodyProperties
                        transform.Enabled
                        (entity.GetFriction world)
                        (entity.GetRestitution world)
                        (entity.GetCollisionCategories world)
                        (entity.GetCollisionMask world)
                        (entity.GetPhysicsId world).CorrelationId
                        tileMapDescriptor
                World.createBody entity (entity.GetId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

        override this.Render (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) with
            | Some tileMap ->
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
                let viewBounds = World.getViewBounds2d world
                let tileMapMessages =
                    TmxMap.getLayeredMessages2d
                        (World.getUpdateTime world)
                        transform.Absolute
                        viewBounds
                        perimeterUnscaled.Position.V2
                        transform.Elevation
                        (entity.GetColor world)
                        (entity.GetGlow world)
                        (entity.GetTileLayerClearance world)
                        (entity.GetTileIndexOffset world)
                        (entity.GetTileIndexOffsetRange world)
                        tileMap
                World.enqueueRenderLayeredMessages2d tileMapMessages world
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
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TmxMap (TmxMap.makeDefault ())
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = 0UL }) None]

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
            let tmxMapPosition = perimeterUnscaled.Position.V2
            let tmxMapDescriptor = TmxMap.getDescriptor tmxMapPosition tmxMap
            let bodyProperties =
                TmxMap.getBodyProperties
                    transform.Enabled
                    (entity.GetFriction world)
                    (entity.GetRestitution world)
                    (entity.GetCollisionCategories world)
                    (entity.GetCollisionMask world)
                    (entity.GetPhysicsId world).CorrelationId
                    tmxMapDescriptor
            World.createBody entity (entity.GetId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
            let viewBounds = World.getViewBounds2d world
            let tmxMap = entity.GetTmxMap world
            let tmxMapMessages =
                TmxMap.getLayeredMessages2d
                    (World.getUpdateTime world)
                    transform.Absolute
                    viewBounds
                    perimeterUnscaled.Position.V2
                    transform.Elevation
                    (entity.GetColor world)
                    (entity.GetGlow world)
                    (entity.GetTileLayerClearance world)
                    (entity.GetTileIndexOffset world)
                    (entity.GetTileIndexOffsetRange world)
                    tmxMap
            World.enqueueRenderLayeredMessages2d tmxMapMessages world

        override this.GetQuickSize (entity, world) =
            let tmxMap = entity.GetTmxMap world
            TmxMap.getQuickSize tmxMap

[<AutoOpen>]
module SkyBoxFacetModule =

    type Entity with
        member this.GetCubeMap world : CubeMap AssetTag = this.Get (nameof this.CubeMap) world
        member this.SetCubeMap (value : CubeMap AssetTag) world = this.Set (nameof this.CubeMap) value world
        member this.CubeMap = lens (nameof this.CubeMap) this this.GetCubeMap this.SetCubeMap

    type SkyBoxFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

        override this.Render (entity, world) =
            let cubeMap = entity.GetCubeMap world
            World.enqueueRenderMessage3d (RenderSkyBoxMessage cubeMap) world
            world

[<AutoOpen>]
module LightFacet3dModule =

    type Entity with
        member this.GetBrightness world : single = this.Get (nameof this.Brightness) world
        member this.SetBrightness (value : single) world = this.Set (nameof this.Brightness) value world
        member this.Brightness = lens (nameof this.Brightness) this this.GetBrightness this.SetBrightness
        member this.GetIntensity world : single = this.Get (nameof this.Intensity) world
        member this.SetIntensity (value : single) world = this.Set (nameof this.Intensity) value world
        member this.Intensity = lens (nameof this.Intensity) this this.GetIntensity this.SetIntensity
        member this.GetLightType world : LightType = this.Get (nameof this.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof this.LightType) value world
        member this.LightType = lens (nameof this.LightType) this this.GetLightType this.SetLightType

    type LightFacet3d () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness 1000.0f
             define Entity.Intensity 1.0f
             define Entity.LightType PointLight]

        override this.Render (entity, world) =
            let position = entity.GetPosition world
            let color = entity.GetColor world
            let brightness = entity.GetBrightness world
            let intensity = entity.GetIntensity world
            let lightType = entity.GetLightType world
            World.enqueueRenderMessage3d (RenderLightMessage3d (position, color, brightness, intensity, lightType)) world
            world

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
        | Forward of single * single

    type Entity with
        member this.GetAlbedoOpt world : Color option = this.Get (nameof this.AlbedoOpt) world
        member this.SetAlbedoOpt (value : Color option) world = this.Set (nameof this.AlbedoOpt) value world
        member this.AlbedoOpt = lens (nameof this.AlbedoOpt) this this.GetAlbedoOpt this.SetAlbedoOpt
        member this.GetAlbedoImage world : Image AssetTag = this.Get (nameof this.AlbedoImage) world
        member this.SetAlbedoImage (value : Image AssetTag) world = this.Set (nameof this.AlbedoImage) value world
        member this.AlbedoImage = lens (nameof this.AlbedoImage) this this.GetAlbedoImage this.SetAlbedoImage
        member this.GetMetalnessOpt world : single option = this.Get (nameof this.MetalnessOpt) world
        member this.SetMetalnessOpt (value : single option) world = this.Set (nameof this.MetalnessOpt) value world
        member this.MetalnessOpt = lens (nameof this.MetalnessOpt) this this.GetMetalnessOpt this.SetMetalnessOpt
        member this.GetMetalnessImage world : Image AssetTag = this.Get (nameof this.MetalnessImage) world
        member this.SetMetalnessImage (value : Image AssetTag) world = this.Set (nameof this.MetalnessImage) value world
        member this.MetalnessImage = lens (nameof this.MetalnessImage) this this.GetMetalnessImage this.SetMetalnessImage
        member this.GetRoughnessOpt world : single option = this.Get (nameof this.RoughnessOpt) world
        member this.SetRoughnessOpt (value : single option) world = this.Set (nameof this.RoughnessOpt) value world
        member this.RoughnessOpt = lens (nameof this.RoughnessOpt) this this.GetRoughnessOpt this.SetRoughnessOpt
        member this.GetRoughnessImage world : Image AssetTag = this.Get (nameof this.RoughnessImage) world
        member this.SetRoughnessImage (value : Image AssetTag) world = this.Set (nameof this.RoughnessImage) value world
        member this.RoughnessImage = lens (nameof this.RoughnessImage) this this.GetRoughnessImage this.SetRoughnessImage
        member this.GetAmbientOcclusionImage world : Image AssetTag = this.Get (nameof this.AmbientOcclusionImage) world
        member this.SetAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof this.AmbientOcclusionImage) value world
        member this.AmbientOcclusionImage = lens (nameof this.AmbientOcclusionImage) this this.GetAmbientOcclusionImage this.SetAmbientOcclusionImage
        member this.GetAmbientOcclusionOpt world : single option = this.Get (nameof this.AmbientOcclusionOpt) world
        member this.SetAmbientOcclusionOpt (value : single option) world = this.Set (nameof this.AmbientOcclusionOpt) value world
        member this.AmbientOcclusionOpt = lens (nameof this.AmbientOcclusionOpt) this this.GetAmbientOcclusionOpt this.SetAmbientOcclusionOpt
        member this.GetNormalImage world : Image AssetTag = this.Get (nameof this.NormalImage) world
        member this.SetNormalImage (value : Image AssetTag) world = this.Set (nameof this.NormalImage) value world
        member this.NormalImage = lens (nameof this.NormalImage) this this.GetNormalImage this.SetNormalImage
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
             define Entity.AlbedoOpt None
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.MetalnessOpt None
             define Entity.MetalnessImage Assets.Default.MaterialMetalness
             define Entity.RoughnessOpt None
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.AmbientOcclusionOpt None
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.TextureMinFilterOpt None
             define Entity.TextureMagFilterOpt None
             define Entity.RenderStyle Deferred]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone // TODO: 3D: make converstion fn from option to voption and vice versa.
            let renderMaterial =
                { AlbedoOpt = match entity.GetAlbedoOpt world with Some albedo -> ValueSome albedo | None -> ValueNone
                  MetalnessOpt = match entity.GetMetalnessOpt world with Some metalness -> ValueSome metalness | None -> ValueNone
                  RoughnessOpt = match entity.GetRoughnessOpt world with Some roughness -> ValueSome roughness | None -> ValueNone
                  AmbientOcclusionOpt = match entity.GetAmbientOcclusionOpt world with Some ambientOcclusion -> ValueSome ambientOcclusion | None -> ValueNone }
            let albedoImage = entity.GetAlbedoImage world
            let metalnessImage = entity.GetMetalnessImage world
            let roughnessImage = entity.GetRoughnessImage world
            let ambientOcclusionImage = entity.GetAmbientOcclusionImage world
            let normalImage = entity.GetNormalImage world
            let minFilterOpt = match entity.GetTextureMinFilterOpt world with Some filter -> ValueSome filter | None -> ValueNone
            let magFilterOpt = match entity.GetTextureMagFilterOpt world with Some filter -> ValueSome filter | None -> ValueNone
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (sort, subsort) -> ForwardRenderType (sort, subsort)
            World.enqueueRenderMessage3d
                (RenderBillboardMessage
                    (absolute, affineMatrix, insetOpt, renderMaterial,
                     albedoImage, metalnessImage, roughnessImage, ambientOcclusionImage, normalImage,
                     minFilterOpt, magFilterOpt, renderType))
                world
            world

        override this.GetQuickSize (_, _) =
            v3 1.0f 2.0f 1.0f

        override this.RayCast (ray, entity, world) =
            // TODO: 3D: intersect against oriented quad rather than box.
            match this.TryGetHighlightBounds (entity, world) with
            | Some bounds ->
                let intersectionOpt = ray.Intersects bounds
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            let bounds = entity.GetBounds world
            Some
                (box3
                    (bounds.Position + bounds.Size * v3 0.0f 0.5f 0.0f)
                    (bounds.Size * v3 1.0f 0.5f 1.0f))

[<AutoOpen>]
module StaticModelFacetModule =

    type Entity with
        member this.GetStaticModel world : StaticModel AssetTag = this.Get (nameof this.StaticModel) world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set (nameof this.StaticModel) value world
        member this.StaticModel = lens (nameof this.StaticModel) this this.GetStaticModel this.SetStaticModel

    type StaticModelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.AlbedoOpt None
             define Entity.MetalnessOpt None
             define Entity.RoughnessOpt None
             define Entity.AmbientOcclusionOpt None
             define Entity.RenderStyle Deferred]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrix = transform.AffineMatrix
            let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
            let staticModel = entity.GetStaticModel world
            let renderMaterial =
                { AlbedoOpt = match entity.GetAlbedoOpt world with Some albedo -> ValueSome albedo | None -> ValueNone
                  MetalnessOpt = match entity.GetMetalnessOpt world with Some metalness -> ValueSome metalness | None -> ValueNone
                  RoughnessOpt = match entity.GetRoughnessOpt world with Some roughness -> ValueSome roughness | None -> ValueNone
                  AmbientOcclusionOpt = match entity.GetAmbientOcclusionOpt world with Some ambientOcclusion -> ValueSome ambientOcclusion | None -> ValueNone }
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (sort, subsort) -> ForwardRenderType (sort, subsort)
            World.enqueueRenderMessage3d (RenderStaticModelMessage (absolute, affineMatrix, insetOpt, renderMaterial, renderType, staticModel)) world
            world

        override this.GetQuickSize (entity, world) =
            let staticModel = entity.GetStaticModel world
            let staticModelMetadata = Metadata.getStaticModelMetadata staticModel
            let bounds = staticModelMetadata.Bounds
            let boundsExtended = bounds.Combine bounds.Mirror
            boundsExtended.Size

        override this.RayCast (ray, entity, world) =
            let rayEntity = ray.Transform (Matrix4x4.Invert (entity.GetAffineMatrix world) |> snd)
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
                            let intersections = raySurface.Intersects (geometry.Indices, geometry.Vertices)
                            intersections |> Seq.map snd' |> Seq.toArray
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

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.AlbedoOpt None
             define Entity.MetalnessOpt None
             define Entity.RoughnessOpt None
             define Entity.AmbientOcclusionOpt None
             define Entity.RenderStyle Deferred]

        override this.Render (entity, world) =
            match entity.GetSurfaceIndex world with
            | -1 -> world
            | surfaceIndex ->
                let mutable transform = entity.GetTransform world
                let absolute = transform.Absolute
                let affineMatrix = transform.AffineMatrix
                let staticModel = entity.GetStaticModel world
                let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
                let renderMaterial =
                    { AlbedoOpt = match entity.GetAlbedoOpt world with Some albedo -> ValueSome albedo | None -> ValueNone
                      MetalnessOpt = match entity.GetMetalnessOpt world with Some metalness -> ValueSome metalness | None -> ValueNone
                      RoughnessOpt = match entity.GetRoughnessOpt world with Some roughness -> ValueSome roughness | None -> ValueNone
                      AmbientOcclusionOpt = match entity.GetAmbientOcclusionOpt world with Some ambientOcclusion -> ValueSome ambientOcclusion | None -> ValueNone }
                let renderType =
                    match entity.GetRenderStyle world with
                    | Deferred -> DeferredRenderType
                    | Forward (sort, subsort) -> ForwardRenderType (sort, subsort)
                World.enqueueRenderMessage3d (RenderStaticModelSurfaceMessage (absolute, affineMatrix, insetOpt, renderMaterial, renderType, staticModel, surfaceIndex)) world
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

[<AutoOpen>]
module EntityDispatcherModule =

    /// A 2d entity dispatcher.
    type EntityDispatcher2d (centered, physical) =
        inherit EntityDispatcher (true, centered, physical)

        static member Properties =
            [define Entity.Centered false
             define Entity.Size Constants.Engine.EntitySize2dDefault]

    /// A 3d entity dispatcher.
    type EntityDispatcher3d (centered, physical) =
        inherit EntityDispatcher (false, centered, physical)

        static member Properties =
            [define Entity.Size Constants.Engine.EntitySize3dDefault]

[<AutoOpen>]
module StaticSpriteDispatcherModule =

    type StaticSpriteDispatcher () =
        inherit EntityDispatcher2d (false, false)

        static member Facets =
            [typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

[<AutoOpen>]
module AnimatedSpriteDispatcherModule =

    type AnimatedSpriteDispatcher () =
        inherit EntityDispatcher2d (false, false)

        static member Facets =
            [typeof<AnimatedSpriteFacet>]

        static member Properties =
            [define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelRun 4
             define Entity.CelCount 16
             define Entity.AnimationDelay 4L
             define Entity.AnimationSheet Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.Flip FlipNone]

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
        member this.GetDisabledColor world : Color = this.Get (nameof this.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof this.DisabledColor) value world
        member this.DisabledColor = lens (nameof this.DisabledColor) this this.GetDisabledColor this.SetDisabledColor

    type GuiDispatcher () =
        inherit EntityDispatcher2d (false, false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Presence Omnipresent
             define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))]

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
        member this.GetDown world : bool = this.Get (nameof this.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof this.Down) value world
        member this.Down = lens (nameof this.Down) this this.GetDown this.SetDown
        member this.GetDownTextOffset world : Vector3 = this.Get (nameof this.DownTextOffset) world
        member this.SetDownTextOffset (value : Vector3) world = this.Set (nameof this.DownTextOffset) value world
        member this.DownTextOffset = lens (nameof this.DownTextOffset) this this.GetDownTextOffset this.SetDownTextOffset
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
        member this.UpEvent = Events.Up --> this
        member this.DownEvent = Events.Down --> this
        member this.ClickEvent = Events.Click --> this

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetDown true world
                        let world = entity.SetTextOffset (entity.GetDownTextOffset world) world
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus () (Events.Down --> entity) eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasDown = entity.GetDown world
            let world = entity.SetDown false world
            let world = entity.SetTextOffset v3Zero world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasDown then
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publishPlus () (Events.Up --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ButtonDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publishPlus () (Events.Click --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Down false
             define Entity.DownTextOffset v3Zero
             define Entity.UpImage Assets.Default.Image
             define Entity.DownImage Assets.Default.Image2
             define Entity.ClickSoundOpt (Some Assets.Default.Sound)
             define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Position.Y
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
        member this.GetLabelImage world : Image AssetTag = this.Get (nameof this.LabelImage) world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set (nameof this.LabelImage) value world
        member this.LabelImage = lens (nameof this.LabelImage) this this.GetLabelImage this.SetLabelImage

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.LabelImage Assets.Default.Image3]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage = entity.GetLabelImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Position.Y
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetLabelImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
        member this.GetBackgroundImageOpt world : Image AssetTag option = this.Get (nameof this.BackgroundImageOpt) world
        member this.SetBackgroundImageOpt (value : Image AssetTag option) world = this.Set (nameof this.BackgroundImageOpt) value world
        member this.BackgroundImageOpt = lens (nameof this.BackgroundImageOpt) this this.GetBackgroundImageOpt this.SetBackgroundImageOpt

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.BackgroundImageOpt None
             define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

        override this.Render (entity, world) =
            match entity.GetBackgroundImageOpt world with
            | Some spriteImage ->
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
                World.enqueueRenderLayeredMessage2d
                    { Elevation = spriteTransform.Elevation
                      Horizon = spriteTransform.Position.Y
                      AssetTag = AssetTag.generalize spriteImage
                      RenderDescriptor2d =
                        SpriteDescriptor
                            { Transform = spriteTransform
                              InsetOpt = ValueNone
                              Image = spriteImage
                              Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                              Blend = Transparent
                              Glow = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> world

        override this.GetQuickSize (entity, world) =
            match entity.GetBackgroundImageOpt world with
            | Some image ->
                match Metadata.tryGetTextureSizeF image with
                | Some size -> size.V3
                | None -> Constants.Engine.EntitySizeGuiDefault
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module ToggleButtonDispatcherModule =

    type Entity with
        member this.GetToggled world : bool = this.Get (nameof this.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof this.Toggled) value world
        member this.Toggled = lens (nameof this.Toggled) this this.GetToggled this.SetToggled
        member this.GetToggledTextOffset world : Vector3 = this.Get (nameof this.ToggledTextOffset) world
        member this.SetToggledTextOffset (value : Vector3) world = this.Set (nameof this.ToggledTextOffset) value world
        member this.ToggledTextOffset = lens (nameof this.ToggledTextOffset) this this.GetToggledTextOffset this.SetToggledTextOffset
        member this.GetPressed world : bool = this.Get (nameof this.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof this.Pressed) value world
        member this.Pressed = lens (nameof this.Pressed) this this.GetPressed this.SetPressed
        member this.GetPressedTextOffset world : Vector3 = this.Get (nameof this.PressedTextOffset) world
        member this.SetPressedTextOffset (value : Vector3) world = this.Set (nameof this.PressedTextOffset) value world
        member this.PressedTextOffset = lens (nameof this.PressedTextOffset) this this.GetPressedTextOffset this.SetPressedTextOffset
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
        member this.ToggleEvent = Events.Toggle --> this
        member this.ToggledEvent = Events.Toggled --> this
        member this.UntoggledEvent = Events.Untoggled --> this

    type ToggleButtonDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed then
                        let world = entity.SetToggled (not (entity.GetToggled world)) world
                        let toggled = entity.GetToggled world
                        let eventAddress = if toggled then Events.Toggled else Events.Untoggled
                        let eventTrace = EventTrace.debug "ToggleDispatcher" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () (eventAddress --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ToggleDispatcher" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publishPlus toggled (Events.Toggle --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound (entity.GetToggleSoundVolume world) toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Toggled false
             define Entity.ToggledTextOffset v3Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v3Zero
             define Entity.UntoggledImage Assets.Default.Image
             define Entity.ToggledImage Assets.Default.Image2
             define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
             define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =

            // update text offset regardless of enabled state
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetToggled world then entity.GetToggledTextOffset world
                else v3Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage =
                if entity.GetToggled world || entity.GetPressed world
                then entity.GetToggledImage world
                else entity.GetUntoggledImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Position.Y
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUntoggledImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module RadioButtonDispatcherModule =

    type Entity with
        member this.GetDialed world : bool = this.Get (nameof this.Dialed) world
        member this.SetDialed (value : bool) world = this.Set (nameof this.Dialed) value world
        member this.Dialed = lens (nameof this.Dialed) this this.GetDialed this.SetDialed
        member this.GetDialedTextOffset world : Vector3 = this.Get (nameof this.DialedTextOffset) world
        member this.SetDialedTextOffset (value : Vector3) world = this.Set (nameof this.DialedTextOffset) value world
        member this.DialedTextOffset = lens (nameof this.DialedTextOffset) this this.GetDialedTextOffset this.SetDialedTextOffset
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
        member this.DialEvent = Events.Dial --> this
        member this.DialedEvent = Events.Dialed --> this
        member this.UndialedEvent = Events.Undialed --> this

    type RadioButtonDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            let wasDialed = entity.GetDialed world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed && not wasDialed then
                        let world = entity.SetDialed true world
                        let dialed = entity.GetDialed world
                        let eventAddress = if dialed then Events.Dialed else Events.Undialed
                        let eventTrace = EventTrace.debug "RadioButtonDispatcher" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () (eventAddress --> entity) eventTrace entity true false world
                        let eventTrace = EventTrace.debug "RadioButtonDispatcher" "handleMouseLeftUp" "Dial" EventTrace.empty
                        let world = World.publishPlus dialed (Events.Dial --> entity) eventTrace entity true false world
                        let world =
                            match entity.GetDialSoundOpt world with
                            | Some dialSound -> World.playSound (entity.GetDialSoundVolume world) dialSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.Dialed false
             define Entity.DialedTextOffset v3Zero
             define Entity.Pressed false
             define Entity.PressedTextOffset v3Zero
             define Entity.UndialedImage Assets.Default.Image
             define Entity.DialedImage Assets.Default.Image2
             define Entity.DialSoundOpt (Some Assets.Default.Sound)
             define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            world

        override this.Update (entity, world) =

            // update text offset regardless of enabled state
            let textOffset =
                if entity.GetPressed world then entity.GetPressedTextOffset world
                elif entity.GetDialed world then entity.GetDialedTextOffset world
                else v3Zero
            entity.SetTextOffset textOffset world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute false
            let spriteImage =
                if entity.GetDialed world || entity.GetPressed world
                then entity.GetDialedImage world
                else entity.GetUndialedImage world
            World.enqueueRenderLayeredMessage2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Position.Y
                  AssetTag = AssetTag.generalize spriteImage
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
        member this.GetStartTime world : int64 = this.Get (nameof this.StartTime) world
        member this.SetStartTime (value : int64) world = this.Set (nameof this.StartTime) value world
        member this.StartTime = lens (nameof this.StartTime) this this.GetStartTime this.SetStartTime
        member this.GetStartDateTime world : DateTime = this.Get (nameof this.StartDateTime) world
        member this.SetStartDateTime (value : DateTime) world = this.Set (nameof this.StartDateTime) value world
        member this.StartDateTime = lens (nameof this.StartDateTime) this this.GetStartDateTime this.SetStartDateTime

    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTime.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 5.0 then
                let world = entity.SetStartTime (World.getUpdateTime world) world
                entity.SetStartDateTime currentDateTime world
            else world

        static member Properties =
            [define Entity.StartTime 0L
             define Entity.StartDateTime DateTime.UtcNow]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let world = resetIntermittent entity world
                let startDateTime = entity.GetStartDateTime world
                let currentDateTime = DateTime.UtcNow
                let elapsedDateTime = currentDateTime - startDateTime
                let time = double (World.getUpdateTime world - entity.GetStartTime world)
                let frames = time / elapsedDateTime.TotalSeconds
                if not (Double.IsNaN frames) then
                    let framesStr = "FPS: " + String.Format ("{0:f2}", frames)
                    entity.SetText framesStr world
                else world
            else world

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
        member this.GetTouched world : bool = this.Get (nameof this.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof this.Touched) value world
        member this.Touched = lens (nameof this.Touched) this this.GetTouched this.SetTouched
        member this.TouchEvent = Events.Touch --> this
        member this.TouchingEvent = Events.Touching --> this
        member this.UntouchEvent = Events.Untouch --> this

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetTouched true world
                        let eventTrace = EventTrace.debug "FeelerDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus data.Position (Events.Touch --> entity) eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasTouched = entity.GetTouched world
            let world = entity.SetTouched false world
            if entity.GetVisible world then
                if entity.GetEnabled world && wasTouched then
                    let eventTrace = EventTrace.debug "FeelerDispatcher" "handleMouseLeftDown" "" EventTrace.empty
                    let world = World.publishPlus data.Position (Events.Untouch --> entity) eventTrace entity true false world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleIncoming evt world =
            let entity = evt.Subscriber : Entity
            if  MouseState.isButtonDown MouseLeft &&
                entity.GetVisible world &&
                entity.GetEnabled world then
                let mousePosition = MouseState.getPosition ()
                let world = entity.SetTouched true world
                let eventTrace = EventTrace.debug "FeelerDispatcher" "handleIncoming" "" EventTrace.empty
                let world = World.publishPlus mousePosition (Events.Touch --> entity) eventTrace entity true false world
                (Resolve, world)
            else (Cascade, world)

        let handleOutgoing evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, entity.SetTouched false world)

        static member Properties =
            [define Entity.Touched false]

        override this.Register (entity, world) =
            let world = World.monitor handleMouseLeftDown Events.MouseLeftDown entity world
            let world = World.monitor handleMouseLeftUp Events.MouseLeftUp entity world
            let world = World.monitor handleIncoming (Events.IncomingFinish --> entity.Screen) entity world
            let world = World.monitor handleOutgoing (Events.OutgoingStart --> entity.Screen) entity world
            world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                if entity.GetTouched world then
                    let mousePosition = World.getMousePosition world
                    let eventTrace = EventTrace.debug "FeelerDispatcher" "Update" "" EventTrace.empty
                    let world = World.publishPlus mousePosition (Events.Touching --> entity) eventTrace entity true false world
                    world
                else world
            else world

        override this.GetQuickSize (_, _) =
            Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module FillBarDispatcherModule =

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

    type FillBarDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.Image9
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Image4]

        override this.Render (entity, world) =

            // border sprite
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter // gui currently ignores rotation
            let horizon = perimeter.Position.Y
            let mutable borderTransform = Transform.makeDefault transform.Centered
            borderTransform.Position <- perimeter.Position
            borderTransform.Size <- perimeter.Size
            borderTransform.Offset <- transform.Offset
            borderTransform.Elevation <- transform.Elevation + 0.5f
            borderTransform.Absolute <- transform.Absolute
            let disabledColor = entity.GetDisabledColor world
            let borderImageColor = (entity.GetBorderColor world).WithA disabledColor.A
            let borderImage = entity.GetBorderImage world
            let world =
                World.enqueueRenderLayeredMessage2d
                    { Elevation = borderTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize borderImage
                      RenderDescriptor2d =
                        SpriteDescriptor
                            { Transform = borderTransform
                              InsetOpt = ValueNone
                              Image = borderImage
                              Color = borderImageColor
                              Blend = Transparent
                              Glow = Color.Zero
                              Flip = FlipNone }}
                    world

            // fill sprite
            let fillSize = perimeter.Size
            let fillInset = fillSize * entity.GetFillInset world * 0.5f
            let fillPosition = perimeter.Position + fillInset
            let fillWidth = (fillSize.X - fillInset.X * 2.0f) * entity.GetFill world
            let fillHeight = fillSize.Y - fillInset.Y * 2.0f
            let fillSize = v3 fillWidth fillHeight 0.0f
            let mutable fillTransform = Transform.makeDefault transform.Centered
            fillTransform.Position <- fillPosition
            fillTransform.Size <- fillSize
            fillTransform.Offset <- transform.Offset
            fillTransform.Elevation <- transform.Elevation
            fillTransform.Absolute <- transform.Absolute
            let fillImageColor = (entity.GetFillColor world).WithA disabledColor.A
            let fillImage = entity.GetFillImage world
            let world =
                World.enqueueRenderLayeredMessage2d
                    { Elevation = fillTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize fillImage
                      RenderDescriptor2d =
                          SpriteDescriptor
                              { Transform = fillTransform
                                InsetOpt = ValueNone
                                Image = fillImage
                                Color = fillImageColor
                                Blend = Transparent
                                Glow = Color.Zero
                                Flip = FlipNone }}
                    world

            // fin
            world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module BasicEmitterDispatcher2dModule =

    type BasicEmitterDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<BasicEmitter2dFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module EffectDispatcher2dModule =

    type EffectDispatcher2d () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<Effect2dFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.Effect (scvalue<Effect> "[Effect None [] [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]")]

[<AutoOpen>]
module BlockDispatcher2dModule =

    type BlockDispatcher2d () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage Assets.Default.Image6]

[<AutoOpen>]
module BoxDispatcher2dModule =

    type BoxDispatcher2d () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6]

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

    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static let computeWalkCelInset (celSize : Vector2) (celRun : int) delay time =
            let compressedTime = time / delay
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
             define Entity.AnimationDelay 4L
             define Entity.FixedRotation true
             define Entity.GravityScale 3.0f
             define Entity.BodyShape (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v3Zero; PropertiesOpt = None })
             define Entity.SideViewCharacterIdleImage Assets.Default.SideViewCharacterIdleImage
             define Entity.SideViewCharacterJumpImage Assets.Default.SideViewCharacterJumpImage
             define Entity.SideViewCharacterWalkSheet Assets.Default.SideViewCharacterWalkImage
             define Entity.SideViewCharacterFacingLeft false]

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                // we have to use a bit of hackery to remember whether the character is facing left or
                // right when there is no velocity
                let facingLeft = entity.GetSideViewCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
                if facingLeft && velocity.X > 1.0f then entity.SetSideViewCharacterFacingLeft false world
                elif not facingLeft && velocity.X < -1.0f then entity.SetSideViewCharacterFacingLeft true world
                else world
            else world

        override this.Render (entity, world) =
            let time = World.getUpdateTime world
            let physicsId = entity.GetPhysicsId world
            let facingLeft = entity.GetSideViewCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity physicsId world
            let celSize = entity.GetCelSize world
            let celRun = entity.GetCelRun world
            let animationDelay = entity.GetAnimationDelay world
            let mutable transform = entity.GetTransform world
            let struct (insetOpt, image) =
                if not (World.isBodyOnGround physicsId world) then
                    let image = entity.GetSideViewCharacterJumpImage world
                    struct (ValueNone, image)
                elif velocity.X < 5.0f && velocity.X > -5.0f then
                    let image = entity.GetSideViewCharacterIdleImage world
                    struct (ValueNone, image)
                else
                    let image = entity.GetSideViewCharacterWalkSheet world
                    struct (ValueSome (computeWalkCelInset celSize celRun animationDelay time), image)
            World.enqueueRenderLayeredMessage2d
                { Elevation = transform.Elevation
                  Horizon = transform.Perimeter.Position.Y
                  AssetTag = AssetTag.generalize image
                  RenderDescriptor2d =
                    SpriteDescriptor
                        { Transform = transform
                          InsetOpt = insetOpt
                          Image = image
                          Color = Color.One
                          Blend = Transparent
                          Glow = Color.Zero
                          Flip = if facingLeft then FlipH else FlipNone }}
                world

[<AutoOpen>]
module TileMapDispatcherModule =

    type TileMapDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<TileMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap]

[<AutoOpen>]
module TmxMapDispatcherModule =

    type TmxMapDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<TmxMapFacet>]

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.Color Color.One
             define Entity.Glow Color.Zero
             define Entity.TileLayerClearance 2.0f
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())]

[<AutoOpen>]
module SkyBoxDispatcherModule =

    type SkyBoxDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<SkyBoxFacet>]

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

[<AutoOpen>]
module LightDispatcher3dModule =

    type LightDispatcher3d () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightFacet3d>]

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness 10.0f
             define Entity.Intensity 1.0f
             define Entity.LightType PointLight]

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardDispatcherModule =

    type StaticBillboardDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticBillboardFacet>]

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.AlbedoOpt None
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.MetalnessOpt None
             define Entity.MetalnessImage Assets.Default.MaterialMetalness
             define Entity.RoughnessOpt None
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.AmbientOcclusionOpt None
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelDispatcherModule =

    type StaticModelDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelFacet>]

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelSurfaceDispatcherModule =

    type StaticModelSurfaceDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<StaticModelSurfaceFacet>]

        static member Properties =
            [define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.RenderStyle Deferred]

[<AutoOpen>]
module StaticModelHierarchyDispatcherModule =

    type World with

        /// Attempt to import scene below the target entity.
        static member tryImportScene static_ staticModel (parent : Either<Group, Entity>) world =
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                // Unity Scene Export Instructions:
                // 1) have FBX Exporter package installed
                // 2) be in PBR Unity Project
                // 3) put all desired objects in empty root GameObject
                // 4) export root GameObject
                // 5) delete all fbx files except the one you exported
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
                            let world = child.SetStatic static_ world
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
                            let transform = light.LightMatrix
                            let position = transform.Translation
                            let mutable rotation = transform
                            rotation.Translation <- v3Zero
                            let rotation = Quaternion.CreateFromRotationMatrix rotation
                            let scale = transform.Scale
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetStatic static_ world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.QuickSize world
                            world' <- world
                        | OpenGL.PhysicallyBased.PhysicallyBasedSurface surface ->
                            let world = world'
                            let (mountToParent, surnames, group) =
                                match parent with
                                | Left group -> (surface.SurfaceNames.Length > 0, surface.SurfaceNames, group)
                                | Right entity -> (true, Array.append entity.Surnames surface.SurfaceNames, entity.Group)
                            let (child, world) = World.createEntity<StaticModelSurfaceDispatcher> DefaultOverlay (Some surnames) group world
                            let transform = surface.SurfaceMatrix
                            let position = transform.Translation
                            let mutable rotation = transform
                            rotation.Translation <- v3Zero
                            let rotation = Quaternion.CreateFromRotationMatrix rotation
                            let scale = transform.Scale
                            let world = child.SetPositionLocal position world
                            let world = child.SetRotationLocal rotation world
                            let world = child.SetScaleLocal scale world
                            let world = child.SetStatic static_ world
                            let world = if mountToParent then child.SetMountOpt (Some (Relation.makeParent ())) world else world
                            let world = child.SetSurfaceIndex i world
                            let world = child.SetStaticModel staticModel world
                            let world = child.SetAlbedoOpt (Some surface.SurfaceMaterial.Albedo) world
                            let world = child.SetMetalnessOpt (Some surface.SurfaceMaterial.Metalness) world
                            let world = child.SetRoughnessOpt (Some surface.SurfaceMaterial.Roughness) world
                            let world = child.SetAmbientOcclusionOpt (Some surface.SurfaceMaterial.AmbientOcclusion) world
                            let world = child.QuickSize world
                            world' <- world
                            i <- inc i)
                world'
            | None -> world

    type Entity with
        member this.GetLoaded world : bool = this.Get (nameof this.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof this.Loaded) value world
        member this.Loaded = lens (nameof this.Loaded) this this.GetLoaded this.SetLoaded

    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let synchronizeChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = World.tryImportScene true (entity.GetStaticModel world) (Right entity) world
            (Cascade, world)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.Loaded false]

        override this.Register (entity, world) =
            let world =
                if not (entity.GetLoaded world) then
                    let world = World.tryImportScene true (entity.GetStaticModel world) (Right entity) world
                    let world = entity.SetLoaded true world
                    world
                else world
            let world = World.monitor synchronizeChildren (entity.ChangeEvent (nameof entity.StaticModel)) entity world
            world