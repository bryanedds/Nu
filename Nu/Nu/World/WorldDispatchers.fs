// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Nito.Collections
open TiledSharp
open Prime
open Nu

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
        member this.GetScriptOpt world : Symbol AssetTag option = this.Get (nameof Entity.ScriptOpt) world
        member this.SetScriptOpt (value : Symbol AssetTag option) world = this.Set (nameof Entity.ScriptOpt) value world
        static member ScriptOpt = lens (nameof Entity.ScriptOpt) (fun (this : Entity) -> this.GetScriptOpt) (fun value this -> this.SetScriptOpt value)
        member this.GetScript world : Scripting.Expr array = this.Get (nameof Entity.Script) world
        member this.SetScript (value : Scripting.Expr array) world = this.Set (nameof Entity.Script) value world
        static member Script = lens (nameof Entity.Script) (fun (this : Entity) -> this.GetScript) (fun value this -> this.SetScript value)
        member internal this.GetScriptUnsubscriptions world : Unsubscription list = this.Get (nameof Entity.ScriptUnsubscriptions) world
        member internal this.SetScriptUnsubscriptions (value : Unsubscription list) world = this.Set (nameof Entity.ScriptUnsubscriptions) value world
        static member ScriptUnsubscriptions = lens (nameof Entity.ScriptUnsubscriptions) (fun (this : Entity) -> this.GetScriptUnsubscriptions) (fun value this -> this.SetScriptUnsubscriptions value)
        member this.GetRegisterScript world : Scripting.Expr = this.Get (nameof Entity.RegisterScript) world
        member this.SetRegisterScript (value : Scripting.Expr) world = this.Set (nameof Entity.RegisterScript) value world
        static member RegisterScript = lens (nameof Entity.RegisterScript) (fun (this : Entity) -> this.GetRegisterScript) (fun value this -> this.SetRegisterScript value)
        member this.GetUnregisterScript world : Scripting.Expr = this.Get (nameof Entity.UnregisterScript) world
        member this.SetUnregisterScript (value : Scripting.Expr) world = this.Set (nameof Entity.UnregisterScript) value world
        static member UnregisterScript = lens (nameof Entity.UnregisterScript) (fun (this : Entity) -> this.GetUnregisterScript) (fun value this -> this.SetUnregisterScript value)
        member this.GetUpdateScript world : Scripting.Expr = this.Get (nameof Entity.UpdateScript) world
        member this.SetUpdateScript (value : Scripting.Expr) world = this.Set (nameof Entity.UpdateScript) value world
        static member UpdateScript = lens (nameof Entity.UpdateScript) (fun (this : Entity) -> this.GetUpdateScript) (fun value this -> this.SetUpdateScript value)
        member this.GetPostUpdateScript world : Scripting.Expr = this.Get (nameof Entity.PostUpdateScript) world
        member this.SetPostUpdateScript (value : Scripting.Expr) world = this.Set (nameof Entity.PostUpdateScript) value world
        static member PostUpdateScript = lens (nameof Entity.PostUpdateScript) (fun (this : Entity) -> this.GetPostUpdateScript) (fun value this -> this.SetPostUpdateScript value)
        member this.GetRenderScript world : Scripting.Expr = this.Get (nameof Entity.RenderScript) world
        member this.SetRenderScript (value : Scripting.Expr) world = this.Set (nameof Entity.RenderScript) value world
        static member RenderScript = lens (nameof Entity.RenderScript) (fun (this : Entity) -> this.GetRenderScript) (fun value this -> this.SetRenderScript value)

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
             define Entity.PostUpdateScript Scripting.Unit
             define Entity.RenderScript Scripting.Unit]

        override this.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetRegisterScript world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChanged (entity.GetChangeEvent (nameof Entity.ScriptFrame)) entity world
            let world = World.monitor handleRegisterScriptChanged (entity.GetChangeEvent (nameof Entity.RegisterScript)) entity world
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
        member this.GetStaticImage world : Image AssetTag = this.Get (nameof Entity.StaticImage) world
        member this.SetStaticImage (value : Image AssetTag) world = this.Set (nameof Entity.StaticImage) value world
        static member StaticImage = lens (nameof Entity.StaticImage) (fun (this : Entity) -> this.GetStaticImage) (fun value this -> this.SetStaticImage value)
        member this.GetInsetOpt world : Box2 option = this.Get (nameof Entity.InsetOpt) world
        member this.SetInsetOpt (value : Box2 option) world = this.Set (nameof Entity.InsetOpt) value world
        static member InsetOpt = lens (nameof Entity.InsetOpt) (fun (this : Entity) -> this.GetInsetOpt) (fun value this -> this.SetInsetOpt value)
        member this.GetColor world : Color = this.Get (nameof Entity.Color) world
        member this.SetColor (value : Color) world = this.Set (nameof Entity.Color) value world
        static member Color = lens (nameof Entity.Color) (fun (this : Entity) -> this.GetColor) (fun value this -> this.SetColor value)
        member this.GetBlend world : Blend = this.Get (nameof Entity.Blend) world
        member this.SetBlend (value : Blend) world = this.Set (nameof Entity.Blend) value world
        static member Blend = lens (nameof Entity.Blend) (fun (this : Entity) -> this.GetBlend) (fun value this -> this.SetBlend value)
        member this.GetGlow world : Color = this.Get (nameof Entity.Glow) world
        member this.SetGlow (value : Color) world = this.Set (nameof Entity.Glow) value world
        static member Glow = lens (nameof Entity.Glow) (fun (this : Entity) -> this.GetGlow) (fun value this -> this.SetGlow value)
        member this.GetFlip world : Flip = this.Get (nameof Entity.Flip) world
        member this.SetFlip (value : Flip) world = this.Set (nameof Entity.Flip) value world
        static member Flip = lens (nameof Entity.Flip) (fun (this : Entity) -> this.GetFlip) (fun value this -> this.SetFlip value)

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
        member this.GetCelSize world : Vector2 = this.Get (nameof Entity.CelSize) world
        member this.SetCelSize (value : Vector2) world = this.Set (nameof Entity.CelSize) value world
        static member CelSize = lens (nameof Entity.CelSize) (fun (this : Entity) -> this.GetCelSize) (fun value this -> this.SetCelSize value)
        member this.GetCelRun world : int = this.Get (nameof Entity.CelRun) world
        member this.SetCelRun (value : int) world = this.Set (nameof Entity.CelRun) value world
        static member CelRun = lens (nameof Entity.CelRun) (fun (this : Entity) -> this.GetCelRun) (fun value this -> this.SetCelRun value)
        member this.GetCelCount world : int = this.Get (nameof Entity.CelCount) world
        member this.SetCelCount (value : int) world = this.Set (nameof Entity.CelCount) value world
        static member CelCount = lens (nameof Entity.CelCount) (fun (this : Entity) -> this.GetCelCount) (fun value this -> this.SetCelCount value)
        member this.GetAnimationDelay world : int64 = this.Get (nameof Entity.AnimationDelay) world
        member this.SetAnimationDelay (value : int64) world = this.Set (nameof Entity.AnimationDelay) value world
        static member AnimationDelay = lens (nameof Entity.AnimationDelay) (fun (this : Entity) -> this.GetAnimationDelay) (fun value this -> this.SetAnimationDelay value)
        member this.GetAnimationSheet world : Image AssetTag = this.Get (nameof Entity.AnimationSheet) world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.Set (nameof Entity.AnimationSheet) value world
        static member AnimationSheet = lens (nameof Entity.AnimationSheet) (fun (this : Entity) -> this.GetAnimationSheet) (fun value this -> this.SetAnimationSheet value)

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
        member this.GetText world : string = this.Get (nameof Entity.Text) world
        member this.SetText (value : string) world = this.Set (nameof Entity.Text) value world
        static member Text = lens (nameof Entity.Text) (fun (this : Entity) -> this.GetText) (fun value this -> this.SetText value)
        member this.GetFont world : Font AssetTag = this.Get (nameof Entity.Font) world
        member this.SetFont (value : Font AssetTag) world = this.Set (nameof Entity.Font) value world
        static member Font = lens (nameof Entity.Font) (fun (this : Entity) -> this.GetFont) (fun value this -> this.SetFont value)
        member this.GetMargins world : Vector3 = this.Get (nameof Entity.Margins) world
        member this.SetMargins (value : Vector3) world = this.Set (nameof Entity.Margins) value world
        static member Margins = lens (nameof Entity.Margins) (fun (this : Entity) -> this.GetMargins) (fun value this -> this.SetMargins value)
        member this.GetJustification world : Justification = this.Get (nameof Entity.Justification) world
        member this.SetJustification (value : Justification) world = this.Set (nameof Entity.Justification) value world
        static member Justification = lens (nameof Entity.Justification) (fun (this : Entity) -> this.GetJustification) (fun value this -> this.SetJustification value)
        member this.GetTextColor world : Color = this.Get (nameof Entity.TextColor) world
        member this.SetTextColor (value : Color) world = this.Set (nameof Entity.TextColor) value world
        static member TextColor = lens (nameof Entity.TextColor) (fun (this : Entity) -> this.GetTextColor) (fun value this -> this.SetTextColor value)
        member this.GetTextDisabledColor world : Color = this.Get (nameof Entity.TextDisabledColor) world
        member this.SetTextDisabledColor (value : Color) world = this.Set (nameof Entity.TextDisabledColor) value world
        static member TextDisabledColor = lens (nameof Entity.TextDisabledColor) (fun (this : Entity) -> this.GetTextDisabledColor) (fun value this -> this.SetTextDisabledColor value)
        member this.GetTextOffset world : Vector3 = this.Get (nameof Entity.TextOffset) world
        member this.SetTextOffset (value : Vector3) world = this.Set (nameof Entity.TextOffset) value world
        static member TextOffset = lens (nameof Entity.TextOffset) (fun (this : Entity) -> this.GetTextOffset) (fun value this -> this.SetTextOffset value)

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

        member this.GetSelfDestruct world : bool = this.Get (nameof Entity.SelfDestruct) world
        member this.SetSelfDestruct (value : bool) world = this.Set (nameof Entity.SelfDestruct) value world
        static member SelfDestruct = lens (nameof Entity.SelfDestruct) (fun (this : Entity) -> this.GetSelfDestruct) (fun value this -> this.SetSelfDestruct value)
        member this.GetEmitterGravity world : Vector3 = this.Get (nameof Entity.EmitterGravity) world
        member this.SetEmitterGravity (value : Vector3) world = this.Set (nameof Entity.EmitterGravity) value world
        static member EmitterGravity = lens (nameof Entity.EmitterGravity) (fun (this : Entity) -> this.GetEmitterGravity) (fun value this -> this.SetEmitterGravity value)
        member this.GetEmitterImage world : Image AssetTag = this.Get (nameof Entity.EmitterImage) world
        member this.SetEmitterImage (value : Image AssetTag) world = this.Set (nameof Entity.EmitterImage) value world
        static member EmitterImage = lens (nameof Entity.EmitterImage) (fun (this : Entity) -> this.GetEmitterImage) (fun value this -> this.SetEmitterImage value)
        member this.GetEmitterBlend world : Blend = this.Get (nameof Entity.EmitterBlend) world
        member this.SetEmitterBlend (value : Blend) world = this.Set (nameof Entity.EmitterBlend) value world
        static member EmitterBlend = lens (nameof Entity.EmitterBlend) (fun (this : Entity) -> this.GetEmitterBlend) (fun value this -> this.SetEmitterBlend value)
        member this.GetEmitterLifeTimeOpt world : int64 = this.Get (nameof Entity.EmitterLifeTimeOpt) world
        member this.SetEmitterLifeTimeOpt (value : int64) world = this.Set (nameof Entity.EmitterLifeTimeOpt) value world
        static member EmitterLifeTimeOpt = lens (nameof Entity.EmitterLifeTimeOpt) (fun (this : Entity) -> this.GetEmitterLifeTimeOpt) (fun value this -> this.SetEmitterLifeTimeOpt value)
        member this.GetEmitterConstraint world : Particles.Constraint = this.Get (nameof Entity.EmitterConstraint) world
        member this.SetEmitterConstraint (value : Particles.Constraint) world = this.Set (nameof Entity.EmitterConstraint) value world
        static member EmitterConstraint = lens (nameof Entity.EmitterConstraint) (fun (this : Entity) -> this.GetEmitterConstraint) (fun value this -> this.SetEmitterConstraint value)
        member this.GetEmitterStyle world : string = this.Get (nameof Entity.EmitterStyle) world
        member this.SetEmitterStyle (value : string) world = this.Set (nameof Entity.EmitterStyle) value world
        static member EmitterStyle = lens (nameof Entity.EmitterStyle) (fun (this : Entity) -> this.GetEmitterStyle) (fun value this -> this.SetEmitterStyle value)
        member this.GetParticleLifeTimeMaxOpt world : int64 = this.Get (nameof Entity.ParticleLifeTimeMaxOpt) world
        member this.SetParticleLifeTimeMaxOpt (value : int64) world = this.Set (nameof Entity.ParticleLifeTimeMaxOpt) value world
        static member ParticleLifeTimeMaxOpt = lens (nameof Entity.ParticleLifeTimeMaxOpt) (fun (this : Entity) -> this.GetParticleLifeTimeMaxOpt) (fun value this -> this.SetParticleLifeTimeMaxOpt value)
        member this.GetParticleRate world : single = this.Get (nameof Entity.ParticleRate) world
        member this.SetParticleRate (value : single) world = this.Set (nameof Entity.ParticleRate) value world
        static member ParticleRate = lens (nameof Entity.ParticleRate) (fun (this : Entity) -> this.GetParticleRate) (fun value this -> this.SetParticleRate value)
        member this.GetParticleMax world : int = this.Get (nameof Entity.ParticleMax) world
        member this.SetParticleMax (value : int) world = this.Set (nameof Entity.ParticleMax) value world
        static member ParticleMax = lens (nameof Entity.ParticleMax) (fun (this : Entity) -> this.GetParticleMax) (fun value this -> this.SetParticleMax value)
        member this.GetBasicParticleSeed world : Particles.BasicParticle = this.Get (nameof Entity.BasicParticleSeed) world
        member this.SetBasicParticleSeed (value : Particles.BasicParticle) world = this.Set (nameof Entity.BasicParticleSeed) value world
        static member BasicParticleSeed = lens (nameof Entity.BasicParticleSeed) (fun (this : Entity) -> this.GetBasicParticleSeed) (fun value this -> this.SetBasicParticleSeed value)
        member this.GetParticleSystem world : ParticleSystem = this.Get (nameof Entity.ParticleSystem) world
        member this.SetParticleSystem (value : ParticleSystem) world = this.Set (nameof Entity.ParticleSystem) value world
        static member ParticleSystem = lens (nameof Entity.ParticleSystem) (fun (this : Entity) -> this.GetParticleSystem) (fun value this -> this.SetParticleSystem value)

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

        static let handleEmitterConstraintChanged evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChanged evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
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
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicEmitter2d"
             define Entity.ParticleLifeTimeMaxOpt 60L
             define Entity.ParticleRate 1.0f
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make 0L 60L; Body = Particles.Body.defaultBody2d; Size = Constants.Engine.ParticleSize2dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Glow = Color.Zero; Flip = FlipNone }
             nonPersistent Entity.ParticleSystem ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.monitor handlePositionChanged (entity.GetChangeEvent (nameof Nu.Entity.Position)) entity world
            let world = World.monitor handleRotationChanged (entity.GetChangeEvent (nameof Entity.Rotation)) entity world
            let world = World.monitor handleEmitterBlendChanged (entity.GetChangeEvent (nameof Entity.EmitterBlend)) entity world
            let world = World.monitor handleEmitterImageChanged (entity.GetChangeEvent (nameof Entity.EmitterImage)) entity world
            let world = World.monitor handleEmitterLifeTimeOptChanged (entity.GetChangeEvent (nameof Entity.EmitterLifeTimeOpt)) entity world
            let world = World.monitor handleEmitterConstraintChanged (entity.GetChangeEvent (nameof Entity.EmitterConstraint)) entity world
            let world = World.monitor handleEmitterStyleChanged (entity.GetChangeEvent (nameof Entity.EmitterStyle)) entity world
            let world = World.monitor handleParticleLifeTimeMaxOptChanged (entity.GetChangeEvent (nameof Entity.ParticleLifeTimeMaxOpt)) entity world
            let world = World.monitor handleParticleRateChanged (entity.GetChangeEvent (nameof Entity.ParticleRate)) entity world
            let world = World.monitor handleParticleMaxChanged (entity.GetChangeEvent (nameof Entity.ParticleMax)) entity world
            let world = World.monitor handleBasicParticleSeedChanged (entity.GetChangeEvent (nameof Entity.BasicParticleSeed)) entity world
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

        member this.GetEffectSymbolOpt world : Symbol AssetTag option = this.Get (nameof Entity.EffectSymbolOpt) world
        member this.SetEffectSymbolOpt (value : Symbol AssetTag option) world = this.Set (nameof Entity.EffectSymbolOpt) value world
        static member EffectSymbolOpt = lens (nameof Entity.EffectSymbolOpt) (fun (this : Entity) -> this.GetEffectSymbolOpt) (fun value this -> this.SetEffectSymbolOpt value)
        member this.GetEffectStartTimeOpt world : int64 option = this.Get (nameof Entity.EffectStartTimeOpt) world
        member this.SetEffectStartTimeOpt (value : int64 option) world = this.Set (nameof Entity.EffectStartTimeOpt) value world
        static member EffectStartTimeOpt = lens (nameof Entity.EffectStartTimeOpt) (fun (this : Entity) -> this.GetEffectStartTimeOpt) (fun value this -> this.SetEffectStartTimeOpt value)
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get (nameof Entity.EffectDefinitions) world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.Set (nameof Entity.EffectDefinitions) value world
        static member EffectDefinitions = lens (nameof Entity.EffectDefinitions) (fun (this : Entity) -> this.GetEffectDefinitions) (fun value this -> this.SetEffectDefinitions value)
        member this.GetEffect world : Effect = this.Get (nameof Entity.Effect) world
        member this.SetEffect (value : Effect) world = this.Set (nameof Entity.Effect) value world
        static member Effect = lens (nameof Entity.Effect) (fun (this : Entity) -> this.GetEffect) (fun value this -> this.SetEffect value)
        member this.GetEffectOffset world : Vector3 = this.Get (nameof Entity.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof Entity.EffectOffset) value world
        static member EffectOffset = lens (nameof Entity.EffectOffset) (fun (this : Entity) -> this.GetEffectOffset) (fun value this -> this.SetEffectOffset value)
        member this.GetEffectCentered world : bool = this.Get (nameof Entity.EffectCentered) world
        member this.SetEffectCentered (value : bool) world = this.Set (nameof Entity.EffectCentered) value world
        static member EffectCentered = lens (nameof Entity.EffectCentered) (fun (this : Entity) -> this.GetEffectCentered) (fun value this -> this.SetEffectCentered value)
        member this.GetEffectTags world : EffectTags = this.Get (nameof Entity.EffectTags) world
        member private this.SetEffectTags (value : EffectTags) world = this.Set (nameof Entity.EffectTags) value world
        static member EffectTags = lensReadOnly (nameof Entity.EffectTags) (fun (this : Entity) -> this.GetEffectTags)
        member this.GetEffectHistoryMax world : int = this.Get (nameof Entity.EffectHistoryMax) world
        member this.SetEffectHistoryMax (value : int) world = this.Set (nameof Entity.EffectHistoryMax) value world
        static member EffectHistoryMax = lens (nameof Entity.EffectHistoryMax) (fun (this : Entity) -> this.GetEffectHistoryMax) (fun value this -> this.SetEffectHistoryMax value)
        member this.GetEffectHistory world : Effects.Slice Deque = this.Get (nameof Entity.EffectHistory) world
        member private this.SetEffectHistory (value : Effects.Slice Deque) world = this.Set (nameof Entity.EffectHistory) value world
        static member EffectHistory = lensReadOnly (nameof Entity.EffectHistory) (fun (this : Entity) -> this.GetEffectHistory)

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
             variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))]

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
            let world = World.monitor handleEffectsChanged (entity.GetChangeEvent (nameof Entity.EffectSymbolOpt)) entity world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
        member this.GetBodyEnabled world : bool = this.Get (nameof Entity.BodyEnabled) world
        member this.SetBodyEnabled (value : bool) world = this.Set (nameof Entity.BodyEnabled) value world
        static member BodyEnabled = lens (nameof Entity.BodyEnabled) (fun (this : Entity) -> this.GetBodyEnabled) (fun value this -> this.SetBodyEnabled value)
        member this.GetBodyType world : BodyType = this.Get (nameof Entity.BodyType) world
        member this.SetBodyType (value : BodyType) world = this.Set (nameof Entity.BodyType) value world
        static member BodyType = lens (nameof Entity.BodyType) (fun (this : Entity) -> this.GetBodyType) (fun value this -> this.SetBodyType value)
        member this.GetAwake world : bool = this.Get (nameof Entity.Awake) world
        member this.SetAwake (value : bool) world = this.Set (nameof Entity.Awake) value world
        static member Awake = lens (nameof Entity.Awake) (fun (this : Entity) -> this.GetAwake) (fun value this -> this.SetAwake value)
        member this.GetDensity world : single = this.Get (nameof Entity.Density) world
        member this.SetDensity (value : single) world = this.Set (nameof Entity.Density) value world
        static member Density = lens (nameof Entity.Density) (fun (this : Entity) -> this.GetDensity) (fun value this -> this.SetDensity value)
        member this.GetFriction world : single = this.Get (nameof Entity.Friction) world
        member this.SetFriction (value : single) world = this.Set (nameof Entity.Friction) value world
        static member Friction = lens (nameof Entity.Friction) (fun (this : Entity) -> this.GetFriction) (fun value this -> this.SetFriction value)
        member this.GetRestitution world : single = this.Get (nameof Entity.Restitution) world
        member this.SetRestitution (value : single) world = this.Set (nameof Entity.Restitution) value world
        static member Restitution = lens (nameof Entity.Restitution) (fun (this : Entity) -> this.GetRestitution) (fun value this -> this.SetRestitution value)
        member this.GetLinearVelocity world : Vector3 = this.Get (nameof Entity.LinearVelocity) world
        member this.SetLinearVelocity (value : Vector3) world = this.Set (nameof Entity.LinearVelocity) value world
        static member LinearVelocity = lens (nameof Entity.LinearVelocity) (fun (this : Entity) -> this.GetLinearVelocity) (fun value this -> this.SetLinearVelocity value)
        member this.GetLinearDamping world : single = this.Get (nameof Entity.LinearDamping) world
        member this.SetLinearDamping (value : single) world = this.Set (nameof Entity.LinearDamping) value world
        static member LinearDamping = lens (nameof Entity.LinearDamping) (fun (this : Entity) -> this.GetLinearDamping) (fun value this -> this.SetLinearDamping value)
        member this.GetAngularVelocity world : Vector3 = this.Get (nameof Entity.AngularVelocity) world
        member this.SetAngularVelocity (value : Vector3) world = this.Set (nameof Entity.AngularVelocity) value world
        static member AngularVelocity = lens (nameof Entity.AngularVelocity) (fun (this : Entity) -> this.GetAngularVelocity) (fun value this -> this.SetAngularVelocity value)
        member this.GetAngularDamping world : single = this.Get (nameof Entity.AngularDamping) world
        member this.SetAngularDamping (value : single) world = this.Set (nameof Entity.AngularDamping) value world
        static member AngularDamping = lens (nameof Entity.AngularDamping) (fun (this : Entity) -> this.GetAngularDamping) (fun value this -> this.SetAngularDamping value)
        member this.GetFixedRotation world : bool = this.Get (nameof Entity.FixedRotation) world
        member this.SetFixedRotation (value : bool) world = this.Set (nameof Entity.FixedRotation) value world
        static member FixedRotation = lens (nameof Entity.FixedRotation) (fun (this : Entity) -> this.GetFixedRotation) (fun value this -> this.SetFixedRotation value)
        member this.GetInertia world : single = this.Get (nameof Entity.Inertia) world
        member this.SetInertia (value : single) world = this.Set (nameof Entity.Inertia) value world
        static member Inertia = lens (nameof Entity.Inertia) (fun (this : Entity) -> this.GetInertia) (fun value this -> this.SetInertia value)
        member this.GetGravityScale world : single = this.Get (nameof Entity.GravityScale) world
        member this.SetGravityScale (value : single) world = this.Set (nameof Entity.GravityScale) value world
        static member GravityScale = lens (nameof Entity.GravityScale) (fun (this : Entity) -> this.GetGravityScale) (fun value this -> this.SetGravityScale value)
        member this.GetCollisionCategories world : string = this.Get (nameof Entity.CollisionCategories) world
        member this.SetCollisionCategories (value : string) world = this.Set (nameof Entity.CollisionCategories) value world
        static member CollisionCategories = lens (nameof Entity.CollisionCategories) (fun (this : Entity) -> this.GetCollisionCategories) (fun value this -> this.SetCollisionCategories value)
        member this.GetCollisionMask world : string = this.Get (nameof Entity.CollisionMask) world
        member this.SetCollisionMask (value : string) world = this.Set (nameof Entity.CollisionMask) value world
        static member CollisionMask = lens (nameof Entity.CollisionMask) (fun (this : Entity) -> this.GetCollisionMask) (fun value this -> this.SetCollisionMask value)
        member this.GetBodyShape world : BodyShape = this.Get (nameof Entity.BodyShape) world
        member this.SetBodyShape (value : BodyShape) world = this.Set (nameof Entity.BodyShape) value world
        static member BodyShape = lens (nameof Entity.BodyShape) (fun (this : Entity) -> this.GetBodyShape) (fun value this -> this.SetBodyShape value)
        member this.GetIgnoreCCD world : bool = this.Get (nameof Entity.IgnoreCCD) world
        member this.SetIgnoreCCD (value : bool) world = this.Set (nameof Entity.IgnoreCCD) value world
        static member IgnoreCCD = lens (nameof Entity.IgnoreCCD) (fun (this : Entity) -> this.GetIgnoreCCD) (fun value this -> this.SetIgnoreCCD value)
        member this.GetBullet world : bool = this.Get (nameof Entity.Bullet) world
        member this.SetBullet (value : bool) world = this.Set (nameof Entity.Bullet) value world
        static member Bullet = lens (nameof Entity.Bullet) (fun (this : Entity) -> this.GetBullet) (fun value this -> this.SetBullet value)
        member this.GetSensor world : bool = this.Get (nameof Entity.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof Entity.Sensor) value world
        static member Sensor = lens (nameof Entity.Sensor) (fun (this : Entity) -> this.GetSensor) (fun value this -> this.SetSensor value)
        member this.GetPhysicsId world : PhysicsId = this.Get (nameof Entity.PhysicsId) world
        static member PhysicsId = lensReadOnly (nameof Entity.PhysicsId) (fun (this : Entity) -> this.GetPhysicsId)
        member this.BodyCollisionEvent = Events.BodyCollision --> this
        member this.BodySeparationEvent = Events.BodySeparation --> this

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
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.BodyType)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Awake)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Density)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.LinearVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.LinearDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.AngularVelocity)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.AngularDamping)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.FixedRotation)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Inertia)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.GravityScale)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.BodyShape)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.IgnoreCCD)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Bullet)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Sensor)) entity world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter
            let bodyProperties =
                { BodyId = (entity.GetPhysicsId world).CorrelationId
                  Position = perimeter.Center
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
        member this.GetJointDevice world : JointDevice = this.Get (nameof Entity.JointDevice) world
        member this.SetJointDevice (value : JointDevice) world = this.Set (nameof Entity.JointDevice) value world
        static member JointDevice = lens (nameof Entity.JointDevice) (fun (this : Entity) -> this.GetJointDevice) (fun value this -> this.SetJointDevice value)

    type JointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.JointDevice JointEmpty
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = 0UL }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.JointDevice)) entity world
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
        member this.GetTileLayerClearance world : single = this.Get (nameof Entity.TileLayerClearance) world
        member this.SetTileLayerClearance (value : single) world = this.Set (nameof Entity.TileLayerClearance) value world
        static member TileLayerClearance = lens (nameof Entity.TileLayerClearance) (fun (this : Entity) -> this.GetTileLayerClearance) (fun value this -> this.SetTileLayerClearance value)
        member this.GetTileIndexOffset world : int = this.Get (nameof Entity.TileIndexOffset) world
        member this.SetTileIndexOffset (value : int) world = this.Set (nameof Entity.TileIndexOffset) value world
        static member TileIndexOffset = lens (nameof Entity.TileIndexOffset) (fun (this : Entity) -> this.GetTileIndexOffset) (fun value this -> this.SetTileIndexOffset value)
        member this.GetTileIndexOffsetRange world : int * int = this.Get (nameof Entity.TileIndexOffsetRange) world
        member this.SetTileIndexOffsetRange (value : int * int) world = this.Set (nameof Entity.TileIndexOffsetRange) value world
        static member TileIndexOffsetRange = lens (nameof Entity.TileIndexOffsetRange) (fun (this : Entity) -> this.GetTileIndexOffsetRange) (fun value this -> this.SetTileIndexOffsetRange value)
        member this.GetTileMap world : TileMap AssetTag = this.Get (nameof Entity.TileMap) world
        member this.SetTileMap (value : TileMap AssetTag) world = this.Set (nameof Entity.TileMap) value world
        static member TileMap = lens (nameof Entity.TileMap) (fun (this : Entity) -> this.GetTileMap) (fun value this -> this.SetTileMap value)

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
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.TileMap)) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof Entity.TileMap))
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
        member this.GetTmxMap world : TmxMap = this.Get (nameof Entity.TmxMap) world
        member this.SetTmxMap (value : TmxMap) world = this.Set (nameof Entity.TmxMap) value world
        static member TmxMap = lens (nameof Entity.TmxMap) (fun (this : Entity) -> this.GetTmxMap) (fun value this -> this.SetTmxMap value)

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
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.BodyEnabled)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Transform)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Friction)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.Restitution)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionCategories)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.CollisionMask)) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof Entity.TmxMap)) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof Entity.TmxMap))
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
        member this.GetCubeMap world : CubeMap AssetTag = this.Get (nameof Entity.CubeMap) world
        member this.SetCubeMap (value : CubeMap AssetTag) world = this.Set (nameof Entity.CubeMap) value world
        static member CubeMap = lens (nameof Entity.CubeMap) (fun (this : Entity) -> this.GetCubeMap) (fun value this -> this.SetCubeMap value)

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
        member this.GetBrightness world : single = this.Get (nameof Entity.Brightness) world
        member this.SetBrightness (value : single) world = this.Set (nameof Entity.Brightness) value world
        static member Brightness = lens (nameof Entity.Brightness) (fun (this : Entity) -> this.GetBrightness) (fun value this -> this.SetBrightness value)
        member this.GetIntensity world : single = this.Get (nameof Entity.Intensity) world
        member this.SetIntensity (value : single) world = this.Set (nameof Entity.Intensity) value world
        static member Intensity = lens (nameof Entity.Intensity) (fun (this : Entity) -> this.GetIntensity) (fun value this -> this.SetIntensity value)
        member this.GetLightType world : LightType = this.Get (nameof Entity.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof Entity.LightType) value world
        static member LightType = lens (nameof Entity.LightType) (fun (this : Entity) -> this.GetLightType) (fun value this -> this.SetLightType value)

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
        member this.GetAlbedoOpt world : Color option = this.Get (nameof Entity.AlbedoOpt) world
        member this.SetAlbedoOpt (value : Color option) world = this.Set (nameof Entity.AlbedoOpt) value world
        static member AlbedoOpt = lens (nameof Entity.AlbedoOpt) (fun (this : Entity) -> this.GetAlbedoOpt) (fun value this -> this.SetAlbedoOpt value)
        member this.GetAlbedoImage world : Image AssetTag = this.Get (nameof Entity.AlbedoImage) world
        member this.SetAlbedoImage (value : Image AssetTag) world = this.Set (nameof Entity.AlbedoImage) value world
        static member AlbedoImage = lens (nameof Entity.AlbedoImage) (fun (this : Entity) -> this.GetAlbedoImage) (fun value this -> this.SetAlbedoImage value)
        member this.GetMetalnessOpt world : single option = this.Get (nameof Entity.MetalnessOpt) world
        member this.SetMetalnessOpt (value : single option) world = this.Set (nameof Entity.MetalnessOpt) value world
        static member MetalnessOpt = lens (nameof Entity.MetalnessOpt) (fun (this : Entity) -> this.GetMetalnessOpt) (fun value this -> this.SetMetalnessOpt value)
        member this.GetMetalnessImage world : Image AssetTag = this.Get (nameof Entity.MetalnessImage) world
        member this.SetMetalnessImage (value : Image AssetTag) world = this.Set (nameof Entity.MetalnessImage) value world
        static member MetalnessImage = lens (nameof Entity.MetalnessImage) (fun (this : Entity) -> this.GetMetalnessImage) (fun value this -> this.SetMetalnessImage value)
        member this.GetRoughnessOpt world : single option = this.Get (nameof Entity.RoughnessOpt) world
        member this.SetRoughnessOpt (value : single option) world = this.Set (nameof Entity.RoughnessOpt) value world
        static member RoughnessOpt = lens (nameof Entity.RoughnessOpt) (fun (this : Entity) -> this.GetRoughnessOpt) (fun value this -> this.SetRoughnessOpt value)
        member this.GetRoughnessImage world : Image AssetTag = this.Get (nameof Entity.RoughnessImage) world
        member this.SetRoughnessImage (value : Image AssetTag) world = this.Set (nameof Entity.RoughnessImage) value world
        static member RoughnessImage = lens (nameof Entity.RoughnessImage) (fun (this : Entity) -> this.GetRoughnessImage) (fun value this -> this.SetRoughnessImage value)
        member this.GetAmbientOcclusionImage world : Image AssetTag = this.Get (nameof Entity.AmbientOcclusionImage) world
        member this.SetAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof Entity.AmbientOcclusionImage) value world
        static member AmbientOcclusionOpt = lens (nameof Entity.AmbientOcclusionOpt) (fun (this : Entity) -> this.GetAmbientOcclusionOpt) (fun value this -> this.SetAmbientOcclusionOpt value)
        member this.GetAmbientOcclusionOpt world : single option = this.Get (nameof Entity.AmbientOcclusionOpt) world
        member this.SetAmbientOcclusionOpt (value : single option) world = this.Set (nameof Entity.AmbientOcclusionOpt) value world
        static member AmbientOcclusionImage = lens (nameof Entity.AmbientOcclusionImage) (fun (this : Entity) -> this.GetAmbientOcclusionImage) (fun value this -> this.SetAmbientOcclusionImage value)
        member this.GetNormalImage world : Image AssetTag = this.Get (nameof Entity.NormalImage) world
        member this.SetNormalImage (value : Image AssetTag) world = this.Set (nameof Entity.NormalImage) value world
        static member NormalImage = lens (nameof Entity.NormalImage) (fun (this : Entity) -> this.GetNormalImage) (fun value this -> this.SetNormalImage value)
        member this.GetTextureMinFilterOpt world : OpenGL.TextureMinFilter option = this.Get (nameof Entity.TextureMinFilterOpt) world
        member this.SetTextureMinFilterOpt (value : OpenGL.TextureMinFilter option) world = this.Set (nameof Entity.TextureMinFilterOpt) value world
        static member TextureMinFilterOpt = lens (nameof Entity.TextureMinFilterOpt) (fun (this : Entity) -> this.GetTextureMinFilterOpt) (fun value this -> this.SetTextureMinFilterOpt value)
        member this.GetTextureMagFilterOpt world : OpenGL.TextureMagFilter option = this.Get (nameof Entity.TextureMagFilterOpt) world
        member this.SetTextureMagFilterOpt (value : OpenGL.TextureMagFilter option) world = this.Set (nameof Entity.TextureMagFilterOpt) value world
        static member TextureMagFilterOpt = lens (nameof Entity.TextureMagFilterOpt) (fun (this : Entity) -> this.GetTextureMagFilterOpt) (fun value this -> this.SetTextureMagFilterOpt value)
        member this.GetRenderStyle world : RenderStyle = this.Get (nameof Entity.RenderStyle) world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set (nameof Entity.RenderStyle) value world
        static member RenderStyle = lens (nameof Entity.RenderStyle) (fun (this : Entity) -> this.GetRenderStyle) (fun value this -> this.SetRenderStyle value)

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
        member this.GetStaticModel world : StaticModel AssetTag = this.Get (nameof Entity.StaticModel) world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set (nameof Entity.StaticModel) value world
        static member StaticModel = lens (nameof Entity.StaticModel) (fun (this : Entity) -> this.GetStaticModel) (fun value this -> this.SetStaticModel value)

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
        member this.GetSurfaceIndex world : int = this.Get (nameof Entity.SurfaceIndex) world
        member this.SetSurfaceIndex (value : int) world = this.Set (nameof Entity.SurfaceIndex) value world
        static member SurfaceIndex = lens (nameof Entity.SurfaceIndex) (fun (this : Entity) -> this.GetSurfaceIndex) (fun value this -> this.SetSurfaceIndex value)

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
        member this.GetDisabledColor world : Color = this.Get (nameof Entity.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof Entity.DisabledColor) value world
        static member DisabledColor = lens (nameof Entity.DisabledColor) (fun (this : Entity) -> this.GetDisabledColor) (fun value this -> this.SetDisabledColor value)

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
        member this.GetDown world : bool = this.Get (nameof Entity.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof Entity.Down) value world
        static member Down = lens (nameof Entity.Down) (fun (this : Entity) -> this.GetDown) (fun value this -> this.SetDown value)
        member this.GetDownTextOffset world : Vector3 = this.Get (nameof Entity.DownTextOffset) world
        member this.SetDownTextOffset (value : Vector3) world = this.Set (nameof Entity.DownTextOffset) value world
        static member DownTextOffset = lens (nameof Entity.DownTextOffset) (fun (this : Entity) -> this.GetDownTextOffset) (fun value this -> this.SetDownTextOffset value)
        member this.GetUpImage world : Image AssetTag = this.Get (nameof Entity.UpImage) world
        member this.SetUpImage (value : Image AssetTag) world = this.Set (nameof Entity.UpImage) value world
        static member UpImage = lens (nameof Entity.UpImage) (fun (this : Entity) -> this.GetUpImage) (fun value this -> this.SetUpImage value)
        member this.GetDownImage world : Image AssetTag = this.Get (nameof Entity.DownImage) world
        member this.SetDownImage (value : Image AssetTag) world = this.Set (nameof Entity.DownImage) value world
        static member DownImage = lens (nameof Entity.DownImage) (fun (this : Entity) -> this.GetDownImage) (fun value this -> this.SetDownImage value)
        member this.GetClickSoundOpt world : Sound AssetTag option = this.Get (nameof Entity.ClickSoundOpt) world
        member this.SetClickSoundOpt (value : Sound AssetTag option) world = this.Set (nameof Entity.ClickSoundOpt) value world
        static member ClickSoundOpt = lens (nameof Entity.ClickSoundOpt) (fun (this : Entity) -> this.GetClickSoundOpt) (fun value this -> this.SetClickSoundOpt value)
        member this.GetClickSoundVolume world : single = this.Get (nameof Entity.ClickSoundVolume) world
        member this.SetClickSoundVolume (value : single) world = this.Set (nameof Entity.ClickSoundVolume) value world
        static member ClickSoundVolume = lens (nameof Entity.ClickSoundVolume) (fun (this : Entity) -> this.GetClickSoundVolume) (fun value this -> this.SetClickSoundVolume value)
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
        member this.GetLabelImage world : Image AssetTag = this.Get (nameof Entity.LabelImage) world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set (nameof Entity.LabelImage) value world
        static member LabelImage = lens (nameof Entity.LabelImage) (fun (this : Entity) -> this.GetLabelImage) (fun value this -> this.SetLabelImage value)

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
        member this.GetBackgroundImageOpt world : Image AssetTag option = this.Get (nameof Entity.BackgroundImageOpt) world
        member this.SetBackgroundImageOpt (value : Image AssetTag option) world = this.Set (nameof Entity.BackgroundImageOpt) value world
        static member BackgroundImageOpt = lens (nameof Entity.BackgroundImageOpt) (fun (this : Entity) -> this.GetBackgroundImageOpt) (fun value this -> this.SetBackgroundImageOpt value)

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
        member this.GetToggled world : bool = this.Get (nameof Entity.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof Entity.Toggled) value world
        static member Toggled = lens (nameof Entity.Toggled) (fun (this : Entity) -> this.GetToggled) (fun value this -> this.SetToggled value)
        member this.GetToggledTextOffset world : Vector3 = this.Get (nameof Entity.ToggledTextOffset) world
        member this.SetToggledTextOffset (value : Vector3) world = this.Set (nameof Entity.ToggledTextOffset) value world
        static member ToggledTextOffset = lens (nameof Entity.ToggledTextOffset) (fun (this : Entity) -> this.GetToggledTextOffset) (fun value this -> this.SetToggledTextOffset value)
        member this.GetPressed world : bool = this.Get (nameof Entity.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof Entity.Pressed) value world
        static member Pressed = lens (nameof Entity.Pressed) (fun (this : Entity) -> this.GetPressed) (fun value this -> this.SetPressed value)
        member this.GetPressedTextOffset world : Vector3 = this.Get (nameof Entity.PressedTextOffset) world
        member this.SetPressedTextOffset (value : Vector3) world = this.Set (nameof Entity.PressedTextOffset) value world
        static member PressedTextOffset = lens (nameof Entity.PressedTextOffset) (fun (this : Entity) -> this.GetPressedTextOffset) (fun value this -> this.SetPressedTextOffset value)
        member this.GetUntoggledImage world : Image AssetTag = this.Get (nameof Entity.UntoggledImage) world
        member this.SetUntoggledImage (value : Image AssetTag) world = this.Set (nameof Entity.UntoggledImage) value world
        static member UntoggledImage = lens (nameof Entity.UntoggledImage) (fun (this : Entity) -> this.GetUntoggledImage) (fun value this -> this.SetUntoggledImage value)
        member this.GetToggledImage world : Image AssetTag = this.Get (nameof Entity.ToggledImage) world
        member this.SetToggledImage (value : Image AssetTag) world = this.Set (nameof Entity.ToggledImage) value world
        static member ToggledImage = lens (nameof Entity.ToggledImage) (fun (this : Entity) -> this.GetToggledImage) (fun value this -> this.SetToggledImage value)
        member this.GetToggleSoundOpt world : Sound AssetTag option = this.Get (nameof Entity.ToggleSoundOpt) world
        member this.SetToggleSoundOpt (value : Sound AssetTag option) world = this.Set (nameof Entity.ToggleSoundOpt) value world
        static member ToggleSoundOpt = lens (nameof Entity.ToggleSoundOpt) (fun (this : Entity) -> this.GetToggleSoundOpt) (fun value this -> this.SetToggleSoundOpt value)
        member this.GetToggleSoundVolume world : single = this.Get (nameof Entity.ToggleSoundVolume) world
        member this.SetToggleSoundVolume (value : single) world = this.Set (nameof Entity.ToggleSoundVolume) value world
        static member ToggleSoundVolume = lens (nameof Entity.ToggleSoundVolume) (fun (this : Entity) -> this.GetToggleSoundVolume) (fun value this -> this.SetToggleSoundVolume value)
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
        member this.GetDialed world : bool = this.Get (nameof Entity.Dialed) world
        member this.SetDialed (value : bool) world = this.Set (nameof Entity.Dialed) value world
        static member Dialed = lens (nameof Entity.Dialed) (fun (this : Entity) -> this.GetDialed) (fun value this -> this.SetDialed value)
        member this.GetDialedTextOffset world : Vector3 = this.Get (nameof Entity.DialedTextOffset) world
        member this.SetDialedTextOffset (value : Vector3) world = this.Set (nameof Entity.DialedTextOffset) value world
        static member DialedTextOffset = lens (nameof Entity.DialedTextOffset) (fun (this : Entity) -> this.GetDialedTextOffset) (fun value this -> this.SetDialedTextOffset value)
        member this.GetUndialedImage world : Image AssetTag = this.Get (nameof Entity.UndialedImage) world
        member this.SetUndialedImage (value : Image AssetTag) world = this.Set (nameof Entity.UndialedImage) value world
        static member UndialedImage = lens (nameof Entity.UndialedImage) (fun (this : Entity) -> this.GetUndialedImage) (fun value this -> this.SetUndialedImage value)
        member this.GetDialedImage world : Image AssetTag = this.Get (nameof Entity.DialedImage) world
        member this.SetDialedImage (value : Image AssetTag) world = this.Set (nameof Entity.DialedImage) value world
        static member DialedImage = lens (nameof Entity.DialedImage) (fun (this : Entity) -> this.GetDialedImage) (fun value this -> this.SetDialedImage value)
        member this.GetDialSoundOpt world : Sound AssetTag option = this.Get (nameof Entity.DialSoundOpt) world
        member this.SetDialSoundOpt (value : Sound AssetTag option) world = this.Set (nameof Entity.DialSoundOpt) value world
        static member DialSoundOpt = lens (nameof Entity.DialSoundOpt) (fun (this : Entity) -> this.GetDialSoundOpt) (fun value this -> this.SetDialSoundOpt value)
        member this.GetDialSoundVolume world : single = this.Get (nameof Entity.DialSoundVolume) world
        member this.SetDialSoundVolume (value : single) world = this.Set (nameof Entity.DialSoundVolume) value world
        static member DialSoundVolume = lens (nameof Entity.DialSoundVolume) (fun (this : Entity) -> this.GetDialSoundVolume) (fun value this -> this.SetDialSoundVolume value)
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
        member this.GetStartTime world : int64 = this.Get (nameof Entity.StartTime) world
        member this.SetStartTime (value : int64) world = this.Set (nameof Entity.StartTime) value world
        static member StartTime = lens (nameof Entity.StartTime) (fun (this : Entity) -> this.GetStartTime) (fun value this -> this.SetStartTime value)
        member this.GetStartDateTime world : DateTime = this.Get (nameof Entity.StartDateTime) world
        member this.SetStartDateTime (value : DateTime) world = this.Set (nameof Entity.StartDateTime) value world
        static member StartDateTime = lens (nameof Entity.StartDateTime) (fun (this : Entity) -> this.GetStartDateTime) (fun value this -> this.SetStartDateTime value)

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
        member this.GetTouched world : bool = this.Get (nameof Entity.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof Entity.Touched) value world
        static member Touched = lens (nameof Entity.Touched) (fun (this : Entity) -> this.GetTouched) (fun value this -> this.SetTouched value)
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
        member this.GetFill world : single = this.Get (nameof Entity.Fill) world
        member this.SetFill (value : single) world = this.Set (nameof Entity.Fill) value world
        static member Fill = lens (nameof Entity.Fill) (fun (this : Entity) -> this.GetFill) (fun value this -> this.SetFill value)
        member this.GetFillInset world : single = this.Get (nameof Entity.FillInset) world
        member this.SetFillInset (value : single) world = this.Set (nameof Entity.FillInset) value world
        static member FillInset = lens (nameof Entity.FillInset) (fun (this : Entity) -> this.GetFillInset) (fun value this -> this.SetFillInset value)
        member this.GetFillColor world : Color = this.Get (nameof Entity.FillColor) world
        member this.SetFillColor (value : Color) world = this.Set (nameof Entity.FillColor) value world
        static member FillColor = lens (nameof Entity.FillColor) (fun (this : Entity) -> this.GetFillColor) (fun value this -> this.SetFillColor value)
        member this.GetFillImage world : Image AssetTag = this.Get (nameof Entity.FillImage) world
        member this.SetFillImage (value : Image AssetTag) world = this.Set (nameof Entity.FillImage) value world
        static member FillImage = lens (nameof Entity.FillImage) (fun (this : Entity) -> this.GetFillImage) (fun value this -> this.SetFillImage value)
        member this.GetBorderColor world : Color = this.Get (nameof Entity.BorderColor) world
        member this.SetBorderColor (value : Color) world = this.Set (nameof Entity.BorderColor) value world
        static member BorderColor = lens (nameof Entity.BorderColor) (fun (this : Entity) -> this.GetBorderColor) (fun value this -> this.SetBorderColor value)
        member this.GetBorderImage world : Image AssetTag = this.Get (nameof Entity.BorderImage) world
        member this.SetBorderImage (value : Image AssetTag) world = this.Set (nameof Entity.BorderImage) value world
        static member BorderImage = lens (nameof Entity.BorderImage) (fun (this : Entity) -> this.GetBorderImage) (fun value this -> this.SetBorderImage value)

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
        member this.GetSideViewCharacterIdleImage world : Image AssetTag = this.Get (nameof Entity.SideViewCharacterIdleImage) world
        member this.SetSideViewCharacterIdleImage (value : Image AssetTag) world = this.Set (nameof Entity.SideViewCharacterIdleImage) value world
        static member SideViewCharacterIdleImage = lens (nameof Entity.SideViewCharacterIdleImage) (fun (this : Entity) -> this.GetSideViewCharacterIdleImage) (fun value this -> this.SetSideViewCharacterIdleImage value)
        member this.GetSideViewCharacterJumpImage world : Image AssetTag = this.Get (nameof Entity.SideViewCharacterJumpImage) world
        member this.SetSideViewCharacterJumpImage (value : Image AssetTag) world = this.Set (nameof Entity.SideViewCharacterJumpImage) value world
        static member SideViewCharacterJumpImage = lens (nameof Entity.SideViewCharacterJumpImage) (fun (this : Entity) -> this.GetSideViewCharacterJumpImage) (fun value this -> this.SetSideViewCharacterJumpImage value)
        member this.GetSideViewCharacterWalkSheet world : Image AssetTag = this.Get (nameof Entity.SideViewCharacterWalkSheet) world
        member this.SetSideViewCharacterWalkSheet (value : Image AssetTag) world = this.Set (nameof Entity.SideViewCharacterWalkSheet) value world
        static member SideViewCharacterWalkSheet = lens (nameof Entity.SideViewCharacterWalkSheet) (fun (this : Entity) -> this.GetSideViewCharacterWalkSheet) (fun value this -> this.SetSideViewCharacterWalkSheet value)
        member this.GetSideViewCharacterFacingLeft world : bool = this.Get (nameof Entity.SideViewCharacterFacingLeft) world
        member this.SetSideViewCharacterFacingLeft (value : bool) world = this.Set (nameof Entity.SideViewCharacterFacingLeft) value world
        static member SideViewCharacterFacingLeft = lens (nameof Entity.SideViewCharacterFacingLeft) (fun (this : Entity) -> this.GetSideViewCharacterFacingLeft) (fun value this -> this.SetSideViewCharacterFacingLeft value)

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
                            let (child, world) = World.createEntity<EntityDispatcher3d> (Some surnames) DefaultOverlay group world
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
                            let (child, world) = World.createEntity<LightDispatcher3d> (Some surnames) DefaultOverlay group world
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
                            let (child, world) = World.createEntity<StaticModelSurfaceDispatcher> (Some surnames) DefaultOverlay group world
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
        member this.GetLoaded world : bool = this.Get (nameof Entity.Loaded) world
        member this.SetLoaded (value : bool) world = this.Set (nameof Entity.Loaded) value world
        static member Loaded = lens (nameof Entity.Loaded) (fun (this : Entity) -> this.GetLoaded) (fun value this -> this.SetLoaded value)

    type StaticModelHierarchyDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let syncChildren evt world =
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
            let world = World.monitor syncChildren (entity.ChangeEvent (nameof Entity.StaticModel)) entity world
            world