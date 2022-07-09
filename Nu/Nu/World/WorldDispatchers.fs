// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Nito.Collections
open TiledSharp
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module DeclarativeOperators2 =

    type World with

        static member internal actualizeView view world =
            match view with
            | Render2d (elevation, horizon, assetTag, descriptor) ->
                let message = { Elevation = elevation; Horizon = horizon; AssetTag = AssetTag.generalize assetTag; RenderDescriptor2d = descriptor }
                World.enqueueRenderLayeredMessage2d message world
            | Render3d renderMessage -> World.enqueueRenderMessage3d renderMessage world
            | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
            | PlaySong (fadeIn, fadeOut, volume, start, assetTag) -> World.playSong fadeIn fadeOut volume start assetTag world
            | FadeOutSong fade -> World.fadeOutSong fade world
            | StopSong -> World.stopSong world
            | SpawnEmitter (_, _) -> world
            | Tag _ -> world
            | Views views -> Array.fold (fun world view -> World.actualizeView view world) world views
            | SegmentedViews views -> SegmentedArray.fold (fun world view -> World.actualizeView view world) world views

[<AutoOpen>]
module FacetModule =

    type World with

        static member internal trySignalEntityFacet (signalObj : obj) facetName (entity : Entity) world =
            let facets = entity.GetFacets world
            match Array.tryFind (fun facet -> getTypeName facet = facetName) facets with
            | Some (:? Facet<'model, 'message, 'command> as facet) ->
                match signalObj with
                | :? Signal<'message, 'command> as signal ->
                    Signal.processSignal facet.Message facet.Command (entity.FacetModelGeneric<'model> facet.ModelName) signal entity world
                | _ -> Log.info "Incorrect signal type returned from event binding."; world
            | _ -> Log.info "Failed to send signal to entity facet."; world

        static member internal signalEntityFacet<'model, 'message, 'command> signal facetName (entity : Entity) world =
            let facets = entity.GetFacets world
            match Array.tryFind (fun facet -> getTypeName facet = facetName) facets with
            | Some (:? Facet<'model, 'message, 'command> as facet) ->
                Signal.processSignal facet.Message facet.Command (entity.FacetModelGeneric<'model> facet.ModelName) signal entity world
            | _ -> Log.info "Failed to send signal to entity."; world

    and Entity with
    
        member this.GetFacetModelGeneric<'model> modelName world =
            this.Get<'model> modelName world

        member this.SetFacetModelGeneric<'model> modelName (value : 'model) world =
            this.Set<'model> modelName value world

        member this.UpdateFacetModelGeneric<'model> modelName updater world =
            this.SetFacetModelGeneric<'model> modelName (updater this.GetFacetModelGeneric<'model> modelName world) world

        member this.FacetModelGeneric<'model> modelName =
            lens<'model> modelName (this.GetFacetModelGeneric<'model> modelName) (this.SetFacetModelGeneric<'model> modelName) this

        member this.TrySignalEntityFacet<'model, 'message, 'command> signal facetName world =
            World.trySignalEntityFacet signal facetName this world

        member this.SignalEntityFacet<'model, 'message, 'command> signal facetName world =
            World.signalEntityFacet<'model, 'message, 'command> signal facetName this world

    and [<AbstractClass>] Facet<'model, 'message, 'command> (physical, initial : 'model) =
        inherit Facet (physical)

        let mutable modelNameOpt =
            Unchecked.defaultof<string>

        member this.ModelName =
            if isNull modelNameOpt then modelNameOpt <- getTypeName this + "Model"
            modelNameOpt
            
        member this.GetModel (entity : Entity) world : 'model =
            entity.GetFacetModelGeneric<'model> this.ModelName world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetFacetModelGeneric<'model> this.ModelName model world

        member this.Model (entity : Entity) =
            lens this.ModelName (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let world =
                match World.tryGetProperty (this.ModelName, entity, world) with
                | (false, _) ->
                    let model = this.Prepare (initial, world)
                    let property = { DesignerType = typeof<'model>; DesignerValue = model }
                    let property = { PropertyType = typeof<DesignerProperty>; PropertyValue = property }
                    World.attachProperty this.ModelName property entity world
                | (true, _) -> world
            let channels = this.Channel (this.Model entity, entity)
            let world = Signal.processChannels this.Message this.Command (this.Model entity) channels entity world
            let content = this.Content (this.Model entity, entity)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent content (FacetOrigin (entity, getTypeName this)) entity entity.Group world |> snd)
                    world content
            let initializers = this.Initializers (this.Model entity, entity)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName property entity world |> snd'
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> entity
                    World.monitor (fun (evt : Event) world ->
                        let world = WorldModule.trySignalFacet (handler evt) (getTypeName this) entity world
                        (Cascade, world))
                        eventAddress (entity :> Simulant) world
                | BindDefinition (left, right) ->
                    WorldModule.bind5 entity left right world
                | LinkDefinition (left, right) ->
                    let world = WorldModule.bind5 entity left right world
                    WorldModule.bind5 right.This right left world)
                world initializers

        override this.Actualize (entity, world) =
            let view = this.View (this.GetModel entity world, entity, world)
            World.actualizeView view world

        override this.TrySignal (signalObj, entity, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> entity.SignalEntityFacet<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) (getTypeName this) world
            | :? Signal<obj, 'command> as signal -> entity.SignalEntityFacet<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) (getTypeName this) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Prepare : 'model * World -> 'model
        default this.Prepare (model, _) = model

        abstract member Channel : Lens<'model, World> * Entity -> Channel<'message, 'command, Entity, World> list
        default this.Channel (_, _) = []

        abstract member Initializers : Lens<'model, World> * Entity -> PropertyInitializer list
        default this.Initializers (_, _) = []

        abstract member Message : 'model * 'message * Entity * World -> Signal<'message, 'command> list * 'model
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Entity * World -> Signal<'message, 'command> list * World
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Entity -> EntityContent list
        default this.Content (_, _) = []

        abstract member View : 'model * Entity * World -> View
        default this.View (_, _, _) = View.empty

[<AutoOpen>]
module ScriptFacetModule =

    type Entity with
        member this.GetScriptOpt world : Symbol AssetTag option = this.Get Property? ScriptOpt world
        member this.SetScriptOpt (value : Symbol AssetTag option) world = this.Set Property? ScriptOpt value world
        member this.ScriptOpt = lens Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world : Scripting.Expr array = this.Get Property? Script world
        member this.SetScript (value : Scripting.Expr array) world = this.Set Property? Script value world
        member this.Script = lens Property? Script this.GetScript this.SetScript this
        member internal this.GetScriptUnsubscriptions world : Unsubscription list = this.Get Property? ScriptUnsubscriptions world
        member internal this.SetScriptUnsubscriptions (value : Unsubscription list) world = this.Set Property? ScriptUnsubscriptions value world
        member internal this.ScriptUnsubscriptions = lens Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetRegisterScript world : Scripting.Expr = this.Get Property? RegisterScript world
        member this.SetRegisterScript (value : Scripting.Expr) world = this.Set Property? RegisterScript value world
        member this.RegisterScript = lens Property? RegisterScript this.GetRegisterScript this.SetRegisterScript this
        member this.GetUnregisterScript world : Scripting.Expr = this.Get Property? UnregisterScript world
        member this.SetUnregisterScript (value : Scripting.Expr) world = this.Set Property? UnregisterScript value world
        member this.UnregisterScript = lens Property? UnregisterScript this.GetUnregisterScript this.SetUnregisterScript this
        member this.GetUpdateScript world : Scripting.Expr = this.Get Property? UpdateScript world
        member this.SetUpdateScript (value : Scripting.Expr) world = this.Set Property? UpdateScript value world
        member this.UpdateScript = lens Property? UpdateScript this.GetUpdateScript this.SetUpdateScript this
        member this.GetPostUpdateScript world : Scripting.Expr = this.Get Property? PostUpdateScript world
        member this.SetPostUpdateScript (value : Scripting.Expr) world = this.Set Property? PostUpdateScript value world
        member this.PostUpdateScript = lens Property? PostUpdateScript this.GetPostUpdateScript this.SetPostUpdateScript this
        member this.GetActualizeScript world : Scripting.Expr = this.Get Property? ActualizeScript world
        member this.SetActualizeScript (value : Scripting.Expr) world = this.Set Property? ActualizeScript value world
        member this.ActualizeScript = lens Property? ActualizeScript this.GetActualizeScript this.SetActualizeScript this

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
             define Entity.ActualizeScript Scripting.Unit]

        override this.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetRegisterScript world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChanged (entity.GetChangeEvent Property? Script) entity world
            let world = World.monitor handleRegisterScriptChanged (entity.GetChangeEvent Property? RegisterScript) entity world
            world

        override this.Unregister (entity, world) =
            World.evalWithLogging (entity.GetUnregisterScript world) (entity.GetScriptFrame world) entity world |> snd'

        override this.Update (entity, world) =
            World.evalWithLogging (entity.GetUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'

#if !DISABLE_ENTITY_POST_UPDATE
        override this.PostUpdate (entity, world) =
            World.evalWithLogging (entity.GetPostUpdateScript world) (entity.GetScriptFrame world) entity world |> snd'
#endif

        override this.Actualize (entity, world) =
            World.evalWithLogging (entity.GetActualizeScript world) (entity.GetScriptFrame world) entity world |> snd'

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with
        member this.GetStaticImage world : Image AssetTag = this.Get Property? StaticImage world
        member this.SetStaticImage (value : Image AssetTag) world = this.Set Property? StaticImage value world
        member this.StaticImage = lens Property? StaticImage this.GetStaticImage this.SetStaticImage this
        member this.GetInsetOpt world : Box2 option = this.Get Property? Inset world
        member this.SetInsetOpt (value : Box2 option) world = this.Set Property? Inset value world
        member this.InsetOpt = lens Property? Inset this.GetInsetOpt this.SetInsetOpt this
        member this.GetColor world : Color = this.Get Property? Color world
        member this.SetColor (value : Color) world = this.Set Property? Color value world
        member this.Color = lens Property? Color this.GetColor this.SetColor this
        member this.GetBlend world : Blend = this.Get Property? Blend world
        member this.SetBlend (value : Blend) world = this.Set Property? Blend value world
        member this.Blend = lens Property? Blend this.GetBlend this.SetBlend this
        member this.GetGlow world : Color = this.Get Property? Glow world
        member this.SetGlow (value : Color) world = this.Set Property? Glow value world
        member this.Glow = lens Property? Glow this.GetGlow this.SetGlow this
        member this.GetFlip world : Flip = this.Get Property? Flip world
        member this.SetFlip (value : Flip) world = this.Set Property? Flip value world
        member this.Flip = lens Property? Flip this.GetFlip this.SetFlip this

    type StaticSpriteFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Glow Color.Zero
             define Entity.InsetOpt None
             define Entity.Flip FlipNone]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetStaticImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
        member this.GetCelSize world : Vector2 = this.Get Property? CelSize world
        member this.SetCelSize (value : Vector2) world = this.Set Property? CelSize value world
        member this.CelSize = lens Property? CelSize this.GetCelSize this.SetCelSize this
        member this.GetCelRun world : int = this.Get Property? CelRun world
        member this.SetCelRun (value : int) world = this.Set Property? CelRun value world
        member this.CelRun = lens Property? CelRun this.GetCelRun this.SetCelRun this
        member this.GetCelCount world : int = this.Get Property? CelCount world
        member this.SetCelCount (value : int) world = this.Set Property? CelCount value world
        member this.CelCount = lens Property? CelCount this.GetCelCount this.SetCelCount this
        member this.GetAnimationDelay world : int64 = this.Get Property? AnimationDelay world
        member this.SetAnimationDelay (value : int64) world = this.Set Property? AnimationDelay value world
        member this.AnimationDelay = lens Property? AnimationDelay this.GetAnimationDelay this.SetAnimationDelay this
        member this.GetAnimationSheet world : Image AssetTag = this.Get Property? AnimationSheet world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.Set Property? AnimationSheet value world
        member this.AnimationSheet = lens Property? AnimationSheet this.GetAnimationSheet this.SetAnimationSheet this

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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
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
            else world

        override this.GetQuickSize (entity, world) =
            (entity.GetCelSize world).V3

[<AutoOpen>]
module TextFacetModule =

    type Entity with
        member this.GetText world : string = this.Get Property? Text world
        member this.SetText (value : string) world = this.Set Property? Text value world
        member this.Text = lens Property? Text this.GetText this.SetText this
        member this.GetFont world : Font AssetTag = this.Get Property? Font world
        member this.SetFont (value : Font AssetTag) world = this.Set Property? Font value world
        member this.Font = lens Property? Font this.GetFont this.SetFont this
        member this.GetMargins world : Vector3 = this.Get Property? Margins world
        member this.SetMargins (value : Vector3) world = this.Set Property? Margins value world
        member this.Margins = lens Property? Margins this.GetMargins this.SetMargins this
        member this.GetJustification world : Justification = this.Get Property? Justification world
        member this.SetJustification (value : Justification) world = this.Set Property? Justification value world
        member this.Justification = lens Property? Justification this.GetJustification this.SetJustification this
        member this.GetTextColor world : Color = this.Get Property? TextColor world
        member this.SetTextColor (value : Color) world = this.Set Property? TextColor value world
        member this.TextColor = lens Property? TextColor this.GetTextColor this.SetTextColor this
        member this.GetTextDisabledColor world : Color = this.Get Property? TextDisabledColor world
        member this.SetTextDisabledColor (value : Color) world = this.Set Property? TextDisabledColor value world
        member this.TextDisabledColor = lens Property? TextDisabledColor this.GetTextDisabledColor this.SetTextDisabledColor this
        member this.GetTextOffset world : Vector3 = this.Get Property? TextOffset world
        member this.SetTextOffset (value : Vector3) world = this.Set Property? TextOffset value world
        member this.TextOffset = lens Property? TextOffset this.GetTextOffset this.SetTextOffset this

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

        override this.Actualize (entity, world) =
            let text = entity.GetText world
            if entity.GetVisible world && not (String.IsNullOrWhiteSpace text) then
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

        member this.GetSelfDestruct world : bool = this.Get Property? SelfDestruct world
        member this.SetSelfDestruct (value : bool) world = this.Set Property? SelfDestruct value world
        member this.SelfDestruct = lens Property? SelfDestruct this.GetSelfDestruct this.SetSelfDestruct this
        member this.GetEmitterGravity world : Vector3 = this.Get Property? EmitterGravity world
        member this.SetEmitterGravity (value : Vector3) world = this.Set Property? EmitterGravity value world
        member this.EmitterGravity = lens Property? EmitterGravity this.GetEmitterGravity this.SetEmitterGravity this
        member this.GetEmitterImage world : Image AssetTag = this.Get Property? EmitterImage world
        member this.SetEmitterImage (value : Image AssetTag) world = this.Set Property? EmitterImage value world
        member this.EmitterImage = lens Property? EmitterImage this.GetEmitterImage this.SetEmitterImage this
        member this.GetEmitterBlend world : Blend = this.Get Property? EmitterBlend world
        member this.SetEmitterBlend (value : Blend) world = this.Set Property? EmitterBlend value world
        member this.EmitterBlend = lens Property? EmitterBlend this.GetEmitterBlend this.SetEmitterBlend this
        member this.GetEmitterLifeTimeOpt world : int64 = this.Get Property? EmitterLifeTimeOpt world
        member this.SetEmitterLifeTimeOpt (value : int64) world = this.Set Property? EmitterLifeTimeOpt value world
        member this.EmitterLifeTimeOpt = lens Property? EmitterLifeTimeOpt this.GetEmitterLifeTimeOpt this.SetEmitterLifeTimeOpt this
        member this.GetParticleLifeTimeMaxOpt world : int64 = this.Get Property? ParticleLifeTimeMaxOpt world
        member this.SetParticleLifeTimeMaxOpt (value : int64) world = this.Set Property? ParticleLifeTimeMaxOpt value world
        member this.ParticleLifeTimeMaxOpt = lens Property? ParticleLifeTimeMaxOpt this.GetParticleLifeTimeMaxOpt this.SetParticleLifeTimeMaxOpt this
        member this.GetParticleRate world : single = this.Get Property? ParticleRate world
        member this.SetParticleRate (value : single) world = this.Set Property? ParticleRate value world
        member this.ParticleRate = lens Property? ParticleRate this.GetParticleRate this.SetParticleRate this
        member this.GetParticleMax world : int = this.Get Property? ParticleMax world
        member this.SetParticleMax (value : int) world = this.Set Property? ParticleMax value world
        member this.ParticleMax = lens Property? ParticleMax this.GetParticleMax this.SetParticleMax this
        member this.GetBasicParticleSeed world : Particles.BasicParticle = this.Get Property? BasicParticleSeed world
        member this.SetBasicParticleSeed (value : Particles.BasicParticle) world = this.Set Property? BasicParticleSeed value world
        member this.BasicParticleSeed = lens Property? BasicParticleSeed this.GetBasicParticleSeed this.SetBasicParticleSeed this
        member this.GetEmitterConstraint world : Particles.Constraint = this.Get Property? EmitterConstraint world
        member this.SetEmitterConstraint (value : Particles.Constraint) world = this.Set Property? EmitterConstraint value world
        member this.EmitterConstraint = lens Property? EmitterConstraint this.GetEmitterConstraint this.SetEmitterConstraint this
        member this.GetEmitterStyle world : string = this.Get Property? EmitterStyle world
        member this.SetEmitterStyle (value : string) world = this.Set Property? EmitterStyle value world
        member this.EmitterStyle = lens Property? EmitterStyle this.GetEmitterStyle this.SetEmitterStyle this
        member this.GetParticleSystem world : ParticleSystem = this.Get Property? ParticleSystem world
        member this.SetParticleSystem (value : ParticleSystem) world = this.Set Property? ParticleSystem value world
        member this.ParticleSystem = lens Property? ParticleSystem this.GetParticleSystem this.SetParticleSystem this

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
            let world = World.monitor handlePositionChanged (entity.GetChangeEvent Property? Position) entity world
            let world = World.monitor handleRotationChanged (entity.GetChangeEvent Property? Rotation) entity world
            let world = World.monitor handleEmitterBlendChanged (entity.GetChangeEvent Property? EmitterBlend) entity world
            let world = World.monitor handleEmitterImageChanged (entity.GetChangeEvent Property? EmitterImage) entity world
            let world = World.monitor handleEmitterLifeTimeOptChanged (entity.GetChangeEvent Property? EmitterLifeTimeOpt) entity world
            let world = World.monitor handleParticleLifeTimeMaxOptChanged (entity.GetChangeEvent Property? ParticleLifeTimeMaxOpt) entity world
            let world = World.monitor handleParticleRateChanged (entity.GetChangeEvent Property? ParticleRate) entity world
            let world = World.monitor handleParticleMaxChanged (entity.GetChangeEvent Property? ParticleMax) entity world
            let world = World.monitor handleBasicParticleSeedChanged (entity.GetChangeEvent Property? BasicParticleSeed) entity world
            let world = World.monitor handleEmitterConstraintChanged (entity.GetChangeEvent Property? EmitterConstraint) entity world
            let world = World.monitor handleEmitterStyleChanged (entity.GetChangeEvent Property? EmitterStyle) entity world
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
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
            else world

[<AutoOpen>]
module Effect2dFacetModule =

    type EffectTags =
        Map<string, Effects.Slice>

    type Entity with

        member this.GetEffectSymbolOpt world : Symbol AssetTag option = this.Get Property? EffectSymbolOpt world
        member this.SetEffectSymbolOpt (value : Symbol AssetTag option) world = this.Set Property? EffectSymbolOpt value world
        member this.EffectSymbolOpt = lens Property? EffectSymbolOpt this.GetEffectSymbolOpt this.SetEffectSymbolOpt this
        member this.GetEffectStartTimeOpt world : int64 option = this.Get Property? EffectStartTimeOpt world
        member this.SetEffectStartTimeOpt (value : int64 option) world = this.Set Property? EffectStartTimeOpt value world
        member this.EffectStartTimeOpt = lens Property? EffectStartTimeOpt this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt this
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get Property? EffectDefinitions world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.Set Property? EffectDefinitions value world
        member this.EffectDefinitions = lens Property? EffectDefinitions this.GetEffectDefinitions this.SetEffectDefinitions this
        member this.GetEffect world : Effect = this.Get Property? Effect world
        member this.SetEffect (value : Effect) world = this.Set Property? Effect value world
        member this.Effect = lens Property? Effect this.GetEffect this.SetEffect this
        member this.GetEffectOffset world : Vector3 = this.Get Property? EffectOffset world
        member this.SetEffectOffset (value : Vector3) world = this.Set Property? EffectOffset value world
        member this.EffectOffset = lens Property? EffectOffset this.GetEffectOffset this.SetEffectOffset this
        member this.GetEffectCentered world : bool = this.Get Property? EffectCentered world
        member this.SetEffectCentered (value : bool) world = this.Set Property? EffectCentered value world
        member this.EffectCentered = lens Property? EffectCentered this.GetEffectCentered this.SetEffectCentered this
        member this.GetEffectTags world : EffectTags = this.Get Property? EffectTags world
        member private this.SetEffectTags (value : EffectTags) world = this.Set Property? EffectTags value world
        member this.EffectTags = lensReadOnly Property? EffectTags this.GetEffectTags this
        member this.GetEffectHistoryMax world : int = this.Get Property? EffectHistoryMax world
        member this.SetEffectHistoryMax (value : int) world = this.Set Property? EffectHistoryMax value world
        member this.EffectHistoryMax = lens Property? EffectHistoryMax this.GetEffectHistoryMax this.SetEffectHistoryMax this
        member this.GetEffectHistory world : Effects.Slice Deque = this.Get Property? EffectHistory world
        member private this.SetEffectHistory (value : Effects.Slice Deque) world = this.Set Property? EffectHistory value world
        member this.EffectHistory = lensReadOnly Property? EffectHistory this.GetEffectHistory this

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
                    if  effectTime >= dec lifetime && // NOTE: dec keeps effect from actualizing past the last frame when it is created mid-frame
                        (match ParticleSystem.getLiveness time particleSystem with Live -> false | Dead -> true) then
                        World.destroyEntity entity world
                    else world
                | (_, _) -> world
            else world

        override this.Actualize (entity, world) =

            // evaluate effect if visible
            if entity.GetVisible world then
                
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

                // actualize effect view
                let world = World.actualizeView view world

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

                // actualize particles
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

            // no need to evaluate non-visible effect
            else world

        override this.Register (entity, world) =
            let effectStartTime = Option.getOrDefault (World.getUpdateTime world) (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.monitor handleEffectsChanged (entity.GetChangeEvent Property? Effects) entity world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
        member this.GetBodyEnabled world : bool = this.Get Property? BodyEnabled world
        member this.SetBodyEnabled (value : bool) world = this.Set Property? BodyEnabled value world
        member this.BodyEnabled = lens Property? BodyEnabled this.GetBodyEnabled this.SetBodyEnabled this
        member this.GetBodyType world : BodyType = this.Get Property? BodyType world
        member this.SetBodyType (value : BodyType) world = this.Set Property? BodyType value world
        member this.BodyType = lens Property? BodyType this.GetBodyType this.SetBodyType this
        member this.GetAwake world : bool = this.Get Property? Awake world
        member this.SetAwake (value : bool) world = this.Set Property? Awake value world
        member this.Awake = lens Property? Awake this.GetAwake this.SetAwake this
        member this.GetDensity world : single = this.Get Property? Density world
        member this.SetDensity (value : single) world = this.Set Property? Density value world
        member this.Density = lens Property? Density this.GetDensity this.SetDensity this
        member this.GetFriction world : single = this.Get Property? Friction world
        member this.SetFriction (value : single) world = this.Set Property? Friction value world
        member this.Friction = lens Property? Friction this.GetFriction this.SetFriction this
        member this.GetRestitution world : single = this.Get Property? Restitution world
        member this.SetRestitution (value : single) world = this.Set Property? Restitution value world
        member this.Restitution = lens Property? Restitution this.GetRestitution this.SetRestitution this
        member this.GetLinearVelocity world : Vector3 = this.Get Property? LinearVelocity world
        member this.SetLinearVelocity (value : Vector3) world = this.Set Property? LinearVelocity value world
        member this.LinearVelocity = lens Property? LinearVelocity this.GetLinearVelocity this.SetLinearVelocity this
        member this.GetLinearDamping world : single = this.Get Property? LinearDamping world
        member this.SetLinearDamping (value : single) world = this.Set Property? LinearDamping value world
        member this.LinearDamping = lens Property? LinearDamping this.GetLinearDamping this.SetLinearDamping this
        member this.GetAngularVelocity world : Vector3 = this.Get Property? AngularVelocity world
        member this.SetAngularVelocity (value : Vector3) world = this.Set Property? AngularVelocity value world
        member this.AngularVelocity = lens Property? AngularVelocity this.GetAngularVelocity this.SetAngularVelocity this
        member this.GetAngularDamping world : single = this.Get Property? AngularDamping world
        member this.SetAngularDamping (value : single) world = this.Set Property? AngularDamping value world
        member this.AngularDamping = lens Property? AngularDamping this.GetAngularDamping this.SetAngularDamping this
        member this.GetFixedRotation world : bool = this.Get Property? FixedRotation world
        member this.SetFixedRotation (value : bool) world = this.Set Property? FixedRotation value world
        member this.FixedRotation = lens Property? FixedRotation this.GetFixedRotation this.SetFixedRotation this
        member this.GetInertia world : single = this.Get Property? Inertia world
        member this.SetInertia (value : single) world = this.Set Property? Inertia value world
        member this.Inertia = lens Property? Inertia this.GetInertia this.SetInertia this
        member this.GetGravityScale world : single = this.Get Property? GravityScale world
        member this.SetGravityScale (value : single) world = this.Set Property? GravityScale value world
        member this.GravityScale = lens Property? GravityScale this.GetGravityScale this.SetGravityScale this
        member this.GetCollisionCategories world : string = this.Get Property? CollisionCategories world
        member this.SetCollisionCategories (value : string) world = this.Set Property? CollisionCategories value world
        member this.CollisionCategories = lens Property? CollisionCategories this.GetCollisionCategories this.SetCollisionCategories this
        member this.GetCollisionMask world : string = this.Get Property? CollisionMask world
        member this.SetCollisionMask (value : string) world = this.Set Property? CollisionMask value world
        member this.CollisionMask = lens Property? CollisionMask this.GetCollisionMask this.SetCollisionMask this
        member this.GetBodyShape world : BodyShape = this.Get Property? BodyShape world
        member this.SetBodyShape (value : BodyShape) world = this.Set Property? BodyShape value world
        member this.BodyShape = lens Property? BodyShape this.GetBodyShape this.SetBodyShape this
        member this.GetIgnoreCCD world : bool = this.Get Property? IgnoreCCD world
        member this.SetIgnoreCCD (value : bool) world = this.Set Property? IgnoreCCD value world
        member this.IgnoreCCD = lens Property? IgnoreCCD this.GetIgnoreCCD this.SetIgnoreCCD this
        member this.GetBullet world : bool = this.Get Property? Bullet world
        member this.SetBullet (value : bool) world = this.Set Property? Bullet value world
        member this.Bullet = lens Property? Bullet this.GetBullet this.SetBullet this
        member this.GetSensor world : bool = this.Get Property? Sensor world
        member this.SetSensor (value : bool) world = this.Set Property? Sensor value world
        member this.Sensor = lens Property? Sensor this.GetSensor this.SetSensor this
        member this.GetPhysicsId world : PhysicsId = this.Get Property? PhysicsId world
        member this.PhysicsId = lensReadOnly Property? PhysicsId this.GetPhysicsId this
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
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = Gen.idEmpty }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Transform) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? BodyEnabled) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? BodyType) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Awake) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Density) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Friction) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Restitution) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? LinearVelocity) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? LinearDamping) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? AngularVelocity) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? AngularDamping) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? FixedRotation) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Inertia) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? GravityScale) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionCategories) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionMask) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? BodyShape) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? IgnoreCCD) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Bullet) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Sensor) entity world
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
        member this.GetJointDevice world : JointDevice = this.Get Property? JointDevice world
        member this.SetJointDevice (value : JointDevice) world = this.Set Property? JointDevice value world
        member this.JointDevice = lens Property? JointDevice this.GetJointDevice this.SetJointDevice this

    type JointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.JointDevice JointEmpty
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = Gen.idEmpty }) None]

        override this.Register (entity, world) =
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Transform) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? JointDevice) entity world
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
        member this.GetTileLayerClearance world : single = this.Get Property? TileLayerClearance world
        member this.SetTileLayerClearance (value : single) world = this.Set Property? TileLayerClearance value world
        member this.TileLayerClearance = lens Property? TileLayerClearance this.GetTileLayerClearance this.SetTileLayerClearance this
        member this.GetTileIndexOffset world : int = this.Get Property? TileIndexOffset world
        member this.SetTileIndexOffset (value : int) world = this.Set Property? TileIndexOffset value world
        member this.TileIndexOffset = lens Property? TileIndexOffset this.GetTileIndexOffset this.SetTileIndexOffset this
        member this.GetTileIndexOffsetRange world : int * int = this.Get Property? TileIndexOffsetRange world
        member this.SetTileIndexOffsetRange (value : int * int) world = this.Set Property? TileIndexOffsetRange value world
        member this.TileIndexOffsetRange = lens Property? TileIndexOffsetRange this.GetTileIndexOffsetRange this.SetTileIndexOffsetRange this
        member this.GetTileMap world : TileMap AssetTag = this.Get Property? TileMap world
        member this.SetTileMap (value : TileMap AssetTag) world = this.Set Property? TileMap value world
        member this.TileMap = lens Property? TileMap this.GetTileMap this.SetTileMap this

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
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = Gen.idEmpty }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? BodyEnabled) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Transform) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Friction) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Restitution) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionCategories) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionMask) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? TileMap) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent Property? TmxMap)
                    entity
                    world
            world

        override this.RegisterPhysics (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) world with
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                match TmxMap.tryGetTileMap (entity.GetTileMap world) world with
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
            else world

        override this.GetQuickSize (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) world with
            | Some tileMap -> TmxMap.getQuickSize tileMap
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module TmxMapFacetModule =

    type Entity with
        member this.GetTmxMap world : TmxMap = this.Get Property? TmxMap world
        member this.SetTmxMap (value : TmxMap) world = this.Set Property? TmxMap value world
        member this.TmxMap = lens Property? TmxMap this.GetTmxMap this.SetTmxMap this

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
             computed Entity.PhysicsId (fun (entity : Entity) world -> { SourceId = entity.GetId world; CorrelationId = Gen.idEmpty }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? BodyEnabled) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Transform) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Friction) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? Restitution) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionCategories) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? CollisionMask) entity world
            let world = World.monitor (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent Property? TmxMap) entity world
            let world =
                World.monitor (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent Property? TmxMap)
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
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
            else world

        override this.GetQuickSize (entity, world) =
            let tmxMap = entity.GetTmxMap world
            TmxMap.getQuickSize tmxMap

[<AutoOpen>]
module LightFacetModule =

    type Entity with
        member this.GetLightType world : LightType = this.Get Property? LightType world
        member this.SetLightType (value : LightType) world = this.Set Property? LightType value world
        member this.LightType = lens Property? LightType this.GetLightType this.SetLightType this
        member this.GetBrightness world : single = this.Get Property? Brightness world
        member this.SetBrightness (value : single) world = this.Set Property? Brightness value world
        member this.Brightness = lens Property? Brightness this.GetBrightness this.SetBrightness this
        member this.GetIntensity world : single = this.Get Property? Intensity world
        member this.SetIntensity (value : single) world = this.Set Property? Intensity value world
        member this.Intensity = lens Property? Intensity this.GetIntensity this.SetIntensity this

    type LightFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.LightType PointLight
             define Entity.Brightness 1000.0f
             define Entity.Intensity 1.0f]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let position = entity.GetPosition world
                let color = entity.GetColor world
                let brightness = entity.GetBrightness world
                let intensity = entity.GetIntensity world
                let lightType = entity.GetLightType world
                World.enqueueRenderMessage3d (RenderLightDescriptor (position, color, brightness, intensity, lightType)) world
            else world

[<AutoOpen>]
module SkyBoxFacetModule =

    type Entity with
        member this.GetCubeMap world : CubeMap AssetTag = this.Get Property? CubeMap world
        member this.SetCubeMap (value : CubeMap AssetTag) world = this.Set Property? CubeMap value world
        member this.CubeMap = lens Property? CubeMap this.GetCubeMap this.SetCubeMap this

    type SkyBoxFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.CubeMap Assets.Default.SkyBoxMap]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let cubeMap = entity.GetCubeMap world
                World.enqueueRenderMessage3d (RenderSkyBoxDescriptor cubeMap) world
            else world

[<AutoOpen>]
module StaticModelFacetModule =

    type RenderStyle =
        | Deferred
        | Forward

    type Entity with
        member this.GetStaticModel world : StaticModel AssetTag = this.Get Property? StaticModel world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set Property? StaticModel value world
        member this.StaticModel = lens Property? StaticModel this.GetStaticModel this.SetStaticModel this
        member this.GetAlbedoOpt world : Color option = this.Get Property? AlbedoOpt world
        member this.SetAlbedoOpt (value : Color option) world = this.Set Property? AlbedoOpt value world
        member this.AlbedoOpt = lens Property? AlbedoOpt this.GetAlbedoOpt this.SetAlbedoOpt this
        member this.GetMetalnessOpt world : single option = this.Get Property? MetalnessOpt world
        member this.SetMetalnessOpt (value : single option) world = this.Set Property? MetalnessOpt value world
        member this.MetalnessOpt = lens Property? MetalnessOpt this.GetMetalnessOpt this.SetMetalnessOpt this
        member this.GetRoughnessOpt world : single option = this.Get Property? RoughnessOpt world
        member this.SetRoughnessOpt (value : single option) world = this.Set Property? RoughnessOpt value world
        member this.RoughnessOpt = lens Property? RoughnessOpt this.GetRoughnessOpt this.SetRoughnessOpt this
        member this.GetAmbientOcclusionOpt world : single option = this.Get Property? AmbientOcclusionOpt world
        member this.SetAmbientOcclusionOpt (value : single option) world = this.Set Property? AmbientOcclusionOpt value world
        member this.AmbientOcclusionOpt = lens Property? AmbientOcclusionOpt this.GetAmbientOcclusionOpt this.SetAmbientOcclusionOpt this
        member this.GetRenderStyle world : RenderStyle = this.Get Property? RenderStyle world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set Property? RenderStyle value world
        member this.RenderStyle = lens Property? RenderStyle this.GetRenderStyle this.SetRenderStyle this

    type StaticModelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.StaticModel Assets.Default.StaticModel
             define Entity.AlbedoOpt None
             define Entity.MetalnessOpt None
             define Entity.RoughnessOpt None
             define Entity.AmbientOcclusionOpt None
             define Entity.RenderStyle Deferred]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let absolute = transform.Absolute
                let affineMatrix = transform.AffineMatrix
                let staticModel = entity.GetStaticModel world
                let renderMaterial =
                    { AlbedoOpt = entity.GetAlbedoOpt world
                      MetalnessOpt = entity.GetMetalnessOpt world
                      RoughnessOpt = entity.GetRoughnessOpt world
                      AmbientOcclusionOpt = entity.GetAmbientOcclusionOpt world }
                let renderType =
                    match entity.GetRenderStyle world with
                    | Deferred -> DeferredRenderType
                    | Forward -> ForwardRenderType
                World.enqueueRenderMessage3d (RenderStaticModelDescriptor (absolute, affineMatrix, renderMaterial, renderType, staticModel)) world
            else world

        override this.GetQuickSize (entity, world) =
            let staticModel = entity.GetStaticModel world
            let staticModelMetadata = World.getStaticModelMetadata staticModel world
            let bounds = staticModelMetadata.Bounds
            let boundsExtended = bounds.Combine bounds.Mirror
            boundsExtended.Size

        override this.RayCast (ray, entity, world) =
            match World.tryGetStaticModelMetadata (entity.GetStaticModel world) world with
            | Some staticModel ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface =
                            Ray
                                (Vector3.Transform (ray.Position, inverse),
                                 Vector3.Normalize (Vector3.TransformNormal (ray.Direction, surface.SurfaceMatrix)))
                        let mutable bounds = geometry.Bounds
                        let boundsIntersectionOpt = raySurface.Intersects bounds
                        if boundsIntersectionOpt.HasValue then
                            let intersections = raySurface.GetIntersections (geometry.Indices, geometry.Vertices)
                            intersections |> Seq.map snd' |> Seq.toArray
                        else [||])
                        staticModel.PhysicallyBasedSurfaces
                Array.concat intersectionses
            | None -> [||]

[<AutoOpen>]
module StaticModelSurfaceFacetModule =

    type Entity with
        member this.GetSurfaceIndex world : int = this.Get Property? SurfaceIndex world
        member this.SetSurfaceIndex (value : int) world = this.Set Property? SurfaceIndex value world
        member this.SurfaceIndex = lens Property? SurfaceIndex this.GetSurfaceIndex this.SetSurfaceIndex this

    type StaticModelSurfaceFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.SurfaceIndex 0
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.AlbedoOpt None
             define Entity.MetalnessOpt None
             define Entity.RoughnessOpt None
             define Entity.AmbientOcclusionOpt None
             define Entity.RenderStyle Deferred]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                match entity.GetSurfaceIndex world with
                | -1 -> world
                | surfaceIndex ->
                    let mutable transform = entity.GetTransform world
                    let absolute = transform.Absolute
                    let affineMatrix = transform.AffineMatrix
                    let staticModel = entity.GetStaticModel world
                    let renderMaterial =
                        { AlbedoOpt = entity.GetAlbedoOpt world
                          MetalnessOpt = entity.GetMetalnessOpt world
                          RoughnessOpt = entity.GetRoughnessOpt world
                          AmbientOcclusionOpt = entity.GetAmbientOcclusionOpt world }
                    let renderType =
                        match entity.GetRenderStyle world with
                        | Deferred -> DeferredRenderType
                        | Forward -> ForwardRenderType
                    World.enqueueRenderMessage3d (RenderStaticModelSurfaceDescriptor (absolute, affineMatrix, renderMaterial, renderType, staticModel, surfaceIndex)) world
            else world

        override this.GetQuickSize (entity, world) =
            let staticModel = World.getStaticModelMetadata (entity.GetStaticModel world) world
            let surfaceIndex = entity.GetSurfaceIndex world
            if surfaceIndex > -1 && surfaceIndex < staticModel.PhysicallyBasedSurfaces.Length then
                let bounds = staticModel.PhysicallyBasedSurfaces.[surfaceIndex].SurfaceBounds
                let boundsExtended = bounds.Combine bounds.Mirror
                boundsExtended.Size
            else Constants.Engine.EntitySize3dDefault

        override this.RayCast (ray, entity, world) =
            match World.tryGetStaticModelMetadata (entity.GetStaticModel world) world with
            | Some staticModel ->
                let surfaceIndex = entity.GetSurfaceIndex world
                let surface = staticModel.PhysicallyBasedSurfaces.[surfaceIndex]
                let geometry = surface.PhysicallyBasedGeometry
                let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                let raySurface =
                    Ray
                        (Vector3.Transform (ray.Position, inverse),
                         Vector3.Normalize (Vector3.TransformNormal (ray.Direction, inverse)))
                let mutable bounds = geometry.Bounds
                let boundsIntersectionOpt = raySurface.Intersects bounds
                if boundsIntersectionOpt.HasValue then
                    let intersections = raySurface.GetIntersections (geometry.Indices, geometry.Vertices)
                    intersections |> Seq.map snd' |> Seq.toArray
                else [||]
            | None -> [||]

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

    type World with

        static member internal signalEntity<'model, 'message, 'command> signal (entity : Entity) world =
            match entity.GetDispatcher world with
            | :? EntityDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (entity.ModelGeneric<'model> ()) signal entity world
            | _ ->
                Log.info "Failed to send signal to entity."
                world

    and Entity with

        member this.UpdateModel<'model> updater world =
            this.SetModelGeneric<'model> (updater (this.GetModelGeneric<'model> world)) world

        member this.Signal<'model, 'message, 'command> signal world =
            World.signalEntity<'model, 'message, 'command> signal this world

    and [<AbstractClass>] EntityDispatcher<'model, 'message, 'command> (is2d, centered, physical, initial : 'model) =
        inherit EntityDispatcher (is2d, centered, physical)

        member this.GetModel (entity : Entity) world : 'model =
            entity.GetModelGeneric<'model> world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetModelGeneric<'model> model world

        member this.Model (entity : Entity) =
            lens Property? Model (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let world =
                let property = World.getEntityModelProperty entity world
                if property.DesignerType = typeof<unit> then
                    let model = this.Prepare (initial, world)
                    entity.SetModelGeneric<'model> model world
                else world
            let channels = this.Channel (this.Model entity, entity)
            let world = Signal.processChannels this.Message this.Command (this.Model entity) channels entity world
            let content = this.Content (this.Model entity, entity)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent content (SimulantOrigin entity) entity entity.Group world |> snd)
                    world content
            let initializers = this.Initializers (this.Model entity, entity)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName property entity world |> snd'
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> entity
                    World.monitor (fun (evt : Event) world ->
                        let world = WorldModule.trySignal (handler evt) entity world
                        (Cascade, world))
                        eventAddress (entity :> Simulant) world
                | BindDefinition (left, right) ->
                    WorldModule.bind5 entity left right world
                | LinkDefinition (left, right) ->
                    let world = WorldModule.bind5 entity left right world
                    WorldModule.bind5 right.This right left world)
                world initializers

        override this.ApplyPhysics (position, rotation, linearVelocity, angularVelocity, entity, world) =
            let model = this.GetModel entity world
            let (signals, model) = this.Physics (position, rotation, linearVelocity, angularVelocity, model, entity, world)
            let world = this.SetModel model entity world
            Signal.processSignals this.Message this.Command (this.Model entity) signals entity world

        override this.Actualize (entity, world) =
            let view = this.View (this.GetModel entity world, entity, world)
            World.actualizeView view world

        override this.TrySignalFacet (signalObj : obj, facetName : string, entity : Entity, world : World) : World =
            entity.TrySignalFacet signalObj facetName world

        override this.TrySignal (signalObj, entity, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> entity.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> entity.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Prepare : 'model * World -> 'model
        default this.Prepare (model, _) = model

        abstract member Channel : Lens<'model, World> * Entity -> Channel<'message, 'command, Entity, World> list
        default this.Channel (_, _) = []

        abstract member Initializers : Lens<'model, World> * Entity -> PropertyInitializer list
        default this.Initializers (_, _) = []

        abstract member Physics : Vector3 * Quaternion * Vector3 * Vector3 * 'model * Entity * World -> Signal<'message, 'command> list * 'model
        default this.Physics (_, _, _, _, model, _, _) = just model

        abstract member Message : 'model * 'message * Entity * World -> Signal<'message, 'command> list * 'model
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Entity * World -> Signal<'message, 'command> list * World
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Entity -> EntityContent list
        default this.Content (_, _) = []

        abstract member View : 'model * Entity * World -> View
        default this.View (_, _, _) = View.empty

    and [<AbstractClass>] EntityDispatcher2d<'model, 'message, 'command> (centered, physical, initial) =
        inherit EntityDispatcher<'model, 'message, 'command> (true, centered, physical, initial)

        static member Properties =
            [define Entity.Centered false
             define Entity.Size Constants.Engine.EntitySize2dDefault]

    and [<AbstractClass>] EntityDispatcher3d<'model, 'message, 'command> (centered, physical, initial) =
        inherit EntityDispatcher<'model, 'message, 'command> (false, centered, physical, initial)

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
        member this.GetDisabledColor world : Color = this.Get Property? DisabledColor world
        member this.SetDisabledColor (value : Color) world = this.Set Property? DisabledColor value world
        member this.DisabledColor = lens Property? DisabledColor this.GetDisabledColor this.SetDisabledColor this

    type GuiDispatcher () =
        inherit EntityDispatcher2d (false, false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Presence Omnipresent
             define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))]

    type [<AbstractClass>] GuiDispatcher<'model, 'message, 'command> (model) =
        inherit EntityDispatcher2d<'model, 'message, 'command> (false, false, model)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.Absolute true
             define Entity.AlwaysUpdate true
             define Entity.Size Constants.Engine.EntitySizeGuiDefault
             define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))]

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
        member this.GetDown world : bool = this.Get Property? Down world
        member this.SetDown (value : bool) world = this.Set Property? Down value world
        member this.Down = lens Property? Down this.GetDown this.SetDown this
        member this.GetDownTextOffset world : Vector3 = this.Get Property? DownTextOffset world
        member this.SetDownTextOffset (value : Vector3) world = this.Set Property? DownTextOffset value world
        member this.DownTextOffset = lens Property? DownTextOffset this.GetDownTextOffset this.SetDownTextOffset this
        member this.GetUpImage world : Image AssetTag = this.Get Property? UpImage world
        member this.SetUpImage (value : Image AssetTag) world = this.Set Property? UpImage value world
        member this.UpImage = lens Property? UpImage this.GetUpImage this.SetUpImage this
        member this.GetDownImage world : Image AssetTag = this.Get Property? DownImage world
        member this.SetDownImage (value : Image AssetTag) world = this.Set Property? DownImage value world
        member this.DownImage = lens Property? DownImage this.GetDownImage this.SetDownImage this
        member this.GetClickSoundOpt world : Sound AssetTag option = this.Get Property? ClickSoundOpt world
        member this.SetClickSoundOpt (value : Sound AssetTag option) world = this.Set Property? ClickSoundOpt value world
        member this.ClickSoundOpt = lens Property? ClickSoundOpt this.GetClickSoundOpt this.SetClickSoundOpt this
        member this.GetClickSoundVolume world : single = this.Get Property? ClickSoundVolume world
        member this.SetClickSoundVolume (value : single) world = this.Set Property? ClickSoundVolume value world
        member this.ClickSoundVolume = lens Property? ClickSoundVolume this.GetClickSoundVolume this.SetClickSoundVolume this
        member this.UpEvent = Events.Up --> this
        member this.DownEvent = Events.Down --> this
        member this.ClickEvent = Events.Click --> this

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
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
            let data = evt.Data : MouseButtonData
            let wasDown = entity.GetDown world
            let world = entity.SetDown false world
            let world = entity.SetTextOffset v3Zero world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute
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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetUpImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
        member this.GetLabelImage world : Image AssetTag = this.Get Property? LabelImage world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set Property? LabelImage value world
        member this.LabelImage = lens Property? LabelImage this.GetLabelImage this.SetLabelImage this

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.LabelImage Assets.Default.Image3]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute
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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetLabelImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
        member this.GetBackgroundImageOpt world : Image AssetTag option = this.Get Property? BackgroundImageOpt world
        member this.SetBackgroundImageOpt (value : Image AssetTag option) world = this.Set Property? BackgroundImageOpt value world
        member this.BackgroundImageOpt = lens Property? BackgroundImageOpt this.GetBackgroundImageOpt this.SetBackgroundImageOpt this

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member Facets =
            [typeof<TextFacet>]

        static member Properties =
            [define Entity.BackgroundImageOpt None
             define Entity.Justification (Justified (JustifyLeft, JustifyMiddle))]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                match entity.GetBackgroundImageOpt world with
                | Some spriteImage ->
                    let mutable transform = entity.GetTransform world
                    let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute
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
            else world

        override this.GetQuickSize (entity, world) =
            match entity.GetBackgroundImageOpt world with
            | Some image ->
                match World.tryGetTextureSizeF image world with
                | Some size -> size.V3
                | None -> Constants.Engine.EntitySizeGuiDefault
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module ToggleButtonDispatcherModule =

    type Entity with
        member this.GetToggled world : bool = this.Get Property? Toggled world
        member this.SetToggled (value : bool) world = this.Set Property? Toggled value world
        member this.Toggled = lens Property? Toggled this.GetToggled this.SetToggled this
        member this.GetToggledTextOffset world : Vector3 = this.Get Property? ToggledTextOffset world
        member this.SetToggledTextOffset (value : Vector3) world = this.Set Property? ToggledTextOffset value world
        member this.ToggledTextOffset = lens Property? ToggledTextOffset this.GetToggledTextOffset this.SetToggledTextOffset this
        member this.GetPressed world : bool = this.Get Property? Pressed world
        member this.SetPressed (value : bool) world = this.Set Property? Pressed value world
        member this.Pressed = lens Property? Pressed this.GetPressed this.SetPressed this
        member this.GetPressedTextOffset world : Vector3 = this.Get Property? PressedTextOffset world
        member this.SetPressedTextOffset (value : Vector3) world = this.Set Property? PressedTextOffset value world
        member this.PressedTextOffset = lens Property? PressedTextOffset this.GetPressedTextOffset this.SetPressedTextOffset this
        member this.GetUntoggledImage world : Image AssetTag = this.Get Property? UntoggledImage world
        member this.SetUntoggledImage (value : Image AssetTag) world = this.Set Property? UntoggledImage value world
        member this.UntoggledImage = lens Property? UntoggledImage this.GetUntoggledImage this.SetUntoggledImage this
        member this.GetToggledImage world : Image AssetTag = this.Get Property? ToggledImage world
        member this.SetToggledImage (value : Image AssetTag) world = this.Set Property? ToggledImage value world
        member this.ToggledImage = lens Property? ToggledImage this.GetToggledImage this.SetToggledImage this
        member this.GetToggleSoundOpt world : Sound AssetTag option = this.Get Property? ToggleSoundOpt world
        member this.SetToggleSoundOpt (value : Sound AssetTag option) world = this.Set Property? ToggleSoundOpt value world
        member this.ToggleSoundOpt = lens Property? ToggleSoundOpt this.GetToggleSoundOpt this.SetToggleSoundOpt this
        member this.GetToggleSoundVolume world : single = this.Get Property? ToggleSoundVolume world
        member this.SetToggleSoundVolume (value : single) world = this.Set Property? ToggleSoundVolume value world
        member this.ToggleSoundVolume = lens Property? ToggleSoundVolume this.GetToggleSoundVolume this.SetToggleSoundVolume this
        member this.ToggleEvent = Events.Toggle --> this
        member this.ToggledEvent = Events.Toggled --> this
        member this.UntoggledEvent = Events.Untoggled --> this

    type ToggleButtonDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute
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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetUntoggledImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module RadioButtonDispatcherModule =

    type Entity with
        member this.GetDialed world : bool = this.Get Property? Dialed world
        member this.SetDialed (value : bool) world = this.Set Property? Dialed value world
        member this.Dialed = lens Property? Dialed this.GetDialed this.SetDialed this
        member this.GetDialedTextOffset world : Vector3 = this.Get Property? DialedTextOffset world
        member this.SetDialedTextOffset (value : Vector3) world = this.Set Property? DialedTextOffset value world
        member this.DialedTextOffset = lens Property? DialedTextOffset this.GetDialedTextOffset this.SetDialedTextOffset this
        member this.GetUndialedImage world : Image AssetTag = this.Get Property? UndialedImage world
        member this.SetUndialedImage (value : Image AssetTag) world = this.Set Property? UndialedImage value world
        member this.UndialedImage = lens Property? UndialedImage this.GetUndialedImage this.SetUndialedImage this
        member this.GetDialedImage world : Image AssetTag = this.Get Property? DialedImage world
        member this.SetDialedImage (value : Image AssetTag) world = this.Set Property? DialedImage value world
        member this.DialedImage = lens Property? DialedImage this.GetDialedImage this.SetDialedImage this
        member this.GetDialSoundOpt world : Sound AssetTag option = this.Get Property? DialSoundOpt world
        member this.SetDialSoundOpt (value : Sound AssetTag option) world = this.Set Property? DialSoundOpt value world
        member this.DialSoundOpt = lens Property? DialSoundOpt this.GetDialSoundOpt this.SetDialSoundOpt this
        member this.GetDialSoundVolume world : single = this.Get Property? DialSoundVolume world
        member this.SetDialSoundVolume (value : single) world = this.Set Property? DialSoundVolume value world
        member this.DialSoundVolume = lens Property? DialSoundVolume this.GetDialSoundVolume this.SetDialSoundVolume this
        member this.DialEvent = Events.Dial --> this
        member this.DialedEvent = Events.Dialed --> this
        member this.UndialedEvent = Events.Undialed --> this

    type RadioButtonDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            let wasDialed = entity.GetDialed world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute
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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetUndialedImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module FpsDispatcherModule =

    type Entity with
        member this.GetStartTime world : int64 = this.Get Property? StartTime world
        member this.SetStartTime (value : int64) world = this.Set Property? StartTime value world
        member this.StartTime = lens Property? StartTime this.GetStartTime this.SetStartTime this
        member this.GetStartDateTime world : DateTime = this.Get Property? StartDateTime world
        member this.SetStartDateTime (value : DateTime) world = this.Set Property? StartDateTime value world
        member this.StartDateTime = lens Property? StartDateTime this.GetStartDateTime this.SetStartDateTime this

    type FpsDispatcher () =
        inherit TextDispatcher ()

        static let resetIntermittent (entity : Entity) world =
            let startDateTime = entity.GetStartDateTime world
            let currentDateTime = DateTime.UtcNow
            let elapsedDateTime = currentDateTime - startDateTime
            if elapsedDateTime.TotalSeconds >= 3.0 then
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
        member this.GetTouched world : bool = this.Get Property? Touched world
        member this.SetTouched (value : bool) world = this.Set Property? Touched value world
        member this.Touched = lens Property? Touched this.GetTouched this.SetTouched this
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
                let mousePositionWorld = World.mouseToWorld2d transform.Absolute data.Position world
                if Math.isPointInBounds2d mousePositionWorld transform.Perimeter.Box2 then // gui currently ignores rotation
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
        member this.GetFill world : single = this.Get Property? Fill world
        member this.SetFill (value : single) world = this.Set Property? Fill value world
        member this.Fill = lens Property? Fill this.GetFill this.SetFill this
        member this.GetFillInset world : single = this.Get Property? FillInset world
        member this.SetFillInset (value : single) world = this.Set Property? FillInset value world
        member this.FillInset = lens Property? FillInset this.GetFillInset this.SetFillInset this
        member this.GetFillColor world : Color = this.Get Property? FillColor world
        member this.SetFillColor (value : Color) world = this.Set Property? FillColor value world
        member this.FillColor = lens Property? FillColor this.GetFillColor this.SetFillColor this
        member this.GetFillImage world : Image AssetTag = this.Get Property? FillImage world
        member this.SetFillImage (value : Image AssetTag) world = this.Set Property? FillImage value world
        member this.FillImage = lens Property? FillImage this.GetFillImage this.SetFillImage this
        member this.GetBorderColor world : Color = this.Get Property? BorderColor world
        member this.SetBorderColor (value : Color) world = this.Set Property? BorderColor value world
        member this.BorderColor = lens Property? BorderColor this.GetBorderColor this.SetBorderColor this
        member this.GetBorderImage world : Image AssetTag = this.Get Property? BorderImage world
        member this.SetBorderImage (value : Image AssetTag) world = this.Set Property? BorderImage value world
        member this.BorderImage = lens Property? BorderImage this.GetBorderImage this.SetBorderImage this

    type FillBarDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.Image11
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Image4]

        override this.Actualize (entity, world) =
            if entity.GetVisible world then

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
            else world

        override this.GetQuickSize (entity, world) =
            match World.tryGetTextureSizeF (entity.GetBorderImage world) world with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module BasicEmitter2dDispatcherModule =

    type BasicEmitter2dDispatcher () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<BasicEmitter2dFacet>]

        static member Properties =
            [define Entity.Centered true]

[<AutoOpen>]
module Effect2dDispatcherModule =

    type Effect2dDispatcher () =
        inherit EntityDispatcher2d (true, false)

        static member Facets =
            [typeof<Effect2dFacet>]

        static member Properties =
            [define Entity.Centered true
             define Entity.Effect (scvalue<Effect> "[Effect None [] [Contents [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]")]

[<AutoOpen>]
module Block2dDispatcherModule =

    type Block2dDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage Assets.Default.Image6]

[<AutoOpen>]
module Box2dDispatcherModule =

    type Box2dDispatcher () =
        inherit EntityDispatcher2d (false, true)

        static member Facets =
            [typeof<RigidBodyFacet>
             typeof<StaticSpriteFacet>]

        static member Properties =
            [define Entity.StaticImage Assets.Default.Image6]

[<AutoOpen>]
module SideViewCharacterDispatcherModule =

    type Entity with
        member this.GetSideViewCharacterIdleImage world : Image AssetTag = this.Get Property? SideViewCharacterIdleImage world
        member this.SetSideViewCharacterIdleImage (value : Image AssetTag) world = this.Set Property? SideViewCharacterIdleImage value world
        member this.SideViewCharacterIdleImage = lens Property? SideViewCharacterIdleImage this.GetSideViewCharacterIdleImage this.SetSideViewCharacterIdleImage this
        member this.GetSideViewCharacterJumpImage world : Image AssetTag = this.Get Property? SideViewCharacterJumpImage world
        member this.SetSideViewCharacterJumpImage (value : Image AssetTag) world = this.Set Property? SideViewCharacterJumpImage value world
        member this.SideViewCharacterJumpImage = lens Property? SideViewCharacterJumpImage this.GetSideViewCharacterJumpImage this.SetSideViewCharacterJumpImage this
        member this.GetSideViewCharacterWalkSheet world : Image AssetTag = this.Get Property? SideViewCharacterWalkSheet world
        member this.SetSideViewCharacterWalkSheet (value : Image AssetTag) world = this.Set Property? SideViewCharacterWalkSheet value world
        member this.SideViewCharacterWalkSheet = lens Property? SideViewCharacterWalkSheet this.GetSideViewCharacterWalkSheet this.SetSideViewCharacterWalkSheet this
        member this.GetSideViewCharacterFacingLeft world : bool = this.Get Property? SideViewCharacterFacingLeft world
        member this.SetSideViewCharacterFacingLeft (value : bool) world = this.Set Property? SideViewCharacterFacingLeft value world
        member this.SideViewCharacterFacingLeft = lens Property? SideViewCharacterFacingLeft this.GetSideViewCharacterFacingLeft this.SetSideViewCharacterFacingLeft this

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

        override this.Actualize (entity, world) =
            if entity.GetVisible world then
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
            else world

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
module LightDispatcherModule =

    type LightDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static member Facets =
            [typeof<LightFacet>]

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.LightType PointLight
             define Entity.Brightness 10.0f
             define Entity.Intensity 1.0f]

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
module StaticSceneDispatcherModule =

    type Entity with
        member this.GetStaticScene world : StaticModel AssetTag = this.Get Property? StaticScene world
        member this.SetStaticScene (value : StaticModel AssetTag) world = this.Set Property? StaticScene value world
        member this.StaticScene = lens Property? StaticScene this.GetStaticScene this.SetStaticScene this

    type StaticSceneDispatcher () =
        inherit EntityDispatcher3d (true, false)

        static let destroyChildren (entity : Entity) world =
            Seq.fold (fun world child ->
                World.destroyEntity child world)
                world (entity.GetChildren world)

        static let tryCreateChildren (entity : Entity) world =
            let staticModel = entity.GetStaticScene world
            match World.tryGetStaticModelMetadata staticModel world with
            | Some staticModelMetadata ->
                // Unity Scene Export Instructions:
                // 1) have FBX Exporter package installed
                // 2) be in PBR Unity Project
                // 3) put all desired objects in empty root GameObject
                // 4) export root GameObject
                // 5) delete all fbx files except the one you exported
                Seq.foldi (fun surfaceIndex world (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                    let childSurnames = Array.add surface.SurfaceName entity.Surnames // NOTE: we may need to check if an entity with the same address already exists because surface name is non-unique.
                    let (child, world) = World.createEntity<StaticModelSurfaceDispatcher> (Some childSurnames) DefaultOverlay Simulants.Default.Group world
                    let bounds = surface.SurfaceBounds
                    let boundsExtended = bounds.Combine bounds.Mirror
                    let transform = surface.SurfaceMatrix
                    let position = transform.Translation
                    let mutable rotation = transform
                    rotation.Translation <- v3Zero
                    let rotation = Quaternion.CreateFromRotationMatrix rotation
                    let scale = transform.Scale ()
                    let world = child.SetSurfaceIndex surfaceIndex world
                    let world = child.SetStaticModel staticModel world
                    let world = child.SetPositionLocal position world
                    let world = child.SetRotationLocal rotation world
                    let world = child.SetScaleLocal scale world
                    let world = child.SetSize boundsExtended.Size world
                    let world = child.SetAlbedoOpt (Some surface.PhysicallyBasedMaterial.Albedo) world
                    let world = child.SetMetalnessOpt (Some surface.PhysicallyBasedMaterial.Metalness) world
                    let world = child.SetRoughnessOpt (Some surface.PhysicallyBasedMaterial.Roughness) world
                    let world = child.SetAmbientOcclusionOpt (Some surface.PhysicallyBasedMaterial.AmbientOcclusion) world
                    let world = child.SetPersistent false world
                    let world = child.SetStatic (entity.GetStatic world) world
                    // TODO: 3D: set mount - let world = child.SetMountOpt ... world
                    world)
                    world staticModelMetadata.PhysicallyBasedSurfaces
            | None -> world

        static let syncChildren evt world =
            let entity = evt.Subscriber : Entity
            let world = destroyChildren entity world
            let world = tryCreateChildren entity world
            (Cascade, world)

        static let syncChildrenStatic evt world =
            let entity = evt.Subscriber : Entity
            let static_ = entity.GetStatic world
            let world =
                Seq.fold
                    (fun world (child : Entity) -> child.SetStatic static_ world)
                    world (entity.GetChildren world)
            (Cascade, world)

        static member Properties =
            [define Entity.StaticScene Assets.Default.StaticModel]

        override this.Register (entity, world) =
            let world = tryCreateChildren entity world
            let world = World.monitor syncChildren (entity.ChangeEvent Property? StaticScene) entity world
            let world = World.monitor syncChildrenStatic (entity.ChangeEvent Property? Static) entity world
            world

[<AutoOpen>]
module GroupDispatcherModule =

    type World with

        static member internal signalGroup<'model, 'message, 'command> signal (group : Group) world =
            match group.GetDispatcher world with
            | :? GroupDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (group.ModelGeneric<'model> ()) signal group world
            | _ ->
                Log.info "Failed to send signal to group."
                world

    and Group with

        member this.UpdateModel<'model> updater world =
            this.SetModelGeneric<'model> (updater (this.GetModelGeneric<'model> world)) world

        member this.Signal<'model, 'message, 'command> signal world =
            World.signalGroup<'model, 'message, 'command> signal this world

    and [<AbstractClass>] GroupDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit GroupDispatcher ()

        member this.GetModel (group : Group) world : 'model =
            group.GetModelGeneric<'model> world

        member this.SetModel (model : 'model) (group : Group) world =
            group.SetModelGeneric<'model> model world

        member this.Model (group : Group) =
            lens Property? Model (this.GetModel group) (flip this.SetModel group) group

        override this.Register (group, world) =
            let world =
                let property = World.getGroupModelProperty group world
                if property.DesignerType = typeof<unit> then
                    let model = this.Prepare (initial, world)
                    group.SetModelGeneric<'model> model world
                else world
            let channels = this.Channel (this.Model group, group)
            let world = Signal.processChannels this.Message this.Command (this.Model group) channels group world
            let content = this.Content (this.Model group, group)
            let world =
                List.fold (fun world content ->
                    World.expandEntityContent content (SimulantOrigin group) group group world |> snd)
                    world content
            let initializers = this.Initializers (this.Model group, group)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName property group world |> snd'
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> group
                    World.monitor (fun (evt : Event) world ->
                        let world = WorldModule.trySignal (handler evt) group world
                        (Cascade, world))
                        eventAddress (group :> Simulant) world
                | BindDefinition (left, right) ->
                    WorldModule.bind5 group left right world
                | LinkDefinition (left, right) ->
                    let world = WorldModule.bind5 group left right world
                    WorldModule.bind5 right.This right left world)
                world initializers

        override this.Actualize (group, world) =
            let view = this.View (this.GetModel group world, group, world)
            World.actualizeView view world

        override this.TrySignal (signalObj, group, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> group.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> group.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Prepare : 'model * World -> 'model
        default this.Prepare (model, _) = model

        abstract member Channel : Lens<'model, World> * Group -> Channel<'message, 'command, Group, World> list
        default this.Channel (_, _) = []

        abstract member Initializers : Lens<'model, World> * Group -> PropertyInitializer list
        default this.Initializers (_, _) = []

        abstract member Message : 'model * 'message * Group * World -> Signal<'message, 'command> list * 'model
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Group * World -> Signal<'message, 'command> list * World
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Group -> EntityContent list
        default this.Content (_, _) = []

        abstract member View : 'model * Group * World -> View
        default this.View (_, _, _) = View.empty

[<AutoOpen>]
module ScreenDispatcherModule =

    type World with

        static member internal signalScreen<'model, 'message, 'command> signal (screen : Screen) world =
            match screen.GetDispatcher world with
            | :? ScreenDispatcher<'model, 'message, 'command> as dispatcher ->
                Signal.processSignal dispatcher.Message dispatcher.Command (screen.ModelGeneric<'model> ()) signal screen world
            | _ ->
                Log.info "Failed to send signal to screen."
                world

    and Screen with

        member this.UpdateModel<'model> updater world =
            this.SetModelGeneric<'model> (updater (this.GetModelGeneric<'model> world)) world

        member this.Signal<'model, 'message, 'command> signal world =
            World.signalScreen<'model, 'message, 'command> signal this world

    and [<AbstractClass>] ScreenDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit ScreenDispatcher ()

        member this.GetModel (screen : Screen) world : 'model =
            screen.GetModelGeneric<'model> world

        member this.SetModel (model : 'model) (screen : Screen) world =
            screen.SetModelGeneric<'model> model world

        member this.Model (screen : Screen) =
            lens Property? Model (this.GetModel screen) (flip this.SetModel screen) screen

        override this.Register (screen, world) =
            let world =
                let property = World.getScreenModelProperty screen world
                if property.DesignerType = typeof<unit> then
                    let model = this.Prepare (initial, world)
                    screen.SetModelGeneric<'model> model world
                else world
            let channels = this.Channel (this.Model screen, screen)
            let world = Signal.processChannels this.Message this.Command (this.Model screen) channels screen world
            let content = this.Content (this.Model screen, screen)
            let world =
                List.fold (fun world content ->
                    World.expandGroupContent content (SimulantOrigin screen) screen world |> snd)
                    world content
            let initializers = this.Initializers (this.Model screen, screen)
            List.fold (fun world initializer ->
                match initializer with
                | PropertyDefinition def ->
                    let property = { PropertyType = def.PropertyType; PropertyValue = PropertyExpr.eval def.PropertyExpr world }
                    World.setProperty def.PropertyName property screen world |> snd'
                | EventHandlerDefinition (handler, partialAddress) ->
                    let eventAddress = partialAddress --> screen
                    World.monitor (fun (evt : Event) world ->
                        let world = WorldModule.trySignal (handler evt) screen world
                        (Cascade, world))
                        eventAddress (screen :> Simulant) world
                | BindDefinition (left, right) ->
                    WorldModule.bind5 screen left right world
                | LinkDefinition (left, right) ->
                    let world = WorldModule.bind5 screen left right world
                    WorldModule.bind5 right.This right left world)
                world initializers

        override this.Actualize (screen, world) =
            let view = this.View (this.GetModel screen world, screen, world)
            World.actualizeView view world

        override this.TrySignal (signalObj, screen, world) =
            match signalObj with
            | :? Signal<'message, obj> as signal -> screen.Signal<'model, 'message, 'command> (match signal with Message message -> msg message | _ -> failwithumf ()) world
            | :? Signal<obj, 'command> as signal -> screen.Signal<'model, 'message, 'command> (match signal with Command command -> cmd command | _ -> failwithumf ()) world
            | _ -> Log.info "Incorrect signal type returned from event binding."; world

        abstract member Prepare : 'model * World -> 'model
        default this.Prepare (model, _) = model

        abstract member Channel : Lens<'model, World> * Screen -> Channel<'message, 'command, Screen, World> list
        default this.Channel (_, _) = []

        abstract member Initializers : Lens<'model, World> * Screen -> PropertyInitializer list
        default this.Initializers (_, _) = []

        abstract member Message : 'model * 'message * Screen * World -> Signal<'message, 'command> list * 'model
        default this.Message (model, _, _, _) = just model

        abstract member Command : 'model * 'command * Screen * World -> Signal<'message, 'command> list * World
        default this.Command (_, _, _, world) = just world

        abstract member Content : Lens<'model, World> * Screen -> GroupContent list
        default this.Content (_, _) = []

        abstract member View : 'model * Screen * World -> View
        default this.View (_, _, _) = View.empty