// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open Nito.Collections
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.Declarative

[<AutoOpen>]
module FacetModule =

    type Entity with
    
        member this.GetModel<'model> world = this.Get<'model> Property? Model world
        member this.SetModel<'model> value world = this.Set<'model> Property? Model value world
        member this.Model<'model> () = Lens.make<'model, World> Property? Model this.GetModel<'model> this.SetModel<'model> this
        member this.GetDescriptors world = this.Get Property? Descriptors world
        member this.SetDescriptors value world = this.Set Property? Descriptors value world
        member this.Descriptors () = Lens.make<EntityDescriptor list, World> Property? Descriptors this.GetDescriptors this.SetDescriptors this

    type [<AbstractClass>] Facet<'model, 'message, 'command> (initial : 'model) =
        inherit Facet ()

        member this.GetModel (entity : Entity) world : 'model =
            entity.GetModel<'model> world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetModel<'model> model world

        member this.Model (entity : Entity) =
            Lens.make Property? Model (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let (model, world) =
                match entity.TryGetProperty Property? Model world with
                | Some model -> (model :> obj :?> 'model, world)
                | None ->
                    let property = { PropertyType = typeof<'model>; PropertyValue = initial }
                    let world = World.attachEntityProperty Property? Model true false property entity world
                    (initial, world)
            let bindings = this.Bindings (model, entity, world)
            let world =
                List.fold (fun world binding ->
                    match binding with
                    | Message binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message ->
                                let (model, commands) = this.Message (message, this.GetModel entity world, entity, world)
                                let world = this.SetModel model entity world
                                List.fold (fun world command ->
                                    this.Command (command, this.GetModel entity world, entity, world))
                                    world commands
                            | None -> world)
                            entity binding.Stream world
                    | Command binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message -> this.Command (message, this.GetModel entity world, entity, world)
                            | None -> world)
                            entity binding.Stream world)
                    world bindings
            let content = this.Content (this.Model entity, entity, world)
            List.fold (fun world content -> World.expandEntityContent None content (Some entity) (etol entity) world) world content

        override this.Actualize (entity, world) =
            let views = this.View (this.GetModel entity world, entity, world)
            List.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Effect effect -> effect world)
                world views

        abstract member Bindings : 'model * Entity * World -> Binding<'message, 'command, Entity, World> list
        default this.Bindings (_, _, _) = []

        abstract member Message : 'message * 'model * Entity * World -> 'model * 'command list
        default this.Message (_, model, _, _) = just model

        abstract member Command : 'command * 'model * Entity * World -> World
        default this.Command (_, _, _, world) = world

        abstract member Content : Lens<'model, World> * Entity * World -> EntityContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Entity * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module EffectFacetModule =

    type EffectTags =
        Map<string, Symbol * Effects.Slice list>

    type Entity with
    
        member this.GetSelfDestruct world : bool = this.Get Property? SelfDestruct world
        member this.SetSelfDestruct (value : bool) world = this.SetFast Property? SelfDestruct false false value world
        member this.SelfDestruct = Lens.make Property? SelfDestruct this.GetSelfDestruct this.SetSelfDestruct this
        member this.GetEffects world : Symbol AssetTag list = this.Get Property? Effects world
        member this.SetEffects (value : Symbol AssetTag list) world = this.SetFast Property? Effects true false value world
        member this.Effects = Lens.make Property? Effects this.GetEffects this.SetEffects this
        member this.GetEffectStartTimeOpt world : int64 option = this.Get Property? EffectStartTimeOpt world
        member this.SetEffectStartTimeOpt (value : int64 option) world = this.SetFast Property? EffectStartTimeOpt false false value world
        member this.EffectStartTimeOpt = Lens.make Property? EffectStartTimeOpt this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt this
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get Property? EffectDefinitions world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.SetFast Property? EffectDefinitions false false value world
        member this.EffectDefinitions = Lens.make Property? EffectDefinitions this.GetEffectDefinitions this.SetEffectDefinitions this
        member this.GetEffect world : Effect = this.Get Property? Effect world
        member this.SetEffect (value : Effect) world = this.SetFast Property? Effect false false value world
        member this.Effect = Lens.make Property? Effect this.GetEffect this.SetEffect this
        member this.GetEffectOffset world : Vector2 = this.Get Property? EffectOffset world
        member this.SetEffectOffset (value : Vector2) world = this.SetFast Property? EffectOffset false false value world
        member this.EffectOffset = Lens.make Property? EffectOffset this.GetEffectOffset this.SetEffectOffset this
        member this.GetEffectPhysicsShapes world : unit = this.Get Property? EffectPhysicsShapes world // NOTE: the default EffectFacet leaves it up to the Dispatcher to do something with the effect's physics output
        member private this.SetEffectPhysicsShapes (value : unit) world = this.SetFast Property? EffectPhysicsShapes false true value world
        member this.EffectPhysicsShapes = Lens.makeReadOnly Property? EffectPhysicsShapes this.GetEffectPhysicsShapes this
        member this.GetEffectTags world : EffectTags = this.Get Property? EffectTags world
        member private this.SetEffectTags (value : EffectTags) world = this.SetFast Property? EffectTags false true value world
        member this.EffectTags = Lens.makeReadOnly Property? EffectTags this.GetEffectTags this
        member this.GetEffectHistoryMax world : int = this.Get Property? EffectHistoryMax world
        member this.SetEffectHistoryMax (value : int) world = this.SetFast Property? EffectHistoryMax false false value world
        member this.EffectHistoryMax = Lens.make Property? EffectHistoryMax this.GetEffectHistoryMax this.SetEffectHistoryMax this
        // NOTE: this line of code horks autocomplete as discussed here - https://github.com/dotnet/fsharp/issues/7073 - member this.GetEffectHistory world : Effects.Slice Deque = this.Get Property? EffectHistory world
        member private this.SetEffectHistory (value : Effects.Slice Deque) world = this.SetFast Property? EffectHistory false true value world
        member this.EffectHistory = Lens.makeReadOnly Property? EffectHistory (this.Get Property? EffectHistory) this
        
        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetEffectStartTimeOpt world with
            | Some effectStartTime -> effectStartTime
            | None -> 0L

        /// The time relative to the start of the effect.
        member this.GetEffectTime world =
            let effectStartTime = this.GetEffectStartTime world
            let tickTime = World.getTickTime world
            tickTime - effectStartTime

    type EffectFacet () =
        inherit Facet ()

        static let setEffect effects (entity : Entity) world =
            match effects with
            | [] -> world
            | effectAssetTags ->
                let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
                let (effectOpts, world) = World.assetTagsToValueOpts<Effect> symbolLoadMetadata effectAssetTags world
                let effects = List.definitize effectOpts
                let effectCombined = EffectSystem.combineEffects effects
                entity.SetEffect effectCombined world

        static let handleEffectsChanged evt world =
            let entity = evt.Subscriber : Entity
            let effectsOpt = entity.GetEffects world
            setEffect effectsOpt entity world

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let effectsOpt = entity.GetEffects world
            setEffect effectsOpt entity world

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.Effects []
             define Entity.EffectStartTimeOpt None
             define Entity.EffectDefinitions Map.empty
             define Entity.Effect Effect.empty
             define Entity.EffectOffset (Vector2 0.5f)
             define Entity.EffectPhysicsShapes ()
             define Entity.EffectTags Map.empty
             define Entity.EffectHistoryMax Constants.Effects.DefaultEffectHistoryMax
             variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.DefaultEffectHistoryMax))]

        override facet.Actualize (entity, world) =
            
            // evaluate effect if visible
            if entity.GetVisibleLayered world && entity.GetInView world then

                // set up effect system to evaluate effect
                let world = entity.SetEffectTags Map.empty world
                let effect = entity.GetEffect world
                let effectTime = entity.GetEffectTime world
                let effectViewType = entity.GetViewType world
                let effectSlice =
                    { Effects.Position = entity.GetPosition world + Vector2.Multiply (entity.GetSize world, entity.GetEffectOffset world)
                      Effects.Size = entity.GetSize world
                      Effects.Rotation = entity.GetRotation world
                      Effects.Depth = entity.GetDepthLayered world
                      Effects.Offset = Vector2 0.5f
                      Effects.Color = Vector4.One
                      Effects.Enabled = true
                      Effects.Volume = 1.0f }
                let effectHistory = entity.EffectHistory.Get world
                let effectEnv = entity.GetEffectDefinitions world
                let effectSystem = EffectSystem.make effectViewType effectHistory effectTime effectEnv

                // eval effect and process resulting artifacts
                let world =
                    
                    // evaluate effect with effect system
                    let (artifacts, _) = EffectSystem.eval effect effectSlice effectSystem

                    // pass a single render message for efficiency
                    let renderMessage = artifacts.RenderArtifacts |> Seq.toArray |> Array.map (function Effects.RenderArtifact descriptor -> descriptor) |> RenderDescriptorsMessage 
                    let world = World.enqueueRenderMessage renderMessage world

                    // pass sound messages
                    let world = Seq.fold (fun world (Effects.SoundArtifact (volume, sound)) -> World.playSound volume sound world) world artifacts.SoundArtifacts
                    
                    // set effects tags all in one pass for efficiency
                    // TODO: also raise event for all new effect tags so they can be handled in scripts?
                    let effectTags = entity.GetEffectTags world
                    let (effectTags, world) =
                        Seq.fold (fun (effectTags, world) (Effects.TagArtifact (name, metadata, slice)) ->
                            match Map.tryFind name effectTags with
                            | Some (metadata, slices) -> (Map.add name (metadata, slice :: slices) effectTags, world)
                            | None -> (Map.add name (metadata, [slice]) effectTags, world))
                            (effectTags, world)
                            artifacts.TagArtifacts
                    entity.SetEffectTags effectTags world

                // update effect history in-place
                effectHistory.AddToFront effectSlice
                if effectHistory.Count > entity.GetEffectHistoryMax world then effectHistory.RemoveFromBack () |> ignore
                world

            // no need to evaluate non-visible effect
            else world

        override facet.Update (entity, world) =
            let effect = entity.GetEffect world
            match (entity.GetSelfDestruct world, effect.LifetimeOpt) with
            | (true, Some lifetime) -> if entity.GetEffectTime world > lifetime then World.destroyEntity entity world else world
            | (_, _) -> world

        override facet.Register (entity, world) =
            let effectStartTime = Option.getOrDefault (World.getTickTime world) (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.monitor handleEffectsChanged (entity.GetChangeEvent Property? Effects) entity world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module ScriptFacetModule =

    type Entity with
    
        member this.GetScriptOpt world : Symbol AssetTag option = this.Get Property? ScriptOpt world
        member this.SetScriptOpt (value : Symbol AssetTag option) world = this.SetFast Property? ScriptOpt true false value world
        member this.ScriptOpt = Lens.make Property? ScriptOpt this.GetScriptOpt this.SetScriptOpt this
        member this.GetScript world : Scripting.Expr array = this.Get Property? Script world
        member this.SetScript (value : Scripting.Expr array) world = this.SetFast Property? Script true false value world
        member this.Script = Lens.make Property? Script this.GetScript this.SetScript this
        member this.GetScriptFrame world : Scripting.DeclarationFrame = this.Get Property? ScriptFrame world
        member internal this.SetScriptFrame (value : Scripting.DeclarationFrame) world = this.SetFast Property? ScriptFrame false true value world
        member this.ScriptFrame = Lens.makeReadOnly Property? ScriptFrame this.GetScriptFrame this
        member internal this.GetScriptUnsubscriptions world : Unsubscription list = this.Get Property? ScriptUnsubscriptions world
        member internal this.SetScriptUnsubscriptions (value : Unsubscription list) world = this.SetFast Property? ScriptUnsubscriptions false true value world
        member internal this.ScriptUnsubscriptions = Lens.make Property? ScriptUnsubscriptions this.GetScriptUnsubscriptions this.SetScriptUnsubscriptions this
        member this.GetOnRegister world : Scripting.Expr = this.Get Property? OnRegister world
        member this.SetOnRegister (value : Scripting.Expr) world = this.SetFast Property? OnRegister true false value world
        member this.OnRegister = Lens.make Property? OnRegister this.GetOnRegister this.SetOnRegister this
        member this.GetOnUnregister world : Scripting.Expr = this.Get Property? OnUnregister world
        member this.SetOnUnregister (value : Scripting.Expr) world = this.SetFast Property? OnUnregister false false value world
        member this.OnUnregister = Lens.make Property? OnUnregister this.GetOnUnregister this.SetOnUnregister this
        member this.GetOnUpdate world : Scripting.Expr = this.Get Property? OnUpdate world
        member this.SetOnUpdate (value : Scripting.Expr) world = this.SetFast Property? OnUpdate false false value world
        member this.OnUpdate = Lens.make Property? OnUpdate this.GetOnUpdate this.SetOnUpdate this
        member this.GetOnPostUpdate world : Scripting.Expr = this.Get Property? OnPostUpdate world
        member this.SetOnPostUpdate (value : Scripting.Expr) world = this.SetFast Property? OnPostUpdate false false value world
        member this.OnPostUpdate = Lens.make Property? OnPostUpdate this.GetOnPostUpdate this.SetOnPostUpdate this
        member this.GetOnSignal world : Scripting.Expr = this.Get Property? OnSignal world
        member this.SetOnSignal (value : Scripting.Expr) world = this.SetFast Property? OnSignal false false value world
        member this.OnSignal = Lens.make Property? OnSignal this.GetOnSignal this.SetOnSignal this
        member this.Signal signal world = World.signalEntity signal this world
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.MessageEvent = Events.Signal --> this

    type ScriptFacet () =
        inherit Facet ()

        static let handleScriptChanged evt world =
            let entity = evt.Subscriber : Entity
            let script = entity.GetScript world
            let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
            let world = entity.SetScriptFrame scriptFrame world
            evalManyWithLogging script scriptFrame entity world |> snd'

        static let handleOnRegisterChanged evt world =
            let entity = evt.Subscriber : Entity
            let world = World.unregisterEntity entity world
            World.registerEntity entity world

        static member Properties =
            [define Entity.PublishChanges true
             define Entity.ScriptOpt None
             define Entity.Script [||]
             define Entity.ScriptFrame (Scripting.DeclarationFrame HashIdentity.Structural)
             define Entity.ScriptUnsubscriptions []
             define Entity.OnRegister Scripting.Unit
             define Entity.OnUnregister Scripting.Unit
             define Entity.OnUpdate Scripting.Unit
             define Entity.OnPostUpdate Scripting.Unit
             define Entity.OnSignal Scripting.Unit]

        override facet.Register (entity, world) =
            let world = World.evalWithLogging (entity.GetOnRegister world) (entity.GetScriptFrame world) entity world |> snd'
            let world = World.monitor handleScriptChanged (entity.GetChangeEvent Property? Script) entity world
            let world = World.monitor handleOnRegisterChanged (entity.GetChangeEvent Property? OnRegister) entity world
            world

        override facet.Unregister (entity, world) =
            World.evalWithLogging (entity.GetOnUnregister world) (entity.GetScriptFrame world) entity world |> snd'

        override facet.Update (entity, world) =
            World.evalWithLogging (entity.GetOnUpdate world) (entity.GetScriptFrame world) entity world |> snd'

        override facet.PostUpdate (entity, world) =
            World.evalWithLogging (entity.GetOnPostUpdate world) (entity.GetScriptFrame world) entity world |> snd'

        override facet.Signal (signal, entity, world) =
            match ScriptingSystem.tryImport typeof<Symbol> signal world with
            | Some signalExpr ->
                ScriptingSystem.addProceduralBindings (Scripting.AddToNewFrame 1) (seq { yield struct ("signal", signalExpr) }) world
                let world = World.evalWithLogging (entity.GetOnSignal world) (entity.GetScriptFrame world) entity world |> snd'
                ScriptingSystem.removeProceduralBindings world
                world
            | None -> failwithumf ()

[<AutoOpen>]
module TextFacetModule =

    type Entity with
    
        member this.GetText world : string = this.Get Property? Text world
        member this.SetText (value : string) world = this.SetFast Property? Text false false value world
        member this.Text = Lens.make Property? Text this.GetText this.SetText this
        member this.GetFont world : Font AssetTag = this.Get Property? Font world
        member this.SetFont (value : Font AssetTag) world = this.SetFast Property? Font false false value world
        member this.Font = Lens.make Property? Font this.GetFont this.SetFont this
        member this.GetMargins world : Vector2 = this.Get Property? Margins world
        member this.SetMargins (value : Vector2) world = this.SetFast Property? Margins false false value world
        member this.Margins = Lens.make Property? Margins this.GetMargins this.SetMargins this
        member this.GetJustification world : Justification = this.Get Property? Justification world
        member this.SetJustification (value : Justification) world = this.SetFast Property? Justification false false value world
        member this.Justification = Lens.make Property? Justification this.GetJustification this.SetJustification this
        member this.GetColor world : Vector4 = this.Get Property? Color world
        member this.SetColor (value : Vector4) world = this.SetFast Property? Color false false value world
        member this.Color = Lens.make Property? Color this.GetColor this.SetColor this

    type TextFacet () =
        inherit Facet ()

        static member Properties =
            [define Entity.Text ""
             define Entity.Font (AssetTag.make<Font> Assets.DefaultPackage "Font")
             define Entity.Margins Vector2.Zero
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.Color (Vector4 (0.0f, 0.0f, 0.0f, 1.0f))]

        override facet.Actualize (text, world) =
            let textStr = text.GetText world
            if text.GetVisibleLayered world && not (String.IsNullOrWhiteSpace textStr) then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = text.GetDepthLayered world
                              PositionY = (text.GetPosition world).Y
                              LayeredDescriptor =
                                TextDescriptor
                                    { Text = textStr
                                      Position = text.GetPosition world + text.GetMargins world
                                      Size = text.GetSize world - text.GetMargins world * 2.0f
                                      ViewType = Absolute
                                      Font = text.GetFont world
                                      Color = text.GetColor world
                                      Justification = text.GetJustification world }}|])
                    world
            else world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with

        member this.GetMinorId world : Guid = this.Get Property? MinorId world
        member this.SetMinorId (value : Guid) world = this.SetFast Property? MinorId false false value world
        member this.MinorId = Lens.make Property? MinorId this.GetMinorId this.SetMinorId this
        member this.GetBodyType world : BodyType = this.Get Property? BodyType world
        member this.SetBodyType (value : BodyType) world = this.SetFast Property? BodyType false false value world
        member this.BodyType = Lens.make Property? BodyType this.GetBodyType this.SetBodyType this
        member this.GetAwake world : bool = this.Get Property? Awake world
        member this.SetAwake (value : bool) world = this.SetFast Property? Awake false false value world
        member this.Awake = Lens.make Property? Awake this.GetAwake this.SetAwake this
        member this.GetDensity world : single = this.Get Property? Density world
        member this.SetDensity (value : single) world = this.SetFast Property? Density false false value world
        member this.Density = Lens.make Property? Density this.GetDensity this.SetDensity this
        member this.GetFriction world : single = this.Get Property? Friction world
        member this.SetFriction (value : single) world = this.SetFast Property? Friction false false value world
        member this.Friction = Lens.make Property? Friction this.GetFriction this.SetFriction this
        member this.GetRestitution world : single = this.Get Property? Restitution world
        member this.SetRestitution (value : single) world = this.SetFast Property? Restitution false false value world
        member this.Restitution = Lens.make Property? Restitution this.GetRestitution this.SetRestitution this
        member this.GetFixedRotation world : bool = this.Get Property? FixedRotation world
        member this.SetFixedRotation (value : bool) world = this.SetFast Property? FixedRotation false false value world
        member this.FixedRotation = Lens.make Property? FixedRotation this.GetFixedRotation this.SetFixedRotation this
        member this.GetAngularVelocity world : single = this.Get Property? AngularVelocity world
        member this.SetAngularVelocity (value : single) world = this.SetFast Property? AngularVelocity false false value world
        member this.AngularVelocity = Lens.make Property? AngularVelocity this.GetAngularVelocity this.SetAngularVelocity this
        member this.GetAngularDamping world : single = this.Get Property? AngularDamping world
        member this.SetAngularDamping (value : single) world = this.SetFast Property? AngularDamping false false value world
        member this.AngularDamping = Lens.make Property? AngularDamping this.GetAngularDamping this.SetAngularDamping this
        member this.GetLinearVelocity world : Vector2 = this.Get Property? LinearVelocity world
        member this.SetLinearVelocity (value : Vector2) world = this.SetFast Property? LinearVelocity false false value world
        member this.LinearVelocity = Lens.make Property? LinearVelocity this.GetLinearVelocity this.SetLinearVelocity this
        member this.GetLinearDamping world : single = this.Get Property? LinearDamping world
        member this.SetLinearDamping (value : single) world = this.SetFast Property? LinearDamping false false value world
        member this.LinearDamping = Lens.make Property? LinearDamping this.GetLinearDamping this.SetLinearDamping this
        member this.GetGravityScale world : single = this.Get Property? GravityScale world
        member this.SetGravityScale (value : single) world = this.SetFast Property? GravityScale false false value world
        member this.GravityScale = Lens.make Property? GravityScale this.GetGravityScale this.SetGravityScale this
        member this.GetCollisionCategories world : string = this.Get Property? CollisionCategories world
        member this.SetCollisionCategories (value : string) world = this.SetFast Property? CollisionCategories false false value world
        member this.CollisionCategories = Lens.make Property? CollisionCategories this.GetCollisionCategories this.SetCollisionCategories this
        member this.GetCollisionMask world : string = this.Get Property? CollisionMask world
        member this.SetCollisionMask (value : string) world = this.SetFast Property? CollisionMask false false value world
        member this.CollisionMask = Lens.make Property? CollisionMask this.GetCollisionMask this.SetCollisionMask this
        member this.GetCollisionBody world : BodyShape = this.Get Property? CollisionBody world
        member this.SetCollisionBody (value : BodyShape) world = this.SetFast Property? CollisionBody false false value world
        member this.CollisionBody = Lens.make Property? CollisionBody this.GetCollisionBody this.SetCollisionBody this
        member this.GetIsBullet world : bool = this.Get Property? IsBullet world
        member this.SetIsBullet (value : bool) world = this.SetFast Property? IsBullet false false value world
        member this.IsBullet = Lens.make Property? IsBullet this.GetIsBullet this.SetIsBullet this
        member this.GetIsSensor world : bool = this.Get Property? IsSensor world
        member this.SetIsSensor (value : bool) world = this.SetFast Property? IsSensor false false value world
        member this.IsSensor = Lens.make Property? IsSensor this.GetIsSensor this.SetIsSensor this
        member this.GetPhysicsId world = { SourceId = this.GetId world; BodyId = this.GetMinorId world }
        member this.PhysicsId = Lens.makeReadOnly Property? PhysicsId this.GetPhysicsId this
        member this.CollisionEvent = Events.Collision --> this

    type RigidBodyFacet () =
        inherit Facet ()

        static let getBodyShape (entity : Entity) world =
            PhysicsEngine.localizeCollisionBody (entity.GetSize world) (entity.GetCollisionBody world)

        static member Properties =
            [Variable? MinorId (fun _ -> makeGuid ())
             define Entity.BodyType Dynamic
             define Entity.Awake true
             define Entity.Density Constants.Physics.NormalDensity
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.FixedRotation false
             define Entity.AngularVelocity 0.0f
             define Entity.AngularDamping 0.0f
             define Entity.LinearVelocity Vector2.Zero
             define Entity.LinearDamping 0.0f
             define Entity.GravityScale 1.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.CollisionBody (BodyBox { Extent = Vector2 0.5f; Center = Vector2.Zero })
             define Entity.IsBullet false
             define Entity.IsSensor false]

        override facet.RegisterPhysics (entity, world) =
            let bodyProperties = 
                { BodyId = (entity.GetPhysicsId world).BodyId
                  Position = entity.GetPosition world + entity.GetSize world * 0.5f
                  Rotation = entity.GetRotation world
                  Shape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  Awake = entity.GetAwake world
                  Enabled = entity.GetEnabled world
                  Density = entity.GetDensity world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  FixedRotation = entity.GetFixedRotation world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  GravityScale = entity.GetGravityScale world
                  CollisionCategories = PhysicsEngine.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = PhysicsEngine.categorizeCollisionMask (entity.GetCollisionMask world)
                  IsBullet = entity.GetIsBullet world
                  IsSensor = entity.GetIsSensor world }
            World.createBody entity (entity.GetId world) bodyProperties world

        override facet.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

        override facet.PropagatePhysics (entity, world) =
            let world = facet.UnregisterPhysics (entity, world)
            facet.RegisterPhysics (entity, world)

        override facet.TryGetCalculatedProperty (name, entity, world) =
            match name with
            | "PhysicsId" -> Some { PropertyType = typeof<PhysicsId>; PropertyValue = entity.GetPhysicsId world }
            | _ -> None

[<AutoOpen>]
module NodeFacetModule =

    type Entity with
    
        member this.GetParentNodeOpt world : Entity Relation option = this.Get Property? ParentNodeOpt world
        member this.SetParentNodeOpt (value : Entity Relation option) world = this.SetFast Property? ParentNodeOpt false false value world
        member this.ParentNodeOpt = Lens.make Property? ParentNodeOpt this.GetParentNodeOpt this.SetParentNodeOpt this
        member this.GetPositionLocal world : Vector2 = this.Get Property? PositionLocal world
        member this.SetPositionLocal (value : Vector2) world = this.SetFast Property? PositionLocal false false value world
        member this.PositionLocal = Lens.make Property? PositionLocal this.GetPositionLocal this.SetPositionLocal this
        member this.GetDepthLocal world : single = this.Get Property? DepthLocal world
        member this.SetDepthLocal (value : single) world = this.SetFast Property? DepthLocal false false value world
        member this.DepthLocal = Lens.make Property? DepthLocal this.GetDepthLocal this.SetDepthLocal this
        member this.GetVisibleLocal world : bool = this.Get Property? VisibleLocal world
        member this.SetVisibleLocal (value : bool) world = this.SetFast Property? VisibleLocal false false value world
        member this.VisibleLocal = Lens.make Property? VisibleLocal this.GetVisibleLocal this.SetVisibleLocal this
        member this.GetEnabledLocal world : bool = this.Get Property? EnabledLocal world
        member this.SetEnabledLocal (value : bool) world = this.SetFast Property? EnabledLocal false false value world
        member this.EnabledLocal = Lens.make Property? EnabledLocal this.GetEnabledLocal this.SetEnabledLocal this
        member private this.GetNodeUnsubscribe world : World -> World = this.Get Property? NodeUnsubscribe world
        member private this.SetNodeUnsubscribe (value : World -> World) world = this.SetFast Property? NodeUnsubscribe false true value world
        member private this.NodeUnsubscribe = Lens.make Property? NodeUnsubscribe this.GetNodeUnsubscribe this.SetNodeUnsubscribe this
        
        member this.SetParentNodeOptWithAdjustment (value : Entity Relation option) world =
            let world =
                match (this.GetParentNodeOpt world, value) with
                | (Some relationOld, Some relationNew) ->
                    let parentOld = this.Resolve relationOld
                    let parentNew = this.Resolve relationNew
                    if parentOld.GetExists world && parentNew.GetExists world then
                        let position = this.GetPositionLocal world + parentNew.GetPosition world
                        let depth = this.GetDepthLocal world + parentNew.GetDepth world
                        let world = this.SetPosition position world
                        let world = this.SetDepth depth world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (Some relationOld, None) ->
                    let parentOld = this.Resolve relationOld
                    if parentOld.GetExists world then
                        let position = this.GetPositionLocal world + parentOld.GetPosition world
                        let depth = this.GetDepthLocal world + parentOld.GetDepth world
                        let world = this.SetPosition position world
                        let world = this.SetDepth depth world
                        let world = this.SetVisible (this.GetVisibleLocal world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world) world
                        world
                    else world
                | (None, Some relationNew) ->
                    let parentNew = this.Resolve relationNew
                    if parentNew.GetExists world then
                        let position = this.GetPosition world - parentNew.GetPosition world
                        let depth = this.GetDepth world - parentNew.GetDepth world
                        let world = this.SetPositionLocal position world
                        let world = this.SetDepthLocal depth world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (None, None) -> world
            this.SetParentNodeOpt value world
        
        member this.GetChildNodes world =
            this.GetChildNodes2 [] world

        member this.ParentNodeExists world =
            match this.GetParentNodeOpt world with
            | Some relation -> (this.Resolve relation).GetExists world
            | None -> false

        member private this.GetChildNodes2 nodes world =
            let nodeOpt =
                if this.FacetedAs<NodeFacet> world
                then Option.map this.Resolve (this.GetParentNodeOpt world)
                else None
            match nodeOpt with
            | Some node -> node.GetChildNodes2 (node :: nodes) world
            | None -> nodes

    and NodeFacet () =
        inherit Facet ()

        static let updatePropertyFromLocal3 propertyName (entity : Entity) world =
            match propertyName with
            | "Position" -> entity.SetPosition (entity.GetPositionLocal world) world
            | "Depth" -> entity.SetDepth (entity.GetDepthLocal world) world
            | "Visible" -> entity.SetVisible (entity.GetVisibleLocal world) world
            | "Enabled" -> entity.SetEnabled (entity.GetEnabledLocal world) world
            | _ -> world

        static let updatePropertyFromLocal propertyName (node : Entity) (entity : Entity) world =
            match propertyName with
            | "PositionLocal" -> entity.SetPosition (node.GetPosition world + entity.GetPositionLocal world) world
            | "DepthLocal" -> entity.SetDepth (node.GetDepth world + entity.GetDepthLocal world) world
            | "VisibleLocal" -> entity.SetVisible (node.GetVisible world && entity.GetVisibleLocal world) world
            | "EnabledLocal" -> entity.SetEnabled (node.GetEnabled world && entity.GetEnabledLocal world) world
            | _ -> world

        static let updatePropertyFromNode propertyName (node : Entity) (entity : Entity) world =
            match propertyName with
            | "Position" -> entity.SetPosition (node.GetPosition world + entity.GetPositionLocal world) world
            | "Depth" -> entity.SetDepth (node.GetDepth world + entity.GetDepthLocal world) world
            | "Visible" -> entity.SetVisible (node.GetVisible world && entity.GetVisibleLocal world) world
            | "Enabled" -> entity.SetEnabled (node.GetEnabled world && entity.GetEnabledLocal world) world
            | _ -> world

        static let handleLocalPropertyChange evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : ChangeData
            match entity.GetParentNodeOpt world with
            | Some relation ->
                let node = entity.Resolve relation
                if World.getEntityExists node world
                then (Cascade, updatePropertyFromLocal data.PropertyName node entity world)
                else (Cascade, updatePropertyFromLocal3 data.PropertyName entity world)
            | None -> (Cascade, updatePropertyFromLocal3 data.PropertyName entity world)

        static let handleNodePropertyChange evt world =
            let entity = evt.Subscriber : Entity
            let node = evt.Publisher :?> Entity
            let data = evt.Data : ChangeData
            (Cascade, updatePropertyFromNode data.PropertyName node entity world)

        static let subscribeToNodePropertyChanges (entity : Entity) world =
            let oldWorld = world
            let world = (entity.GetNodeUnsubscribe world) world
            match entity.GetParentNodeOpt world with
            | Some nodeRelation ->
                let node = entity.Resolve nodeRelation
                if node = entity then
                    Log.trace "Cannot mount entity to itself."
                    World.choose oldWorld
                elif entity.FacetedAs<RigidBodyFacet> world then
                    Log.trace "Cannot mount a rigid body entity onto another entity. Instead, consider using physics constraints."
                    World.choose oldWorld
                else
                    let (unsubscribe, world) = World.monitorPlus handleNodePropertyChange node.Position.ChangeEvent entity world
                    let (unsubscribe2, world) = World.monitorPlus handleNodePropertyChange node.Depth.ChangeEvent entity world
                    let (unsubscribe3, world) = World.monitorPlus handleNodePropertyChange node.Visible.ChangeEvent entity world
                    let (unsubscribe4, world) = World.monitorPlus handleNodePropertyChange node.Enabled.ChangeEvent entity world
                    entity.SetNodeUnsubscribe (unsubscribe4 >> unsubscribe3 >> unsubscribe2 >> unsubscribe) world
            | None -> world

        static let handleNodeChange evt world =
            subscribeToNodePropertyChanges evt.Subscriber world

        static member Properties =
            [define Entity.ParentNodeOpt None
             define Entity.PositionLocal Vector2.Zero
             define Entity.DepthLocal 0.0f
             define Entity.VisibleLocal true
             define Entity.EnabledLocal true
             define Entity.NodeUnsubscribe (id : World -> World)]

        override facet.Register (entity, world) =
            let world = entity.SetNodeUnsubscribe id world // ensure unsubscribe function reference doesn't get copied in Gaia...
            let world = World.monitor handleNodeChange entity.ParentNodeOpt.ChangeEvent entity world
            let world = World.monitorPlus handleLocalPropertyChange entity.PositionLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.DepthLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.VisibleLocal.ChangeEvent entity world |> snd
            let world = World.monitorPlus handleLocalPropertyChange entity.EnabledLocal.ChangeEvent entity world |> snd
            let world = subscribeToNodePropertyChanges entity world
            world

        override facet.Unregister (entity, world) =
            (entity.GetNodeUnsubscribe world) world // NOTE: not sure if this is necessary.

        override facet.TryGetCalculatedProperty (propertyName, entity, world) =
            match propertyName with
            | "ParentNodeExists" -> Some { PropertyType = typeof<bool>; PropertyValue = entity.ParentNodeExists world }
            | _ -> None

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with

        member this.GetStaticImage world : Image AssetTag = this.Get Property? StaticImage world
        member this.SetStaticImage (value : Image AssetTag) world = this.SetFast Property? StaticImage false false value world
        member this.StaticImage = Lens.make Property? StaticImage this.GetStaticImage this.SetStaticImage this

    type StaticSpriteFacet () =
        inherit Facet ()

        static member Properties =
            [define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackage "Image4")]

        override facet.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepthLayered world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = None
                                      Image = entity.GetStaticImage world
                                      Color = Vector4.One }}|])
                    world
            else world

        override facet.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
    
        member this.GetCelSize world : Vector2 = this.Get Property? CelSize world
        member this.SetCelSize (value : Vector2) world = this.SetFast Property? CelSize false false value world
        member this.CelSize = Lens.make Property? CelSize this.GetCelSize this.SetCelSize this
        member this.GetCelRun world : int = this.Get Property? CelRun world
        member this.SetCelRun (value : int) world = this.SetFast Property? CelRun false false value world
        member this.CelRun = Lens.make Property? CelRun this.GetCelRun this.SetCelRun this
        member this.GetCelCount world : int = this.Get Property? CelCount world
        member this.SetCelCount (value : int) world = this.SetFast Property? CelCount false false value world
        member this.CelCount = Lens.make Property? CelCount this.GetCelCount this.SetCelCount this
        member this.GetAnimationDelay world : int64 = this.Get Property? AnimationDelay world
        member this.SetAnimationDelay (value : int64) world = this.SetFast Property? AnimationDelay false false value world
        member this.AnimationDelay = Lens.make Property? AnimationDelay this.GetAnimationDelay this.SetAnimationDelay this
        member this.GetAnimationSheet world : Image AssetTag = this.Get Property? AnimationSheet world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.SetFast Property? AnimationSheet false false value world
        member this.AnimationSheet = Lens.make Property? AnimationSheet this.GetAnimationSheet this.SetAnimationSheet this

    type AnimatedSpriteFacet () =
        inherit Facet ()

        static let getSpriteInsetOpt (entity : Entity) world =
            let celCount = entity.GetCelCount world
            let celRun = entity.GetCelRun world
            if celCount <> 0 && celRun <> 0 then
                let cel = int (World.getTickTime world / entity.GetAnimationDelay world) % celCount
                let celSize = entity.GetCelSize world
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = single celI * celSize.X
                let celY = single celJ * celSize.Y
                let inset = Vector4 (celX, celY, celX + celSize.X, celY + celSize.Y)
                Some inset
            else None

        static member Properties =
            [define Entity.CelCount 16 
             define Entity.CelSize (Vector2 (16.0f, 16.0f))
             define Entity.CelRun 4
             define Entity.AnimationDelay 4L
             define Entity.AnimationSheet (AssetTag.make<Image> Assets.DefaultPackage "Image7")]

        override facet.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepthLayered world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = getSpriteInsetOpt entity world
                                      Image = entity.GetAnimationSheet world
                                      Color = Vector4.One }}|])
                    world
            else world

        override facet.GetQuickSize (entity, world) =
            entity.GetCelSize world

[<AutoOpen>]
module EntityDispatcherModule =

    type [<AbstractClass>] EntityDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit EntityDispatcher ()

        member this.GetModel (entity : Entity) world : 'model =
            entity.GetModel<'model> world

        member this.SetModel (model : 'model) (entity : Entity) world =
            entity.SetModel<'model> model world

        member this.Model (entity : Entity) =
            Lens.make Property? Model (this.GetModel entity) (flip this.SetModel entity) entity

        override this.Register (entity, world) =
            let (model, world) =
                match entity.TryGetProperty Property? Model world with
                | Some model -> (model :> obj :?> 'model, world)
                | None ->
                    let property = { PropertyType = typeof<'model>; PropertyValue = initial }
                    let world = World.attachEntityProperty Property? Model true false property entity world
                    (initial, world)
            let bindings = this.Bindings (model, entity, world)
            let world =
                List.fold (fun world binding ->
                    match binding with
                    | Message binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message ->
                                let (model, commands) = this.Message (message, this.GetModel entity world, entity, world)
                                let world = this.SetModel model entity world
                                List.fold (fun world command ->
                                    this.Command (command, this.GetModel entity world, entity, world))
                                    world commands
                            | None -> world)
                            entity binding.Stream world
                    | Command binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message -> this.Command (message, this.GetModel entity world, entity, world)
                            | None -> world)
                            entity binding.Stream world)
                    world bindings
            let content = this.Content (this.Model entity, entity, world)
            List.fold (fun world content -> World.expandEntityContent None content (Some entity) (etol entity) world) world content

        override this.Actualize (entity, world) =
            let views = this.View (this.GetModel entity world, entity, world)
            List.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Effect effect -> effect world)
                world views

        abstract member Bindings : 'model * Entity * World -> Binding<'message, 'command, Entity, World> list
        default this.Bindings (_, _, _) = []
        
        abstract member Message : 'message * 'model * Entity * World -> 'model * 'command list
        default this.Message (_, model, _, _) = just model
        
        abstract member Command : 'command * 'model * Entity * World -> World
        default this.Command (_, _, _, world) = world

        abstract member Content : Lens<'model, World> * Entity * World -> EntityContent list
        default this.Content (_, _, _) = []
        
        abstract member View : 'model * Entity * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module EffectDispatcherModule =

    type EffectDispatcher () =
        inherit EntityDispatcher ()

        static member FacetNames =
            [typeof<EffectFacet>.Name]

        static member Properties =
            [define Entity.Effect (scvalue<Effect> "[Effect None [] [Composite [Shift 0] [[StaticSprite [Resource Default Image] [] Nil]]]]")]

[<AutoOpen>]
module NodeDispatcherModule =

    type NodeDispatcher () =
        inherit EntityDispatcher ()

        static member FacetNames =
            [typeof<NodeFacet>.Name]

    type [<AbstractClass>] NodeDispatcher<'model, 'message, 'command> (model) =
        inherit EntityDispatcher<'model, 'message, 'command> (model)

        static member FacetNames =
            [typeof<NodeFacet>.Name]

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
    
        member this.GetDisabledColor world : Vector4 = this.Get Property? DisabledColor world
        member this.SetDisabledColor (value : Vector4) world = this.SetFast Property? DisabledColor false false value world
        member this.DisabledColor = Lens.make Property? DisabledColor this.GetDisabledColor this.SetDisabledColor this
        member this.GetSwallowMouseLeft world : bool = this.Get Property? SwallowMouseLeft world
        member this.SetSwallowMouseLeft (value : bool) world = this.SetFast Property? SwallowMouseLeft false false value world
        member this.SwallowMouseLeft = Lens.make Property? SwallowMouseLeft this.GetSwallowMouseLeft this.SetSwallowMouseLeft this

    type GuiDispatcher () =
        inherit EntityDispatcher ()

        static let handleMouseLeft evt world =
            let gui = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let handling =
                if gui.GetSelected world && gui.GetVisibleLayered world then
                    let mousePositionWorld = World.mouseToWorld (gui.GetViewType world) data.Position world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds mousePositionWorld (gui.GetBounds world) then
                       Resolve
                    else Cascade
                else Cascade
            (handling, world)

        static member FacetNames =
            [typeof<NodeFacet>.Name
             typeof<ScriptFacet>.Name]

        static member Properties =
            [define Entity.ViewType Absolute
             define Entity.AlwaysUpdate true
             define Entity.PublishChanges true
             define Entity.DisabledColor (Vector4 0.75f)
             define Entity.SwallowMouseLeft true]

        override dispatcher.Register (gui, world) =
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftDown gui world |> snd
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftUp gui world |> snd
            world

    type [<AbstractClass>] GuiDispatcher<'model, 'message, 'command> (model) =
        inherit EntityDispatcher<'model, 'message, 'command> (model)

        static let handleMouseLeft evt world =
            let gui = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let handling =
                if gui.GetSelected world && gui.GetVisibleLayered world then
                    let mousePositionWorld = World.mouseToWorld (gui.GetViewType world) data.Position world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds mousePositionWorld (gui.GetBounds world) then
                       Resolve
                    else Cascade
                else Cascade
            (handling, world)

        static member FacetNames =
            [typeof<NodeFacet>.Name
             typeof<ScriptFacet>.Name]

        static member Properties =
            [define Entity.ViewType Absolute
             define Entity.AlwaysUpdate true
             define Entity.PublishChanges true
             define Entity.DisabledColor (Vector4 0.75f)
             define Entity.SwallowMouseLeft true]

        override dispatcher.Register (gui, world) =
            let world = base.Register (gui, world)
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftDown gui world |> snd
            let world = World.monitorPlus handleMouseLeft Events.MouseLeftUp gui world |> snd
            world

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
    
        member this.GetDown world : bool = this.Get Property? Down world
        member this.SetDown (value : bool) world = this.SetFast Property? Down false false value world
        member this.Down = Lens.make Property? Down this.GetDown this.SetDown this
        member this.GetUpImage world : Image AssetTag = this.Get Property? UpImage world
        member this.SetUpImage (value : Image AssetTag) world = this.SetFast Property? UpImage false false value world
        member this.UpImage = Lens.make Property? UpImage this.GetUpImage this.SetUpImage this
        member this.GetDownImage world : Image AssetTag = this.Get Property? DownImage world
        member this.SetDownImage (value : Image AssetTag) world = this.SetFast Property? DownImage false false value world
        member this.DownImage = Lens.make Property? DownImage this.GetDownImage this.SetDownImage this
        member this.GetClickSoundOpt world : Audio AssetTag option = this.Get Property? ClickSoundOpt world
        member this.SetClickSoundOpt (value : Audio AssetTag option) world = this.SetFast Property? ClickSoundOpt false false value world
        member this.ClickSoundOpt = Lens.make Property? ClickSoundOpt this.GetClickSoundOpt this.SetClickSoundOpt this
        member this.GetOnClick world : Scripting.Expr = this.Get Property? OnClick world
        member this.SetOnClick (value : Scripting.Expr) world = this.SetFast Property? OnClick false false value world
        member this.OnClick = Lens.make Property? OnClick this.GetOnClick this.SetOnClick this
        member this.UpEvent = Events.Up --> this
        member this.DownEvent = Events.Down --> this
        member this.ClickEvent = Events.Click --> this

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if button.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world then
                        let world = button.SetDown true world
                        let eventTrace = EventTrace.record "ButtonDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish () (Events.Down --> button) eventTrace button world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if button.GetSelected world then
                let wasDown = button.GetDown world
                let world = button.SetDown false world
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world && wasDown then
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publish () (Events.Up --> button) eventTrace button world
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publish () (Events.Click --> button) eventTrace button world
                        let world = World.evalWithLogging (button.GetOnClick world) (button.GetScriptFrame world) button world |> snd'
                        let world =
                            match button.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound 1.0f clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member FacetNames =
            [typeof<TextFacet>.Name]

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Down false
             define Entity.UpImage (AssetTag.make<Image> Assets.DefaultPackage "Image")
             define Entity.DownImage (AssetTag.make<Image> Assets.DefaultPackage "Image2")
             define Entity.ClickSoundOpt (Some (AssetTag.make<Audio> Assets.DefaultPackage "Sound"))
             define Entity.OnClick Scripting.Unit]

        override dispatcher.Register (button, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown button world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp button world |> snd
            world

        override dispatcher.Actualize (button, world) =
            if button.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = button.GetDepthLayered world
                              PositionY = (button.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = button.GetPosition world
                                      Size = button.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = if button.GetDown world then button.GetDownImage world else button.GetUpImage world
                                      Color = if button.GetEnabled world then Vector4.One else button.GetDisabledColor world }}|])
                    world
            else world

        override dispatcher.GetQuickSize (button, world) =
            match Metadata.tryGetTextureSizeF (button.GetUpImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
    
        member this.GetLabelImage world : Image AssetTag = this.Get Property? LabelImage world
        member this.SetLabelImage (value : Image AssetTag) world = this.SetFast Property? LabelImage false false value world
        member this.LabelImage = Lens.make Property? LabelImage this.GetLabelImage this.SetLabelImage this

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.LabelImage (AssetTag.make<Image> Assets.DefaultPackage "Image3")]

        override dispatcher.Actualize (label, world) =
            if label.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = label.GetDepthLayered world
                              PositionY = (label.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = label.GetPosition world
                                      Size = label.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = label.GetLabelImage world
                                      Color = if label.GetEnabled world then Vector4.One else label.GetDisabledColor world }}|])
                    world
            else world

        override dispatcher.GetQuickSize (label, world) =
            match Metadata.tryGetTextureSizeF (label.GetLabelImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
    
        member this.GetBackgroundImage world : Image AssetTag = this.Get Property? BackgroundImage world
        member this.SetBackgroundImage (value : Image AssetTag) world = this.SetFast Property? BackgroundImage false false value world
        member this.BackgroundImage = Lens.make Property? BackgroundImage this.GetBackgroundImage this.SetBackgroundImage this

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member FacetNames =
            [typeof<TextFacet>.Name]

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.BackgroundImage (AssetTag.make<Image> Assets.DefaultPackage "Image3")]

        override dispatcher.Actualize (text, world) =
            if text.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = text.GetDepthLayered world
                              PositionY = (text.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = text.GetPosition world
                                      Size = text.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = text.GetBackgroundImage world
                                      Color = if text.GetEnabled world then Vector4.One else text.GetDisabledColor world }}|])
                    world
            else world

        override dispatcher.GetQuickSize (text, world) =
            match Metadata.tryGetTextureSizeF (text.GetBackgroundImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with
    
        member this.GetOpen world : bool = this.Get Property? Open world
        member this.SetOpen (value : bool) world = this.SetFast Property? Open false false value world
        member this.Open = Lens.make Property? Open this.GetOpen this.SetOpen this
        member this.GetPressed world : bool = this.Get Property? Pressed world
        member this.SetPressed (value : bool) world = this.SetFast Property? Pressed false false value world
        member this.Pressed = Lens.make Property? Pressed this.GetPressed this.SetPressed this
        member this.GetOpenImage world : Image AssetTag = this.Get Property? OpenImage world
        member this.SetOpenImage (value : Image AssetTag) world = this.SetFast Property? OpenImage false false value world
        member this.OpenImage = Lens.make Property? OpenImage this.GetOpenImage this.SetOpenImage this
        member this.GetClosedImage world : Image AssetTag = this.Get Property? ClosedImage world
        member this.SetClosedImage (value : Image AssetTag) world = this.SetFast Property? ClosedImage false false value world
        member this.ClosedImage = Lens.make Property? ClosedImage this.GetClosedImage this.SetClosedImage this
        member this.GetToggleSoundOpt world : Audio AssetTag option = this.Get Property? ToggleSoundOpt world
        member this.SetToggleSoundOpt (value : Audio AssetTag option) world = this.SetFast Property? ToggleSoundOpt false false value world
        member this.ToggleSoundOpt = Lens.make Property? ToggleSoundOpt this.GetToggleSoundOpt this.SetToggleSoundOpt this
        member this.GetOnToggle world : Scripting.Expr = this.Get Property? OnToggle world
        member this.SetOnToggle (value : Scripting.Expr) world = this.SetFast Property? OnToggle false false value world
        member this.OnToggle = Lens.make Property? OnToggle this.GetOnToggle this.SetOnToggle this
        member this.ToggleEvent = Events.Toggle --> this

    type ToggleDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let toggle = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if toggle.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (toggle.GetBounds world) then
                    if toggle.GetEnabled world then
                        let world = toggle.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let toggle = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if toggle.GetSelected world then
                let wasPressed = toggle.GetPressed world
                let world = toggle.SetPressed false world
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (toggle.GetBounds world) then
                    if toggle.GetEnabled world && wasPressed then
                        let world = toggle.SetOpen (not (toggle.GetOpen world)) world
                        let eventAddress = if toggle.GetOpen world then Events.Open else Events.Close
                        let eventTrace = EventTrace.record "ToggleDispatcher" "handleMouseLeftUp" EventTrace.empty
                        let world = World.publish () (eventAddress --> toggle) eventTrace toggle world
                        let eventTrace = EventTrace.record4 "ToggleDispatcher" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publish () (Events.Toggle --> toggle) eventTrace toggle world
                        let world = World.evalWithLogging (toggle.GetOnToggle world) (toggle.GetScriptFrame world) toggle world |> snd'
                        let world =
                            match toggle.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound 1.0f toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Open true
             define Entity.Pressed false
             define Entity.OpenImage (AssetTag.make<Image> Assets.DefaultPackage "Image")
             define Entity.ClosedImage (AssetTag.make<Image> Assets.DefaultPackage "Image2")
             define Entity.ToggleSoundOpt (Some (AssetTag.make<Audio> Assets.DefaultPackage "Sound"))
             define Entity.OnToggle Scripting.Unit]

        override dispatcher.Register (toggle, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown toggle world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp toggle world |> snd
            world

        override dispatcher.Actualize (toggle, world) =
            if toggle.GetVisibleLayered world then
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = toggle.GetDepthLayered world
                              PositionY = (toggle.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = toggle.GetPosition world
                                      Size = toggle.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = if toggle.GetOpen world && not (toggle.GetPressed world) then toggle.GetOpenImage world else toggle.GetClosedImage world
                                      Color = if toggle.GetEnabled world then Vector4.One else toggle.GetDisabledColor world }}|])
                    world
            else world

        override dispatcher.GetQuickSize (toggle, world) =
            match Metadata.tryGetTextureSizeF (toggle.GetOpenImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
    
        member this.GetTouched world : bool = this.Get Property? Touched world
        member this.SetTouched (value : bool) world = this.SetFast Property? Touched false false value world
        member this.Touched = Lens.make Property? Touched this.GetTouched this.SetTouched this
        member this.GetOnTouch world : Scripting.Expr = this.Get Property? OnTouch world
        member this.SetOnTouch (value : Scripting.Expr) world = this.SetFast Property? OnTouch false false value world
        member this.OnTouch = Lens.make Property? OnTouch this.GetOnTouch this.SetOnTouch this
        member this.GetOnUntouch world : Scripting.Expr = this.Get Property? OnUntouch world
        member this.SetOnUntouch (value : Scripting.Expr) world = this.SetFast Property? OnUntouch false false value world
        member this.OnUntouch = Lens.make Property? OnUntouch this.GetOnUntouch this.SetOnUntouch this
        member this.TouchEvent = Events.Touch --> this
        member this.UntouchEvent = Events.Untouch --> this


    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if feeler.GetSelected world then
                let mousePositionWorld = World.mouseToWorld (feeler.GetViewType world) data.Position world
                if  feeler.GetVisibleLayered world &&
                    Math.isPointInBounds mousePositionWorld (feeler.GetBounds world) then
                    if feeler.GetEnabled world then
                        let world = feeler.SetTouched true world
                        let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish data.Position (Events.Touch --> feeler) eventTrace feeler world
                        let world = World.evalWithLogging (feeler.GetOnTouch world) (feeler.GetScriptFrame world) feeler world |> snd'
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if feeler.GetSelected world && feeler.GetVisibleLayered world then
                if feeler.GetEnabled world then
                    let world = feeler.SetTouched false world
                    let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                    let world = World.publish data.Position (Events.Untouch --> feeler) eventTrace feeler world
                    let world = World.evalWithLogging (feeler.GetOnUntouch world) (feeler.GetScriptFrame world) feeler world |> snd'
                    (Resolve, world)
                else (Resolve, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Touched false
             define Entity.OnTouch Scripting.Unit
             define Entity.OnUntouch Scripting.Unit]

        override dispatcher.Register (feeler, world) =
            let world = World.monitorPlus handleMouseLeftDown Events.MouseLeftDown feeler world |> snd
            let world = World.monitorPlus handleMouseLeftUp Events.MouseLeftUp feeler world |> snd
            world

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member this.GetFill world : single = this.Get Property? Fill world
        member this.SetFill (value : single) world = this.SetFast Property? Fill false false value world
        member this.Fill = Lens.make Property? Fill this.GetFill this.SetFill this
        member this.GetFillInset world : single = this.Get Property? FillInset world
        member this.SetFillInset (value : single) world = this.SetFast Property? FillInset false false value world
        member this.FillInset = Lens.make Property? FillInset this.GetFillInset this.SetFillInset this
        member this.GetFillImage world : Image AssetTag = this.Get Property? FillImage world
        member this.SetFillImage (value : Image AssetTag) world = this.SetFast Property? FillImage false false value world
        member this.FillImage = Lens.make Property? FillImage this.GetFillImage this.SetFillImage this
        member this.GetBorderImage world : Image AssetTag = this.Get Property? BorderImage world
        member this.SetBorderImage (value : Image AssetTag) world = this.SetFast Property? BorderImage false false value world
        member this.BorderImage = Lens.make Property? BorderImage this.GetBorderImage this.SetBorderImage this

    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        let getFillBarSpriteDims (fillBar : Entity) world =
            let spriteSize = fillBar.GetSize world
            let spriteInset = spriteSize * fillBar.GetFillInset world * 0.5f
            let spritePosition = fillBar.GetPosition world + spriteInset
            let spriteWidth = (spriteSize.X - spriteInset.X * 2.0f) * fillBar.GetFill world
            let spriteHeight = spriteSize.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member Properties =
            [define Entity.Size (Vector2 (256.0f, 64.0f))
             define Entity.SwallowMouseLeft false
             define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillImage (AssetTag.make<Image> Assets.DefaultPackage "Image9")
             define Entity.BorderImage (AssetTag.make<Image> Assets.DefaultPackage "Image10")]

        override dispatcher.Actualize (fillBar, world) =
            if fillBar.GetVisibleLayered world then
                let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar world
                let fillBarColor = if fillBar.GetEnabled world then Vector4.One else fillBar.GetDisabledColor world
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = fillBar.GetDepthLayered world
                              PositionY = (fillBar.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = fillBar.GetPosition world
                                      Size = fillBar.GetSize world
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = fillBar.GetBorderImage world
                                      Color = fillBarColor }}
                          LayerableDescriptor
                            { Depth = fillBar.GetDepthLayered world
                              PositionY = (fillBar.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = fillBarSpritePosition
                                      Size = fillBarSpriteSize
                                      Rotation = 0.0f
                                      Offset = Vector2.Zero
                                      ViewType = Absolute
                                      InsetOpt = None
                                      Image = fillBar.GetFillImage world
                                      Color = fillBarColor }}|])
                    world
            else world

        override dispatcher.GetQuickSize (fillBar, world) =
            match Metadata.tryGetTextureSizeF (fillBar.GetBorderImage world) (World.getMetadata world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit EntityDispatcher ()

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

        static member Properties =
            [define Entity.BodyType Static
             define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackage "Image4")]

[<AutoOpen>]
module BoxDispatcherModule =

    type BoxDispatcher () =
        inherit EntityDispatcher ()

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

        static member Properties =
            [define Entity.StaticImage (AssetTag.make<Image> Assets.DefaultPackage "Image4")]

[<AutoOpen>]
module CharacterDispatcherModule =

    type Entity with
        
        member this.GetCharacterIdleLeftImage = this.Get Property? CharacterIdleLeftImage
        member this.SetCharacterIdleLeftImage = this.Set Property? CharacterIdleLeftImage
        member this.CharacterIdleLeftImage = Lens.make<Image AssetTag, World> Property? CharacterIdleLeftImage this.GetCharacterIdleLeftImage this.SetCharacterIdleLeftImage this
        member this.GetCharacterIdleRightImage = this.Get Property? CharacterIdleRightImage
        member this.SetCharacterIdleRightImage = this.Set Property? CharacterIdleRightImage
        member this.CharacterIdleRightImage = Lens.make<Image AssetTag, World> Property? CharacterIdleRightImage this.GetCharacterIdleRightImage this.SetCharacterIdleRightImage this
        member this.GetCharacterJumpLeftImage = this.Get Property? CharacterJumpLeftImage
        member this.SetCharacterJumpLeftImage = this.Set Property? CharacterJumpLeftImage
        member this.CharacterJumpLeftImage = Lens.make<Image AssetTag, World> Property? CharacterJumpLeftImage this.GetCharacterJumpLeftImage this.SetCharacterJumpLeftImage this
        member this.GetCharacterJumpRightImage = this.Get Property? CharacterJumpRightImage
        member this.SetCharacterJumpRightImage = this.Set Property? CharacterJumpRightImage
        member this.CharacterJumpRightImage = Lens.make<Image AssetTag, World> Property? CharacterJumpRightImage this.GetCharacterJumpRightImage this.SetCharacterJumpRightImage this
        member this.GetCharacterWalkLeftSheet = this.Get Property? CharacterWalkLeftSheet
        member this.SetCharacterWalkLeftSheet = this.Set Property? CharacterWalkLeftSheet
        member this.CharacterWalkLeftSheet = Lens.make<Image AssetTag, World> Property? CharacterWalkLeftSheet this.GetCharacterWalkLeftSheet this.SetCharacterWalkLeftSheet this
        member this.GetCharacterWalkRightSheet = this.Get Property? CharacterWalkRightSheet
        member this.SetCharacterWalkRightSheet = this.Set Property? CharacterWalkRightSheet
        member this.CharacterWalkRightSheet = Lens.make<Image AssetTag, World> Property? CharacterWalkRightSheet this.GetCharacterWalkRightSheet this.SetCharacterWalkRightSheet this
        member this.GetCharacterFacingLeft = this.Get Property? CharacterFacingLeft
        member this.SetCharacterFacingLeft = this.Set Property? CharacterFacingLeft
        member this.CharacterFacingLeft = Lens.make<bool, World> Property? CharacterFacingLeft this.GetCharacterFacingLeft this.SetCharacterFacingLeft this
        
    type CharacterDispatcher () =
        inherit EntityDispatcher ()

        static let computeWalkCelInset (celSize : Vector2) (celRun : int) delay time =
            let timeCompressed = time / delay
            let frame = timeCompressed % int64 celRun
            let i = single (frame % 3L)
            let j = single (frame / 3L)
            let offset = v2 (i * celSize.X) (j * celSize.Y) 
            v4 offset.X offset.Y (offset.X + celSize.X) (offset.Y + celSize.Y)

        static member FacetNames =
            [typeof<RigidBodyFacet>.Name]

        static member Properties =
            [define Entity.AnimationDelay 6L
             define Entity.CelSize (v2 28.0f 28.0f)
             define Entity.CelRun 8
             define Entity.FixedRotation true
             define Entity.GravityScale 3.0f
             define Entity.CollisionBody (BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = v2Zero })
             define Entity.CharacterIdleLeftImage (AssetTag.make Assets.DefaultPackage "CharacterIdleLeft")
             define Entity.CharacterIdleRightImage (AssetTag.make Assets.DefaultPackage "CharacterIdleRight")
             define Entity.CharacterJumpLeftImage (AssetTag.make Assets.DefaultPackage "CharacterJumpLeft")
             define Entity.CharacterJumpRightImage (AssetTag.make Assets.DefaultPackage "CharacterJumpRight")
             define Entity.CharacterWalkLeftSheet (AssetTag.make Assets.DefaultPackage "CharacterWalkLeft")
             define Entity.CharacterWalkRightSheet (AssetTag.make Assets.DefaultPackage "CharacterWalkRight")
             define Entity.CharacterFacingLeft false]

        override this.Update (entity, world) =
            // we have to a bit of hackery to remember whether the character is facing left or right
            // when there is no velocity
            let facingLeft = entity.GetCharacterFacingLeft world
            let velocity = World.getBodyLinearVelocity (entity.GetPhysicsId world) world
            if facingLeft && velocity.X > 1.0f then entity.SetCharacterFacingLeft false world
            elif not facingLeft && velocity.X < -1.0f then entity.SetCharacterFacingLeft true world
            else world

        override this.Actualize (entity, world) =
            if entity.GetVisibleLayered world && entity.GetInView world then
                let time = World.getTickTime world
                let physicsId = entity.GetPhysicsId world
                let facingLeft = entity.GetCharacterFacingLeft world
                let velocity = World.getBodyLinearVelocity physicsId world
                let celSize = entity.GetCelSize world
                let celRun = entity.GetCelRun world
                let animationDelay = entity.GetAnimationDelay world
                let (insetOpt, image) =
                    if not (World.isBodyOnGround physicsId world) then
                        let image =
                            if facingLeft
                            then entity.GetCharacterJumpLeftImage world
                            else entity.GetCharacterJumpRightImage world
                        (None, image)
                    elif velocity.X < 5.0f && velocity.X > -5.0f then
                        let image =
                            if facingLeft
                            then entity.GetCharacterIdleLeftImage world
                            else entity.GetCharacterIdleRightImage world
                        (None, image)
                    elif velocity.X < 0.0f then
                        let image = entity.GetCharacterWalkLeftSheet world
                        (Some (computeWalkCelInset celSize celRun animationDelay time), image)
                    else
                        let image = entity.GetCharacterWalkRightSheet world
                        (Some (computeWalkCelInset celSize celRun animationDelay time), image)
                World.enqueueRenderMessage
                    (RenderDescriptorsMessage
                        [|LayerableDescriptor
                            { Depth = entity.GetDepthLayered world
                              PositionY = (entity.GetPosition world).Y
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = v2Zero
                                      ViewType = entity.GetViewType world
                                      InsetOpt = insetOpt
                                      Image = image
                                      Color = v4One }}|])
                    world
            else world

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with
    
        member this.GetTileMapAsset world : TileMap AssetTag = this.Get Property? TileMapAsset world
        member this.SetTileMapAsset (value : TileMap AssetTag) world = this.SetFast Property? TileMapAsset false false value world
        member this.TileMapAsset = Lens.make Property? TileMapAsset this.GetTileMapAsset this.SetTileMapAsset this
        member this.GetParallax world : single = this.Get Property? Parallax world
        member this.SetParallax (value : single) world = this.SetFast Property? Parallax false false value world
        member this.Parallax = Lens.make Property? Parallax this.GetParallax this.SetParallax this

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        let tryMakeTileMapData (tileMapAsset : TileMap AssetTag) world =
            let metadataMap = World.getMetadata world
            match Metadata.tryGetTileMapMetadata tileMapAsset metadataMap with
            | Some (_, _, map) ->
                let mapSize = Vector2i (map.Width, map.Height)
                let tileSize = Vector2i (map.TileWidth, map.TileHeight)
                let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
                let tileMapSize = Vector2i (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
                let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
                let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
                let tileSetSize =
                    let tileSetWidthOpt = tileSet.Image.Width
                    let tileSetHeightOpt = tileSet.Image.Height
                    Vector2i (tileSetWidthOpt.Value / tileSize.X, tileSetHeightOpt.Value / tileSize.Y)
                Some { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }
            | None -> None

        let makeTileData (tm : Entity) tmd (tl : TmxLayer) tileIndex world =
            let mapRun = tmd.MapSize.X
            let tileSetRun = tmd.TileSetSize.X
            let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
            let tile = tl.Tiles.[tileIndex]
            let gid = tile.Gid - tmd.TileSet.FirstGid
            let gidPosition = gid * tmd.TileSize.X
            let gid2 = Vector2i (gid % tileSetRun, gid / tileSetRun)
            let tileMapPosition = tm.GetPosition world
            let tilePosition =
                Vector2i
                    (int tileMapPosition.X + tmd.TileSize.X * i,
                     int tileMapPosition.Y - tmd.TileSize.Y * (j + 1)) // subtraction for right-handedness
            let tileSetTileOpt = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; TileSetTileOpt = tileSetTileOpt }

        let getTileBodyProperties6 (tm : Entity) tmd tli td ti cexpr world =
            let tileShape = PhysicsEngine.localizeCollisionBody (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            { BodyId = makeGuidFromInts tli ti
              Position =
                Vector2
                    (single (td.TilePosition.X + tmd.TileSize.X / 2),
                     single (td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y))
              Rotation = tm.GetRotation world
              Shape = tileShape
              BodyType = BodyType.Static
              Awake = false
              Enabled = true
              Density = Constants.Physics.NormalDensity
              Friction = tm.GetFriction world
              Restitution = tm.GetRestitution world
              FixedRotation = true
              AngularVelocity = 0.0f
              AngularDamping = 0.0f
              LinearVelocity = Vector2.Zero
              LinearDamping = 0.0f
              GravityScale = 0.0f
              CollisionCategories = PhysicsEngine.categorizeCollisionMask (tm.GetCollisionCategories world)
              CollisionMask = PhysicsEngine.categorizeCollisionMask (tm.GetCollisionMask world)
              IsBullet = false
              IsSensor = false }

        let getTileBodyProperties tm tmd (tl : TmxLayer) tli ti world =
            let td = makeTileData tm tmd tl ti world
            match td.TileSetTileOpt with
            | Some tileSetTile ->
                match tileSetTile.Properties.TryGetValue Constants.Physics.CollisionProperty with
                | (true, cexpr) ->
                    let tileBody =
                        match cexpr with
                        | "" -> BodyBox { Extent = Vector2 0.5f; Center = Vector2.Zero }
                        | _ -> scvalue<BodyShape> cexpr
                    let tileBodyProperties = getTileBodyProperties6 tm tmd tli td ti tileBody world
                    Some tileBodyProperties
                | (false, _) -> None
            | None -> None

        let getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex (tileLayer : TmxLayer) world =
            if tileLayer.Properties.ContainsKey Constants.Physics.CollisionProperty then
                Seq.foldi
                    (fun i bodyPropertyList _ ->
                        match getTileBodyProperties tileMap tileMapData tileLayer tileLayerIndex i world with
                        | Some bodyProperties -> bodyProperties :: bodyPropertyList
                        | None -> bodyPropertyList)
                    [] tileLayer.Tiles |>
                Seq.toList
            else []

        let registerTileLayerPhysics (tileMap : Entity) tileMapData tileLayerIndex world tileLayer =
            let bodyPropertyList = getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex tileLayer world
            World.createBodies tileMap (tileMap.GetId world) bodyPropertyList world

        let registerTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            match tryMakeTileMapData tileMapAsset world with
            | Some tileMapData ->
                Seq.foldi
                    (registerTileLayerPhysics tileMap tileMapData)
                    world
                    tileMapData.Map.Layers
            | None -> Log.debug ("Could not make tile map data for '" + scstring tileMapAsset + "'."); world

        let getTileLayerPhysicsIds (tileMap : Entity) tileMapData tileLayer tileLayerIndex world =
            Seq.foldi
                (fun tileIndex physicsIds _ ->
                    let tileData = makeTileData tileMap tileMapData tileLayer tileIndex world
                    match tileData.TileSetTileOpt with
                    | Some tileSetTile ->
                        if tileSetTile.Properties.ContainsKey Constants.Physics.CollisionProperty then
                            let physicsId = { SourceId = tileMap.GetId world; BodyId = makeGuidFromInts tileLayerIndex tileIndex }
                            physicsId :: physicsIds
                        else physicsIds
                    | None -> physicsIds)
                [] tileLayer.Tiles |>
            Seq.toList

        let unregisterTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            match tryMakeTileMapData tileMapAsset world with
            | Some tileMapData ->
                Seq.foldi
                    (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                        if tileLayer.Properties.ContainsKey Constants.Physics.CollisionProperty then
                            let physicsIds = getTileLayerPhysicsIds tileMap tileMapData tileLayer tileLayerIndex world
                            World.destroyBodies physicsIds world
                        else world)
                    world
                    tileMapData.Map.Layers
            | None -> Log.debug ("Could not make tile map data for '" + scstring tileMapAsset + "'."); world

        static member Properties =
            [define Entity.Omnipresent true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask "@"
             define Entity.TileMapAsset (AssetTag.make<TileMap> Assets.DefaultPackage "TileMap")
             define Entity.Parallax 0.0f]

        override dispatcher.Register (tileMap, world) =
            registerTileMapPhysics tileMap world

        override dispatcher.Unregister (tileMap, world) =
            unregisterTileMapPhysics tileMap world
            
        override dispatcher.PropagatePhysics (tileMap, world) =
            let world = unregisterTileMapPhysics tileMap world
            registerTileMapPhysics tileMap world

        override dispatcher.Actualize (tileMap, world) =
            if tileMap.GetVisible world then
                match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getMetadata world) with
                | Some (_, images, map) ->
                    let layers = List.ofSeq map.Layers
                    let tileSourceSize = Vector2i (map.TileWidth, map.TileHeight)
                    let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                    let viewType = tileMap.GetViewType world
                    List.foldi
                        (fun i world (layer : TmxLayer) ->
                            let depth = tileMap.GetDepthLayered world + single i * 2.0f // MAGIC_VALUE: assumption
                            let parallaxTranslation =
                                match viewType with
                                | Absolute -> Vector2.Zero
                                | Relative -> tileMap.GetParallax world * depth * -World.getEyeCenter world
                            let parallaxPosition = tileMap.GetPosition world + parallaxTranslation
                            let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                            if World.isBoundsInView viewType (Math.makeBounds parallaxPosition size) world then
                                World.enqueueRenderMessage
                                    (RenderDescriptorsMessage
                                        [|LayerableDescriptor 
                                            { Depth = depth
                                              PositionY = (tileMap.GetPosition world).Y
                                              LayeredDescriptor =
                                                TileLayerDescriptor
                                                    { Position = parallaxPosition
                                                      Size = size
                                                      Rotation = tileMap.GetRotation world
                                                      ViewType = viewType
                                                      MapSize = Vector2i (map.Width, map.Height)
                                                      Tiles = layer.Tiles
                                                      TileSourceSize = tileSourceSize
                                                      TileSize = tileSize
                                                      TileSet = map.Tilesets.[0] // MAGIC_VALUE: I have no idea how to tell which tile set each tile is from...
                                                      TileSetImage = List.head images }}|]) // MAGIC_VALUE: for same reason as above
                                    world
                            else world)
                        world
                        layers
                | None -> world
            else world

        override dispatcher.GetQuickSize (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getMetadata world) with
            | Some (_, _, map) -> Vector2 (single (map.Width * map.TileWidth), single (map.Height * map.TileHeight))
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module LayerDispatcherModule =

    type Layer with
    
        member this.GetModel<'model> world = this.Get<'model> Property? Model world
        member this.SetModel<'model> value world = this.Set<'model> Property? Model value world
        member this.Model<'model> () = Lens.make<'model, World> Property? Model this.GetModel<'model> this.SetModel<'model> this

    type [<AbstractClass>] LayerDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit LayerDispatcher ()

        member this.GetModel (layer : Layer) world : 'model =
            layer.GetModel<'model> world

        member this.SetModel (model : 'model) (layer : Layer) world =
            layer.SetModel<'model> model world

        member this.Model (layer : Layer) =
            Lens.make Property? Model (this.GetModel layer) (flip this.SetModel layer) layer

        override this.Register (layer, world) =
            let (model, world) =
                match layer.TryGetProperty Property? Model world with
                | Some model -> (model :> obj :?> 'model, world)
                | None ->
                    let property = { PropertyType = typeof<'model>; PropertyValue = initial }
                    let world = World.attachLayerProperty Property? Model property layer world
                    (initial, world)
            let bindings = this.Bindings (model, layer, world)
            let world =
                List.fold (fun world binding ->
                    match binding with
                    | Message binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message ->
                                let (model, commands) = this.Message (message, this.GetModel layer world, layer, world)
                                let world = this.SetModel model layer world
                                List.fold (fun world command ->
                                    this.Command (command, this.GetModel layer world, layer, world))
                                    world commands
                            | None -> world)
                            layer binding.Stream world
                    | Command binding ->
                        Stream.monitor (fun evt world ->
                            match binding.MakeValueOpt evt with
                            | Some message -> this.Command (message, this.GetModel layer world, layer, world)
                            | None -> world)
                            layer binding.Stream world)
                    world bindings
            let content = this.Content (this.Model layer, layer, world)
            List.fold (fun world content -> World.expandEntityContent None content None layer world) world content

        override this.Actualize (layer, world) =
            let views = this.View (this.GetModel layer world, layer, world)
            List.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Effect effect -> effect world)
                world views

        abstract member Bindings : 'model * Layer * World -> Binding<'message, 'command, Layer, World> list
        default this.Bindings (_, _, _) = []
        
        abstract member Message : 'message * 'model * Layer * World -> 'model * 'command list
        default this.Message (_, model, _, _) = just model

        abstract member Command : 'command * 'model * Layer * World -> World
        default this.Command (_, _, _, world) = world

        abstract member Content : Lens<'model, World> * Layer * World -> EntityContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Layer * World -> View list
        default this.View (_, _, _) = []

[<AutoOpen>]
module ScreenDispatcherModule =

    type Screen with
    
        member this.GetModel<'model> world = this.Get<'model> Property? Model world
        member this.SetModel<'model> value world = this.Set<'model> Property? Model value world
        member this.Model<'model> () = Lens.make<'model, World> Property? Model this.GetModel<'model> this.SetModel<'model> this

    type [<AbstractClass>] ScreenDispatcher<'model, 'message, 'command> (initial : 'model) =
        inherit ScreenDispatcher ()

        member this.GetModel (screen : Screen) world : 'model =
            screen.GetModel<'model> world

        member this.SetModel (model : 'model) (screen : Screen) world =
            screen.SetModel<'model> model world

        member this.Model (screen : Screen) =
            Lens.make Property? Model (this.GetModel screen) (flip this.SetModel screen) screen

        override this.Register (screen, world) =
            let (model, world) =
                match screen.TryGetProperty Property? Model world with
                | Some model -> (model :> obj :?> 'model, world)
                | None ->
                    let property = { PropertyType = typeof<'model>; PropertyValue = initial }
                    let world = World.attachScreenProperty Property? Model property screen world
                    (initial, world)
            let bindings = this.Bindings (model, screen, world)
            let world =
                List.fold (fun world binding ->
                    match binding with
                    | Message binding ->
                        Stream.monitor (fun evt world ->
                            let messageOpt = binding.MakeValueOpt evt
                            match messageOpt with
                            | Some message ->
                                let (model, commands) = this.Message (message, this.GetModel screen world, screen, world)
                                let world = this.SetModel model screen world
                                List.fold (fun world command ->
                                    this.Command (command, this.GetModel screen world, screen, world))
                                    world commands
                            | None -> world)
                            screen binding.Stream world
                    | Command binding ->
                        Stream.monitor (fun evt world ->
                            let messageOpt = binding.MakeValueOpt evt
                            match messageOpt with
                            | Some message -> this.Command (message, this.GetModel screen world, screen, world)
                            | None -> world)
                            screen binding.Stream world)
                    world bindings
            let content = this.Content (this.Model screen, screen, world)
            let world = List.fold (fun world content -> World.expandLayerContent None content screen world) world content
            world

        override this.Actualize (screen, world) =
            let views = this.View (this.GetModel screen world, screen, world)
            List.fold (fun world view ->
                match view with
                | Render descriptor -> World.enqueueRenderMessage (RenderDescriptorsMessage [|descriptor|]) world
                | PlaySound (volume, assetTag) -> World.playSound volume assetTag world
                | PlaySong (fade, volume, assetTag) -> World.playSong fade volume assetTag world
                | FadeOutSong fade -> World.fadeOutSong fade world
                | StopSong -> World.stopSong world
                | Effect effect -> effect world)
                world views

        abstract member Bindings : 'model * Screen * World -> Binding<'message, 'command, Screen, World> list
        default this.Bindings (_, _, _) = []

        abstract member Message : 'message * 'model * Screen * World -> 'model * 'command list
        default this.Message (_, model, _, _) = just model

        abstract member Command : 'command * 'model * Screen * World -> World
        default this.Command (_, _, _, world) = world

        abstract member Content : Lens<'model, World> * Screen * World -> LayerContent list
        default this.Content (_, _, _) = []

        abstract member View : 'model * Screen * World -> View list
        default this.View (_, _, _) = []