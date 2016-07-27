// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.ComponentModel
open FSharpx.Collections
open OpenTK
open Prime
open TiledSharp
open Prime
open Nu

[<AutoOpen>]
module MountFacetModule =

    type Entity with
    
        member this.GetOptMountRelation world : Entity Relation option = this.Get Property? OptMountRelation world
        member this.SetOptMountRelation (value : Entity Relation option) world = this.Set Property? OptMountRelation value world
        member this.TagOptMountRelation = PropertyTag.make this Property? OptMountRelation this.GetOptMountRelation this.SetOptMountRelation
        member this.GetPositionLocal world : Vector2 = this.Get Property? PositionLocal world
        member this.SetPositionLocal (value : Vector2) world = this.Set Property? PositionLocal value world
        member this.TagPositionLocal = PropertyTag.make this Property? PositionLocal this.GetPositionLocal this.SetPositionLocal
        member this.GetDepthLocal world : single = this.Get Property? DepthLocal world
        member this.SetDepthLocal (value : single) world = this.Set Property? DepthLocal value world
        member this.TagDepthLocal = PropertyTag.make this Property? DepthLocal this.GetDepthLocal this.SetDepthLocal
        member private this.GetMountUpdateCountNp world : int64 = this.Get Property? MountUpdateCountNp world
        member private this.SetMountUpdateCountNp (value : int64) world = this.Set Property? MountUpdateCountNp value world
        member private this.TagMountUpdateCountNp = PropertyTag.make this Property? MountUpdateCountNp this.GetMountUpdateCountNp this.SetMountUpdateCountNp
        member private this.GetMountUnsubscribeNp world : World -> World = this.Get Property? MountUnsubscribeNp world
        member private this.SetMountUnsubscribeNp (value : World -> World) world = this.Set Property? MountUnsubscribeNp value world
        member private this.TagMountUnsubscribeNp = PropertyTag.make this Property? MountUnsubscribeNp this.GetMountUnsubscribeNp this.SetMountUnsubscribeNp

    type MountFacet () =
        inherit Facet ()

        static let handleRelationChange evt world =
            let entity = evt.Subscriber : Entity
            let target = evt.Publisher :?> Entity
            let transform = target.GetTransform world
            let updateCount = World.getUpdateCount world
            if  transform <> target.GetTransform evt.Data.OldWorld &&
                updateCount <> entity.GetMountUpdateCountNp world then
                let transform =
                    { transform with
                        Position = transform.Position + entity.GetPositionLocal world
                        Depth = transform.Depth + entity.GetDepthLocal world }
                let world = entity.SetTransform transform world
                let world = entity.SetMountUpdateCountNp updateCount world
                (Cascade, world)
            else (Cascade, world)

        static let rec handleEntityChange evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetOptMountRelation evt.Data.OldWorld <> entity.GetOptMountRelation world then
                let world = (entity.GetMountUnsubscribeNp world) world
                let (unsubscribe, world) = World.monitorPlus handleRelationChange (entity.GetChangeEvent Property? Position) entity world
                let (unsubscribe2, world) = World.monitorPlus handleRelationChange (entity.GetChangeEvent Property? Depth) entity world
                let world = entity.SetMountUnsubscribeNp (unsubscribe2 >> unsubscribe) world
                (Cascade, world)
            else (Cascade, world)

        static member PropertyDefinitions =
            [Define? OptMountRelation (None : Entity Relation option)
             Define? PositionLocal Vector2.Zero
             Define? DepthLocal 0.0f
             Define? MountUpdateCountNp Int64.MinValue
             Define? MountUnsubscribeNp (id : World -> World)]

        override facet.Register (entity, world) =
            let world = World.monitor handleEntityChange (entity.GetChangeEvent Property? Position) entity world
            let world = World.monitor handleEntityChange (entity.GetChangeEvent Property? Depth) entity world
            let (unsubscribe, world) =
                match entity.GetOptMountRelation world with
                | Some target ->
                    let address = Relation.resolve entity.EntityAddress target
                    let (unsubscribe, world) = World.monitorPlus handleRelationChange (Events.EntityChange Property? Position ->>- address) entity world
                    let (unsubscribe2, world) = World.monitorPlus handleRelationChange (Events.EntityChange Property? Depth ->>- address) entity world
                    (unsubscribe2 >> unsubscribe, world)
                | None -> (id, world)
            entity.SetMountUnsubscribeNp unsubscribe world

[<AutoOpen>]
module EffectFacetModule =

    open Effects

    type EffectTags =
        Map<string, Symbol * Slice list>

    type Entity with
    
        member this.GetSelfDestruct world : bool = this.Get Property? SelfDestruct world
        member this.SetSelfDestruct (value : bool) world = this.Set Property? SelfDestruct value world
        member this.TagSelfDestruct = PropertyTag.make this Property? SelfDestruct this.GetSelfDestruct this.SetSelfDestruct
        member this.GetOptEffects world : AssetTag list option = this.Get Property? OptEffects world
        member this.SetOptEffects (value : AssetTag list option) world = this.Set Property? OptEffects value world
        member this.TagOptEffects = PropertyTag.make this Property? OptEffects this.GetOptEffects this.SetOptEffects
        member this.GetOptEffectsLc world : AssetTag list option = this.Get Property? OptEffectsLc world
        member private this.SetOptEffectsLc (value : AssetTag list option) world = this.Set Property? OptEffectsLc value world
        member this.TagOptEffectsLc = PropertyTag.makeReadOnly this Property? OptEffectsLc this.GetOptEffectsLc
        member this.GetOptEffectStartTime world : int64 option = this.Get Property? OptEffectStartTime world
        member this.SetOptEffectStartTime (value : int64 option) world = this.Set Property? OptEffectStartTime value world
        member this.TagOptEffectStartTime = PropertyTag.make this Property? OptEffectStartTime this.GetOptEffectStartTime this.SetOptEffectStartTime
        member this.GetEffectDefinitions world : Definitions = this.Get Property? EffectDefinitions world
        member this.SetEffectDefinitions (value : Definitions) world = this.Set Property? EffectDefinitions value world
        member this.TagEffectDefinitions = PropertyTag.make this Property? EffectDefinitions this.GetEffectDefinitions this.SetEffectDefinitions
        member this.GetEffect world : Effect = this.Get Property? Effect world
        member this.SetEffect (value : Effect) world = this.Set Property? Effect value world
        member this.TagEffect = PropertyTag.make this Property? Effect this.GetEffect this.SetEffect
        member this.GetEffectOffset world : Vector2 = this.Get Property? EffectOffset world
        member this.SetEffectOffset (value : Vector2) world = this.Set Property? EffectOffset value world
        member this.TagEffectOffset = PropertyTag.make this Property? EffectOffset this.GetEffectOffset this.SetEffectOffset
        member this.GetEffectHistoryMax world : int = this.Get Property? EffectHistoryMax world
        member this.SetEffectHistoryMax (value : int) world = this.Set Property? EffectHistoryMax value world
        member this.TagEffectHistoryMax = PropertyTag.make this Property? EffectHistoryMax this.GetEffectHistoryMax this.SetEffectHistoryMax
        member this.GetEffectHistoryNp world : Slice Deque = this.Get Property? EffectHistoryNp world
        member private this.SetEffectHistoryNp (value : Slice Deque) world = this.Set Property? EffectHistoryNp value world
        member this.TagEffectHistoryNp = PropertyTag.makeReadOnly this Property? EffectHistoryNp this.GetEffectHistoryNp
        member this.GetEffectPhysicsShapesNp world : unit = this.Get Property? EffectPhysicsShapesNp world // NOTE: the default EffectFacet leaves it up to the Dispatcher to do something with the effect's physics output
        member private this.SetEffectPhysicsShapesNp (value : unit) world = this.Set Property? EffectPhysicsShapesNp value world
        member this.TagEffectPhysicsShapesNp = PropertyTag.makeReadOnly this Property? EffectPhysicsShapesNp this.GetEffectPhysicsShapesNp
        member this.GetEffectTagsNp world : EffectTags = this.Get Property? EffectTagsNp world
        member private this.SetEffectTagsNp (value : EffectTags) world = this.Set Property? EffectTagsNp value world
        member this.TagEffectTagsNp = PropertyTag.makeReadOnly this Property? EffectTagsNp this.GetEffectTagsNp
        
        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetOptEffectStartTime world with
            | Some effectStartTime -> effectStartTime
            | None -> 0L

        /// The time relative to the start of the effect.
        member this.GetEffectTime world =
            let effectStartTime = this.GetEffectStartTime world
            let tickTime = World.getTickTime world
            tickTime - effectStartTime

    type EffectFacet () =
        inherit Facet ()

        static let assetTagsToOptEffects assetTags world =
            let (optSymbols, world) = World.tryFindSymbols assetTags world
            let optEffects =
                List.map
                    (fun optSymbol ->
                        match optSymbol with
                        | Some symbol ->
                            try let effect = valueize<Effect> symbol in Some effect
                            with exn -> Log.info ^ "Failed to convert symbol '" + scstring symbol + "' to Effect due to: " + scstring exn; None
                        | None -> None)
                    optSymbols
            (optEffects, world)

        static let setEffect optEffects (entity : Entity) world =
            match optEffects with
            | Some effectAssetTags ->
                let (effects, world) = assetTagsToOptEffects effectAssetTags world |> mapFst List.definitize
                let effectCombined = EffectSystem.combineEffects effects
                entity.SetEffect effectCombined world
            | None -> entity.SetEffect Effect.empty world

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let optEffects = entity.GetOptEffects world
            let world = setEffect optEffects entity world
            (Cascade, world)

        static member PropertyDefinitions =
            [Define? SelfDestruct false
             Define? OptEffects (None : AssetTag list option)
             Define? OptEffectsLc (None : AssetTag list option)
             Define? OptEffectStartTime (None : int64 option)
             Define? EffectDefinitions (Map.empty : Definitions)
             Define? Effect Effect.empty
             Define? EffectOffset Vector2.Zero
             Define? EffectHistoryMax Constants.Effects.DefaultEffectHistoryMax
             Define? EffectHistoryNp Deque.empty<Slice>
             Define? EffectPhysicsShapesNp ()
             Define? EffectTagsNp (Map.empty : EffectTags)]

        override facet.Actualize (entity, world) =
            if entity.InView world then
                let world = entity.SetEffectTagsNp Map.empty world
                let effect = entity.GetEffect world
                let effectTime = entity.GetEffectTime world
                let effectViewType = entity.GetViewType world
                let effectSlice =
                    { Position = entity.GetPosition world + Vector2.Multiply (entity.GetSize world, entity.GetEffectOffset world)
                      Size = entity.GetSize world
                      Rotation = entity.GetRotation world
                      Depth = entity.GetDepth world
                      Offset = Vector2 0.5f
                      Color = Vector4.One
                      Enabled = true
                      Volume = 1.0f }
                let effectHistory = entity.GetEffectHistoryNp world
                let effectEnv = entity.GetEffectDefinitions world
                let effectSystem = EffectSystem.make effectViewType effectHistory effectTime effectEnv
                let world =
                    let artifacts = EffectSystem.eval effect effectSlice effectSystem
                    List.fold (fun world artifact ->
                        match artifact with
                        | RenderArtifact renderDescriptors -> World.addRenderMessage (RenderDescriptorsMessage renderDescriptors) world
                        | SoundArtifact (volume, sound) -> World.playSound volume sound world
                        | TagArtifact (name, metadata, slice) ->
                            let effectTags = entity.GetEffectTagsNp world
                            let effectTags =
                                match Map.tryFind name effectTags with
                                | Some (metadata, slices) -> Map.add name (metadata, slice :: slices) effectTags
                                | None -> Map.add name (metadata, [slice]) effectTags
                            // TODO: also raise event for all new effect tags so they can be handled in scripts?
                            entity.SetEffectTagsNp effectTags world)
                        world
                        artifacts
                let effectHistoryMax = entity.GetEffectHistoryMax world
                let effectHistory = Deque.cons effectSlice effectHistory
                let effectHistory = if Deque.length effectHistory > effectHistoryMax then fst ^ Deque.unconj effectHistory else effectHistory
                entity.SetEffectHistoryNp effectHistory world
            else world

        override facet.Update (entity, world) =
            
            // update for combined effects changes
            let world =
                let optEffects = entity.GetOptEffects world
                if entity.GetOptEffectsLc world <> optEffects
                then let world = setEffect optEffects entity world in entity.SetOptEffectsLc optEffects world
                else world

            // update for self-destruction
            let world =
                let effect = entity.GetEffect world
                match (entity.GetSelfDestruct world, effect.OptLifetime) with
                | (true, Some lifetime) -> if entity.GetEffectTime world > lifetime then World.destroyEntity entity world else world
                | (_, _) -> world

            // fin
            world

        override facet.Register (entity, world) =
            let effectStartTime = Option.getOrDefault (World.getTickTime world) (entity.GetOptEffectStartTime world)
            let world = entity.SetOptEffectStartTime (Some effectStartTime) world
            World.monitor handleAssetsReload Events.AssetsReload entity world

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with

        member this.GetMinorId world : Guid = this.Get Property? MinorId world
        member this.SetMinorId (value : Guid) world = this.Set Property? MinorId value world
        member this.TagMinorId = PropertyTag.make this Property? MinorId this.GetMinorId this.SetMinorId
        member this.GetBodyType world : BodyType = this.Get Property? BodyType world
        member this.SetBodyType (value : BodyType) world = this.Set Property? BodyType value world
        member this.TagBodyType = PropertyTag.make this Property? BodyType this.GetBodyType this.SetBodyType
        member this.GetAwake world : bool = this.Get Property? Awake world
        member this.SetAwake (value : bool) world = this.Set Property? Awake value world
        member this.TagAwake = PropertyTag.make this Property? Awake this.GetAwake this.SetAwake
        member this.GetEnabled world : bool = this.Get Property? Enabled world
        member this.SetEnabled (value : bool) world = this.Set Property? Enabled value world
        member this.TagEnabled = PropertyTag.make this Property? Enabled this.GetEnabled this.SetEnabled
        member this.GetDensity world : single = this.Get Property? Density world
        member this.SetDensity (value : single) world = this.Set Property? Density value world
        member this.TagDensity = PropertyTag.make this Property? Density this.GetDensity this.SetDensity
        member this.GetFriction world : single = this.Get Property? Friction world
        member this.SetFriction (value : single) world = this.Set Property? Friction value world
        member this.TagFriction = PropertyTag.make this Property? Friction this.GetFriction this.SetFriction
        member this.GetRestitution world : single = this.Get Property? Restitution world
        member this.SetRestitution (value : single) world = this.Set Property? Restitution value world
        member this.TagRestitution = PropertyTag.make this Property? Restitution this.GetRestitution this.SetRestitution
        member this.GetFixedRotation world : bool = this.Get Property? FixedRotation world
        member this.SetFixedRotation (value : bool) world = this.Set Property? FixedRotation value world
        member this.TagFixedRotation = PropertyTag.make this Property? FixedRotation this.GetFixedRotation this.SetFixedRotation
        member this.GetAngularVelocity world : single = this.Get Property? AngularVelocity world
        member this.SetAngularVelocity (value : single) world = this.Set Property? AngularVelocity value world
        member this.TagAngularVelocity = PropertyTag.make this Property? AngularVelocity this.GetAngularVelocity this.SetAngularVelocity
        member this.GetAngularDamping world : single = this.Get Property? AngularDamping world
        member this.SetAngularDamping (value : single) world = this.Set Property? AngularDamping value world
        member this.TagAngularDamping = PropertyTag.make this Property? AngularDamping this.GetAngularDamping this.SetAngularDamping
        member this.GetLinearVelocity world : Vector2 = this.Get Property? LinearVelocity world
        member this.SetLinearVelocity (value : Vector2) world = this.Set Property? LinearVelocity value world
        member this.TagLinearVelocity = PropertyTag.make this Property? LinearVelocity this.GetLinearVelocity this.SetLinearVelocity
        member this.GetLinearDamping world : single = this.Get Property? LinearDamping world
        member this.SetLinearDamping (value : single) world = this.Set Property? LinearDamping value world
        member this.TagLinearDamping = PropertyTag.make this Property? LinearDamping this.GetLinearDamping this.SetLinearDamping
        member this.GetGravityScale world : single = this.Get Property? GravityScale world
        member this.SetGravityScale (value : single) world = this.Set Property? GravityScale value world
        member this.TagGravityScale = PropertyTag.make this Property? GravityScale this.GetGravityScale this.SetGravityScale
        member this.GetCollisionCategories world : string = this.Get Property? CollisionCategories world
        member this.SetCollisionCategories (value : string) world = this.Set Property? CollisionCategories value world
        member this.TagCollisionCategories = PropertyTag.make this Property? CollisionCategories this.GetCollisionCategories this.SetCollisionCategories
        member this.GetCollisionMask world : string = this.Get Property? CollisionMask world
        member this.SetCollisionMask (value : string) world = this.Set Property? CollisionMask value world
        member this.TagCollisionMask = PropertyTag.make this Property? CollisionMask this.GetCollisionMask this.SetCollisionMask
        member this.GetCollisionBody world : BodyShape = this.Get Property? CollisionBody world
        member this.SetCollisionBody (value : BodyShape) world = this.Set Property? CollisionBody value world
        member this.TagCollisionBody = PropertyTag.make this Property? CollisionBody this.GetCollisionBody this.SetCollisionBody
        member this.GetIsBullet world : bool = this.Get Property? IsBullet world
        member this.SetIsBullet (value : bool) world = this.Set Property? IsBullet value world
        member this.TagIsBullet = PropertyTag.make this Property? IsBullet this.GetIsBullet this.SetIsBullet
        member this.GetIsSensor world : bool = this.Get Property? IsSensor world
        member this.SetIsSensor (value : bool) world = this.Set Property? IsSensor value world
        member this.TagIsSensor = PropertyTag.make this Property? IsSensor this.GetIsSensor this.SetIsSensor
        member this.GetPhysicsId world = { SourceId = this.GetId world; BodyId = this.GetMinorId world }
        member this.TagPhysicsId = PropertyTag.makeReadOnly this Property? PhysicsId this.GetPhysicsId

    type RigidBodyFacet () =
        inherit Facet ()

        static let getBodyShape (entity : Entity) world =
            Physics.localizeCollisionBody (entity.GetSize world) (entity.GetCollisionBody world)

        static member PropertyDefinitions =
            [Variable? MinorId ^ fun () -> makeGuid ()
             Define? BodyType Dynamic
             Define? Awake true
             Define? Enabled true
             Define? Density Constants.Physics.NormalDensity
             Define? Friction 0.0f
             Define? Restitution 0.0f
             Define? FixedRotation false
             Define? AngularVelocity 0.0f
             Define? AngularDamping 1.0f
             Define? LinearVelocity Vector2.Zero
             Define? LinearDamping 1.0f
             Define? GravityScale 1.0f
             Define? CollisionCategories "1"
             Define? CollisionMask "@"
             Define? CollisionBody ^ BodyBox { Extent = Vector2 0.5f; Center = Vector2.Zero }
             Define? IsBullet false
             Define? IsSensor false]

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
                  CollisionCategories = Physics.categorizeCollisionMask ^ entity.GetCollisionCategories world
                  CollisionMask = Physics.categorizeCollisionMask ^ entity.GetCollisionMask world
                  IsBullet = entity.GetIsBullet world
                  IsSensor = entity.GetIsSensor world }
            World.createBody entity (entity.GetId world) bodyProperties world

        override facet.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetPhysicsId world) world

        override facet.PropagatePhysics (entity, world) =
            let world = facet.UnregisterPhysics (entity, world)
            facet.RegisterPhysics (entity, world)

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with

        member this.GetStaticImage world : AssetTag = this.Get Property? StaticImage world
        member this.SetStaticImage (value : AssetTag) world = this.Set Property? StaticImage value world
        member this.TagStaticImage = PropertyTag.make this Property? StaticImage this.GetStaticImage this.SetStaticImage

    type StaticSpriteFacet () =
        inherit Facet ()

        static member PropertyDefinitions =
            [Define? StaticImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

        override facet.Actualize (entity, world) =
            if entity.InView world then
                World.addRenderMessage
                    (RenderDescriptorsMessage
                        [LayerableDescriptor
                            { Depth = entity.GetDepth world
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      OptInset = None
                                      Image = entity.GetStaticImage world
                                      Color = Vector4.One }}])
                    world
            else world

        override facet.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeAsVector2 (entity.GetStaticImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
    
        member this.GetCelSize world : Vector2 = this.Get Property? CelSize world
        member this.SetCelSize (value : Vector2) world = this.Set Property? CelSize value world
        member this.TagCelSize = PropertyTag.make this Property? CelSize this.GetCelSize this.SetCelSize
        member this.GetCelRun world : int = this.Get Property? CelRun world
        member this.SetCelRun (value : int) world = this.Set Property? CelRun value world
        member this.TagCelRun = PropertyTag.make this Property? CelRun this.GetCelRun this.SetCelRun
        member this.GetCelCount world : int = this.Get Property? CelCount world
        member this.SetCelCount (value : int) world = this.Set Property? CelCount value world
        member this.TagCelCount = PropertyTag.make this Property? CelCount this.GetCelCount this.SetCelCount
        member this.GetAnimationStutter world : int64 = this.Get Property? AnimationStutter world
        member this.SetAnimationStutter (value : int64) world = this.Set Property? AnimationStutter value world
        member this.TagAnimationStutter = PropertyTag.make this Property? AnimationStutter this.GetAnimationStutter this.SetAnimationStutter
        member this.GetAnimationSheet world : AssetTag = this.Get Property? AnimationSheet world
        member this.SetAnimationSheet (value : AssetTag) world = this.Set Property? AnimationSheet value world
        member this.TagAnimationSheet = PropertyTag.make this Property? AnimationSheet this.GetAnimationSheet this.SetAnimationSheet

    type AnimatedSpriteFacet () =
        inherit Facet ()

        static let getOptSpriteInset (entity : Entity) world =
            let celCount = entity.GetCelCount world
            let celRun = entity.GetCelRun world
            if celCount <> 0 && celRun <> 0 then
                let cel = int (World.getTickTime world / entity.GetAnimationStutter world) % celCount
                let celSize = entity.GetCelSize world
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = single celI * celSize.X
                let celY = single celJ * celSize.Y
                let inset = Vector4 (celX, celY, celX + celSize.X, celY + celSize.Y)
                Some inset
            else None

        static member PropertyDefinitions =
            [Define? CelCount 16 
             Define? CelSize ^ Vector2 (16.0f, 16.0f)
             Define? CelRun 4
             Define? AnimationStutter 4L
             Define? AnimationSheet { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image7" }]

        override facet.Actualize (entity, world) =
            if entity.InView world then
                World.addRenderMessage
                    (RenderDescriptorsMessage
                        [LayerableDescriptor
                            { Depth = entity.GetDepth world
                              LayeredDescriptor =
                                SpriteDescriptor
                                    { Position = entity.GetPosition world
                                      Size = entity.GetSize world
                                      Rotation = entity.GetRotation world
                                      Offset = Vector2.Zero
                                      ViewType = entity.GetViewType world
                                      OptInset = getOptSpriteInset entity world
                                      Image = entity.GetAnimationSheet world
                                      Color = Vector4.One }}])
                    world
            else world

        override facet.GetQuickSize (entity, world) =
            entity.GetCelSize world

[<AutoOpen>]
module GuiDispatcherModule =

    type Entity with
    
        member this.GetDisabledColor world : Vector4 = this.Get Property? DisabledColor world
        member this.SetDisabledColor (value : Vector4) world = this.Set Property? DisabledColor value world
        member this.TagDisabledColor = PropertyTag.make this Property? DisabledColor this.GetDisabledColor this.SetDisabledColor
        member this.GetSwallowMouseLeft world : bool = this.Get Property? SwallowMouseLeft world
        member this.SetSwallowMouseLeft (value : bool) world = this.Set Property? SwallowMouseLeft value world
        member this.TagSwallowMouseLeft = PropertyTag.make this Property? SwallowMouseLeft this.GetSwallowMouseLeft this.SetSwallowMouseLeft

    type GuiDispatcher () =
        inherit EntityDispatcher ()

        static let handleMouseLeft evt world =
            let gui = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let handling =
                if World.isEntitySelected gui world && gui.GetVisible world then
                    let mousePositionWorld = World.mouseToWorld (gui.GetViewType world) data.Position world
                    if data.Down &&
                       gui.GetSwallowMouseLeft world &&
                       Math.isPointInBounds mousePositionWorld (gui.GetBounds world) then
                       Resolve
                    else Cascade
                else Cascade
            (handling, world)
        
        static member PropertyDefinitions =
            [Define? ViewType Absolute
             Define? Enabled true
             Define? DisabledColor ^ Vector4 0.75f
             Define? SwallowMouseLeft true]

        override dispatcher.Register (gui, world) =
            world |>
                World.monitor handleMouseLeft Events.MouseLeftDown gui |>
                World.monitor handleMouseLeft Events.MouseLeftUp gui

[<AutoOpen>]
module ButtonDispatcherModule =

    type Entity with
    
        member this.GetDown world : bool = this.Get Property? Down world
        member this.SetDown (value : bool) world = this.Set Property? Down value world
        member this.TagDown = PropertyTag.make this Property? Down this.GetDown this.SetDown
        member this.GetUpImage world : AssetTag = this.Get Property? UpImage world
        member this.SetUpImage (value : AssetTag) world = this.Set Property? UpImage value world
        member this.TagUpImage = PropertyTag.make this Property? UpImage this.GetUpImage this.SetUpImage
        member this.GetDownImage world : AssetTag = this.Get Property? DownImage world
        member this.SetDownImage (value : AssetTag) world = this.Set Property? DownImage value world
        member this.TagDownImage = PropertyTag.make this Property? DownImage this.GetDownImage this.SetDownImage
        member this.GetOptClickSound world : AssetTag option = this.Get Property? OptClickSound world
        member this.SetOptClickSound (value : AssetTag option) world = this.Set Property? OptClickSound value world
        member this.TagOptClickSound = PropertyTag.make this Property? OptClickSound this.GetOptClickSound this.SetOptClickSound

    type ButtonDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if World.isEntitySelected button world then
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisible world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world then
                        let world = button.SetDown true world
                        let eventTrace = EventTrace.record "ButtonDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish () (Events.Down ->- button) eventTrace button world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let button = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if World.isEntitySelected button world then
                let wasDown = button.GetDown world
                let world = button.SetDown false world
                let mousePositionWorld = World.mouseToWorld (button.GetViewType world) data.Position world
                if  button.GetVisible world &&
                    Math.isPointInBounds mousePositionWorld (button.GetBounds world) then
                    if button.GetEnabled world && wasDown then
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publish () (Events.Up ->- button) eventTrace button world
                        let eventTrace = EventTrace.record4 "ButtonDispatcher" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publish () (Events.Click ->- button) eventTrace button world
                        let world =
                            match button.GetOptClickSound world with
                            | Some clickSound -> World.playSound 1.0f clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft false
             Define? Down false
             Define? UpImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image" }
             Define? DownImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image2" }
             Define? OptClickSound ^ Some { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Sound" }]

        override dispatcher.Register (button, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown button |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp button

        override dispatcher.Actualize (button, world) =
            World.addRenderMessage
                (RenderDescriptorsMessage
                    [LayerableDescriptor
                        { Depth = button.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = button.GetPosition world
                                  Size = button.GetSize world
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = if button.GetDown world then button.GetDownImage world else button.GetUpImage world
                                  Color = if button.GetEnabled world then Vector4.One else button.GetDisabledColor world }}])
                world

        override dispatcher.GetQuickSize (button, world) =
            match Metadata.tryGetTextureSizeAsVector2 (button.GetUpImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module LabelDispatcherModule =

    type Entity with
    
        member this.GetLabelImage world : AssetTag = this.Get Property? LabelImage world
        member this.SetLabelImage (value : AssetTag) world = this.Set Property? LabelImage value world
        member this.TagLabelImage = PropertyTag.make this Property? LabelImage this.GetLabelImage this.SetLabelImage

    type LabelDispatcher () =
        inherit GuiDispatcher ()

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft true
             Define? LabelImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image4" }]

        override dispatcher.Actualize (label, world) =
            World.addRenderMessage
                (RenderDescriptorsMessage
                    [LayerableDescriptor
                        { Depth = label.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = label.GetPosition world
                                  Size = label.GetSize world
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = label.GetLabelImage world
                                  Color = if label.GetEnabled world then Vector4.One else label.GetDisabledColor world }}])
                world

        override dispatcher.GetQuickSize (label, world) =
            match Metadata.tryGetTextureSizeAsVector2 (label.GetLabelImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module TextDispatcherModule =

    type Entity with
    
        member this.GetText world : string = this.Get Property? Text world
        member this.SetText (value : string) world = this.Set Property? Text value world
        member this.TagText = PropertyTag.make this Property? Text this.GetText this.SetText
        member this.GetTextFont world : AssetTag = this.Get Property? TextFont world
        member this.SetTextFont (value : AssetTag) world = this.Set Property? TextFont value world
        member this.TagTextFont = PropertyTag.make this Property? TextFont this.GetTextFont this.SetTextFont
        member this.GetTextOffset world : Vector2 = this.Get Property? TextOffset world
        member this.SetTextOffset (value : Vector2) world = this.Set Property? TextOffset value world
        member this.TagTextOffset = PropertyTag.make this Property? TextOffset this.GetTextOffset this.SetTextOffset
        member this.GetTextColor world : Vector4 = this.Get Property? TextColor world
        member this.SetTextColor (value : Vector4) world = this.Set Property? TextColor value world
        member this.TagTextColor = PropertyTag.make this Property? TextColor this.GetTextColor this.SetTextColor
        member this.GetBackgroundImage world : AssetTag = this.Get Property? BackgroundImage world
        member this.SetBackgroundImage (value : AssetTag) world = this.Set Property? BackgroundImage value world
        member this.TagBackgroundImage = PropertyTag.make this Property? BackgroundImage this.GetBackgroundImage this.SetBackgroundImage

    type TextDispatcher () =
        inherit GuiDispatcher ()

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft true
             Define? Text String.Empty
             Define? TextFont { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Font" }
             Define? TextOffset Vector2.Zero
             Define? TextColor Vector4.One
             Define? BackgroundImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image4" }]

        override dispatcher.Actualize (text, world) =
            World.addRenderMessage
                (RenderDescriptorsMessage
                    [LayerableDescriptor
                        { Depth = text.GetDepth world
                          LayeredDescriptor =
                            TextDescriptor
                                { Text = text.GetText world
                                  Position = (text.GetPosition world + text.GetTextOffset world)
                                  Size = text.GetSize world - text.GetTextOffset world
                                  ViewType = Absolute
                                  Font = text.GetTextFont world
                                  Color = text.GetTextColor world }}
                     LayerableDescriptor
                        { Depth = text.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = text.GetPosition world
                                  Size = text.GetSize world
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = text.GetBackgroundImage world
                                  Color = if text.GetEnabled world then Vector4.One else text.GetDisabledColor world }}])
                world

        override dispatcher.GetQuickSize (text, world) =
            match Metadata.tryGetTextureSizeAsVector2 (text.GetBackgroundImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module ToggleDispatcherModule =

    type Entity with
    
        member this.GetOn world : bool = this.Get Property? On world
        member this.SetOn (value : bool) world = this.Set Property? On value world
        member this.TagOn = PropertyTag.make this Property? On this.GetOn this.SetOn
        member this.GetPressed world : bool = this.Get Property? Pressed world
        member this.SetPressed (value : bool) world = this.Set Property? Pressed value world
        member this.TagPressed = PropertyTag.make this Property? Pressed this.GetPressed this.SetPressed
        member this.GetOffImage world : AssetTag = this.Get Property? OffImage world
        member this.SetOffImage (value : AssetTag) world = this.Set Property? OffImage value world
        member this.TagOffImage = PropertyTag.make this Property? OffImage this.GetOffImage this.SetOffImage
        member this.GetOnImage world : AssetTag = this.Get Property? OnImage world
        member this.SetOnImage (value : AssetTag) world = this.Set Property? OnImage value world
        member this.TagOnImage = PropertyTag.make this Property? OnImage this.GetOnImage this.SetOnImage
        member this.GetOptToggleSound world : AssetTag option = this.Get Property? OptToggleSound world
        member this.SetOptToggleSound (value : AssetTag option) world = this.Set Property? OptToggleSound value world
        member this.TagOptToggleSound = PropertyTag.make this Property? OptToggleSound this.GetOptToggleSound this.SetOptToggleSound

    type ToggleDispatcher () =
        inherit GuiDispatcher ()
        
        let handleMouseLeftDown evt world =
            let toggle = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if World.isEntitySelected toggle world then
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisible world &&
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
            if World.isEntitySelected toggle world then
                let wasPressed = toggle.GetPressed world
                let world = toggle.SetPressed false world
                let mousePositionWorld = World.mouseToWorld (toggle.GetViewType world) data.Position world
                if  toggle.GetVisible world &&
                    Math.isPointInBounds mousePositionWorld (toggle.GetBounds world) then
                    if toggle.GetEnabled world && wasPressed then
                        let world = toggle.SetOn (not ^ toggle.GetOn world) world
                        let eventAddress = if toggle.GetOn world then Events.On else Events.Off
                        let eventTrace = EventTrace.record "ToggleDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish () (eventAddress ->- toggle) eventTrace toggle world
                        let world =
                            match toggle.GetOptToggleSound world with
                            | Some toggleSound -> World.playSound 1.0f toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft false
             Define? On false
             Define? Pressed false
             Define? OffImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image" }
             Define? OnImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image2" }
             Define? OptToggleSound ^ Some { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Sound" }]

        override dispatcher.Register (toggle, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown toggle |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp toggle

        override dispatcher.Actualize (toggle, world) =
            World.addRenderMessage
                (RenderDescriptorsMessage
                    [LayerableDescriptor
                        { Depth = toggle.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = toggle.GetPosition world
                                  Size = toggle.GetSize world
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = if toggle.GetOn world || toggle.GetPressed world then toggle.GetOnImage world else toggle.GetOffImage world
                                  Color = if toggle.GetEnabled world then Vector4.One else toggle.GetDisabledColor world }}])
                world

        override dispatcher.GetQuickSize (toggle, world) =
            match Metadata.tryGetTextureSizeAsVector2 (toggle.GetOffImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module FeelerDispatcherModule =

    type Entity with
    
        member this.GetTouched world : bool = this.Get Property? Touched world
        member this.SetTouched (value : bool) world = this.Set Property? Touched value world
        member this.TagTouched = PropertyTag.make this Property? Touched this.GetTouched this.SetTouched

    type FeelerDispatcher () =
        inherit GuiDispatcher ()

        let handleMouseLeftDown evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if World.isEntitySelected feeler world then
                let mousePositionWorld = World.mouseToWorld (feeler.GetViewType world) data.Position world
                if  feeler.GetVisible world &&
                    Math.isPointInBounds mousePositionWorld (feeler.GetBounds world) then
                    if feeler.GetEnabled world then
                        let world = feeler.SetTouched true world
                        let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                        let world = World.publish data.Position (Events.Touch ->- feeler) eventTrace feeler world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        let handleMouseLeftUp evt world =
            let feeler = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if World.isEntitySelected feeler world && feeler.GetVisible world then
                if feeler.GetEnabled world then
                    let world = feeler.SetTouched false world
                    let eventTrace = EventTrace.record "FeelerDispatcher" "handleMouseLeftDown" EventTrace.empty
                    let world = World.publish data.Position (Events.Untouch ->- feeler) eventTrace feeler world
                    (Resolve, world)
                else (Resolve, world)
            else (Cascade, world)

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft false
             Define? Touched false]

        override dispatcher.Register (feeler, world) =
            world |>
                World.monitor handleMouseLeftDown Events.MouseLeftDown feeler |>
                World.monitor handleMouseLeftUp Events.MouseLeftUp feeler

        override dispatcher.GetQuickSize (_, _) =
            Vector2 64.0f

[<AutoOpen>]
module FillBarDispatcherModule =

    type Entity with
    
        member this.GetFill world : single = this.Get Property? Fill world
        member this.SetFill (value : single) world = this.Set Property? Fill value world
        member this.TagFill = PropertyTag.make this Property? Fill this.GetFill this.SetFill
        member this.GetFillInset world : single = this.Get Property? FillInset world
        member this.SetFillInset (value : single) world = this.Set Property? FillInset value world
        member this.TagFillInset = PropertyTag.make this Property? FillInset this.GetFillInset this.SetFillInset
        member this.GetFillImage world : AssetTag = this.Get Property? FillImage world
        member this.SetFillImage (value : AssetTag) world = this.Set Property? FillImage value world
        member this.TagFillImage = PropertyTag.make this Property? FillImage this.GetFillImage this.SetFillImage
        member this.GetBorderImage world : AssetTag = this.Get Property? BorderImage world
        member this.SetBorderImage (value : AssetTag) world = this.Set Property? BorderImage value world
        member this.TagBorderImage = PropertyTag.make this Property? BorderImage this.GetBorderImage this.SetBorderImage

    type FillBarDispatcher () =
        inherit GuiDispatcher ()
        
        let getFillBarSpriteDims (fillBar : Entity) world =
            let spriteSize = fillBar.GetSize world
            let spriteInset = spriteSize * fillBar.GetFillInset world * 0.5f
            let spritePosition = fillBar.GetPosition world + spriteInset
            let spriteWidth = (spriteSize.X - spriteInset.X * 2.0f) * fillBar.GetFill world
            let spriteHeight = spriteSize.Y - spriteInset.Y * 2.0f
            (spritePosition, Vector2 (spriteWidth, spriteHeight))

        static member PropertyDefinitions =
            [Define? SwallowMouseLeft true
             Define? Fill 0.0f
             Define? FillInset 0.0f
             Define? FillImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image9" }
             Define? BorderImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image10" }]

        override dispatcher.Actualize (fillBar, world) =
            let (fillBarSpritePosition, fillBarSpriteSize) = getFillBarSpriteDims fillBar world
            let fillBarColor = if fillBar.GetEnabled world then Vector4.One else fillBar.GetDisabledColor world
            World.addRenderMessage
                (RenderDescriptorsMessage
                    [LayerableDescriptor
                        { Depth = fillBar.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = fillBar.GetPosition world
                                  Size = fillBar.GetSize world
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = fillBar.GetBorderImage world
                                  Color = fillBarColor }}
                     LayerableDescriptor
                        { Depth = fillBar.GetDepth world
                          LayeredDescriptor =
                            SpriteDescriptor
                                { Position = fillBarSpritePosition
                                  Size = fillBarSpriteSize
                                  Rotation = 0.0f
                                  Offset = Vector2.Zero
                                  ViewType = Absolute
                                  OptInset = None
                                  Image = fillBar.GetFillImage world
                                  Color = fillBarColor }}])
                world

        override dispatcher.GetQuickSize (fillBar, world) =
            match Metadata.tryGetTextureSizeAsVector2 (fillBar.GetBorderImage world) (World.getAssetMetadataMap world) with
            | Some size -> size
            | None -> Constants.Engine.DefaultEntitySize

[<AutoOpen>]
module BlockDispatcherModule =

    type BlockDispatcher () =
        inherit EntityDispatcher ()

        static member PropertyDefinitions =
            [Define? BodyType Static
             Define? StaticImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

[<AutoOpen>]
module BoxDispatcherModule =

    type BoxDispatcher () =
        inherit EntityDispatcher ()

        static member PropertyDefinitions =
            [Define? StaticImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image3" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

[<AutoOpen>]
module TopViewCharacterDispatcherModule =

    type TopViewCharacterDispatcher () =
        inherit EntityDispatcher ()

        static member PropertyDefinitions =
            [Define? FixedRotation true
             Define? LinearDamping 10.0f
             Define? GravityScale 0.0f
             Define? CollisionBody ^ BodyCircle { Radius = 0.5f; Center = Vector2.Zero }
             Define? StaticImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image7" }]
        
        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

[<AutoOpen>]
module SideViewCharacterDispatcherModule =

    type SideViewCharacterDispatcher () =
        inherit EntityDispatcher ()

        static member PropertyDefinitions =
            [Define? FixedRotation true
             Define? LinearDamping 3.0f
             Define? CollisionBody ^ BodyCapsule { Height = 0.5f; Radius = 0.25f; Center = Vector2.Zero }
             Define? StaticImage { PackageName = Constants.Assets.DefaultPackageName; AssetName = "Image6" }]

        static member IntrinsicFacetNames =
            [typeof<RigidBodyFacet>.Name
             typeof<StaticSpriteFacet>.Name]

[<AutoOpen>]
module TileMapDispatcherModule =

    type Entity with
    
        member this.GetTileMapAsset world : AssetTag = this.Get Property? TileMapAsset world
        member this.SetTileMapAsset (value : AssetTag) world = this.Set Property? TileMapAsset value world
        member this.TagTileMapAsset = PropertyTag.make this Property? TileMapAsset this.GetTileMapAsset this.SetTileMapAsset
        member this.GetParallax world : single = this.Get Property? Parallax world
        member this.SetParallax (value : single) world = this.Set Property? Parallax value world
        member this.TagParallax = PropertyTag.make this Property? Parallax this.GetParallax this.SetParallax

        static member makeTileMapData (tileMapAsset : AssetTag) world =
            let metadataMap = World.getAssetMetadataMap world
            let map = __c ^ Metadata.getTileMapMetadata tileMapAsset metadataMap
            let mapSize = Vector2i (map.Width, map.Height)
            let tileSize = Vector2i (map.TileWidth, map.TileHeight)
            let tileSizeF = Vector2 (single tileSize.X, single tileSize.Y)
            let tileMapSize = Vector2i (mapSize.X * tileSize.X, mapSize.Y * tileSize.Y)
            let tileMapSizeF = Vector2 (single tileMapSize.X, single tileMapSize.Y)
            let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
            let tileSetSize =
                let optTileSetWidth = tileSet.Image.Width
                let optTileSetHeight = tileSet.Image.Height
                Vector2i (optTileSetWidth.Value / tileSize.X, optTileSetHeight.Value / tileSize.Y)
            { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

        static member makeTileData (tm : Entity) tmd (tl : TmxLayer) tileIndex world =
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
            let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
            { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

    type TileMapDispatcher () =
        inherit EntityDispatcher ()

        let getTileBodyProperties6 (tm : Entity) tmd tli td ti cexpr world =
            let tileShape = Physics.localizeCollisionBody (Vector2 (single tmd.TileSize.X, single tmd.TileSize.Y)) cexpr
            { BodyId = makeGuidFromInts tli ti
              Position =
                Vector2
                    (single ^ td.TilePosition.X + tmd.TileSize.X / 2,
                     single ^ td.TilePosition.Y + tmd.TileSize.Y / 2 + tmd.TileMapSize.Y)
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
              CollisionCategories = Physics.categorizeCollisionMask ^ tm.GetCollisionCategories world
              CollisionMask = Physics.categorizeCollisionMask ^ tm.GetCollisionMask world
              IsBullet = false
              IsSensor = false }

        let getTileBodyProperties tm tmd (tl : TmxLayer) tli ti world =
            let td = Entity.makeTileData tm tmd tl ti world
            match td.OptTileSetTile with
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
                    []
                    tileLayer.Tiles
            else []

        let registerTileLayerPhysics (tileMap : Entity) tileMapData tileLayerIndex world tileLayer =
            let bodyPropertyList = getTileLayerBodyPropertyList tileMap tileMapData tileLayerIndex tileLayer world
            World.createBodies tileMap (tileMap.GetId world) bodyPropertyList world

        let registerTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            let tileMapData = Entity.makeTileMapData tileMapAsset world
            Seq.foldi
                (registerTileLayerPhysics tileMap tileMapData)
                world
                tileMapData.Map.Layers

        let getTileLayerPhysicsIds (tileMap : Entity) tileMapData tileLayer tileLayerIndex world =
            Seq.foldi
                (fun tileIndex physicsIds _ ->
                    let tileData = Entity.makeTileData tileMap tileMapData tileLayer tileIndex world
                    match tileData.OptTileSetTile with
                    | Some tileSetTile ->
                        if tileSetTile.Properties.ContainsKey Constants.Physics.CollisionProperty then
                            let physicsId = { SourceId = tileMap.GetId world; BodyId = makeGuidFromInts tileLayerIndex tileIndex }
                            physicsId :: physicsIds
                        else physicsIds
                    | None -> physicsIds)
                []
                tileLayer.Tiles

        let unregisterTileMapPhysics (tileMap : Entity) world =
            let tileMapAsset = tileMap.GetTileMapAsset world
            let tileMapData = Entity.makeTileMapData tileMapAsset world
            Seq.foldi
                (fun tileLayerIndex world (tileLayer : TmxLayer) ->
                    if tileLayer.Properties.ContainsKey Constants.Physics.CollisionProperty then
                        let physicsIds = getTileLayerPhysicsIds tileMap tileMapData tileLayer tileLayerIndex world
                        World.destroyBodies physicsIds world
                    else world)
                world
                tileMapData.Map.Layers

        static member PropertyDefinitions =
            [Define? Omnipresent true
             Define? Friction 0.0f
             Define? Restitution 0.0f
             Define? CollisionCategories "1"
             Define? CollisionMask "@"
             Define? TileMapAsset { PackageName = Constants.Assets.DefaultPackageName; AssetName = "TileMap" }
             Define? Parallax 0.0f]

        override dispatcher.Register (tileMap, world) =
            registerTileMapPhysics tileMap world

        override dispatcher.Unregister (tileMap, world) =
            unregisterTileMapPhysics tileMap world
            
        override dispatcher.PropagatePhysics (tileMap, world) =
            world |>
                unregisterTileMapPhysics tileMap |>
                registerTileMapPhysics tileMap

        override dispatcher.Actualize (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getAssetMetadataMap world) with
            | Some (_, images, map) ->
                let layers = List.ofSeq map.Layers
                let tileSourceSize = Vector2i (map.TileWidth, map.TileHeight)
                let tileSize = Vector2 (single map.TileWidth, single map.TileHeight)
                let viewType = tileMap.GetViewType world
                List.foldi
                    (fun i world (layer : TmxLayer) ->
                        let depth = tileMap.GetDepth world + single i * 2.0f // MAGIC_VALUE: assumption
                        let parallaxTranslation =
                            match viewType with
                            | Absolute -> Vector2.Zero
                            | Relative -> tileMap.GetParallax world * depth * -World.getEyeCenter world
                        let parallaxPosition = tileMap.GetPosition world + parallaxTranslation
                        let size = Vector2 (tileSize.X * single map.Width, tileSize.Y * single map.Height)
                        if World.inView viewType (Math.makeBounds parallaxPosition size) world then
                            World.addRenderMessage
                                (RenderDescriptorsMessage
                                    [LayerableDescriptor 
                                        { Depth = depth
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
                                                  TileSetImage = List.head images }}]) // MAGIC_VALUE: for same reason as above
                                world
                        else world)
                    world
                    layers
            | None -> world

        override dispatcher.GetQuickSize (tileMap, world) =
            match Metadata.tryGetTileMapMetadata (tileMap.GetTileMapAsset world) (World.getAssetMetadataMap world) with
            | Some (_, _, map) -> Vector2 (single ^ map.Width * map.TileWidth, single ^ map.Height * map.TileHeight)
            | None -> Constants.Engine.DefaultEntitySize