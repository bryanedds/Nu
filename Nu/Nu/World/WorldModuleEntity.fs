// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open System.IO
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldModuleEntity =

    /// A reflective property getter.
    type internal PropertyGetter = Entity -> World -> Property

    /// A reflective property setter.
    type internal PropertySetter = Property -> Entity -> World -> struct (bool * World)

    /// Reflective property getters / setters.
    let internal EntityGetters = Dictionary<string, PropertyGetter> StringComparer.Ordinal
    let internal EntitySetters = Dictionary<string, PropertySetter> StringComparer.Ordinal

    /// Mutable clipboard that allows its state to persist beyond undo / redo.
    let mutable private Clipboard : obj option = None

    /// Publishing IDs.
    let internal EntityChangeCountsId = Gen.id
    let internal EntityBindingCountsId = Gen.id

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    type private KeyEquality () =
        inherit OptimizedClosures.FSharpFunc<
            KeyValuePair<Entity, UMap<Entity, EntityState>>,
            KeyValuePair<Entity, UMap<Entity, EntityState>>,
            bool> ()
        override this.Invoke _ = failwithumf ()
        override this.Invoke
            (entityStateKey : KeyValuePair<Entity, UMap<Entity, EntityState>>,
             entityStateKey2 : KeyValuePair<Entity, UMap<Entity, EntityState>>) =
            refEq entityStateKey.Key entityStateKey2.Key &&
            refEq entityStateKey.Value entityStateKey2.Value
    let private keyEquality = KeyEquality ()

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    let mutable private getFreshKeyAndValueEntity = Unchecked.defaultof<Entity>
    let mutable private getFreshKeyAndValueWorld = Unchecked.defaultof<World>
    let private getFreshKeyAndValue () =
        let mutable entityStateOpt = Unchecked.defaultof<_>
        let _ = UMap.tryGetValue (getFreshKeyAndValueEntity, getFreshKeyAndValueWorld.EntityStates, &entityStateOpt)
        KeyValuePair (KeyValuePair (getFreshKeyAndValueEntity, getFreshKeyAndValueWorld.EntityStates), entityStateOpt)
    let private getFreshKeyAndValueCached =
        getFreshKeyAndValue

    // OPTIMIZATION: cache one entity change address to reduce allocation where possible.
    let mutable changeEventNamesFree = true
    let changeEventNamesCached = [|"Change"; ""; "Event"; ""; ""; ""|]

    type World with

        // OPTIMIZATION: a ton of optimization has gone down in here...!
        static member private entityStateRefresher (entity : Entity) world =
            getFreshKeyAndValueEntity <- entity
            getFreshKeyAndValueWorld <- world
            let entityStateOpt =
                KeyedCache.getValueFast
                    keyEquality
                    getFreshKeyAndValueCached
                    (KeyValuePair (entity, world.EntityStates))
                    (World.getEntityCachedOpt world)
            getFreshKeyAndValueEntity <- Unchecked.defaultof<Entity>
            getFreshKeyAndValueWorld <- Unchecked.defaultof<World>
            match entityStateOpt :> obj with
            | null ->
                Unchecked.defaultof<EntityState>
            | _ ->
                if entityStateOpt.Imperative then entity.EntityStateOpt <- entityStateOpt
                entityStateOpt
    
        static member private entityStateFinder (entity : Entity) world =
            let entityStateOpt = entity.EntityStateOpt
            if isNull (entityStateOpt :> obj) || entityStateOpt.Invalidated
            then World.entityStateRefresher entity world
            else entityStateOpt

        static member private entityStateAdder entityState (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 3
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            let simulants =
                match world.Simulants.TryGetValue parent with
                | (true, entitiesOpt) ->
                    match entitiesOpt with
                    | Some entities ->
                        let entities = USet.add (entity :> Simulant) entities
                        UMap.add parent (Some entities) world.Simulants
                    | None ->
                        let entities = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) (entity :> Simulant)
                        UMap.add parent (Some entities) world.Simulants
                | (false, _) -> failwith ("Cannot add entity '" + scstring entity + "' to non-existent parent '" + scstring parent + "'.")
            let simulants =
                if not (UMap.containsKey (entity :> Simulant) simulants)
                then UMap.add (entity :> Simulant) None simulants
                else simulants
            let entityStates = UMap.add entity entityState world.EntityStates
            World.choose { world with Simulants = simulants; EntityStates = entityStates }

        static member private entityStateRemover (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 3
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            let simulants =
                match world.Simulants.TryGetValue parent with
                | (true, entitiesOpt) ->
                    match entitiesOpt with
                    | Some entities ->
                        let entities = USet.remove (entity :> Simulant) entities
                        if USet.isEmpty entities
                        then UMap.add parent None world.Simulants
                        else UMap.add parent (Some entities) world.Simulants
                    | None -> world.Simulants
                | (false, _) -> world.Simulants
            let simulants = UMap.remove (entity :> Simulant) simulants
            let entityStates = UMap.remove entity world.EntityStates
            World.choose { world with Simulants = simulants; EntityStates = entityStates }

        static member private entityStateSetter entityState (entity : Entity) world =
#if DEBUG
            if not (UMap.containsKey entity world.EntityStates) then
                failwith ("Cannot set the state of a non-existent entity '" + scstring entity + "'")
#endif
            let entityStates = UMap.add entity entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private addEntityState entityState (entity : Entity) world =
            World.synchronizeEntityState entityState entity world
            World.entityStateAdder entityState entity world

        static member private removeEntityState (entity : Entity) world =
            World.entityStateRemover entity world

        static member private publishEntityChange propertyName (propertyValue : obj) publishChangeBindings publishChangeEvents (entity : Entity) world =

            // publish change binding
            let world =
                if publishChangeBindings
                then World.publishChangeBinding propertyName entity world
                else world

            // publish change event
            let world =
                if publishChangeEvents then
                    let changeData = { Name = propertyName; Value = propertyValue }
                    let entityNames = Address.getNames entity.EntityAddress
                    let mutable changeEventNamesUtilized = false
                    let changeEventAddress =
                        // OPTIMIZATION: this optimization should be hit >= 90% of the time. The 10% of cases where
                        // it isn't should be acceptable.
                        if  Array.length entityNames = 3 &&
                            changeEventNamesFree then
                            changeEventNamesFree <- false
                            changeEventNamesUtilized <- true
                            changeEventNamesCached.[1] <- propertyName
                            changeEventNamesCached.[3] <- entityNames.[0]
                            changeEventNamesCached.[4] <- entityNames.[1]
                            changeEventNamesCached.[5] <- entityNames.[2]
                            rtoa<ChangeData> changeEventNamesCached
                        else rtoa<ChangeData> (Array.append [|"Change"; propertyName; "Event"|] entityNames)
                    let eventTrace = EventTrace.debug "World" "publishEntityChange" "" EventTrace.empty
                    let world = World.publishPlus changeData changeEventAddress eventTrace entity false false world
                    if changeEventNamesUtilized then changeEventNamesFree <- true
                    world
                else world

            // fin
            world

        static member inline private getEntityStateOpt entity world =
            World.entityStateFinder entity world

#if DEBUG
        static member internal getEntityState entity world =
            let entityStateOpt = World.entityStateFinder entity world
            match entityStateOpt :> obj with
            | null -> failwith ("Could not find entity '" + scstring entity + "'.")
            | _ -> entityStateOpt
#else
        static member inline internal getEntityState entity world =
            World.entityStateFinder entity world
#endif

        static member internal getEntityXtensionProperties entity world =
            let entityState = World.getEntityState entity world
            entityState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private synchronizeEntityState (entityState : EntityState) (entity : Entity) world =

            // grab address
            let entityAddress = entity.EntityAddress

            // apply publish bindings state
            match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityBindingCountsId, world) with
            | (true, entityBindingCounts) -> if UMap.containsKey entityAddress entityBindingCounts then entityState.PublishChangeBindings <- true
            | (false, _) -> ()
            
            // apply publish changes state
            match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityChangeCountsId, world) with
            | (true, entityChangeCounts) -> if UMap.containsKey entityAddress entityChangeCounts then entityState.PublishChangeEvents <- true
            | (false, _) -> ()
            
            // apply mounted state
            entityState.Mounted <- UMap.containsKey entity world.EntityMounts

        static member inline private setEntityState entityState entity world =
            World.entityStateSetter entityState entity world

        // NOTE: P1: I think we could use an in ref in updater to avoid allocation on every call...
        // OPTIMIZATION: inlined to elide updater closure allocation.
        static member inline private updateEntityStateInternal updater (entityState : EntityState) entity world =
            let entityStateOpt = updater entityState : EntityState
            match entityStateOpt :> obj with
            | null -> struct (false, world)
            | _ -> struct (true, if entityStateOpt.Imperative then world else World.setEntityState entityStateOpt entity world)

        // OPTIMIZATION: inlined to elide updater closure allocation.
        static member inline private updateEntityStateWithoutEvent updater entity world =
            let entityState = World.getEntityState entity world
            World.updateEntityStateInternal updater entityState entity world

        // OPTIMIZATION: inlined to elide updater closure allocation.
        static member private updateEntityState updater propertyName propertyValue entity world =
            let entityState = World.getEntityState entity world
            let struct (changed, world) = World.updateEntityStateInternal updater entityState entity world
            let world =
                if changed then
                    let publishChangeBindings = entityState.PublishChangeBindings
                    let publishChangeEvents = entityState.PublishChangeEvents
                    World.publishEntityChange propertyName propertyValue publishChangeBindings publishChangeEvents entity world
                else world
            struct (changed, world)

        // OPTIMIZATION: inlined to elide updater closure allocation.
        static member private updateEntityStatePlus updater propertyName propertyValue entity world =

            // cache old values
            let oldWorld = world
            let oldEntityState = World.getEntityState entity oldWorld
            let oldStatic = oldEntityState.Static
            let oldLight = oldEntityState.Light
            let oldPresence = oldEntityState.Presence
            let oldBounds = oldEntityState.Bounds

            // update entity, updating in entity also if changed
            let struct (changed, world) = World.updateEntityStateInternal updater oldEntityState entity world
            let world =
                if changed
                then World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                else world

            // publish entity change event if needed
            let world =
                if changed then
                    let publishChangeBindings = oldEntityState.PublishChangeBindings
                    let publishChangeEvents = oldEntityState.PublishChangeEvents
                    World.publishEntityChange propertyName propertyValue publishChangeBindings publishChangeEvents entity world
                else world
            struct (changed, world)

        static member private publishEntityChanges entity world =
            let entityState = World.getEntityState entity world
            let properties = World.getProperties entityState
            let publishChangeBindings = entityState.PublishChangeBindings
            let publishChangeEvents = entityState.PublishChangeEvents
            if publishChangeEvents || publishChangeBindings then
                List.fold (fun world (propertyName, _, propertyValue) ->
                    let entityState = World.getEntityState entity world
                    let publishChangeBindings = entityState.PublishChangeBindings
                    let publishChangeEvents = entityState.PublishChangeEvents
                    World.publishEntityChange propertyName propertyValue publishChangeBindings publishChangeEvents entity world)
                    world properties
            else world

        static member inline internal publishTransformEvents (oldTransform : Transform byref, newTransform : Transform byref, publishChangeBindings, publishChangeEvents, entity, world) =
            if publishChangeEvents || publishChangeBindings then
                let positionChanged = v3Neq newTransform.Position oldTransform.Position
                let scaleChanged = v3Neq newTransform.Scale oldTransform.Scale
                let offsetChanged = v3Neq newTransform.Offset oldTransform.Offset
                let anglesChanged = v3Neq newTransform.Angles oldTransform.Angles
                let sizeChanged = v3Neq newTransform.Size oldTransform.Size
                let elevationChanged = newTransform.Elevation <> oldTransform.Elevation
                let overflowChanged = newTransform.Overflow <> oldTransform.Overflow
                let centeredChanged = newTransform.Centered <> oldTransform.Centered
                let perimeterUnscaledChanged = positionChanged || offsetChanged || sizeChanged || centeredChanged
                // OPTIMIZATION: eliding data for computed change events for speed.
                let world = World.publishEntityChange Property? Transform () publishChangeBindings publishChangeEvents entity world
                let world =
                    if perimeterUnscaledChanged then
                        let world = World.publishEntityChange Property? Bounds () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? PerimeterOriented () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? Center () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? Bottom () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? Perimeter () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? PerimeterUnscaled () publishChangeBindings publishChangeEvents entity world
                        let world = if positionChanged || centeredChanged then World.publishEntityChange Property? Position newTransform.Position publishChangeBindings publishChangeEvents entity world else world
                        let world = if scaleChanged || centeredChanged then World.publishEntityChange Property? Scale newTransform.Scale publishChangeBindings publishChangeEvents entity world else world
                        let world = if offsetChanged || centeredChanged then World.publishEntityChange Property? Offset newTransform.Offset publishChangeBindings publishChangeEvents entity world else world
                        let world = if sizeChanged || centeredChanged then World.publishEntityChange Property? Size newTransform.Size publishChangeBindings publishChangeEvents entity world else world
                        let world = if centeredChanged then World.publishEntityChange Property? Centered newTransform.Centered publishChangeBindings publishChangeEvents entity world else world
                        world
                    else world
                let world =
                    if anglesChanged then
                        let world = World.publishEntityChange Property? Rotation () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? Angles () publishChangeBindings publishChangeEvents entity world
                        let world = World.publishEntityChange Property? Degrees () publishChangeBindings publishChangeEvents entity world
                        world
                    else world
                let world =
                    if elevationChanged
                    then World.publishEntityChange Property? Elevation newTransform.Elevation publishChangeBindings publishChangeEvents entity world
                    else world
                let world =
                    if overflowChanged
                    then World.publishEntityChange Property? Overflow newTransform.Overflow publishChangeBindings publishChangeEvents entity world
                    else world
                world
            else world

        static member internal getEntityExists entity world =
            notNull (World.getEntityStateOpt entity world :> obj)

        static member internal getEntityImperative entity world =
            (World.getEntityState entity world).Imperative

        static member internal setEntityImperative value entity world =
            World.updateEntityState
                (fun entityState ->
                    if value <> entityState.Imperative then
                        if value then
                            let properties = UMap.makeFromSeq StringComparer.Ordinal Imperative (Xtension.toSeq entityState.Xtension)
                            let xtension = Xtension.make true properties
                            entityState.Xtension <- xtension
                            entityState.Imperative <- true
                            entityState
                        else
                            let properties = UMap.makeFromSeq StringComparer.Ordinal Functional (Xtension.toSeq entityState.Xtension)
                            let xtension = Xtension.make false properties
                            let entityState = EntityState.diverge entityState
                            entityState.Xtension <- xtension
                            entityState.Imperative <- false
                            entityState
                    else Unchecked.defaultof<_>)
                Property? Imperative value entity world

        static member internal getEntityModelProperty entity world =
            let entityState = World.getEntityState entity world
            entityState.Model

        static member internal getEntityModel<'a> entity world =
            let entityState = World.getEntityState entity world
            entityState.Model.DesignerValue :?> 'a

        static member internal setEntityModelProperty (value : DesignerProperty) entity world =
            World.updateEntityState
                (fun entityState ->
                    if value.DesignerValue =/= entityState.Model.DesignerValue then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.Model <- { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }
                        entityState
                    else Unchecked.defaultof<_>)
                Property? Model value.DesignerValue entity world

        static member internal setEntityModel<'a> (value : 'a) entity world =
            World.updateEntityState
                (fun entityState ->
                    let valueObj = value :> obj
                    if valueObj =/= entityState.Model.DesignerValue then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = valueObj }
                        entityState
                    else Unchecked.defaultof<_>)
                Property? Model value entity world
                
        static member internal getEntityScriptFrame entity world =
            let entityState = World.getEntityState entity world
            match entityState.ScriptFrameOpt with
            | null ->
                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                let scriptFrame = Scripting.DeclarationFrame StringComparer.Ordinal
                entityState.ScriptFrameOpt <- scriptFrame
                scriptFrame
            | scriptFrame -> scriptFrame

        static member internal setEntityScriptFrame value entity world =
            World.updateEntityState (fun entityState ->
                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                if value <> entityState.ScriptFrameOpt then
                    entityState.ScriptFrameOpt <- value
                    entityState
                else Unchecked.defaultof<_>)
                Property? ScriptFrame value entity world

        // NOTE: wouldn't macros be nice?
        static member internal getEntityDispatcher entity world = (World.getEntityState entity world).Dispatcher
        static member internal getEntityFacets entity world = (World.getEntityState entity world).Facets
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal getEntityPositionLocal entity world = (World.getEntityState entity world).PositionLocal
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal getEntityRotationLocal entity world = (World.getEntityState entity world).RotationLocal
        static member internal getEntityScale entity world = (World.getEntityState entity world).Scale
        static member internal getEntityScaleLocal entity world = (World.getEntityState entity world).ScaleLocal
        static member internal getEntityOffset entity world = (World.getEntityState entity world).Offset
        static member internal getEntityAngles entity world = (World.getEntityState entity world).Angles
        static member internal getEntityAnglesLocal entity world = (World.getEntityState entity world).AnglesLocal
        static member internal getEntityDegrees entity world = (World.getEntityState entity world).Degrees
        static member internal getEntityDegreesLocal entity world = Math.radiansToDegrees3d (World.getEntityState entity world).AnglesLocal
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal getEntityElevation entity world = (World.getEntityState entity world).Elevation
        static member internal getEntityElevationLocal entity world = (World.getEntityState entity world).ElevationLocal
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Transform.Overflow
        static member internal getEntityPresence entity world = (World.getEntityState entity world).Presence
        static member internal getEntityAbsolute entity world = (World.getEntityState entity world).Absolute
        static member internal getEntityPublishChangeBindings entity world = (World.getEntityState entity world).PublishChangeBindings
        static member internal getEntityPublishChangeEvents entity world = (World.getEntityState entity world).PublishChangeEvents
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal getEntityEnabledLocal entity world = (World.getEntityState entity world).EnabledLocal
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal getEntityVisibleLocal entity world = (World.getEntityState entity world).VisibleLocal
        static member internal getEntityAlwaysUpdate entity world = (World.getEntityState entity world).AlwaysUpdate
        static member internal getEntityPublishUpdates entity world = (World.getEntityState entity world).PublishUpdates
        static member internal getEntityPublishPostUpdates entity world = (World.getEntityState entity world).PublishPostUpdates
        static member internal getEntityPublishActualizes entity world = (World.getEntityState entity world).PublishActualizes
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal getEntityIgnorePropertyBindings entity world = (World.getEntityState entity world).IgnorePropertyBindings
        static member internal getEntityMounted entity world = (World.getEntityState entity world).Mounted
        static member internal getEntityIs2d entity world = (World.getEntityState entity world).Is2d
        static member internal getEntityCentered entity world = (World.getEntityState entity world).Centered
        static member internal getEntityStatic entity world = (World.getEntityState entity world).Static
        static member internal getEntityLight entity world = (World.getEntityState entity world).Light
        static member internal getEntityPhysical entity world = (World.getEntityState entity world).Physical
        static member internal getEntityOptimized entity world = (World.getEntityState entity world).Optimized
        static member internal getEntityShouldMutate entity world = (World.getEntityState entity world).Imperative
        static member internal getEntityDestroying (entity : Entity) world = List.exists ((=) (entity :> Simulant)) world.WorldExtension.DestructionListRev
        static member internal getEntityMountOpt entity world = (World.getEntityState entity world).MountOpt
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityOverlayNameOpt entity world = (World.getEntityState entity world).OverlayNameOpt
        static member internal getEntityOrder entity world = (World.getEntityState entity world).Order
        static member internal getEntityId entity world = (World.getEntityState entity world).IdRef.Value
        static member internal getEntitySurnames entity world = (World.getEntityState entity world).Surnames
        static member internal getEntityName entity world = (World.getEntityState entity world).Surnames |> Array.last
        static member internal setEntityPublishChangeEvents value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishChangeEvents then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.PublishChangeEvents <- value; entityState) else Unchecked.defaultof<_>) Property? PublishChangeEvents value entity world
        static member internal setEntityPublishChangeBindings value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishChangeBindings then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.PublishChangeBindings <- value; entityState) else Unchecked.defaultof<_>) Property? PublishChangeBindings value entity world
        static member internal setEntityAlwaysUpdate value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.AlwaysUpdate then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.AlwaysUpdate <- value; entityState) else Unchecked.defaultof<_>) Property? AlwaysUpdate value entity world
        static member internal setEntityPublishUpdates value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishUpdates then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.PublishUpdates <- value; entityState) else Unchecked.defaultof<_>) Property? PublishUpdates value entity world
        static member internal setEntityPublishPostUpdates value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishPostUpdates then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.PublishPostUpdates <- value; entityState) else Unchecked.defaultof<_>) Property? PublishPostUpdates value entity world
        static member internal setEntityPublishActualizes value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishActualizes then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.PublishActualizes <- value; entityState) else Unchecked.defaultof<_>) Property? PublishActualizes value entity world
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> if value <> entityState.Persistent then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.Persistent <- value; entityState) else Unchecked.defaultof<_>) Property? Persistent value entity world
        static member internal setEntityIgnorePropertyBindings value entity world = World.updateEntityState (fun entityState -> if value <> entityState.IgnorePropertyBindings then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.IgnorePropertyBindings <- value; entityState) else Unchecked.defaultof<_>) Property? IgnorePropertyBindings value entity world
        static member internal setEntityMounted value entity world = World.updateEntityState (fun entityState -> if value <> entityState.Mounted then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.Mounted <- value; entityState) else Unchecked.defaultof<_>) Property? Mounted value entity world
        static member internal setEntityStatic value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.Static then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.Static <- value; entityState) else Unchecked.defaultof<_>) Property? Static value entity world
        static member internal setEntityLight value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.Light then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.Light <- value; entityState) else Unchecked.defaultof<_>) Property? Light value entity world
        static member internal setEntityOrder value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.Order then (let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState in entityState.Order <- value; entityState) else Unchecked.defaultof<_>) Property? Order value entity world

        static member internal setEntityPresence (value : Presence) entity world =
            World.updateEntityStatePlus (fun entityState ->
                if presenceNeq value entityState.Presence then
                    let omnipresent = value.OmnipresentType
                    if omnipresent || not entityState.Absolute then // a transform that is Absolute must remain Omnipresent
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.Presence <- value
                        entityState
                    else entityState
                else Unchecked.defaultof<_>)
                Property? Omnipresent value entity world

        static member internal setEntityAbsolute value entity world =
            World.updateEntityStatePlus (fun entityState ->
                if value <> entityState.Absolute then
                    let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                    entityState.Absolute <- value
                    if value then entityState.Presence <- Omnipresent // setting a transform to Absolute requires that it also be Omnipresent
                    entityState
                else Unchecked.defaultof<_>)
                Property? Absolute value entity world

        static member inline internal getEntityRotationMatrix entity world =
            (World.getEntityState entity world).Transform.RotationMatrix

        static member inline internal getEntityAffineMatrix entity world =
            (World.getEntityState entity world).Transform.AffineMatrix
        
        static member internal getEntityAffineMatrixLocal entity world =
            let entityState = World.getEntityState entity world
            Matrix4x4.CreateFromTrs (entityState.PositionLocal, entityState.RotationLocal, entityState.ScaleLocal)

        static member internal getEntityMounters entity world =
            match world.EntityMounts.TryGetValue entity with
            | (true, mounters) -> Seq.filter (flip World.getEntityExists world) mounters |> SegmentedList.ofSeq |> seq
            | (false, _) -> Seq.empty

        static member internal traverseEntityMounters effect entity (world : World) =
            let mounters = World.getEntityMounters entity world
            Seq.fold (fun world mounter -> effect entity mounter world) world mounters

        static member internal getEntityEntities (entity : Entity) world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue (entity :> Simulant) with
            | (true, entitiesOpt) ->
                match entitiesOpt with
                | Some entities -> entities |> Seq.map cast<Entity> |> seq
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        static member internal traverseEntityEntities effect entity (world : World) =
            let mounters = World.getEntityEntities entity world
            Seq.fold (fun world mounter -> effect entity mounter world) world mounters

        static member internal addEntityToMounts mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some newMount ->
                match world.EntityMounts.TryGetValue newMount with
                | (true, mounters) ->
                    let mounters = USet.add entity mounters
                    let world = { world with EntityMounts = UMap.add newMount mounters world.EntityMounts }
                    world
                | (false, _) ->
                    let mounters = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) entity
                    let world = World.choose { world with EntityMounts = UMap.add newMount mounters world.EntityMounts }
                    let world = if World.getEntityExists newMount world then World.setEntityMounted true newMount world |> snd' else world
                    world
            | None -> world

        static member internal removeEntityFromMounts mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some oldMount ->
                match world.EntityMounts.TryGetValue oldMount with
                | (true, mounters) ->
                    let mounters = USet.remove entity mounters
                    if USet.isEmpty mounters then
                        let world = World.choose { world with EntityMounts = UMap.remove oldMount world.EntityMounts }
                        let world = if World.getEntityExists oldMount world then World.setEntityMounted false oldMount world |> snd' else world
                        world
                    else World.choose { world with EntityMounts = UMap.add oldMount mounters world.EntityMounts }
                | (false, _) -> world
            | None -> world

        static member internal propagateEntityAffineMatrix3 mount mounter world =
            let mounterState = World.getEntityState mounter world
            if World.isHalted world || not mounterState.Physical then
                let affineMatrixWorld = World.getEntityAffineMatrix mount world
                let affineMatrixLocal = World.getEntityAffineMatrixLocal mounter world
                let affineMatrix = affineMatrixLocal * affineMatrixWorld
                let position = affineMatrix.Translation
                let rotation = affineMatrix.Rotation ()
                let scale = affineMatrix.Scale ()
                let mutable transform = mounterState.Transform
                transform.Position <- position
                transform.Rotation <- rotation
                transform.Scale <- scale
                let world = World.setEntityTransformByRef (&transform, mounterState, mounter, world) |> snd'
                World.traverseEntityMounters World.propagateEntityAffineMatrix3 mounter world
            else world

        static member internal propagateEntityProperties3 mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some newMount when World.getEntityExists newMount world ->
                let world = World.propagateEntityAffineMatrix3 newMount entity world
                let world = World.propagateEntityElevation3 newMount entity world
                let world = World.propagateEntityEnabled3 newMount entity world
                let world = World.propagateEntityVisible3 newMount entity world
                world
            | _ -> world

        static member internal propagateEntityAffineMatrix entity world =
            World.traverseEntityMounters World.propagateEntityAffineMatrix3 entity world

        static member internal setEntityMountOpt value entity world =
            let newMountOpt = value
            let oldMountOpt = World.getEntityMountOpt entity world
            let changed = newMountOpt <> oldMountOpt
            let world =
                if changed then

                    // update property
                    let struct (_, world) =
                        World.updateEntityStateWithoutEvent (fun entityState ->
                            if newMountOpt <> entityState.MountOpt then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.MountOpt <- newMountOpt
                                entityState
                            else Unchecked.defaultof<_>)
                            entity world

                    // update mount hierarchy
                    let world = World.removeEntityFromMounts oldMountOpt entity world
                    let world = World.addEntityToMounts newMountOpt entity world

                    // propagate properties from mount
                    let world = World.propagateEntityProperties3 newMountOpt entity world

                    // publish change event unconditionally
                    let world = World.publishEntityChange Property? MountOpt newMountOpt true true entity world

                    // publish life cycle event unconditionally
                    let eventTrace = EventTrace.debug "World" "lifeCycle" "" EventTrace.empty
                    let world = World.publish (MountOptChangeData (oldMountOpt, newMountOpt, entity)) (Events.LifeCycle (nameof Entity)) eventTrace entity world
                    world

                else world
            struct (changed, world)

        static member inline internal getEntityTransform entity world =
            (World.getEntityState entity world).Transform

        static member internal setEntityTransformByRefWithoutEvent (valueInRef : Transform inref, entityState : EntityState, entity : Entity, world) =
            let oldWorld = world
            let oldEntityState = entityState
            let oldStatic = oldEntityState.Static
            let oldLight = oldEntityState.Light
            let oldPresence = oldEntityState.Presence
            let oldBounds = oldEntityState.Bounds
            let struct (changed, world) =
                let (value : Transform) = valueInRef // NOTE: unfortunately, a Transform copy is required to pass the lambda barrier, even if this is inlined...
                World.updateEntityStateWithoutEvent
                    (fun entityState ->
                        if not (Transform.equalsByRef (&value, &entityState.Transform))
                        then EntityState.setTransformByRef (&value, entityState)
                        else Unchecked.defaultof<_>)
                    entity world
            if changed
            then World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
            else world

        static member internal setEntityTransformByRef (value : Transform byref, entityState : EntityState, entity : Entity, world) =
            let oldWorld = world
            let oldEntityState = entityState
            let mutable oldTransform = oldEntityState.Transform
            let oldStatic = oldTransform.Static
            let oldLight = oldTransform.Light
            let oldPresence = oldTransform.Presence
            let oldBounds = oldTransform.Bounds
            let struct (changed, world) =
                let (value : Transform) = value // NOTE: unfortunately, a Transform copy is required to pass the lambda barrier.
                let struct (changed, world) =
                    World.updateEntityStateWithoutEvent
                        (fun entityState ->
                            if not (Transform.equalsByRef (&value, &entityState.Transform))
                            then EntityState.setTransformByRef (&value, entityState)
                            else Unchecked.defaultof<_>)
                        entity world
                let world = World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                struct (changed, world)
            if changed then
                let publishChangeBindings = oldEntityState.PublishChangeBindings
                let publishChangeEvents = oldEntityState.PublishChangeEvents
                let world = World.publishTransformEvents (&oldTransform, &value, publishChangeBindings, publishChangeEvents, entity, world)
                struct (changed, world)
            else struct (changed, world)

        static member internal setEntityPosition value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Position then
                if entityState.Optimized then
                    entityState.Position <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Position <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityPositionLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.PositionLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.PositionLocal <- value
                    let position =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            Vector3.Transform (value, affineMatrix)
                        | _ -> value
                    entityState.Position <- position
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place.

                    // update PositionLocal property
                    let struct (_, world) =
                        World.updateEntityState (fun entityState ->
                            if v3Neq value entityState.PositionLocal then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.PositionLocal <- value
                                entityState
                            else Unchecked.defaultof<_>)
                            Property? PositionLocal value entity world

                    // make sure we've got the latest entity state
                    let entityState = World.getEntityState entity world

                    // compute position
                    let position =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            Vector3.Transform (value, affineMatrix)
                        | _ -> value

                    // update property
                    let world = World.setEntityPosition position entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityRotation value entity world =
            let entityState = World.getEntityState entity world
            if quatNeq value entityState.Rotation then
                if entityState.Optimized then
                    entityState.Rotation <- value
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Rotation <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityRotationLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if quatNeq value entityState.RotationLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                let anglesLocal = value.RollPitchYaw
                if entityState.Optimized then
                    entityState.RotationLocal <- value
                    entityState.AnglesLocal <- anglesLocal
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationLocal = World.getEntityRotation mount world
                            rotationLocal * value
                        | _ -> value
                    entityState.Rotation <- rotation
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place.

                    // update RotationLocal property
                    let struct (_, world) =
                        World.updateEntityState (fun entityState ->
                            if quatNeq value entityState.RotationLocal then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.RotationLocal <- value
                                entityState.AnglesLocal <- anglesLocal
                                entityState
                            else Unchecked.defaultof<_>)
                            Property? RotationLocal value entity world

                    // make sure we've got the latest entity state
                    let entityState = World.getEntityState entity world

                    // send additional events if needed
                    let world =
                        let publishChangeBindings = entityState.PublishChangeBindings
                        let publishChangeEvents = entityState.PublishChangeEvents
                        if publishChangeBindings || publishChangeEvents then
                            let world = World.publishEntityChange Property? AnglesLocal anglesLocal publishChangeBindings publishChangeEvents entity world
                            let world = World.publishEntityChange Property? DegreesLocal (Math.radiansToDegrees3d anglesLocal) publishChangeBindings publishChangeEvents entity world
                            world
                        else world

                    // make sure we've got the latest entity state again
                    let entityState = World.getEntityState entity world

                    // compute rotation
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * value
                        | _ -> value

                    // update property
                    let world = World.setEntityRotation rotation entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityScale value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Scale then
                if entityState.Optimized then
                    entityState.Scale <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Scale <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityScaleLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.ScaleLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ScaleLocal <- value
                    let scale =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value
                    entityState.Scale <- scale
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place.

                    // update ScaleLocal property
                    let struct (_, world) =
                        World.updateEntityState (fun entityState ->
                            if v3Neq value entityState.ScaleLocal then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.ScaleLocal <- value
                                entityState
                            else Unchecked.defaultof<_>)
                            Property? ScaleLocal value entity world

                    // compute scale
                    let scale =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value

                    // update property
                    let world = World.setEntityScale scale entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityOffset value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Offset then
                if entityState.Optimized then
                    entityState.Offset <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Offset <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntitySize value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Size then
                if entityState.Optimized then
                    entityState.Size <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Size <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityAngles value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Angles then
                if entityState.Optimized then
                    entityState.Angles <- value
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Angles <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityAnglesLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.AnglesLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                let rotationLocal = value.RollPitchYaw
                if entityState.Optimized then
                    entityState.RotationLocal <- rotationLocal
                    entityState.AnglesLocal <- value
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * rotationLocal
                        | _ -> rotationLocal
                    entityState.Rotation <- rotation
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place.

                    // update AnglesLocal property
                    let struct (_, world) =
                        World.updateEntityState (fun entityState ->
                            if v3Neq value entityState.AnglesLocal then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.RotationLocal <- rotationLocal
                                entityState.AnglesLocal <- value
                                entityState
                            else Unchecked.defaultof<_>)
                            Property? RotationLocal value entity world

                    // make sure we've got the latest entity state
                    let entityState = World.getEntityState entity world

                    // send additional events if needed
                    let world =
                        let publishChangeBindings = entityState.PublishChangeBindings
                        let publishChangeEvents = entityState.PublishChangeEvents
                        if publishChangeBindings || publishChangeEvents then
                            let world = World.publishEntityChange Property? AnglesLocal value publishChangeBindings publishChangeEvents entity world
                            let world = World.publishEntityChange Property? DegreesLocal (Math.radiansToDegrees3d value) publishChangeBindings publishChangeEvents entity world
                            world
                        else world

                    // make sure we've got the latest entity state again
                    let entityState = World.getEntityState entity world

                    // update rotation property if mounting, otherwise update angles property
                    match Option.bind (tryResolve entity) entityState.MountOpt with
                    | Some mount when World.getEntityExists mount world ->
                        let rotationMount = World.getEntityRotation mount world
                        let rotation = rotationMount * rotationLocal
                        let world = World.setEntityRotation rotation entity world |> snd'
                        struct (true, world)
                    | _ ->
                        let world = World.setEntityAngles value entity world |> snd'
                        struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityDegrees value entity world =
            World.setEntityAngles (Math.degreesToRadians3d value) entity world

        static member internal setEntityDegreesLocal value entity world =
            World.setEntityAnglesLocal (Math.degreesToRadians3d value) entity world

        static member internal propagateEntityElevation3 mount mounter world =
            let elevationMount = World.getEntityElevation mount world
            let elevationLocal = World.getEntityElevationLocal mounter world
            let elevation = elevationMount + elevationLocal
            let world = World.setEntityElevation elevation mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityElevation3 mounter world

        static member internal propagateEntityElevation entity world =
            World.traverseEntityMounters World.propagateEntityElevation3 entity world
        
        static member internal setEntityElevation value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Elevation then
                if entityState.Optimized then
                    entityState.Transform.Elevation <- value
                    let world = if entityState.Mounted then World.propagateEntityElevation entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Elevation <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world = if World.getEntityMounted entity world then World.propagateEntityElevation entity world else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityElevationLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if value <> entityState.ElevationLocal then

                // OPTIMIZATION: do elevation updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ElevationLocal <- value
                    let elevationMount =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f
                    entityState.Transform.Elevation <- elevationMount + value
                    let world = if entityState.Mounted then World.propagateEntityElevation entity world else world
                    struct (true, world)

                else // do elevation updates and propagation out-of-place.

                    // update ElevationLocal property
                    let struct (_, world) =
                        World.updateEntityState (fun entityState ->
                            if value <> entityState.ElevationLocal then
                                let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                                entityState.ElevationLocal <- value
                                entityState
                            else Unchecked.defaultof<_>)
                            Property? ElevationLocal value entity world

                    // compute mount elevation
                    let elevationMount =
                        match Option.bind (tryResolve entity) (World.getEntityMountOpt entity world) with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f

                    // update property
                    let world = World.setEntityElevation (elevationMount + value) entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal propagateEntityEnabled3 mount mounter world =
            let enabledMount = World.getEntityEnabled mount world
            let enabledLocal = World.getEntityEnabledLocal mounter world
            let enabled = enabledMount && enabledLocal
            let world = World.setEntityEnabled enabled mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityEnabled3 mounter world

        static member internal propagateEntityEnabled entity world =
            World.traverseEntityMounters World.propagateEntityEnabled3 entity world
            
        static member internal setEntityEnabled value entity world =
            let struct (changed, world) =
                World.updateEntityState (fun entityState ->
                    if value <> entityState.Enabled then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.Enabled <- value
                        entityState
                    else Unchecked.defaultof<_>)
                    Property? Enabled value entity world
            let world = if changed && World.getEntityMounted entity world then World.propagateEntityEnabled entity world else world
            struct (true, world)

        static member internal setEntityEnabledLocal value entity world =
            let struct (changed, world) =
                World.updateEntityState (fun entityState ->
                    if value <> entityState.EnabledLocal then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.EnabledLocal <- value
                        entityState
                    else Unchecked.defaultof<_>)
                    Property? EnabledLocal value entity world
            let world =
                if changed then
                    let mountOpt = Option.bind (tryResolve entity) (World.getEntityMountOpt entity world)
                    let enabledMount =
                        match mountOpt with
                        | Some mount when World.getEntityExists mount world -> World.getEntityEnabled mount world
                        | _ -> true
                    let enabled = enabledMount && value
                    World.setEntityEnabled enabled entity world |> snd'
                else world
            struct (changed, world)

        static member internal propagateEntityVisible3 mount mounter world =
            let visibleMount = World.getEntityVisible mount world
            let visibleLocal = World.getEntityVisibleLocal mounter world
            let visible = visibleMount && visibleLocal
            let world = World.setEntityVisible visible mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityVisible3 mounter world

        static member internal propagateEntityVisible entity world =
            World.traverseEntityMounters World.propagateEntityVisible3 entity world
            
        static member internal setEntityVisible value entity world =
            let struct (changed, world) =
                World.updateEntityState (fun entityState ->
                    if value <> entityState.Visible then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.Visible <- value
                        entityState
                    else Unchecked.defaultof<_>)
                    Property? Visible value entity world
            let world = if changed && World.getEntityMounted entity world then World.propagateEntityVisible entity world else world
            struct (true, world)

        static member internal setEntityVisibleLocal value entity world =
            let struct (changed, world) =
                World.updateEntityState (fun entityState ->
                    if value <> entityState.VisibleLocal then
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.VisibleLocal <- value
                        entityState
                    else Unchecked.defaultof<_>)
                    Property? VisibleLocal value entity world
            let world =
                if changed then
                    let mountOpt = Option.bind (tryResolve entity) (World.getEntityMountOpt entity world)
                    let visibleMount =
                        match mountOpt with
                        | Some mount when World.getEntityExists mount world -> World.getEntityVisible mount world
                        | _ -> true
                    let visible = visibleMount && value
                    World.setEntityVisible visible entity world |> snd'
                else world
            struct (changed, world)
        
        static member internal setEntityOverflow value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Overflow then
                if entityState.Optimized then
                    entityState.Transform.Overflow <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Overflow <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityCentered value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Centered then
                if entityState.Optimized then
                    entityState.Centered <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Centered <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeterUnscaled entity world =
            (World.getEntityState entity world).Transform.PerimeterUnscaled

        static member internal setEntityPerimeterUnscaled value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.PerimeterUnscaled then
                if entityState.Optimized then
                    entityState.PerimeterUnscaled <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.PerimeterUnscaled <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeter entity world =
            (World.getEntityState entity world).Transform.Perimeter

        static member internal setEntityPerimeter value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.Perimeter then
                if entityState.Optimized then
                    entityState.Perimeter <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Perimeter <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityCenter entity world =
            (World.getEntityState entity world).Transform.Center

        static member internal setEntityCenter value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Center then
                if entityState.Optimized then
                    entityState.Center <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Center <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityBottom entity world =
            (World.getEntityState entity world).Transform.Bottom

        static member internal setEntityBottom value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Bottom then
                if entityState.Optimized then
                    entityState.Bottom <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Bottom <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeterOriented entity world =
            (World.getEntityState entity world).Transform.PerimeterOriented

        static member internal getEntityBounds entity world =
            (World.getEntityState entity world).Transform.Bounds

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ("Invalid facet name '" + facetName + "'.")

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun (propertyDefinition : PropertyDefinition) ->
                        let mutable property = Unchecked.defaultof<_>
                        match Xtension.tryGetProperty (propertyDefinition.PropertyName, entityState.Xtension, &property) with
                        | true -> property.PropertyType <> propertyDefinition.PropertyType
                        | false -> false)
                    facetPropertyDefinitions
            else false

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let propertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyNameCounts propertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name (Reflection.getPropertyDefinitions facetType)
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyNameCounts facetPropertyDefinitions

            // compute the difference of the counts
            let finalPropertyDefinitionNameCounts =
                Map.map
                    (fun propertyName propertyCount ->
                        match Map.tryFind propertyName facetPropertyDefinitionNameCounts with
                        | Some facetPropertyCount -> propertyCount - facetPropertyCount
                        | None -> propertyCount)
                    propertyDefinitionNameCounts

            // build a set of all property names where the final counts are negative
            Map.fold
                (fun propertyNamesToDetach propertyName propertyCount ->
                    if propertyCount = 0
                    then Set.add propertyName propertyNamesToDetach
                    else propertyNamesToDetach)
                Set.empty
                finalPropertyDefinitionNameCounts

        /// Get an entity's intrinsic facet names.
        static member getEntityIntrinsicFacetNames entityState =
            let intrinsicFacetNames = entityState.Dispatcher |> getType |> Reflection.getIntrinsicFacetNames
            Set.ofList intrinsicFacetNames

        /// Get an entity's facet names via reflection.
        static member getEntityFacetNamesReflectively (entityState : EntityState) =
            let facetNames = Array.map getTypeName entityState.Facets
            Set.ofArray facetNames

        static member private tryRemoveFacet facetName (entityState : EntityState) entityOpt world =
            match Array.tryFind (fun facet -> getTypeName facet = facetName) entityState.Facets with
            | Some facet ->
                let (entityState, world) =
                    match entityOpt with
                    | Some entity ->
                        let world = World.setEntityState entityState entity world
                        let world = facet.Register (entity, world)
                        let world =
                            if WorldModule.isSelected entity world
                            then facet.UnregisterPhysics (entity, world)
                            else world
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.diverge propertyNames entityState
                let entityState =
                    let facetNames = Set.remove facetName entityState.FacetNames
                    let facets = Array.remove ((=) facet) entityState.Facets
                    let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                    entityState.FacetNames <- facetNames
                    entityState.Facets <- facets
                    entityState
                match entityOpt with
                | Some entity ->
                    let oldWorld = world
                    let oldEntityState = entityState
                    let oldStatic = oldEntityState.Static
                    let oldLight = oldEntityState.Light
                    let oldPresence = oldEntityState.Presence
                    let oldBounds = oldEntityState.Bounds
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ("Failure to remove facet '" + facetName + "' from entity.")

        static member private tryAddFacet facetName (entityState : EntityState) entityOpt world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let entityState =
                        let facetNames = Set.add facetName entityState.FacetNames
                        let facets = Array.add facet entityState.Facets
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.FacetNames <- facetNames
                        entityState.Facets <- facets
                        entityState
                    let entityState = Reflection.attachProperties EntityState.diverge facet entityState world
                    match entityOpt with
                    | Some entity ->
                        let oldWorld = world
                        let oldEntityState = entityState
                        let oldStatic = oldEntityState.Static
                        let oldLight = oldEntityState.Light
                        let oldPresence = oldEntityState.Presence
                        let oldBounds = oldEntityState.Bounds
                        let world = World.setEntityState entityState entity world
                        let world = World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                        let world = facet.Register (entity, world)
                        let world =
                            if WorldModule.isSelected entity world
                            then facet.RegisterPhysics (entity, world)
                            else world
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ("Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Surnames + "'.")
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState entityOpt world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState entityOpt world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member private updateEntityPublishEventFlag setFlag entity eventAddress world =
            let publishEvent =
                match UMap.tryFind eventAddress (World.getSubscriptions world) with
                | Some subscriptions ->
                    if OMap.isEmpty subscriptions
                    then failwithumf () // NOTE: event system is defined to clean up all empty subscription entries
                    else true
                | None -> false
            if World.getEntityExists entity world
            then setFlag publishEvent entity world
            else struct (false, world)

        static member internal trySetFacetNames facetNames entityState entityOpt world =
            let intrinsicFacetNames = World.getEntityIntrinsicFacetNames entityState
            let extrinsicFacetNames = Set.fold (flip Set.remove) facetNames intrinsicFacetNames
            let facetNamesToRemove = Set.difference entityState.FacetNames extrinsicFacetNames
            let facetNamesToAdd = Set.difference extrinsicFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState entityOpt world =
            let facetNamesToRemove = Set.difference oldFacetNames entityState.FacetNames
            let facetNamesToAdd = Set.difference entityState.FacetNames oldFacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.diverge entityDispatchers facets entityState.Dispatcher entityState world

        static member internal applyEntityOverlay oldOverlayer overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OverlayNameOpt with
            | Some overlayName ->
                let oldFacetNames = entityState.FacetNames
                let entityState = Overlayer.applyOverlayToFacetNames EntityState.diverge overlayName overlayName entityState oldOverlayer overlayer
                match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                | Right (entityState, world) ->
                    let oldWorld = world
                    let oldEntityState = entityState
                    let oldStatic = oldEntityState.Static
                    let oldLight = oldEntityState.Light
                    let oldPresence = oldEntityState.Presence
                    let oldBounds = oldEntityState.Bounds
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.diverge overlayName overlayName facetNames entityState oldOverlayer overlayer
                    let world = World.setEntityState entityState entity world
                    World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                | Left error -> Log.info ("There was an issue in applying a reloaded overlay: " + error); world
            | None -> world

        static member internal tryGetEntityXtensionProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ ->
                match EntityState.tryGetProperty (propertyName, entityStateOpt, &property) with
                | true ->
                    if EntityState.containsRuntimeProperties entityStateOpt then
                        match property.PropertyValue with
                        | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                        | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (entity :> obj) (world :> obj) }; true
                        | _ -> true
                    else true
                | false -> false

        static member internal tryGetEntityProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ ->
                match EntityGetters.TryGetValue propertyName with
                | (true, getter) -> property <- getter entity world; true
                | (false, _) ->
                    match EntityState.tryGetProperty (propertyName, entityStateOpt, &property) with
                    | true ->
                        if EntityState.containsRuntimeProperties entityStateOpt then
                            match property.PropertyValue with
                            | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                            | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (entity :> obj) (world :> obj) }; true
                            | _ -> true
                        else true
                    | false -> false

        static member internal getEntityXtensionProperty propertyName entity world =
            let mutable property = Unchecked.defaultof<_>
            match World.tryGetEntityXtensionProperty (propertyName, entity, world, &property) with
            | true -> property
            | false -> failwithf "Could not find xtension property '%s'." propertyName

        static member internal getEntityXtensionValue<'a> propertyName entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> failwithf "Could not find entity '%s'." (scstring entity)
            | _ ->
                let property = EntityState.getProperty propertyName entityStateOpt
                match property.PropertyValue with
                | :? DesignerProperty as dp -> dp.DesignerValue :?> 'a
                | :? ComputedProperty as cp -> cp.ComputedGet (entity :> obj) (world :> obj) :?> 'a
                | _ -> property.PropertyValue :?> 'a

        static member internal getEntityProperty propertyName entity world =
            let mutable property = Unchecked.defaultof<_>
            match World.tryGetEntityProperty (propertyName, entity, world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityXtensionPropertyWithoutEvent propertyName property entityState entity world =
            let mutable propertyOld = Unchecked.defaultof<_>
            let struct (success, changed, world) =
                match EntityState.tryGetProperty (propertyName, entityState, &propertyOld) with
                | true ->
                    if EntityState.containsRuntimeProperties entityState then
                        match propertyOld.PropertyValue with
                        | :? DesignerProperty as dp ->
                            if property.PropertyValue =/= dp.DesignerValue then
                                let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                                match EntityState.trySetProperty propertyName property entityState with
                                | struct (true, entityState) -> struct (true, true, if entityState.Imperative then world else World.setEntityState entityState entity world)
                                | struct (false, _) -> struct (false, false, world)
                            else (true, false, world)
                        | :? ComputedProperty as cp ->
                            match cp.ComputedSetOpt with
                            | Some computedSet ->
                                if property.PropertyValue =/= cp.ComputedGet (box entity) (box world)
                                then struct (true, true, computedSet property.PropertyValue entity world :?> World)
                                else struct (true, false, world)
                            | None -> struct (false, false, world)
                        | _ ->
                            if property.PropertyValue =/= propertyOld.PropertyValue then
                                if entityState.Imperative then
                                    // OPTIMIZATION: special-case for imperative
                                    propertyOld.PropertyValue <- property.PropertyValue
                                    struct (true, true, world)
                                else
                                    match EntityState.trySetProperty propertyName property entityState with
                                    | struct (true, entityState) -> (true, true, if entityState.Imperative then world else World.setEntityState entityState entity world)
                                    | struct (false, _) -> struct (false, false, world)
                            else struct (true, false, world)
                    elif property.PropertyValue =/= propertyOld.PropertyValue then
                        if entityState.Imperative then
                            // OPTIMIZATION: special-case for imperative
                            propertyOld.PropertyValue <- property.PropertyValue
                            struct (true, true, world)
                        else
                            match EntityState.trySetProperty propertyName property entityState with
                            | struct (true, entityState) -> (true, true, if entityState.Imperative then world else World.setEntityState entityState entity world)
                            | struct (false, _) -> struct (false, false, world)
                    else struct (true, false, world)
                | false -> struct (false, false, world)
            struct (success, changed, world)

        static member internal trySetEntityXtensionPropertyFast propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, world) ->
                    if changed then
                        let publishChangeBindings = entityStateOpt.PublishChangeBindings
                        let publishChangeEvents = entityStateOpt.PublishChangeEvents
                        World.publishEntityChange propertyName property.PropertyValue publishChangeBindings publishChangeEvents entity world
                    else world
                | struct (false, _, world) -> world
            else world

        static member internal trySetEntityXtensionProperty propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, world) ->
                    let world =
                        if changed then
                            let publishChangeBindings = entityStateOpt.PublishChangeBindings
                            let publishChangeEvents = entityStateOpt.PublishChangeEvents
                            World.publishEntityChange propertyName property.PropertyValue publishChangeBindings publishChangeEvents entity world
                        else world
                    struct (true, changed, world)
                | struct (false, _, _) as result -> result
            else struct (false, false, world)

        static member internal setEntityXtensionPropertyWithoutEvent propertyName property entity world =
            let entityState = World.getEntityState entity world
            match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityState entity world with
            | struct (true, changed, world) -> struct (true, changed, world)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal setEntityXtensionValue<'a> propertyName (value : 'a) entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                let entityState = entityStateOpt
                let propertyOld = EntityState.getProperty propertyName entityState
                let mutable changed = false // OPTIMIZATION: avoid passing around structs.
                let world =
                    if EntityState.containsRuntimeProperties entityState then
                        match propertyOld.PropertyValue with
                        | :? DesignerProperty as dp ->
                            if value =/= dp.DesignerValue then
                                changed <- true
                                let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                                let entityState = EntityState.setProperty propertyName property entityState
                                if entityState.Imperative then world else World.setEntityState entityState entity world
                            else world
                        | :? ComputedProperty as cp ->
                            match cp.ComputedSetOpt with
                            | Some computedSet ->
                                if value =/= cp.ComputedGet (box entity) (box world) then
                                    changed <- true
                                    computedSet propertyOld.PropertyValue entity world :?> World
                                else world
                            | None -> world
                        | _ ->
                            if value =/= propertyOld.PropertyValue then
                                changed <- true
                                if entityState.Imperative then
                                    // OPTIMIZATION: special-case for imperative
                                    propertyOld.PropertyValue <- value
                                    world
                                else
                                    let property = { propertyOld with PropertyValue = value }
                                    let entityState = EntityState.setProperty propertyName property entityState
                                    if entityState.Imperative then world else World.setEntityState entityState entity world
                            else world
                    elif value =/= propertyOld.PropertyValue then
                        changed <- true
                        if entityState.Imperative then
                            // OPTIMIZATION: special-case for imperative
                            propertyOld.PropertyValue <- value
                            world
                        else
                            let property = { propertyOld with PropertyValue = value }
                            let entityState = EntityState.setProperty propertyName property entityState
                            if entityState.Imperative then world else World.setEntityState entityState entity world
                    else world
                if changed then
                    let publishChangeBindings = entityStateOpt.PublishChangeBindings
                    let publishChangeEvents = entityStateOpt.PublishChangeEvents
                    World.publishEntityChange propertyName propertyOld.PropertyValue publishChangeBindings publishChangeEvents entity world
                else world
            else failwithf "Could not find entity '%s'." (scstring entity)

        static member internal setEntityXtensionProperty propertyName property entity world =
            match World.trySetEntityXtensionProperty propertyName property entity world with
            | struct (true, changed, world) -> struct (changed, world)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityPropertyFast propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) -> setter property entity world |> snd'
            | (false, _) -> World.trySetEntityXtensionPropertyFast propertyName property entity world

        static member internal trySetEntityProperty propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) -> let struct (changed, world) = setter property entity world in struct (true, changed, world)
            | (false, _) -> World.trySetEntityXtensionProperty propertyName property entity world

        static member internal setEntityProperty propertyName property entity world =
            match World.trySetEntityProperty propertyName property entity world with
            | struct (true, changed, world) -> struct (changed, world)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal attachEntityProperty propertyName property entity world =
            if World.getEntityExists entity world then
                let struct (_, world) =
                    World.updateEntityState
                        (fun entityState -> EntityState.attachProperty propertyName property entityState)
                        propertyName property.PropertyValue entity world
                world
            else failwith ("Cannot attach entity property '" + propertyName + "'; entity '" + scstring entity.Names + "' is not found.")

        static member internal detachEntityProperty propertyName entity world =
            if World.getEntityExists entity world then
                let struct (_, world) =
                    World.updateEntityStateWithoutEvent
                        (fun entityState -> EntityState.detachProperty propertyName entityState)
                        entity world
                world
            else failwith ("Cannot detach entity property '" + propertyName + "'; entity '" + scstring entity.Names + "' is not found.")

        static member internal getEntityDefaultOverlayName dispatcherName world =
            match World.tryFindRoutedOverlayNameOpt dispatcherName world with
            | Some _ as opt -> opt
            | None -> Some dispatcherName

        static member internal getEntityInView2d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.isBoundsInView2d transform.Bounds.Box2 world

        static member internal getEntityInPlay2d entity world =
            World.getEntityInView2d entity world // same meaning as in view for 2d

        static member internal getEntityInPlay3d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.isBoundsInPlay3d transform.Bounds world

        static member internal getEntityInView3d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.isBoundsInView3d transform.Light presence transform.Bounds world

        static member internal getEntityQuickSize (entity : Entity) world =
            let dispatcher = World.getEntityDispatcher entity world
            let facets = World.getEntityFacets entity world
            let quickSize = dispatcher.GetQuickSize (entity, world)
            Array.fold
                (fun (maxSize : Vector3) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector3
                        (Math.Max (quickSize.X, maxSize.X),
                         Math.Max (quickSize.Y, maxSize.Y),
                         Math.Max (quickSize.Z, maxSize.Z)))
                quickSize
                facets

        static member internal getEntitySortingPriority2d entity world =
            let entityState = World.getEntityState entity world
            { SortElevation = entityState.Transform.Elevation
              SortHorizon = entityState.Transform.Perimeter.Position.Y
              SortTarget = entity }

        static member internal rayCastEntity ray (entity : Entity) world =
            let facets = World.getEntityFacets entity world
            let dispatcher = World.getEntityDispatcher entity world
            let intersectionsFacets = facets |> Array.map (fun facet -> facet.RayCast (ray, entity, world)) |> Array.concat
            let intersectionsDispatcher = dispatcher.RayCast (ray, entity, world)
            let intersections = Array.append intersectionsFacets intersectionsDispatcher
            Array.sort intersections

        static member internal updateEntityPublishUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishUpdates entity (atooa entity.UpdateEvent) world

#if !DISABLE_ENTITY_POST_UPDATE
        static member internal updateEntityPublishPostUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishPostUpdates entity (atooa entity.PostUpdateEvent) world
#endif

        static member internal updateEntityPublishActualizeFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishActualizes entity (atooa entity.ActualizeEvent) world

        static member internal updateEntityPublishFlags entity world =
            let mutable changed = false // bit of funky mutation in the face of #if
            let struct (changed', world) = World.updateEntityPublishUpdateFlag entity world
            changed <- changed || changed'
#if !DISABLE_ENTITY_POST_UPDATE
            let struct (changed', world) = World.updateEntityPublishPostUpdateFlag entity world
            changed <- changed || changed'
#else
            let struct (changed', world) = World.updateEntityPublishActualizeFlag entity world
            changed <- changed || changed'
            struct (changed, world)
#endif

        static member internal divergeEntity entity world =
            let entityState = World.getEntityState entity world
            let entityState = EntityState.diverge entityState
            World.setEntityState entityState entity world

        static member internal registerEntity entity world =
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            let facets = World.getEntityFacets entity world
            let world = dispatcher.Register (entity, world)
            let world =
                Array.fold (fun world (facet : Facet) ->
                    let world = facet.Register (entity, world)
                    if WorldModule.isSelected entity world
                    then facet.RegisterPhysics (entity, world)
                    else world)
                    world facets
            let struct (_, world) = World.updateEntityPublishFlags entity world
            let eventTrace = EventTrace.debug "World" "registerEntity" "Register" EventTrace.empty
            let eventAddresses = EventSystemDelegate.getEventAddresses1 (Events.Register --> entity)
            let world = Array.fold (fun world eventAddress -> World.publish () eventAddress eventTrace entity world) world eventAddresses
            let eventTrace = EventTrace.debug "World" "registerEntity" "LifeCycle" EventTrace.empty
            let world = World.publish (RegisterData entity) (Events.LifeCycle (nameof Entity)) eventTrace entity world
            world

        static member internal unregisterEntity (entity : Entity) world =
            let eventTrace = EventTrace.debug "World" "unregisterEntity" "LifeCycle" EventTrace.empty
            let world = World.publish (UnregisteringData entity) (Events.LifeCycle (nameof Entity)) eventTrace entity world
            let eventTrace = EventTrace.debug "World" "unregister" "Unregistering" EventTrace.empty
            let eventAddresses = EventSystemDelegate.getEventAddresses1 (Events.Unregistering --> entity)
            let world = Array.fold (fun world eventAddress -> World.publish () eventAddress eventTrace entity world) world eventAddresses
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            let facets = World.getEntityFacets entity world
            let world = dispatcher.Unregister (entity, world)
            Array.fold (fun world (facet : Facet) ->
                let world = facet.Unregister (entity, world)
                if WorldModule.isSelected entity world
                then facet.UnregisterPhysics (entity, world)
                else world)
                world facets

        static member internal registerEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            Array.fold (fun world (facet : Facet) -> facet.RegisterPhysics (entity, world)) world facets

        static member internal unregisterEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            Array.fold (fun world (facet : Facet) -> facet.UnregisterPhysics (entity, world)) world facets

        static member internal propagateEntityPhysics entity world =
            let world = World.unregisterEntityPhysics entity world
            let world = World.registerEntityPhysics entity world
            world

        static member internal addEntity mayReplace entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            let isNew = not (World.getEntityExists entity world)
            if isNew || mayReplace then

                // get old world for entity tree rebuild and change events
                let oldWorld = world
                
                // add entity to world
                let world = World.addEntityState entityState entity world

                // update mount hierarchy
                let mountOpt = World.getEntityMountOpt entity world
                let world = World.addEntityToMounts mountOpt entity world

                // mutate respective spatial tree if entity is selected
                let world =
                    if WorldModule.isSelected entity world then
                        if World.getEntityIs2d entity world then
                            let quadtree =
                                MutantCache.mutateMutant
                                    (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                                    (fun entityTree ->
                                        let entityState = World.getEntityState entity world
                                        Quadtree.addElement entityState.Presence entityState.Bounds.Box2 entity entityTree
                                        entityTree)
                                    (World.getQuadtree world)
                            World.setQuadtree quadtree world
                        else
                            let octree =
                                MutantCache.mutateMutant
                                    (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                                    (fun entityTree ->
                                        let entityState = World.getEntityState entity world
                                        let element = Octelement.make entityState.Static entityState.Light entityState.Presence entity
                                        Octree.addElement entityState.Bounds element entityTree
                                        entityTree)
                                    (World.getOctree world)
                            World.setOctree octree world
                    else world

                // register entity if needed
                if isNew
                then World.registerEntity entity world
                else world

            // handle failure
            else failwith ("Adding an entity that the world already contains '" + scstring entity + "'.")

        static member internal destroyEntityImmediateInternal recur entity world =

            // attempt to remove from destruction list
            let world = World.tryRemoveSimulantFromDestruction entity world

            // ensure entity exists in the world
            if World.getEntityExists entity world then

                // get old world for entity tree rebuild
                let oldWorld = world

                // cache entity children for later possible destruction
                let children = World.getEntityEntities entity world

                // unregister entity
                let world = World.unregisterEntity entity world

                // remove mount from hierarchy
                let mountOpt = World.getEntityMountOpt entity world
                let world = World.removeEntityFromMounts mountOpt entity world

                // destroy any scheduled tasklets
                let world = World.removeTasklets entity world

                // mutate entity tree if entity is selected
                let world =
                    if WorldModule.isSelected entity world then
                        if World.getEntityIs2d entity world then
                            let quadtree =
                                MutantCache.mutateMutant
                                    (fun () -> world.WorldExtension.Dispatchers.RebuildQuadtree world)
                                    (fun quadtree ->
                                        let entityState = World.getEntityState entity oldWorld
                                        Quadtree.removeElement entityState.Presence entityState.Bounds.Box2 entity quadtree
                                        quadtree)
                                    (World.getQuadtree world)
                            World.setQuadtree quadtree world
                        else
                            let octree =
                                MutantCache.mutateMutant
                                    (fun () -> world.WorldExtension.Dispatchers.RebuildOctree world)
                                    (fun octree ->
                                        let entityState = World.getEntityState entity oldWorld
                                        let element = Octelement.make entityState.Static entityState.Light entityState.Presence entity
                                        Octree.removeElement entityState.Bounds element octree
                                        octree)
                                    (World.getOctree world)
                            World.setOctree octree world
                    else world

                // remove cached entity event addresses
                EventSystemDelegate.cleanEventAddressCache entity.EntityAddress

                // invalidate entity state
                let entityState = World.getEntityState entity world
                entityState.Invalidated <- true

                // remove the entity from the world
                let world = World.removeEntityState entity world

                // destroy children when recurring
                if recur then
                    Seq.fold (fun world child ->
                        World.destroyEntityImmediateInternal recur child world)
                        world children
                else world

            // pass
            else world

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =
            World.destroyEntityImmediateInternal true entity world

        /// Create an entity and add it to the world.
        [<FunctionBinding "createEntity">]
        static member createEntity5 dispatcherName names overlayDescriptor (group : Group) world =

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'.")

            // compute the optional overlay name
            let overlayNameDefault = Overlay.dispatcherNameToOverlayName dispatcherName
            let overlayNameOpt =
                match overlayDescriptor with
                | NoOverlay -> None
                | RoutedOverlay -> World.tryFindRoutedOverlayNameOpt dispatcherName world
                | DefaultOverlay -> Some (Option.getOrDefault overlayNameDefault (World.tryFindRoutedOverlayNameOpt dispatcherName world))
                | ExplicitOverlay overlayName -> Some overlayName

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make (World.getImperative world) names overlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let overlayer = World.getOverlayer world
            let entityState =
                match overlayNameOpt with
                | Some overlayName ->

                    // apply overlay to facets
                    let entityState = Overlayer.applyOverlayToFacetNames id dispatcherName overlayName entityState overlayer overlayer

                    // synchronize the entity's facets (and attach their properties)
                    match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> Log.debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties id entityState.Dispatcher entityState world

            // apply the entity state's overlay if exists
            let entityState =
                match entityState.OverlayNameOpt with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something
                    if overlayNameDefault <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay id overlayNameDefault overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // make entity address
            let entityAddress = group.GroupAddress <-- rtoa<Entity> entityState.Surnames

            // make entity reference
            let entity = Entity entityAddress

            // add entity's state to world
            let world =
                if World.getEntityExists entity world then
                    if World.getEntityDestroying entity world
                    then World.destroyEntityImmediate entity world
                    else failwith ("Entity '" + scstring entity + " already exists and cannot be created."); world
                else world
            let world = World.addEntity false entityState entity world

            // update mount hierarchy
            let mountOpt = World.getEntityMountOpt entity world
            let world = World.addEntityToMounts mountOpt entity world

            // propagate properties
            let world =
                if World.getEntityMounted entity world then
                    let world = World.propagateEntityAffineMatrix entity world
                    let world = World.propagateEntityElevation entity world
                    let world = World.propagateEntityEnabled entity world
                    let world = World.propagateEntityVisible entity world
                    world
                else world

            // fin
            (entity, world)

        /// Create an entity from a simulant descriptor.
        static member createEntity4 overlayDescriptor descriptor group world =
            let (entity, world) =
                World.createEntity5 descriptor.SimulantDispatcherName descriptor.SimulantSurnamesOpt overlayDescriptor group world
            let world =
                List.fold (fun world (propertyName, property) ->
                    World.setEntityProperty propertyName property entity world |> snd')
                    world descriptor.SimulantProperties
            let world =
                if WorldModule.isSelected entity world
                then World.propagateEntityPhysics entity world
                else world
            (entity, world)

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> surnamesOpt overlayNameDescriptor group world =
            World.createEntity5 typeof<'d>.Name surnamesOpt overlayNameDescriptor group world

        /// Duplicate an entity.
        static member duplicateEntity source (destination : Entity) world =
            let entityStateOpt = World.getEntityStateOpt source world
            match entityStateOpt :> obj with
            | null -> world
            | _ ->
                let entityState = { entityStateOpt with Order = Core.getUniqueTimeStamp (); IdRef = ref Gen.id; Surnames = destination.Surnames }
                World.addEntity false entityState destination world

        /// Rename an entity. Note that since this destroys the renamed entity immediately, you should not call this
        /// inside an event handler that involves the reassigned entity itself. Note this also renames all of its
        /// descendents accordingly.
        static member renameEntityImmediate source (destination : Entity) world =
            let entityStateOpt = World.getEntityStateOpt source world
            match entityStateOpt :> obj with
            | null -> world
            | _ ->
                let entityState = { entityStateOpt with IdRef = ref Gen.id; Surnames = destination.Surnames }
                let children = World.getEntityEntities source world
                let world = World.destroyEntityImmediateInternal false source world
                let world = World.addEntity false entityState destination world
                Seq.fold (fun world (child : Entity) ->
                    let destination = destination / child.Name
                    World.renameEntityImmediate child destination world)
                    world children

        /// Rename an entity.
        [<FunctionBinding>]
        static member renameEntity source destination world =
            World.frame (World.renameEntityImmediate source destination) Simulants.Game world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOverlayNameOpt overlayNameOpt entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOverlayNameOpt = oldEntityState.OverlayNameOpt
            let entityState =
                if oldEntityState.Imperative then
                    oldEntityState.OverlayNameOpt <- overlayNameOpt
                    oldEntityState
                else { oldEntityState with OverlayNameOpt = overlayNameOpt }
            match (oldOverlayNameOpt, overlayNameOpt) with
            | (Some oldOverlayName, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let (entityState, world) =
                    let oldFacetNames = entityState.FacetNames
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.diverge oldOverlayName overlayName entityState overlayer overlayer
                    match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                    | Right (entityState, world) -> (entityState, world)
                    | Left error -> Log.debug error; (entityState, world)
                let facetNames = World.getEntityFacetNamesReflectively entityState
                let entityState = Overlayer.applyOverlay EntityState.copy oldOverlayName overlayName facetNames entityState overlayer
                let oldWorld = world
                let oldEntityState = entityState
                let oldStatic = oldEntityState.Static
                let oldLight = oldEntityState.Light
                let oldPresence = oldEntityState.Presence
                let oldBounds = oldEntityState.Bounds
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                let world = World.publishEntityChanges entity world
                (Right (), world)
            | (None, None) ->
                (Right (), world)
            | (_, _) ->
                (Left "Could not set the entity's overlay name because setting an overlay to or from None is currently unimplemented.", world)
            
        /// Try to set the entity's facet names from script.
        [<FunctionBinding "trySetEntityOverlayNameOpt">]
        static member trySetEntityOverlayNameOptFromScript overlayNameOpt entity world =
            match World.trySetEntityOverlayNameOpt overlayNameOpt entity world with
            | (Right _, world) -> world
            | (Left _, world) -> world

        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            match World.trySetFacetNames facetNames entityState (Some entity) world with
            | Right (entityState, world) ->
                let oldWorld = world
                let oldEntityState = entityState
                let oldStatic = oldEntityState.Static
                let oldLight = oldEntityState.Light
                let oldPresence = oldEntityState.Presence
                let oldBounds = oldEntityState.Bounds
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree oldStatic oldLight oldPresence oldBounds entity oldWorld world
                let world = World.publishEntityChanges entity world
                (Right (), world)
            | Left error -> (Left error, world)

        /// Try to set the entity's facet names from script.
        [<FunctionBinding "trySetEntityFacetNames">]
        static member trySetEntityFacetNamesFromScript facetNames entity world =
            match World.trySetEntityFacetNames facetNames entity world with
            | (Right _, world) -> world
            | (Left _, world) -> world

        /// View all of the properties of an entity.
        static member internal viewEntityProperties entity world =
            let state = World.getEntityState entity world
            World.viewProperties state

        /// Construct a screen reference in an optimized way.
        /// OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease
        /// address hashing.
        static member internal makeScreenFast (entity : Entity) world =
            match (World.getGameState world).SelectedScreenOpt with
            | Some screen when screen.Name = Array.head (Address.getNames entity.EntityAddress) -> screen
            | Some _ | None ->
                match (World.getGameState world).OmniScreenOpt with
                | Some omniScreen when omniScreen.Name = Array.head (Address.getNames entity.EntityAddress) -> omniScreen
                | Some _ | None -> Screen (Array.head (entity.EntityAddress.Names))

        static member internal updateEntityInEntityTree oldStatic oldLight (oldPresence : Presence) oldBounds (entity : Entity) oldWorld world =

            // only do this when entity is selected
            if WorldModule.isSelected entity world then

                // OPTIMIZATION: work with the entity state directly to avoid function call overheads
                let entityState = World.getEntityState entity world
                let newStatic = entityState.Static
                let newLight = entityState.Light
                let newPresence = entityState.Presence
                let newBounds = entityState.Bounds

                // OPTIMIZATION: only update when relevant entity state has changed.
                if  newStatic <> oldStatic ||
                    newLight <> oldLight ||
                    presenceNeq newPresence oldPresence ||
                    box3Neq oldBounds newBounds then

                    // update entity in entity tree
                    if entityState.Is2d then
                        let quadree =
                            MutantCache.mutateMutant
                                (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildQuadtree oldWorld)
                                (fun quadree -> Quadtree.updateElement oldPresence oldBounds.Box2 newPresence newBounds.Box2 entity quadree; quadree)
                                (World.getQuadtree world)
                        World.setQuadtree quadree world
                    else
                        let octree =
                            MutantCache.mutateMutant
                                (fun () -> oldWorld.WorldExtension.Dispatchers.RebuildOctree oldWorld)
                                (fun octree ->
                                    let element = Octelement.make newStatic newLight newPresence entity
                                    Octree.updateElement oldPresence oldBounds newPresence newBounds element octree
                                    octree)
                                (World.getOctree world)
                        World.setOctree octree world

                // fin
                else world

            // fin
            else world

        /// Attempt to get the dispatcher name for an entity currently on the world's clipboard.
        static member tryGetEntityDispatcherNameOnClipboard (_ : World) =
            match Clipboard with
            | Some (:? EntityState as entityState) -> Some (getTypeName entityState.Dispatcher)
            | _ -> None

        /// Copy an entity to the world's clipboard.
        static member copyEntityToClipboard entity world =
            let entityState = World.getEntityState entity world
            Clipboard <- Some (entityState :> obj)

        /// Cut an entity to the world's clipboard.
        static member cutEntityToClipboard entity world =
            World.copyEntityToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the world's clipboard.
        static member pasteEntityFromClipboard atMouse rightClickPosition positionSnap degreesSnap scaleSnap surnamesOpt (group : Group) world =
            // TODO: 3D: implement this for 3d.
            match Clipboard with
            | Some entityStateObj ->
                let entityState = entityStateObj :?> EntityState
                let (id, surnames) = Gen.idAndSurnamesIf surnamesOpt
                let entityState = { entityState with Order = Core.getUniqueTimeStamp (); IdRef = ref id; Surnames = surnames }
                let position2d =
                    let viewport = World.getViewport world
                    let eyePosition = World.getEyePosition2d world
                    let eyeSize = World.getEyeSize2d world
                    if atMouse
                    then viewport.MouseToWorld2d (entityState.Absolute, rightClickPosition, eyePosition, eyeSize)
                    else viewport.MouseToWorld2d (entityState.Absolute, (World.getEyeSize2d world * 0.5f), eyePosition, eyeSize)
                let mutable transform = entityState.Transform
                transform.Position <- position2d.V3
                transform.Snap (positionSnap, degreesSnap, scaleSnap)
                let entityState = EntityState.setTransformByRef (&transform, entityState)
                let entity = Entity (group.GroupAddress <-- rtoa<Entity> surnames)
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

    /// Initialize property getters.
    let private initGetters () =
        EntityGetters.Assign ("Dispatcher", fun entity world -> { PropertyType = typeof<EntityDispatcher>; PropertyValue = World.getEntityDispatcher entity world })
        EntityGetters.Assign ("Facets", fun entity world -> { PropertyType = typeof<Facet array>; PropertyValue = World.getEntityFacets entity world })
        EntityGetters.Assign ("Transform", fun entity world -> { PropertyType = typeof<Transform>; PropertyValue = (World.getEntityState entity world).Transform })
        EntityGetters.Assign ("PerimeterUnscaled", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeterUnscaled entity world })
        EntityGetters.Assign ("Perimeter", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeter entity world })
        EntityGetters.Assign ("Center", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityCenter entity world })
        EntityGetters.Assign ("Bottom", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityBottom entity world })
        EntityGetters.Assign ("PerimeterOriented", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeterOriented entity world })
        EntityGetters.Assign ("Bounds", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityBounds entity world })
        EntityGetters.Assign ("Position", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPosition entity world })
        EntityGetters.Assign ("PositionLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPositionLocal entity world })
        EntityGetters.Assign ("Rotation", fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotation entity world })
        EntityGetters.Assign ("RotationLocal", fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotationLocal entity world })
        EntityGetters.Assign ("Scale", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScale entity world })
        EntityGetters.Assign ("ScaleLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScaleLocal entity world })
        EntityGetters.Assign ("Offset", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityOffset entity world })
        EntityGetters.Assign ("Angles", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAngles entity world })
        EntityGetters.Assign ("AnglesLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAnglesLocal entity world })
        EntityGetters.Assign ("Degrees", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegrees entity world })
        EntityGetters.Assign ("DegreesLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegreesLocal entity world })
        EntityGetters.Assign ("Size", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntitySize entity world })
        EntityGetters.Assign ("Elevation", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevation entity world })
        EntityGetters.Assign ("ElevationLocal", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevationLocal entity world })
        EntityGetters.Assign ("Overflow", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityOverflow entity world })
        EntityGetters.Assign ("Presence", fun entity world -> { PropertyType = typeof<Presence>; PropertyValue = World.getEntityPresence entity world })
        EntityGetters.Assign ("Absolute", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAbsolute entity world })
        EntityGetters.Assign ("Model", fun entity world -> let designerProperty = World.getEntityModelProperty entity world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        EntityGetters.Assign ("MountOpt", fun entity world -> { PropertyType = typeof<Entity Relation option>; PropertyValue = World.getEntityMountOpt entity world })
        EntityGetters.Assign ("Imperative", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityImperative entity world })
        EntityGetters.Assign ("PublishChangeBindings", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeBindings entity world })
        EntityGetters.Assign ("PublishChangeEvents", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeEvents entity world })
        EntityGetters.Assign ("Enabled", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabled entity world })
        EntityGetters.Assign ("EnabledLocal", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabledLocal entity world })
        EntityGetters.Assign ("Visible", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisible entity world })
        EntityGetters.Assign ("VisibleLocal", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisibleLocal entity world })
        EntityGetters.Assign ("AlwaysUpdate", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysUpdate entity world })
        EntityGetters.Assign ("PublishUpdates", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishUpdates entity world })
        EntityGetters.Assign ("PublishPostUpdates", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishPostUpdates entity world })
        EntityGetters.Assign ("PublishActualizes", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishActualizes entity world })
        EntityGetters.Assign ("Persistent", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPersistent entity world })
        EntityGetters.Assign ("IgnorePropertyBindings", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIgnorePropertyBindings entity world })
        EntityGetters.Assign ("Mounted", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityMounted entity world })
        EntityGetters.Assign ("Is2d", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIs2d entity world })
        EntityGetters.Assign ("Centered", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityCentered entity world })
        EntityGetters.Assign ("Static", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityStatic entity world })
        EntityGetters.Assign ("Light", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityLight entity world })
        EntityGetters.Assign ("Physical", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPhysical entity world })
        EntityGetters.Assign ("Optimized", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityOptimized entity world })
        EntityGetters.Assign ("Destroying", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityDestroying entity world })
        EntityGetters.Assign ("OverlayNameOpt", fun entity world -> { PropertyType = typeof<string option>; PropertyValue = World.getEntityOverlayNameOpt entity world })
        EntityGetters.Assign ("FacetNames", fun entity world -> { PropertyType = typeof<string Set>; PropertyValue = World.getEntityFacetNames entity world })
        EntityGetters.Assign ("Order", fun entity world -> { PropertyType = typeof<int64>; PropertyValue = World.getEntityOrder entity world })
        EntityGetters.Assign ("Id", fun entity world -> { PropertyType = typeof<Guid>; PropertyValue = World.getEntityId entity world })
        EntityGetters.Assign ("Surnames", fun entity world -> { PropertyType = typeof<string array>; PropertyValue = World.getEntitySurnames entity world })
        EntityGetters.Assign ("Name", fun entity world -> { PropertyType = typeof<string>; PropertyValue = World.getEntityName entity world })

    /// Initialize property setters.
    let private initSetters () =
        EntitySetters.Assign ("Transform", fun property entity world -> let mutable transform = property.PropertyValue :?> Transform in World.setEntityTransformByRef (&transform, World.getEntityState entity world, entity, world))
        EntitySetters.Assign ("PerimeterUnscaled", fun property entity world -> World.setEntityPerimeterUnscaled (property.PropertyValue :?> Box3) entity world)
        EntitySetters.Assign ("Perimeter", fun property entity world -> World.setEntityPerimeter (property.PropertyValue :?> Box3) entity world)
        EntitySetters.Assign ("Center", fun property entity world -> World.setEntityCenter (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Bottom", fun property entity world -> World.setEntityBottom (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Position", fun property entity world -> World.setEntityPosition (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("PositionLocal", fun property entity world -> World.setEntityPositionLocal (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Scale", fun property entity world -> World.setEntityScale (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("ScaleLocal", fun property entity world -> World.setEntityScaleLocal (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Rotation", fun property entity world -> World.setEntityRotation (property.PropertyValue :?> Quaternion) entity world)
        EntitySetters.Assign ("RotationLocal", fun property entity world -> World.setEntityRotationLocal (property.PropertyValue :?> Quaternion) entity world)
        EntitySetters.Assign ("Offset", fun property entity world -> World.setEntityOffset (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Angles", fun property entity world -> World.setEntityAngles (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("AnglesLocal", fun property entity world -> World.setEntityAnglesLocal (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Degrees", fun property entity world -> World.setEntityDegrees (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("DegreesLocal", fun property entity world -> World.setEntityDegreesLocal (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Size", fun property entity world -> World.setEntitySize (property.PropertyValue :?> Vector3) entity world)
        EntitySetters.Assign ("Elevation", fun property entity world -> World.setEntityElevation (property.PropertyValue :?> single) entity world)
        EntitySetters.Assign ("ElevationLocal", fun property entity world -> World.setEntityElevationLocal (property.PropertyValue :?> single) entity world)
        EntitySetters.Assign ("Overflow", fun property entity world -> World.setEntityOverflow (property.PropertyValue :?> single) entity world)
        EntitySetters.Assign ("Presence", fun property entity world -> World.setEntityPresence (property.PropertyValue :?> Presence) entity world)
        EntitySetters.Assign ("Absolute", fun property entity world -> World.setEntityAbsolute (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Model", fun property entity world -> World.setEntityModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } entity world)
        EntitySetters.Assign ("MountOpt", fun property entity world -> World.setEntityMountOpt (property.PropertyValue :?> Entity Relation option) entity world)
        EntitySetters.Assign ("Imperative", fun property entity world -> World.setEntityImperative (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Enabled", fun property entity world -> World.setEntityEnabled (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("EnabledLocal", fun property entity world -> World.setEntityEnabledLocal (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Visible", fun property entity world -> World.setEntityVisible (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("VisibleLocal", fun property entity world -> World.setEntityVisibleLocal (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Centered", fun property entity world -> World.setEntityCentered (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Static", fun property entity world -> World.setEntityStatic (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Light", fun property entity world -> World.setEntityLight (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("AlwaysUpdate", fun property entity world -> World.setEntityAlwaysUpdate (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("Persistent", fun property entity world -> World.setEntityPersistent (property.PropertyValue :?> bool) entity world)
        EntitySetters.Assign ("IgnorePropertyBindings", fun property entity world -> World.setEntityIgnorePropertyBindings (property.PropertyValue :?> bool) entity world)

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()