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

    /// Dynamic property getters / setters.
    let internal EntityGetters = Dictionary<string, Entity -> World -> Property> HashIdentity.Structural
    let internal EntitySetters = Dictionary<string, Property -> Entity -> World -> bool * World> HashIdentity.Structural

    /// Mutable clipboard that allows its state to persist beyond undo / redo.
    let mutable private Clipboard : obj option = None

    /// Publishing IDs.
    let internal EntityChangeCountsId = Gen.id
    let internal EntityBindingCountsId = Gen.id

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    type private KeyEquality () =
        inherit OptimizedClosures.FSharpFunc<
            KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>,
            KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>,
            bool> ()
        override this.Invoke _ = failwithumf ()
        override this.Invoke
            (entityStateKey : KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>,
             entityStateKey2 : KeyValuePair<Entity Address, UMap<Entity Address, EntityState>>) =
            refEq entityStateKey.Key entityStateKey2.Key &&
            refEq entityStateKey.Value entityStateKey2.Value
    let private keyEquality = KeyEquality ()

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    let mutable private getFreshKeyAndValueEntity = Unchecked.defaultof<Entity>
    let mutable private getFreshKeyAndValueWorld = Unchecked.defaultof<World>
    let private getFreshKeyAndValue () =
        let mutable entityStateOpt = Unchecked.defaultof<_>
        let _ = UMap.tryGetValue (getFreshKeyAndValueEntity.EntityAddress, getFreshKeyAndValueWorld.EntityStates, &entityStateOpt)
        KeyValuePair (KeyValuePair (getFreshKeyAndValueEntity.EntityAddress, getFreshKeyAndValueWorld.EntityStates), entityStateOpt)
    let private getFreshKeyAndValueCached =
        getFreshKeyAndValue

    // OPTIMIZATION: cache one entity change address
    let mutable changeEventNamesFree = true
    let changeEventNamesCached = [|"Change"; ""; "Event"; ""; ""; ""|]

    type World with
    
        // OPTIMIZATION: a ton of optimization has gone down in here...!
        static member private entityStateFinder (entity : Entity) world =
            let entityStateOpt = entity.EntityStateOpt
            if isNull (entityStateOpt :> obj) || entityStateOpt.Invalidated then
                getFreshKeyAndValueEntity <- entity
                getFreshKeyAndValueWorld <- world
                let entityStateOpt =
                    KeyedCache.getValueFast
                        keyEquality
                        getFreshKeyAndValueCached
                        (KeyValuePair (entity.EntityAddress, world.EntityStates))
                        (World.getEntityCachedOpt world)
                getFreshKeyAndValueEntity <- Unchecked.defaultof<Entity>
                getFreshKeyAndValueWorld <- Unchecked.defaultof<World>
                match entityStateOpt :> obj with
                | null ->
                    Unchecked.defaultof<EntityState>
                | _ ->
                    if entityStateOpt.ShouldMutate then entity.EntityStateOpt <- entityStateOpt
                    entityStateOpt
            else entityStateOpt

        static member private entityStateAdder entityState (entity : Entity) world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [|screenName; groupName; entityName|] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some groupDirectory ->
                        match UMap.tryFind groupName groupDirectory.Value with
                        | Some entityDirectory ->
                            let entityDirectory' = UMap.add entityName entity.EntityAddress entityDirectory.Value
                            let groupDirectory' = UMap.add groupName (KeyValuePair (entityDirectory.Key, entityDirectory')) groupDirectory.Value
                            UMap.add screenName (KeyValuePair (groupDirectory.Key, groupDirectory')) world.ScreenDirectory
                        | None -> failwith ("Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent group.")
                    | None -> failwith ("Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent screen.")
                | _ -> failwith ("Invalid entity address '" + scstring entity.EntityAddress + "'.")
            let entityStates = UMap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateRemover (entity : Entity) world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [|screenName; groupName; entityName|] ->
                    match UMap.tryFind screenName world.ScreenDirectory with
                    | Some groupDirectory ->
                        match UMap.tryFind groupName groupDirectory.Value with
                        | Some entityDirectory ->
                            let entityDirectory' = UMap.remove entityName entityDirectory.Value
                            let groupDirectory' = UMap.add groupName (KeyValuePair (entityDirectory.Key, entityDirectory')) groupDirectory.Value
                            UMap.add screenName (KeyValuePair (groupDirectory.Key, groupDirectory')) world.ScreenDirectory
                        | None -> failwith ("Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent group.")
                    | None -> failwith ("Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent screen.")
                | _ -> failwith ("Invalid entity address '" + scstring entity.EntityAddress + "'.")
            let entityStates = UMap.remove entity.EntityAddress world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateSetter entityState (entity : Entity) world =
#if DEBUG
            if not (UMap.containsKey entity.EntityAddress world.EntityStates) then
                failwith ("Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'")
#endif
            let entityStates = UMap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private addEntityState entityState (entity : Entity) world =
            World.entityStateAdder entityState entity world

        static member private removeEntityState (entity : Entity) world =
            World.entityStateRemover entity world

        static member private publishEntityChange propertyName (propertyValue : obj) (entity : Entity) world =

            // publish change binding
            let world =
                if World.getEntityPublishChangeBindings entity world
                then World.publishBindingChange propertyName entity world
                else world

            // publish change event
            let world =
                if World.getEntityPublishChangeEvents entity world then
                    let changeData = { Name = propertyName; Value = propertyValue }
                    let entityNames = Address.getNames entity.EntityAddress
                    let mutable changeEventNamesUtilized = false
                    let changeEventAddress =
                        if  changeEventNamesFree then
                            changeEventNamesFree <- false
                            changeEventNamesUtilized <- true
                            changeEventNamesCached.[1] <- propertyName
                            changeEventNamesCached.[3] <- entityNames.[0]
                            changeEventNamesCached.[4] <- entityNames.[1]
                            changeEventNamesCached.[5] <- entityNames.[2]
                            rtoa<ChangeData> changeEventNamesCached
                        else rtoa<ChangeData> [|"Change"; propertyName; "Event"; entityNames.[0]; entityNames.[1]; entityNames.[2]|]
                    let eventTrace = EventTrace.debug "World" "publishEntityChange" "" EventTrace.empty
                    let sorted = propertyName = "ParentNodeOpt"
                    let world = World.publishPlus changeData changeEventAddress eventTrace entity sorted world
                    if changeEventNamesUtilized then changeEventNamesFree <- true
                    world
                else world

            // fin
            world

        static member private getEntityStateOpt entity world =
            World.entityStateFinder entity world

        static member internal getEntityState entity world =
#if DEBUG
            let entityStateOpt = World.entityStateFinder entity world
            match entityStateOpt :> obj with
            | null -> failwith ("Could not find entity with address '" + scstring entity.EntityAddress + "'.")
            | _ -> entityStateOpt
#else
            World.entityStateFinder entity world
#endif

        static member internal getEntityXtensionProperties entity world =
            let entityState = World.getEntityState entity world
            entityState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private setEntityState entityState entity world =
            World.entityStateSetter entityState entity world

        // NOTE: P1: I think we could use an in ref in updater to avoid allocation on every call...
        static member private updateEntityStateInternal updater (entityState : EntityState) entity world =
            match updater entityState with
            | Some (entityState : EntityState) ->
                if entityState.ShouldMutate
                then (true, world)
                else (true, World.setEntityState entityState entity world)
            | None -> (false, world)

        static member private updateEntityStateWithoutEvent updater entity world =
            let entityState = World.getEntityState entity world
            let (changed, world) = World.updateEntityStateInternal updater entityState entity world
            (changed, world)

        static member private updateEntityState updater propertyName propertyValue entity world =
            let entityState = World.getEntityState entity world
            let (changed, world) = World.updateEntityStateInternal updater entityState entity world
            if changed
            then World.publishEntityChange propertyName propertyValue entity world
            else world

        static member private updateEntityStatePlus updater propertyName propertyValue entity world =

            // cache old values
            let oldWorld = world
            let oldEntityState = World.getEntityState entity oldWorld
            let oldOmnipresent = oldEntityState.Omnipresent

            // OPTIMIZATION: don't update entity tree if entity is omnipresent
            let (changed, world) =
                if not oldOmnipresent then
                    let oldOmnipresent = oldEntityState.Omnipresent
                    let oldAbsolute = oldEntityState.Absolute
                    let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                    let (changed, world) = World.updateEntityStateInternal updater oldEntityState entity world
                    let world =
                        if changed
                        then World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
                        else world
                    (changed, world)
                else World.updateEntityStateInternal updater oldEntityState entity world

            // publish entity change event if needed
            if changed
            then World.publishEntityChange propertyName propertyValue entity world
            else world

        static member private publishEntityChanges entity world =
            let entityState = World.getEntityState entity world
            let properties = World.getProperties entityState
            if entityState.PublishChangeEvents then
                List.fold (fun world (propertyName, _, propertyValue) ->
                    World.publishEntityChange propertyName propertyValue entity world)
                    world properties
            else world

        static member internal getEntityExists entity world =
            notNull (World.getEntityStateOpt entity world :> obj)

        static member internal getEntityStateBoundsMax entityState =
            // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
            let transform = entityState.Transform
            match transform.Rotation with
            | 0.0f ->
                let boundsOverflow = v4BoundsOverflow transform.Position transform.Size entityState.Overflow
                boundsOverflow // no need to transform when unrotated
            | _ ->
                let boundsOverflow = v4BoundsOverflow transform.Position transform.Size entityState.Overflow
                let position = boundsOverflow.Position
                let size = boundsOverflow.Size
                let center = position + size * 0.5f
                let corner = position + size
                let centerToCorner = corner - center
                let quaternion = Quaternion.CreateFromAxisAngle (Vector3.UnitZ, Constants.Math.DegreesToRadiansF * 45.0f)
                let newSizeOver2 = v2Dup (Vector2.Transform (centerToCorner, quaternion)).Y
                let newPosition = center - newSizeOver2
                let newSize = newSizeOver2 * 2.0f
                v4Bounds newPosition newSize

        static member internal getEntityImperative entity world =
            (World.getEntityState entity world).Imperative

        static member internal setEntityImperative value entity world =
            World.updateEntityState
                (fun entityState ->
                    if value <> entityState.Imperative then
                        if value then
                            let properties = UMap.makeFromSeq Imperative (Xtension.toSeq entityState.Xtension)
                            let xtension = Xtension.make properties false true true
                            entityState.Xtension <- xtension
                            entityState.Imperative <- true
                            Some entityState
                        else
                            let properties = UMap.makeFromSeq Functional (Xtension.toSeq entityState.Xtension)
                            let xtension = Xtension.make properties false true false
                            let entityState = EntityState.diverge entityState
                            entityState.Xtension <- xtension
                            entityState.Imperative <- false
                            Some entityState
                    else None)
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
                        let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                        entityState.Model <- { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }
                        Some entityState
                    else None)
                Property? Model value.DesignerValue entity world

        static member internal setEntityModel<'a> (value : 'a) entity world =
            World.updateEntityState
                (fun entityState ->
                    let valueObj = value :> obj
                    if valueObj =/= entityState.Model.DesignerValue then
                        let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                        entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = valueObj }
                        Some entityState
                    else None)
                Property? Model value entity world
                
        static member internal getEntityScriptFrame entity world =
            let entityState = World.getEntityState entity world
            match entityState.ScriptFrameOpt with
            | null ->
                let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                let scriptFrame = Scripting.DeclarationFrame HashIdentity.Structural
                entityState.ScriptFrameOpt <- scriptFrame
                scriptFrame
            | scriptFrame -> scriptFrame

        static member internal setEntityScriptFrame value entity world =
            World.updateEntityState (fun entityState ->
                let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                if value <> entityState.ScriptFrameOpt then
                    entityState.ScriptFrameOpt <- value
                    Some entityState
                else None)
                Property? ScriptFrame value entity world

        // NOTE: wouldn't macros be nice?
        static member internal getEntityDispatcher entity world = (World.getEntityState entity world).Dispatcher
        static member internal getEntityFacets entity world = (World.getEntityState entity world).Facets
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Transform.Position
        static member internal getEntitySize entity world = (World.getEntityState entity world).Transform.Size
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Transform.Rotation
        static member internal getEntityElevation entity world = (World.getEntityState entity world).Transform.Elevation
        static member internal getEntityFlags entity world = (World.getEntityState entity world).Transform.Flags
        static member internal getEntityOmnipresent entity world = (World.getEntityState entity world).Omnipresent
        static member internal getEntityAbsolute entity world = (World.getEntityState entity world).Absolute
        static member internal getEntityPublishChangeBindings entity world = (World.getEntityState entity world).PublishChangeBindings
        static member internal getEntityPublishChangeEvents entity world = (World.getEntityState entity world).PublishChangeEvents
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal getEntityAlwaysUpdate entity world = (World.getEntityState entity world).AlwaysUpdate
        static member internal getEntityPublishUpdates entity world = (World.getEntityState entity world).PublishUpdates
        static member internal getEntityPublishPostUpdates entity world = (World.getEntityState entity world).PublishPostUpdates
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal getEntityOptimized entity world = (World.getEntityState entity world).Optimized
        static member internal getEntityShouldMutate entity world = (World.getEntityState entity world).ShouldMutate
        static member internal getEntityDestroying (entity : Entity) world = List.exists ((=) (entity :> Simulant)) world.DestructionListRev
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Overflow
        static member internal getEntityOverlayNameOpt entity world = (World.getEntityState entity world).OverlayNameOpt
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityCreationTimeStamp entity world = (World.getEntityState entity world).CreationTimeStamp
        static member internal getEntityName entity world = (World.getEntityState entity world).Name
        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal setEntityOmnipresent value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.Omnipresent then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Omnipresent <- value; entityState) else None) Property? Omnipresent value entity world
        static member internal setEntityAbsolute value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.Absolute then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Absolute <- value; entityState) else None) Property? Absolute value entity world
        static member internal setEntityPublishChangeEvents value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishChangeEvents then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.PublishChangeEvents <- value; entityState) else None) Property? PublishChangeEvents value entity world
        static member internal setEntityPublishChangeBindings value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishChangeBindings then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.PublishChangeBindings <- value; entityState) else None) Property? PublishChangeBindings value entity world
        static member internal setEntityEnabled value entity world = World.updateEntityState (fun entityState -> if value <> entityState.Enabled then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Enabled <- value; entityState) else None) Property? Enabled value entity world
        static member internal setEntityVisible value entity world = World.updateEntityState (fun entityState -> if value <> entityState.Visible then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Visible <- value; entityState) else None) Property? Visible value entity world
        static member internal setEntityAlwaysUpdate value entity world = World.updateEntityStatePlus (fun entityState -> if value <> entityState.AlwaysUpdate then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.AlwaysUpdate <- value; entityState) else None) Property? AlwaysUpdate value entity world
        static member internal setEntityPublishUpdates value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishUpdates then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.PublishUpdates <- value; entityState) else None) Property? PublishUpdates value entity world
        static member internal setEntityPublishPostUpdates value entity world = World.updateEntityState (fun entityState -> if value <> entityState.PublishPostUpdates then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.PublishPostUpdates <- value; entityState) else None) Property? PublishPostUpdates value entity world
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> if value <> entityState.Persistent then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Persistent <- value; entityState) else None) Property? Persistent value entity world
        static member internal setEntityOverflow value entity world = World.updateEntityStatePlus (fun entityState -> if v2Neq value entityState.Overflow then Some (let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState in entityState.Overflow <- value; entityState) else None) Property? Overflow value entity world

        static member internal setEntityTransformByRefWithoutEvent (valueInRef : Transform inref, entity, world) =
            let oldWorld = world
            let oldEntityState = World.getEntityState entity world
            let oldOmnipresent = oldEntityState.Omnipresent
            let oldAbsolute = oldEntityState.Absolute
            let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
            let (changed, world) =
                let (value : Transform) = valueInRef // NOTE: unfortunately, a Transform copy is required to pass the lambda barrier.
                World.updateEntityStateWithoutEvent
                    (fun entityState ->
                        if not (Transform.equalsByRef (&value, &entityState.Transform))
                        then Some (EntityState.setTransformByRef (&value, entityState))
                        else None)
                    entity world
            if changed then
                let ignoredFlags = TransformMasks.InvalidatedMask ||| TransformMasks.DirtyMask
                let oldFlags = oldEntityState.Transform.Flags ||| ignoredFlags
                let newFlags = valueInRef.Flags ||| ignoredFlags
                if oldFlags <> newFlags then failwith "Cannot change transform flags via setEntityTransformEithoutEvent."
                World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
            else world

        static member internal setEntityTransformByRef (valueInRef : Transform inref, entity, world) =
            let oldWorld = world
            let oldEntityState = World.getEntityState entity world
            let oldOmnipresent = oldEntityState.Omnipresent
            let oldTransform = oldEntityState.Transform
            let (changed, world) =
                if oldOmnipresent then
                    let (value : Transform) = valueInRef // NOTE: unfortunately, a Transform copy is required to pass the lambda barrier.
                    World.updateEntityStateWithoutEvent
                        (fun entityState ->
                            if not (Transform.equalsByRef (&value, &entityState.Transform))
                            then Some (EntityState.setTransformByRef (&value, entityState))
                            else None)
                        entity world
                else
                    let oldAbsolute = oldEntityState.Absolute
                    let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                    let (value : Transform) = valueInRef // NOTE: unfortunately, a Transform copy is required to pass the lambda barrier.
                    let (changed, world) =
                        World.updateEntityStateWithoutEvent
                            (fun entityState ->
                                if not (Transform.equalsByRef (&value, &entityState.Transform))
                                then Some (EntityState.setTransformByRef (&value, entityState))
                                else None)
                            entity world
                    let world = World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
                    (changed, world)
            if changed then
#if DEBUG
                let ignoredFlags = TransformMasks.InvalidatedMask ||| TransformMasks.DirtyMask
                let oldFlags = oldEntityState.Transform.Flags ||| ignoredFlags
                let newFlags = valueInRef.Flags ||| ignoredFlags
                if oldFlags <> newFlags then failwith "Cannot change transform flags via setEntityTransform."
#endif
                if World.getEntityPublishChangeEvents entity world then
                    let positionChanged = v2Neq valueInRef.Position oldTransform.Position
                    let sizeChanged = v2Neq valueInRef.Size oldTransform.Size
                    let rotationChanged = valueInRef.Rotation <> oldTransform.Rotation
                    let elevationChanged = valueInRef.Elevation <> oldTransform.Elevation
                    let world = World.publishEntityChange Property? Transform valueInRef entity world
                    let world = if positionChanged || sizeChanged then World.publishEntityChange Property? Bounds (v4Bounds valueInRef.Position valueInRef.Size) entity world else world
                    let world = if positionChanged then World.publishEntityChange Property? Position valueInRef.Position entity world else world
                    let world = if positionChanged || sizeChanged then World.publishEntityChange Property? Center (valueInRef.Position + valueInRef.Size * 0.5f) entity world else world
                    let world = if positionChanged || sizeChanged then World.publishEntityChange Property? Bottom (valueInRef.Position + valueInRef.Size.WithY 0.0f * 0.5f) entity world else world
                    let world = if sizeChanged then World.publishEntityChange Property? Size valueInRef.Size entity world else world
                    let world = if rotationChanged then World.publishEntityChange Property? Rotation valueInRef.Rotation entity world else world
                    let world = if elevationChanged then World.publishEntityChange Property? Elevation valueInRef.Elevation entity world else world
                    world
                else world
            else world

        static member internal setEntityPosition value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            if v2Neq value transform.Position then
                transform.Position <- value
                World.setEntityTransformByRef (&transform, entity, world)
            else world
        
        static member internal setEntitySize value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            if v2Neq value transform.Size then
                transform.Size <- value
                World.setEntityTransformByRef (&transform, entity, world)
            else world
        
        static member internal setEntityRotation value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            if value <> transform.Rotation then
                transform.Rotation <- value
                World.setEntityTransformByRef (&transform, entity, world)
            else world
        
        static member internal setEntityElevation value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            if value <> transform.Elevation then
                transform.Elevation <- value
                World.setEntityTransformByRef (&transform, entity, world)
            else world

        static member internal getEntityBounds entity world =
            let mutable transform = &(World.getEntityState entity world).Transform
            v4Bounds transform.Position transform.Size

        static member internal setEntityBounds (value : Vector4) entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            let position = value.Position
            let size = value.Size
            if v2Neq transform.Position position || v2Neq transform.Size size then
                transform.Position <- position
                transform.Size <- size
                World.setEntityTransformByRef (&transform, entity, world)
            else world

        static member internal getEntityCenter entity world =
            let mutable transform = &(World.getEntityState entity world).Transform
            transform.Position + transform.Size * 0.5f

        static member internal setEntityCenter value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            let position = value - transform.Size * 0.5f
            if v2Neq transform.Position position then
                transform.Position <- position
                World.setEntityTransformByRef (&transform, entity, world)
            else world

        static member internal getEntityBottom entity world =
            let mutable transform = &(World.getEntityState entity world).Transform
            transform.Position + transform.Size.WithY 0.0f * 0.5f

        static member internal setEntityBottom value entity world =
            let mutable transform = (World.getEntityState entity world).Transform
            let position = value - transform.Size.WithY 0.0f * 0.5f
            if v2Neq transform.Position position then
                transform.Position <- position
                World.setEntityTransformByRef (&transform, entity, world)
            else world

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
                    let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                    entityState.FacetNames <- facetNames
                    entityState.Facets <- facets
                    entityState
                match entityOpt with
                | Some entity ->
                    let oldWorld = world
                    let oldEntityState = entityState
                    let oldOmnipresent = oldEntityState.Omnipresent
                    let oldAbsolute = oldEntityState.Absolute
                    let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
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
                        let entityState = if entityState.ShouldMutate then entityState else EntityState.diverge entityState
                        entityState.FacetNames <- facetNames
                        entityState.Facets <- facets
                        entityState
                    let entityState = Reflection.attachProperties EntityState.diverge facet entityState world
                    match entityOpt with
                    | Some entity ->
                        let oldWorld = world
                        let oldEntityState = entityState
                        let oldOmnipresent = oldEntityState.Omnipresent
                        let oldAbsolute = oldEntityState.Absolute
                        let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                        let world = World.setEntityState entityState entity world
                        let world = World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
                        let world = facet.Register (entity, world)
                        let world =
                            if WorldModule.isSelected entity world
                            then facet.RegisterPhysics (entity, world)
                            else world
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ("Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'.")
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
            let publishUpdates =
                match UMap.tryFind eventAddress (World.getSubscriptions world) with
                | Some subscriptions ->
                    if OMap.isEmpty subscriptions
                    then failwithumf () // NOTE: event system is defined to clean up all empty subscription entries
                    else true
                | None -> false
            if World.getEntityExists entity world
            then setFlag publishUpdates entity world
            else world

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
                    let oldOmnipresent = oldEntityState.Omnipresent
                    let oldAbsolute = oldEntityState.Absolute
                    let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.diverge overlayName overlayName facetNames entityState oldOverlayer overlayer
                    let world = World.setEntityState entityState entity world
                    World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
                | Left error -> Log.info ("There was an issue in applying a reloaded overlay: " + error); world
            | None -> world

        static member internal tryGetEntityProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ ->
                match EntityState.tryGetProperty (propertyName, entityStateOpt, &property) with
                | true ->
                    match property.PropertyValue with
                    | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (entity :> obj) (world :> obj) }; true
                    | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                    | _ -> true
                | false ->
                    match EntityGetters.TryGetValue propertyName with
                    | (true, getter) -> property <- getter entity world; true
                    | (false, _) -> false

        static member internal getEntityProperty propertyName entity world =
            let mutable property = Unchecked.defaultof<_>
            match World.tryGetEntityProperty (propertyName, entity, world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityPropertyWithoutEvent propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> (false, false, world)
            | _ ->
                match EntitySetters.TryGetValue propertyName with
                | (false, _) ->
                    let mutable propertyOld = Unchecked.defaultof<_>
                    let (success, changed, world) =
                        match EntityState.tryGetProperty (propertyName, entityStateOpt, &propertyOld) with
                        | true ->
                            match propertyOld.PropertyValue with
                            | :? ComputedProperty as cp ->
                                match cp.ComputedSetOpt with
                                | Some computedSet ->
                                    if property.PropertyValue =/= cp.ComputedGet (box entity) (box world)
                                    then (true, true, computedSet property.PropertyValue entity world :?> World)
                                    else (true, false, world)
                                | None -> (false, false, world)
                            | :? DesignerProperty as dp ->
                                if property.PropertyValue =/= dp.DesignerValue then
                                    let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                                    match EntityState.trySetProperty propertyName property entityStateOpt with
                                    | (true, entityState) -> (true, true, World.setEntityState entityState entity world)
                                    | (false, _) -> (false, false, world)
                                else (true, false, world)
                            | _ ->
                                if property.PropertyValue =/= propertyOld.PropertyValue then
                                    match EntityState.trySetProperty propertyName property entityStateOpt with
                                    | (true, entityState) -> (true, true, World.setEntityState entityState entity world)
                                    | (false, _) -> (false, false, world)
                                else (true, false, world)
                        | false -> (false, false, world)
                    (success, changed, world)
                | (true, setter) ->
                    let (changed, world) = setter property entity world
                    (true, changed, world)

        static member internal setEntityPropertyWithoutEvent propertyName property entity world =
            match World.trySetEntityPropertyWithoutEvent propertyName property entity world with
            | (true, changed, world) -> (true, changed, world)
            | (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityProperty propertyName property entity world =
            match World.trySetEntityPropertyWithoutEvent propertyName property entity world with
            | (true, changed, world) ->
                let world =
                    if changed
                    then World.updateEntityState Some propertyName property.PropertyValue entity world
                    else world
                (true, world)
            | (false, _, world) -> (false, world)

        static member internal setEntityProperty propertyName property entity world =
            match World.trySetEntityProperty propertyName property entity world with
            | (true, world) -> world
            | (false, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal attachEntityProperty propertyName property entity world =
            if World.getEntityExists entity world then
                World.updateEntityState
                    (fun entityState -> Some (EntityState.attachProperty propertyName property entityState))
                    propertyName property.PropertyValue entity world
            else failwith ("Cannot attach entity property '" + propertyName + "'; entity '" + entity.Name + "' is not found.")

        static member internal detachEntityProperty propertyName entity world =
            if World.getEntityExists entity world then
                World.updateEntityStateWithoutEvent
                    (fun entityState -> Some (EntityState.detachProperty propertyName entityState))
                    entity world |>
                snd
            else failwith ("Cannot detach entity property '" + propertyName + "'; entity '" + entity.Name + "' is not found.")

        static member internal getEntityDefaultOverlayName dispatcherName world =
            match Option.flatten (World.tryFindRoutedOverlayNameOpt dispatcherName world) with
            | Some _ as opt -> opt
            | None -> Some dispatcherName

        static member internal getEntityBoundsOverflow entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            v4BoundsOverflow transform.Position transform.Size entityState.Overflow

        static member internal getEntityBoundsMax entity world =
            let entityState = World.getEntityState entity world
            World.getEntityStateBoundsMax entityState

        static member internal getEntityInView entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            if not transform.Omnipresent then
                let boundsOverflow = v4BoundsOverflow transform.Position transform.Size entityState.Overflow
                World.isBoundsInView transform.Absolute boundsOverflow world
            else true

        static member internal getEntityQuickSize (entity : Entity) world =
            let dispatcher = World.getEntityDispatcher entity world
            let facets = World.getEntityFacets entity world
            let quickSize = dispatcher.GetQuickSize (entity, world)
            Array.fold
                (fun (maxSize : Vector2) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector2
                        (Math.Max (quickSize.X, maxSize.X),
                         Math.Max (quickSize.Y, maxSize.Y)))
                quickSize
                facets

        static member internal getEntitySortingPriority entity world =
            let entityState = World.getEntityState entity world
            { SortElevation = entityState.Transform.Elevation
              SortPositionY = entityState.Transform.Position.Y
              SortTarget = entity }

        static member internal updateEntityPublishUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishUpdates entity (atooa entity.UpdateEventCached) world

#if !DISABLE_ENTITY_POST_UPDATE
        static member internal updateEntityPublishPostUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishPostUpdates entity (atooa entity.PostUpdateEventCached) world
#endif

        static member internal updateEntityPublishFlags entity world =
            let world = World.updateEntityPublishUpdateFlag entity world
#if !DISABLE_ENTITY_POST_UPDATE
            let world = World.updateEntityPublishPostUpdateFlag entity world
#endif
            world

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
            let world = World.updateEntityPublishFlags entity world
            let eventTrace = EventTrace.debug "World" "registerEntity" "" EventTrace.empty
            let eventAddresses = EventSystemDelegate.getEventAddresses1 (rtoa<unit> [|"Register"; "Event"|] --> entity)
            let world = Array.fold (fun world eventAddress -> World.publish () eventAddress eventTrace entity world) world eventAddresses
            world

        static member internal unregisterEntity (entity : Entity) world =
            let eventTrace = EventTrace.debug "World" "unregisteringEntity" "" EventTrace.empty
            let eventAddresses = EventSystemDelegate.getEventAddresses1 (rtoa<unit> [|"Unregistering"; "Event"|] --> entity)
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

                // mutate entity tree if entity is selected
                let world =
                    if WorldModule.isSelected entity world then
                        let entityTree =
                            MutantCache.mutateMutant
                                (fun () -> oldWorld.Dispatchers.RebuildEntityTree oldWorld)
                                (fun entityTree ->
                                    let entityState = World.getEntityState entity world
                                    let entityMaxBounds = World.getEntityStateBoundsMax entityState
                                    SpatialTree.addElement (entityState.Omnipresent || entityState.Absolute) entityMaxBounds entity entityTree
                                    entityTree)
                                (World.getEntityTree world)
                        World.setEntityTree entityTree world
                    else world

                // register entity if needed
                if isNew
                then World.registerEntity entity world
                else world

            // handle failure
            else failwith ("Adding an entity that the world already contains at address '" + scstring entity.EntityAddress + "'.")

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =

            // attempt to remove from destruction list
            let world = World.tryRemoveSimulantFromDestruction entity world

            // ensure entity exists in the world
            if World.getEntityExists entity world then

                // unregister entity
                let world = World.unregisterEntity entity world

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // mutate entity tree if entity is selected
                let world =
                    if WorldModule.isSelected entity world then
                        let entityTree =
                            MutantCache.mutateMutant
                                (fun () -> oldWorld.Dispatchers.RebuildEntityTree world)
                                (fun entityTree ->
                                    let entityState = World.getEntityState entity oldWorld
                                    let entityMaxBounds = World.getEntityStateBoundsMax entityState
                                    SpatialTree.removeElement (entityState.Omnipresent || entityState.Absolute) entityMaxBounds entity entityTree
                                    entityTree)
                                (World.getEntityTree world)
                        World.setEntityTree entityTree world
                    else world

                // remove cached entity event addresses
                EventSystemDelegate.cleanEventAddressCache entity.EntityAddress
                
                // invalidate entity state
                let entityState = World.getEntityState entity world
                entityState.Invalidated <- true

                // remove the entity from the world
                let world = World.removeEntityState entity world
                world

            // pass
            else world

        /// Create an entity and add it to the world.
        [<FunctionBinding "createEntity">]
        static member createEntity5 dispatcherName nameOpt overlayDescriptor (group : Group) world =

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")

            // compute the optional overlay name
            let overlayNameOpt =
                match overlayDescriptor with
                | NoOverlay -> None
                | RoutedOverlay -> Option.flatten (World.tryFindRoutedOverlayNameOpt dispatcherName world)
                | DefaultOverlay -> Some (Option.getOrDefault dispatcherName (Option.flatten (World.tryFindRoutedOverlayNameOpt dispatcherName world)))
                | ExplicitOverlay overlayName -> Some overlayName

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make nameOpt (World.getStandAlone world) overlayNameOpt dispatcher

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
                    if dispatcherName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay id dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // make entity address
            let entityAddress = group.GroupAddress <-- ntoa<Entity> entityState.Name

            // apply publish bindings state
            match World.tryGetKeyedValue<UMap<Entity Address, int>> EntityBindingCountsId world with
            | Some entityBindingCounts -> if UMap.containsKey entityAddress entityBindingCounts then entityState.PublishChangeBindings <- true
            | None -> ()

            // apply publish changes state
            match World.tryGetKeyedValue<UMap<Entity Address, int>> EntityChangeCountsId world with
            | Some entityChangeCounts -> if UMap.containsKey entityAddress entityChangeCounts then entityState.PublishChangeEvents <- true
            | None -> ()

            // add entity's state to world
            let entity = Entity entityAddress
            let world =
                if World.getEntityExists entity world then
                    if World.getEntityDestroying entity world
                    then World.destroyEntityImmediate entity world
                    else failwith ("Entity '" + scstring entity + " already exists and cannot be created."); world
                else world
            let world = World.addEntity false entityState entity world
            (entity, world)

        /// Create an entity from an simulant descriptor.
        static member createEntity4 overlayDescriptor descriptor group world =
            let (entity, world) =
                World.createEntity5 descriptor.SimulantDispatcherName descriptor.SimulantNameOpt overlayDescriptor group world
            let world =
                List.fold (fun world (propertyName, property) ->
                    World.setEntityProperty propertyName property entity world)
                    world descriptor.SimulantProperties
            let world =
                if WorldModule.isSelected entity world
                then World.propagateEntityPhysics entity world
                else world
            (entity, world)

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> nameOpt overlayNameDescriptor group world =
            World.createEntity5 typeof<'d>.Name nameOpt overlayNameDescriptor group world

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor nameOpt (group : Group) world =

            // make the dispatcher
            let dispatcherName = entityDescriptor.EntityDispatcherName
            let dispatchers = World.getEntityDispatchers world
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    Log.info ("Could not locate dispatcher '" + dispatcherName + "'.")
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher =
                        match Map.tryFind dispatcherName dispatchers with
                        | Some dispatcher -> dispatcher
                        | None -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'. Did you forget to provide this dispatcher from your NuPlugin?")
                    (dispatcherName, dispatcher)

            // get the default overlay name option
            let defaultOverlayNameOpt = World.getEntityDefaultOverlayName dispatcherName world

            // make the bare entity state with name as id
            let entityState = EntityState.make None (World.getStandAlone world) defaultOverlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let overlayer = World.getOverlayer world
            let entityState = Reflection.tryReadOverlayNameOptToTarget id entityDescriptor.EntityProperties entityState
            let entityState = if Option.isNone entityState.OverlayNameOpt then { entityState with OverlayNameOpt = defaultOverlayNameOpt } else entityState
            let entityState =
                match (defaultOverlayNameOpt, entityState.OverlayNameOpt) with
                | (Some defaultOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames id defaultOverlayName overlayName entityState overlayer overlayer
                | (_, _) -> entityState

            // read the entity state's facet names
            let entityState = Reflection.readFacetNamesToTarget id entityDescriptor.EntityProperties entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties id entityState.Dispatcher entityState world
            
            // synchronize the entity state's facets (and attach their properties)
            let entityState =
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> Log.debug error; entityState

            // attempt to apply the entity state's overlay
            let entityState =
                match entityState.OverlayNameOpt with
                | Some overlayName ->
                    // OPTIMIZATION: applying overlay only when it will change something
                    if dispatcherName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay id dispatcherName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // read the entity state's values
            let entityState = Reflection.readPropertiesToTarget id entityDescriptor.EntityProperties entityState

            // apply the name if one is provided
            let entityState =
                match nameOpt with
                | Some name -> { entityState with Name = name }
                | None -> entityState

            // try to add entity state to the world
            let entity = Entity (group.GroupAddress <-- ntoa<Entity> entityState.Name)
            let world =
                if World.getEntityExists entity world then
                    if World.getEntityDestroying entity world
                    then World.destroyEntityImmediate entity world
                    else failwith ("Entity '" + scstring entity + " already exists and cannot be created."); world
                else world
            let world = World.addEntity true entityState entity world
            (entity, world)

        /// Read an entity from a file.
        [<FunctionBinding>]
        static member readEntityFromFile (filePath : string) nameOpt group world =
            let entityDescriptorStr = File.ReadAllText filePath
            let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
            World.readEntity entityDescriptor nameOpt group world

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let overlayer = World.getOverlayer world
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.Dispatcher
            let entityDescriptor = { entityDescriptor with EntityDispatcherName = entityDispatcherName }
            let entityFacetNames = World.getEntityFacetNamesReflectively entityState
            let overlaySymbolsOpt =
                match entityState.OverlayNameOpt with
                | Some overlayName -> Some (Overlayer.getOverlaySymbols overlayName entityFacetNames overlayer)
                | None -> None
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OverlayNameOpt" && propertyType = typeof<string option> then
                    let defaultOverlayNameOpt = World.getEntityDefaultOverlayName entityDispatcherName world
                    defaultOverlayNameOpt <> (propertyValue :?> string option)
                else
                    match overlaySymbolsOpt with
                    | Some overlaySymbols -> Overlayer.shouldPropertySerialize propertyName propertyType entityState overlaySymbols
                    | None -> true
            let getEntityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            { entityDescriptor with EntityProperties = getEntityProperties }

        /// Reassign an entity's identity and / or group. Note that since this destroys the reassigned entity
        /// immediately, you should not call this inside an event handler that involves the reassigned entity itself.
        static member reassignEntityImmediate entity nameOpt (group : Group) world =
            let entityState = World.getEntityState entity world
            let world = World.destroyEntityImmediate entity world
            let (id, name) = Gen.idAndNameIf nameOpt
            let entityState = { entityState with Id = id; Name = name } // no need to diverge here
            let transmutedEntity = Entity (group.GroupAddress <-- ntoa<Entity> name)
            let world = World.addEntity false entityState transmutedEntity world
            (transmutedEntity, world)

        /// Reassign an entity's identity and / or group.
        [<FunctionBinding>]
        static member reassignEntity entity nameOpt group world =
            World.frame (World.reassignEntityImmediate entity nameOpt group >> snd) world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOverlayNameOpt overlayNameOpt entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOverlayNameOpt = oldEntityState.OverlayNameOpt
            let entityState =
                if oldEntityState.ShouldMutate then
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
                let oldOmnipresent = oldEntityState.Omnipresent
                let oldAbsolute = oldEntityState.Absolute
                let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
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
                let oldOmnipresent = oldEntityState.Omnipresent
                let oldAbsolute = oldEntityState.Absolute
                let oldBoundsMax = if not oldEntityState.Omnipresent then World.getEntityStateBoundsMax oldEntityState else v4Zero
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax entity oldWorld world
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
            let properties = World.getProperties state
            properties |> Array.ofList |> Array.map a_c

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

        static member internal updateEntityInEntityTree oldOmnipresent oldAbsolute oldBoundsMax (entity : Entity) oldWorld world =

            // only do this when entity is selected
            if WorldModule.isSelected entity world then

                // OPTIMIZATION: work with the entity state directly to avoid function call overheads
                let entityState = World.getEntityState entity world
                let oldOmnipresent = oldOmnipresent || oldAbsolute
                let newOmnipresent = entityState.Omnipresent || entityState.Absolute
                if newOmnipresent <> oldOmnipresent then

                    // remove and add entity in entity tree
                    let entityTree =
                        MutantCache.mutateMutant
                            (fun () -> oldWorld.Dispatchers.RebuildEntityTree oldWorld)
                            (fun entityTree ->
                                let newBoundsMax = World.getEntityStateBoundsMax entityState
                                SpatialTree.removeElement oldOmnipresent oldBoundsMax entity entityTree
                                SpatialTree.addElement newOmnipresent newBoundsMax entity entityTree
                                let entityBoundsMax = World.getEntityStateBoundsMax entityState
                                SpatialTree.updateElement oldBoundsMax entityBoundsMax entity entityTree
                                entityTree)
                            (World.getEntityTree world)
                    World.setEntityTree entityTree world

                // OPTIMIZATION: only update when entity is not omnipresent
                elif not newOmnipresent then

                    // update entity in entity tree
                    let entityTree =
                        MutantCache.mutateMutant
                            (fun () -> oldWorld.Dispatchers.RebuildEntityTree oldWorld)
                            (fun entityTree ->
                                let entityBoundsMax = World.getEntityStateBoundsMax entityState
                                SpatialTree.updateElement oldBoundsMax entityBoundsMax entity entityTree
                                entityTree)
                            (World.getEntityTree world)
                    World.setEntityTree entityTree world

                // just world
                else world
            else world

        /// Copy an entity to the clipboard.
        static member copyEntityToClipboard entity world =
            let entityState = World.getEntityState entity world
            Clipboard <- Some (entityState :> obj)

        /// Cut an entity to the clipboard.
        static member cutEntityToClipboard entity world =
            World.copyEntityToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        static member pasteEntityFromClipboard atMouse rightClickPosition positionSnap rotationSnap (group : Group) world =
            match Clipboard with
            | Some entityStateObj ->
                let entityState = entityStateObj :?> EntityState
                let id = Gen.id
                let name = Gen.name
                let entityState = { entityState with Id = id; Name = name } // no need to diverge here
                let position =
                    if atMouse
                    then World.mouseToWorld entityState.Absolute rightClickPosition world
                    else World.mouseToWorld entityState.Absolute (World.getEyeSize world * 0.5f) world
                let transform = { EntityState.getTransform entityState with Position = position }
                let transform = Math.snapTransform positionSnap rotationSnap transform
                let entityState = EntityState.setTransformByRef (&transform, entityState)
                let entity = Entity (group.GroupAddress <-- ntoa<Entity> name)
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

    /// Initialize property getters.
    let private initGetters () =
        EntityGetters.Add ("Dispatcher", fun entity world -> { PropertyType = typeof<EntityDispatcher>; PropertyValue = World.getEntityDispatcher entity world })
        EntityGetters.Add ("Facets", fun entity world -> { PropertyType = typeof<Facet array>; PropertyValue = World.getEntityFacets entity world })
        EntityGetters.Add ("Transform", fun entity world -> { PropertyType = typeof<Transform>; PropertyValue = (World.getEntityState entity world).Transform })
        EntityGetters.Add ("Bounds", fun entity world -> { PropertyType = typeof<Vector4>; PropertyValue = World.getEntityBounds entity world })
        EntityGetters.Add ("Position", fun entity world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEntityPosition entity world })
        EntityGetters.Add ("Center", fun entity world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEntityCenter entity world })
        EntityGetters.Add ("Bottom", fun entity world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEntityBottom entity world })
        EntityGetters.Add ("Size", fun entity world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEntitySize entity world })
        EntityGetters.Add ("Rotation", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityRotation entity world })
        EntityGetters.Add ("Elevation", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevation entity world })
        EntityGetters.Add ("Omnipresent", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityOmnipresent entity world })
        EntityGetters.Add ("Absolute", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAbsolute entity world })
        EntityGetters.Add ("Model", fun entity world -> let designerProperty = World.getEntityModelProperty entity world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
        EntityGetters.Add ("Overflow", fun entity world -> { PropertyType = typeof<Vector2>; PropertyValue = World.getEntityOverflow entity world })
        EntityGetters.Add ("Imperative", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityImperative entity world })
        EntityGetters.Add ("PublishChangeBindings", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeBindings entity world })
        EntityGetters.Add ("PublishChangeEvents", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeEvents entity world })
        EntityGetters.Add ("Enabled", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabled entity world })
        EntityGetters.Add ("Visible", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisible entity world })
        EntityGetters.Add ("AlwaysUpdate", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysUpdate entity world })
        EntityGetters.Add ("PublishUpdates", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishUpdates entity world })
        EntityGetters.Add ("PublishPostUpdates", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishPostUpdates entity world })
        EntityGetters.Add ("Persistent", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPersistent entity world })
        EntityGetters.Add ("Optimized", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityOptimized entity world })
        EntityGetters.Add ("ShouldMutate", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityShouldMutate entity world })
        EntityGetters.Add ("Destroying", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityDestroying entity world })
        EntityGetters.Add ("OverlayNameOpt", fun entity world -> { PropertyType = typeof<string option>; PropertyValue = World.getEntityOverlayNameOpt entity world })
        EntityGetters.Add ("FacetNames", fun entity world -> { PropertyType = typeof<string Set>; PropertyValue = World.getEntityFacetNames entity world })
        EntityGetters.Add ("CreationTimeStamp", fun entity world -> { PropertyType = typeof<int64>; PropertyValue = World.getEntityCreationTimeStamp entity world })
        EntityGetters.Add ("Name", fun entity world -> { PropertyType = typeof<string>; PropertyValue = World.getEntityName entity world })
        EntityGetters.Add ("Id", fun entity world -> { PropertyType = typeof<Guid>; PropertyValue = World.getEntityId entity world })

    /// Initialize property setters.
    let private initSetters () =
        EntitySetters.Add ("Transform", fun property entity world ->
            let transformOld = &(World.getEntityState entity world).Transform
            let mutable transform = property.PropertyValue :?> Transform
            if not (Transform.equalsByRef (&transform, &transformOld)) then (true, World.setEntityTransformByRef (&transform, entity, world)) else (false, world))
        EntitySetters.Add ("Bounds", fun property entity world -> if property.PropertyValue =/= World.getEntityBounds entity world then (true, World.setEntityBounds (property.PropertyValue :?> Vector4) entity world) else (false, world))
        EntitySetters.Add ("Position", fun property entity world -> if property.PropertyValue =/= World.getEntityPosition entity world then (true, World.setEntityPosition (property.PropertyValue :?> Vector2) entity world) else (false, world))
        EntitySetters.Add ("Center", fun property entity world -> if property.PropertyValue =/= World.getEntityCenter entity world then (true, World.setEntityCenter (property.PropertyValue :?> Vector2) entity world) else (false, world))
        EntitySetters.Add ("Bottom", fun property entity world -> if property.PropertyValue =/= World.getEntityBottom entity world then (true, World.setEntityBottom (property.PropertyValue :?> Vector2) entity world) else (false, world))
        EntitySetters.Add ("Size", fun property entity world -> if property.PropertyValue =/= World.getEntitySize entity world then (true, World.setEntitySize (property.PropertyValue :?> Vector2) entity world) else (false, world))
        EntitySetters.Add ("Rotation", fun property entity world -> if property.PropertyValue =/= World.getEntityRotation entity world then (true, World.setEntityRotation (property.PropertyValue :?> single) entity world) else (false, world))
        EntitySetters.Add ("Elevation", fun property entity world -> if property.PropertyValue =/= World.getEntityElevation entity world then (true, World.setEntityElevation (property.PropertyValue :?> single) entity world) else (false, world))
        EntitySetters.Add ("Omnipresent", fun property entity world -> if property.PropertyValue =/= World.getEntityOmnipresent entity world then (true, World.setEntityOmnipresent (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("Absolute", fun property entity world -> if property.PropertyValue =/= World.getEntityAbsolute entity world then (true, World.setEntityAbsolute (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("Model", fun property entity world -> if property.PropertyValue =/= World.getEntityModel entity world then (true, World.setEntityModelProperty { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } entity world) else (false, world))
        EntitySetters.Add ("Overflow", fun property entity world -> if property.PropertyValue =/= World.getEntityOverflow entity world then (true, World.setEntityOverflow (property.PropertyValue :?> Vector2) entity world) else (false, world))
        EntitySetters.Add ("Imperative", fun property entity world -> if property.PropertyValue =/= World.getEntityImperative entity world then (true, World.setEntityImperative (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("Enabled", fun property entity world -> if property.PropertyValue =/= World.getEntityEnabled entity world then (true, World.setEntityEnabled (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("Visible", fun property entity world -> if property.PropertyValue =/= World.getEntityVisible entity world then (true, World.setEntityVisible (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("AlwaysUpdate", fun property entity world -> if property.PropertyValue =/= World.getEntityAlwaysUpdate entity world then (true, World.setEntityAlwaysUpdate (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("Persistent", fun property entity world -> if property.PropertyValue =/= World.getEntityPersistent entity world then (true, World.setEntityPersistent (property.PropertyValue :?> bool) entity world) else (false, world))
        EntitySetters.Add ("FacetNames", fun _ _ world -> (false, world)) // TODO: consider logging an error?

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()