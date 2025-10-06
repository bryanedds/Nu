// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Collections.Frozen
open System.IO
open System.Numerics
open Prime

/// Entity functions for the world (1/2).
[<AutoOpen>]
module WorldModuleEntity =

    /// Dynamic property getter and setter.
    type private PropertyGetter = Entity -> World -> Property
    type private PropertySetter = Property -> Entity -> World -> bool

    /// Dynamic property getters / setters.
    let mutable private EntityGetters = Unchecked.defaultof<FrozenDictionary<string, PropertyGetter>>
    let mutable private EntitySetters = Unchecked.defaultof<FrozenDictionary<string, PropertySetter>>

    /// Entity change (publishing) count key.
    let internal EntityChangeCountsKey = string Gen.id

    /// OPTIMIZATION: cache layout facet type for quick containment check.
    let mutable internal LayoutFacetType = Unchecked.defaultof<Type>

    type World with

        static member private entityStateFinder (entity : Entity) world =
            let entityStateOpt = entity.EntityStateOpt
            if isNull (entityStateOpt :> obj) || entityStateOpt.Invalidated then
                match world.EntityStates.TryGetValue entity with
                | (true, entityState) ->
                    entity.EntityStateOpt <- entityState
                    entityState
                | (false, _) -> Unchecked.defaultof<EntityState>
            else entityStateOpt

        static member private entityStateAdder entityState (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 4
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            match world.Simulants.TryGetValue parent with
            | (true, entitiesOpt) ->
                match entitiesOpt with
                | Some entities ->
                    entities.Add entity |> ignore<bool>
                    world.Simulants.[parent] <- Some entities
                | None ->
                    let entities = hashSetPlus HashIdentity.Structural [entity :> Simulant]
                    world.Simulants.[parent] <- Some entities
            | (false, _) -> failwith ("Cannot add entity '" + scstring entity + "' to non-existent parent '" + scstring parent + "'.")
            if not (world.Simulants.ContainsKey entity) then world.Simulants.[entity] <- None
            world.EntityStates.[entity] <- entityState

        static member private entityStateRemover (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 4
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            match world.Simulants.TryGetValue parent with
            | (true, entitiesOpt) ->
                match entitiesOpt with
                | Some entities ->
                    entities.Remove entity |> ignore<bool>
                    if entities.Count = 0
                    then world.Simulants[parent] <- None
                    else world.Simulants.[parent] <- Some entities
                | None -> ()
            | (false, _) -> ()
            world.Simulants.Remove entity |> ignore<bool>
            world.EntityStates.Remove entity |> ignore<bool>

        static member private entityStateSetter entityState (entity : Entity) world =
#if DEBUG
            if not (world.EntityStates.ContainsKey entity) then
                failwith ("Cannot set the state of a non-existent entity '" + scstring entity + "'")
#endif
            world.EntityStates.[entity] <- entityState

        static member private addEntityState (entityState : EntityState) (entity : Entity) world =

            // apply publish change events state
            let entityAddress = entity.EntityAddress
            match World.tryGetKeyedValueFast<Dictionary<Entity Address, int>> (EntityChangeCountsKey, world) with
            | (true, entityChangeCounts) -> if entityChangeCounts.ContainsKey entityAddress then entityState.PublishChangeEvents <- true
            | (false, _) -> ()

            // apply mounted state
            entityState.Mounted <- world.EntityMounts.ContainsKey entity 

            // add entity state to world
            World.entityStateAdder entityState entity world

        static member private removeEntityState (entity : Entity) world =
            World.entityStateRemover entity world

        static member private publishEntityChange propertyName (previousValue : obj) (propertyValue : obj) publishChangeEvents (entity : Entity) world =
            if publishChangeEvents then
                let changeData = { Name = propertyName; Previous = previousValue; Value = propertyValue }
                let entityNames = Address.getNames entity.EntityAddress
                // OPTIMIZATION: this works together with RigidBodyFacet to reduce the bookkeeping footprint of its
                // subscriptions. This does have some run-time performance cost associated with it, however.
                if Constants.Physics.BodyPropertyAffectingPropertyNames.Contains propertyName then
                    let changeEventAddress = rtoa<ChangeData> (Array.append [|Constants.Lens.ChangeName; "BodyPropertiesAffecting"; Constants.Lens.EventName|] entityNames)
                    let eventTrace = EventTrace.debug "World" "publishEntityChange" "BodyPropertiesAffecting" EventTrace.empty
                    World.publishPlus changeData changeEventAddress eventTrace entity false false world
                let changeEventAddress = rtoa<ChangeData> (Array.append [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName|] entityNames)
                let eventTrace = EventTrace.debug "World" "publishEntityChange" "" EventTrace.empty
                World.publishPlus changeData changeEventAddress eventTrace entity false false world

        static member inline internal getEntityStateOpt entity world =
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

        static member internal getEntityXtension entity world =
            let entityState = World.getEntityState entity world
            entityState.Xtension

        static member private publishEntityChanges entity world =
            let entityState = World.getEntityState entity world
            let properties = World.getSimulantStateProperties entityState
            let publishChangeEvents = entityState.PublishChangeEvents
            if publishChangeEvents then
                for (propertyName, _, propertyValue) in properties do
                    let entityState = World.getEntityState entity world
                    let publishChangeEvents = entityState.PublishChangeEvents
                    World.publishEntityChange propertyName propertyValue propertyValue publishChangeEvents entity world

        static member internal publishTransformEvents (transformOld : Transform byref, transformNew : Transform byref, is2d, publishChangeEvents, entity : Entity, world) =
            if publishChangeEvents then
                let positionChanged = v3Neq transformNew.Position transformOld.Position
                let rotationChanged = quatNeq transformNew.Rotation transformOld.Rotation
                let scaleChanged = v3NeqApprox transformNew.Scale transformOld.Scale 0.0001f // NOTE: just guessing at epsilon...
                let offsetChanged = v3Neq transformNew.Offset transformOld.Offset
                let sizeChanged = v3Neq transformNew.Size transformOld.Size
                let elevationChanged = transformNew.Elevation <> transformOld.Elevation
                let overflowChanged = transformNew.Overflow <> transformOld.Overflow
                World.publishEntityChange (nameof Transform) () () publishChangeEvents entity world // OPTIMIZATION: eliding data for computed change events for speed.
                if is2d then
                    let perimeterChanged = positionChanged || scaleChanged || offsetChanged || sizeChanged
                    let boundsChanged = perimeterChanged || rotationChanged
                    if boundsChanged then
                        World.publishEntityChange Constants.Engine.BoundsPropertyName transformOld.Bounds2d transformNew.Bounds2d publishChangeEvents entity world
                        if perimeterChanged then
                            World.publishEntityChange (nameof transformNew.Perimeter) transformOld.Perimeter transformNew.Perimeter publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterUnscaled) transformOld.PerimeterUnscaled transformNew.PerimeterUnscaled publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterCenter) transformOld.PerimeterCenter transformNew.PerimeterCenter publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterBottom) transformOld.PerimeterBottom transformNew.PerimeterBottom publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterBottomLeft) transformOld.PerimeterBottomLeft transformNew.PerimeterBottomLeft publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterMin) transformOld.PerimeterMin transformNew.PerimeterMin publishChangeEvents entity world
                            World.publishEntityChange (nameof transformNew.PerimeterMax) transformOld.PerimeterMax transformNew.PerimeterMax publishChangeEvents entity world
                        if positionChanged then World.publishEntityChange (nameof transformNew.Position) transformOld.Position transformNew.Position publishChangeEvents entity world
                        if scaleChanged then World.publishEntityChange (nameof transformNew.Scale) transformOld.Scale transformNew.Scale publishChangeEvents entity world
                        if offsetChanged then World.publishEntityChange (nameof transformNew.Offset) transformOld.Offset transformNew.Offset publishChangeEvents entity world
                        if sizeChanged then World.publishEntityChange (nameof transformNew.Size) transformOld.Size transformNew.Size publishChangeEvents entity world
                else
                    let boundsChanged = positionChanged || rotationChanged || scaleChanged || offsetChanged || sizeChanged
                    if boundsChanged then
                        World.publishEntityChange Constants.Engine.BoundsPropertyName transformOld.Bounds3d transformNew.Bounds3d publishChangeEvents entity world
                        if positionChanged then World.publishEntityChange (nameof transformNew.Position) transformOld.Position transformNew.Position publishChangeEvents entity world
                        if scaleChanged then World.publishEntityChange (nameof transformNew.Scale) transformOld.Scale transformNew.Scale publishChangeEvents entity world
                        if offsetChanged then World.publishEntityChange (nameof transformNew.Offset) transformOld.Offset transformNew.Offset publishChangeEvents entity world
                        if sizeChanged then World.publishEntityChange (nameof transformNew.Size) transformOld.Size transformNew.Size publishChangeEvents entity world
                if rotationChanged then
                    World.publishEntityChange (nameof transformNew.Rotation) transformOld.Rotation transformNew.Rotation publishChangeEvents entity world
                    World.publishEntityChange (nameof transformNew.Angles) () () publishChangeEvents entity world
                    World.publishEntityChange (nameof transformNew.Degrees) () () publishChangeEvents entity world
                if elevationChanged then
                    World.publishEntityChange (nameof transformNew.Elevation) transformOld.Elevation transformNew.Elevation publishChangeEvents entity world
                if overflowChanged then
                    World.publishEntityChange (nameof transformNew.Overflow) transformOld.Overflow transformNew.Overflow publishChangeEvents entity world

        static member internal getEntityExists entity world =
            notNull (World.getEntityStateOpt entity world :> obj)

        static member internal getEntitySelected (entity : Entity) world =
            let gameState = World.getGameState Game.Handle world
            match gameState.SelectedScreenOpt with
            | Some selectedScreen when entity.Screen.Name = selectedScreen.Name -> true
            | _ -> false

        static member internal computeEntityPresenceOverride entity world =
            let entityState = World.getEntityState entity world
            let dispatcherOverride =
                if not entityState.Absolute then
                    match entityState.Dispatcher.PresenceOverride with
                    | ValueSome _ as override_ -> override_
                    | _ -> ValueNone
                else ValueSome Omnipresent
            let facetOverrides = Array.map (fun (facet : Facet) -> facet.PresenceOverride) entityState.Facets
            Presence.highestOverride2 dispatcherOverride facetOverrides

        static member internal getEntityModelProperty entity world =
            let entityState = World.getEntityState entity world
            entityState.Model

        static member internal setEntityModelProperty initializing reinitializing (value : DesignerProperty) entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                entityState.Model <- { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }
                entityState.Dispatcher.TrySynchronize (initializing, reinitializing, entity, world)
                World.publishEntityChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue entityState.PublishChangeEvents entity world
                true
            else false

        static member internal getEntityModelGeneric<'a> entity world =
            let entityState = World.getEntityState entity world
            match entityState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                let modelSymbol = valueToSymbol modelObj
                try let model = symbolToValue modelSymbol
                    entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                    model
                with _ ->
                    Log.warn "Could not convert existing entity model value to new type; attempting to use fallback model value instead."
                    match entityState.Dispatcher.TryGetFallbackModel<'a> (modelSymbol, entity, world) with
                    | None -> typeof<'a>.GetDefaultValue () :?> 'a
                    | Some model ->
                        entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = model }
                        model

        static member internal setEntityModelGeneric<'a> initializing reinitializing (value : 'a) entity world =
            let entityState = World.getEntityState entity world
            let valueObj = value :> obj
            let previous = entityState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = valueObj }
                entityState.Dispatcher.TrySynchronize (initializing, reinitializing, entity, world)
                World.publishEntityChange Constants.Engine.ModelPropertyName previous.DesignerValue value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal getEntityContent entity world =
            let screenState = World.getEntityState entity world
            screenState.Content

        static member internal setEntityContent (value : EntityContent) entity world =
            let screenState = World.getEntityState entity world
            screenState.Content <- value

        static member internal getEntityDispatcher entity world = (World.getEntityState entity world).Dispatcher
        static member internal getEntityFacets entity world = (World.getEntityState entity world).Facets
        static member internal getEntityPerimeterCenter entity world = (World.getEntityState entity world).Transform.PerimeterCenter
        static member internal getEntityPerimeterBottom entity world = (World.getEntityState entity world).Transform.PerimeterBottom
        static member internal getEntityPerimeterBottomLeft entity world = (World.getEntityState entity world).Transform.PerimeterBottomLeft
        static member internal getEntityPerimeterMin entity world = (World.getEntityState entity world).Transform.PerimeterMin
        static member internal getEntityPerimeterMax entity world = (World.getEntityState entity world).Transform.PerimeterMax
        static member internal getEntityPerimeterCenterLocal entity world = (World.getEntityState entity world).PerimeterCenterLocal
        static member internal getEntityPerimeterBottomLocal entity world = (World.getEntityState entity world).PerimeterBottomLocal
        static member internal getEntityPerimeterBottomLeftLocal entity world = (World.getEntityState entity world).PerimeterBottomLeftLocal
        static member internal getEntityPerimeterMinLocal entity world = (World.getEntityState entity world).PerimeterMinLocal
        static member internal getEntityPerimeterMaxLocal entity world = (World.getEntityState entity world).PerimeterMaxLocal
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
        static member internal getEntityDegreesLocal entity world = Math.RadiansToDegrees3d (World.getEntityState entity world).AnglesLocal
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal getEntityElevation entity world = (World.getEntityState entity world).Elevation
        static member internal getEntityElevationLocal entity world = (World.getEntityState entity world).ElevationLocal
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Transform.Overflow
        static member internal getEntityPresence entity world = (World.getEntityState entity world).Presence
        static member internal getEntityPresenceOverride entity world = (World.getEntityState entity world).PresenceOverride
        static member internal getEntityMountOpt entity world = (World.getEntityState entity world).MountOpt
        static member internal getEntityPropagationSourceOpt entity world = (World.getEntityState entity world).PropagationSourceOpt
        static member internal getEntityAbsolute entity world = (World.getEntityState entity world).Absolute
        static member internal getEntityPublishChangeEvents entity world = (World.getEntityState entity world).PublishChangeEvents
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal getEntityEnabledLocal entity world = (World.getEntityState entity world).EnabledLocal
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal getEntityVisibleLocal entity world = (World.getEntityState entity world).VisibleLocal
        static member internal getEntityCastShadow entity world = (World.getEntityState entity world).CastShadow
        static member internal getEntityPickable entity world = (World.getEntityState entity world).Pickable
        static member internal getEntityAlwaysUpdate entity world = (World.getEntityState entity world).AlwaysUpdate
        static member internal getEntityAlwaysRender entity world = (World.getEntityState entity world).AlwaysRender
        static member internal getEntityPublishUpdates entity world = (World.getEntityState entity world).PublishUpdates
        static member internal getEntityProtected entity world = (World.getEntityState entity world).Protected
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal getEntityMounted entity world = (World.getEntityState entity world).Mounted
        static member internal getEntityIs2d entity world = (World.getEntityState entity world).Is2d
        static member internal getEntityIs3d entity world = (World.getEntityState entity world).Is3d
        static member internal getEntityStatic entity world = (World.getEntityState entity world).Static
        static member internal getEntityPhysical entity world = (World.getEntityState entity world).Physical
        static member internal getEntityLightProbe entity world = (World.getEntityState entity world).LightProbe
        static member internal getEntityLight entity world = (World.getEntityState entity world).Light
        static member internal getEntityOptimized entity world = (World.getEntityState entity world).Optimized
        static member internal getEntityDestroying (entity : Entity) world = (World.getDestructionList world).Contains entity
        static member internal getEntityOverlayNameOpt entity world = (World.getEntityState entity world).OverlayNameOpt
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityPropagatedDescriptorOpt entity world = (World.getEntityState entity world).PropagatedDescriptorOpt
        static member internal getEntityOrder entity world = (World.getEntityState entity world).Order
        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntitySurnames entity world = (World.getEntityState entity world).Surnames
        static member internal getEntityName entity world = (World.getEntityState entity world).Surnames |> Array.last

        static member internal setEntityPublishChangeEvents value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishChangeEvents
            if value <> previous then
                entityState.PublishChangeEvents <- value
                World.publishEntityChange (nameof entityState.PublishChangeEvents) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityPublishUpdates value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishUpdates
            if value <> previous then
                entityState.PublishUpdates <- value
                World.publishEntityChange (nameof entityState.PublishUpdates) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityProtected value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Protected
            if value <> previous then
                entityState.Protected <- value
                World.publishEntityChange (nameof entityState.Protected) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityPersistent value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Persistent
            if value <> previous then
                entityState.Persistent <- value
                World.publishEntityChange (nameof entityState.Persistent) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityMounted value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Mounted
            if value <> previous then
                entityState.Mounted <- value
                World.publishEntityChange (nameof entityState.Mounted) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityPropagatedDescriptorOpt value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PropagatedDescriptorOpt
            if value <> previous then
                entityState.PropagatedDescriptorOpt <- value
                World.publishEntityChange (nameof entityState.PropagatedDescriptorOpt) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityOrder value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Order
            if value <> previous then
                entityState.Order <- value
                World.publishEntityChange (nameof entityState.Order) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member inline internal getEntityRotationMatrix entity world =
            (World.getEntityState entity world).Transform.RotationMatrix

        static member inline internal getEntityAffineMatrix entity world =
            (World.getEntityState entity world).Transform.AffineMatrix

        static member internal getEntityAffineMatrixLocal entity world =
            let entityState = World.getEntityState entity world
            Matrix4x4.CreateAffine (entityState.PositionLocal, entityState.RotationLocal, entityState.ScaleLocal)

        static member
#if !DEBUG
            inline
#endif
            internal getEntityTransform entity world =
            let entityState = World.getEntityState entity world
            Transform.cleanRotationMatrixInternal &entityState.Transform // OPTIMIZATION: ensure rotation matrix is clean so that redundant cleans don't happen when transform is handed out.
            entityState.Transform

        /// Check that an entity has any children.
        static member getEntityHasChildren (entity : Entity) world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue entity with
            | (true, childrenOpt) ->
                match childrenOpt with
                | Some children -> Seq.notEmpty children 
                | None -> false
            | (false, _) -> false

        /// Get all of the entities directly parented by an entity.
        static member getEntityChildren (entity : Entity) world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue entity with
            | (true, childrenOpt) ->
                match childrenOpt with
                | Some children -> Seq.map cast<Entity> children
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Traverse all of the entities directly parented by an entity.
        static member traverseEntityChildren effect (entity : Entity) (world : World) =
            let mounters = World.getEntityChildren entity world
            Seq.fold (fun world mounter -> effect entity mounter world) world mounters

        /// Get all of the entities descending from an entity.
        static member getEntityDescendants (entity : Entity) world =
            let simulants = World.getSimulants world
            seq {
                match simulants.TryGetValue entity with
                | (true, childrenOpt) ->
                    match childrenOpt with
                    | Some children ->
                        for child in children do
                            let childEntity = child :?> Entity
                            yield childEntity
                            yield! World.getEntityDescendants childEntity world
                    | None -> ()
                | (false, _) -> () }

        /// Get all of the entities parenting an entity.
        static member getEntityAncestors (entity : Entity) world =
            seq {
                match entity.Parent with
                | :? Entity as parent when World.getEntityExists parent world ->
                    yield parent
                    yield! World.getEntityAncestors parent world
                | _ -> () }

        /// Check that an entity should be allowed to mount another entity.
        static member getEntityAllowedToMount entity world =
            let mutable bodyTypeProperty = Unchecked.defaultof<Property>
            if  World.tryGetEntityProperty (Constants.Engine.BodyTypePropertyName, entity, world, &bodyTypeProperty) &&
                bodyTypeProperty.PropertyType = typeof<BodyType> then
                let bodyType = bodyTypeProperty.PropertyValue :?> BodyType
                bodyType = Static || bodyType = Kinematic || bodyType = KinematicCharacter
            else true

        /// Check that an entity has any other entities mounted on it.
        static member getEntityHasMounters entity world =
            match world.EntityMounts.TryGetValue entity with
            | (true, mounters) -> Seq.exists (flip World.getEntityExists world) mounters
            | (false, _) -> false

        /// Get all of the entities directly mounted on an entity.
        static member getEntityMounters entity world =
            match world.EntityMounts.TryGetValue entity with
            | (true, mounters) -> Seq.filter (flip World.getEntityExists world) mounters
            | (false, _) -> Seq.empty

        /// Traverse all of the entities directly mounted on an entity.
        static member traverseEntityMounters effect (entity : Entity) (world : World) =
            let mounters = world |> World.getEntityMounters entity |> SList.ofSeq // eager to avoid inconsistency
            for mounter in mounters do effect entity mounter world

        static member internal addEntityToMounts mountOpt entity (world : World) =
            match Option.bind (flip tryResolve entity) mountOpt with
            | Some mountNew ->
                match world.EntityMounts.TryGetValue mountNew with
                | (true, mounters) -> mounters.Add entity |> ignore<bool>
                | (false, _) ->
                    let mounters = hashSetPlus HashIdentity.Structural [entity]
                    world.EntityMounts.[mountNew] <- mounters
                    if World.getEntityExists mountNew world then World.setEntityMounted true mountNew world |> ignore<bool>
                let mountData = { Mount = mountNew; Mounter = entity }
                let eventTrace = EventTrace.debug "World" "addEntityToMounts" "" EventTrace.empty
                World.publishPlus mountData (Events.MountEvent --> mountNew) eventTrace entity false false world
            | None -> ()

        static member internal removeEntityFromMounts mountOpt entity (world : World) =
            match Option.bind (flip tryResolve entity) mountOpt with
            | Some mountOld ->
                match world.EntityMounts.TryGetValue mountOld with
                | (true, mounters) ->
                    mounters.Remove entity |> ignore<bool>
                    if mounters.Count = 0 then
                        world.EntityMounts.Remove mountOld |> ignore<bool>
                        if World.getEntityExists mountOld world then World.setEntityMounted false mountOld world |> ignore<bool>
                    else world.EntityMounts.[mountOld] <- mounters
                | (false, _) -> ()
                let mountData = { Mount = mountOld; Mounter = entity }
                let eventTrace = EventTrace.debug "World" "removeEntityFromMounts" "" EventTrace.empty
                World.publishPlus mountData (Events.UnmountEvent --> mountOld) eventTrace entity false false world
            | None -> ()

        static member internal propagateEntityAffineMatrix3 mount mounter world =
            let mounterState = World.getEntityState mounter world
            if  not mounterState.Physical || // OPTIMIZATION: skip call to getEntityAllowedToMount when non-physical.
                World.getEntityAllowedToMount mounter world then
                let mountState = World.getEntityState mount world
                let mutable transform = mounterState.Transform
                transform.Position <- Vector3.Transform (mounterState.PositionLocal, mountState.AffineMatrix)
                transform.Rotation <- mountState.Rotation * mounterState.RotationLocal
                transform.Scale <- mounterState.ScaleLocal * mountState.Scale
                World.setEntityTransformByRef (&transform, mounterState, mounter, world) |> ignore<bool>

        static member internal propagateEntityProperties3 mountOpt entity world =
            match Option.bind (flip tryResolve entity) mountOpt with
            | Some mount when World.getEntityExists mount world ->
                World.propagateEntityAffineMatrix3 mount entity world
                World.propagateEntityElevation3 mount entity world
                World.propagateEntityEnabled3 mount entity world
                World.propagateEntityVisible3 mount entity world
            | _ -> ()

        static member internal propagateEntityAffineMatrix entity world =
            World.traverseEntityMounters World.propagateEntityAffineMatrix3 entity world

        static member private updateEntityPublishEventFlag setFlag entity eventAddress world =
            let publishEvent =
                match Dictionary.tryFind eventAddress (World.getSubscriptions world) with
                | Some subscriptions ->
                    if subscriptions.Count = 0
                    then failwithumf () // NOTE: event system is defined to clean up all empty subscription entries
                    else true
                | None -> false
            if World.getEntityExists entity world
            then setFlag publishEvent entity world
            else false

        static member internal updateEntityPresenceOverride entity world =
            let entityState = World.getEntityState entity world
            let value = World.computeEntityPresenceOverride entity world
            let previous = entityState.PresenceOverride
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.PresenceOverride <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world

        static member getPropagationSources world =
            world.WorldExtension.PropagationTargets
            |> Seq.map _.Value
            |> Seq.concat
            |> Seq.choose (fun entity -> World.getEntityPropagationSourceOpt entity world)
            |> hashSetPlus HashIdentity.Structural
            |> Seq.filter (fun entity -> World.getEntityExists entity world)
            |> Seq.toArray

        /// Check that entity has entities to propagate its structure to.
        static member hasPropagationTargets entity world =
            match world.WorldExtension.PropagationTargets.TryGetValue entity with
            | (true, targets) -> targets.Count > 0
            | (false, _) -> false

        /// Find all the entities to which an entity may propagate its structure.
        static member getPropagationTargets entity world =
            match world.WorldExtension.PropagationTargets.TryGetValue entity with
            | (true, targets) -> seq targets
            | (false, _) -> Seq.empty

        static member internal addEntityToPropagationTargets source entity world =
            match world.WorldExtension.PropagationTargets.TryGetValue source with
            | (true, targets) ->
                targets.Add entity |> ignore<bool>
                world.WorldExtension.PropagationTargets.[source] <- targets
            | (false, _) ->
                if World.getEntityExists source world then
                    match World.getEntityPropagatedDescriptorOpt source world with
                    | None ->
                        let propagatedDescriptor = World.writeEntity false false EntityDescriptor.empty source world
                        World.setEntityPropagatedDescriptorOpt (Some propagatedDescriptor) source world |> ignore<bool>
                    | Some _ -> ()
                let targets = hashSetPlus HashIdentity.Structural [entity]
                world.WorldExtension.PropagationTargets.[source] <- targets

        static member internal removeEntityFromPropagationTargets source entity world =
            match world.WorldExtension.PropagationTargets.TryGetValue source with
            | (true, targets) ->
                targets.Remove entity |> ignore<bool>
                if targets.Count = 0
                then world.WorldExtension.PropagationTargets.Remove source |> ignore<bool>
                else world.WorldExtension.PropagationTargets.[source] <- targets
            | (false, _) -> ()

        static member internal updateEntityInPropagationTargets (sourceOldOpt : Entity option) sourceNewOpt entity world =
            if sourceOldOpt <> sourceNewOpt then
                match sourceOldOpt with
                | Some originOld -> World.removeEntityFromPropagationTargets originOld entity world
                | None -> ()
                match sourceNewOpt with
                | Some originNew -> World.addEntityToPropagationTargets originNew entity world
                | None -> ()

        static member internal setEntityMountOpt value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.MountOpt
            if value <> previous then

                // validate mount value
                match value with
                | Some mount ->
                    let mountAddress = Address.resolve mount entity.EntityAddress
                    let mountToEntity = Address.relate entity.EntityAddress mountAddress
                    if Array.notExists (function Constants.Address.ParentName | "???" | "??" | "?" -> true | _ -> false) mountToEntity.Names then
                        failwith "Cannot mount an entity circularly."
                | None -> ()

                // update property
                entityState.MountOpt <- value

                // update mount hierarchy
                World.removeEntityFromMounts previous entity world
                World.addEntityToMounts value entity world

                // propagate properties from mount
                World.propagateEntityProperties3 value entity world

                // publish change event unconditionally
                World.publishEntityChange Constants.Engine.MountOptPropertyName previous value true entity world

                // publish life cycle event unconditionally
                let eventTrace = EventTrace.debug "World" "setEntityMount" "" EventTrace.empty
                World.publishPlus (MountOptChangeData (previous, value, entity)) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world
                true

            else false

        static member internal setEntityPropagationSourceOpt value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PropagationSourceOpt
            if value <> previous then
                entityState.PropagationSourceOpt <- value
                World.updateEntityInPropagationTargets previous value entity world
                World.publishEntityChange (nameof entityState.PropagationSourceOpt) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityAbsolute value (entity : Entity) world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Absolute
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Absolute <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.Absolute) previous value entityState.PublishChangeEvents entity world
                World.updateEntityPresenceOverride entity world
                true
            else false

        static member internal setEntityStatic value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Static
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Static <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.Static) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityAlwaysUpdate value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.AlwaysUpdate
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.AlwaysUpdate <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.AlwaysUpdate) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityAlwaysRender value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.AlwaysRender
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.AlwaysRender <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.AlwaysRender) previous value entityState.PublishChangeEvents entity world
                true
            else false

        static member internal setEntityPresence (value : Presence) (entity : Entity) world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Presence
            if presenceNeq value previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Presence <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.Presence) previous value entityState.PublishChangeEvents entity world
                World.updateEntityPresenceOverride entity world
                true
            else false

        static member internal setEntityTransformByRefWithoutEvent (value : Transform inref, entityState : EntityState, entity : Entity, world : World) =
            if not (Transform.equalsByRef (&value, &entityState.Transform)) then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Transform <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world

        static member internal setEntityTransformByRef (value : Transform byref, entityState : EntityState, entity : Entity, world) : bool =
            let mutable previous = entityState.Transform
            if not (Transform.equalsByRef (&value, &previous)) then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Transform <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world
                World.publishTransformEvents (&previous, &value, entityState.Is2d, entityState.PublishChangeEvents, entity, world)
                true
            else false

        static member internal setEntityPerimeterCenter value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.PerimeterCenter
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPerimeterBottom value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.PerimeterBottom
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPerimeterBottomLeft value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.PerimeterBottomLeft
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPerimeterMin value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.PerimeterMin
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPerimeterMax value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.PerimeterMax
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPerimeterCenterLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.PerimeterCenterLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityPerimeterBottomLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.PerimeterBottomLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityPerimeterBottomLeftLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.PerimeterBottomLeftLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityPerimeterMinLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.PerimeterMinLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityPerimeterMaxLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.PerimeterMaxLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityPosition value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Position then
                if entityState.Optimized then
                    entityState.Position <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Position <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal setEntityPositionLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.PositionLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.PositionLocal <- value
                    let position =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            value.Transform affineMatrix
                        | _ -> value
                    entityState.Position <- position
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true

                else // do updates and propagation out-of-place

                    // update PositionLocal property
                    let previous = entityState.PositionLocal
                    if v3Neq value previous then
                        let centerPrevious = entityState.PerimeterCenterLocal
                        let bottomPrevious = entityState.PerimeterBottomLocal
                        let bottomLeftPrevious = entityState.PerimeterBottomLeftLocal
                        let minPrevious = entityState.PerimeterMinLocal
                        let maxPrevious = entityState.PerimeterMaxLocal
                        entityState.PositionLocal <- value
                        if entityState.PublishChangeEvents then
                            World.publishEntityChange (nameof entityState.PerimeterCenterLocal) centerPrevious entityState.PerimeterCenterLocal true entity world
                            World.publishEntityChange (nameof entityState.PerimeterBottomLocal) bottomPrevious entityState.PerimeterBottomLocal true entity world
                            World.publishEntityChange (nameof entityState.PerimeterBottomLeftLocal) bottomLeftPrevious entityState.PerimeterBottomLeftLocal true entity world
                            World.publishEntityChange (nameof entityState.PerimeterMinLocal) minPrevious entityState.PerimeterMinLocal true entity world
                            World.publishEntityChange (nameof entityState.PerimeterMaxLocal) maxPrevious entityState.PerimeterMaxLocal true entity world
                            World.publishEntityChange (nameof entityState.PositionLocal) previous value true entity world

                    // compute position
                    let position =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            value.Transform affineMatrix
                        | _ -> value

                    // update property
                    World.setEntityPosition position entity world |> ignore<bool>
                    true

            // nothing changed
            else false

        static member internal setEntityRotation value entity world =
            let entityState = World.getEntityState entity world
            if quatNeq value entityState.Rotation then
                if entityState.Optimized then
                    entityState.Rotation <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Rotation <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

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
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationLocal = World.getEntityRotation mount world
                            rotationLocal * value
                        | _ -> value
                    entityState.Rotation <- rotation
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true

                else // do updates and propagation out-of-place

                    // update RotationLocal property
                    let previous = entityState.RotationLocal
                    let previousAnglesLocal = entityState.AnglesLocal
                    let previousDegreesLocal = entityState.DegreesLocal
                    if quatNeq value previous then
                        entityState.RotationLocal <- value
                        entityState.AnglesLocal <- anglesLocal
                        let publishChangeEvents = entityState.PublishChangeEvents
                        World.publishEntityChange (nameof entityState.RotationLocal) previous value publishChangeEvents entity world
                        World.publishEntityChange (nameof entityState.AnglesLocal) previousAnglesLocal anglesLocal publishChangeEvents entity world
                        World.publishEntityChange (nameof entityState.DegreesLocal) previousDegreesLocal (Math.RadiansToDegrees3d anglesLocal) publishChangeEvents entity world

                    // compute rotation
                    let rotation =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * value
                        | _ -> value

                    // update property
                    World.setEntityRotation rotation entity world |> ignore<bool>
                    true

            // nothing changed
            else false

        static member internal setEntityScale value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Scale then
                if entityState.Optimized then
                    entityState.Scale <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Scale <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal setEntityScaleLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.ScaleLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ScaleLocal <- value
                    let scale =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value
                    entityState.Scale <- scale
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true

                else // do updates and propagation out-of-place

                    // update ScaleLocal property
                    let previous = entityState.ScaleLocal
                    if v3Neq value previous then
                        entityState.ScaleLocal <- value
                        World.publishEntityChange (nameof entityState.ScaleLocal) previous value entityState.PublishChangeEvents entity world

                    // compute scale
                    let scale =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value

                    // update property
                    World.setEntityScale scale entity world |> ignore<bool>
                    true

            // nothing changed
            else false

        static member internal setEntityOffset value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Offset then
                if entityState.Optimized then
                    entityState.Offset <- value
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Offset <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal setEntitySize value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Size then
                let centerPrevious = entityState.PerimeterCenterLocal
                let bottomPrevious = entityState.PerimeterBottomLocal
                let bottomLeftPrevious = entityState.PerimeterBottomLeftLocal
                let minPrevious = entityState.PerimeterMinLocal
                let maxPrevious = entityState.PerimeterMaxLocal
                if entityState.Optimized then
                    entityState.Size <- value
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Size <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    if entityState.PublishChangeEvents then
                        World.publishEntityChange (nameof entityState.PerimeterCenterLocal) centerPrevious entityState.PerimeterCenterLocal true entity world
                        World.publishEntityChange (nameof entityState.PerimeterBottomLocal) bottomPrevious entityState.PerimeterBottomLocal true entity world
                        World.publishEntityChange (nameof entityState.PerimeterBottomLeftLocal) bottomLeftPrevious entityState.PerimeterBottomLeftLocal true entity world
                        World.publishEntityChange (nameof entityState.PerimeterMinLocal) minPrevious entityState.PerimeterMinLocal true entity world
                        World.publishEntityChange (nameof entityState.PerimeterMaxLocal) maxPrevious entityState.PerimeterMaxLocal true entity world
                    true
            else false

        static member internal setEntityAngles value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Angles then
                if entityState.Optimized then
                    entityState.Angles <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Angles <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

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
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * rotationLocal
                        | _ -> rotationLocal
                    entityState.Rotation <- rotation
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true

                else // do updates and propagation out-of-place

                    // update AnglesLocal property
                    let previous = entityState.AnglesLocal
                    let previousRotationLocal = entityState.RotationLocal
                    let previousDegreesLocal = entityState.DegreesLocal
                    if v3Neq value previous then
                        entityState.RotationLocal <- rotationLocal
                        entityState.AnglesLocal <- value
                        let publishChangeEvents = entityState.PublishChangeEvents
                        World.publishEntityChange (nameof entityState.RotationLocal) previousRotationLocal rotationLocal publishChangeEvents entity world
                        World.publishEntityChange (nameof entityState.AnglesLocal) previous value publishChangeEvents entity world
                        World.publishEntityChange (nameof entityState.DegreesLocal) previousDegreesLocal (Math.RadiansToDegrees3d value) publishChangeEvents entity world

                    // update rotation property if mounting, otherwise update angles property
                    match Option.bind (flip tryResolve entity) entityState.MountOpt with
                    | Some mount when World.getEntityExists mount world ->
                        let rotationMount = World.getEntityRotation mount world
                        let rotation = rotationMount * rotationLocal
                        World.setEntityRotation rotation entity world |> ignore<bool>
                        true
                    | _ ->
                        World.setEntityAngles value entity world |> ignore<bool>
                        true

            // nothing changed
            else false

        static member internal setEntityDegrees value entity world =
            World.setEntityAngles (Math.DegreesToRadians3d value) entity world

        static member internal setEntityDegreesLocal value entity world =
            World.setEntityAnglesLocal (Math.DegreesToRadians3d value) entity world

        static member internal propagateEntityElevation3 mount mounter world =

            // HACK: for layout-based entities like guis, elevate them to make sure they're enough above their parent.
            // This is worth the hack because it's an important UX improvement.
            if  World.getEntityElevationLocal mounter world < 1.0f &&
                Array.exists (fun facet -> getType facet = LayoutFacetType) (World.getEntityFacets mount world) &&
                Array.exists (fun facet -> getType facet = LayoutFacetType) (World.getEntityFacets mounter world) then
                World.setEntityElevationLocal 1.0f mounter world |> ignore<bool>

            // propagate elevation
            let elevationMount = World.getEntityElevation mount world
            let elevationLocal = World.getEntityElevationLocal mounter world
            let elevation = elevationMount + elevationLocal
            World.setEntityElevation elevation mounter world |> ignore<bool>
            World.traverseEntityMounters World.propagateEntityElevation3 mounter world

        static member internal propagateEntityElevation entity world =
            World.traverseEntityMounters World.propagateEntityElevation3 entity world

        static member internal setEntityElevation value entity world : bool =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Elevation then
                if entityState.Optimized then
                    entityState.Transform.Elevation <- value
                else
                    let mutable transform = entityState.Transform
                    transform.Elevation <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                if World.getEntityMounted entity world then World.propagateEntityElevation entity world
                true
            else false

        static member internal setEntityElevationLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if value <> entityState.ElevationLocal then

                // OPTIMIZATION: do elevation updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ElevationLocal <- value
                    let elevationMount =
                        match Option.bind (flip tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f
                    entityState.Transform.Elevation <- elevationMount + value
                    if entityState.Mounted then World.propagateEntityElevation entity world
                    true

                else // do elevation updates and propagation out-of-place

                    // update ElevationLocal property
                    let previous = entityState.ElevationLocal
                    if value <> previous then
                        entityState.ElevationLocal <- value
                        World.publishEntityChange (nameof entityState.ElevationLocal) previous value entityState.PublishChangeEvents entity world

                    // compute mount elevation
                    let elevationMount =
                        match Option.bind (flip tryResolve entity) (World.getEntityMountOpt entity world) with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f

                    // update property
                    World.setEntityElevation (elevationMount + value) entity world |> ignore<bool>
                    true

            // nothing changed
            else false

        static member internal propagateEntityEnabled3 mount mounter world =
            let enabledMount = World.getEntityEnabled mount world
            let enabledLocal = World.getEntityEnabledLocal mounter world
            let enabled = enabledMount && enabledLocal
            World.setEntityEnabled enabled mounter world |> ignore<bool>
            World.traverseEntityMounters World.propagateEntityEnabled3 mounter world

        static member internal propagateEntityEnabled entity world =
            World.traverseEntityMounters World.propagateEntityEnabled3 entity world

        static member internal setEntityEnabled value entity world : bool =
            let entityState = World.getEntityState entity world
            let previous = entityState.Enabled
            if value <> previous then
                entityState.Enabled <- value
                World.publishEntityChange (nameof entityState.Enabled) previous value entityState.PublishChangeEvents entity world
                if World.getEntityMounted entity world then World.propagateEntityEnabled entity world
                true
            else false

        static member internal setEntityEnabledLocal value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.EnabledLocal
            if value <> previous then
                entityState.EnabledLocal <- value
                World.publishEntityChange (nameof entityState.EnabledLocal) previous value entityState.PublishChangeEvents entity world
                let mountOpt = Option.bind (flip tryResolve entity) (World.getEntityMountOpt entity world)
                let enabledMount =
                    match mountOpt with
                    | Some mount when World.getEntityExists mount world -> World.getEntityEnabled mount world
                    | _ -> true
                let enabled = enabledMount && value
                World.setEntityEnabled enabled entity world |> ignore<bool>
                true
            else false

        static member internal propagateEntityVisible3 mount mounter world =
            let visibleMount = World.getEntityVisible mount world
            let visibleLocal = World.getEntityVisibleLocal mounter world
            let visible = visibleMount && visibleLocal
            World.setEntityVisible visible mounter world |> ignore<bool>
            World.traverseEntityMounters World.propagateEntityVisible3 mounter world

        static member internal propagateEntityVisible entity world =
            World.traverseEntityMounters World.propagateEntityVisible3 entity world
            
        static member internal setEntityVisible value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Visible
            if value <> previous then
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                entityState.Visible <- value
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChange (nameof entityState.Visible) previous value entityState.PublishChangeEvents entity world
                if World.getEntityMounted entity world then World.propagateEntityVisible entity world
                true
            else false

        static member internal setEntityVisibleLocal value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.VisibleLocal
            if value <> previous then
                entityState.VisibleLocal <- value
                World.publishEntityChange (nameof entityState.VisibleLocal) previous value entityState.PublishChangeEvents entity world
                let mountOpt = Option.bind (flip tryResolve entity) (World.getEntityMountOpt entity world)
                let visibleMount =
                    match mountOpt with
                    | Some mount when World.getEntityExists mount world -> World.getEntityVisible mount world
                    | _ -> true
                let visible = visibleMount && value
                World.setEntityVisible visible entity world |> ignore<bool>
                true
            else false

        static member internal setEntityCastShadow value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.CastShadow
            if value <> previous then
                entityState.CastShadow <- value
                true
            else false

        static member internal setEntityPickable value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Pickable
            if value <> previous then
                entityState.Pickable <- value
                true
            else false

        static member internal setEntityOverflow value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Overflow then
                if entityState.Optimized then
                    entityState.Transform.Overflow <- value
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Overflow <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal getEntityPerimeterUnscaled entity world =
            (World.getEntityState entity world).Transform.PerimeterUnscaled

        static member internal setEntityPerimeterUnscaled value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.PerimeterUnscaled then
                if entityState.Optimized then
                    entityState.PerimeterUnscaled <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.PerimeterUnscaled <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal getEntityPerimeter entity world =
            (World.getEntityState entity world).Transform.Perimeter

        static member internal setEntityPerimeter value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.Perimeter then
                if entityState.Optimized then
                    entityState.Perimeter <- value
                    if entityState.Mounted then World.propagateEntityAffineMatrix entity world
                    true
                else
                    let mutable transform = entityState.Transform
                    transform.Perimeter <- value
                    World.setEntityTransformByRef (&transform, entityState, entity, world) |> ignore<bool>
                    true
            else false

        static member internal getEntityBounds entity world =
            let entityState = World.getEntityState entity world
            if entityState.Is2d
            then entityState.Transform.Bounds2d
            else entityState.Transform.Bounds3d

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match facets.TryGetValue facetName with
            | (true, facet) -> Right facet
            | (false, _) -> Left ("Invalid facet name '" + facetName + "'.")

        static member private isFacetCompatibleWithEntity entityDispatchers facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatchers facet entityState then
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
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                match entityOpt with
                | Some entity ->
                    facet.Unregister (entity, world)
                    if WorldModule.getSelected entity world then facet.UnregisterPhysics (entity, world)
                | None -> ()
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                Reflection.detachPropertiesViaNames propertyNames entityState
                let facetNames = Set.remove facetName entityState.FacetNames
                let facets = Array.remove ((=) facet) entityState.Facets
                entityState.FacetNames <- facetNames
                entityState.Facets <- facets
                match entityOpt with
                | Some entity ->
                    World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                    Right ()
                | None -> Right ()
            | None -> Left ("Failure to remove facet '" + facetName + "' from entity.")

        static member private tryAddFacet facetName (entityState : EntityState) entityOpt world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let visibleInViewOld = entityState.VisibleInView
                    let staticInPlayOld = entityState.StaticInPlay
                    let lightProbeOld = entityState.LightProbe
                    let lightOld = entityState.Light
                    let presenceOld = entityState.Presence
                    let presenceInPlayOld = entityState.PresenceInPlay
                    let boundsOld = entityState.Bounds
                    let facetNames = Set.add facetName entityState.FacetNames
                    let facets = Array.add facet entityState.Facets
                    entityState.FacetNames <- facetNames
                    entityState.Facets <- facets
                    Reflection.attachProperties facet entityState world
                    match entityOpt with
                    | Some entity ->
                        World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                        World.updateEntityPresenceOverride entity world
                        facet.Register (entity, world)
                        if WorldModule.getSelected entity world then facet.RegisterPhysics (entity, world)
                        Right ()
                    | None -> Right ()
                else Left ("Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Surnames + "'.")
            | Left error -> Left error

        static member internal tryRemoveFacets facetNamesToRemove entityState entityOpt world =
            let mutable result = Right ()
            for facetName in facetNamesToRemove do
                match result with
                | Right () -> result <- World.tryRemoveFacet facetName entityState entityOpt world
                | Left _ -> ()
            result

        static member internal tryAddFacets facetNamesToAdd entityState entityOpt world =
            let mutable result = Right ()
            for facetName in facetNamesToAdd do
                match result with
                | Right () -> result <- World.tryAddFacet facetName entityState entityOpt world
                | Left _ -> ()
            result

        static member internal trySetFacetNames facetNames entityState entityOpt world =
            let intrinsicFacetNames = World.getEntityIntrinsicFacetNames entityState
            let extrinsicFacetNames = Set.fold (flip Set.remove) facetNames intrinsicFacetNames
            let facetNamesToRemove = Set.difference entityState.FacetNames extrinsicFacetNames
            let facetNamesToAdd = Set.difference extrinsicFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right () -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames facetNamesOld entityState entityOpt world =
            let facetNamesToRemove = Set.difference facetNamesOld entityState.FacetNames
            let facetNamesToAdd = Set.difference entityState.FacetNames facetNamesOld
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right () -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets entityDispatchers facets entityState.Dispatcher entityState world

        static member internal applyEntityOverlay overlayerOld overlayer entity world =
            let entityState = World.getEntityState entity world
            match entityState.OverlayNameOpt with
            | Some overlayName ->
                let visibleInViewOld = entityState.VisibleInView
                let staticInPlayOld = entityState.StaticInPlay
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let presenceInPlayOld = entityState.PresenceInPlay
                let boundsOld = entityState.Bounds
                let facetNamesOld = entityState.FacetNames
                Overlayer.applyOverlayToFacetNames overlayName overlayName entityState overlayerOld overlayer
                match World.trySynchronizeFacetsToNames facetNamesOld entityState (Some entity) world with
                | Right () ->
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    Overlayer.applyOverlay6 overlayName overlayName facetNames entityState overlayerOld overlayer
                    World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                | Left error -> Log.info ("There was an issue in applying a reloaded overlay: " + error)
            | None -> ()

        static member internal tryGetEntityXtensionProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ -> EntityState.tryGetProperty (propertyName, entityStateOpt, &property)

        static member internal tryGetEntityProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ ->
                match EntityGetters.TryGetValue propertyName with
                | (true, getter) -> property <- getter entity world; true
                | (false, _) ->
                    if EntityState.tryGetProperty (propertyName, entityStateOpt, &property) then
                        if EntityState.containsRuntimeProperties entityStateOpt then
                            match property.PropertyValue with
                            | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                            | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (entity :> obj) (world :> obj) }; true
                            | _ -> true
                        else true
                    else false

        static member internal getEntityXtensionValue<'a> propertyName entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> failwithf "Could not find entity '%s'." (scstring entity)
            | _ ->
                let mutable property = Unchecked.defaultof<_>
                if EntityState.tryGetProperty (propertyName, entityStateOpt, &property) then
                    let valueObj =
                        match property.PropertyValue with
                        | :? DesignerProperty as dp -> dp.DesignerValue
                        | :? ComputedProperty as cp -> cp.ComputedGet entity world
                        | _ -> property.PropertyValue
                    match valueObj with
                    | :? 'a as value -> value
                    | null -> null :> obj :?> 'a
                    | value ->
                        let value =
                            try value |> valueToSymbol |> symbolToValue
                            with _ ->
                                let value = typeof<'a>.GetDefaultValue ()
                                Log.warn "Could not gracefully promote value to the required type, so using a default value instead."
                                value :?> 'a
                        match property.PropertyValue with
                        | :? DesignerProperty as dp -> dp.DesignerType <- typeof<'a>; dp.DesignerValue <- value
                        | :? ComputedProperty -> () // nothing to do
                        | _ -> property.PropertyType <- typeof<'a>; property.PropertyValue <- value
                        value
                else
                    let value =
                        match entityStateOpt.OverlayNameOpt with
                        | Some overlayName ->
                            match World.tryGetOverlayerPropertyValue propertyName typeof<'a> overlayName entityStateOpt.FacetNames world with
                            | Some value -> value :?> 'a
                            | None ->
                                let definitions = Reflection.getPropertyDefinitions (getType entityStateOpt.Dispatcher)
                                match List.tryFind (fun (pd : PropertyDefinition) -> pd.PropertyName = propertyName) definitions with
                                | Some definition ->
                                    match definition.PropertyExpr with
                                    | DefineExpr value -> value :?> 'a
                                    | VariableExpr eval -> eval world :?> 'a
                                    | ComputedExpr property -> property.ComputedGet entity world :?> 'a
                                | None -> failwithumf ()
                        | None ->
                            let definitions = Reflection.getPropertyDefinitions (getType entityStateOpt.Dispatcher)
                            match List.tryFind (fun (pd : PropertyDefinition) -> pd.PropertyName = propertyName) definitions with
                            | Some definition ->
                                match definition.PropertyExpr with
                                | DefineExpr value -> value :?> 'a
                                | VariableExpr eval -> eval world :?> 'a
                                | ComputedExpr property -> property.ComputedGet entity world :?> 'a
                            | None -> failwithumf ()
                    let property = { PropertyType = typeof<'a>; PropertyValue = value }
                    Xtension.attachProperty propertyName property entityStateOpt.Xtension
                    value

        static member internal tryGetEntityXtensionValue<'a> propertyName entity world : 'a voption =
            // NOTE: we're only using exceptions as flow control in order to avoid code duplication and perf costs.
            // TODO: P1: see if we can find a way to refactor this situation without incurring any additional overhead on the getEntityXtensionValue call.
            try World.getEntityXtensionValue<'a> propertyName entity world |> ValueSome
            with _ -> ValueNone

        static member internal getEntityProperty propertyName entity world =
            let mutable property = Unchecked.defaultof<_>
            if World.tryGetEntityProperty (propertyName, entity, world, &property) then property
            else failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityXtensionPropertyWithoutEvent propertyName (property : Property) entityState entity (world : World) =
            let mutable propertyOld = Unchecked.defaultof<_>
            match EntityState.tryGetProperty (propertyName, entityState, &propertyOld) with
            | true ->
                if EntityState.containsRuntimeProperties entityState then
                    match propertyOld.PropertyValue with
                    | :? DesignerProperty as dp ->
                        let previous = dp.DesignerValue
                        if property.PropertyValue =/= previous then
                            let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                            if EntityState.trySetProperty propertyName property entityState
                            then struct (true, true, previous)
                            else struct (false, false, previous)
                        else (true, false, previous)
                    | :? ComputedProperty as cp ->
                        match cp.ComputedSetOpt with
                        | Some computedSet ->
                            let previous = cp.ComputedGet (box entity) (box world)
                            if property.PropertyValue =/= previous then
                                computedSet property.PropertyValue entity world |> ignore<obj> // TODO: P0: fix.
                                struct (true, true, previous)
                            else struct (true, false, previous)
                        | None -> struct (false, false, Unchecked.defaultof<_>)
                    | _ ->
                        let previous = propertyOld.PropertyValue
                        if property.PropertyValue =/= previous then
                            propertyOld.PropertyValue <- property.PropertyValue
                            struct (true, true, previous)
                        else struct (true, false, previous)
                else
                    let previous = propertyOld.PropertyValue
                    if property.PropertyValue =/= previous then
                        propertyOld.PropertyValue <- property.PropertyValue
                        struct (true, true, previous)
                    else struct (true, false, previous)
            | false -> struct (false, false, Unchecked.defaultof<_>)

        static member internal trySetEntityXtensionPropertyFast propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, previous) ->
                    if changed then World.publishEntityChange propertyName previous property.PropertyValue entityStateOpt.PublishChangeEvents entity world
                | struct (false, _, _) -> ()

        static member internal trySetEntityXtensionProperty propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, previous) ->
                    if changed then World.publishEntityChange propertyName previous property.PropertyValue entityStateOpt.PublishChangeEvents entity world
                    struct (true, changed)
                | struct (false, changed, _) -> struct (false, changed)
            else struct (false, false)

        static member internal trySetEntityXtensionValue<'a> propertyName (value : 'a) entity world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetEntityXtensionProperty propertyName property entity world

        static member internal setEntityXtensionPropertyWithoutEvent propertyName property entity world =
            let entityState = World.getEntityState entity world
            match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityState entity world with
            | struct (true, changed, _) -> struct (true, changed)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal setEntityXtensionValue<'a> propertyName (value : 'a) entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                let entityState = entityStateOpt
                let propertyOld = EntityState.getProperty propertyName entityState
                let mutable previous = Unchecked.defaultof<obj> // OPTIMIZATION: avoid passing around structs.
                let mutable changed = false // OPTIMIZATION: avoid passing around structs.
                if EntityState.containsRuntimeProperties entityState then
                    match propertyOld.PropertyValue with
                    | :? DesignerProperty as dp ->
                        previous <- dp.DesignerValue
                        if value =/= previous then
                            changed <- true
                            let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                            EntityState.setProperty propertyName property entityState
                    | :? ComputedProperty as cp ->
                        match cp.ComputedSetOpt with
                        | Some computedSet ->
                            previous <- cp.ComputedGet (box entity) (box world)
                            if value =/= previous then
                                changed <- true
                                computedSet propertyOld.PropertyValue entity world |> ignore<obj> // TODO: P0: fix.
                        | None -> ()
                    | _ ->
                        previous <- propertyOld.PropertyValue
                        if value =/= previous then
                            changed <- true
                            propertyOld.PropertyValue <- value
                else
                    previous <- propertyOld.PropertyValue
                    if value =/= previous then
                        changed <- true
                        propertyOld.PropertyValue <- value
                if changed then
                    World.publishEntityChange propertyName previous value entityStateOpt.PublishChangeEvents entity world
            else failwithf "Could not find entity '%s'." (scstring entity)

        static member internal setEntityXtensionProperty propertyName property entity world =
            match World.trySetEntityXtensionProperty propertyName property entity world with
            | struct (true, changed) -> changed
            | struct (false, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityPropertyFast propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getEntityExists entity world then
                    setter property entity world |> ignore<bool>
            | (false, _) -> World.trySetEntityXtensionPropertyFast propertyName property entity world

        static member internal trySetEntityProperty propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getEntityExists entity world then
                    let changed = setter property entity world
                    struct (true, changed)
                else (false, false)
            | (false, _) -> World.trySetEntityXtensionProperty propertyName property entity world

        static member internal setEntityPropertyFast propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) -> setter property entity world |> ignore<bool>
            | (false, _) -> World.trySetEntityXtensionPropertyFast propertyName property entity world

        static member internal setEntityProperty propertyName property entity world =
            match World.trySetEntityProperty propertyName property entity world with
            | struct (true, changed) -> changed
            | struct (false, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal attachEntityMissingProperties entity world =
            let entityState = World.getEntityState entity world
            let definitions = Reflection.getReflectivePropertyDefinitions entityState
            for definition in definitions do
                let propertyName = definition.Key
                let propertyDefinition = definition.Value
                let mutable property = Unchecked.defaultof<_>
                if not (World.tryGetEntityProperty (propertyName, entity, world, &property)) then
                    let propertyValue = PropertyExpr.eval propertyDefinition.PropertyExpr world
                    let property = { PropertyType = propertyDefinition.PropertyType; PropertyValue = propertyValue }
                    EntityState.attachProperty propertyName property entityState

        static member internal getEntityDefaultOverlayName dispatcherName world =
            match World.tryGetRoutedOverlayNameOpt dispatcherName world with
            | Some _ as opt -> opt
            | None -> Some dispatcherName

        static member internal getEntityInView2dAbsolute entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.IsOmnipresent || World.boundsInView2dAbsolute transform.Bounds2d.Box2 world

        static member internal getEntityInView2dRelative entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.IsOmnipresent || World.boundsInView2dRelative transform.Bounds2d.Box2 world

        static member internal getEntityInPlay2dAbsolute entity world =
            World.getEntityInView2dAbsolute entity world

        static member internal getEntityInPlay2dRelative entity world =
            World.getEntityInView2dRelative entity world

        static member internal getEntityInPlay3d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.IsOmnipresent || World.boundsInPlay3d transform.Bounds3d world

        static member internal getEntityInView3d entity world =
            let entityState = World.getEntityState entity world
            let lightProbe = entityState.Dispatcher.LightProbe
            let light = entityState.Dispatcher.Light
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.IsOmnipresent || World.boundsInView3d lightProbe light presence transform.Bounds3d world

        static member internal getEntityAttributesInferred (entity : Entity) world =
            let dispatcher = World.getEntityDispatcher entity world
            let facets = World.getEntityFacets entity world
            let attributes = dispatcher.GetAttributesInferred (entity, world)
            Array.fold
                (fun attributes (facet : Facet) ->
                    let attributes2 = facet.GetAttributesInferred (entity, world)
                    AttributesInferred.choose attributes attributes2)
                attributes
                facets

        static member internal getEntitySortingPriority2d entity world =
            let entityState = World.getEntityState entity world
            { SortElevation = entityState.Transform.Elevation
              SortHorizon = entityState.Transform.Horizon
              SortTarget = entity }

        static member internal autoBoundsEntity entity world =
            let attributes = World.getEntityAttributesInferred entity world
            if not attributes.Unimportant then
                World.setEntitySize attributes.SizeInferred entity world |> ignore<bool>
                World.setEntityOffset attributes.OffsetInferred entity world |> ignore<bool>

        static member internal rayCastEntity ray (entity : Entity) world =
            let facets = World.getEntityFacets entity world
            let dispatcher = World.getEntityDispatcher entity world
            let intersectionsFacets = facets |> Array.map (fun facet -> facet.RayCast (ray, entity, world)) |> Array.concat
            let intersectionsDispatcher = dispatcher.RayCast (ray, entity, world)
            let intersections = Array.append intersectionsFacets intersectionsDispatcher
            if Array.isEmpty intersections then
                let intersectionOpt = ray.Intersects (World.getEntityBounds entity world)
                [|Intersection.ofNullable intersectionOpt|]
            else Array.sort intersections

        static member internal updateEntityPublishUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishUpdates entity (atooa (Events.UpdateEvent --> entity)) world

        static member internal registerEntityIndex (ty : Type) (entity : Entity) (world : World) =
            for ty in ty :: Reflection.getBaseTypesExceptObject ty do
                match world.EntitiesIndexed.TryGetValue struct (entity.Group, ty) with
                | (true, entities) ->
                    entities.Add entity |> ignore<bool>
                | (false, _) ->
                    world.EntitiesIndexed.[struct (entity.Group, ty)] <- HashSet.singleton HashIdentity.Structural entity

        static member internal unregisterEntityIndex (ty : Type) (entity : Entity) (world : World) =
            for ty in ty :: Reflection.getBaseTypesExceptObject ty do
                match world.EntitiesIndexed.TryGetValue struct (entity.Group, ty) with
                | (true, entities) ->
                    entities.Remove entity |> ignore<bool>
                    if entities.Count = 0 then world.EntitiesIndexed.Remove struct (entity.Group, ty) |> ignore<bool>
                | (false, _) -> ()

        static member internal registerEntity entity world =
            let facets = World.getEntityFacets entity world
            for facet in facets do
                World.registerEntityIndex (getType facet) entity world
                facet.Register (entity, world)
                if WorldModule.getSelected entity world then facet.RegisterPhysics (entity, world)
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            dispatcher.RegisterPhysics (entity, world)
            World.registerEntityIndex (getType dispatcher) entity world
            dispatcher.Register (entity, world)
            World.updateEntityPublishUpdateFlag entity world |> ignore<bool>
            let eventTrace = EventTrace.debug "World" "registerEntity" "Register" EventTrace.empty
            let eventAddresses = EventGraph.getEventAddresses1 (Events.RegisterEvent --> entity)
            for eventAddress in eventAddresses do
                World.publishPlus () eventAddress eventTrace entity false false world
            let eventTrace = EventTrace.debug "World" "registerEntity" "LifeCycle" EventTrace.empty
            World.publishPlus (RegisterData entity) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world

        static member internal unregisterEntity (entity : Entity) world =
            let eventTrace = EventTrace.debug "World" "unregisterEntity" "LifeCycle" EventTrace.empty
            World.publishPlus (UnregisteringData entity) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world
            let eventTrace = EventTrace.debug "World" "unregister" "Unregistering" EventTrace.empty
            let eventAddresses = EventGraph.getEventAddresses1 (Events.UnregisteringEvent --> entity)
            for eventAddress in eventAddresses do
                World.publishPlus () eventAddress eventTrace entity false false world
            let facets = World.getEntityFacets entity world
            for facet in facets do
                facet.Unregister (entity, world)
                if WorldModule.getSelected entity world then
                    facet.UnregisterPhysics (entity, world)
                World.unregisterEntityIndex (getType facet) entity world
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            dispatcher.Unregister (entity, world)
            dispatcher.UnregisterPhysics (entity, world)
            World.unregisterEntityIndex (getType dispatcher) entity world

        static member internal registerEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            for facet in facets do
                facet.RegisterPhysics (entity, world)
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            dispatcher.RegisterPhysics (entity, world)

        static member internal unregisterEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            for facet in facets do
                facet.UnregisterPhysics (entity, world)
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            dispatcher.UnregisterPhysics (entity, world)

        static member internal propagateEntityPhysics entity world =
            World.unregisterEntityPhysics entity world
            World.registerEntityPhysics entity world

        static member internal addEntity entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            if not (World.getEntityExists entity world) then

                // add entity to world
                World.addEntityState entityState entity world

                // update mount hierarchy
                let mountOpt = World.getEntityMountOpt entity world
                World.addEntityToMounts mountOpt entity world

                // update propagation hierarchy
                let propagationSourceOpt = World.getEntityPropagationSourceOpt entity world
                match propagationSourceOpt with
                | Some origin -> World.addEntityToPropagationTargets origin entity world
                | None -> ()

                // mutate respective spatial tree if entity is selected
                if WorldModule.getSelected entity world then
                    if World.getEntityIs2d entity world then
                        let quadtree = World.getQuadtree world
                        let entityState = World.getEntityState entity world
                        let element = Quadelement.make entityState.VisibleInView entityState.StaticInPlay entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 entity
                        Quadtree.addElement entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 element quadtree
                    else
                        let octree = World.getOctree world
                        let entityState = World.getEntityState entity world
                        let element = Octelement.make entityState.VisibleInView entityState.StaticInPlay entityState.LightProbe entityState.Light entityState.Presence entityState.PresenceInPlay entityState.Bounds entity
                        Octree.addElement entityState.Presence entityState.PresenceInPlay entityState.Bounds element octree

                // register entity
                World.registerEntity entity world

            // handle failure
            else failwith ("Adding an entity that the world already contains '" + scstring entity + "'.")

        static member internal destroyEntityImmediateInternal recur entity world =

            // attempt to remove from destruction list
            World.tryRemoveSimulantFromDestruction entity world

            // ensure entity exists in the world
            if World.getEntityExists entity world then

                // cache entity children for later possible destruction
                let children = World.getEntityChildren entity world

                // update mount hierarchy
                World.setEntityMountOpt None entity world |> ignore<bool>

                // update propagation hierarchy
                World.setEntityPropagationSourceOpt None entity world |> ignore<bool>

                // unregister entity
                World.unregisterEntity entity world

                // destroy any scheduled tasklets
                World.removeTasklets entity world

                // remove from simulant imsim tracking
                World.removeSimulantImSim entity world

                // mutate respective entity tree if entity is selected
                if WorldModule.getSelected entity world then
                    if World.getEntityIs2d entity world then
                        let quadtree = World.getQuadtree world
                        let entityState = World.getEntityState entity world
                        let element = Quadelement.make entityState.VisibleInView entityState.StaticInPlay entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 entity
                        Quadtree.removeElement entityState.Presence entityState.PresenceInPlay entityState.Bounds.Box2 element quadtree
                    else
                        let octree = World.getOctree world
                        let entityState = World.getEntityState entity world
                        let element = Octelement.make entityState.VisibleInView entityState.StaticInPlay entityState.LightProbe entityState.Light entityState.Presence entityState.PresenceInPlay entityState.Bounds entity
                        Octree.removeElement entityState.Presence entityState.PresenceInPlay entityState.Bounds element octree

                // remove cached entity event addresses
                EventGraph.cleanEventAddressCache entity.EntityAddress

                // invalidate entity state
                let entityState = World.getEntityState entity world
                entityState.Invalidated <- true

                // remove the entity from the world
                World.removeEntityState entity world

                // destroy children when recurring
                if recur then
                    for child in children do
                        World.destroyEntityImmediateInternal recur child world

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =
            World.destroyEntityImmediateInternal true entity world

        /// Create an entity and add it to the world.
        static member createEntity7 skipProcessing dispatcherName mountOpt overlayDescriptor surnames (group : Group) world =

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match dispatchers.TryGetValue dispatcherName with
                | (true, dispatcher) -> dispatcher
                | (false, _) -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'.")

            // compute the optional overlay name
            let overlayNameDefault = Overlay.dispatcherNameToOverlayName dispatcherName
            let overlayNameOpt =
                match overlayDescriptor with
                | NoOverlay -> None
                | RoutedOverlay -> World.tryGetRoutedOverlayNameOpt dispatcherName world
                | DefaultOverlay -> Some (Option.defaultValue overlayNameDefault (World.tryGetRoutedOverlayNameOpt dispatcherName world))
                | ExplicitOverlay overlayName -> Some overlayName

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make mountOpt surnames overlayNameOpt dispatcher

            // attach the entity state's intrinsic properties
            let facets = World.getFacets world
            let dispatcherType = getType dispatcher
            let dispatcherTypes = dispatcherType :: Reflection.getBaseTypesExceptObject dispatcherType |> List.rev
            for (facetNames, ty) in List.map (fun ty -> (Reflection.getIntrinsicFacetNamesNoInherit ty, ty)) dispatcherTypes do
                Reflection.attachIntrinsicFacetsViaNames dispatchers facets facetNames entityState world
                let definitions = Reflection.getPropertyDefinitionsNoInherit ty
                Reflection.attachPropertiesViaDefinitions definitions entityState world

            // apply the entity state's overlay to its facet names
            let overlayer = World.getOverlayer world
            match overlayNameOpt with
            | Some overlayName ->

                // apply overlay to facets
                Overlayer.applyOverlayToFacetNames dispatcherName overlayName entityState overlayer overlayer

                // synchronize the entity's facets (and attach their properties)
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right () -> ()
                | Left error -> Log.error error
            | None -> ()

            // apply the entity state's overlay if exists
            match entityState.OverlayNameOpt with
            | Some overlayName ->
                // OPTIMIZATION: apply overlay only when it will change something
                if overlayNameDefault <> overlayName then
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    Overlayer.applyOverlay overlayNameDefault overlayName facetNames entityState overlayer
            | None -> ()

            // populate local angles value from local rotation
            entityState.AnglesLocal <- entityState.RotationLocal.RollPitchYaw

            // make entity address
            let entityAddress = group.GroupAddress <-- rtoa<Entity> entityState.Surnames

            // make entity reference
            let entity = Entity entityAddress

            // add entity's state to world
            if World.getEntityExists entity world then
                if World.getEntityDestroying entity world
                then World.destroyEntityImmediate entity world
                else failwith ("Entity '" + scstring entity + "' already exists and cannot be created.")
            World.addEntity entityState entity world

            // update publish update flag
            World.updateEntityPublishUpdateFlag entity world |> ignore<bool>

            // update presence property from override
            World.updateEntityPresenceOverride entity world

            // process entity first time if in the middle of simulant update phase
            if not skipProcessing && WorldModule.UpdatingSimulants && World.getEntitySelected entity world then
                WorldModule.tryProcessEntity true entity world

            // propagate properties
            match Option.bind (flip tryResolve entity) (World.getEntityMountOpt entity world) with
            | Some mount ->
                // NOTE: this results in an n^2 application of propagation, which might need to be optimized by
                // creating a new function that only propagates from the mount to this individual entity.
                World.propagateEntityAffineMatrix mount world
                World.propagateEntityElevation mount world
                World.propagateEntityEnabled mount world
                World.propagateEntityVisible mount world
            | None when World.getEntityMounted entity world ->
                World.propagateEntityAffineMatrix entity world
                World.propagateEntityElevation entity world
                World.propagateEntityEnabled entity world
                World.propagateEntityVisible entity world
            | None -> ()

            // insert a propagated descriptor if needed
            match World.getEntityPropagatedDescriptorOpt entity world with
            | None when World.hasPropagationTargets entity world ->
                let propagatedDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
                World.setEntityPropagatedDescriptorOpt (Some propagatedDescriptor) entity world |> ignore<bool>
            | Some _ | None -> ()

            // fin
            entity

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> mountOpt overlayDescriptor surnamesOpt group world =
            World.createEntity7 false typeof<'d>.Name mountOpt overlayDescriptor surnamesOpt group world

        /// Change the dispatcher of the given entity.
        static member changeEntityDispatcher dispatcherName entity world =
            let dispatcherNameCurrent = getTypeName (World.getEntityDispatcher entity world)
            if dispatcherNameCurrent <> dispatcherName then
                let dispatchers = World.getEntityDispatchers world
                if dispatchers.ContainsKey dispatcherName then
                    let entityDescriptor = World.writeEntity true true EntityDescriptor.empty entity world
                    let entityDescriptor = { entityDescriptor with EntityDispatcherName = dispatcherName }
                    World.destroyEntityImmediate entity world
                    World.readEntity true true entityDescriptor (Some entity.Name) entity.Parent world |> ignore<Entity>

        /// Write an entity to an entity descriptor.
        static member writeEntity writeOrder writePropagationHistory (entityDescriptor : EntityDescriptor) (entity : Entity) world =
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
                if propertyName = "Order" then
                    writeOrder && entityState.Order <> 0
                elif propertyName = "PropagatedDescriptorOpt" then
                    writePropagationHistory && entityState.PropagatedDescriptorOpt.IsSome
                elif propertyName = Constants.Engine.OverlayNameOptPropertyName && propertyType = typeof<string option> then
                    World.getEntityDefaultOverlayName entityDispatcherName world <> (propertyValue :?> string option)
                else
                    match overlaySymbolsOpt with
                    | Some overlaySymbols -> Overlayer.shouldPropertySerialize propertyName propertyType entityState overlaySymbols
                    | None -> true
            let entityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            let entityDescriptor = { entityDescriptor with EntityProperties = entityProperties }
            let entityDescriptor =
                if not (Gen.isNameGenerated entity.Name)
                then EntityDescriptor.setNameOpt (Some entity.Name) entityDescriptor
                else entityDescriptor
            let entities = World.getEntityChildren entity world
            { entityDescriptor with EntityDescriptors = World.writeEntities writeOrder writePropagationHistory entities world }

        /// Write multiple entities to a group descriptor.
        static member writeEntities writeOrder writePropagationHistory entities world =
            entities
            |> Seq.sortBy (fun (entity : Entity) -> World.getEntityOrder entity world)
            |> Seq.filter (fun (entity : Entity) -> World.getEntityPersistent entity world && not (World.getEntityProtected entity world))
            |> Seq.fold (fun entityDescriptors entity -> World.writeEntity writeOrder writePropagationHistory EntityDescriptor.empty entity world :: entityDescriptors) []
            |> Seq.rev
            |> Seq.toList

        /// Write an entity to a file.
        static member writeEntityToFile writeOrder writePropagationHistory (filePath : string) enity world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<GameDescriptor>).PrettyPrinter
            let enityDescriptor = World.writeEntity writeOrder writePropagationHistory EntityDescriptor.empty enity world
            let enityDescriptorStr = scstring enityDescriptor
            let enityDescriptorPretty = PrettyPrinter.prettyPrint enityDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, enityDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read an entity from an entity descriptor.
        static member readEntity tryReadOrder tryReadPropagationHistory (entityDescriptor : EntityDescriptor) (nameOpt : string option) (parent : Simulant) world =

            // optionally filter entity properties
            let entityProperties = entityDescriptor.EntityProperties
            let entityProperties = if tryReadOrder then entityProperties else Map.remove "Order" entityDescriptor.EntityProperties
            let entityProperties = if tryReadPropagationHistory then entityProperties else Map.remove "PropagatedDescriptorOpt" entityDescriptor.EntityProperties

            // make the dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcherName = entityDescriptor.EntityDispatcherName
            let (dispatcherName, dispatcher) =
                match dispatchers.TryGetValue dispatcherName with
                | (true, dispatcher) -> (dispatcherName, dispatcher)
                | (false, _) -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'.")

            // get the default overlay name option
            let defaultOverlayNameOpt = World.getEntityDefaultOverlayName dispatcherName world

            // make the bare entity state with name as id
            let mountOpt =
                match entityProperties.TryGetValue Constants.Engine.MountOptPropertyName with
                | (true, mountOpt) -> symbolToValue mountOpt
                | (false, _) -> Some Address.parent // HACK: this is to preserve legacy behavior where entities written without a mount are mounted to the parent.
            let entityState = EntityState.make mountOpt None defaultOverlayNameOpt dispatcher

            // attach the entity state's intrinsic properties
            let facetMap = World.getFacets world
            let dispatcherType = getType dispatcher
            let dispatcherTypes = dispatcherType :: Reflection.getBaseTypesExceptObject dispatcherType |> List.rev
            for (facetNames, ty) in List.map (fun ty -> (Reflection.getIntrinsicFacetNamesNoInherit ty, ty)) dispatcherTypes do
                Reflection.attachIntrinsicFacetsViaNames dispatchers facetMap facetNames entityState world
                let definitions = Reflection.getPropertyDefinitionsNoInherit ty
                Reflection.attachPropertiesViaDefinitions definitions entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let overlayer = World.getOverlayer world
            Reflection.tryReadOverlayNameOptToTarget entityProperties entityState
            if Option.isNone entityState.OverlayNameOpt then entityState.OverlayNameOpt <- defaultOverlayNameOpt
            match (defaultOverlayNameOpt, entityState.OverlayNameOpt) with
            | (Some defaultOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames defaultOverlayName overlayName entityState overlayer overlayer
            | (_, _) -> ()

            // read the entity state's facet names
            Reflection.readFacetNamesToTarget entityProperties entityState

            // synchronize the entity state's facets (and attach their properties)
            match World.trySynchronizeFacetsToNames Set.empty entityState None world with
            | Right () -> ()
            | Left error -> Log.error error

            // attempt to apply the entity state's overlay
            match entityState.OverlayNameOpt with
            | Some overlayName ->
                // OPTIMIZATION: applying overlay only when it will change something
                if Overlay.dispatcherNameToOverlayName dispatcherName <> overlayName then
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    Overlayer.applyOverlay dispatcherName overlayName facetNames entityState overlayer
            | None -> ()

            // try to read entity name
            let entityNameOpt = EntityDescriptor.getNameOpt entityDescriptor

            // read the entity state's values
            Reflection.readPropertiesToTarget entityProperties entityState

            // populate local angles value from local rotation
            entityState.AnglesLocal <- entityState.RotationLocal.RollPitchYaw

            // configure the name and surnames
            let (name, surnames) =
                match nameOpt with
                | Some name -> (name, Array.add name parent.SimulantAddress.Names)
                | None ->
                    match entityNameOpt with
                    | Some entityName -> (entityName, Array.add entityName parent.SimulantAddress.Names)
                    | None ->
                        let name = Gen.name
                        let surnames = Array.add name parent.SimulantAddress.Names
                        (name, surnames)
            let entityState = { entityState with Surnames = surnames }

            // make entity address
            let entityAddress = parent.SimulantAddress.Names |> Array.add name |> rtoa

            // make entity reference
            let entity = Entity entityAddress

            // add entity's state to world, destroying any existing entity if appropriate
            if World.getEntityExists entity world then
                if World.getEntityDestroying entity world
                then World.destroyEntityImmediateInternal true entity world
                else failwith ("Entity '" + scstring entity + "' already exists and cannot be created.")
            World.addEntity entityState entity world

            // update publish update flag
            World.updateEntityPublishUpdateFlag entity world |> ignore<bool>

            // update presence property from override
            World.updateEntityPresenceOverride entity world

            // update mount hierarchy
            let mountOpt = World.getEntityMountOpt entity world
            World.addEntityToMounts mountOpt entity world

            // read the entity's children
            World.readEntities tryReadOrder tryReadPropagationHistory entityDescriptor.EntityDescriptors entity world |> ignore<Entity list>

            // process entity first time if in the middle of simulant update phase
            if WorldModule.UpdatingSimulants && World.getEntitySelected entity world then
                WorldModule.tryProcessEntity true entity world

            // insert a propagated descriptor if needed
            match World.getEntityPropagatedDescriptorOpt entity world with
            | None when World.hasPropagationTargets entity world ->
                let propagatedDescriptor = World.writeEntity false false EntityDescriptor.empty entity world
                World.setEntityPropagatedDescriptorOpt (Some propagatedDescriptor) entity world |> ignore<bool>
            | Some _ | None -> ()

            // fin
            entity

        /// Read an entity from a file.
        static member readEntityFromFile tryReadOrder tryReadPropagationHistory filePath nameOpt parent world =
            let entityDescriptorStr = File.ReadAllText filePath
            let entityDescriptor = scvalue<EntityDescriptor> entityDescriptorStr
            World.readEntity tryReadOrder tryReadPropagationHistory entityDescriptor nameOpt parent world

        /// Read multiple entities.
        static member readEntities tryReadOrder tryReadPropagationHistory (entityDescriptors : EntityDescriptor list) (parent : Simulant) world =
            [for entityDescriptor in entityDescriptors do
                if String.notEmpty entityDescriptor.EntityDispatcherName then
                    let nameOpt = EntityDescriptor.getNameOpt entityDescriptor
                    World.readEntity tryReadOrder tryReadPropagationHistory entityDescriptor nameOpt parent world
                else Log.info "Entity with empty dispatcher name encountered."]

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOverlayNameOpt overlayNameOpt entity world =
            let entityState = World.getEntityState entity world
            let visibleInViewOld = entityState.VisibleInView
            let staticInPlayOld = entityState.StaticInPlay
            let lightProbeOld = entityState.LightProbe
            let lightOld = entityState.Light
            let presenceOld = entityState.Presence
            let presenceInPlayOld = entityState.PresenceInPlay
            let boundsOld = entityState.Bounds
            let entityState = World.getEntityState entity world
            let overlayerNameOldOpt = entityState.OverlayNameOpt
            entityState.OverlayNameOpt <- overlayNameOpt
            match (overlayerNameOldOpt, overlayNameOpt) with
            | (Some overlayerNameOld, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let facetNamesOld = entityState.FacetNames
                Overlayer.applyOverlayToFacetNames overlayerNameOld overlayName entityState overlayer overlayer
                match World.trySynchronizeFacetsToNames facetNamesOld entityState (Some entity) world with
                | Right () -> ()
                | Left error -> Log.error error
                let facetNames = World.getEntityFacetNamesReflectively entityState
                Overlayer.applyOverlay overlayerNameOld overlayName facetNames entityState overlayer
                World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                World.publishEntityChanges entity world
                Right ()
            | (None, None) ->
                Right ()
            | (_, _) ->
                Left "Could not set the entity's overlay name to None because doing so is currently not implemented."
            
        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            let facetNamesOld = entityState.FacetNames
            if facetNames <> facetNamesOld then
                match World.trySetFacetNames facetNames entityState (Some entity) world with
                | Right () ->
                    let visibleInViewOld = entityState.VisibleInView
                    let staticInPlayOld = entityState.StaticInPlay
                    let lightProbeOld = entityState.LightProbe
                    let lightOld = entityState.Light
                    let presenceOld = entityState.Presence
                    let presenceInPlayOld = entityState.PresenceInPlay
                    let boundsOld = entityState.Bounds
                    World.updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld presenceOld presenceInPlayOld boundsOld entity world
                    World.publishEntityChange Constants.Engine.FacetNamesPropertyName facetNamesOld entityState.FacetNames true entity world
                    World.publishEntityChanges entity world
                    Right true
                | Left error -> Left error
            else Right false

        /// Set the entity's facet names.
        static member setEntityFacetNames facetNames entity world =
            match World.trySetEntityFacetNames facetNames entity world with
            | Right changed -> changed
            | Left _ -> false

        static member internal updateEntityInEntityTree visibleInViewOld staticInPlayOld lightProbeOld lightOld (presenceOld : Presence) (presenceInPlayOld : Presence) boundsOld (entity : Entity) world : unit =

            // only do this when entity is selected
            if WorldModule.getSelected entity world then

                // OPTIMIZATION: work with the entity state directly to avoid function call overheads
                let entityState = World.getEntityState entity world
                let visibleInViewNew = entityState.VisibleInView
                let staticInPlayNew = entityState.StaticInPlay
                let lightProbeNew = entityState.LightProbe
                let lightNew = entityState.Light
                let presenceNew = entityState.Presence
                let presenceInPlayNew = entityState.PresenceInPlay
                let boundsNew = entityState.Bounds

                // OPTIMIZATION: only update when relevant entity state has changed.
                if  visibleInViewNew <> visibleInViewOld ||
                    staticInPlayNew <> staticInPlayOld ||
                    lightProbeNew <> lightProbeOld ||
                    lightNew <> lightOld ||
                    presenceNeq presenceNew presenceOld ||
                    presenceNeq presenceInPlayNew presenceInPlayOld ||
                    box3Neq boundsOld boundsNew then

                    // update entity in entity tree
                    if entityState.Is2d then
                        let quadree = World.getQuadtree world
                        let element = Quadelement.make visibleInViewNew staticInPlayNew presenceNew presenceInPlayNew boundsNew.Box2 entity
                        Quadtree.updateElement presenceOld presenceInPlayOld boundsOld.Box2 presenceNew presenceInPlayNew boundsNew.Box2 element quadree
                    else
                        let octree = World.getOctree world
                        let element = Octelement.make visibleInViewNew staticInPlayNew lightProbeNew lightNew presenceNew presenceInPlayNew boundsNew entity
                        Octree.updateElement presenceOld presenceInPlayOld boundsOld presenceNew presenceInPlayNew boundsNew element octree

        static member internal viewEntityProperties entity world =
            let state = World.getEntityState entity world
            World.viewSimulantStateProperties state

        /// Notify the engine that an entity's MMCC model has changed in some automatically undetectable way (such as being mutated directly by user code).
        static member notifyEntityModelChange entity world =
            let entityState = World.getEntityState entity world
            entityState.Dispatcher.TrySynchronize (false, false, entity, world)
            let publishChangeEvents = entityState.PublishChangeEvents
            World.publishEntityChange Constants.Engine.ModelPropertyName entityState.Model.DesignerValue entityState.Model.DesignerValue publishChangeEvents entity world

    /// Initialize property getters.
    let private initGetters () =
        let entityGetters =
            dictPlus StringComparer.Ordinal
                [("Dispatcher", fun entity world -> { PropertyType = typeof<EntityDispatcher>; PropertyValue = World.getEntityDispatcher entity world })
                 ("Facets", fun entity world -> { PropertyType = typeof<Facet array>; PropertyValue = World.getEntityFacets entity world })
                 ("Transform", fun entity world -> { PropertyType = typeof<Transform>; PropertyValue = (World.getEntityState entity world).Transform })
                 ("PerimeterCenter", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterCenter entity world })
                 ("PerimeterBottom", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterBottom entity world })
                 ("PerimeterBottomLeft", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterBottomLeft entity world })
                 ("PerimeterMin", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterMin entity world })
                 ("PerimeterMax", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterMax entity world })
                 ("PerimeterCenterLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterCenterLocal entity world })
                 ("PerimeterBottomLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterBottomLocal entity world })
                 ("PerimeterBottomLeftLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterBottomLeftLocal entity world })
                 ("PerimeterMinLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterMinLocal entity world })
                 ("PerimeterMaxLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPerimeterMaxLocal entity world })
                 ("Position", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPosition entity world })
                 ("PositionLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPositionLocal entity world })
                 ("Rotation", fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotation entity world })
                 ("RotationLocal", fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotationLocal entity world })
                 ("Scale", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScale entity world })
                 ("ScaleLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScaleLocal entity world })
                 ("Offset", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityOffset entity world })
                 ("Angles", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAngles entity world })
                 ("AnglesLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAnglesLocal entity world })
                 ("Degrees", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegrees entity world })
                 ("DegreesLocal", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegreesLocal entity world })
                 ("Size", fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntitySize entity world })
                 ("Elevation", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevation entity world })
                 ("ElevationLocal", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevationLocal entity world })
                 ("Overflow", fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityOverflow entity world })
                 ("PerimeterUnscaled", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeterUnscaled entity world })
                 ("Perimeter", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeter entity world })
                 ("Bounds", fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityBounds entity world })
                 ("Presence", fun entity world -> { PropertyType = typeof<Presence>; PropertyValue = World.getEntityPresence entity world })
                 ("PresenceOverride", fun entity world -> { PropertyType = typeof<Presence voption>; PropertyValue = World.getEntityPresenceOverride entity world })
                 ("Absolute", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAbsolute entity world })
                 ("Model", fun entity world -> let designerProperty = World.getEntityModelProperty entity world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue })
                 ("MountOpt", fun entity world -> { PropertyType = typeof<Entity Address option>; PropertyValue = World.getEntityMountOpt entity world })
                 ("PropagationSourceOpt", fun entity world -> { PropertyType = typeof<Entity option>; PropertyValue = World.getEntityPropagationSourceOpt entity world })
                 ("PublishChangeEvents", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeEvents entity world })
                 ("Enabled", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabled entity world })
                 ("EnabledLocal", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabledLocal entity world })
                 ("Visible", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisible entity world })
                 ("VisibleLocal", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisibleLocal entity world })
                 ("CastShadow", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityCastShadow entity world })
                 ("Pickable", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPickable entity world })
                 ("AlwaysUpdate", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysUpdate entity world })
                 ("AlwaysRender", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysRender entity world })
                 ("PublishUpdates", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishUpdates entity world })
                 ("Protected", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityProtected entity world })
                 ("Persistent", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPersistent entity world })
                 ("Mounted", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityMounted entity world })
                 ("Is2d", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIs2d entity world })
                 ("Is3d", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIs3d entity world })
                 ("Static", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityStatic entity world })
                 ("LightProbe", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityLightProbe entity world })
                 ("Light", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityLight entity world })
                 ("Physical", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPhysical entity world })
                 ("Optimized", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityOptimized entity world })
                 ("Destroying", fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityDestroying entity world })
                 ("OverlayNameOpt", fun entity world -> { PropertyType = typeof<string option>; PropertyValue = World.getEntityOverlayNameOpt entity world })
                 ("FacetNames", fun entity world -> { PropertyType = typeof<string Set>; PropertyValue = World.getEntityFacetNames entity world })
                 ("PropagatedDescriptorOpt", fun entity world -> { PropertyType = typeof<EntityDescriptor option>; PropertyValue = World.getEntityPropagatedDescriptorOpt entity world })
                 ("Order", fun entity world -> { PropertyType = typeof<int64>; PropertyValue = World.getEntityOrder entity world })
                 ("Id", fun entity world -> { PropertyType = typeof<Guid>; PropertyValue = World.getEntityId entity world })
                 ("Surnames", fun entity world -> { PropertyType = typeof<string array>; PropertyValue = World.getEntitySurnames entity world })
                 ("Name", fun entity world -> { PropertyType = typeof<string>; PropertyValue = World.getEntityName entity world })]
        EntityGetters <- entityGetters.ToFrozenDictionary ()

    /// Initialize property setters.
    let private initSetters () =
        let entitySetters =
            dictPlus StringComparer.Ordinal
                [("Transform", fun property entity world -> let mutable transform = property.PropertyValue :?> Transform in World.setEntityTransformByRef (&transform, World.getEntityState entity world, entity, world))
                 ("PerimeterCenter", fun property entity world -> World.setEntityPerimeterCenter (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterBottom", fun property entity world -> World.setEntityPerimeterBottom (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterBottomLeft", fun property entity world -> World.setEntityPerimeterBottomLeft (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterMin", fun property entity world -> World.setEntityPerimeterMin (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterMax", fun property entity world -> World.setEntityPerimeterMax (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterCenterLocal", fun property entity world -> World.setEntityPerimeterCenterLocal (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterBottomLocal", fun property entity world -> World.setEntityPerimeterBottomLocal (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterBottomLeftLocal", fun property entity world -> World.setEntityPerimeterBottomLeftLocal (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterMinLocal", fun property entity world -> World.setEntityPerimeterMinLocal (property.PropertyValue :?> Vector3) entity world)
                 ("PerimeterMaxLocal", fun property entity world -> World.setEntityPerimeterMaxLocal (property.PropertyValue :?> Vector3) entity world)
                 ("Position", fun property entity world -> World.setEntityPosition (property.PropertyValue :?> Vector3) entity world)
                 ("PositionLocal", fun property entity world -> World.setEntityPositionLocal (property.PropertyValue :?> Vector3) entity world)
                 ("Rotation", fun property entity world -> World.setEntityRotation (property.PropertyValue :?> Quaternion) entity world)
                 ("RotationLocal", fun property entity world -> World.setEntityRotationLocal (property.PropertyValue :?> Quaternion) entity world)
                 ("Scale", fun property entity world -> World.setEntityScale (property.PropertyValue :?> Vector3) entity world)
                 ("ScaleLocal", fun property entity world -> World.setEntityScaleLocal (property.PropertyValue :?> Vector3) entity world)
                 ("Offset", fun property entity world -> World.setEntityOffset (property.PropertyValue :?> Vector3) entity world)
                 ("Angles", fun property entity world -> World.setEntityAngles (property.PropertyValue :?> Vector3) entity world)
                 ("AnglesLocal", fun property entity world -> World.setEntityAnglesLocal (property.PropertyValue :?> Vector3) entity world)
                 ("Degrees", fun property entity world -> World.setEntityDegrees (property.PropertyValue :?> Vector3) entity world)
                 ("DegreesLocal", fun property entity world -> World.setEntityDegreesLocal (property.PropertyValue :?> Vector3) entity world)
                 ("Size", fun property entity world -> World.setEntitySize (property.PropertyValue :?> Vector3) entity world)
                 ("Elevation", fun property entity world -> World.setEntityElevation (property.PropertyValue :?> single) entity world)
                 ("ElevationLocal", fun property entity world -> World.setEntityElevationLocal (property.PropertyValue :?> single) entity world)
                 ("Overflow", fun property entity world -> World.setEntityOverflow (property.PropertyValue :?> single) entity world)
                 ("PerimeterUnscaled", fun property entity world -> World.setEntityPerimeterUnscaled (property.PropertyValue :?> Box3) entity world)
                 ("Perimeter", fun property entity world -> World.setEntityPerimeter (property.PropertyValue :?> Box3) entity world)
                 ("Presence", fun property entity world -> World.setEntityPresence (property.PropertyValue :?> Presence) entity world)
                 ("Absolute", fun property entity world -> World.setEntityAbsolute (property.PropertyValue :?> bool) entity world)
                 ("Model", fun property entity world -> World.setEntityModelProperty false false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } entity world)
                 ("MountOpt", fun property entity world -> World.setEntityMountOpt (property.PropertyValue :?> Entity Address option) entity world)
                 ("PropagationSourceOpt", fun property entity world -> World.setEntityPropagationSourceOpt (property.PropertyValue :?> Entity option) entity world)
                 ("Enabled", fun property entity world -> World.setEntityEnabled (property.PropertyValue :?> bool) entity world)
                 ("EnabledLocal", fun property entity world -> World.setEntityEnabledLocal (property.PropertyValue :?> bool) entity world)
                 ("Visible", fun property entity world -> World.setEntityVisible (property.PropertyValue :?> bool) entity world)
                 ("VisibleLocal", fun property entity world -> World.setEntityVisibleLocal (property.PropertyValue :?> bool) entity world)
                 ("CastShadow", fun property entity world -> World.setEntityCastShadow (property.PropertyValue :?> bool) entity world)
                 ("Pickable", fun property entity world -> World.setEntityPickable (property.PropertyValue :?> bool) entity world)
                 ("Static", fun property entity world -> World.setEntityStatic (property.PropertyValue :?> bool) entity world)
                 ("AlwaysUpdate", fun property entity world -> World.setEntityAlwaysUpdate (property.PropertyValue :?> bool) entity world)
                 ("AlwaysRender", fun property entity world -> World.setEntityAlwaysRender (property.PropertyValue :?> bool) entity world)
                 ("Persistent", fun property entity world -> World.setEntityPersistent (property.PropertyValue :?> bool) entity world)
                 ("FacetNames", fun property entity world -> World.setEntityFacetNames (property.PropertyValue :?> string Set) entity world)
                 ("PropagatedDescriptorOpt", fun property entity world -> World.setEntityPropagatedDescriptorOpt (property.PropertyValue :?> EntityDescriptor option) entity world)
                 ("Order", fun property entity world -> World.setEntityOrder (property.PropertyValue :?> int64) entity world)]
        EntitySetters <- entitySetters.ToFrozenDictionary ()

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()