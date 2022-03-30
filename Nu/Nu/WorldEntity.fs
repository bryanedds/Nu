// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.IO
open FSharpx.Collections
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldEntityModule =

    type Entity with

        member this.GetDispatcher world = World.getEntityDispatcher this world
        member this.Dispatcher = lensReadOnly Property? Dispatcher this.GetDispatcher this
        member this.GetModelGeneric<'a> world = World.getEntityModel<'a> this world
        member this.SetModelGeneric<'a> value world = World.setEntityModel<'a> value this world |> snd'
        member this.ModelGeneric<'a> () = lens Property? Model this.GetModelGeneric<'a> this.SetModelGeneric<'a> this
        member this.GetEcs world = World.getScreenEcs this.Screen world
        member this.Ecs = lensReadOnly Property? Ecs this.GetEcs this
        member this.GetFacets world = World.getEntityFacets this world
        member this.Facets = lensReadOnly Property? Facets this.GetFacets this
        member this.GetTransform world = (World.getEntityState this world).Transform
        member this.SetTransform value world = World.setEntityTransformByRef (&value, World.getEntityState this world, this, world) |> snd'
        member this.Transform = lens Property? Transform this.GetTransform this.SetTransform this
        member this.GetBounds world = World.getEntityBounds this world
        member this.SetBounds value world = World.setEntityBounds value this world |> snd'
        member this.Bounds = lens Property? Bounds this.GetBounds this.SetBounds this
        member this.GetPosition world = World.getEntityPosition this world
        member this.SetPosition value world = World.setEntityPosition value this world |> snd'
        member this.Position = lens Property? Position this.GetPosition this.SetPosition this
        member this.GetPositionLocal world = World.getEntityPositionLocal this world
        member this.SetPositionLocal value world = World.setEntityPositionLocal value this world |> snd'
        member this.PositionLocal = lens Property? PositionLocal this.GetPositionLocal this.SetPositionLocal this
        member this.GetCenter world = World.getEntityCenter this world
        member this.SetCenter value world = World.setEntityCenter value this world |> snd'
        member this.Center = lens Property? Center this.GetCenter this.SetCenter this
        member this.GetBottom world = World.getEntityBottom this world
        member this.SetBottom value world = World.setEntityBottom value this world |> snd'
        member this.Bottom = lens Property? Bottom this.GetBottom this.SetBottom this
        member this.GetSize world = World.getEntitySize this world
        member this.SetSize value world = World.setEntitySize value this world |> snd'
        member this.Size = lens Property? Size this.GetSize this.SetSize this
        member this.GetRotation world = World.getEntityRotation this world
        member this.SetRotation value world = World.setEntityRotation value this world |> snd'
        member this.Rotation = lens Property? Rotation this.GetRotation this.SetRotation this
        member this.GetAngle world = World.getEntityAngle this world
        member this.SetAngle value world = World.setEntityAngle value this world |> snd'
        member this.Angle = lens Property? Angle this.GetAngle this.SetAngle this
        member this.GetElevation world = World.getEntityElevation this world
        member this.SetElevation value world = World.setEntityElevation value this world |> snd'
        member this.Elevation = lens Property? Elevation this.GetElevation this.SetElevation this
        member this.GetElevationLocal world = World.getEntityElevationLocal this world
        member this.SetElevationLocal value world = World.setEntityElevationLocal value this world |> snd'
        member this.ElevationLocal = lens Property? ElevationLocal this.GetElevationLocal this.SetElevationLocal this
        member this.GetFlags world = World.getEntityFlags this world
        member this.Flags = lensReadOnly Property? Flags this.GetFlags this
        member this.GetOmnipresent world = World.getEntityOmnipresent this world
        member this.SetOmnipresent value world = World.setEntityOmnipresent value this world |> snd'
        member this.Omnipresent = lens Property? Omnipresent this.GetOmnipresent this.SetOmnipresent this
        member this.GetAbsolute world = World.getEntityAbsolute this world
        member this.SetAbsolute value world = World.setEntityAbsolute value this world |> snd'
        member this.Absolute = lens Property? Absolute this.GetAbsolute this.SetAbsolute this
        member this.GetOverflow world = World.getEntityOverflow this world
        member this.SetOverflow value world = World.setEntityOverflow value this world |> snd'
        member this.Overflow = lens Property? Overflow this.GetOverflow this.SetOverflow this
        member this.GetParentOpt world = World.getEntityParentOpt this world
        member this.SetParentOpt value world = World.setEntityParentOpt value this world |> snd'
        member this.ParentOpt = lens Property? ParentOpt this.GetParentOpt this.SetParentOpt this
        member this.GetImperative world = World.getEntityImperative this world
        member this.SetImperative value world = World.setEntityImperative value this world |> snd'
        member this.Imperative = lens Property? Imperative this.GetImperative this.SetImperative this
        member this.GetEnabled world = World.getEntityEnabled this world
        member this.SetEnabled value world = World.setEntityEnabled value this world |> snd'
        member this.Enabled = lens Property? Enabled this.GetEnabled this.SetEnabled this
        member this.GetEnabledLocal world = World.getEntityEnabledLocal this world
        member this.SetEnabledLocal value world = World.setEntityEnabledLocal value this world |> snd'
        member this.EnabledLocal = lens Property? EnabledLocal this.GetEnabledLocal this.SetEnabledLocal this
        member this.GetVisible world = World.getEntityVisible this world
        member this.SetVisible value world = World.setEntityVisible value this world |> snd'
        member this.Visible = lens Property? Visible this.GetVisible this.SetVisible this
        member this.GetVisibleLocal world = World.getEntityVisibleLocal this world
        member this.SetVisibleLocal value world = World.setEntityVisibleLocal value this world |> snd'
        member this.VisibleLocal = lens Property? VisibleLocal this.GetVisibleLocal this.SetVisibleLocal this
        member this.GetAlwaysUpdate world = World.getEntityAlwaysUpdate this world
        member this.SetAlwaysUpdate value world = World.setEntityAlwaysUpdate value this world |> snd'
        member this.AlwaysUpdate = lens Property? AlwaysUpdate this.GetAlwaysUpdate this.SetAlwaysUpdate this
        member this.GetPersistent world = World.getEntityPersistent this world
        member this.SetPersistent value world = World.setEntityPersistent value this world |> snd'
        member this.Persistent = lens Property? Persistent this.GetPersistent this.SetPersistent this
        member this.GetIgnorePropertyBindings world = World.getEntityIgnorePropertyBindings this world
        member this.SetIgnorePropertyBindings value world = World.setEntityIgnorePropertyBindings value this world |> snd'
        member this.IgnorePropertyBindings = lens Property? IgnorePropertyBindings this.GetIgnorePropertyBindings this.SetIgnorePropertyBindings this
        member this.GetOptimized world = World.getEntityOptimized this world
        member this.Optimized = lensReadOnly Property? Optimized this.GetOptimized this
        member this.GetDestroying world = World.getEntityDestroying this world
        member this.Destroying = lensReadOnly Property? Destroying this.GetDestroying this
        member this.GetScriptFrame world = World.getEntityScriptFrame this world
        member this.ScriptFrame = lensReadOnly Property? Script this.GetScriptFrame this
        member this.GetOverlayNameOpt world = World.getEntityOverlayNameOpt this world
        member this.OverlayNameOpt = lensReadOnly Property? OverlayNameOpt this.GetOverlayNameOpt this
        member this.GetFacetNames world = World.getEntityFacetNames this world
        member this.FacetNames = lensReadOnly Property? FacetNames this.GetFacetNames this
        member this.GetCreationTimeStamp world = World.getEntityCreationTimeStamp this world
        member this.CreationTimeStamp = lensReadOnly Property? CreationTimeStamp this.GetCreationTimeStamp this
        member this.GetId world = World.getEntityId this world
        member this.Id = lensReadOnly Property? Id this.GetId this

        member this.RegisterEvent = Events.Register --> this
        member this.UnregisteringEvent = Events.Unregistering --> this
        member this.ChangeEvent propertyName = Events.Change propertyName --> this
        member this.UpdateEvent = Events.Update --> this
#if !DISABLE_ENTITY_POST_UPDATE
        member this.PostUpdateEvent = Events.PostUpdate --> this
#endif

        /// The state of an entity.
        /// The only place this accessor should be used is in performance-sensitive code.
        /// Otherwise, you should get and set the required entity properties via the Entity interface.
        member this.State world =
            let entityState = World.getEntityState this world
#if DEBUG
            if World.getImperative world && not entityState.Optimized then
                failwith "Can get the entity state of an entity only if it is Optimized (Imperative, Omnipresent, and not PublishChangeEvents)."
#endif
            entityState

        /// The copied state of an entity.
        /// The only place this accessor should be used is in performance-sensitive code.
        /// Otherwise, you should get and set the required entity properties via the Entity interface.
        member this.StateReadOnly world =
            world |> World.getEntityState this |> EntityState.copy

        /// Optimize an entity by setting { Imperative = true; Omnipresent = true }.
        member this.Optimize world =
            let world = this.SetImperative true world
            let world = this.SetOmnipresent true world
            world

        /// Set the transform of an entity.
        member this.SetTransformByRef (value : Transform inref, world) =
            World.setEntityTransformByRef (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity without generating any change events.
        member this.SetTransformByRefWithoutEvent (value : Transform inref, world) =
            World.setEntityTransformByRefWithoutEvent (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity without generating any change events.
        member this.SetTransformWithoutEvent value world =
            World.setEntityTransformByRefWithoutEvent (&value, World.getEntityState this world, this, world)

        /// Set the transform of an entity snapped to the give position and rotation snaps.
        member this.SetTransformSnapped positionSnap rotationSnap value world =
            let transform = Math.snapTransform positionSnap rotationSnap value
            this.SetTransform transform world

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetEntityProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getEntityProperty propertyName this world

        /// Get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a =
            let mutable property = Unchecked.defaultof<Property>
            if World.tryGetEntityXtensionProperty (propertyName, this, world, &property)
            then property.PropertyValue :?> 'a
            else Unchecked.defaultof<'a>

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getEntityXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetEntityProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setEntityProperty propertyName property this world |> snd'

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            World.trySetEntityXtensionProperty propertyName property this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            World.setEntityXtensionValue<'a> propertyName value this world

        /// Set an xtension property value without publishing an event.
        member internal this.SetXtensionPropertyWithoutEvent<'a> propertyName (value : 'a) world =
            let property = { PropertyType = typeof<'a>; PropertyValue = value }
            let struct (_, _, world) = World.setEntityXtensionPropertyWithoutEvent propertyName property this world
            world

        /// Attach a property.
        member this.AttachProperty propertyName property world =
            World.attachEntityProperty propertyName property this world

        /// Detach a property.
        member this.DetachProperty propertyName world =
            World.detachEntityProperty propertyName this world

        /// Get an entity's sorting priority.
        member this.GetSortingPriority world = World.getEntitySortingPriority this world

        /// Get an entity's quick size.
        member this.GetQuickSize world = World.getEntityQuickSize this world

        /// Get an entity's bounds, taking into account its overflow.
        member this.GetBoundsOverflow world = World.getEntityBoundsOverflow this world

        /// Get an entity's bounds maximum.
        member this.GetBoundsMax world = World.getEntityBoundsMax this world

        /// Check that an entity is in the camera's view.
        member this.GetInView world = World.getEntityInView this world

        /// Check that an entity is selected.
        member this.IsSelected world =
            let gameState = World.getGameState world
            match gameState.OmniScreenOpt with
            | Some omniScreen when Address.head this.EntityAddress = Address.head omniScreen.ScreenAddress -> true
            | _ ->
                match gameState.SelectedScreenOpt with
                | Some screen when Address.head this.EntityAddress = Address.head screen.ScreenAddress -> true
                | _ -> false

        /// Check that an entity exists in the world.
        member this.Exists world = World.getEntityExists this world

        /// Set an entity's size by its quick size.
        member this.QuickSize world = World.setEntitySize (this.GetQuickSize world) this world

        /// Set an entity's parent while adjusting its parent properties such that they do not change.
        member this.SetParentOptWithAdjustment (value : Entity Relation option) world =
            let world =
                match (this.GetParentOpt world, value) with
                | (Some relationOld, Some relationNew) ->
                    let parentOld = this.Resolve relationOld : Entity
                    let parentNew = this.Resolve relationNew : Entity
                    if parentOld.Exists world && parentNew.Exists world then
                        let position = this.GetPositionLocal world + parentNew.GetPosition world
                        let elevation = this.GetElevationLocal world + parentNew.GetElevation world
                        let world = this.SetPosition position world
                        let world = this.SetElevation elevation world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (Some relationOld, None) ->
                    let parentOld = this.Resolve relationOld
                    if parentOld.Exists world then
                        let position = this.GetPositionLocal world + parentOld.GetPosition world
                        let elevation = this.GetElevationLocal world + parentOld.GetElevation world
                        let world = this.SetPosition position world
                        let world = this.SetElevation elevation world
                        let world = this.SetVisible (this.GetVisibleLocal world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world) world
                        world
                    else world
                | (None, Some relationNew) ->
                    let parentNew = this.Resolve relationNew
                    if parentNew.Exists world then
                        let position = this.GetPosition world - parentNew.GetPosition world
                        let elevation = this.GetElevation world - parentNew.GetElevation world
                        let world = this.SetPositionLocal position world
                        let world = this.SetElevationLocal elevation world
                        let world = this.SetVisible (this.GetVisibleLocal world && parentNew.GetVisible world) world
                        let world = this.SetEnabled (this.GetEnabledLocal world && parentNew.GetEnabled world) world
                        world
                    else world
                | (None, None) -> world
            this.SetParentOpt value world

        /// Check whether the entity's parent exists.
        member this.ParentExists world =
            match this.GetParentOpt world with
            | Some relation -> (this.Resolve relation : Entity).Exists world
            | None -> false

        /// Get an entity's immediate children.
        member this.GetChildren world = World.getEntityChildren this world

        /// Get an entity's children recursively.
        member this.GetDescendants world = World.getEntityDescendants this world

        /// Traverse an entity's immediate children.
        member this.TraverseChildren effect world = World.traverseEntityChildren effect this world

        /// Traverse an entity's children recursively.
        member this.TraverseDescendants effect world = World.traverseEntityDescendants effect this world

        /// Apply physics changes to an entity.
        member this.ApplyPhysics position rotation linearVelocity angularVelocity world =
            let oldTransform = this.GetTransform world
            let mutable newTransform = oldTransform
            let world =
                if  oldTransform.Position <> position ||
                    oldTransform.Rotation <> rotation then
                    newTransform.Position <- position
                    newTransform.Rotation <- rotation
                    this.SetTransformByRefWithoutEvent (&newTransform, world)
                else world
            let world = this.SetXtensionPropertyWithoutEvent Property? LinearVelocity linearVelocity world
            let world = this.SetXtensionPropertyWithoutEvent Property? AngularVelocity angularVelocity world
            let dispatcher = this.GetDispatcher world
            dispatcher.ApplyPhysics (position, rotation, linearVelocity, angularVelocity, this, world)

        /// Propagate entity physics properties into the physics system.
        member this.PropagatePhysics world =
            if WorldModule.isSelected this world
            then World.propagateEntityPhysics this world
            else world

        /// Check that an entity uses a facet of the given type.
        member this.Has (facetType, world) = Array.exists (fun facet -> getType facet = facetType) (this.GetFacets world)

        /// Check that an entity uses a facet of the given type.
        member this.Has<'a> world = this.Has (typeof<'a>, world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that an entity dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Resolve a relation in the context of an entity.
        member this.Resolve relation = resolve<Entity> this relation

        /// Relate an entity to a simulant.
        member this.Relate simulant = relate<Entity> this simulant

        /// Get an entity's change event address.
        member this.GetChangeEvent propertyName = Events.Change propertyName --> this.EntityAddress

        /// Try to signal an entity's facet.
        member this.TrySignalFacet (signalObj : obj) facetName world = (this.GetDispatcher world).TrySignalFacet (signalObj, facetName, this, world)

        /// Try to signal an entity.
        member this.TrySignal signal world = (this.GetDispatcher world).TrySignal (signal, this, world)

    type World with

        static member internal updateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.Update (entity, world)
            let facets = entity.GetFacets world
            let world = Array.fold (fun world (facet : Facet) -> facet.Update (entity, world)) world facets
            if World.getEntityPublishUpdates entity world then
                let eventTrace = EventTrace.debug "World" "updateEntity" "" EventTrace.empty
                World.publishPlus () entity.UpdateEvent eventTrace Simulants.Game false false world
            else world

#if !DISABLE_ENTITY_POST_UPDATE
        static member internal postUpdateEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.PostUpdate (entity, world)
            let facets = entity.GetFacets world
            let world = Array.fold (fun world (facet : Facet) -> facet.PostUpdate (entity, world)) world facets
            if World.getEntityPublishPostUpdates entity world then
                let eventTrace = EventTrace.debug "World" "postUpdateEntity" "" EventTrace.empty
                World.publishPlus () entity.PostUpdateEvent eventTrace Simulants.Game false false world
            else world
#endif

        static member internal actualizeEntity (entity : Entity) world =
            let dispatcher = entity.GetDispatcher world
            let world = dispatcher.Actualize (entity, world)
            let facets = entity.GetFacets world
            Array.fold (fun world (facet : Facet) -> facet.Actualize (entity, world)) world facets

        /// Get all the entities contained by a group.
        [<FunctionBinding>]
        static member getEntities (group : Group) world =
            match Address.getNames group.GroupAddress with
            | [|screenName; groupName|] ->
                match UMap.tryFind screenName (World.getScreenDirectory world) with
                | Some groupDirectory ->
                    match UMap.tryFind groupName groupDirectory.Value with
                    | Some entityDirectory -> entityDirectory.Value |> USet.toSeq
                    | None -> failwith ("Invalid group address '" + scstring group.GroupAddress + "'.")
                | None -> failwith ("Invalid group address '" + scstring group.GroupAddress + "'.")
            | _ -> failwith ("Invalid group address '" + scstring group.GroupAddress + "'.")

        /// Destroy an entity in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyEntity (entity : Entity) world =
            World.addSimulantToDestruction entity world

        /// Destroy multiple entities in the world immediately. Can be dangerous if existing in-flight publishing
        /// depends on any of the entities' existences. Consider using World.destroyEntities instead.
        static member destroyEntitiesImmediate (entities : Entity seq) world =
            List.foldBack
                (fun entity world -> World.destroyEntityImmediate entity world)
                (List.ofSeq entities)
                world

        /// Destroy multiple entities in the world at the end of the current update.
        [<FunctionBinding>]
        static member destroyEntities entities world =
            World.frame (World.destroyEntitiesImmediate entities) world

        /// Sort the given entities.
        static member sortEntities entities world =
            /// OPTIMIZATION: using arrays for speed
            entities |>
            Array.ofSeq |>
            Array.rev |>
            Array.map (fun (entity : Entity) -> entity.GetSortingPriority world) |>
            Array.sortStableWith SortPriority.compare |>
            Array.map (fun p -> p.SortTarget :?> Entity)

        /// Try to pick an entity at the given position.
        [<FunctionBinding>]
        static member tryPickEntity position entities world =
            /// OPTIMIZATION: using arrays for speed
            let entitiesSorted = World.sortEntities entities world
            Array.tryFind
                (fun (entity : Entity) ->
                    let positionWorld = World.mouseToWorld (entity.GetAbsolute world) position world
                    let picked = Math.isPointInBounds positionWorld (entity.GetBounds world)
                    picked)
                entitiesSorted

        /// Try to find the entity in the given entity's group with the closest previous time stamp.
        static member tryFindPreviousEntity (entity : Entity) world =
            let timeStamp = World.getEntityCreationTimeStamp entity world
            let mutable previousTimeStampDeltaOpt = ValueNone
            let mutable previousOpt = ValueNone
            let entities = World.getEntities entity.Group world |> Seq.toArray
            for entity2 in entities do
                let timeStamp2 = World.getEntityCreationTimeStamp entity2 world
                let timeStampDelta = timeStamp - timeStamp2
                if timeStampDelta > 0L then
                    match previousTimeStampDeltaOpt with
                    | ValueSome timeStampDelta2 ->
                        if timeStampDelta < timeStampDelta2 then
                            previousTimeStampDeltaOpt <- ValueSome timeStampDelta
                            previousOpt <- ValueSome entity2
                    | ValueNone ->
                        previousTimeStampDeltaOpt <- ValueSome timeStampDelta
                        previousOpt <- ValueSome entity2
            match previousOpt with
            | ValueSome previous -> Some previous
            | ValueNone -> None

        /// Reorder an entity's creation time stamp between before and after's.
        static member reorderEntity entityBefore entityAfter entity world =
            let timeStampBefore = World.getEntityCreationTimeStamp entityBefore world;
            let timeStampAfter = World.getEntityCreationTimeStamp entityAfter world;
            let timeStamp = (timeStampBefore + timeStampAfter) / 2L;
            World.setEntityCreationTimeStamp timeStamp entity world |> snd'

        /// Reorder an entity's creation time stamp between target and its previous sibling's.
        static member insertEntity target entity world =
            match World.tryFindPreviousEntity target world with
            | Some previous -> World.reorderEntity previous target entity world
            | None -> world

        /// Write multiple entities to a group descriptor.
        static member writeEntities entities groupDescriptor world =
            entities |>
            Seq.sortBy (fun (entity : Entity) -> entity.GetCreationTimeStamp world) |>
            Seq.filter (fun (entity : Entity) -> entity.GetPersistent world) |>
            Seq.fold (fun entityDescriptors entity -> World.writeEntity entity EntityDescriptor.empty world :: entityDescriptors) groupDescriptor.EntitieDescriptors |>
            fun entityDescriptors -> { groupDescriptor with EntitieDescriptors = entityDescriptors }

        /// Write an entity to a file.
        [<FunctionBinding>]
        static member writeEntityToFile (filePath : string) enity world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.getOrDefault typeof<GameDescriptor>).PrettyPrinter
            let enityDescriptor = World.writeEntity enity EntityDescriptor.empty world
            let enityDescriptorStr = scstring enityDescriptor
            let enityDescriptorPretty = PrettyPrinter.prettyPrint enityDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, enityDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read multiple entities from a group descriptor.
        static member readEntities groupDescriptor group world =
            List.foldBack
                (fun entityDescriptor (entities, world) ->
                    let entityNamesOpt = EntityDescriptor.getNamesOpt entityDescriptor
                    let (entity, world) = World.readEntity entityDescriptor entityNamesOpt group world
                    (entity :: entities, world))
                    groupDescriptor.EntitieDescriptors
                    ([], world)

        /// Turn an entity lens into a series of live entities.
        static member expandEntities (lens : Lens<obj, World>) sieve unfold mapper origin owner group world =
            let mapperGeneralized = fun i a w -> mapper i a w :> SimulantContent
            World.expandSimulants lens sieve unfold mapperGeneralized origin owner group world

        /// Turn entity content into a live entity.
        static member expandEntityContent content origin (owner : Simulant) group world =
            if World.getGroupExists group world then
                match EntityContent.expand content group world with
                | Choice1Of3 (lens, sieve, unfold, mapper) ->
                    let world = World.expandEntities lens sieve unfold mapper origin owner group world
                    (None, world)
                | Choice2Of3 (entityName, descriptor, handlers, binds, content) ->
                    let entityNames =
                        match owner with
                        | :? Entity as ownerEntity -> Array.add entityName ownerEntity.Names
                        | _ -> [|entityName|]
                    let descriptor = { descriptor with SimulantNamesOpt = Some entityNames }
                    let (entity, world) = World.createEntity4 DefaultOverlay descriptor group world
                    let handlers =
                        List.map (fun (handler, eventAddress, _) ->
                            let eventNameIndex = Address.findIndex (fun name -> name = "Event") eventAddress
                            let partialAddress = Address.take (inc eventNameIndex) eventAddress
                            (handler, partialAddress --> entity, entity :> Simulant)) handlers
                    let binds = List.map (fun (_, left, right, twoWay) -> (entity :> Simulant, left, right, twoWay)) binds
                    let world =
                        // only set parent if one was not specified by the descriptor properties
                        if not (List.exists (fun (name, _) -> name = Property? ParentOpt) descriptor.SimulantProperties) then
                            let parentOpt = if owner :? Entity then Some (Relation.makeParent ()) else None
                            World.setEntityParentOpt parentOpt entity world |> snd'
                        else world
                    let world =
                        World.monitor
                            (fun _ world -> (Cascade, World.destroyEntity entity world))
                            (Events.Unregistering --> owner.SimulantAddress)
                            entity
                            world
                    let world =
                        List.fold (fun world (simulant, left : World Lens, right, twoWay) ->
                            if twoWay then
                                let world = WorldModule.bind5 simulant left right world
                                WorldModule.bind5 simulant right left world
                            else WorldModule.bind5 simulant left right world)
                            world binds
                    let world =
                        List.fold (fun world (handler, address, simulant) ->
                            World.monitor (fun (evt : Event) world ->
                                let signal = handler evt
                                let world =
                                    match origin with
                                    | SimulantOrigin simulant -> WorldModule.trySignal signal simulant world
                                    | FacetOrigin (simulant, facetName) -> WorldModule.trySignalFacet signal facetName simulant world
                                (Cascade, world))
                                address simulant world)
                            world handlers
                    let world =
                        List.fold (fun world content ->
                            World.expandEntityContent content origin entity group world |> snd)
                            world (snd content)
                    (Some entity, world)
                | Choice3Of3 (entityName, filePath) ->
                    let entityNames =
                        match owner with
                        | :? Entity as ownerEntity -> Array.add entityName ownerEntity.Names
                        | _ -> [|entityName|]
                    let (entity, world) = World.readEntityFromFile filePath (Some entityNames) group world
                    let parentOpt = if owner :? Entity then Some (Relation.makeParent ()) else None
                    let world = World.setEntityParentOpt parentOpt entity world |> snd'
                    let world =
                        match origin with
                        | SimulantOrigin simulant
                        | FacetOrigin (simulant, _) ->
                            World.monitor
                                (fun _ world -> (Cascade, World.destroyEntity entity world))
                                (Events.Unregistering --> simulant.SimulantAddress)
                                entity
                                world
                    (Some entity, world)
            else (None, world)

namespace Debug
open Nu
type Entity =

    /// Provides a full view of all the properties of an entity. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view entity world = World.viewEntityProperties entity world