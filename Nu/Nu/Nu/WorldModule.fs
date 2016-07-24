// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Diagnostics
open System.Reflection
open FSharpx.Collections
open OpenTK
open TiledSharp
open Prime
open Nu

[<AutoOpen>]
module WorldModule =

    // Mutable clipboard that allows its state to persist beyond undo / redo.
    let private RefClipboard = ref<obj option> None

    type World with

        (* Debug *)

        /// Choose a world to be used for debugging. Call this whenever the most recently constructed
        /// world value is to be discarded in favor of the given world value.
        static member choose (world : World) =
#if DEBUG
            Debug.World.Chosen <- world :> obj
#endif
            world

        (* EntityTree *)

        /// Rebuild the entity tree if needed.
        static member internal rebuildEntityTree screen world =
            world.Dispatchers.RebuildEntityTree screen world

        (* EventSystem *)

        /// Get event subscriptions.
        static member getSubscriptions world =
            EventWorld.getSubscriptions<World> world

        /// Get event unsubscriptions.
        static member getUnsubscriptions world =
            EventWorld.getUnsubscriptions<World> world

        /// Add event state to the world.
        static member addEventState key state world =
            EventWorld.addEventState<'a, World> key state world

        /// Remove event state from the world.
        static member removeEventState key world =
            EventWorld.removeEventState<World> key world

        /// Get event state from the world.
        static member getEventState<'a> key world =
            EventWorld.getEventState<'a, World> key world

        /// Get whether events are being traced.
        static member getEventTracing world =
            EventWorld.getEventTracing world

        /// Set whether events are being traced.
        static member setEventTracing tracing world =
            EventWorld.setEventTracing tracing world

        /// Get the state of the event filter.
        static member getEventFilter world =
            EventWorld.getEventFilter world

        /// Set the state of the event filter.
        static member setEventFilter filter world =
            EventWorld.setEventFilter filter world

        /// TODO: document.
        static member getSubscriptionsSorted (publishSorter : SubscriptionSorter<World>) eventAddress world =
            EventWorld.getSubscriptionsSorted publishSorter eventAddress world

        /// TODO: document.
        static member getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<World>) eventAddress world =
            EventWorld.getSubscriptionsSorted3 publishSorter eventAddress world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsBy by subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> Constants.Engine.EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (world : World) =
            EventWorld.sortSubscriptionsNone subscriptions world

        /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
        static member publish7<'a, 'p when 'p :> Simulant> getSubscriptions publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish7<'a, 'p, World> getSubscriptions publishSorter eventData eventAddress eventTrace publisher world

        /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
        static member publish6<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish6<'a, 'p, World> publishSorter eventData eventAddress eventTrace publisher world

        /// Publish an event.
        static member publish<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
            EventWorld.publish6<'a, 'p, World> World.sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
            EventWorld.unsubscribe<World> subscriptionKey world

        /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
        static member subscribePlus5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event, and be provided with an unsubscription callback.
        static member subscribePlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribePlus<'a, 's, World> subscription eventAddress subscriber world

        /// Subscribe to an event using the given subscriptionKey.
        static member subscribe5<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.subscribe<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
        static member monitorPlus<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitorPlus<'a, 's, World> subscription eventAddress subscriber world

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
            EventWorld.monitor<'a, 's, World> subscription eventAddress subscriber world

        (* Dispatchers *)

        /// Get the game dispatchers of the world.
        static member getGameDispatchers world =
            world.Dispatchers.GameDispatchers

        /// Get the screen dispatchers of the world.
        static member getScreenDispatchers world =
            world.Dispatchers.ScreenDispatchers

        /// Get the group dispatchers of the world.
        static member getGroupDispatchers world =
            world.Dispatchers.GroupDispatchers

        /// Get the entity dispatchers of the world.
        static member getEntityDispatchers world =
            world.Dispatchers.EntityDispatchers

        /// Get the facets of the world.
        static member getFacets world =
            world.Dispatchers.Facets

        (* Subsystems *)

        static member internal getSubsystemMap world =
            Subsystems.getSubsystemMap world.Subsystems

        static member internal getSubsystem<'s when 's :> World Subsystem> name world : 's =
            Subsystems.getSubsystem name world.Subsystems

        static member internal getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
            Subsystems.getSubsystemBy by name world.Subsystems

        // NOTE: it'd be nice to get rid of this function to improve encapsulation, but I can't seem to do so in practice...
        static member internal setSubsystem<'s when 's :> World Subsystem> (subsystem : 's) name world =
            World.choose { world with Subsystems = Subsystems.setSubsystem subsystem name world.Subsystems }

        static member internal updateSubsystem<'s when 's :> World Subsystem> (updater : 's -> World -> 's) name world =
            World.choose { world with Subsystems = Subsystems.updateSubsystem updater name world.Subsystems world }

        static member internal updateSubsystems (updater : World Subsystem -> World -> World Subsystem) world =
            World.choose { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

        static member internal clearSubsystemsMessages world =
            World.choose { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

        (* AmbientState *)

        static member internal getAmbientState world =
            world.AmbientState

        static member internal getAmbientStateBy by world =
            by world.AmbientState

        static member internal updateAmbientState updater world =
            World.choose { world with AmbientState = updater world.AmbientState }

        static member internal updateAmbientStateWithoutEvent updater world =
            let _ = world
            let world = World.choose { world with AmbientState = updater world.AmbientState }
            let _ = EventTrace.record "World" "updateAmbientState" EventTrace.empty
            world
    
        /// Get the tick rate.
        static member getTickRate world =
            World.getAmbientStateBy AmbientState.getTickRate world

        /// Get the tick rate as a floating-point value.
        static member getTickRateF world =
            World.getAmbientStateBy AmbientState.getTickRateF world

        /// Set the tick rate without waiting for the end of the current update. Only use
        /// this if you need it and understand the engine internals well enough to know the
        /// consequences.
        static member setTickRateImmediately tickRate world =
            World.updateAmbientState (AmbientState.setTickRateImmediately tickRate) world

        /// Set the tick rate.
        static member setTickRate tickRate world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Command = { Execute = fun world -> World.setTickRateImmediately tickRate world }}) world

        /// Reset the tick time to 0.
        static member resetTickTime world =
            World.updateAmbientState
                (AmbientState.addTasklet
                    { ScheduledTime = World.getTickTime world; Command = { Execute = fun world -> World.updateAmbientState AmbientState.resetTickTime world }}) world

        /// Get the world's tick time.
        static member getTickTime world =
            World.getAmbientStateBy AmbientState.getTickTime world

        /// Query that the world is ticking.
        static member isTicking world =
            World.getAmbientStateBy AmbientState.isTicking world

        static member internal updateTickTime world =
            World.updateAmbientStateWithoutEvent AmbientState.updateTickTime world

        /// Get the world's update count.
        static member getUpdateCount world =
            World.getAmbientStateBy AmbientState.getUpdateCount world

        static member internal incrementUpdateCount world =
            World.updateAmbientStateWithoutEvent AmbientState.incrementUpdateCount world

        /// Get the the liveness state of the engine.
        static member getLiveness world =
            World.getAmbientStateBy AmbientState.getLiveness world

        /// Place the engine into a state such that the app will exit at the end of the current update.
        static member exit world =
            World.updateAmbientState AmbientState.exit world

        static member internal getTasklets world =
            World.getAmbientStateBy AmbientState.getTasklets world

        static member internal clearTasklets world =
            World.updateAmbientStateWithoutEvent AmbientState.clearTasklets world

        static member internal restoreTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.restoreTasklets tasklets) world

        /// Add a tasklet to be executed by the engine at the scheduled time.
        static member addTasklet tasklet world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklet tasklet) world

        /// Add multiple tasklets to be executed by the engine at the scheduled times.
        static member addTasklets tasklets world =
            World.updateAmbientStateWithoutEvent (AmbientState.addTasklets tasklets) world

        /// Get a value from the camera used to view the world.
        static member getCameraBy by world =
            World.getAmbientStateBy (AmbientState.getCameraBy by) world

        /// Get the camera used to view the world.
        static member getCamera world =
            World.getAmbientStateBy AmbientState.getCamera world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            World.updateAmbientState (AmbientState.updateCamera updater) world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            AmbientState.getAssetMetadataMap ^ World.getAmbientState world

        static member internal setAssetMetadataMap assetMetadataMap world =
            World.updateAmbientState (AmbientState.setAssetMetadataMap assetMetadataMap) world

        static member internal getOverlayerBy by world =
            let overlayer = World.getAmbientStateBy AmbientState.getOverlayer world
            by overlayer

        static member internal getOverlayer world =
            World.getOverlayerBy id world

        static member internal setOverlayer overlayer world =
            World.updateAmbientState (AmbientState.setOverlayer overlayer) world

        /// Get intrinsic overlays.
        static member getIntrinsicOverlays world =
            World.getOverlayerBy Overlayer.getIntrinsicOverlays world

        /// Get extrinsic overlays.
        static member getExtrinsicOverlays world =
            World.getOverlayerBy Overlayer.getExtrinsicOverlays world

        static member internal getOverlayRouter world =
            World.getAmbientStateBy AmbientState.getOverlayRouter world

        static member internal getSymbolStoreBy by world =
            World.getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

        static member internal getSymbolStore world =
            World.getAmbientStateBy AmbientState.getSymbolStore world

        static member internal setSymbolStore symbolStore world =
            World.updateAmbientState (AmbientState.setSymbolStore symbolStore) world

        static member internal updateSymbolStore updater world =
            World.updateAmbientState (AmbientState.updateSymbolStore updater) world

        /// Try to load a symbol store package with the given name.
        static member tryLoadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage packageName) world

        /// Unload a symbol store package with the given name.
        static member unloadSymbolStorePackage packageName world =
            World.updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

        /// Try to find a symbol with the given asset tag.
        static member tryFindSymbol assetTag world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbol assetTag symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Try to find symbols with the given asset tags.
        static member tryFindSymbols assetTags world =
            let symbolStore = World.getSymbolStore world
            let (symbol, symbolStore) = SymbolStore.tryFindSymbols assetTags symbolStore
            let world = World.setSymbolStore symbolStore world
            (symbol, world)

        /// Reload all the symbols in the symbol store.
        static member reloadSymbols world =
            World.updateSymbolStore SymbolStore.reloadSymbols world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            World.getAmbientStateBy AmbientState.getUserState world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            World.updateAmbientState (AmbientState.updateUserState updater) world

        (* OptEntityCache *)

        /// Get the opt entity cache.
        static member internal getOptEntityCache world =
            world.OptEntityCache

        /// Get the opt entity cache.
        static member internal setOptEntityCache optEntityCache world =
            World.choose { world with OptEntityCache = optEntityCache }

        (* ScreenDirectory *)

        /// Get the opt entity cache.
        static member internal getScreenDirectory world =
            world.ScreenDirectory

        (* Facet *)

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ^ "Invalid facet name '" + facetName + "'."

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun definition ->
                        match Xtension.tryGetProperty definition.PropertyName entityState.Xtension with
                        | Some property -> property.GetType () <> definition.PropertyType
                        | None -> false)
                    facetPropertyDefinitions
            else false

        static member private getFacetNamesToAdd oldFacetNames newFacetNames =
            Set.difference newFacetNames oldFacetNames

        static member private getFacetNamesToRemove oldFacetNames newFacetNames =
            Set.difference oldFacetNames newFacetNames

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let propertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts propertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name ^ Reflection.getPropertyDefinitions facetType
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyDefinitionNameCounts facetPropertyDefinitions

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

        static member private tryRemoveFacet facetName entityState optEntity world =
            match List.tryFind (fun facet -> getTypeName facet = facetName) entityState.FacetsNp with
            | Some facet ->
                let (entityState, world) =
                    match optEntity with
                    | Some entity ->
                        let world = facet.Unregister (entity, world)
                        let entityState = World.getEntityState entity world
                        (entityState, world)
                    | None -> (entityState, world)
                let facetNames = Set.remove facetName entityState.FacetNames
                let facetsNp = List.remove ((=) facet) entityState.FacetsNp
                let entityState = { entityState with FacetNames = facetNames }
                let entityState = { entityState with FacetsNp = facetsNp }
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.copy propertyNames entityState
                match optEntity with
                | Some entity ->
                    let oldWorld = world
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    let world = World.updateEntityInEntityTree entity oldWorld world
                    let world = World.publishEntityChange entityState entity oldWorld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> let _ = World.choose world in Left ^ "Failure to remove facet '" + facetName + "' from entity."

        static member private tryAddFacet facetName (entityState : EntityState) optEntity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let facetNames = Set.add facetName entityState.FacetNames
                    let facetsNp = facet :: entityState.FacetsNp
                    let entityState = { entityState with FacetNames = facetNames }
                    let entityState = { entityState with FacetsNp = facetsNp }
                    let entityState = Reflection.attachProperties EntityState.copy facet entityState
                    match optEntity with
                    | Some entity ->
                        let oldWorld = world
                        let world = World.setEntityStateWithoutEvent entityState entity world
                        let world = World.updateEntityInEntityTree entity oldWorld world
                        let world = World.publishEntityChange entityState entity oldWorld world
                        let world = facet.Register (entity, world)
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else let _ = World.choose world in Left ^ "Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Name + "'."
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState optEntity world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState optEntity world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState optEntity world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member internal trySetFacetNames facetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove entityState.FacetNames facetNames
            let facetNamesToAdd = World.getFacetNamesToAdd entityState.FacetNames facetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames oldFacetNames entityState optEntity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entityState.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState optEntity world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState optEntity world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.copy entityDispatchers facets entityState.DispatcherNp entityState

        (* SimulantState *)

        /// View the member properties of some SimulantState.
        static member internal viewMemberProperties (state : SimulantState) =
            state |>
            getType |>
            getProperties |>
            Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state))

        /// View the xtension properties of some SimulantState.
        static member internal viewXProperties (state : SimulantState) =
            state.GetXtension () |>
            Xtension.toSeq |>
            Array.ofSeq |>
            Array.sortBy fst |>
            Array.map (fun (name, property) -> (name, property.PropertyValue))

        /// Provides a full view of all the member values of some SimulantState.
        static member internal view state =
            Array.append
                (World.viewMemberProperties state)
                (World.viewXProperties state)

        (* EntityState *)

        static member private optEntityStateKeyEquality 
            (entityAddress : Entity Address, world : World)
            (entityAddress2 : Entity Address, world2 : World) =
            refEq entityAddress entityAddress2 && refEq world world2

        static member private optEntityGetFreshKeyAndValue entity world =
            let optEntityState = Vmap.tryFind entity.EntityAddress ^ world.EntityStates
            ((entity.EntityAddress, world), optEntityState)

        static member private optEntityStateFinder entity world =
            KeyedCache.getValue
                World.optEntityStateKeyEquality
                (fun () -> World.optEntityGetFreshKeyAndValue entity world)
                (entity.EntityAddress, world)
                (World.getOptEntityCache world)

        static member private entityStateSetter entityState entity world =
#if DEBUG
            if not ^ Vmap.containsKey entity.EntityAddress world.EntityStates then
                failwith ^ "Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'"
#endif
            let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private entityStateAdder entityState entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Vmap.add entityName entity.EntityAddress entityDirectory
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent group."
                    | None -> failwith ^ "Cannot add entity '" + scstring entity.EntityAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        static member private entityStateRemover entity world =
            let screenDirectory =
                match Address.getNames entity.EntityAddress with
                | [screenName; groupName; entityName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let entityDirectory = Vmap.remove entityName entityDirectory
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent group."
                    | None -> failwith ^ "Cannot remove entity '" + scstring entity.EntityAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid entity address '" + scstring entity.EntityAddress + "'."
            let entityStates = Vmap.remove entity.EntityAddress world.EntityStates
            World.choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

        /// Query that the world contains an entity.
        static member containsEntity entity world =
            Option.isSome ^ World.getOptEntityState entity world

        static member private getEntityStateBoundsMax entityState =
            // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
            match entityState.Rotation with
            | 0.0f ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                boundsOverflow // no need to transform is unrotated
            | _ ->
                let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
                let position = boundsOverflow.Xy
                let size = Vector2 (boundsOverflow.Z, boundsOverflow.W) - position
                let center = position + size * 0.5f
                let corner = position + size
                let centerToCorner = corner - center
                let quaternion = Quaternion.FromAxisAngle (Vector3.UnitZ, Constants.Math.DegreesToRadiansF * 45.0f)
                let newSizeOver2 = Vector2 (Vector2.Transform (centerToCorner, quaternion)).Y
                let newPosition = center - newSizeOver2
                let newSize = newSizeOver2 * 2.0f
                Vector4 (newPosition.X, newPosition.Y, newPosition.X + newSize.X, newPosition.Y + newSize.Y)

        static member private publishEntityChange entityState (entity : Entity) oldWorld world =
            if entityState.PublishChangesNp then
                let eventTrace = EventTrace.record "World" "publishEntityChange" EventTrace.empty
                World.publish { Participant = entity; OldWorld = oldWorld } entity.ChangeAddress eventTrace entity world
            else world

        static member private getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member private getEntityState entity world =
            match World.getOptEntityState entity world with
            | Some entityState -> entityState
            | None -> failwith ^ "Could not find entity with address '" + scstring entity.EntityAddress + "'."

        static member private addEntityState entityState entity world =
            World.entityStateAdder entityState entity world

        static member private removeEntityState entity world =
            World.entityStateRemover entity world

        static member private setEntityStateWithoutEvent entityState entity world =
            World.entityStateSetter entityState entity world

        static member private setEntityState entityState (entity : Entity) world =
            let oldWorld = world
            let world = World.entityStateSetter entityState entity world
            World.publishEntityChange entityState entity oldWorld world

        static member private updateEntityStateWithoutEvent updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityStateWithoutEvent entityState entity world

        static member private updateEntityState updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

        static member private updateEntityStatePlus updater entity world =
            let oldWorld = world
            let world = World.updateEntityStateWithoutEvent updater entity world
            let world = World.updateEntityInEntityTree entity oldWorld world
            World.publishEntityChange (World.getEntityState entity world) entity oldWorld world

        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntityName entity world = (World.getEntityState entity world).Name
        static member internal getEntityXtension entity world = (World.getEntityState entity world).Xtension // TODO: try to get rid of this
        static member internal getEntityDispatcherNp entity world = (World.getEntityState entity world).DispatcherNp
        static member internal getEntityCreationTimeStampNp entity world = (World.getEntityState entity world).CreationTimeStampNp
        static member internal getEntityOptSpecialization entity world = (World.getEntityState entity world).OptSpecialization
        static member internal getEntityOptOverlayName entity world = (World.getEntityState entity world).OptOverlayName
        static member internal setEntityOptOverlayName value entity world = World.updateEntityState (fun entityState -> { entityState with OptOverlayName = value }) entity world
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal setEntityPosition value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with EntityState.Position = value }) entity world
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal setEntitySize value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Size = value }) entity world
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal setEntityRotation value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Rotation = value }) entity world
        static member internal getEntityDepth entity world = (World.getEntityState entity world).Depth
        static member internal setEntityDepth value entity world = World.updateEntityState (fun entityState -> { entityState with Depth = value }) entity world
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Overflow
        static member internal setEntityOverflow value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Overflow = value }) entity world
        static member internal getEntityViewType entity world = (World.getEntityState entity world).ViewType
        static member internal setEntityViewType value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with ViewType = value }) entity world
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal setEntityVisible value entity world = World.updateEntityState (fun entityState -> { entityState with Visible = value }) entity world
        static member internal getEntityOmnipresent entity world = (World.getEntityState entity world).Omnipresent
        static member internal setEntityOmnipresent value entity world = World.updateEntityStatePlus (fun entityState -> { entityState with Omnipresent = value }) entity world
        static member internal getEntityPublishUpdatesNp entity world = (World.getEntityState entity world).PublishUpdatesNp
        static member internal setEntityPublishUpdatesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishUpdatesNp = value }) entity world
        static member internal getEntityPublishChangesNp entity world = (World.getEntityState entity world).PublishChangesNp
        static member internal setEntityPublishChangesNp value entity world = World.updateEntityState (fun entityState -> { entityState with PublishChangesNp = value }) entity world
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal setEntityPersistent value entity world = World.updateEntityState (fun entityState -> { entityState with Persistent = value }) entity world
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityFacetsNp entity world = (World.getEntityState entity world).FacetsNp
        static member internal getEntityTransform entity world = EntityState.getTransform (World.getEntityState entity world)
        static member internal setEntityTransform value entity world = World.updateEntityStatePlus (EntityState.setTransform value) entity world
        static member internal attachEntityProperty name value entity world = World.setEntityState (EntityState.attachProperty name value ^ World.getEntityState entity world) entity world
        static member internal detachEntityProperty name entity world = World.setEntityState (EntityState.detachProperty name ^ World.getEntityState entity world) entity world

        static member internal getEntityProperty propertyName entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getEntityId entity world :> obj, typeof<Guid>)
            | "Name" -> (World.getEntityName entity world :> obj, typeof<Name>)
            | "Xtension" -> (World.getEntityXtension entity world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getEntityDispatcherNp entity world :> obj, typeof<EntityDispatcher>)
            | "CreationTimeStampNp" -> (World.getEntityCreationTimeStampNp entity world :> obj, typeof<int64>)
            | "OptOverlayName" -> (World.getEntityOptOverlayName entity world :> obj, typeof<string option>)
            | "OptSpecialization" -> (World.getEntityOptSpecialization entity world :> obj, typeof<string option>)
            | "Position" -> (World.getEntityPosition entity world :> obj, typeof<Vector2>)
            | "Size" -> (World.getEntitySize entity world :> obj, typeof<Vector2>)
            | "Rotation" -> (World.getEntityRotation entity world :> obj, typeof<single>)
            | "Depth" -> (World.getEntityDepth entity world :> obj, typeof<single>)
            | "Overflow" -> (World.getEntityOverflow entity world :> obj, typeof<Vector2>)
            | "ViewType" -> (World.getEntityViewType entity world :> obj, typeof<ViewType>)
            | "Visible" -> (World.getEntityVisible entity world :> obj, typeof<bool>)
            | "Omnipresent" -> (World.getEntityOmnipresent entity world :> obj, typeof<bool>)
            | "PublishUpdatesNp" -> (World.getEntityPublishUpdatesNp entity world :> obj, typeof<bool>)
            | "PublishChangesNp" -> (World.getEntityPublishChangesNp entity world :> obj, typeof<bool>)
            | "Persistent" -> (World.getEntityPersistent entity world :> obj, typeof<bool>)
            | "FacetNames" -> (World.getEntityFacetNames entity world :> obj, typeof<string Set>)
            | "FacetsNp" -> (World.getEntityFacetsNp entity world :> obj, typeof<Facet list>)
            | "Transform" -> (World.getEntityTransform entity world :> obj, typeof<Transform>)
            | _ ->
                let property = EntityState.getProperty (World.getEntityState entity world) propertyName
                (property.PropertyValue, property.PropertyType)

        static member internal getEntityPropertyValue propertyName entity world : 'a =
            let property = World.getEntityProperty propertyName entity world
            fst property :?> 'a

        static member internal setEntityPropertyValue propertyName (value : 'a) entity world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change entity id."
            | "Name" -> failwith "Cannot change entity name."
            | "DispatcherNp" -> failwith "Cannot change entity dispatcher."
            | "CreationTimeStampNp" -> failwith "Cannot change entity creation time stamp."
            | "OptSpecialization" -> failwith "Cannot change entity specialization."
            | "Position" -> World.setEntityPosition (value :> obj :?> Vector2) entity world
            | "Size" -> World.setEntitySize (value :> obj :?> Vector2) entity world
            | "Rotation" -> World.setEntityRotation (value :> obj :?> single) entity world
            | "Depth" -> World.setEntityDepth (value :> obj :?> single) entity world
            | "Overflow" -> World.setEntityOverflow (value :> obj :?> Vector2) entity world
            | "ViewType" -> World.setEntityViewType (value :> obj :?> ViewType) entity world
            | "Visible" -> World.setEntityVisible (value :> obj :?> bool) entity world
            | "Omnipresent" -> World.setEntityOmnipresent (value :> obj :?> bool) entity world
            | "PublishUpdatesNp" -> failwith "Cannot change entity publish updates."
            | "PublishChangesNp" -> failwith "Cannot change entity publish updates."
            | "Persistent" -> World.setEntityPersistent (value :> obj :?> bool) entity world
            | "FacetNames" -> failwith "Cannot change entity facet names with a property setter."
            | "FacetsNp" -> failwith "Cannot change entity facets with a property setter."
            | "Transform" -> World.setEntityTransform (value :> obj :?> Transform) entity world
            | _ -> World.setEntityState (EntityState.set (World.getEntityState entity world) propertyName value) entity world

        /// Get the maxima bounds of the entity as determined by size, position, rotation, and overflow.
        static member getEntityBoundsMax entity world =
            let entityState = World.getEntityState entity world
            World.getEntityStateBoundsMax entityState

        /// Get an entity's picking priority.
        static member getEntityPickingPriority (participant : Participant) world =
            match participant with
            | :? Entity as entity ->
                let entityState = World.getEntityState entity world
                let dispatcher = entityState.DispatcherNp
                dispatcher.GetPickingPriority (entity, entityState.Depth, world)
            | _ -> failwithumf ()

        /// Get an entity's facet names via reflection.
        static member getEntityFacetNamesReflectively entityState =
            List.map getTypeName entityState.FacetsNp

        static member private updateEntityPublishChanges entity world =
            let entityChangeEventAddress = entity.ChangeAddress |> atooa
            let publishChanges =
                let subscriptions = Vmap.tryFind entityChangeEventAddress (World.getSubscriptions world)
                match subscriptions with
                | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                | Some (_ :: _) -> true
                | None -> false
            if World.containsEntity entity world
            then World.setEntityPublishChangesNp publishChanges entity world
            else world

        static member private updateEntityPublishUpdates entity world =
            let entityUpdateEventAddress = entity.UpdateAddress |> atooa
            let publishUpdates =
                let subscriptions = Vmap.tryFind entityUpdateEventAddress (World.getSubscriptions world)
                match subscriptions with
                | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                | Some (_ :: _) -> true
                | None -> false
            if World.containsEntity entity world
            then World.setEntityPublishUpdatesNp publishUpdates entity world
            else world

        static member private addEntity mayReplace entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            let isNew = not ^ World.containsEntity entity world
            if isNew || mayReplace then

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // adding entity to world
                let world = World.addEntityState entityState entity world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity world
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.addElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // register entity if needed
                if isNew then
                    let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                    let facets = World.getEntityFacetsNp entity world
                    let world = dispatcher.Register (entity, world)
                    let world = List.fold (fun world (facet : Facet) -> facet.Register (entity, world)) world facets
                    let world = World.updateEntityPublishChanges entity world
                    let world = World.updateEntityPublishUpdates entity world
                    let eventTrace = EventTrace.record "World" "addEntity" EventTrace.empty
                    World.publish () (ftoa<unit> !!"Entity/Add" ->- entity) eventTrace entity world
                else world

            // handle failure
            else failwith ^ "Adding an entity that the world already contains at address '" + scstring entity.EntityAddress + "'."

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world = World.removeEntity entity world

        /// Create an entity and add it to the world.
        static member createEntity dispatcherName optSpecialization optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher = Map.find dispatcherName dispatchers
            
            // compute the default opt overlay name
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = OverlayRouter.findOptOverlayName intrinsicOverlayName overlayRouter

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make optSpecialization optName defaultOptOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let entityState =
                match defaultOptOverlayName with
                | Some defaultOverlayName ->

                    // apply overlay to facets
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy intrinsicOverlayName defaultOverlayName entityState overlayer overlayer

                    // synchronize the entity's facets (and attach their properties)
                    match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> Log.debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState

            // apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something (EG - when it's not the intrinsic overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy intrinsicOverlayName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // add entity's state to world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity false entityState entity world
            (entity, world)

        static member private removeEntity entity world =
            
            // ensure entity exists in the world
            if World.containsEntity entity world then
                
                // publish event and unregister entity
                let eventTrace = EventTrace.record "World" "removeEntity" EventTrace.empty
                let world = World.publish () (ftoa<unit> !!"Entity/Removing" ->- entity) eventTrace entity world
                let dispatcher = World.getEntityDispatcherNp entity world : EntityDispatcher
                let facets = World.getEntityFacetsNp entity world
                let world = dispatcher.Unregister (entity, world)
                let world = List.fold (fun world (facet : Facet) -> facet.Unregister (entity, world)) world facets

                // get old world for entity tree rebuild
                let oldWorld = world
                
                // pulling out screen state
                let screen = entity.EntityAddress |> Address.head |> ntoa<Screen> |> Screen.proxy
                let screenState = World.getScreenState screen world

                // mutate entity tree
                let entityTree =
                    MutantCache.mutateMutant
                        (fun () -> World.rebuildEntityTree screen oldWorld)
                        (fun entityTree ->
                            let entityState = World.getEntityState entity oldWorld
                            let entityMaxBounds = World.getEntityStateBoundsMax entityState
                            QuadTree.removeElement (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds entity entityTree
                            entityTree)
                        screenState.EntityTreeNp
                let screenState = { screenState with EntityTreeNp = entityTree }
                let world = World.setScreenState screenState screen world

                // remove the entity from the world
                World.removeEntityState entity world

            // pass
            else world

        /// Read an entity from an entity descriptor.
        static member readEntity entityDescriptor optName group world =

            // grab overlay dependencies
            let overlayer = World.getOverlayer world
            let overlayRouter = World.getOverlayRouter world

            // create the dispatcher
            let dispatcherName = entityDescriptor.EntityDispatcher
            let dispatchers = World.getEntityDispatchers world
            let (dispatcherName, dispatcher) =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> (dispatcherName, dispatcher)
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<EntityDispatcher>.Name
                    let dispatcher = Map.find dispatcherName dispatchers
                    (dispatcherName, dispatcher)

            // compute the default overlay names
            let intrinsicOverlayName = dispatcherName
            let defaultOptOverlayName = OverlayRouter.findOptOverlayName intrinsicOverlayName overlayRouter

            // make the bare entity state with name as id
            let entityState = EntityState.make None None defaultOptOverlayName dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // read the entity state's overlay and apply it to its facet names if applicable
            let entityState = Reflection.tryReadOptOverlayNameToTarget EntityState.copy entityDescriptor.EntityProperties entityState
            let entityState =
                match (defaultOptOverlayName, entityState.OptOverlayName) with
                | (Some defaultOverlayName, Some overlayName) -> Overlayer.applyOverlayToFacetNames EntityState.copy defaultOverlayName overlayName entityState overlayer overlayer
                | (_, _) -> entityState

            // read the entity state's facet names
            let entityState = Reflection.readFacetNamesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties EntityState.copy dispatcher entityState
            
            // synchronize the entity state's facets (and attach their properties)
            let entityState =
                match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                | Right (entityState, _) -> entityState
                | Left error -> Log.debug error; entityState

            // attempt to apply the entity state's overlay
            let entityState =
                match entityState.OptOverlayName with
                | Some overlayName ->
                    // OPTIMIZATION: applying overlay only when it will change something (EG - when it's not the default overlay)
                    if intrinsicOverlayName <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay EntityState.copy intrinsicOverlayName overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // read the entity state's values
            let entityState = Reflection.readPropertiesToTarget EntityState.copy entityDescriptor.EntityProperties entityState

            // apply the name if one is provided
            let entityState =
                match optName with
                | Some name -> { entityState with Name = name }
                | None -> entityState

            // add entity state to the world
            let entity = group.GroupAddress -<<- ntoa<Entity> entityState.Name |> Entity.proxy
            let world = World.addEntity true entityState entity world
            (entity, world)

        /// Write an entity to an entity descriptor.
        static member writeEntity (entity : Entity) entityDescriptor world =
            let entityState = World.getEntityState entity world
            let entityDispatcherName = getTypeName entityState.DispatcherNp
            let entityDescriptor = { entityDescriptor with EntityDispatcher = entityDispatcherName }
            let shouldWriteProperty = fun propertyName propertyType (propertyValue : obj) ->
                if propertyName = "OptOverlayName" && propertyType = typeof<string option> then
                    let overlayRouter = World.getOverlayRouter world
                    let defaultOptOverlayName = OverlayRouter.findOptOverlayName entityDispatcherName overlayRouter
                    defaultOptOverlayName <> (propertyValue :?> string option)
                else
                    let overlayer = World.getOverlayer world
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    Overlayer.shouldPropertySerialize5 facetNames propertyName propertyType entityState overlayer
            let getEntityProperties = Reflection.writePropertiesFromTarget shouldWriteProperty entityDescriptor.EntityProperties entityState
            { entityDescriptor with EntityProperties = getEntityProperties }

        /// Reassign an entity's identity and / or group. Note that since this destroys the reassigned entity
        /// immediately, you should not call this inside an event handler that involves the reassigned entity itself.
        static member reassignEntity entity optName group world =
            let entityState = World.getEntityState entity world
            let world = World.removeEntity entity world
            let id = makeGuid ()
            let name = match optName with Some name -> name | None -> Name.make ^ scstring id
            let entityState = { entityState with Id = id; Name = name }
            let transmutedEntity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
            let world = World.addEntity false entityState transmutedEntity world
            (transmutedEntity, world)

        static member internal updateEntityPublishingFlags eventAddress world =
            let eventNames = Address.getNames eventAddress
            match eventNames with
            | head :: neck :: tail when Name.getNameStr head = "Entity" && Name.getNameStr neck = "Change" ->
                let publishChanges =
                    match Vmap.tryFind eventAddress (EventWorld.getSubscriptions world) with
                    | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                    | Some (_ :: _) -> true
                    | None -> false
                let entity = Entity.proxy ^ ltoa<Entity> tail
                let world = if World.containsEntity entity world then World.setEntityPublishChangesNp publishChanges entity world else world
                world
            | head :: tail when Name.getNameStr head = "Update" ->
                let publishUpdates =
                    match Vmap.tryFind eventAddress (EventWorld.getSubscriptions world) with
                    | Some [] -> failwithumf () // NOTE: implementation of event system should clean up all empty subscription entries, AFAIK
                    | Some (_ :: _) -> true
                    | None -> false
                let entity = Entity.proxy ^ ltoa<Entity> tail
                let world = if World.containsEntity entity world then World.setEntityPublishUpdatesNp publishUpdates entity world else world
                world
            | _ -> world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOptOverlayName optOverlayName entity world =
            let oldEntityState = World.getEntityState entity world
            let oldOptOverlayName = oldEntityState.OptOverlayName
            let entityState = { oldEntityState with OptOverlayName = optOverlayName }
            match (oldOptOverlayName, optOverlayName) with
            | (Some oldOverlayName, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let (entityState, world) =
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy oldOverlayName overlayName entityState overlayer overlayer
                    match World.trySynchronizeFacetsToNames entityState.FacetNames entityState (Some entity) world with
                    | Right (entityState, world) -> (entityState, world)
                    | Left error -> Log.debug error; (entityState, world)
                let facetNames = World.getEntityFacetNamesReflectively entityState
                let entityState = Overlayer.applyOverlay EntityState.copy oldOverlayName overlayName facetNames entityState overlayer
                let oldWorld = world
                let world = World.setEntityStateWithoutEvent entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChange entityState entity oldWorld world
                Right world
            | (_, _) -> let _ = World.choose world in Left "Could not set the entity's overlay name."

        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            match World.trySetFacetNames facetNames entityState (Some entity) world with
            | Right (entityState, world) ->
                let oldWorld = world
                let world = World.setEntityStateWithoutEvent entityState entity world
                let world = World.updateEntityInEntityTree entity oldWorld world
                let world = World.publishEntityChange entityState entity oldWorld world
                Right world
            | Left error -> Left error

        static member applyEntityOverlay oldOverlayer overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OptOverlayName with
            | Some overlayName ->
                let oldFacetNames = entityState.FacetNames
                let entityState = Overlayer.applyOverlayToFacetNames EntityState.copy overlayName overlayName entityState oldOverlayer overlayer
                match World.trySynchronizeFacetsToNames oldFacetNames entityState (Some entity) world with
                | Right (entityState, world) ->
                    let oldWorld = world
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.copy overlayName overlayName facetNames entityState oldOverlayer overlayer
                    let world = World.setEntityStateWithoutEvent entityState entity world
                    World.updateEntityInEntityTree entity oldWorld world
                | Left error -> Log.info ^ "There was an issue in applying a reloaded overlay: " + error; world
            | None -> world

        static member viewEntityMemberProperties entity world =
            let state = World.getEntityState entity world
            World.viewMemberProperties state

        static member viewEntityXProperties entity world =
            let state = World.getEntityState entity world
            World.viewXProperties state

        static member viewEntity entity world =
            let state = World.getEntityState entity world
            World.view state

        (* GroupState *)

        static member private groupStateSetter groupState group world =
#if DEBUG
            if not ^ Vmap.containsKey group.GroupAddress world.GroupStates then
                failwith ^ "Cannot set the state of a non-existent group '" + scstring group.GroupAddress + "'"
#endif
            let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with GroupStates = groupStates }

        static member private groupStateAdder groupState group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        match Vmap.tryFind groupName groupDirectory with
                        | Some (groupAddress, entityDirectory) ->
                            let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                        | None ->
                            let entityDirectory = Vmap.makeEmpty ()
                            let groupDirectory = Vmap.add groupName (group.GroupAddress, entityDirectory) groupDirectory
                            Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add group '" + scstring group.GroupAddress + "' to non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member private groupStateRemover group world =
            let screenDirectory =
                match Address.getNames group.GroupAddress with
                | [screenName; groupName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (screenAddress, groupDirectory) ->
                        let groupDirectory = Vmap.remove groupName groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove group '" + scstring group.GroupAddress + "' from non-existent screen."
                | _ -> failwith ^ "Invalid group address '" + scstring group.GroupAddress + "'."
            let groupStates = Vmap.remove group.GroupAddress world.GroupStates
            World.choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

        static member private getOptGroupState group world =
            Vmap.tryFind group.GroupAddress world.GroupStates

        static member private getGroupState group world =
            match World.getOptGroupState group world with
            | Some groupState -> groupState
            | None -> failwith ^ "Could not find group with address '" + scstring group.GroupAddress + "'."

        static member private addGroupState groupState group world =
            World.groupStateAdder groupState group world

        static member private removeGroupState group world =
            World.groupStateRemover group world

        static member private setGroupStateWithoutEvent groupState group world =
            World.groupStateSetter groupState group world

        static member private setGroupState groupState group world =
            let _ = world
            let world = World.groupStateSetter groupState group world
            let _ = EventTrace.record "World" "setGroupState" EventTrace.empty
            world

        static member private updateGroupState updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

        /// Query that the world contains a group.
        static member containsGroup group world =
            Option.isSome ^ World.getOptGroupState group world

        static member internal getGroupId group world = (World.getGroupState group world).Id
        static member internal getGroupName group world = (World.getGroupState group world).Name
        static member internal getGroupXtension group world = (World.getGroupState group world).Xtension // TODO: try to get rid of this
        static member internal getGroupDispatcherNp group world = (World.getGroupState group world).DispatcherNp
        static member internal getGroupCreationTimeStampNp group world = (World.getGroupState group world).CreationTimeStampNp
        static member internal getGroupOptSpecialization group world = (World.getGroupState group world).OptSpecialization
        static member internal getGroupPersistent group world = (World.getGroupState group world).Persistent
        static member internal setGroupPersistent value group world = World.updateGroupState (fun groupState -> { groupState with Persistent = value }) group world

        static member internal getGroupProperty propertyName group world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getGroupId group world :> obj, typeof<Guid>)
            | "Name" -> (World.getGroupName group world :> obj, typeof<Name>)
            | "Xtension" -> (World.getGroupXtension group world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getGroupDispatcherNp group world :> obj, typeof<GroupDispatcher>)
            | "CreationTimeStampNp" -> (World.getGroupCreationTimeStampNp group world :> obj, typeof<int64>)
            | "OptSpecialization" -> (World.getGroupOptSpecialization group world :> obj, typeof<string option>)
            | "Persistent" -> (World.getGroupPersistent group world :> obj, typeof<bool>)
            | _ ->
                let property = GroupState.getProperty (World.getGroupState group world) propertyName
                (property.PropertyValue, property.PropertyType)

        static member internal getGroupPropertyValue propertyName group world : 'a =
            let property = World.getGroupProperty propertyName group world
            fst property :?> 'a

        static member internal setGroupPropertyValue propertyName (value : 'a) group world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change group id."
            | "Name" -> failwith "Cannot change group name."
            | "DispatcherNp" -> failwith "Cannot change group dispatcher."
            | "CreationTimeStampNp" -> failwith "Cannot change group creation time stamp."
            | "OptSpecialization" -> failwith "Cannot change group specialization."
            | "Persistent" -> World.setGroupPersistent (value :> obj :?> bool) group world
            | _ -> World.setGroupState (GroupState.set (World.getGroupState group world) propertyName value) group world

        static member private addGroup mayReplace groupState group world =
            let isNew = not ^ World.containsGroup group world
            if isNew || mayReplace then
                let world = World.addGroupState groupState group world
                if isNew then
                    let dispatcher = World.getGroupDispatcherNp group world
                    let world = dispatcher.Register (group, world)
                    let eventTrace = EventTrace.record "World" "addGroup" EventTrace.empty
                    World.publish () (ftoa<unit> !!"Group/Add" ->- group) eventTrace group world
                else world
            else failwith ^ "Adding a group that the world already contains at address '" + scstring group.GroupAddress + "'."

        static member internal removeGroup3 removeEntities group world =
            let eventTrace = EventTrace.record "World" "removeGroup" EventTrace.empty
            let world = World.publish () (ftoa<unit> !!"Group/Removing" ->- group) eventTrace group world
            if World.containsGroup group world then
                let dispatcher = World.getGroupDispatcherNp group world
                let world = dispatcher.Unregister (group, world)
                let world = removeEntities group world
                World.removeGroupState group world
            else world

        /// Create a group and add it to the world.
        static member createGroup dispatcherName optSpecialization optName screen world =
            let dispatchers = World.getGroupDispatchers world
            let dispatcher = Map.find dispatcherName dispatchers
            let groupState = GroupState.make optSpecialization optName dispatcher
            let groupState = Reflection.attachProperties GroupState.copy dispatcher groupState
            let group = screen.ScreenAddress -<<- ntoa<Group> groupState.Name |> Group.proxy
            let world = World.addGroup false groupState group world
            (group, world)

        static member internal writeGroup4 writeEntities group groupDescriptor world =
            let groupState = World.getGroupState group world
            let groupDispatcherName = getTypeName groupState.DispatcherNp
            let groupDescriptor = { groupDescriptor with GroupDispatcher = groupDispatcherName }
            let getGroupProperties = Reflection.writePropertiesFromTarget tautology3 groupDescriptor.GroupProperties groupState
            let groupDescriptor = { groupDescriptor with GroupProperties = getGroupProperties }
            writeEntities group groupDescriptor world

        static member internal readGroup5 readEntities groupDescriptor optName screen world =

            // create the dispatcher
            let dispatcherName = groupDescriptor.GroupDispatcher
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName dispatchers
            
            // make the bare group state with name as id
            let groupState = GroupState.make None None dispatcher

            // attach the group state's instrinsic properties from its dispatcher if any
            let groupState = Reflection.attachProperties GroupState.copy groupState.DispatcherNp groupState

            // read the group state's value
            let groupState = Reflection.readPropertiesToTarget GroupState.copy groupDescriptor.GroupProperties groupState

            // apply the name if one is provided
            let groupState =
                match optName with
                | Some name -> { groupState with Name = name }
                | None -> groupState

            // add the group's state to the world
            let group = screen.ScreenAddress -<<- ntoa<Group> groupState.Name |> Group.proxy
            let world = World.addGroup true groupState group world

            // read the group's entities
            let world = readEntities groupDescriptor group world |> snd
            (group, world)

        static member viewGroupMemberProperties group world =
            let state = World.getGroupState group world
            World.viewMemberProperties state

        static member viewGroupXProperties group world =
            let state = World.getGroupState group world
            World.viewXProperties state

        static member viewGroup group world =
            let state = World.getGroupState group world
            World.view state

        (* ScreenState *)

        static member private screenStateSetter screenState screen world =
#if DEBUG
            if not ^ Vmap.containsKey screen.ScreenAddress world.ScreenStates then
                failwith ^ "Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'"
#endif
            let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenStates = screenStates }

        static member private screenStateAdder screenState screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] ->
                    match Vmap.tryFind screenName world.ScreenDirectory with
                    | Some (_, groupDirectory) ->
                        // NOTE: this is logically a redundant operation...
                        Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                    | None ->
                        let groupDirectory = Vmap.makeEmpty ()
                        Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private screenStateRemover screen world =
            let screenDirectory =
                match Address.getNames screen.ScreenAddress with
                | [screenName] -> Vmap.remove screenName world.ScreenDirectory
                | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            let screenStates = Vmap.remove screen.ScreenAddress world.ScreenStates
            World.choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

        static member private getOptScreenState screen world =
            Vmap.tryFind screen.ScreenAddress world.ScreenStates

        static member private getScreenState screen world =
            match World.getOptScreenState screen world with
            | Some screenState -> screenState
            | None -> failwith ^ "Could not find screen with address '" + scstring screen.ScreenAddress + "'."

        static member private addScreenState screenState screen world =
            World.screenStateAdder screenState screen world

        static member private removeScreenState screen world =
            World.screenStateRemover screen world

        static member private setScreenStateWithoutEvent screenState screen world =
            World.screenStateSetter screenState screen world

        static member private setScreenState screenState screen world =
            let _ = world
            let world = World.screenStateSetter screenState screen world
            let _ = EventTrace.record "World" "setScreenState" EventTrace.empty
            world

        static member private updateScreenState updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        static member containsScreen screen world =
            Option.isSome ^ World.getOptScreenState screen world

        static member internal getScreenId screen world = (World.getScreenState screen world).Id
        static member internal getScreenName screen world = (World.getScreenState screen world).Name
        static member internal getScreenXtension screen world = (World.getScreenState screen world).Xtension // TODO: try to get rid of this
        static member internal getScreenDispatcherNp screen world = (World.getScreenState screen world).DispatcherNp
        static member internal getScreenCreationTimeStampNp screen world = (World.getScreenState screen world).CreationTimeStampNp
        static member internal getScreenOptSpecialization screen world = (World.getScreenState screen world).OptSpecialization
        static member internal getScreenEntityTreeNp screen world = (World.getScreenState screen world).EntityTreeNp
        static member internal setScreenEntityTreeNp value screen world = World.updateScreenState (fun screenState -> { screenState with EntityTreeNp = value }) screen world
        static member internal getScreenTransitionStateNp screen world = (World.getScreenState screen world).TransitionStateNp
        static member internal setScreenTransitionStateNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionStateNp = value }) screen world
        static member internal getScreenTransitionTicksNp screen world = (World.getScreenState screen world).TransitionTicksNp
        static member internal setScreenTransitionTicksNp value screen world = World.updateScreenState (fun screenState -> { screenState with TransitionTicksNp = value }) screen world
        static member internal getScreenIncoming screen world = (World.getScreenState screen world).Incoming
        static member internal setScreenIncoming value screen world = World.updateScreenState (fun screenState -> { screenState with Incoming = value }) screen world
        static member internal getScreenOutgoing screen world = (World.getScreenState screen world).Outgoing
        static member internal setScreenOutgoing value screen world = World.updateScreenState (fun screenState -> { screenState with Outgoing = value }) screen world
        static member internal getScreenPersistent screen world = (World.getScreenState screen world).Persistent
        static member internal setScreenPersistent value screen world = World.updateScreenState (fun screenState -> { screenState with Persistent = value }) screen world
        
        static member internal getScreenProperty propertyName screen world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> (World.getScreenId screen world :> obj, typeof<Guid>)
            | "Name" -> (World.getScreenName screen world :> obj, typeof<Name>)
            | "Xtension" -> (World.getScreenXtension screen world :> obj, typeof<Xtension>)
            | "DispatcherNp" -> (World.getScreenDispatcherNp screen world :> obj, typeof<ScreenDispatcher>)
            | "CreationTimeStampNp" -> (World.getScreenCreationTimeStampNp screen world :> obj, typeof<int64>)
            | "OptSpecialization" -> (World.getScreenOptSpecialization screen world :> obj, typeof<string option>)
            | "EntityTreeNp" -> (World.getScreenEntityTreeNp screen world :> obj, typeof<Entity QuadTree MutantCache>)
            | "TransitionStateNp" -> (World.getScreenTransitionStateNp screen world :> obj, typeof<TransitionState>)
            | "TransitionTicksNp" -> (World.getScreenTransitionTicksNp screen world :> obj, typeof<int64>)
            | "Incoming" -> (World.getScreenIncoming screen world :> obj, typeof<Transition>)
            | "Outgoing" -> (World.getScreenOutgoing screen world :> obj, typeof<Transition>)
            | "Persistent" -> (World.getScreenPersistent screen world :> obj, typeof<bool>)
            | _ ->
                let property = ScreenState.getProperty (World.getScreenState screen world) propertyName
                (property.PropertyValue, property.PropertyType)

        static member internal getScreenPropertyValue propertyName screen world : 'a =
            let property = World.getScreenProperty propertyName screen world
            fst property :?> 'a

        static member internal setScreenPropertyValue propertyName (value : 'a) screen world =
            match propertyName with // NOTE: string match for speed
            | "Id" -> failwith "Cannot change screen id."
            | "Name" -> failwith "Cannot change screen name."
            | "DispatcherNp" -> failwith "Cannot change screen dispatcher."
            | "CreationTimeStampNp" -> failwith "Cannot change screen creation time stamp."
            | "OptSpecialization" -> failwith "Cannot change screen specialization."
            | "EntityTreeNp" -> failwith "Cannot change screen entity tree."
            | "TransitionStateNp" -> World.setScreenTransitionStateNp (value :> obj :?> TransitionState) screen world
            | "TransitionTicksNp" -> World.setScreenTransitionTicksNp (value :> obj :?> int64) screen world
            | "Incoming" -> World.setScreenIncoming (value :> obj :?> Transition) screen world
            | "Outgoing" -> World.setScreenOutgoing (value :> obj :?> Transition) screen world
            | "Persistent" -> World.setScreenPersistent (value :> obj :?> bool) screen world
            | _ -> World.setScreenState (ScreenState.set (World.getScreenState screen world) propertyName value) screen world

        static member internal updateEntityInEntityTree entity oldWorld world =

            // OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease address hashing
            // OPTIMIZATION: assumes a valid entity address with List.head on its names
            let screen =
                match (World.getGameState world).OptSelectedScreen with
                | Some screen when screen.ScreenName = List.head ^ Address.getNames entity.EntityAddress -> screen
                | Some _ | None -> entity.EntityAddress |> Address.getNames |> List.head |> ntoa<Screen> |> Screen.proxy

            // proceed with updating entity in entity tree
            let screenState = World.getScreenState screen world
            let entityTree =
                MutantCache.mutateMutant
                    (fun () -> World.rebuildEntityTree screen oldWorld)
                    (fun entityTree ->
                        let oldEntityState = World.getEntityState entity oldWorld
                        let oldEntityBoundsMax = World.getEntityStateBoundsMax oldEntityState
                        let entityState = World.getEntityState entity world
                        let entityBoundsMax = World.getEntityStateBoundsMax entityState
                        QuadTree.updateElement
                            (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityBoundsMax
                            (entityState.Omnipresent || entityState.ViewType = Absolute) entityBoundsMax
                            entity entityTree
                        entityTree)
                    screenState.EntityTreeNp
            let screenState = { screenState with EntityTreeNp = entityTree }
            World.setScreenStateWithoutEvent screenState screen world

        static member internal addScreen mayReplace screenState screen world =
            let isNew = not ^ World.containsScreen screen world
            if isNew || mayReplace then
                let world = World.addScreenState screenState screen world
                if isNew then
                    let dispatcher = World.getScreenDispatcherNp screen world
                    let world = dispatcher.Register (screen, world)
                    let eventTrace = EventTrace.record "World" "addScreen" EventTrace.empty
                    World.publish () (ftoa<unit> !!"Screen/Add" ->- screen) eventTrace screen world
                else world
            else failwith ^ "Adding a screen that the world already contains at address '" + scstring screen.ScreenAddress + "'."

        static member internal removeScreen3 removeGroups screen world =
            let eventTrace = EventTrace.record "World" "removeScreen" EventTrace.empty
            let world = World.publish () (ftoa<unit> !!"Screen/Removing" ->- screen) eventTrace screen world
            if World.containsScreen screen world then
                let dispatcher = World.getScreenDispatcherNp screen world
                let world = dispatcher.Unregister (screen, world)
                let world = removeGroups screen world
                World.removeScreenState screen world
            else world

        static member internal writeScreen4 writeGroups screen screenDescriptor world =
            let screenState = World.getScreenState screen world
            let screenDispatcherName = getTypeName screenState.DispatcherNp
            let screenDescriptor = { screenDescriptor with ScreenDispatcher = screenDispatcherName }
            let getScreenProperties = Reflection.writePropertiesFromTarget tautology3 screenDescriptor.ScreenProperties screenState
            let screenDescriptor = { screenDescriptor with ScreenProperties = getScreenProperties }
            writeGroups screen screenDescriptor world

        static member internal readScreen4 readGroups screenDescriptor optName world =
            
            // create the dispatcher
            let dispatcherName = screenDescriptor.ScreenDispatcher
            let dispatchers = World.getScreenDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<ScreenDispatcher>.Name
                    Map.find dispatcherName dispatchers
            
            // make the bare screen state with name as id
            let screenState = ScreenState.make None None dispatcher

            // attach the screen state's instrinsic properties from its dispatcher if any
            let screenState = Reflection.attachProperties ScreenState.copy screenState.DispatcherNp screenState

            // read the screen state's value
            let screenState = Reflection.readPropertiesToTarget ScreenState.copy screenDescriptor.ScreenProperties screenState

            // apply the name if one is provided
            let screenState =
                match optName with
                | Some name -> { screenState with Name = name }
                | None -> screenState
            
            // add the screen's state to the world
            let screen = screenState.Name |> ntoa |> Screen.proxy
            let screenState =
                if World.containsScreen screen world
                then { screenState with EntityTreeNp = World.getScreenEntityTreeNp screen world }
                else screenState
            let world = World.addScreen true screenState screen world
            
            // read the screen's groups
            let world = readGroups screenDescriptor screen world |> snd
            (screen, world)

        static member viewScreenMemberProperties entity world =
            let state = World.getScreenState entity world
            World.viewMemberProperties state

        static member viewScreenXProperties entity world =
            let state = World.getScreenState entity world
            World.viewXProperties state

        static member viewScreen entity world =
            let state = World.getScreenState entity world
            World.view state

        (* GameState *)

        static member internal getGameState world =
            world.GameState

        static member internal setGameState gameState world =
            let _ = world
            let world = World.choose { world with GameState = gameState }
            let _ = EventTrace.record "World" "setGameState" EventTrace.empty
            world

        static member internal updateGameState updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            (World.getGameState world).OptScreenTransitionDestination

        /// Set the current destination screen on the precondition that no screen transition is currently underway.
        static member internal setOptScreenTransitionDestination destination world =
            World.updateGameState
                (fun gameState -> { gameState with OptScreenTransitionDestination = destination })
                world
                
        // Make the world.
        static member internal make eventSystem dispatchers subsystems ambientState gameState =
            let world =
                { EventSystem = eventSystem
                  Dispatchers = dispatchers
                  Subsystems = subsystems
                  OptEntityCache = Unchecked.defaultof<KeyedCache<Entity Address * World, EntityState option>>
                  ScreenDirectory = Vmap.makeEmpty ()
                  AmbientState = ambientState
                  GameState = gameState
                  ScreenStates = Vmap.makeEmpty ()
                  GroupStates = Vmap.makeEmpty ()
                  EntityStates = Vmap.makeEmpty () }
            World.choose world

        (* Clipboard *)
        
        /// Copy an entity to the clipboard.
        static member copyToClipboard entity world =
            let entityState = World.getEntityState entity world
            RefClipboard := Some (entityState :> obj)

        /// Cut an entity to the clipboard.
        static member cutToClipboard entity world =
            World.copyToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the clipboard.
        static member pasteFromClipboard atMouse rightClickPosition positionSnap rotationSnap group world =
            match !RefClipboard with
            | Some entityStateObj ->
                let entityState = entityStateObj :?> EntityState
                let id = makeGuid ()
                let name = Name.make ^ scstring id
                let entityState = { entityState with Id = id; Name = name }
                let camera = World.getCamera world
                let position =
                    if atMouse
                    then Camera.mouseToWorld entityState.ViewType rightClickPosition camera
                    else Camera.mouseToWorld entityState.ViewType (camera.EyeSize * 0.5f) camera
                let transform = { EntityState.getTransform entityState with Position = position }
                let transform = Math.snapTransform positionSnap rotationSnap transform
                let entityState = EntityState.setTransform transform entityState
                let entity = group.GroupAddress -<<- ntoa<Entity> name |> Entity.proxy
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)