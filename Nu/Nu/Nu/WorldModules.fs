// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open FSharpx.Collections
open OpenTK
open Prime
open Nu

[<AutoOpen; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimulantOperators =

    /// Convert a name string to a screen's proxy.
    let (!>) screenNameStr = Screen.proxy ^ ntoa ^ Name.make screenNameStr

    /// Convert a name to a screen's proxy.
    let ntos screenName = Screen.proxy ^ ntoa screenName

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    let gtoe (group : Group) entityName = Entity.proxy ^ atoa<Group, Entity> group.GroupAddress ->- ntoa entityName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    let stog (screen : Screen) groupName = Group.proxy ^ atoa<Screen, Group> screen.ScreenAddress ->- ntoa groupName

    /// Convert an entity's proxy to a group's by removing the entity's name from the end.
    let etog (entity : Entity) = !< entity

    /// Convert a group's proxy to a screen's by removing the group's name from the end.
    let gtos group = Screen.proxy ^ Address.take<Group, Screen> 1 group.GroupAddress

module Simulants =

    /// The game. Always exists.
    let Game = { GameAddress = Address.empty }

    /// The default screen - may or may not exist.
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    
    /// The default group - may or may not exist.
    let DefaultGroup = DefaultScreen => Constants.Engine.DefaultGroupName
    
    /// The default entity - may or may not exist.
    let DefaultEntity = DefaultGroup => Constants.Engine.DefaultEntityName

module Descriptors =

    /// Describe a game with the given properties values and contained screens.
    let Game<'d when 'd :> GameDispatcher> properties screens =
        { GameDispatcher = typeof<'d>.Name
          GameProperties = Map.ofSeq properties
          Screens = List.ofSeq screens }

    /// Describe a screen with the given properties values and contained groups.
    let Screen<'d when 'd :> ScreenDispatcher> properties groups =
        { ScreenDispatcher = typeof<'d>.Name
          ScreenProperties = Map.ofSeq properties
          Groups = List.ofSeq groups }

    /// Describe a group with the given properties values and contained entities.
    let Group<'d when 'd :> GroupDispatcher> properties entities =
        { GroupDispatcher = typeof<'d>.Name
          GroupProperties = Map.ofSeq properties
          Entities = List.ofSeq entities }

    /// Describe an entity with the given properties values.
    let Entity<'d when 'd :> EntityDispatcher> properties =
        { EntityDispatcher = typeof<'d>.Name
          EntityProperties = Map.ofSeq properties }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module World =

    (* Debug *)

    /// Choose a world to be used for debugging. Call this whenever the most recently constructed
    /// world value is to be discarded in favor of the given world value.
    let choose (world : World) =
#if DEBUG
        Debug.World.Chosen <- world :> obj
#endif
        world

    (* F# reach-arounds... *)

    let mutable rebuildEntityTree =
        Unchecked.defaultof<Screen -> World -> Entity QuadTree>

    (* Subsystems *)

    let internal getSubsystemMap world =
        Subsystems.getSubsystemMap world.Subsystems

    let internal getSubsystem<'s when 's :> World Subsystem> name world : 's =
        Subsystems.getSubsystem name world.Subsystems

    let internal getSubsystemBy<'s, 't when 's :> World Subsystem> (by : 's -> 't) name world : 't =
        Subsystems.getSubsystemBy by name world.Subsystems

    let internal setSubsystem<'s when 's :> World Subsystem> (subsystem : 's) name world =
        choose { world with Subsystems = Subsystems.setSubsystem subsystem name world.Subsystems }

    let internal updateSubsystem<'s when 's :> World Subsystem> (updater : 's -> World -> 's) name world =
        choose { world with Subsystems = Subsystems.updateSubsystem updater name world.Subsystems world }

    let internal updateSubsystems (updater : World Subsystem -> World -> World Subsystem) world =
        choose { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

    let internal clearSubsystemsMessages world =
        choose { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

    (* Dispatchers *)

    /// Get the game dispatchers of the world.
    let getGameDispatchers world =
        world.Dispatchers.GameDispatchers

    /// Get the screen dispatchers of the world.
    let getScreenDispatchers world =
        world.Dispatchers.ScreenDispatchers

    /// Get the group dispatchers of the world.
    let getGroupDispatchers world =
        world.Dispatchers.GroupDispatchers

    /// Get the entity dispatchers of the world.
    let getEntityDispatchers world =
        world.Dispatchers.EntityDispatchers

    /// Get the facets of the world.
    let getFacets world =
        world.Dispatchers.Facets

    (* EventSystem *)

    /// Get event subscriptions.
    let getSubscriptions world =
        Eventable.getSubscriptions<World> world

    /// Get event unsubscriptions.
    let getUnsubscriptions world =
        Eventable.getUnsubscriptions<World> world

    /// Add event state to the world.
    let addEventState key state world =
        Eventable.addEventState<'a, World> key state world

    /// Remove event state from the world.
    let removeEventState key world =
        Eventable.removeEventState<World> key world

    /// Get event state from the world.
    let getEventState<'a> key world =
        Eventable.getEventState<'a, World> key world

    /// Get whether events are being traced.
    let getEventTracing world =
        Eventable.getEventTracing world

    /// Set whether events are being traced.
    let setEventTracing tracing world =
        Eventable.setEventTracing tracing world

    /// Get the state of the event filter.
    let getEventFilter world =
        Eventable.getEventFilter world

    /// Set the state of the event filter.
    let setEventFilter filter world =
        Eventable.setEventFilter filter world

    /// TODO: document.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter<World>) eventAddress world =
        Eventable.getSubscriptionsSorted publishSorter eventAddress world

    /// TODO: document.
    let getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<World>) eventAddress world =
        Eventable.getSubscriptionsSorted3 publishSorter eventAddress world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : World) =
        Eventable.sortSubscriptionsBy by subscriptions world

    /// Sort subscriptions by their place in the world's simulant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> Constants.Engine.EntityPublishingPriority)
            subscriptions
            world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (world : World) =
        Eventable.sortSubscriptionsNone subscriptions world

    /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
    let publish7<'a, 'p when 'p :> Simulant> getSubscriptions publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish7<'a, 'p, World> getSubscriptions publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish6<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish6<'a, 'p, World> publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event.
    let publish<'a, 'p when 'p :> Simulant>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish6<'a, 'p, World> sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
        Eventable.unsubscribe<World> subscriptionKey world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribePlus5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribePlus<'a, 's, World> subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribe5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribe<'a, 's, World> subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.monitorPlus<'a, 's, World> subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant.
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.monitor<'a, 's, World> subscription eventAddress subscriber world

    (* AmbientState *)

    let private getAmbientState world =
        world.AmbientState

    let private getAmbientStateBy by world =
        by world.AmbientState

    let private setAmbientStateWithoutEvent state world =
        choose { world with AmbientState = state }

    let private setAmbientState state world =
        let oldWorldWithOldState = world
        let world = setAmbientStateWithoutEvent state world
        let eventTrace = EventTrace.record "World" "setAmbientState" EventTrace.empty
        publish { OldWorldWithOldState = oldWorldWithOldState } Events.AmbientStateChange eventTrace Simulants.Game world

    let private updateAmbientState updater world =
        setAmbientState (updater world.AmbientState) world

    let private updateAmbientStateWithoutEvent updater world =
        setAmbientStateWithoutEvent (updater world.AmbientState) world

    /// Get the tick rate.
    let getTickRate world =
        getAmbientStateBy AmbientState.getTickRate world

    /// Get the tick rate as a floating-point value.
    let getTickRateF world =
        getAmbientStateBy AmbientState.getTickRateF world

    /// Set the tick rate without waiting for the end of the current update. Only use
    /// this if you need it and understand the engine internals well enough to know the
    /// consequences.
    let setTickRateImmediately tickRate world =
        updateAmbientState (AmbientState.setTickRateImmediately tickRate) world

    /// Set the tick rate.
    let rec setTickRate tickRate world =
        updateAmbientState
            (AmbientState.addTasklet
                { ScheduledTime = getTickTime world; Operation = fun world -> setTickRateImmediately tickRate world }) world

    /// Reset the tick time to 0.
    and resetTickTime world =
        updateAmbientState
            (AmbientState.addTasklet
                { ScheduledTime = getTickTime world; Operation = fun world -> setAmbientState (AmbientState.resetTickTime world.AmbientState) world }) world

    /// Get the world's tick time.
    and getTickTime world =
        getAmbientStateBy AmbientState.getTickTime world

    /// Query that the world is ticking.
    let isTicking world =
        getAmbientStateBy AmbientState.isTicking world

    let internal updateTickTime world =
        updateAmbientStateWithoutEvent AmbientState.updateTickTime world

    /// Get the world's update count.
    let getUpdateCount world =
        getAmbientStateBy AmbientState.getUpdateCount world

    let internal incrementUpdateCount world =
        updateAmbientStateWithoutEvent AmbientState.incrementUpdateCount world

    /// Get the the liveness state of the engine.
    let getLiveness world =
        getAmbientStateBy AmbientState.getLiveness world

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit world =
        updateAmbientState AmbientState.exit world

    let internal getTasklets world =
        getAmbientStateBy AmbientState.getTasklets world

    let internal clearTasklets world =
        updateAmbientStateWithoutEvent AmbientState.clearTasklets world

    let internal restoreTasklets tasklets world =
        updateAmbientStateWithoutEvent (AmbientState.restoreTasklets tasklets) world

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet world =
        updateAmbientStateWithoutEvent (AmbientState.addTasklet tasklet) world

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets world =
        updateAmbientStateWithoutEvent (AmbientState.addTasklets tasklets) world

    /// Get a value from the camera used to view the world.
    let getCameraBy by world =
        getAmbientStateBy (AmbientState.getCameraBy by) world

    /// Get the camera used to view the world.
    let getCamera world =
        getAmbientStateBy AmbientState.getCamera world

    /// Update the camera used to view the world.
    let updateCamera updater world =
        updateAmbientState (AmbientState.updateCamera updater) world

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        AmbientState.getAssetMetadataMap world.AmbientState

    let internal setAssetMetadataMap assetMetadataMap world =
        updateAmbientState (AmbientState.setAssetMetadataMap assetMetadataMap) world

    let internal getOverlayerBy by world =
        let overlayer = getAmbientStateBy AmbientState.getOverlayer world
        by overlayer

    let internal getOverlayer world =
        getOverlayerBy id world

    let internal setOverlayer overlayer world =
        updateAmbientState (AmbientState.setOverlayer overlayer) world

    /// Get intrinsic overlays.
    let getIntrinsicOverlays world =
        getOverlayerBy Overlayer.getIntrinsicOverlays world

    /// Get extrinsic overlays.
    let getExtrinsicOverlays world =
        getOverlayerBy Overlayer.getExtrinsicOverlays world

    let internal getOverlayRouter world =
        getAmbientStateBy AmbientState.getOverlayRouter world

    let internal getSymbolStoreBy by world =
        getAmbientStateBy (AmbientState.getSymbolStoreBy by) world

    let internal getSymbolStore world =
        getAmbientStateBy AmbientState.getSymbolStore world

    let internal setSymbolStore symbolStore world =
        updateAmbientState (AmbientState.setSymbolStore symbolStore) world

    let internal updateSymbolStore updater world =
        updateAmbientState (AmbientState.updateSymbolStore updater) world

    /// Try to load a symbol store package with the given name.
    let tryLoadSymbolStorePackage packageName world =
        updateSymbolStore (SymbolStore.tryLoadSymbolStorePackage packageName) world

    /// Unload a symbol store package with the given name.
    let unloadSymbolStorePackage packageName world =
        updateSymbolStore (SymbolStore.unloadSymbolStorePackage packageName) world

    /// Try to find a symbol with the given asset tag.
    let tryFindSymbol assetTag world =
        let symbolStore = getSymbolStore world
        let (symbol, symbolStore) = SymbolStore.tryFindSymbol assetTag symbolStore
        let world = setSymbolStore symbolStore world
        (symbol, world)

    /// Try to find symbols with the given asset tags.
    let tryFindSymbols assetTags world =
        let symbolStore = getSymbolStore world
        let (symbol, symbolStore) = SymbolStore.tryFindSymbols assetTags symbolStore
        let world = setSymbolStore symbolStore world
        (symbol, world)

    /// Reload all the symbols in the symbol store.
    let reloadSymbols world =
        updateSymbolStore SymbolStore.reloadSymbols world

    /// Get the user state of the world, casted to 'u.
    let getUserState world : 'u =
        getAmbientStateBy AmbientState.getUserState world

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) world =
        updateAmbientState (AmbientState.updateUserState updater) world

    (* Built-in Event Handlers *)

    /// Ignore all handled events.
    let handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Cascade, world)

    /// Swallow all handled events.
    let handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, world)
        
    /// Handle event by exiting app.
    let handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, exit world)

    (* OptEntityCache *)

    /// Get the opt entity cache.
    let internal getOptEntityCache world =
        world.OptEntityCache

    (* EntityState *)

    let private optEntityStateKeyEquality 
        (entityAddress : Entity Address, world : World)
        (entityAddress2 : Entity Address, world2 : World) =
        refEq entityAddress entityAddress2 && refEq world world2

    let private optEntityGetFreshKeyAndValue entity world =
        let optEntityState = Vmap.tryFind entity.EntityAddress world.EntityStates
        ((entity.EntityAddress, world), optEntityState)

    let private optEntityStateFinder entity world =
        KeyedCache.getValue
            optEntityStateKeyEquality
            (fun () -> optEntityGetFreshKeyAndValue entity world)
            (entity.EntityAddress, world)
            (getOptEntityCache world)

    let private entityStateSetter entityState entity world =
#if DEBUG
        if not ^ Vmap.containsKey entity.EntityAddress world.EntityStates then
            failwith ^ "Cannot set the state of a non-existent entity '" + scstring entity.EntityAddress + "'"
#endif
        let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
        choose { world with EntityStates = entityStates }

    let private entityStateAdder entityState entity world =
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
        choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

    let private entityStateRemover entity world =
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
        choose { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

    let internal getEntityStateBoundsMax entityState =
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

    let internal publishEntityChange entityState (entity : Entity) oldWorld world =
        if entityState.PublishChangesNp then
            publish
                { Participant = entity; OldWorld = oldWorld }
                entity.ChangeAddress
                (EventTrace.record "World" "publishEntityChange" EventTrace.empty)
                entity
                world
        else world

    let inline internal getOptEntityState entity world =
        optEntityStateFinder entity world

    let internal getEntityState entity world =
        match getOptEntityState entity world with
        | Some entityState -> entityState
        | None -> failwith ^ "Could not find entity with address '" + scstring entity.EntityAddress + "'."

    let internal addEntityState entityState entity world =
        entityStateAdder entityState entity world

    let internal removeEntityState entity world =
        entityStateRemover entity world

    let inline internal setEntityStateWithoutEvent entityState entity world =
        entityStateSetter entityState entity world

    let internal setEntityState entityState (entity : Entity) world =
        let oldWorld = world
        let world = entityStateSetter entityState entity world
        publishEntityChange entityState entity oldWorld world

    let internal updateEntityStateWithoutEvent updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityStateWithoutEvent entityState entity world

    let internal updateEntityState updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityState entityState entity world

    let getEntityBoundsMax entity world =
        let entityState = getEntityState entity world
        getEntityStateBoundsMax entityState

    (* GroupState *)

    let private groupStateSetter groupState group world =
#if DEBUG
        if not ^ Vmap.containsKey group.GroupAddress world.GroupStates then
            failwith ^ "Cannot set the state of a non-existent group '" + scstring group.GroupAddress + "'"
#endif
        let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
        choose { world with GroupStates = groupStates }

    let private groupStateAdder groupState group world =
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
        choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let private groupStateRemover group world =
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
        choose { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let inline internal getOptGroupState group world =
        Vmap.tryFind group.GroupAddress world.GroupStates

    let internal getGroupState group world =
        match getOptGroupState group world with
        | Some groupState -> groupState
        | None -> failwith ^ "Could not find group with address '" + scstring group.GroupAddress + "'."

    let internal addGroupState groupState group world =
        groupStateAdder groupState group world

    let internal removeGroupState group world =
        groupStateRemover group world

    let inline internal setGroupStateWithoutEvent groupState group world =
        groupStateSetter groupState group world

    let internal setGroupState groupState group world =
        let oldWorld = world
        let world = groupStateSetter groupState group world
        publish
            { Participant = group; OldWorld = oldWorld }
            (Events.GroupChange ->- group)
            (EventTrace.record "World" "setGroupState" EventTrace.empty)
            group
            world

    let internal updateGroupState updater group world =
        let groupState = getGroupState group world
        let groupState = updater groupState
        setGroupState groupState group world

    (* ScreenState *)

    let private screenStateSetter screenState screen world =
#if DEBUG
        if not ^ Vmap.containsKey screen.ScreenAddress world.ScreenStates then
            failwith ^ "Cannot set the state of a non-existent screen '" + scstring screen.ScreenAddress + "'"
#endif
        let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
        choose { world with ScreenStates = screenStates }

    let private screenStateAdder screenState screen world =
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
        choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let private screenStateRemover screen world =
        let screenDirectory =
            match Address.getNames screen.ScreenAddress with
            | [screenName] -> Vmap.remove screenName world.ScreenDirectory
            | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
        let screenStates = Vmap.remove screen.ScreenAddress world.ScreenStates
        choose { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let inline internal getOptScreenState screen world =
        Vmap.tryFind screen.ScreenAddress world.ScreenStates

    let internal getScreenState screen world =
        match getOptScreenState screen world with
        | Some screenState -> screenState
        | None -> failwith ^ "Could not find screen with address '" + scstring screen.ScreenAddress + "'."

    let internal addScreenState screenState screen world =
        screenStateAdder screenState screen world

    let internal removeScreenState screen world =
        screenStateRemover screen world

    let inline internal setScreenStateWithoutEvent screenState screen world =
        screenStateSetter screenState screen world

    let internal setScreenState screenState screen world =
        let oldWorld = world
        let world = screenStateSetter screenState screen world
        publish
            { Participant = screen; OldWorld = oldWorld }
            (Events.ScreenChange ->- screen)
            (EventTrace.record "World" "setScreenState" EventTrace.empty)
            screen
            world

    let internal updateScreenState updater screen world =
        let screenState = getScreenState screen world
        let screenState = updater screenState
        setScreenState screenState screen world

    let internal updateEntityInEntityTree entity oldWorld world =

        // OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease address hashing
        // OPTIMIZATION: assumes a valid entity address with List.head on its names
        let screen =
            match world.GameState.OptSelectedScreen with
            | Some screen when screen.ScreenName = List.head ^ Address.getNames entity.EntityAddress -> screen
            | Some _ | None -> entity |> etog |> gtos

        // proceed with updating entity in entity tree
        let screenState = getScreenState screen world
        let entityTree =
            MutantCache.mutateMutant
                (fun () -> rebuildEntityTree screen oldWorld)
                (fun entityTree ->
                    let oldEntityState = getEntityState entity oldWorld
                    let oldEntityBoundsMax = getEntityStateBoundsMax oldEntityState
                    let entityState = getEntityState entity world
                    let entityBoundsMax = getEntityStateBoundsMax entityState
                    QuadTree.updateElement
                        (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityBoundsMax
                        (entityState.Omnipresent || entityState.ViewType = Absolute) entityBoundsMax
                        entity entityTree
                    entityTree)
                screenState.EntityTreeNp
        let screenState = { screenState with EntityTreeNp = entityTree }
        setScreenStateWithoutEvent screenState screen world

    let internal updateEntityStatePlus updater entity world =
        let oldWorld = world
        let world = updateEntityStateWithoutEvent updater entity world
        let world = updateEntityInEntityTree entity oldWorld world
        publishEntityChange (getEntityState entity world) entity oldWorld world

    (* GameState *)

    let internal getGameState world =
        world.GameState

    let internal setGameState gameState world =
        let oldWorld = world
        let world = choose { world with GameState = gameState }
        publish
            { Participant = Simulants.Game; OldWorld = oldWorld }
            (Events.GameChange ->- Simulants.Game)
            (EventTrace.record "World" "setGameState" EventTrace.empty)
            Simulants.Game
            world

    let internal updateGameState updater world =
        let gameState = getGameState world
        let gameState = updater gameState
        setGameState gameState world

    /// Get the current destination screen if a screen transition is currently underway.
    let getOptScreenTransitionDestination world =
        (getGameState world).OptScreenTransitionDestination

    /// Set the current destination screen on the precondition that no screen transition is currently underway.
    let internal setOptScreenTransitionDestination destination world =
        updateGameState
            (fun gameState -> { gameState with OptScreenTransitionDestination = destination })
            world
