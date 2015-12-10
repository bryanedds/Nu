// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx.Collections
open OpenTK
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module World =

    (* F# reach-arounds... *)

    let mutable rebuildEntityTree =
        Unchecked.defaultof<Screen -> World -> Entity QuadTree>

    (* Subsystems *)

    let internal getSubsystem<'s when 's :> Subsystem> name world =
        Subsystems.getSubsystem<'s> name world.Subsystems

    let internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
        Subsystems.getSubsystemBy<'s, 't> by name world.Subsystems

    let internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
        { world with Subsystems = Subsystems.setSubsystem<'s> subsystem name world.Subsystems }

    let internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
        { world with Subsystems = Subsystems.updateSubsystem<'s, World> updater name world.Subsystems world }

    let internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
        { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

    let internal clearSubsystemsMessages world =
        { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

    (* Callbacks *)

    let internal clearTasklets world =
        { world with Callbacks = Callbacks.clearTasklets world.Callbacks }

    let internal restoreTasklets (tasklets : Tasklet Queue) world =
        { world with Callbacks = Callbacks.restoreTasklets tasklets world.Callbacks }

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet world =
        { world with Callbacks = Callbacks.addTasklet tasklet world.Callbacks }

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets world =
        { world with Callbacks = Callbacks.addTasklets tasklets world.Callbacks }

    /// Add callback state to the world.
    let addCallbackState key state world =
        { world with Callbacks = Callbacks.addCallbackState key state world.Callbacks }

    /// Remove callback state from the world.
    let removeCallbackState key world =
        { world with Callbacks = Callbacks.removeCallbackState key world.Callbacks }

    /// Get callback state from the world.
    let getCallbackState<'a> key world =
        Callbacks.getCallbackState<'a> key world.Callbacks

    (* Components *)

    /// Get the facet of the world.
    let getFacets world =
        Components.getFacets world.Components

    /// Get the entity dispatchers of the world.
    let getEntityDispatchers world =
        Components.getEntityDispatchers world.Components

    /// Get the group dispatchers of the world.
    let getGroupDispatchers world =
        Components.getGroupDispatchers world.Components

    /// Get the screen dispatchers of the world.
    let getScreenDispatchers world =
        Components.getScreenDispatchers world.Components

    /// Get the game dispatchers of the world.
    let getGameDispatchers world =
        Components.getGameDispatchers world.Components

    (* Publishing *)

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> (HashIdentity.FromFunctions Address<obj>.hash Address<obj>.equals)

    let internal sortFstDesc (priority : single, _) (priority2 : single, _) =
        // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0

    let private getAnyEventAddresses eventAddress =
        // OPTIMIZATION: uses memoization.
        if not ^ Address.isEmpty eventAddress then
            let anyEventAddressesKey = Address.allButLast eventAddress
            match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
            | (true, anyEventAddresses) -> anyEventAddresses
            | (false, _) ->
                let eventAddressNames = Address.getNames eventAddress
                let anyEventAddressNames = Address.getNames Events.Any
                let anyEventAddresses =
                    [for i in 0 .. List.length eventAddressNames - 1 do
                        let subNames = List.take i eventAddressNames @ anyEventAddressNames
                        yield ltoa subNames]
                AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                anyEventAddresses
        else failwith "Event name cannot be empty."

    let private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry rQueue) world : (single * SubscriptionEntry) list =
        List.foldBack
            (fun (key, simulant : Simulant, subscription) subscriptions ->
                let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                let subscription = (priority, (key, simulant, subscription))
                subscription :: subscriptions)
            subscriptions
            []
            
    /// TODO: document.
    let getSubscriptionsSpecific (publishSorter : SubscriptionSorter) eventAddress world =
        match Vmap.tryFind eventAddress world.Callbacks.Subscriptions with
        | Some subList -> publishSorter subList world
        | None -> []

    /// TODO: document.
    let getSubscriptions (publishSorter : SubscriptionSorter) eventAddress world =
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Vmap.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
        let optSubLists = Vmap.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        publishSorter subList world

    let private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
        let boxableSubscription = fun (evt : obj) world ->
            try subscription (evt :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
        (subscriber : Simulant) (publisher : 'p) (eventData : 'a) (eventAddress : 'a Address) eventTrace subscription world =
        let evt =
            { Data = eventData
              Address = eventAddress
              Trace = eventTrace
              Subscriber = subscriber :?> 's
              Publisher = publisher :> Simulant }
        let callableSubscription = unbox<BoxableSubscription> subscription
        let result = callableSubscription evt world
        Some result

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () = Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () = Guid.NewGuid ()

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    /// Sort subscriptions by their place in the world's simulant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> Constants.Engine.EntityPublishingPriority)
            subscriptions
            world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
        subscriptions

    /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
    let publish6<'a, 'p when 'p :> Simulant> getSubscriptions publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptions publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world) (_, subscriber : Simulant, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.State.Liveness with Running -> true | Exiting -> false) then
                        match Address.getNames subscriber.SimulantAddress with
                        | [] -> publishEvent<'a, 'p, Game> subscriber publisher eventData eventAddress eventTrace subscription world
                        | [_] -> publishEvent<'a, 'p, Screen> subscriber publisher eventData eventAddress eventTrace subscription world
                        | [_; _] -> publishEvent<'a, 'p, Group> subscriber publisher eventData eventAddress eventTrace subscription world
                        | [_; _; _] -> publishEvent<'a, 'p, Entity> subscriber publisher eventData eventAddress eventTrace subscription world
                        | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish5<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        publish6 getSubscriptions publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event.
    let publish<'a, 'p when 'p :> Simulant>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        publish5 sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
        match Vmap.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
        | Some (eventAddress, subscriber) ->
            match Vmap.tryFind eventAddress world.Callbacks.Subscriptions with
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriber', _) ->
                            subscriptionKey' = subscriptionKey &&
                            subscriber' = subscriber)
                        subscriptionList
                let subscriptions = 
                    match subscriptionList with
                    | [] -> Vmap.remove eventAddress world.Callbacks.Subscriptions
                    | _ -> Vmap.add eventAddress subscriptionList world.Callbacks.Subscriptions
                let unsubscriptions = Vmap.remove subscriptionKey world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            | None -> world // TODO: consider an assert fail here?
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Simulant, boxSubscription subscription)
                match Vmap.tryFind objEventAddress world.Callbacks.Subscriptions with
                | Some subscriptionEntries -> Vmap.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                | None -> Vmap.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
            let unsubscriptions = Vmap.add subscriptionKey (objEventAddress, subscriber :> Simulant) world.Callbacks.Unsubscriptions
            let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
            let world = { world with Callbacks = callbacks }
            (unsubscribe subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus5 (makeSubscriptionKey ()) subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus5 subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world |> snd

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribe5 (makeSubscriptionKey ()) subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        if not ^ Address.isEmpty subscriber.SimulantAddress then
            let monitorKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe5<'a, 's> monitorKey subscription eventAddress subscriber world
            let unsubscribe = fun world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ world -> (Cascade, unsubscribe world)
            let removingEventAddress = ftoa<unit> !!(typeof<'s>.Name + "/Removing") ->>- subscriber.SimulantAddress
            let world = subscribe5<unit, 's> removalKey subscription' removingEventAddress subscriber world
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the lifetime of a simulant.
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        monitorPlus subscription eventAddress subscriber world |> snd

    (* WorldState *)

    let private getState world =
        world.State

    let private setStateWithoutEvent state world =
        { world with State = state }

    let private setState state world =
        let oldWorld = world
        let world = setStateWithoutEvent state world
        publish { WorldStateChangeData.OldWorld = oldWorld } Events.WorldStateChange ["World.setState"] Simulants.Game world

    /// Get the tick rate.
    let getTickRate world =
        WorldState.getTickRate world.State

    /// Get the tick rate as a floating-point value.
    let getTickRateF world =
        WorldState.getTickRateF world.State

    /// Set the tick rate without waiting for the end of the current update. Only use
    /// this if you need it and understand the engine internals well enough to know the
    /// consequences.
    let setTickRateImmediately tickRate world =
        setState (WorldState.setTickRateImmediately tickRate world.State) world

    /// Set the tick rate.
    let rec setTickRate tickRate world =
        addTasklet { ScheduledTime = getTickTime world; Operation = fun world -> setTickRateImmediately tickRate world } world

    /// Reset the tick time to 0.
    and resetTickTime world =
        addTasklet { ScheduledTime = getTickTime world; Operation = fun world -> setState (WorldState.resetTickTime world.State) world } world

    /// Get the world's tick time.
    and getTickTime world =
        WorldState.getTickTime world.State

    /// Query that the world is ticking.
    let isTicking world =
        WorldState.isTicking world.State

    let internal updateTickTime world =
        setStateWithoutEvent (WorldState.updateTickTime world.State) world

    /// Get the world's update count.
    let getUpdateCount world =
        WorldState.getUpdateCount world.State

    let internal incrementUpdateCount world =
        setStateWithoutEvent (WorldState.incrementUpdateCount world.State) world

    /// Get the the liveness state of the engine.
    let getLiveness world =
        WorldState.getLiveness world.State

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit world =
        setState (WorldState.exit world.State) world

    /// Get a value from the camera used to view the world.
    let getCameraBy by world =
        WorldState.getCameraBy by world.State

    /// Get the camera used to view the world.
    let getCamera world =
        WorldState.getCamera world.State

    /// Set the camera used to view the world.
    let setCamera camera world =
        setState (WorldState.setCamera camera world.State) world

    /// Update the camera used to view the world.
    let updateCamera updater world =
        setState (WorldState.updateCamera updater world.State) world

    /// Get the current destination screen if a screen transition is currently underway.
    let getOptScreenTransitionDestination world =
        world.State.OptScreenTransitionDestination

    let internal setOptScreenTransitionDestination destination world =
        setState (WorldState.setOptScreenTransitionDestination destination world.State) world

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        world.State.AssetMetadataMap

    let internal setAssetMetadataMap assetMetadataMap world =
        setState (WorldState.setAssetMetadataMap assetMetadataMap world.State) world

    let internal setOverlayer overlayer world =
        setState (WorldState.setOverlayer overlayer world.State) world

    /// Get the user state of the world, casted to 'u.
    let getUserState world : 'u =
        WorldState.getUserState world.State

    /// Set the user state of the world.
    let setUserState (userState : 'u) world =
        setState (WorldState.setUserState userState world.State) world

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) world =
        setState (WorldState.updateUserState updater world.State) world

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
            world.State.OptEntityCache

    let private entityStateSetter entityState entity world =
#if DEBUG
        if not ^ Vmap.containsKey entity.EntityAddress world.EntityStates then
            failwith ^ "Cannot set the state of a non-existent entity '" + acstring entity.EntityAddress + "'"
#endif
        let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
        { world with EntityStates = entityStates }

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
                    | None -> failwith ^ "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith ^ "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith ^ "Invalid entity address '" + acstring entity.EntityAddress + "'."
        let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
        { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

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
                    | None -> failwith ^ "Cannot remove entity '" + acstring entity.EntityAddress + "' from non-existent group."
                | None -> failwith ^ "Cannot remove entity '" + acstring entity.EntityAddress + "' from non-existent screen."
            | _ -> failwith ^ "Invalid entity address '" + acstring entity.EntityAddress + "'."
        let entityStates = Vmap.remove entity.EntityAddress world.EntityStates
        { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

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
        if entityState.PublishChanges then
            publish
                { Simulant = entity; OldWorld = oldWorld }
                (Events.EntityChange ->- entity)
                ["World.publishEntityChange"]
                entity
                world
        else world

    let inline internal getOptEntityState entity world =
        optEntityStateFinder entity world

    let internal getEntityState (entity : Entity) world =
        match getOptEntityState entity world with
        | Some entityState -> entityState
        | None -> failwith ^ "Could not find entity with address '" + acstring entity + "'."

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
            failwith ^ "Cannot set the state of a non-existent group '" + acstring group.GroupAddress + "'"
#endif
        let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
        { world with GroupStates = groupStates }

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
                | None -> failwith ^ "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."
        let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
        { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let private groupStateRemover group world =
        let screenDirectory =
            match Address.getNames group.GroupAddress with
            | [screenName; groupName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (screenAddress, groupDirectory) ->
                    let groupDirectory = Vmap.remove groupName groupDirectory
                    Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                | None -> failwith ^ "Cannot remove group '" + acstring group.GroupAddress + "' from non-existent screen."
            | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."
        let groupStates = Vmap.remove group.GroupAddress world.GroupStates
        { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let inline internal getOptGroupState group world =
        Vmap.tryFind group.GroupAddress world.GroupStates

    let internal getGroupState group world =
        match getOptGroupState group world with
        | Some groupState -> groupState
        | None -> failwith ^ "Could not find group with address '" + acstring group + "'."

    let internal addGroupState groupState group world =
        groupStateAdder groupState group world

    let internal removeGroupState group world =
        groupStateRemover group world

    let inline internal setGroupStateWithoutEvent groupState group world =
        groupStateSetter groupState group world

    let internal setGroupState groupState group world =
        let oldWorld = world
        let world = groupStateSetter groupState group world
        if groupState.PublishChanges then
            publish
                { Simulant = group; OldWorld = oldWorld }
                (Events.GroupChange ->- group)
                ["World.setGroupState"]
                group
                world
        else world

    let internal updateGroupState updater group world =
        let groupState = getGroupState group world
        let groupState = updater groupState
        setGroupState groupState group world

    (* ScreenState *)

    let private screenStateSetter screenState screen world =
#if DEBUG
        if not ^ Vmap.containsKey screen.ScreenAddress world.ScreenStates then
            failwith ^ "Cannot set the state of a non-existent screen '" + acstring screen.ScreenAddress + "'"
#endif
        let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
        { world with ScreenStates = screenStates }

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
            | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."
        let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
        { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let private screenStateRemover screen world =
        let screenDirectory =
            match Address.getNames screen.ScreenAddress with
            | [screenName] -> Vmap.remove screenName world.ScreenDirectory
            | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."
        let screenStates = Vmap.remove screen.ScreenAddress world.ScreenStates
        { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let inline internal getOptScreenState screen world =
        Vmap.tryFind screen.ScreenAddress world.ScreenStates

    let internal getScreenState (screen : Screen) world =
        match getOptScreenState screen world with
        | Some screenState -> screenState
        | None -> failwith ^ "Could not find screen with address '" + acstring screen + "'."

    let internal addScreenState screenState screen world =
        screenStateAdder screenState screen world

    let internal removeScreenState screen world =
        screenStateRemover screen world

    let inline internal setScreenStateWithoutEvent screenState screen world =
        screenStateSetter screenState screen world

    let internal setScreenState screenState screen world =
        let oldWorld = world
        let world = screenStateSetter screenState screen world
        if screenState.PublishChanges then
            publish
                { Simulant = screen; OldWorld = oldWorld }
                (Events.ScreenChange ->- screen)
                ["World.setScreenState"]
                screen
                world
        else world

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
        let world = { world with GameState = gameState }
        if gameState.PublishChanges then
            publish
                { OldWorld = oldWorld; Simulant = Simulants.Game }
                (Events.GameChange ->- Simulants.Game)
                ["World.setGameState"]
                Simulants.Game
                world
        else world

    let internal updateGameState updater world =
        let gameState = getGameState world
        let gameState = updater gameState
        setGameState gameState world