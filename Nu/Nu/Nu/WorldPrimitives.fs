namespace Nu
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<RequireQualifiedAccess>]
module World =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    let internal getSubsystem<'s when 's :> Subsystem> name world =
        Map.find name world.Subsystems :?> 's

    let internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
        let subsystem = getSubsystem<'s> name world
        by subsystem

    let internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
        let subsystems = Map.add name (subsystem :> Subsystem) world.Subsystems
        { world with Subsystems = subsystems }

    let internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
        let subsystem = getSubsystem<'s> name world
        let subsystem = updater subsystem world
        setSubsystem subsystem name world

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () =
        Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () =
        Guid.NewGuid ()

    /// Try to get a simulant at the given address from the world.
    let mutable private getOptSimulantForPublishing =
        Unchecked.defaultof<Simulant Address -> World -> Simulant option>

    /// Get a simulant at the given address from the world.
    let private getSimulantForPublishing address world =
        Option.get <| getOptSimulantForPublishing address world

    // OPTIMIZATION: priority annotated as single to decrease GC pressure.
    let private sortFstDesc (priority : single, _) (priority2 : single, _) =
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0
    
    let private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
        let boxableSubscription = fun (event : obj) world ->
            try subscription (event :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world : (single * SubscriptionEntry) list =
        List.fold
            (fun subscriptions (key, address, subscription) ->
                match getOptSimulantForPublishing (atoua address) world with
                | Some simulant ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, address, subscription))
                    subscription :: subscriptions
                | None -> (0.0f, (key, address, subscription)) :: subscriptions)
            []
            subscriptions

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    /// Sort subscriptions by their editor picking priority.
    let sortSubscriptionsByPickingPriority subscriptions world =
        sortSubscriptionsBy
            (fun (entity : Entity) world -> entity.DispatcherNp.GetPickingPriority (entity, world))
            subscriptions
            world

    /// Sort subscriptions by their place in the world's simulant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> EntityPublishingPriority)
            subscriptions
            world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
        subscriptions

    let private getAnyEventAddresses eventAddress =
        // OPTIMIZATION: uses memoization.
        if not <| Address.isEmpty eventAddress then
            let anyEventAddressesKey = Address.allButLast eventAddress
            match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
            | (true, anyEventAddresses) -> anyEventAddresses
            | (false, _) ->
                let eventAddressList = eventAddress.Names
                let anyEventAddressList = AnyEventAddress.Names
                let anyEventAddresses =
                    [for i in 0 .. List.length eventAddressList - 1 do
                        let subNameList = List.take i eventAddressList @ anyEventAddressList
                        yield Address.make subNameList]
                AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                anyEventAddresses
        else failwith "Event name cannot be empty."

    let private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
        let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        let subListRev = List.rev subList
        publishSorter subListRev world

    let private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
        (subscriberAddress : obj Address) (publisherAddress : 'p Address) (eventAddress : 'a Address) (eventData : 'a) subscription world =
        let event =
            { SubscriberAddress = Address.changeType<obj, 's> subscriberAddress
              PublisherAddress = Address.changeType<'p, Simulant> publisherAddress
              EventAddress = eventAddress
              Subscriber = getSimulantForPublishing (Address.changeType<obj, Simulant> subscriberAddress) world :?> 's
              Data = eventData }
        let callableSubscription = unbox<BoxableSubscription> subscription
        let result = callableSubscription event world
        Some result

    /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
    let publish<'a, 'p when 'p :> Simulant>
        publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world) (_, subscriberAddress, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.State.Liveness with Running -> true | Exiting -> false) then
                        match subscriberAddress.Names with
                        | [] -> publishEvent<'a, 'p, Game> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_] -> publishEvent<'a, 'p, Screen> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_; _] -> publishEvent<'a, 'p, Group> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | [_; _; _] -> publishEvent<'a, 'p, Entity> subscriberAddress publisherAddress eventAddress eventData subscription world
                        | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event.
    let publish4<'a, 'p when 'p :> Simulant>
        (eventData : 'a) (eventAddress : 'a Address) (publisherAddress : 'p Address) world =
        publish sortSubscriptionsByHierarchy eventData eventAddress publisherAddress world

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
        if not <| Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, atooa subscriberAddress, boxSubscription subscription)
                match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
            let unsubscriptions = Map.add subscriptionKey (objEventAddress, atooa subscriberAddress) world.Callbacks.Unsubscriptions
            let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
            { world with Callbacks = callbacks }
        else failwith "Event name cannot be empty."

    /// Subscribe to an event.
    let subscribe4<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
        subscribe (makeSubscriptionKey ()) subscription eventAddress subscriberAddress world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
        match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
        | Some (eventAddress, subscriberAddress) ->
            match Map.tryFind eventAddress world.Callbacks.Subscriptions with
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriberAddress', _) ->
                            subscriptionKey' = subscriptionKey &&
                            subscriberAddress' = subscriberAddress)
                        subscriptionList
                let subscriptions = 
                    match subscriptionList with
                    | [] -> Map.remove eventAddress world.Callbacks.Subscriptions
                    | _ -> Map.add eventAddress subscriptionList world.Callbacks.Subscriptions
                let unsubscriptions = Map.remove subscriptionKey world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            | None -> world
        | None -> world

    /// Keep active a subscription for the lifetime of a simulant.
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriberAddress : 's Address) world =
        if not <| Address.isEmpty subscriberAddress then
            let monitorKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe<'a, 's> monitorKey subscription eventAddress subscriberAddress world
            let subscription' = fun _ world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                (Cascade, world)
            let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriberAddress
            subscribe<unit, 's> removalKey subscription' removingEventAddress subscriberAddress world
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Clear the physics messages.
    let internal clearPhysicsMessages world =
        updateSubsystem (fun is _ -> is.ClearMessages ()) IntegratorSubsystemName world

    /// Clear the rendering messages.
    let internal clearRenderMessages world =
        updateSubsystem (fun rs _ -> rs.ClearMessages ()) RendererSubsystemName world

    /// Clear the audio messages.
    let internal clearAudioMessages world =
        updateSubsystem (fun aps _ -> aps.ClearMessages ()) AudioPlayerSubsystemName world

    /// Add a physics message to the world.
    let addPhysicsMessage message world =
        updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

    /// Add a rendering message to the world.
    let addRenderMessage message world =
        updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

    /// Add an audio message to the world.
    let addAudioMessage (message : AudioMessage) world =
        updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

    /// Set the EntityDispatchers field of the world.
    let internal setEntityDispatchers dispatchers world =
        let components = { world.Components with EntityDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GroupDispatchers field of the world.
    let internal setGroupDispatchers dispatchers world =
        let components = { world.Components with GroupDispatchers = dispatchers }
        { world with Components = components }

    /// Set the ScreenDispatchers field of the world.
    let internal setScreenDispatchers dispatchers world =
        let components = { world.Components with ScreenDispatchers = dispatchers }
        { world with Components = components }

    /// Set the GameDispatchers field of the world.
    let internal setGameDispatchers dispatchers world =
        let components = { world.Components with GameDispatchers = dispatchers }
        { world with Components = components }

    /// Set the Facets field of the world.
    let internal setFacets facets world =
        let components = { world.Components with Facets = facets }
        { world with Components = components }

    /// Add a task to be executed by the engine at the specified task tick.
    let addTask task world =
        let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Add multiple task to be executed by the engine at the specified task tick.
    let addTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
        { world with Callbacks = callbacks }

    /// Restore tasks to be executed by the engine at the specified task tick.
    let internal restoreTasks tasks world =
        let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
        { world with Callbacks = callbacks }

    /// Clear all tasks.
    let internal clearTasks world =
        let callbacks = { world.Callbacks with Tasks = [] }
        { world with Callbacks = callbacks }

    /// Add callback state to the world.
    let addCallbackState key state world =
        let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Remove callback state from the world.
    let removeCallbackState key world =
        let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Get callback state from the world.
    let getCallbackState<'a> key world =
        let state = Map.find key world.Callbacks.CallbackStates
        state :?> 'a

    /// Get the state of the world.
    let getState world = world.State

    /// Set the state of the world.
    let setState state world =
        let oldState = world.State
        let world = { world with State = state }
        publish4 { OldWorldState = oldState } WorldStateChangeEventAddress GameAddress world

    /// Update the state of the world.
    let updateState updater world =
        setState (updater world.State) world

    /// Get the tick time.
    let getTickTime world =
        world.State.TickTime

    /// Increment the tick time.
    let internal incrementTickTime world =
        let state = { world.State with TickTime = world.State.TickTime + 1L }
        setState state world

    /// Get the the liveness state of the world.
    let getLiveness world =
        world.State.Liveness

    /// Place the world into a state such that the app will exit at the end of the current frame.
    let exit world =
        let state = { world.State with Liveness = Exiting }
        setState state world

    /// Query that the engine is in game-playing mode.
    let isGamePlaying world =
        Interactivity.isGamePlaying world.State.Interactivity

    /// Query that the physics system is running.
    let isPhysicsRunning world =
        Interactivity.isPhysicsRunning world.State.Interactivity

    /// Get the interactivity state of the world.
    let getInteractivity world =
        world.State.Interactivity

    /// Set the level of the world's interactivity.
    let setInteractivity interactivity world =
        let state = { world.State with Interactivity = interactivity }
        setState state world

    /// Update the the level of the world's interactivity.
    let updateInteractivity updater world =
        let interactivity = updater <| getInteractivity world
        setInteractivity interactivity world

    /// Get the camera used to view the world.
    let getCamera world =
        world.State.Camera

    /// Set the camera used to view the world.
    let setCamera camera world =
        let state = { world.State with Camera = camera }
        setState state world

    /// Update the camera used to view the world.
    let updateCamera updater world =
        let camera = updater <| getCamera world
        setCamera camera world

    /// Get the OptScreenTransitionDestinationAddress field of the world.
    let getOptScreenTransitionDestinationAddress world =
        world.State.OptScreenTransitionDestinationAddress

    /// Set the OptScreenTransitionDestinationAddress field of the world.
    let internal setOptScreenTransitionDestinationAddress address world =
        let state = { world.State with OptScreenTransitionDestinationAddress = address  }
        setState state world

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        world.State.AssetMetadataMap

    /// Set the asset metadata map.
    let internal setAssetMetadataMap assetMetadataMap world =
        let state = { world.State with AssetMetadataMap = assetMetadataMap }
        setState state world

    /// Set the Overlayer field of the world.
    let internal setOverlayer overlayer world =
        let state = { world.State with Overlayer = overlayer }
        setState state world

    /// Get the user state of the world, casted to 'u.
    let getUserState world : 'u =
        world.State.UserState :?> 'u

    /// Set the user state of the world.
    let setUserState (userState : 'u) world =
        let state = { world.State with UserState = userState }
        setState state world

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) world =
        let state = getUserState world
        let state = updater state
        setUserState state world

    /// Ignore all handled events.
    let handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Cascade, world)

    /// Swallow all handled events.
    let handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, world)
        
    /// Handle event by exiting app.
    let handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, exit world)