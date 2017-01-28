// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open Prime

/// The context in which all events take place. Effectively a mix-in for the 'w type, where 'w is a type that
/// represents the client program.
type EventWorld<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
    interface
        abstract member GetLiveness : unit -> Liveness
        abstract member GetEventSystem : unit -> 'w EventSystem
        abstract member UpdateEventSystem : ('w EventSystem -> 'w EventSystem) -> 'w
        abstract member ContainsParticipant : Participant -> bool
        abstract member PublishEvent<'a, 'p when 'p :> Participant> : Participant -> 'p -> 'a -> 'a Address -> EventTrace -> obj -> 'w -> Handling * 'w
        end

[<RequireQualifiedAccess>]
module EventWorld =

    let mutable EventAddressCaching = false
    let EventAddressCache = Dictionary<obj, obj> HashIdentity.Structural
    let EventAddressListCache = Dictionary<obj Address, obj List> HashIdentity.Structural

    /// Get the event system.
    let getEventSystem<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        world.GetEventSystem ()

    /// Get the event system as tranformed via 'by'.
    let getEventSystemBy<'a, 'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (by : 'w EventSystem -> 'a) (world : 'w) : 'a =
        let eventSystem = world.GetEventSystem ()
        by eventSystem

    /// Update the event system in the world.
    let updateEventSystem<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> updater (world : 'w) =
        world.UpdateEventSystem updater

    /// Get event subscriptions.
    let getSubscriptions<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getSubscriptions world

    /// Get event unsubscriptions.
    let getUnsubscriptions<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getUnsubscriptions world

    /// Set event subscriptions.
    let private setSubscriptions<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> subscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setSubscriptions subscriptions)

    /// Set event unsubscriptions.
    let private setUnsubscriptions<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> unsubscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setUnsubscriptions unsubscriptions)

    /// Add event state to the world.
    let addEventState<'a, 'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> key (state : 'a) (world : 'w) =
        world.UpdateEventSystem (EventSystem.addEventState key state)

    /// Remove event state from the world.
    let removeEventState<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> key (world : 'w) =
        world.UpdateEventSystem (EventSystem.removeEventState key)

    /// Get event state from the world.
    let getEventState<'a, 'g ,'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> key (world : 'w) : 'a =
        getEventSystemBy (EventSystem.getEventState<'a, 'w> key) world

    /// Get whether events are being traced.
    let getEventTracing<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getEventTracing<'w> world

    /// Set whether events are being traced.
    let setEventTracing<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> tracing (world : 'w) =
        updateEventSystem (EventSystem.setEventTracing tracing) world

    /// Get the state of the event filter.
    let getEventFilter<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getEventFilter world

    /// Set the state of the event filter.
    let setEventFilter<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> filter (world : 'w) =
        updateEventSystem (EventSystem.setEventFilter filter) world

    /// Get the event context of the world.
    let getEventContext<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getEventContext world

    /// Set the event context of the world.
    let setEventContext<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> context (world : 'w) =
        updateEventSystem (EventSystem.setEventContext context) world

    /// Qualify the event context of the world.
    let qualifyEventContext<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (address : obj Address) (world : 'w) =
        getEventSystemBy (EventSystem.qualifyEventContext address) world

    /// Set whether event addresses are cached internally.
    /// If you enable caching, be sure to use EventWorld.cleanEventAddressCache to keep the cache from expanding
    /// indefinitely.
    let setEventAddressCaching caching =
        if not caching then
            EventAddressCache.Clear ()
            EventAddressListCache.Clear ()
        EventAddressCaching <- caching

    /// Remove from the event address cache all addresses belonging to the given target.
    let cleanEventAddressCache (eventTarget : 'a Address) =
        if EventAddressCaching then
            let eventTargetOa = atooa eventTarget
            match EventAddressListCache.TryGetValue eventTargetOa with
            | (true, entries) ->
                for entry in entries do EventAddressCache.Remove entry |> ignore
                EventAddressListCache.Remove eventTargetOa |> ignore
            | (false, _) -> ()
        else ()

    // NOTE: event addresses are ordered from general to specific. This is so a generalized subscriber can preempt
    // any specific subscribers. Whether this is the best order is open for discussion.
    let private getEventAddresses1 (eventAddress : 'a Address) =
        // OPTIMIZATION: imperative for speed
        let eventAddressNames = eventAddress |> Address.getNames |> Array.ofList
        Seq.foldi (fun index eventAddresses _ ->
            let index = eventAddressNames.Length - index - 1
            let eventAddressNamesAny = Array.zeroCreate eventAddressNames.Length // TODO: use Array.zeroCreateUnchecked if / when it becomes available
            Array.Copy (eventAddressNames, 0, eventAddressNamesAny, 0, eventAddressNames.Length)
            eventAddressNamesAny.[index] <- Address.head Events.Wildcard
            let eventAddressAny = eventAddressNamesAny |> List.ofArray |> Address.ltoa
            eventAddressAny :: eventAddresses)
            [eventAddress]
            eventAddressNames

    let private getEventAddresses2 (eventAddress : 'a Address) allowWildcard =
        if allowWildcard then
            if EventAddressCaching then
                match EventAddressCache.TryGetValue eventAddress with
                | (false, _) ->
                    let eventAddressNames = Address.getNames eventAddress
                    let eventAddresses = getEventAddresses1 eventAddress
                    let eventTargetIndex = List.findIndex (fun name -> Name.getNameStr name = "Event") eventAddressNames + 1
                    if eventTargetIndex < List.length eventAddressNames then
                        let eventTarget = eventAddressNames |> List.skip eventTargetIndex |> Address.makeFromList
                        match EventAddressListCache.TryGetValue eventTarget with
                        | (false, _) -> EventAddressListCache.Add (eventTarget, List [eventAddress :> obj]) |> ignore
                        | (true, list) -> list.Add eventAddress
                        EventAddressCache.Add (eventAddress, eventAddresses)
                    eventAddresses
                | (true, eventAddressesObj) -> eventAddressesObj :?> 'a Address list
            else getEventAddresses1 eventAddress
        else [eventAddress]

    let private debugSubscriptionTypeMismatch () =
        Log.debug ^
            "If you've reached this exception, then you've probably inadvertantly mixed up an event type " +
            "parameter for some form of EventWorld.publish or subscribe. " +
            "This exception can also crop up when your implementation of EventWorld.PublishEvent doesn't " +
            "correctly specialize its 's and 'w types for EventWorld.publishEvent calls."

    let private boxSubscription<'a, 's, 'g, 'w when 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscription : Event<'a, 's> -> 'w -> Handling * 'w) =
        let boxableSubscription = fun (evtObj : obj) world ->
            match subscription :> obj with
            | :? (Event<obj, 's> -> 'w -> Handling * 'w) as subscriptionDynamic ->
                let evt = try evtObj :?> Event<obj, 's> with exn -> debugSubscriptionTypeMismatch (); reraise ()
                subscriptionDynamic evt world
            | _ ->
                let evt = try evtObj :?> Event<'a, 's> with exn -> debugSubscriptionTypeMismatch (); reraise ()
                subscription evt world
        boxableSubscription :> obj

    let getSortableSubscriptions
        (getSortPriority : Participant -> 'w -> IComparable)
        (subscriptions : SubscriptionEntry list)
        (world : 'w) :
        (IComparable * SubscriptionEntry) list =
        List.foldBack
            (fun (key, participant : Participant, subscription) subscriptions ->
                let priority = getSortPriority participant world
                let subscription = (priority, (key, participant, subscription))
                subscription :: subscriptions)
            subscriptions
            []

    let private getSubscriptionsSorted (publishSorter : SubscriptionSorter<'w>) eventAddress allowWildcard (world : 'w) =
        let eventSystem = getEventSystem world
        let subscriptions = EventSystem.getSubscriptions eventSystem
        let eventAddresses = getEventAddresses2 eventAddress allowWildcard
        let subListOpts = List.map (fun eventAddress -> UMap.tryFind eventAddress subscriptions) eventAddresses
        let subLists = List.definitize subListOpts
        let subList = List.concat subLists
        publishSorter subList world

    let private logEvent<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> eventAddress eventTrace (world : 'w) =
        EventSystem.logEvent<'w> eventAddress eventTrace (getEventSystem world)

    let private pushEventAddress<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> eventAddress (world : 'w) =
        updateEventSystem (EventSystem.pushEventAddress eventAddress) world

    let private popEventAddress<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        updateEventSystem EventSystem.popEventAddress world

    let private getEventAddresses<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> (world : 'w) =
        getEventSystemBy EventSystem.getEventAddresses world

    /// Publish an event directly.
    let publishEvent<'a, 'p, 's, 'g, 'w when 'p :> Participant and 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscriber : Participant) (publisher : 'p) (eventData : 'a) (eventAddress : 'a Address) eventTrace subscription (world : 'w) =
        let evt =
            { Data = eventData
              DataType = typeof<'a>
              Address = eventAddress
              Trace = eventTrace
              Subscriber = subscriber :?> 's
              Publisher = publisher :> Participant }
        let callableSubscription = unbox<BoxableSubscription<'w>> subscription
        let oldEventContext = getEventContext world
        let world = setEventContext subscriber world
        let (handling, world) = callableSubscription evt world
        let world = setEventContext oldEventContext world
        (handling, world)

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : 'w) =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith (fun ((p : IComparable), _) ((p2 : IComparable), _) -> p.CompareTo p2) subscriptions
        List.map snd subscriptions

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : 'w) =
        subscriptions

    /// Publish an event, using the given publishSorter procedures to arrange the order to which subscriptions are published.
    let publishPlus<'a, 'p, 'g, 'w when 'p :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (publishSorter : SubscriptionSorter<'w>)
        (eventData : 'a)
        (eventAddress : 'a Address)
        eventTrace
        (publisher : 'p)
        allowWildcard
        (world : 'w) =
        let objEventAddress = atooa eventAddress in logEvent<'g, 'w> objEventAddress eventTrace world
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress allowWildcard world
        let (_, world) =
            List.foldWhile
                (fun (handling, world : 'w) (_, subscriber : Participant, subscription) ->
#if DEBUG
                    let eventAddresses = getEventAddresses world
                    let cycleDetected = List.containsTriplicates eventAddresses
                    if cycleDetected then Log.info ^ "Event cycle detected in '" + scstring eventAddresses + "'."
#else
                    let cycleDetected = false
#endif
                    if not cycleDetected &&
                       (match handling with Cascade -> true | Resolve -> false) &&
                       (match world.GetLiveness () with Running -> true | Exiting -> false) then
#if DEBUG
                        let world = pushEventAddress objEventAddress world
#endif
                        let (handling, world) = world.PublishEvent subscriber publisher eventData eventAddress eventTrace subscription world
#if DEBUG
                        let world = popEventAddress world
#endif
                        Some (handling, world)
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event with no subscription sorting.
    let publish<'a, 'p, 'g, 'w when 'p :> Participant and 'w :> EventWorld<'g, 'w>>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publishPlus<'a, 'p, 'g, 'w> sortSubscriptionsNone eventData eventAddress eventTrace publisher true world

    /// Unsubscribe from an event.
    let unsubscribe<'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> subscriptionKey (world : 'w) =
        let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
        match UMap.tryFind subscriptionKey unsubscriptions with
        | Some (eventAddress, subscriber) ->
            match UMap.tryFind eventAddress subscriptions with
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriber', _) -> subscriptionKey' = subscriptionKey && subscriber' = subscriber)
                        subscriptionList
                let subscriptions = 
                    match subscriptionList with
                    | [] -> UMap.remove eventAddress subscriptions
                    | _ -> UMap.add eventAddress subscriptionList subscriptions
                let unsubscriptions = UMap.remove subscriptionKey unsubscriptions
                let world = setSubscriptions subscriptions world
                let world = setUnsubscriptions unsubscriptions world
                publish<_, _, 'g, 'w>
                    eventAddress
                    (ltoa<obj Address> [!!"Unsubscribe"; !!"Event"])
                    (EventTrace.record "EventWorld" "unsubscribe" EventTrace.empty)
                    (EventSystem.getGlobalPariticipant (world.GetEventSystem ()))
                    world
            | None -> world
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's, 'g, 'w when 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        subscriptionKey (subscription : Event<'a, 's> -> 'w -> Handling * 'w) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Participant, boxSubscription subscription)
                match UMap.tryFind objEventAddress subscriptions with
                | Some subscriptionEntries -> UMap.add objEventAddress (subscriptionEntries @ [subscriptionEntry]) subscriptions // NOTE: inefficient to add to back of list! Use a Queue instead!
                | None -> UMap.add objEventAddress [subscriptionEntry] subscriptions
            let unsubscriptions = UMap.add subscriptionKey (objEventAddress, subscriber :> Participant) unsubscriptions
            let world = setSubscriptions subscriptions world
            let world = setUnsubscriptions unsubscriptions world
            let world =
                publish
                    objEventAddress
                    (ltoa<obj Address> [!!"Subscribe"; !!"Event"])
                    (EventTrace.record "EventWorld" "subscribePlus5" EventTrace.empty)
                    (EventSystem.getGlobalPariticipant (world.GetEventSystem ()))
                    world
            (unsubscribe<'g, 'w> subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event.
    let subscribe<'a, 's, 'g, 'w when 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscription : Event<'a, 's> -> 'w -> 'w) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus (makeGuid ()) (fun evt world -> (Cascade, subscription evt world)) eventAddress subscriber world |> snd

    /// Keep active a subscription for the life span of a participant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's, 'g, 'w when 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscription : Event<'a, 's> -> 'w -> Handling * 'w) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        let subscriberAddress = subscriber.ParticipantAddress
        if not ^ Address.isEmpty subscriberAddress then
            let monitorKey = makeGuid ()
            let removalKey = makeGuid ()
            let world = subscribePlus<'a, 's, 'g, 'w> monitorKey subscription eventAddress subscriber world |> snd
            let unsubscribe = fun (world : 'w) ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ eventSystem -> (Cascade, unsubscribe eventSystem)
            let removingEventAddress = ltoa<unit> [!!typeof<'s>.Name; !!"Unregistering"; !!"Event"] ->>- subscriberAddress
            let world = subscribePlus<unit, 's, 'g, 'w> removalKey subscription' removingEventAddress subscriber world |> snd
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the life span of a participant.
    let monitor<'a, 's, 'g, 'w when 's :> Participant and 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscription : Event<'a, 's> -> 'w -> 'w) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 's, 'g, 'w> (fun evt world -> (Cascade, subscription evt world)) eventAddress subscriber world |> snd