// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open Prime

/// Adds the capability to use purely-functional events with the given program type 'w.
type Eventable<'w when 'w :> 'w Eventable> =
    interface
        abstract member GetLiveness : unit -> Liveness
        abstract member GetEventSystem : unit -> 'w EventSystem
        abstract member GetEmptyParticipant : unit -> Participant
        abstract member UpdateEventSystem : ('w EventSystem -> 'w EventSystem) -> 'w
        abstract member ContainsParticipant : Participant -> bool
        abstract member PublishEvent<'a, 'p when 'p :> Participant> : Participant -> 'p -> 'a -> 'a Address -> EventTrace -> obj -> 'w -> Handling * 'w
        end

[<RequireQualifiedAccess>]
module Eventable =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> (HashIdentity.FromFunctions Address<obj>.hash Address<obj>.equals)

    /// Get the event system.
    let getEventSystem<'w when 'w :> 'w Eventable> (world : 'w) =
        world.GetEventSystem ()

    /// Get the event system as tranformed via 'by'.
    let getEventSystemBy<'a, 'w when 'w :> 'w Eventable> (by : EventSystem<'w> -> 'a) (world : 'w) : 'a =
        let eventSystem = world.GetEventSystem ()
        by eventSystem

    /// Update the event system in the world.
    let updateEventSystem<'w when 'w :> 'w Eventable> updater (world : 'w) =
        world.UpdateEventSystem updater

    /// Get event subscriptions.
    let getSubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy EventSystem.getSubscriptions world

    /// Get event unsubscriptions.
    let getUnsubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy EventSystem.getUnsubscriptions world

    /// Set event subscriptions.
    let private setSubscriptions<'w when 'w :> 'w Eventable> subscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setSubscriptions subscriptions)

    /// Set event unsubscriptions.
    let private setUnsubscriptions<'w when 'w :> 'w Eventable> unsubscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setUnsubscriptions unsubscriptions)

    /// Add event state to the world.
    let addEventState<'a, 'w when 'w :> 'w Eventable> key (state : 'a) (world : 'w) =
        world.UpdateEventSystem (EventSystem.addEventState key state)

    /// Remove event state from the world.
    let removeEventState<'w when 'w :> 'w Eventable> key (world : 'w) =
        world.UpdateEventSystem (EventSystem.removeEventState key)

    /// Get event state from the world.
    let getEventState<'a, 'w when 'w :> 'w Eventable> key (world : 'w) : 'a =
        getEventSystemBy (EventSystem.getEventState<'a, 'w> key) world

    /// Get whether events are being traced.
    let getEventTracing<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy (EventSystem.getEventTracing<'w>) world

    /// Set whether events are being traced.
    let setEventTracing<'w when 'w :> 'w Eventable> tracing (world : 'w) =
        updateEventSystem (EventSystem.setEventTracing tracing) world

    /// Get the state of the event filter.
    let getEventFilter<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy (EventSystem.getEventFilter) world

    /// Set the state of the event filter.
    let setEventFilter<'w when 'w :> 'w Eventable> filter (world : 'w) =
        updateEventSystem (EventSystem.setEventFilter filter) world

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

    let private boxSubscription<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable> (subscription : Subscription<'a, 's, 'w>) =
        let boxableSubscription = fun (evt : obj) world ->
            try subscription (evt :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                Log.debug ^
                    "If you've reached this exception, then you've probably inadvertantly mixed up an event type " +
                    "parameter for some form of Eventable.publish or subscribe. " +
                    "This exception can also crop up when your implementation of Eventable.PublishEvent doesn't " +
                    "correctly specialize its 's and 'w types for Eventable.publishEvent calls."
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let getSortableSubscriptions
        (getEntityPublishingPriority : Participant -> 'w -> single) (subscriptions : SubscriptionEntry list) (world : 'w) :
        (single * SubscriptionEntry) list =
        List.foldBack
            (fun (key, participant : Participant, subscription) subscriptions ->
                let priority = participant.GetPublishingPriority getEntityPublishingPriority world
                let subscription = (priority, (key, participant, subscription))
                subscription :: subscriptions)
            subscriptions
            []

    let getSubscriptionsSorted (publishSorter : SubscriptionSorter<'w>) eventAddress (world : 'w) =
        let eventSystem = getEventSystem world
        let subscriptions = EventSystem.getSubscriptions eventSystem
        match Vmap.tryFind eventAddress subscriptions with
        | Some subList -> publishSorter subList world
        | None -> []

    let getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<'w>) eventAddress (world : 'w) =
        let eventSystem = getEventSystem world
        let subscriptions = EventSystem.getSubscriptions eventSystem
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Vmap.tryFind anyEventAddress subscriptions) anyEventAddresses
        let optSubLists = Vmap.tryFind eventAddress subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        publishSorter subList world

    let logEvent<'w when 'w :> 'w Eventable> eventAddress eventTrace (world : 'w) =
        EventSystem.logEvent<'w> eventAddress eventTrace (getEventSystem world)

    let publishEvent<'a, 'p, 's, 'w when 'p :> Participant and 's :> Participant and 'w :> 'w Eventable>
        (subscriber : Participant) (publisher : 'p) (eventData : 'a) (eventAddress : 'a Address) eventTrace subscription (world : 'w) =
        let evt =
            { Data = eventData
              Address = eventAddress
              Trace = eventTrace
              Subscriber = subscriber :?> 's
              Publisher = publisher :> Participant }
        let callableSubscription = unbox<BoxableSubscription<'w>> subscription
        callableSubscription evt world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : 'w) =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith Pair.sortFstDescending subscriptions
        List.map snd subscriptions

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : 'w) =
        subscriptions

    /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
    let publish7<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        getSubscriptions (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        let objEventAddress = atooa eventAddress
        logEvent<'w> objEventAddress eventTrace world
        let subscriptions = getSubscriptions publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (handling, world : 'w) (_, subscriber : Participant, subscription) ->
                    if  (match handling with Cascade -> true | Resolve -> false) &&
                        (match world.GetLiveness () with Running -> true | Exiting -> false) then
                        let publishResult = world.PublishEvent subscriber publisher eventData eventAddress eventTrace subscription world
                        Some publishResult
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish6<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publish7<'a, 'p, 'w> getSubscriptionsSorted3 publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event with no subscription sorting.
    let publish<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publish6<'a, 'p, 'w> sortSubscriptionsNone eventData eventAddress eventTrace publisher world

    /// Unsubscribe from an event.
    let unsubscribe<'w when 'w :> 'w Eventable> subscriptionKey (world : 'w) =
        let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
        match Vmap.tryFind subscriptionKey unsubscriptions with
        | Some (eventAddress, subscriber) ->
            match Vmap.tryFind eventAddress subscriptions with
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriber', _) -> subscriptionKey' = subscriptionKey && subscriber' = subscriber)
                        subscriptionList
                let subscriptions = 
                    match subscriptionList with
                    | [] -> Vmap.remove eventAddress subscriptions
                    | _ -> Vmap.add eventAddress subscriptionList subscriptions
                let unsubscriptions = Vmap.remove subscriptionKey unsubscriptions
                let world = setSubscriptions subscriptions world
                let world = setUnsubscriptions unsubscriptions world
                publish
                    eventAddress
                    (ntoa<obj Address> !!"Unsubscribe")
                    (EventTrace.record "Eventable" "unsubscribe" EventTrace.empty)
                    (world.GetEmptyParticipant ())
                    world
            | None -> world // TODO: consider an assert fail here?
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Participant, boxSubscription subscription)
                match Vmap.tryFind objEventAddress subscriptions with
                | Some subscriptionEntries -> Vmap.add objEventAddress (subscriptionEntry :: subscriptionEntries) subscriptions
                | None -> Vmap.add objEventAddress [subscriptionEntry] subscriptions
            let unsubscriptions = Vmap.add subscriptionKey (objEventAddress, subscriber :> Participant) unsubscriptions
            let world = setSubscriptions subscriptions world
            let world = setUnsubscriptions unsubscriptions world
            let world =
                publish
                    objEventAddress
                    (ntoa<obj Address> !!"Subscribe")
                    (EventTrace.record "Eventable" "subscribePlus5" EventTrace.empty)
                    (world.GetEmptyParticipant ())
                    world
            (unsubscribe<'w> subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        subscribePlus5 (makeGuid ()) subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        subscribePlus5 subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) world |> snd

    /// Subscribe to an event.
    let subscribe<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribe5 (makeGuid ()) subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a participant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        let subscriberAddress = subscriber.ParticipantAddress
        if not ^ Address.isEmpty subscriberAddress then
            let monitorKey = makeGuid ()
            let removalKey = makeGuid ()
            let world = subscribe5<'a, 's, 'w> monitorKey subscription eventAddress subscriber world
            let unsubscribe = fun (world : 'w) ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ eventSystem -> (Cascade, unsubscribe eventSystem)
            let removingEventAddress = ftoa<unit> !!(typeof<'s>.Name + "/Removing") ->>- subscriberAddress
            let world = subscribe5<unit, 's, 'w> removalKey subscription' removingEventAddress subscriber world
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the lifetime of a participant.
    let monitor<'a, 's, 'w when 's :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 's, 'w> subscription eventAddress subscriber world |> snd