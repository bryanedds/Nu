// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open FSharpx.Collections
open Prime

[<RequireQualifiedAccess>]
module Events =

    /// Represents any event.
    let Any = ntoa<obj> !!"*"

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EventSystem =

    /// Get all tasklets.
    let getTasklets eventSystem =
        eventSystem.Tasklets

    /// Clear all held tasklets.
    let clearTasklets eventSystem =
        { eventSystem with Tasklets = Queue.empty }

    /// Restore previously-held tasklets.
    let restoreTasklets<'w when 'w :> 'w Eventable> (tasklets : 'w Tasklet Queue) eventSystem =
        { eventSystem with Tasklets = Queue.ofSeq ^ Seq.append (eventSystem.Tasklets :> 'w Tasklet seq) (tasklets :> 'w Tasklet seq) }

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet eventSystem =
        { eventSystem with Tasklets = Queue.conj tasklet eventSystem.Tasklets }

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets eventSystem =
        { eventSystem with Tasklets = Queue.ofSeq ^ Seq.append (tasklets :> 'w Tasklet seq) (eventSystem.Tasklets :> 'w Tasklet seq) }

    /// Add event state.
    let addEventState key (state : 'a) eventSystem =
        { eventSystem with EventStates = Vmap.add key (state :> obj) eventSystem.EventStates }

    /// Remove event state.
    let removeEventState key eventSystem =
        { eventSystem with EventStates = Vmap.remove key eventSystem.EventStates }

    /// Get subscriptions.
    let getSubscriptions eventSystem =
        eventSystem.Subscriptions

    /// Get unsubscriptions.
    let getUnsubscriptions eventSystem =
        eventSystem.Unsubscriptions

    /// Set subscriptions.
    let internal setSubscriptions subscriptions eventSystem =
        { eventSystem with Subscriptions = subscriptions }

    /// Set unsubscriptions.
    let internal setUnsubscriptions unsubscriptions eventSystem =
        { eventSystem with Unsubscriptions = unsubscriptions }

    /// Get event state.
    let getEventState<'a, 'w when 'w :> 'w Eventable> key (eventSystem : 'w EventSystem) =
        let state = Vmap.find key eventSystem.EventStates
        state :?> 'a

    /// Make an event system.
    let make () =
        { Subscriptions = Vmap.makeEmpty ()
          Unsubscriptions = Vmap.makeEmpty ()
          EventStates = Vmap.makeEmpty ()
          Tasklets = Queue.empty }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Eventable =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> (HashIdentity.FromFunctions Address<obj>.hash Address<obj>.equals)

    let getEventSystem<'w when 'w :> 'w Eventable> (world : 'w) =
        world.GetEventSystem ()

    let getEventSystemBy<'a, 'w when 'w :> 'w Eventable> (by : EventSystem<'w> -> 'a) (world : 'w) : 'a =
        let eventSystem = world.GetEventSystem ()
        by eventSystem

    let updateEventSystem<'w when 'w :> 'w Eventable> updater (world : 'w) =
        world.UpdateEventSystem updater

    /// Get event subscriptions.
    let getSubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy EventSystem.getSubscriptions world

    /// Get event unsubscriptions.
    let getUnsubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy EventSystem.getUnsubscriptions world

    /// Set event subscriptions.
    let internal setSubscriptions<'w when 'w :> 'w Eventable> subscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setSubscriptions subscriptions)

    /// Set event unsubscriptions.
    let internal setUnsubscriptions<'w when 'w :> 'w Eventable> unsubscriptions (world : 'w) =
        world.UpdateEventSystem (EventSystem.setUnsubscriptions unsubscriptions)

    /// Add event state to the world.
    let addEventState<'a, 'w when 'w :> 'w Eventable> key (state : 'a) (world : 'w) =
        world.UpdateEventSystem (EventSystem.addEventState key state)

    /// Remove event state from the world.
    let removeEventState<'w when 'w :> 'w Eventable> key (world : 'w) =
        world.UpdateEventSystem (EventSystem.removeEventState key)

    /// Get event state from the world.
    let getEventState<'a, 'w when 'w :> 'w Eventable> key (world : 'w) : 'a =
        let eventSystem = getEventSystem world
        EventSystem.getEventState<'a, 'w> key eventSystem

    let getTasklets<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventSystemBy EventSystem.getTasklets world

    let clearTasklets<'w when 'w :> 'w Eventable> (world : 'w) =
        world.UpdateEventSystem EventSystem.clearTasklets

    let restoreTasklets<'w when 'w :> 'w Eventable> (tasklets : 'w Tasklet Queue) (world : 'w) =
        world.UpdateEventSystem (EventSystem.restoreTasklets tasklets)

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet<'w when 'w :> 'w Eventable> tasklet (world : 'w) =
        world.UpdateEventSystem (EventSystem.addTasklet tasklet)

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets<'w when 'w :> 'w Eventable> tasklets (world : 'w) =
        world.UpdateEventSystem (EventSystem.addTasklets tasklets)

    let getAnyEventAddresses eventAddress =
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

    let getSortableSubscriptions
        (getEntityPublishingPriority : Participant -> 'w -> single) (subscriptions : SubscriptionEntry rQueue) (world : 'w) :
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

    let boxSubscription<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable> (subscription : Subscription<'a, 'p, 'w>) =
        let boxableSubscription = fun (evt : obj) world ->
            try subscription (evt :?> Event<'a, 'p>) world
            with
            | :? InvalidCastException ->
                Log.debug ^
                    "If you've reached this exception, then you've probably inadvertantly mixed " +
                    "up an event type parameter for some form of Eventable.publish or subscribe."
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

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
    let publish6<'a, 'p, 'w when 'p :> Participant and 'w :> Eventable<'w>>
        getSubscriptions (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptions publishSorter objEventAddress world
        let publishPlus =
            match world.TryGetPublishEvent () with
            | Some publishPlus -> publishPlus
            | None -> publishEvent<'a, 'p, Participant, 'w>
        let (_, world) =
            List.foldWhile
                (fun (handling, world : 'w) (_, subscriber : Participant, subscription) ->
                    if  (match handling with Cascade -> true | Resolve -> false) &&
                        (match world.GetLiveness () with Running -> true | Exiting -> false) then
                        let publishResult = publishPlus subscriber publisher eventData eventAddress eventTrace subscription world
                        Some publishResult
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish<'a, 'p, 'w when 'p :> Participant and 'w :> Eventable<'w>>
        (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publish6<'a, 'p, 'w> getSubscriptionsSorted3 publishSorter eventData eventAddress eventTrace publisher world

    /// Unsubscribe from an event.
    let unsubscribe<'w when 'w :> Eventable<'w>> subscriptionKey (world : 'w) =
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
                world |> setSubscriptions subscriptions |> setUnsubscriptions unsubscriptions
            | None -> world // TODO: consider an assert fail here?
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) (world : 'w) =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Participant, boxSubscription subscription)
                match Vmap.tryFind objEventAddress subscriptions with
                | Some subscriptionEntries -> Vmap.add objEventAddress (subscriptionEntry :: subscriptionEntries) subscriptions
                | None -> Vmap.add objEventAddress [subscriptionEntry] subscriptions
            let unsubscriptions = Vmap.add subscriptionKey (objEventAddress, subscriber :> Participant) unsubscriptions
            let world = world |> setSubscriptions subscriptions |> setUnsubscriptions unsubscriptions
            (unsubscribe<'w> subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) (world : 'w) =
        subscribePlus5 (Guid.NewGuid ()) subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) (world : 'w) =
        subscribePlus5 subscriptionKey (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) world |> snd

    /// Subscribe to an event.
    let subscribe<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) world =
        subscribe5 (Guid.NewGuid ()) subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a participant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) (world : 'w) =
        let subscriberAddress = subscriber.ParticipantAddress
        if not ^ Address.isEmpty subscriberAddress then
            let monitorKey = Guid.NewGuid ()
            let removalKey = Guid.NewGuid ()
            let world = subscribe5<'a, 'p, 'w> monitorKey subscription eventAddress subscriber world
            let unsubscribe = fun (world : 'w) ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ eventSystem -> (Cascade, unsubscribe eventSystem)
            let removingEventAddress = ftoa<unit> !!(typeof<'p>.Name + "/Removing") ->>- subscriberAddress
            let world = subscribe5<unit, 'p, 'w> removalKey subscription' removingEventAddress subscriber world
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the lifetime of a participant.
    let monitor<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 'p, 'w>) (eventAddress : 'a Address) (subscriber : 'p) (world : 'w) =
        monitorPlus<'a, 'p, 'w> subscription eventAddress subscriber world |> snd