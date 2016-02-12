// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open FSharpx.Collections
open Prime

/// Adds the ability for a type to be address.
/// TODO: consider moving to Address.fs.
type Addressable =
    interface
        abstract member GetAddress : unit -> obj Address
        end

/// Specifies whether an application is running or exiting.
/// TODO: maybe move to... somewhere else?
type Liveness =
    | Running
    | Exiting

/// Describes whether an event has been resolved or should cascade to down-stream handlers.
type EventHandling =
    | Resolve
    | Cascade

/// An event used by the game engine's purely-functional event system.
and [<ReferenceEquality>] Event<'a, 's when 's :> Addressable> =
    { Data : 'a
      Address : 'a Address
      Trace : string list
      Publisher : Addressable
      Subscriber : 's }

/// Describes an event subscription.
and Subscription<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable> =
    Event<'a, 's> -> 'w -> EventHandling * 'w

/// Describes an event subscription that can be boxed / unboxed.
and BoxableSubscription<'w when 'w :> 'w Eventable> =
    obj -> 'w -> EventHandling * 'w

/// An entry in the subscription map.
and SubscriptionEntry =
    Guid * Addressable * obj

/// A map of event subscriptions.
and SubscriptionEntries =
    Vmap<obj Address, SubscriptionEntry rQueue>

/// Abstracts over a subscription sorting procedure.
and SubscriptionSorter<'w when 'w :> 'w Eventable> =
    SubscriptionEntry rQueue -> 'w -> SubscriptionEntry rQueue

/// A map of subscription keys to unsubscription data.
and UnsubscriptionEntries =
    Vmap<Guid, obj Address * Addressable>

/// A tasklet to be completed at the given time, with time being accounted for by the world
/// state's TickTime value.
and [<ReferenceEquality>] Tasklet<'w when 'w :> 'w Eventable> =
    { ScheduledTime : int64
      Operation : 'w -> 'w }

/// Event enabler.
and [<ReferenceEquality>] Eventor<'w when 'w :> 'w Eventable> =
    private
        { Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          Tasklets : 'w Tasklet Queue
          CallbackStates : Vmap<Guid, obj> }

/// Adds the capability to use purely-functional events with the given type 'w.
and Eventable<'w when 'w :> 'w Eventable> =
    interface
        abstract member GetLiveness : unit -> Liveness
        abstract member GetEventor : unit -> 'w Eventor
        abstract member UpdateEventor : ('w Eventor -> 'w Eventor) -> 'w
        end

[<RequireQualifiedAccess>]
module Events =

    /// Represents any event.
    let Any = ntoa<obj> !!"*"

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Eventor =

    /// Get all tasklets.
    let getTasklets eventor =
        eventor.Tasklets

    /// Clear all held tasklets.
    let clearTasklets eventor =
        { eventor with Tasklets = Queue.empty }

    /// Restore previously-held tasklets.
    let restoreTasklets<'w when 'w :> 'w Eventable> (tasklets : 'w Tasklet Queue) eventor =
        { eventor with Tasklets = Queue.ofSeq ^ Seq.append (eventor.Tasklets :> 'w Tasklet seq) (tasklets :> 'w Tasklet seq) }

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet eventor =
        { eventor with Tasklets = Queue.conj tasklet eventor.Tasklets }

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets eventor =
        { eventor with Tasklets = Queue.ofSeq ^ Seq.append (tasklets :> 'w Tasklet seq) (eventor.Tasklets :> 'w Tasklet seq) }

    /// Add callback state.
    let addCallbackState key state eventor =
        { eventor with CallbackStates = Vmap.add key (state :> obj) eventor.CallbackStates }

    /// Remove callback state.
    let removeCallbackState key eventor =
        { eventor with CallbackStates = Vmap.remove key eventor.CallbackStates }

    /// Get subscriptions.
    let getSubscriptions eventor =
        eventor.Subscriptions

    /// Get unsubscriptions.
    let getUnsubscriptions eventor =
        eventor.Unsubscriptions

    /// Set subscriptions.
    let internal setSubscriptions subscriptions eventor =
        { eventor with Subscriptions = subscriptions }

    /// Set unsubscriptions.
    let internal setUnsubscriptions unsubscriptions eventor =
        { eventor with Unsubscriptions = unsubscriptions }

    /// Get callback state.
    let getCallbackState<'a, 'w when 'w :> 'w Eventable> key (eventor : 'w Eventor) =
        let state = Vmap.find key eventor.CallbackStates
        state :?> 'a

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Eventable =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> (HashIdentity.FromFunctions Address<obj>.hash Address<obj>.equals)

    let private sortFstDesc (priority : single, _) (priority2 : single, _) =
        // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0

    let getEventor<'w when 'w :> 'w Eventable> (world : 'w) =
        world.GetEventor ()

    let getEventorBy<'a, 'w when 'w :> 'w Eventable> (by : Eventor<'w> -> 'a) (world : 'w) : 'a =
        let eventSystem = world.GetEventor ()
        by eventSystem

    let updateEventor<'w when 'w :> 'w Eventable> updater (world : 'w) =
        world.UpdateEventor updater

    let getTasklets<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventorBy Eventor.getTasklets world

    let clearTasklets<'w when 'w :> 'w Eventable> (world : 'w) =
        world.UpdateEventor Eventor.clearTasklets

    let restoreTasklets<'w when 'w :> 'w Eventable> (tasklets : 'w Tasklet Queue) (world : 'w) =
        world.UpdateEventor (Eventor.restoreTasklets tasklets)

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet<'w when 'w :> 'w Eventable> tasklet (world : 'w) =
        world.UpdateEventor (Eventor.addTasklet tasklet)

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets<'w when 'w :> 'w Eventable> tasklets (world : 'w) =
        world.UpdateEventor (Eventor.addTasklets tasklets)

    /// Get callback subscriptions.
    let getSubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventorBy Eventor.getSubscriptions world

    /// Get callback unsubscriptions.
    let getUnsubscriptions<'w when 'w :> 'w Eventable> (world : 'w) =
        getEventorBy Eventor.getUnsubscriptions world

    /// Set callback subscriptions.
    let internal setSubscriptions<'w when 'w :> 'w Eventable> subscriptions (world : 'w) =
        world.UpdateEventor (Eventor.setSubscriptions subscriptions)

    /// Set callback unsubscriptions.
    let internal setUnsubscriptions<'w when 'w :> 'w Eventable> unsubscriptions (world : 'w) =
        world.UpdateEventor (Eventor.setUnsubscriptions unsubscriptions)

    /// Add callback state to the world.
    let addCallbackState<'w when 'w :> 'w Eventable> key state (world : 'w) =
        world.UpdateEventor (Eventor.addCallbackState key state)

    /// Remove callback state from the world.
    let removeCallbackState<'w when 'w :> 'w Eventable> key (world : 'w) =
        world.UpdateEventor (Eventor.removeCallbackState key)

    /// Get callback state from the world.
    let getCallbackState<'a, 'w when 'w :> 'w Eventable> key (world : 'w) : 'a =
        let eventor = getEventor world
        Eventor.getCallbackState<'a, 'w> key eventor

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
        (getParticipantPublishingPriority : Addressable -> 'w -> single)
        (subscriptions : SubscriptionEntry rQueue)
        (world : 'w) :
        (single * SubscriptionEntry) list =
        List.foldBack
            (fun (key, participant : Addressable, subscription) subscriptions ->
                let priority = getParticipantPublishingPriority participant world
                let subscription = (priority, (key, participant, subscription))
                subscription :: subscriptions)
            subscriptions
            []

    /// TODO: document.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter<'w>) eventAddress (world : 'w) =
        let eventor = getEventor world
        let subscriptions = Eventor.getSubscriptions eventor
        match Vmap.tryFind eventAddress subscriptions with
        | Some subList -> publishSorter subList world
        | None -> []

    /// TODO: document.
    let getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<'w>) eventAddress (world : 'w) =
        let eventor = getEventor world
        let subscriptions = Eventor.getSubscriptions eventor
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Vmap.tryFind anyEventAddress subscriptions) anyEventAddresses
        let optSubLists = Vmap.tryFind eventAddress subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        publishSorter subList world

    let boxSubscription<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable> (subscription : Subscription<'a, 's, 'w>) =
        let boxableSubscription = fun (evt : obj) world ->
            try subscription (evt :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let publishEvent<'a, 'p, 's, 'w when 'p :> Addressable and 's :> Addressable and 'w :> 'w Eventable>
        (subscriber : obj) (publisher : 'p) (eventData : 'a) (eventAddress : 'a Address) eventTrace subscription (world : 'w) =
        let evt =
            { Data = eventData
              Address = eventAddress
              Trace = eventTrace
              Subscriber = subscriber :?> 's
              Publisher = publisher :> Addressable }
        let callableSubscription = unbox<BoxableSubscription<'w>> subscription
        let result = callableSubscription evt world
        Some result

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : 'w) =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    /// Sort subscriptions by their place in the world's participant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> 0.125f) // TODO: remove hard coding
            subscriptions
            world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : 'w) =
        subscriptions

    /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
    let publish6<'a, 'p, 'w when 'p :> Addressable and 'w :> Eventable<'w>>
        getSubscriptions (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptions publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world : 'w) (_, subscriber : Addressable, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.GetLiveness () with Running -> true | Exiting -> false) then
                        publishEvent<'a, 'p, Addressable, 'w> subscriber publisher eventData eventAddress eventTrace subscription world
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish5<'a, 'p, 'w when 'p :> Addressable and 'w :> Eventable<'w>>
        (publishSorter : SubscriptionSorter<'w>) (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publish6<'a, 'p, 'w> getSubscriptionsSorted3 publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event.
    let publish<'a, 'p, 'w when 'p :> Addressable and 'w :> Eventable<'w>>
        (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) (world : 'w) =
        publish5<'a, 'p, 'w> sortSubscriptionsByHierarchy eventData eventAddress eventTrace publisher world

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
    let subscribePlus5<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let (subscriptions, unsubscriptions) = (getSubscriptions world, getUnsubscriptions world)
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Addressable, boxSubscription subscription)
                match Vmap.tryFind objEventAddress subscriptions with
                | Some subscriptionEntries -> Vmap.add objEventAddress (subscriptionEntry :: subscriptionEntries) subscriptions
                | None -> Vmap.add objEventAddress [subscriptionEntry] subscriptions
            let unsubscriptions = Vmap.add subscriptionKey (objEventAddress, subscriber :> Addressable) unsubscriptions
            let world = world |> setSubscriptions subscriptions |> setUnsubscriptions unsubscriptions
            (unsubscribe<'w> subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        subscribePlus5 (Guid.NewGuid ()) subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        subscribePlus5 subscriptionKey (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) world |> snd

    /// Subscribe to an event.
    let subscribe<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribe5 (Guid.NewGuid ()) subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a participant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        let subscriberAddress = subscriber.GetAddress ()
        if not ^ Address.isEmpty subscriberAddress then
            let monitorKey = Guid.NewGuid ()
            let removalKey = Guid.NewGuid ()
            let world = subscribe5<'a, 's, 'w> monitorKey subscription eventAddress subscriber world
            let unsubscribe = fun (world : 'w) ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ eventor -> (Cascade, unsubscribe eventor)
            let removingEventAddress = ftoa<unit> !!(typeof<'s>.Name + "/Removing") ->>- subscriberAddress
            let world = subscribe5<unit, 's, 'w> removalKey subscription' removingEventAddress subscriber world
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the lifetime of a participant.
    let monitor<'a, 's, 'w when 's :> Addressable and 'w :> 'w Eventable>
        (subscription : Subscription<'a, 's, 'w>) (eventAddress : 'a Address) (subscriber : 's) (world : 'w) =
        monitorPlus<'a, 's, 'w> subscription eventAddress subscriber world |> snd

    /// Make an event system.
    let make () =
        { Subscriptions = Vmap.makeEmpty ()
          Unsubscriptions = Vmap.makeEmpty ()
          Tasklets = Queue.empty
          CallbackStates = Vmap.makeEmpty () }