// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open Prime

/// Specifies whether a process is live or dead.
type [<Struct>] Liveness =
   | Live
   | Dead

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type [<Struct>] Handling =
    | Resolve
    | Cascade

/// An entry in the subscription map.
type [<ReferenceEquality>] SubscriptionEntry =
    { SubscriptionCallback : obj
      SubscriptionSubscriber : Simulant
      SubscriptionId : Guid }

/// Abstracts over a subscription sorting procedure.
type SubscriptionSorter =
    (Guid * SubscriptionEntry) seq -> obj -> (Guid * SubscriptionEntry) seq

/// Describes an event subscription that can be boxed / unboxed.
type 'w BoxableSubscription =
    Event<obj, Simulant> -> 'w -> Handling * 'w

/// A map of event subscriptions.
type SubscriptionEntries =
    UMap<obj Address, OMap<Guid, SubscriptionEntry>>

/// A map of subscription keys to unsubscription data.
type UnsubscriptionEntries =
    UMap<Guid, obj Address * Simulant>

[<RequireQualifiedAccess>]
module EventGraph =

    /// OPTIMIZATION: caches event address for fast wildcard address generation.
    let private EventAddressCache = Dictionary<obj, obj> HashIdentity.Structural
    let private EventAddressListCache = Dictionary<obj Address, obj List> HashIdentity.Structural

    /// A functional publisher-neutral mechanism for handling simulation events in Nu.
    /// Publisher-neutrality means that the user can subscribe to events regardless if the event source exists or not.
    /// This decouples subscription lifetime from event source lifetime.
    type [<ReferenceEquality>] EventGraph =
        private
            { // cache line 1 (assuming 16 byte header)
              Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              EventStates : SUMap<Guid, obj>
              EventTracerOpt : (string -> unit) option
              EventFilter : EventFilter
              GlobalSimulantGeneralized : GlobalSimulantGeneralized }

    /// Get the generalized global simulant of the event system.
    let getGlobalSimulantGeneralized eventGraph =
        eventGraph.GlobalSimulantGeneralized

    /// Get event state.
    let getEventState<'a> key (eventGraph : EventGraph) =
        let state = SUMap.find key eventGraph.EventStates
        state :?> 'a

    /// Add event state.
    let addEventState<'a> key (state : 'a) (eventGraph : EventGraph) =
        { eventGraph with EventStates = SUMap.add key (state :> obj) eventGraph.EventStates }

    /// Remove event state.
    let removeEventState key (eventGraph : EventGraph) =
        { eventGraph with EventStates = SUMap.remove key eventGraph.EventStates }

    /// Get subscriptions.
    let getSubscriptions (eventGraph : EventGraph) =
        eventGraph.Subscriptions

    /// Get unsubscriptions.
    let getUnsubscriptions (eventGraph : EventGraph) =
        eventGraph.Unsubscriptions

    /// Set subscriptions.
    let internal setSubscriptions subscriptions (eventGraph : EventGraph) =
        { eventGraph with Subscriptions = subscriptions }

    /// Set unsubscriptions.
    let internal setUnsubscriptions unsubscriptions (eventGraph : EventGraph) =
        { eventGraph with Unsubscriptions = unsubscriptions }

    /// Get how events are being traced.
    let getEventTracerOpt (eventGraph : EventGraph) =
        eventGraph.EventTracerOpt

    /// Set how events are being traced.
    let setEventTracerOpt tracing (eventGraph : EventGraph) =
        { eventGraph with EventTracerOpt = tracing }

    /// Get the state of the event filter.
    let getEventFilter (eventGraph : EventGraph) =
        eventGraph.EventFilter

    /// Set the state of the event filter.
    let setEventFilter filter (eventGraph : EventGraph) =
        { eventGraph with EventFilter = filter }

    /// Remove from the event address cache all addresses belonging to the given target.
    let cleanEventAddressCache (eventTarget : 'a Address) =
        let eventTargetOa = atooa eventTarget
        match EventAddressListCache.TryGetValue eventTargetOa with
        | (true, entries) ->
            for entry in entries do EventAddressCache.Remove entry |> ignore
            EventAddressListCache.Remove eventTargetOa |> ignore
        | (false, _) -> ()

    // NOTE: event addresses are ordered from general to specific. This is so a generalized subscriber can preempt
    // any specific subscribers.
    // OPTIMIZATION: imperative for speed.
    let getEventAddresses1 (eventAddress : 'a Address) =

        // create target event address array
        let eventAddressNames = Address.getNames eventAddress
        let eventAddressNamesLength = eventAddressNames.Length
        match Address.indexOf Constants.Lens.EventName eventAddress with
        | -1 ->

            // make addresses array
            let eventAddresses = Array.zeroCreate (inc eventAddressNamesLength)

            // populate wildcard addresses
            Array.iteri (fun i _ ->
                let eventAddressNamesAny = Array.zeroCreate eventAddressNamesLength
                Array.Copy (eventAddressNames, 0, eventAddressNamesAny, 0, eventAddressNamesLength)
                eventAddressNamesAny.[i] <- Address.WildcardName
                let eventAddressAny = Address.rtoa eventAddressNamesAny
                eventAddresses.[i] <- eventAddressAny)
                eventAddressNames

            // make concrete address the last element
            eventAddresses.[dec eventAddresses.Length] <- eventAddress
            eventAddresses

        // has appropriate Event name...
        | eventAddressEventNameIndex ->

            // make addresses array
            let eventAddressEventNamesLength = inc eventAddressEventNameIndex
            let eventAddressSimulantsIndex = eventAddressEventNamesLength
            let eventAddressSimulantsLength = eventAddressNamesLength - eventAddressSimulantsIndex
            let eventAddresses = Array.zeroCreate (eventAddressNamesLength + eventAddressSimulantsLength + 1)

            // populate wildcard addresses
            for i in 0 .. dec eventAddressNamesLength do
                let eventAddressNames' = Array.zeroCreate eventAddressNamesLength
                Array.Copy (eventAddressNames, 0, eventAddressNames', 0, eventAddressNamesLength)
                eventAddressNames'.[i] <- Address.WildcardName
                let eventAddress' = Address.rtoa eventAddressNames'
                eventAddresses.[i] <- eventAddress'

            // populate ellipsis addresses
            for i in 0 .. dec eventAddressSimulantsLength do
                let j = eventAddressSimulantsIndex + i
                let k = eventAddressNamesLength + i
                let eventAddressNames' = Array.zeroCreate (j + 1)
                Array.Copy (eventAddressNames, 0, eventAddressNames', 0, j)
                eventAddressNames'.[j] <- Address.EllipsisName
                let eventAddress' = Address.rtoa eventAddressNames'
                eventAddresses.[k] <- eventAddress'

            // make concrete address the last element
            eventAddresses.[dec eventAddresses.Length] <- eventAddress
            eventAddresses

    /// Get the wild-carded addresses of an event address.
    let getEventAddresses2 (eventAddress : 'a Address) (_ : EventGraph) =
        match EventAddressCache.TryGetValue eventAddress with
        | (false, _) ->
            let eventAddressNames = Address.getNames eventAddress
            let eventAddresses = getEventAddresses1 eventAddress
            match Array.tryFindIndex (fun name -> name = "Event") eventAddressNames with
            | Some eventIndex ->
                let eventTargetIndex = inc eventIndex
                if eventTargetIndex < Array.length eventAddressNames then
                    let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> Address.makeFromArray
                    match EventAddressListCache.TryGetValue eventTarget with
                    | (false, _) -> EventAddressListCache.Add (eventTarget, List [eventAddress :> obj]) |> ignore
                    | (true, list) -> list.Add eventAddress
                    EventAddressCache.Add (eventAddress, eventAddresses)
                eventAddresses
            | None ->
                failwith
                    ("The event address '" + scstring eventAddress +
                        "' is missing the 'Event' name. All event addresses must separate the event names from the publisher names with 'Event', " +
                        "like 'Click/Event/Button', or 'Mouse/Left/Down/Event' if there is no publisher.")
        | (true, eventAddressesObj) -> eventAddressesObj :?> 'a Address array

    /// Get subscriptions for eventAddress sorted by publishSorter.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress (eventGraph : EventGraph) (world : 'w) =
        let eventSubscriptions = getSubscriptions eventGraph
        let eventAddresses = getEventAddresses2 eventAddress eventGraph
        let subscriptionOpts = Array.map (fun eventAddress -> UMap.tryFind eventAddress eventSubscriptions) eventAddresses
        let subscriptions = subscriptionOpts |> Array.definitize |> Array.map OMap.toSeq |> Seq.concat
        publishSorter subscriptions world

    /// Log an event.
    let logEvent (address : obj Address) (trace : EventTrace) (eventGraph : EventGraph) =
        match eventGraph.EventTracerOpt with
        | Some tracer ->
            let addressStr = scstring address
            let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
            if EventFilter.filter addressStr traceRev eventGraph.EventFilter then tracer (addressStr + "|" + scstring traceRev)
        | None -> ()

    /// Make an event delegate.
    let make eventTracerOpt eventFilter globalSimulantGeneralized config =
        let eventGraph =
            { Subscriptions = UMap.makeEmpty HashIdentity.Structural config
              Unsubscriptions = UMap.makeEmpty HashIdentity.Structural config
              EventStates = SUMap.makeEmpty HashIdentity.Structural config
              EventTracerOpt = eventTracerOpt
              EventFilter = eventFilter
              GlobalSimulantGeneralized = globalSimulantGeneralized }
        eventGraph

    /// Get the subscriptions with the given sorting criteria.
    let getSortableSubscriptions
        (getSortPriority : Simulant -> 'w -> IComparable)
        (subscriptionEntries : (Guid * SubscriptionEntry) seq)
        (world : 'w) =
        Seq.map
            (fun (_, subscription : SubscriptionEntry) ->
                // NOTE: we just take the sort priority of the first callback found when callbacks are compressed. This
                // is semantically sub-optimal, but should be fine for all of our cases.
                let priority = getSortPriority subscription.SubscriptionSubscriber world
                struct (priority, subscription))
            subscriptionEntries

    /// Publish an event.
    let inline publishEvent<'a, 'p, 's, 'w when 'p :> Simulant and 's :> Simulant>
        (subscriber : Simulant) (publisher : 'p) (eventData : obj) (eventAddress : 'a Address) eventTrace (subscription : obj) (world : 'w) =
        let evt = { Data = eventData; Subscriber = subscriber; Publisher = publisher :> Simulant; Address = atooa eventAddress; Trace = eventTrace }
        let callableSubscription = subscription :?> 'w BoxableSubscription
        callableSubscription evt world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : (Guid * SubscriptionEntry) seq) (world : 'w) : seq<Guid * SubscriptionEntry> =
        getSortableSubscriptions by subscriptions world |>
        Array.ofSeq |>
        Array.sortWith (fun (struct ((p : IComparable), _)) (struct ((p2 : IComparable), _)) -> p.CompareTo p2) |>
        Array.map (fun (struct (_, subscription)) -> (subscription.SubscriptionId, subscription)) |>
        Array.toSeq

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry array) (_ : 'w) =
        subscriptions

/// A functional publisher-neutral event mechanism for handling simulation events in Nu.
/// Publisher-neutrality means that the user can subscribe to events regardless if the event source exists or not.
/// This decouples subscription lifetime from event source lifetime.
type EventGraph = EventGraph.EventGraph