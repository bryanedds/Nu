// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open Prime

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type Handling =
    | Resolve
    | Cascade

/// Specifies whether an event-based application is running or exiting.
type Liveness =
    | Running
    | Exiting

/// An event used by the event system.
type [<ReferenceEquality>] Event<'a, 's when 's :> Participant> =
    { Data : 'a
      DataType : Type // here so type can be recovered in a dynamic context
      Address : 'a Address
      Trace : EventTrace
      Publisher : Participant
      Subscriber : 's }

/// Describes an event subscription.
type Subscription<'a, 's, 'w when 's :> Participant> =
    Event<'a, 's> -> 'w -> Handling * 'w

/// An entry in the subscription map.
type SubscriptionEntry =
    Guid * Participant * obj

/// Abstracts over a subscription sorting procedure.
/// TODO: for efficiency, consider using Ulist rather than list.
type 'w SubscriptionSorter =
    SubscriptionEntry list -> 'w -> SubscriptionEntry list

/// Describes an event subscription that can be boxed / unboxed.
type internal 'w BoxableSubscription =
    obj -> 'w -> Handling * 'w

/// A map of event subscriptions.
/// TODO: for efficiency, consider using Ulist rather than list.
type internal SubscriptionEntries =
    Umap<obj Address, SubscriptionEntry list>

/// A map of subscription keys to unsubscription data.
type internal UnsubscriptionEntries =
    Umap<Guid, obj Address * Participant>

module Events =

    /// Represents a wildcard in an event.
    let Wildcard = ntoa<obj> !!"@"

[<AutoOpen>]
module EventSystemModule =

    /// A publisher-neutral, purely functional event system.
    type [<ReferenceEquality>] 'w EventSystem =
        private
            { Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              EventContext : obj Address
              EventStates : Umap<Guid, obj>
              EventTracer : string -> unit
              EventTracing : bool
              EventFilter : EventFilter.Filter
              EventAddresses : obj Address list }

    [<RequireQualifiedAccess>]
    module EventSystem =

        /// Add event state.
        let addEventState<'a, 'w> key (state : 'a) (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = Umap.add key (state :> obj) eventSystem.EventStates }

        /// Remove event state.
        let removeEventState<'w> key (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = Umap.remove key eventSystem.EventStates }

        /// Get subscriptions.
        let getSubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Subscriptions

        /// Get unsubscriptions.
        let getUnsubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Unsubscriptions

        /// Set subscriptions.
        let internal setSubscriptions<'w> subscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Subscriptions = subscriptions }

        /// Set unsubscriptions.
        let internal setUnsubscriptions<'w> unsubscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Unsubscriptions = unsubscriptions }

        /// Get event state.
        let getEventState<'a, 'w> key (eventSystem : 'w EventSystem) =
            let state = Umap.find key eventSystem.EventStates
            state :?> 'a

        /// Get whether events are being traced.
        let getEventTracing<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventTracing

        /// Set whether events are being traced.
        let setEventTracing<'w> tracing (eventSystem : 'w EventSystem) =
            { eventSystem with EventTracing = tracing }

        /// Get the state of the event filter.
        let getEventFilter<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventFilter

        /// Set the state of the event filter.
        let setEventFilter<'w> filter (eventSystem : 'w EventSystem) =
            { eventSystem with EventFilter = filter }

        /// Get the context of the event system.
        let getEventContext (eventSystem : 'w EventSystem) =
            eventSystem.EventContext

        /// Qualify the event context of the world.
        let qualifyEventContext (address : obj Address) (eventSystem : 'w EventSystem) =
            let context = getEventContext eventSystem
            let contextLength = Address.length context
            let addressLength = Address.length address
            if contextLength = addressLength then
                Address.tryTake (contextLength - 1) context =
                    Address.tryTake (addressLength - 1) address
            elif contextLength < addressLength then
                context = Address.take contextLength address
            elif contextLength > addressLength then
                address = Address.take addressLength context
            else false

        /// Set the context of the event system.
        let setEventContext context (eventSystem : 'w EventSystem) =
            { eventSystem with EventContext = context }

        /// Log an event.
        let logEvent<'w> (address : obj Address) (trace : EventTrace) (eventSystem : 'w EventSystem) =
            if eventSystem.EventTracing then
                let addressStr = scstring address
                let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
                if EventFilter.filter addressStr traceRev eventSystem.EventFilter then
                    eventSystem.EventTracer ^ addressStr + "|" + scstring traceRev

        /// Push an event address to the list for cycle-detection.
        let pushEventAddress<'w> eventAddress (eventSystem : 'w EventSystem) =
            { eventSystem with EventAddresses = eventAddress :: eventSystem.EventAddresses }
            
        /// Pop an event address to the list for cycle-detection.
        let popEventAddress<'w> (eventSystem : 'w EventSystem) =
            { eventSystem with EventAddresses = List.tail eventSystem.EventAddresses }
            
        /// Get the current event address list for cycle-detection.
        let getEventAddresses<'w> (eventSystem : 'w EventSystem) =
            eventSystem.EventAddresses

        /// Make an event system.
        let make eventTracer eventTracing eventFilter =
            { Subscriptions = Umap.makeEmpty None
              Unsubscriptions = Umap.makeEmpty None
              EventContext = Address.empty
              EventStates = Umap.makeEmpty None
              EventTracer = eventTracer
              EventTracing = eventTracing
              EventFilter = eventFilter
              EventAddresses = [] }

/// A publisher-neutral, purely functional event system.
type 'w EventSystem = 'w EventSystemModule.EventSystem