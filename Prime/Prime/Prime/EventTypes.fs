// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections.Generic
open FSharpx.Collections
open Prime

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type Handling =
    | Resolve
    | Cascade

/// Specifies whether an event-based application is running or exiting.
type Liveness =
    | Running
    | Exiting

/// A participant in the event system.
type Participant =
    interface
        abstract member ParticipantAddress : Participant Address
        abstract member GetPublishingPriority : (Participant -> 'w -> single) -> 'w -> single
        end

/// Operators for the Participant type.
type ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

/// The data for a change in a participant.
type [<StructuralEquality; NoComparison>] ParticipantChangeData<'p, 'w when 'p :> Participant and 'w :> 'w Eventable> =
    { Participant : 'p
      OldWorld : 'w }

/// An event used by the event system.
and [<ReferenceEquality>] Event<'a, 'p when 'p :> Participant> =
    { Data : 'a
      Address : 'a Address
      Trace : string list
      Publisher : Participant
      Subscriber : 'p }

/// Describes a means to publish an event.
and PublishEvent<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable> =
    Participant -> 'p -> 'a -> 'a Address -> string list -> obj -> 'w -> Handling * 'w

/// Describes an event subscription.
and Subscription<'a, 'p, 'w when 'p :> Participant and 'w :> 'w Eventable> =
    Event<'a, 'p> -> 'w -> Handling * 'w

/// An entry in the subscription map.
and SubscriptionEntry =
    Guid * Participant * obj

/// Abstracts over a subscription sorting procedure.
and SubscriptionSorter<'w when 'w :> 'w Eventable> =
    SubscriptionEntry rQueue -> 'w -> SubscriptionEntry rQueue

/// Describes an event subscription that can be boxed / unboxed.
and internal BoxableSubscription<'w when 'w :> 'w Eventable> =
    obj -> 'w -> Handling * 'w

/// A map of event subscriptions.
and internal SubscriptionEntries =
    Vmap<obj Address, SubscriptionEntry rQueue>

/// A map of subscription keys to unsubscription data.
and internal UnsubscriptionEntries =
    Vmap<Guid, obj Address * Participant>

/// A publisher-neutral, purely functional event system.
and [<ReferenceEquality>] EventSystem<'w when 'w :> 'w Eventable> =
    private
        { Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          EventStates : Vmap<Guid, obj> }

/// Adds the capability to use purely-functional events with the given type 'w.
and Eventable<'w when 'w :> 'w Eventable> =
    interface
        abstract member GetLiveness : unit -> Liveness
        abstract member GetEventSystem : unit -> 'w EventSystem
        abstract member UpdateEventSystem : ('w EventSystem -> 'w EventSystem) -> 'w
        abstract member GetCustomEventPublisher : unit -> PublishEvent<'a, 'p, 'w> option
        abstract member ContainsParticipant : Participant -> bool
        end