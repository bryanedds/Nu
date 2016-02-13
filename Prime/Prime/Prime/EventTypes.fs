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
/// NOTE: would better have been name 'Participant', but not done so due to legacy constraints.
type Simulant =
    interface
        abstract member SimulantAddress : Simulant Address
        abstract member GetPublishingPriority : (Simulant -> 'w -> single) -> 'w -> single
        end

/// Operators for the Simulant type.
/// NOTE: would better have been name 'ParticipantOperators', but not done so due to legacy constraints.
type SimulantOperators =
    private
        | SimulantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (simulant : Simulant) = acatf address (atooa simulant.SimulantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, simulant : Simulant) = SimulantOperators.acatf address simulant

/// The data for a world state change event.
type [<StructuralEquality; NoComparison>] WorldStateChangeData<'w when 'w :> 'w Eventable> =
    { OldWorld : 'w }

/// The data for an simulant change event.
and [<StructuralEquality; NoComparison>] SimulantChangeData<'s, 'w when 's :> Simulant and 'w :> 'w Eventable> =
    { Simulant : 's
      OldWorld : 'w }

/// An event used by the event system.
and [<ReferenceEquality>] Event<'a, 's when 's :> Simulant> =
    { Data : 'a
      Address : 'a Address
      Trace : string list
      Publisher : Simulant
      Subscriber : 's }

/// Describes a means to publish an event.
and PublishEvent<'a, 's, 'w when 's :> Simulant and 'w :> 'w Eventable> =
    Simulant -> 's -> 'a -> 'a Address -> string list -> obj -> 'w -> Handling * 'w

/// Describes an event subscription.
and Subscription<'a, 's, 'w when 's :> Simulant and 'w :> 'w Eventable> =
    Event<'a, 's> -> 'w -> Handling * 'w

/// An entry in the subscription map.
and SubscriptionEntry =
    Guid * Simulant * obj

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
    Vmap<Guid, obj Address * Simulant>

/// A tasklet to be completed at the given time, with time being accounted for by the world
/// state's TickTime value.
and [<ReferenceEquality>] Tasklet<'w when 'w :> 'w Eventable> =
    { ScheduledTime : int64
      Operation : 'w -> 'w }

/// A publisher-neutral, purely functional event system.
and [<ReferenceEquality>] EventSystem<'w when 'w :> 'w Eventable> =
    private
        { Subscriptions : SubscriptionEntries
          Unsubscriptions : UnsubscriptionEntries
          EventStates : Vmap<Guid, obj>
          Tasklets : 'w Tasklet Queue }

/// Adds the capability to use purely-functional events with the given type 'w.
and Eventable<'w when 'w :> 'w Eventable> =
    interface
        abstract member GetLiveness : unit -> Liveness
        abstract member GetUpdateCount : unit -> int64
        abstract member GetEventSystem : unit -> 'w EventSystem
        abstract member UpdateEventSystem : ('w EventSystem -> 'w EventSystem) -> 'w
        abstract member TryGetPublishEvent : unit -> PublishEvent<'a, 's, 'w> option
        abstract member ContainsSimulant : Simulant -> bool
        end