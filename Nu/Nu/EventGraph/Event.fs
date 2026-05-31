// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System

/// The generalized event type (can be used to handle any event).
type Event = Event<obj, Simulant>

/// An event used by the event system.
and Event<'a, 's when 's :> Simulant> =
    { Data : 'a
      Subscriber : 's
      Publisher : Simulant
      Address : 'a Address
      Trace : EventTrace }

    /// Make an event value.
    static member make<'t, 'x when 'x :> Simulant> (data : 't) (subscriber : 'x) publisher (address : 't Address) trace =
        { Data = data
          Subscriber = subscriber
          Publisher = publisher
          Address = address
          Trace = trace }

    /// Specialize an event.
    static member specialize<'t, 'x when 'x :> Simulant> (evt : Event) : Event<'t, 'x> =
        { Data = evt.Data :?> 't
          Subscriber = evt.Subscriber :?> 'x
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

    /// Generalize an event.
    static member generalize (evt : Event<'t, 'x>) : Event =
        { Data = evt.Data :> obj
          Subscriber = evt.Subscriber
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }