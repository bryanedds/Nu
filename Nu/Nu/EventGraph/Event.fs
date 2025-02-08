﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

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

[<RequireQualifiedAccess>]
module Event =

    /// Make an event value.
    let make<'a, 's when 's :> Simulant> (data : 'a) (subscriber : 's) publisher (address : 'a Address) trace =
        { Data = data
          Subscriber = subscriber
          Publisher = publisher
          Address = address
          Trace = trace }

    /// Specialize an event.
    let specialize<'a, 's when 's :> Simulant> (evt : Event) : Event<'a, 's> =
        { Data = evt.Data :?> 'a
          Subscriber = evt.Subscriber :?> 's
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

    /// Generalize an event.
    let generalize (evt : Event<'a, 's>) : Event =
        { Data = evt.Data :> obj
          Subscriber = evt.Subscriber
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }