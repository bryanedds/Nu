// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2014.

namespace Prime
open Prime

[<AutoOpen>]
module PropagateModule =

    /// Records value changes to a list.
    /// Built for use as state-sychronization primitive, like in game networking.
    type Propagation<'s, 'r> =
        | Propagation of 's * 'r list
        override this.ToString () =
            match this with
            | Propagation (state, recordings) -> "State: " + string state + " Recordings: " + List.joinBy string " " recordings

module Propagate =

    /// Propagate a value change with recording.
    let inline (>>.) (propagation : Propagation<'s, 'r>) ((setter : ('s -> 't)), (recorder : 't -> 'r)) =
        match propagation with
        | Propagation (state, recordings) ->
            let state' = setter state
            Propagation (state', recorder state' :: recordings)

    /// Propagate a value change without recording.
    let inline (>.) (propagation : Propagation<'s, 's>) (setter : ('s -> 's)) : Propagation<'s, 's> =
        (>>.) propagation (setter, id)

    /// Propagator with an initial state.
    let propagate state =
        Propagation (state, [])