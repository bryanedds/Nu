namespace Prime

type Propagation<'s, 'r> =
    | Propagation of 's * 'r list
    override this.ToString () =
        match this with Propagation (state, recordings) -> "State: " + str state + " Recordings: " + List.joinBy str " " recordings

module Propagate =

    let inline ( >>. ) (propagation : Propagation<'s, 'r>) ((setter : ('s -> 't)), (recorder : 't -> 'r)) =
        match propagation with
        | Propagation (state, recordings) ->
            let state2 = setter state
            Propagation (state2, recorder state2 :: recordings)

    let inline ( >. ) (propagation : Propagation<'s, 's>) (setter : ('s -> 's)) : Propagation<'s, 's> =
        ( >>. ) propagation (setter, id)

    let propagate state =
        Propagation (state, [])