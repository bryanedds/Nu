// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open LanguagePrimitives
open Prime

/// An observation in the functional reactive style.
type [<ReferenceEquality>] Observation<'a, 'o, 'w when 'o :> Participant and 'w :> 'w EventWorld> =
    { Observer : 'o
      Subscribe : 'w -> 'a Address * ('w -> 'w) * 'w }

module Observation =

    /// Make an observation of an event at the given address.
    let [<DebuggerHidden; DebuggerStepThrough>] observe<'a, 'o, 'w when 'o :> Participant and 'w :> 'w EventWorld>
        (eventAddress : 'a Address) (observer : 'o) : Observation<'a, 'o, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observer.ParticipantAddress
            let unsubscribe = fun world -> EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Observation" "observe" evt.Trace
                let world = EventWorld.publish6<'a, Participant, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'product
    /// form', which is defined as a pair of the data of the combined events. Think of it as 'zip'
    /// for event streams.
    /// TODO: unit test for this!
    let [<DebuggerHidden; DebuggerStepThrough>] product
        (eventAddress : 'b Address) (observation : Observation<'a, 'o, 'w>) : Observation<'a * 'b, 'o, 'w> =
        let subscribe = fun world ->

            // init event state, subscription keys and addresses
            let stateKey = makeGuid ()
            let state = (List.empty<'a>, List.empty<'b>)
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = (ntoa<'a * 'b> ^ Name.make ^ scstring subscriptionKey'') ->>- observation.Observer.ParticipantAddress
            
            // unsubscribe from 'a and 'b events, and remove event state
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                let world = EventWorld.unsubscribe<'w> subscriptionKey' world
                EventWorld.removeEventState stateKey world

            // subscription for 'a events
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record4 "Observation" "product" "'a" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let aList = evt.Data :: aList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publish6<'a * 'b, Participant, 'w> EventWorld.sortSubscriptionsNone (a, b) subscriptionAddress'' eventTrace evt.Publisher world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscription for 'b events
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record4 "Observation" "product" "'b" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let bList = evt.Data :: bList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publish6<'a * 'b, Participant, 'w> EventWorld.sortSubscriptionsNone (a, b) subscriptionAddress'' eventTrace evt.Publisher world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscripe 'a and 'b events
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription subscriptionAddress observation.Observer world
            let world = EventWorld.subscribe5<'b, 'o, 'w> subscriptionKey subscription' subscriptionAddress' observation.Observer world
            (subscriptionAddress'', unsubscribe, world)

        // fin
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'sum
    /// form', which is defined as an Either of the data of the combined events, where only data
    /// from the most recent event is available at a time.
    /// TODO: unit test for this!
    let [<DebuggerHidden; DebuggerStepThrough>] sum
        (eventAddress : 'b Address) (observation : Observation<'a, 'o, 'w>) : Observation<Either<'a, 'b>, 'o, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = (ntoa<Either<'a, 'b>> ^ Name.make ^ scstring subscriptionKey'') ->>- observation.Observer.ParticipantAddress
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                EventWorld.unsubscribe<'w> subscriptionKey' world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Observation" "sum" evt.Trace
                let eventData = Left evt.Data
                let world = EventWorld.publish6<Either<'a, 'b>, Participant, 'w> EventWorld.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record "Observation" "sum" evt.Trace
                let eventData = Right evt.Data
                let world = EventWorld.publish6<Either<'a, 'b>, Participant, 'w> EventWorld.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'b, 'o, 'w> subscriptionKey' subscription' subscriptionAddress' observation.Observer world
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription subscriptionAddress observation.Observer world
            (subscriptionAddress'', unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Filter an observation by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filter
        (pred : Event<'a, 'o> -> 'w -> bool) (observation : Observation<'a, 'o, 'w>) =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let world =
                    if pred evt world then
                        let eventTrace = EventTrace.record "Observation" "filter" evt.Trace
                        EventWorld.publish6<'a, Participant, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Map an observation by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map
        (mapper : Event<'a, 'o> -> 'w -> 'b) (observation : Observation<'a, 'o, 'w>) : Observation<'b, 'o, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'b> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Observation" "map" evt.Trace
                let world = EventWorld.publish6<'b, Participant, 'w> EventWorld.sortSubscriptionsNone (mapper evt world) subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track4
        (tracker : 'c -> Event<'a, 'o> -> 'w -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (observation : Observation<'a, 'o, 'w>) : Observation<'b, 'o, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'b> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Observation" "track4" evt.Trace
                        let eventData = transformer state
                        EventWorld.publish6<'b, Participant, 'w> EventWorld.sortSubscriptionsNone eventData subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track2
        (tracker : 'a -> Event<'a, 'o> -> 'w -> 'a * bool) (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey None world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let optState = EventWorld.getEventState stateKey world
                let state = match optState with Some state -> state | None -> evt.Data
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Observation" "track2" evt.Trace
                        EventWorld.publish6<'a, Participant, 'w> EventWorld.sortSubscriptionsNone state subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track
        (tracker : 'b -> 'w -> 'b * bool) (state : 'b) (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        let subscribe = fun world ->
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Observation" "track" evt.Trace
                        EventWorld.publish6<'a, Participant, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribePlus handleEvent observation world =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (address, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'w> subscriptionKey world
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey handleEvent address observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        let observation = { Observer = observation.Observer; Subscribe = subscribe }
        observation.Subscribe world |> _bc

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribe handleEvent observation world =
        subscribePlus handleEvent observation world |> snd

    /// Terminate an observation when an event at the given address is raised.
    let [<DebuggerHidden; DebuggerStepThrough>] until
        (eventAddress : unit Address) (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        let subscribe = fun world ->
            let eventKey = makeGuid ()
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = (ntoa<'a> ^ Name.make ^ scstring subscriptionKey) ->>- observation.Observer.ParticipantAddress
            let (eventAddress', unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = EventWorld.unsubscribe<'w> subscriptionKey world
                EventWorld.unsubscribe<'w> eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = EventWorld.subscribe5 eventKey handleEvent eventAddress observation.Observer world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Observation" "until" evt.Trace
                let world = EventWorld.publish6<'a, Participant, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = EventWorld.subscribe5<'a, 'o, 'w> subscriptionKey subscription eventAddress' observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Terminate an observation when the observer is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] lifetime
        (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        let removingEventAddress = ftoa<unit> !!(typeof<'o>.Name + "/Removing") ->>- observation.Observer.ParticipantAddress
        until removingEventAddress observation

    /// Subscribe to an observation until the observer is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] monitorWithUnsub eventAddress observation world =
        (observation |> lifetime |> subscribePlus eventAddress) world

    /// Subscribe to an observation until the observer is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] monitor eventAddress observation world =
        monitorWithUnsub eventAddress observation world |> snd
    
    (* Advanced Combinators *)

    /// Scan over an observation, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan4 (f : 'b -> Event<'a, 'o> -> 'w -> 'b) g s (observation : Observation<'a, 'o, 'w>) : Observation<'c, 'o, 'w> =
        track4 (fun b a w -> (f b a w, true)) g s observation
        
    /// Scan over an observation, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan2 (f : 'a -> Event<'a, 'o> -> 'w -> 'a) (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        track2 (fun a a2 w -> (f a a2 w, true)) observation
        
    /// Scan over an observation, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan (f : 'b -> Event<'a, 'o> -> 'w -> 'b) s (observation : Observation<'a, 'o, 'w>) : Observation<'b, 'o, 'w> =
        scan4 f id s observation

    /// Transform an observation into a running average of its event's numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline average (observation : Observation<'a, 'o, 'w>) : Observation<'a, 'o, 'w> =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            observation

    /// Transform an observation into a running map from its event's data to keys as defined by 'f'.
    let [<DebuggerHidden; DebuggerStepThrough>] organize f (observation : Observation<'a, 'o, 'w>) : Observation<('a * 'b) option * Map<'b, 'a>, 'o, 'w> =
        scan
            (fun (_, m) a world ->
                let b = f a world
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            observation

    /// Transform an observation into a running set of its event's unique data as defined by 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] groupBy by (observation : Observation<'a, 'o, 'w>) : Observation<'b * bool * 'b Set, 'o, 'w> =
        scan
            (fun (_, _, set) a world ->
                let b = by a world
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            observation

    /// Transform an observation into a running set of its event's unique data.
    let [<DebuggerHidden; DebuggerStepThrough>] group (observation : Observation<'a, 'o, 'w>) : Observation<'a * bool * 'a Set, 'o, 'w> =
        groupBy (fun a _ -> a.Data) observation

    /// Transform an observation into a running sum of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline sumN observation = scan2 (fun n a _ -> n + a.Data) observation

    /// Transform an observation into a running product of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline productN observation = scan2 (fun n a _ -> n * a.Data) observation
    
    /// Transform an observation of pairs into its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] first observation = map (fun a _ -> fst a.Data) observation
    
    /// Transform an observation of pairs into its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] second observation = map (fun a _ -> snd a.Data) observation
    
    /// Transform an observation's pairs by a mapping of its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapFirst mapper observation = map (fun a _ -> (mapper ^ fst a.Data, snd a.Data)) observation
    
    /// Transform an observation of pairs by a mapping of its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapSecond mapper observation = map (fun a _ -> (fst a.Data, mapper ^ snd a.Data)) observation
    
    /// Transform an observation by duplicating its data into pairs.
    let [<DebuggerHidden; DebuggerStepThrough>] duplicate observation = map (fun a _ -> (a.Data, a.Data)) observation
    
    /// Take only the first n events from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] take n observation = track (fun m _ -> (m + 1, m < n)) 0 observation
    
    /// Skip the first n events in an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] skip n observation = track (fun m _ -> (m + 1, m >= n)) 0 observation
    
    /// Take only the first event from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] head observation = take 1 observation
    
    /// Skip the first event of an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] tail observation = skip 1 observation
    
    /// Take only the nth event from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] nth n observation = observation |> skip n |> head
    
    /// Take only the first event from an observation that satisfies 'p'.
    let [<DebuggerHidden; DebuggerStepThrough>] search p observation = observation |> filter p |> head
    
    /// Filter out the None data values from an observation and strip the Some constructor from
    /// the remaining values.
    let [<DebuggerHidden; DebuggerStepThrough>] choose (observation : Observation<'a option, 'o, 'w>) =
        observation |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    
    /// Transform an observation into a running maximum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] max observation = scan2 (fun n a _ -> if n < a.Data then a.Data else n) observation
    
    /// Transform an observation into a running minimum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] min observation = scan2 (fun n a _ -> if a.Data < n then a.Data else n) observation

    /// Filter out the events with non-unique data as defined by 'by' from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] distinctBy by observation = observation |> organize by |> first |> choose
    
    /// Filter out the events with non-unique data from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] distinct observation = distinctBy (fun a -> a.Data) observation

    /// Filter out participant change events that do not relate to those returned by 'valueGetter'.
    let [<DebuggerHidden; DebuggerStepThrough>] participantValue
        (valueGetter : 'w -> 'b) (observation : Observation<ParticipantChangeData<'a, 'w>, 'o, 'w>) =
        filter (fun a world ->
            let oldValue = valueGetter a.Data.OldWorld
            let newValue = valueGetter world
            oldValue <> newValue)
            observation

[<AutoOpen>]
module ObservationOperators =
    open Observation

    /// Pipe-right arrow that provides special precedence for observations.
    let (-|>) = (|>)

    /// Make an observation of the observer's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-- ) (participant : 'a, valueGetter : 'w -> 'b) (observer : 'o) =
        let changeEventAddress = ftoa<ParticipantChangeData<'a, 'w>> !!(typeof<'a>.Name + "/Change") ->>- participant.ParticipantAddress
        observe changeEventAddress observer |> participantValue valueGetter

    /// Propagate the event data of an observation to a value in the observing participant when the
    /// observer exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] (-->) observation valueSetter =
        subscribe (fun a world ->
            let world =
                if world.ContainsParticipant a.Subscriber
                then valueSetter a.Data world
                else world
            (Cascade, world))
            observation

    // Propagate a value from the given source participant to a value in the given destination participant.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-> ) (source : 'a, valueGetter : 'w -> 'b) (destination : 'o, valueSetter : 'b -> 'w -> 'w) =
        (source, valueGetter) *-- destination --> fun _ world ->
            let sourceValue = valueGetter world
            valueSetter sourceValue world