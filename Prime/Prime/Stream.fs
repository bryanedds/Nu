// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics
open Prime

/// A stream in the functional reactive style.
type [<ReferenceEquality>] Stream<'a, 'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>> =
    { Subscribe : 'w -> 'a Address * ('w -> 'w) * 'w }

module Stream =

    (* Event-Based Combinators *)

    /// Make a stream of an event at the given address.
    let [<DebuggerHidden; DebuggerStepThrough>] stream<'a, 'g, 'w when 'g :> Participant and 'w :> EventWorld<'g, 'w>>
        (eventAddress : 'a Address) : Stream<'a, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let unsubscribe = fun world -> EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "stream" evt.Trace
                let world = EventWorld.publishPlus<'a, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace globalParticipant false world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent4
        (tracker : 'c -> Event<'a, 'g> -> 'w -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (stream : Stream<'a, 'g, 'w>) :
        Stream<'b, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'b> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "trackEvent4" evt.Trace
                        let eventData = transformer state
                        EventWorld.publishPlus<'b, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone eventData subscriptionAddress eventTrace globalParticipant false world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent2
        (tracker : 'a -> Event<'a, 'g> -> 'w -> 'a * bool)
        (stream : Stream<'a, 'g, 'w>) :
        Stream<'a, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey None world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let stateOpt = EventWorld.getEventState stateKey world
                let state = match stateOpt with Some state -> state | None -> evt.Data
                let (state, tracked) = tracker state evt world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "trackEvent2" evt.Trace
                        EventWorld.publishPlus<'a, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone state subscriptionAddress eventTrace globalParticipant false world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent
        (tracker : 'b -> 'w -> 'b * bool) (state : 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let stateKey = makeGuid ()
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = EventWorld.removeEventState stateKey world
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let state = EventWorld.getEventState stateKey world
                let (state, tracked) = tracker state world
                let world = EventWorld.addEventState stateKey state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "trackEvent" evt.Trace
                        EventWorld.publishPlus<'a, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace globalParticipant false world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldMapEvent (f : 'b -> Event<'a, 'g> -> 'w -> 'b) g s (stream : Stream<'a, 'g, 'w>) : Stream<'c, 'g, 'w> =
        trackEvent4 (fun b a w -> (f b a w, true)) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldEvent (f : 'b -> Event<'a, 'g> -> 'w -> 'b) s (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        trackEvent4 (fun b a w -> (f b a w, true)) id s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduceEvent (f : 'a -> Event<'a, 'g> -> 'w -> 'a) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        trackEvent2 (fun a a2 w -> (f a a2 w, true)) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filterEvent
        (pred : Event<'a, 'g> -> 'w -> bool) (stream : Stream<'a, 'g, 'w>) =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let world =
                    if pred evt world then
                        let eventTrace = EventTrace.record "Stream" "filterEvent" evt.Trace
                        EventWorld.publishPlus<'a, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace globalParticipant false world
                    else world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] mapEvent
        (mapper : Event<'a, 'g> -> 'w -> 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'b> !!(scstring subscriptionKey)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "mapEvent" evt.Trace
                let world = EventWorld.publishPlus<'b, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone (mapper evt world) subscriptionAddress eventTrace globalParticipant false world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription eventAddress globalParticipant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    (* World-Accessing Combinators *)

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld4
        (tracker : 'c -> 'a -> 'w -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        trackEvent4 (fun c evt world -> tracker c evt.Data world) transformer state stream

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld2
        (tracker : 'a -> 'a -> 'w -> 'a * bool) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        trackEvent2 (fun a evt world -> tracker a evt.Data world) stream

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld
        (tracker : 'b -> 'w -> 'b * bool) (state : 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        trackEvent tracker state stream

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldMapWorld (f : 'b -> 'a -> 'w -> 'b) g s (stream : Stream<'a, 'g, 'w>) : Stream<'c, 'g, 'w> =
        foldMapEvent (fun b evt world -> f b evt.Data world) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldWorld (f : 'b -> 'a -> 'w -> 'b) s (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        foldEvent (fun b evt world -> f b evt.Data world) s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduceWorld (f : 'a -> 'a -> 'w -> 'a) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        reduceEvent (fun a evt world -> f a evt.Data world) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filterWorld (pred : 'a -> 'w -> bool) (stream : Stream<'a, 'g, 'w>) =
        filterEvent (fun evt world -> pred evt.Data world) stream

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] mapWorld (mapper : 'a -> 'w -> 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        mapEvent (fun evt world -> mapper evt.Data world) stream

    (* Primitive Combinators *)

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track4
        (tracker : 'c -> 'a -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        trackEvent4 (fun c evt _ -> tracker c evt.Data) transformer state stream

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track2
        (tracker : 'a -> 'a -> 'a * bool) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        trackEvent2 (fun a evt _ -> tracker a evt.Data) stream

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track
        (tracker : 'b -> 'b * bool) (state : 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        trackEvent (fun b _ -> tracker b) state stream

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldMap (f : 'b -> 'a -> 'b) g s (stream : Stream<'a, 'g, 'w>) : Stream<'c, 'g, 'w> =
        foldMapEvent (fun b evt _ -> f b evt.Data) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] fold (f : 'b -> 'a -> 'b) s (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        foldEvent (fun b evt _ -> f b evt.Data) s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduce (f : 'a -> 'a -> 'a) (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        reduceEvent (fun a evt _ -> f a evt.Data) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filter (pred : 'a -> bool) (stream : Stream<'a, 'g, 'w>) =
        filterEvent (fun evt _ -> pred evt.Data) stream

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map (mapper : 'a -> 'b) (stream : Stream<'a, 'g, 'w>) : Stream<'b, 'g, 'w> =
        mapEvent (fun evt _ -> mapper evt.Data) stream

    /// Combine two streams. Combination is in 'product form', which is defined as a pair of the data of the combined
    /// events. Think of it as 'zip' for event streams.
    let [<DebuggerHidden; DebuggerStepThrough>] product
        (stream : Stream<'a, 'g, 'w>) (stream' : Stream<'b, 'g, 'w>) : Stream<'a * 'b, 'g, 'w> =
        let subscribe = fun (world : 'w) ->

            // initialize event state, subscription keys and addresses
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let stateKey = makeGuid ()
            let state = (List.empty<'a>, List.empty<'b>)
            let world = EventWorld.addEventState stateKey state world
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream'.Subscribe world
            let subscriptionAddress'' = ntoa<'a * 'b> !!(scstring subscriptionKey'')
            
            // unsubscribe from 'a and 'b events, and remove event state
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = EventWorld.unsubscribe<'g, 'w> subscriptionKey world
                let world = EventWorld.unsubscribe<'g, 'w> subscriptionKey' world
                EventWorld.removeEventState stateKey world

            // subscription for 'a events
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record4 "Stream" "product" "'a" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let aList = evt.Data :: aList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publishPlus<'a * 'b, 'g, 'g, _> EventWorld.sortSubscriptionsNone (a, b) subscriptionAddress'' eventTrace globalParticipant false world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscription for 'b events
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record4 "Stream" "product" "'b" evt.Trace
                let (aList : 'a list, bList : 'b list) = EventWorld.getEventState stateKey world
                let bList = evt.Data :: bList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let world = EventWorld.publishPlus<'a * 'b, 'g, 'g, _> EventWorld.sortSubscriptionsNone (a, b) subscriptionAddress'' eventTrace globalParticipant false world
                        (state, world)
                    | state -> (state, world)
                let world = EventWorld.addEventState stateKey state world
                (Cascade, world)

            // subscripe 'a and 'b events
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription subscriptionAddress globalParticipant world |> snd
            let world = EventWorld.subscribePlus<'b, 'g, 'g, 'w> subscriptionKey subscription' subscriptionAddress' globalParticipant world |> snd
            (subscriptionAddress'', unsubscribe, world)

        // fin
        { Subscribe = subscribe }

    /// Combine two streams. Combination is in 'sum form', which is defined as an Either of the data of the combined
    /// events, where only data from the most recent event is available at a time.
    let [<DebuggerHidden; DebuggerStepThrough>] sum
        (stream : Stream<'a, 'g, 'w>) (stream' : Stream<'b, 'g, 'w>) : Stream<Either<'a, 'b>, 'g, 'w> =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream'.Subscribe world
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> !!(scstring subscriptionKey'')
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = EventWorld.unsubscribe<'g, 'w> subscriptionKey world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey' world
            let subscription = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "sum" evt.Trace
                let eventData = Left evt.Data
                let world = EventWorld.publishPlus<Either<'a, 'b>, 'g, 'g, _> EventWorld.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace globalParticipant false world
                (Cascade, world)
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "sum" evt.Trace
                let eventData = Right evt.Data
                let world = EventWorld.publishPlus<Either<'a, 'b>, 'g, 'g, _> EventWorld.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace globalParticipant false world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'b, 'g, 'g, 'w> subscriptionKey' subscription' subscriptionAddress' globalParticipant world |> snd
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey subscription subscriptionAddress globalParticipant world |> snd
            (subscriptionAddress'', unsubscribe, world)
        { Subscribe = subscribe }

    /// Terminate a stream when a given stream receives a value.
    let [<DebuggerHidden; DebuggerStepThrough>] until
        (stream : Stream<'b, 'g, 'w>) (stream' : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        let subscribe = fun (world : 'w) ->
            let globalParticipant = world.GetEventSystem () |> EventSystem.getGlobalPariticipant :?> 'g
            let subscriptionKey = makeGuid ()
            let subscriptionKey' = makeGuid ()
            let subscriptionKey'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream'.Subscribe world
            let subscriptionAddress'' = ntoa<'a> !!(scstring subscriptionKey'')
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = EventWorld.unsubscribe<'g, 'w> subscriptionKey' world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let subscription = fun _ world ->
                let world = unsubscribe world
                (Cascade, world)
            let subscription' = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "until" evt.Trace
                let world = EventWorld.publishPlus<'a, 'g, 'g, 'w> EventWorld.sortSubscriptionsNone evt.Data subscriptionAddress'' eventTrace globalParticipant false world
                (Cascade, world)
            let world = EventWorld.subscribePlus<'a, 'g, 'g, 'w> subscriptionKey' subscription' subscriptionAddress' globalParticipant world |> snd
            let world = EventWorld.subscribePlus<'b, 'g, 'g, 'w> subscriptionKey subscription subscriptionAddress globalParticipant world |> snd
            (subscriptionAddress'', unsubscribe, world)
        { Subscribe = subscribe }

    /// Terminate a stream when the subscriber is unregistered from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] lifetime<'s, 'a, 'g, 'w when 's :> Participant and 'w :> EventWorld<'g, 'w>>
        (subscriber : 's) (stream_ : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        let removingEventAddress = ltoa<unit> [!!typeof<'s>.Name; !!"Unregistering"; !!"Event"] ->>- subscriber.ParticipantAddress
        let removingStream = stream removingEventAddress
        until removingStream stream_

    /// Subscribe to a stream, handling each event with the given subscription,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribePlus subscription (subscriber : 's) stream world =
        let subscribe = fun world ->
            let subscriptionKey = makeGuid ()
            let subscriptionAddress = ntoa<'a> !!(scstring subscriptionKey)
            let (address, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                EventWorld.unsubscribe<'g, 'w> subscriptionKey world
            let world = EventWorld.subscribePlus<'a, 's, 'g, 'w> subscriptionKey subscription address subscriber world |> snd
            (subscriptionAddress, unsubscribe, world)
        let stream = { Subscribe = subscribe }
        stream.Subscribe world |> _bc

    /// Subscribe to a stream, handling each event with the given subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribe subscription subscriber stream world =
        subscribePlus (fun evt world -> (Cascade, subscription evt world)) subscriber stream world |> snd

    /// Subscribe to a stream until the subscriber is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] monitorPlus subscription subscriber stream world =
        (stream |> lifetime subscriber |> subscribePlus subscription subscriber) world

    /// Subscribe to a stream until the subscriber is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] monitor subscription subscriber stream world =
        monitorPlus (fun evt world -> (Cascade, subscription evt world)) subscriber stream world |> snd

    (* Derived Combinators *)

    /// Transform a stream into a running average of its event's numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline average (stream : Stream<'a, 'g, 'w>) : Stream<'a, 'g, 'w> =
        foldMap
            (fun (avg : 'a, den : 'a) a ->
                let den' = den + one ()
                let dod' = den / den'
                let avg' = avg * dod' + a / den
                (avg', den'))
            fst
            (zero (), zero ())
            stream

    /// Transform a stream into a running map from its event's data to keys as defined by 'f'.
    let [<DebuggerHidden; DebuggerStepThrough>] organize f (stream : Stream<'a, 'g, 'w>) : Stream<('a * 'b) option * Map<'b, 'a>, 'g, 'w> =
        fold
            (fun (_, m) a ->
                let b = f a
                if Map.containsKey b m
                then (None, m)
                else (Some (a, b), Map.add b a m))
            (None, Map.empty)
            stream

    /// Transform a stream into a running set of its event's unique data as defined via 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] groupBy by (stream : Stream<'a, 'g, 'w>) : Stream<'b * bool * 'b Set, 'g, 'w> =
        fold
            (fun (_, _, set) a ->
                let b = by a
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            stream

    /// Transform a stream into a running set of its event's unique data.
    let [<DebuggerHidden; DebuggerStepThrough>] group (stream : Stream<'a, 'g, 'w>) : Stream<'a * bool * 'a Set, 'g, 'w> =
        groupBy id stream

    /// Transform a stream into a running sum of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline sumN stream = reduce (+) stream

    /// Transform a stream into a running product of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline productN stream = reduce (*) stream

    /// Transform a stream of pairs into its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] first stream = map fst stream

    /// Transform a stream of pairs into its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] second stream = map snd stream

    /// Transform a stream's pairs by a mapping of its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapFirst mapper stream = map (fun a -> (mapper ^ fst a, snd a)) stream

    /// Transform a stream of pairs by a mapping of its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapSecond mapper stream = map (fun a -> (fst a, mapper ^ snd a)) stream

    /// Transform a stream by duplicating its data into pairs.
    let [<DebuggerHidden; DebuggerStepThrough>] duplicate stream = map (fun a -> (a, a)) stream

    /// Take only the first n events from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] take n stream = track (fun m -> (m + 1, m < n)) 0 stream

    /// Skip the first n events in a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] skip n stream = track (fun m -> (m + 1, m >= n)) 0 stream

    /// Take only the first event from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] head stream = take 1 stream

    /// Skip the first event of a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] tail stream = skip 1 stream

    /// Take only the nth event from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] nth n stream = stream |> skip n |> head

    /// Take only the first event from a stream that satisfies 'p'.
    let [<DebuggerHidden; DebuggerStepThrough>] search p stream = stream |> filter p |> head

    /// Filter out the None data values from a stream and strip the Some constructor from
    /// the remaining values.
    let [<DebuggerHidden; DebuggerStepThrough>] choose (stream : Stream<'a option, 'g, 'w>) =
        stream |> filter Option.isSome |> map Option.get

    /// Transform a stream into a running maximum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] max stream = reduce (fun n a -> if n < a then a else n) stream
    
    /// Transform a stream into a running minimum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] min stream = reduce (fun n a -> if a < n then a else n) stream

    /// Filter out the events with non-unique data as defined via 'by' from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinctBy by stream = stream |> organize by |> first |> choose

    /// Filter out the events with non-unique data from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinct stream = distinctBy id stream

[<AutoOpen>]
module StreamOperators =

    // open related module
    open Stream

    /// Stream sequencing operator.
    let (---) = (|>)

    /// Make a stream of the subscriber's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( !-- ) (property : PropertyTag<'a, 'b, 'w>) =
        let changeEventAddress = ltoa<ParticipantChangeData<'a, 'w>> [!!typeof<'a>.Name; !!"Change"; !!property.Name; !!"Event"] ->>- property.This.ParticipantAddress
        stream changeEventAddress --- mapEvent (fun _ world -> property.Get world)

    /// Propagate the event data of a stream to a property in the observing participant when the
    /// subscriber exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] ( --> ) stream (property : PropertyTag<'a, 'b, 'w>) =
        subscribe (fun a world ->
            if world.ParticipantExists a.Subscriber then
                match property.SetOpt with
                | Some set -> set a.Data world
                | None -> world // TODO: log info here about property not being set-able?
            else world)
            property.This
            stream