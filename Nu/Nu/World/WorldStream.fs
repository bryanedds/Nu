// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open Prime

/// The Stream comonad.
type [<ReferenceEquality>] Stream<'a> =
    { Subscribe : World -> 'a Address * (World -> World) * World }

// TODO: document track functions.
[<RequireQualifiedAccess>]
module Stream =

    /// Make a stream of an event at the given address.
    let [<DebuggerHidden; DebuggerStepThrough>] make<'a>
        (eventAddress : 'a Address) : Stream<'a> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let unsubscribe = fun world -> World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "stream" "" evt.Trace
                let world = World.publishPlus<'a, Simulant> evt.Data subscriptionAddress eventTrace globalSimulant false false world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Generalize a stream's data type to obj.
    let [<DebuggerHidden; DebuggerStepThrough>] generalize<'a>
        (stream : Stream<'a>) : Stream<obj> =
        { Subscribe = fun world -> let (address, unsub, world) = stream.Subscribe world in (atooa address, unsub, world) }

    (* Side-Effecting Combinators *)

    let [<DebuggerHidden; DebuggerStepThrough>] trackEffect4
        (tracker : 'c -> Event<'a, Simulant> -> World -> 'c * bool * World)
        (transformer : 'c -> 'b)
        (state : 'c)
        (stream : Stream<'a>) :
        Stream<'b> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let stateId = makeGuid ()
            let world = World.addEventState stateId state world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'b> (scstring subscriptionId)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeEventState stateId world
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let state = World.getEventState stateId world
                let (state, tracked, world) = tracker state evt world
                let world = World.addEventState stateId state world
                let world =
                    if tracked then
                        let eventData = transformer state
                        let eventTrace = EventTrace.record "Stream" "trackEvent4" "" evt.Trace
                        World.publishPlus<'b, Simulant> eventData subscriptionAddress eventTrace globalSimulant false false world
                    else world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    let [<DebuggerHidden; DebuggerStepThrough>] trackEffect2
        (tracker : 'a -> Event<'a, Simulant> -> World -> 'a * bool * World)
        (stream : Stream<'a>) :
        Stream<'a> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let stateId = makeGuid ()
            let world = World.addEventState stateId None world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeEventState stateId world
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let stateOpt = World.getEventState stateId world
                let state = match stateOpt with Some state -> state | None -> evt.Data
                let (state, tracked, world) = tracker state evt world
                let world = World.addEventState stateId state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "trackEvent2" "" evt.Trace
                        World.publishPlus<'a, Simulant> state subscriptionAddress eventTrace globalSimulant false false world
                    else world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    let [<DebuggerHidden; DebuggerStepThrough>] trackEffect
        (tracker : 'b -> World -> 'b * bool * World) (state : 'b) (stream : Stream<'a>) : Stream<'a> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let stateId = makeGuid ()
            let world = World.addEventState stateId state world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeEventState stateId world
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let state = World.getEventState stateId world
                let (state, tracked, world) = tracker state world
                let world = World.addEventState stateId state world
                let world =
                    if tracked then
                        let eventTrace = EventTrace.record "Stream" "trackEvent" "" evt.Trace
                        World.publishPlus<'a, Simulant> evt.Data subscriptionAddress eventTrace globalSimulant false false world
                    else world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldThenEffect (f : 'b -> Event<'a, Simulant> -> World -> 'b * World) g s (stream : Stream<'a>) : Stream<'c> =
        trackEffect4 (fun b a w -> (Triple.insert true (f b a w))) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldEffect (f : 'b -> Event<'a, Simulant> -> World -> 'b * World) s (stream : Stream<'a>) : Stream<'b> =
        trackEffect4 (fun b a w -> (Triple.insert true (f b a w))) id s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduceEffect (f : 'a -> Event<'a, Simulant> -> World -> 'a * World) (stream : Stream<'a>) : Stream<'a> =
        trackEffect2 (fun a a2 w -> (Triple.insert true (f a a2 w))) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filterEffect
        (pred : Event<'a, Simulant> -> World -> bool * World) (stream : Stream<'a>) =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let (passed, world) = pred evt world
                let world =
                    if passed then
                        let eventTrace = EventTrace.record "Stream" "filterEvent" "" evt.Trace
                        World.publishPlus<'a, Simulant> evt.Data subscriptionAddress eventTrace globalSimulant false false world
                    else world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] mapEffect
        (mapper : Event<'a, Simulant> -> World -> 'b * World) (stream : Stream<'a>) : Stream<'b> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'b> (scstring subscriptionId)
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let callback = fun evt world ->
                let (eventData, world) = mapper evt world
                let eventTrace = EventTrace.record "Stream" "mapEvent" "" evt.Trace
                let world = World.publishPlus<'b, Simulant> eventData subscriptionAddress eventTrace globalSimulant false false world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback eventAddress globalSimulant world |> snd
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe }

    /// Map over two streams.
    let [<DebuggerHidden; DebuggerStepThrough>] map2Effect
        (mapper : Event<'a, Simulant> -> Event<'b, Simulant> -> World -> 'c * World)
        (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<'c> =
        let subscribe = fun (world : World) ->

            // initialize event state, subscription keys and addresses
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let stateId = makeGuid ()
            let state = (List.empty<Event<'a, Simulant>>, List.empty<Event<'b, Simulant>>)
            let world = World.addEventState stateId state world
            let subscriptionId = makeGuid ()
            let subscriptionId' = makeGuid ()
            let subscriptionId'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream2.Subscribe world
            let subscriptionAddress'' = ntoa<'c> (scstring subscriptionId'')

            // unsubscribe from 'a and 'b events, and remove event state
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = World.unsubscribe subscriptionId world
                let world = World.unsubscribe subscriptionId' world
                World.removeEventState stateId world

            // callback for 'a events
            let callback = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "product" "'a" evt.Trace
                let (aList : Event<'a, Simulant> list, bList : Event<'b, Simulant> list) = World.getEventState stateId world
                let aList = evt :: aList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let (eventData, world) = mapper a b world
                        let world = World.publishPlus<'c, Simulant> eventData subscriptionAddress'' eventTrace globalSimulant false false world
                        (state, world)
                    | state -> (state, world)
                let world = World.addEventState stateId state world
                (Cascade, world)

            // callback for 'b events
            let callback' = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "product" "'b" evt.Trace
                let (aList : Event<'a, Simulant> list, bList : Event<'b, Simulant> list) = World.getEventState stateId world
                let bList = evt :: bList
                let (state, world) =
                    match (List.rev aList, List.rev bList) with
                    | (a :: aList, b :: bList) ->
                        let state = (aList, bList)
                        let (eventData, world) = mapper a b world
                        let world = World.publishPlus<'c, Simulant> eventData subscriptionAddress'' eventTrace globalSimulant false false world
                        (state, world)
                    | state -> (state, world)
                let world = World.addEventState stateId state world
                (Cascade, world)

            // subscripe 'a and 'b events
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback subscriptionAddress globalSimulant world |> snd
            let world = World.subscribePlus<'b, Simulant> subscriptionId callback' subscriptionAddress' globalSimulant world |> snd
            (subscriptionAddress'', unsubscribe, world)

        // fin
        { Subscribe = subscribe }

    (* Event-Accessing Combinators *)

    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent4
        (tracker : 'c -> Event<'a, Simulant> -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (stream : Stream<'a>) :
        Stream<'b> =
        trackEffect4 (fun state evt world -> Triple.append world (tracker state evt world)) transformer state stream

    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent2
        (tracker : 'a -> Event<'a, Simulant> -> World -> 'a * bool)
        (stream : Stream<'a>) :
        Stream<'a> =
        trackEffect2 (fun state evt world -> Triple.append world (tracker state evt world)) stream

    let [<DebuggerHidden; DebuggerStepThrough>] trackEvent
        (tracker : 'b -> World -> 'b * bool) (state : 'b) (stream : Stream<'a>) : Stream<'a> =
        trackEffect (fun state world -> Triple.append world (tracker state world)) state stream

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldThenEvent (f : 'b -> Event<'a, Simulant> -> World -> 'b) g s (stream : Stream<'a>) : Stream<'c> =
        foldThenEffect (fun state evt world -> (f state evt world, world)) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldEvent (f : 'b -> Event<'a, Simulant> -> World -> 'b) s (stream : Stream<'a>) : Stream<'b> =
        foldEffect (fun state evt world -> (f state evt world, world)) s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduceEvent (f : 'a -> Event<'a, Simulant> -> World -> 'a) (stream : Stream<'a>) : Stream<'a> =
        reduceEffect (fun value evt world -> (f value evt world, world)) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filterEvent
        (pred : Event<'a, Simulant> -> World -> bool) (stream : Stream<'a>) =
        filterEffect (fun evt world -> (pred evt world, world)) stream

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] mapEvent
        (mapper : Event<'a, Simulant> -> World -> 'b) (stream : Stream<'a>) : Stream<'b> =
        mapEffect (fun evt world -> (mapper evt world, world)) stream

    /// Map over two streams by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map2Event
        (mapper : Event<'a, Simulant> -> Event<'b, Simulant> -> World -> 'c) (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<'c> =
        map2Effect (fun evtA evtB world -> (mapper evtA evtB world, world)) stream stream2

    (* World-Accessing Combinators *)

    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld4
        (tracker : 'c -> 'a -> World -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (stream : Stream<'a>) : Stream<'b> =
        trackEvent4 (fun c evt world -> tracker c evt.Data world) transformer state stream

    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld2
        (tracker : 'a -> 'a -> World -> 'a * bool) (stream : Stream<'a>) : Stream<'a> =
        trackEvent2 (fun a evt world -> tracker a evt.Data world) stream

    let [<DebuggerHidden; DebuggerStepThrough>] trackWorld
        (tracker : 'b -> World -> 'b * bool) (state : 'b) (stream : Stream<'a>) : Stream<'a> =
        trackEvent tracker state stream

    /// Fold over a stream, then map the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldThenWorld (f : 'b -> 'a -> World -> 'b) g s (stream : Stream<'a>) : Stream<'c> =
        foldThenEvent (fun b evt world -> f b evt.Data world) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] foldWorld (f : 'b -> 'a -> World -> 'b) s (stream : Stream<'a>) : Stream<'b> =
        foldEvent (fun b evt world -> f b evt.Data world) s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduceWorld (f : 'a -> 'a -> World -> 'a) (stream : Stream<'a>) : Stream<'a> =
        reduceEvent (fun a evt world -> f a evt.Data world) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filterWorld (pred : 'a -> World -> bool) (stream : Stream<'a>) =
        filterEvent (fun evt world -> pred evt.Data world) stream

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] mapWorld (mapper : 'a -> World -> 'b) (stream : Stream<'a>) : Stream<'b> =
        mapEvent (fun evt world -> mapper evt.Data world) stream

    /// Map over two streams by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map2World
        (mapper : 'a -> 'b -> World -> 'c) (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<'c> =
        map2Event (fun evtA evtB world -> mapper evtA.Data evtB.Data world) stream stream2

    (* Primitive Combinators *)

    let [<DebuggerHidden; DebuggerStepThrough>] track4
        (tracker : 'c -> 'a -> 'c * bool) (transformer : 'c -> 'b) (state : 'c) (stream : Stream<'a>) : Stream<'b> =
        trackWorld4 (fun c a _ -> tracker c a) transformer state stream

    let [<DebuggerHidden; DebuggerStepThrough>] track2
        (tracker : 'a -> 'a -> 'a * bool) (stream : Stream<'a>) : Stream<'a> =
        trackWorld2 (fun a a2 _ -> tracker a a2) stream

    let [<DebuggerHidden; DebuggerStepThrough>] track
        (tracker : 'b -> 'b * bool) (state : 'b) (stream : Stream<'a>) : Stream<'a> =
        trackWorld (fun b _ -> tracker b) state stream

    let [<DebuggerHidden; DebuggerStepThrough>] foldThen (f : 'b -> 'a -> 'b) g s (stream : Stream<'a>) : Stream<'c> =
        foldThenWorld (fun b a _ -> f b a) g s stream

    /// Fold over a stream, aggegating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] fold (f : 'b -> 'a -> 'b) s (stream : Stream<'a>) : Stream<'b> =
        foldWorld (fun b a _ -> f b a) s stream

    /// Reduce over a stream, accumulating the result.
    let [<DebuggerHidden; DebuggerStepThrough>] reduce (f : 'a -> 'a -> 'a) (stream : Stream<'a>) : Stream<'a> =
        reduceWorld (fun a a2 _ -> f a a2) stream

    /// Filter a stream by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filter (pred : 'a -> bool) (stream : Stream<'a>) =
        filterWorld (fun a _ -> pred a) stream

    /// Map over a stream by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map (mapper : 'a -> 'b) (stream : Stream<'a>) : Stream<'b> =
        mapWorld (fun a _ -> mapper a) stream

    /// Map over two streams by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map2
        (mapper : 'a -> 'b -> 'c) (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<'c> =
        map2World (fun a b _ -> mapper a b) stream stream2

    /// Combine two streams. Combination is in 'product form', which is defined as a pair of the data of the combined
    /// events. Think of it as 'zip' for event streams.
    let [<DebuggerHidden; DebuggerStepThrough>] product
        (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<'a * 'b> =
        map2 (fun a b -> (a, b)) stream stream2

    /// Combine two streams. Combination is in 'sum form', which is defined as an Either of the data of the combined
    /// events, where only data from the most recent event is available at a time.
    let [<DebuggerHidden; DebuggerStepThrough>] sum
        (stream : Stream<'a>) (stream2 : Stream<'b>) : Stream<Either<'a, 'b>> =
        let subscribe = fun world ->
            let subscriptionId = makeGuid ()
            let subscriptionId' = makeGuid ()
            let subscriptionId'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream2.Subscribe world
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> (scstring subscriptionId'')
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = World.unsubscribe subscriptionId world
                World.unsubscribe subscriptionId' world
            let callback = fun evt world ->
                let eventData = Left evt.Data
                let eventTrace = EventTrace.record "Stream" "sum" "" evt.Trace
                let world = World.publishPlus<Either<'a, 'b>, Simulant> eventData subscriptionAddress'' eventTrace globalSimulant false false world
                (Cascade, world)
            let callback' = fun evt world ->
                let eventData = Right evt.Data
                let eventTrace = EventTrace.record "Stream" "sum" "" evt.Trace
                let world = World.publishPlus<Either<'a, 'b>, Simulant> eventData subscriptionAddress'' eventTrace globalSimulant false false world
                (Cascade, world)
            let world = World.subscribePlus<'b, Simulant> subscriptionId' callback' subscriptionAddress' globalSimulant world |> snd
            let world = World.subscribePlus<'a, Simulant> subscriptionId callback subscriptionAddress globalSimulant world |> snd
            (subscriptionAddress'', unsubscribe, world)
        { Subscribe = subscribe }

    /// Take events from a stream while predicate is true.
    let [<DebuggerHidden; DebuggerStepThrough>] during pred stream =
        trackEffect (fun () world -> ((), pred world, world)) () stream

    /// Terminate a stream when a given stream receives a value.
    let [<DebuggerHidden; DebuggerStepThrough>] until
        (stream : Stream<'b>) (stream2 : Stream<'a>) : Stream<'a> =
        let subscribe = fun (world : World) ->
            let globalSimulant = World.getGlobalSimulantGeneralized world
            let subscriptionId = makeGuid ()
            let subscriptionId' = makeGuid ()
            let subscriptionId'' = makeGuid ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let (subscriptionAddress', unsubscribe', world) = stream2.Subscribe world
            let subscriptionAddress'' = ntoa<'a> (scstring subscriptionId'')
            let unsubscribe = fun world ->
                let world = unsubscribe (unsubscribe' world)
                let world = World.unsubscribe subscriptionId' world
                World.unsubscribe subscriptionId world
            let callback = fun _ world ->
                let world = unsubscribe world
                (Cascade, world)
            let callback' = fun evt world ->
                let eventTrace = EventTrace.record "Stream" "until" "" evt.Trace
                let world = World.publishPlus<'a, Simulant> evt.Data subscriptionAddress'' eventTrace globalSimulant false false world
                (Cascade, world)
            let world = World.subscribePlus<'a, Simulant> subscriptionId' callback' subscriptionAddress' globalSimulant world |> snd
            let world = World.subscribePlus<'b, Simulant> subscriptionId callback subscriptionAddress globalSimulant world |> snd
            (subscriptionAddress'', unsubscribe, world)
        { Subscribe = subscribe }

    /// Terminate a stream when the subscriber is unregistered from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] lifetime<'s, 'a when 's :> Simulant>
        (subscriber : 's) (stream_ : Stream<'a>) : Stream<'a> =
        let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> subscriber.SimulantAddress
        let removingStream = make unregisteringEventAddress
        until removingStream stream_

    /// Subscribe to a stream, handling each event with the given callback,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// callback.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribeEffect callback (subscriber : 's) stream world =
        let subscribe = fun world ->
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let (address, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                World.unsubscribe subscriptionId world
            let world = World.subscribePlus<'a, 's> subscriptionId callback address subscriber world |> snd
            (subscriptionAddress, unsubscribe, world)
        let stream = { Subscribe = subscribe }
        stream.Subscribe world |> _bc

    /// Subscribe to a stream, handling each event with the given callback.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribe callback subscriber stream world =
        subscribeEffect (fun evt world -> (Cascade, callback evt world)) subscriber stream world |> snd

    /// Subscribe to a stream until the subscriber is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// callback.
    let [<DebuggerHidden; DebuggerStepThrough>] monitorEffect callback subscriber stream world =
        (stream |> lifetime subscriber |> subscribeEffect callback subscriber) world

    /// Subscribe to a stream until the subscriber is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] monitor callback subscriber stream world =
        monitorEffect (fun evt world -> (Cascade, callback evt world)) subscriber stream world |> snd

    /// Subscribe to a stream for the life span of an entity and a given facet.
    let [<DebuggerHidden; DebuggerStepThrough>] senseEffect<'a> callback (entity : Entity) facetName stream world =
        let subscribe world =
            let removalId = makeGuid ()
            let fastenId = makeGuid ()
            let subscriptionId = makeGuid ()
            let subscriptionAddress = ntoa<'a> (scstring subscriptionId)
            let (address, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun (world : World) ->
                let world = unsubscribe world
                let world = World.unsubscribe removalId world
                let world = World.unsubscribe fastenId world
                let world = World.unsubscribe subscriptionId world
                world
            let callback' = fun _ world -> (Cascade, unsubscribe world)
            let callback'' = fun changeEvent world ->
                let previous = changeEvent.Data.Previous :?> string Set
                let value = changeEvent.Data.Value :?> string Set
                if previous.Contains facetName && not (value.Contains facetName)
                then (Cascade, unsubscribe world)
                else (Cascade, world)
            let unregisteringEventAddress = rtoa<unit> [|"Unregistering"; "Event"|] --> entity.EntityAddress
            let changeFacetNamesEventAddress = rtoa<ChangeData> [|"Change"; "FacetNames"; "Event"|] --> entity.EntityAddress
            let world = World.subscribePlus<unit, Simulant> removalId callback' unregisteringEventAddress entity world |> snd
            let world = World.subscribePlus<ChangeData, Simulant> fastenId callback'' changeFacetNamesEventAddress entity world |> snd
            let world = World.subscribePlus<'a, Entity> subscriptionId callback address entity world |> snd
            (subscriptionAddress, unsubscribe, world)
        let stream = { Subscribe = subscribe }
        stream.Subscribe world |> _bc

    /// Subscribe to a stream for the life span of an entity and a given facet.
    let [<DebuggerHidden; DebuggerStepThrough>] sense callback entity facet stream world =
        senseEffect (fun evt world -> (Cascade, callback evt world)) entity facet stream world |> snd

    /// Insert a persistent state value into the stream.
    let [<DebuggerHidden; DebuggerStepThrough>] insert state stream =
        stream |>
        fold (fun (stateOpt, _) b -> (Some (Option.defaultValue state stateOpt), b)) (None, Unchecked.defaultof<_>) |>
        map (mapFst Option.get)

    (* Derived Combinators *)

    /// Append a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] inline append streamL streamR =
        map Either.amb (sum streamL streamR)

    /// Filter the left values out from the stream.
    let [<DebuggerHidden; DebuggerStepThrough>] inline filterLeft stream =
        filter Either.isLeft stream |> map Either.getLeft

    /// Filter the right values out from the stream.
    let [<DebuggerHidden; DebuggerStepThrough>] inline filterRight stream =
        filter Either.isRight stream |> map Either.getRight

    /// Transform a stream into a running average of its event's numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline average (stream : Stream<'a>) : Stream<'a> =
        foldThen
            (fun (avg : 'a, den : 'a) a ->
                let den' = den + one ()
                let dod' = den / den'
                let avg' = avg * dod' + a / den
                (avg', den'))
            fst
            (zero (), zero ())
            stream

    /// Transform a stream into a running map from its event's data to keys as defined by 'f'.
    let [<DebuggerHidden; DebuggerStepThrough>] organize f (stream : Stream<'a>) : Stream<('a * 'b) option * Map<'b, 'a>> =
        fold
            (fun (_, m) a ->
                let b = f a
                if Map.containsKey b m
                then (None, m)
                else (Some (a, b), Map.add b a m))
            (None, Map.empty)
            stream

    /// Transform a stream into a running set of its event's unique data as defined via 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] groupBy by (stream : Stream<'a>) : Stream<'b * bool * 'b Set> =
        fold
            (fun (_, _, set) a ->
                let b = by a
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            stream

    /// Transform a stream into a running set of its event's unique data.
    let [<DebuggerHidden; DebuggerStepThrough>] group (stream : Stream<'a>) : Stream<'a * bool * 'a Set> =
        groupBy id stream

    /// Filter a stream of options for actual values.
    let [<DebuggerHidden; DebuggerStepThrough>] definitize (stream : Stream<'a option>) =
        stream |>
        filter Option.isSome |>
        map Option.get

    /// Filter events with unchanging data.
    let [<DebuggerHidden; DebuggerStepThrough>] optimizeBy (by : 'a -> 'b) (stream : Stream<'a>) =
        fold
            (fun (s, _) a ->
                let n = by a
                match s with
                | None -> (Some n, Some a)
                | Some b -> if b = n then (Some n, None) else (Some n, Some a))
            (None, None)
            stream |>
        map snd |>
        definitize

    /// Filter events with unchanging data.
    let [<DebuggerHidden; DebuggerStepThrough>] optimize (stream : Stream<'a>) =
        stream |> optimizeBy id

    /// Transform a stream into a running sum of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline sumN stream = reduce (+) stream

    /// Transform a stream into a running product of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline productN stream = reduce (*) stream

    /// Transform a stream of pairs into its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] first stream = map fst stream

    /// Transform a stream of pairs into its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] second stream = map snd stream

    /// Transform a stream's pairs by a mapping of its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapFirst mapper stream = map (fun a -> (mapper (fst a), snd a)) stream

    /// Transform a stream of pairs by a mapping of its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] mapSecond mapper stream = map (fun a -> (fst a, mapper (snd a))) stream

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

    /// Filter out the None data values from a stream and strip the Some constructor from the remaining values.
    let [<DebuggerHidden; DebuggerStepThrough>] choose (stream : Stream<'a option>) = stream |> filter Option.isSome |> map Option.get

    /// Transform a stream into a running maximum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] max stream = reduce (fun n a -> if n < a then a else n) stream
    
    /// Transform a stream into a running minimum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] min stream = reduce (fun n a -> if a < n then a else n) stream

    /// Filter out the events with non-unique data as defined via 'by' from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinctBy by stream = stream |> organize by |> first |> choose

    /// Filter out the events with non-unique data from a stream.
    let [<DebuggerHidden; DebuggerStepThrough>] distinct stream = distinctBy id stream

    /// Identity for streams.
    let [<DebuggerHidden; DebuggerStepThrough>] id (stream : _ Stream) = stream

    /// Take events from a stream only when World.getAdvancing evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] whenAdvancing stream = filterEvent (fun _ -> World.getAdvancing) stream

    /// Take events from a stream only when World.getHalted evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] whenHalted stream = filterEvent (fun _ -> World.getHalted) stream

    /// Take events from a stream only when the simulant is contained by, or is the same as,
    /// the currently selected screen. Game is always considered 'selected' as well.
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelected simulant stream = filterEvent (fun _ -> World.getSelected simulant) stream

    /// Take events from a stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelectedScreenIdling stream = filterEvent (fun _ -> WorldTypes.getSelectedScreenIdling) stream
    
    /// Take events from a stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] whenSelectedScreenTransitioning stream = filterEvent (fun _ -> WorldTypes.getSelectedScreenTransitioning) stream