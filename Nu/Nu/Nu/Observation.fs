// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Diagnostics
open LanguagePrimitives
open Prime

/// An observation in the functional reactive style.
/// TODO: I bet there's either a monad or arrow here...
type [<ReferenceEquality>] Observation<'a, 'o when 'o :> Simulant> =
    { Observer : 'o
      Subscribe : World -> 'a Address * (World -> World) * World }

module Observation =

    (* Primitive Combinators *)

    /// Make an observation of an event at the given address.
    let [<DebuggerHidden; DebuggerStepThrough>] observe<'a, 'o when 'o :> Simulant> (eventAddress : 'a Address) (observer : 'o) : Observation<'a, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let unsubscribe = fun world -> World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = "Observation.observe" :: evt.Trace
                let world = World.publish5<'a, Simulant> World.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'product
    /// form', which is defined as a pair of the data of the combined events. Think of it as 'zip'
    /// for event streams.
    /// NOTE: This function is currently broken.
    /// TODO: fix by implementing this with callback state instead of the rat's nest of subscriptions.
    let [<DebuggerHidden; DebuggerStepThrough>] product (eventAddress : 'b Address) (observation : Observation<'a, 'o>) : Observation<'a * 'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<'a * 'b> ^ Name.make ^ acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun evt world ->
                let subscription' = fun event' world ->
                    let eventTrace = "Observation.product" :: evt.Trace
                    let eventData = (evt.Data, event'.Data)
                    let world = World.publish5<'a * 'b, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace evt.Publisher world
                    let world = World.unsubscribe subscriptionKey' world
                    (Cascade, world)
                let world = World.subscribe5<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observation.Observer world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription subscriptionAddress observation.Observer world
            (subscriptionAddress'', unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'sum
    /// form', which is defined as an Either of the data of the combined events, where only data
    /// from the most recent event is available at a time.
    let [<DebuggerHidden; DebuggerStepThrough>] sum (eventAddress : 'b Address) (observation : Observation<'a, 'o>) : Observation<Either<'a, 'b>, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> ^ Name.make ^ acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun evt world ->
                let eventTrace = "Observation.sum" :: evt.Trace
                let eventData = Left evt.Data
                let world = World.publish5<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let subscription' = fun evt world ->
                let eventTrace = "Observation.sum" :: evt.Trace
                let eventData = Right evt.Data
                let world = World.publish5<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' eventTrace evt.Publisher world
                (Cascade, world)
            let world = World.subscribe5<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observation.Observer world
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription subscriptionAddress observation.Observer world
            (subscriptionAddress'', unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Filter an observation by the given 'pred' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] filter (pred : Event<'a, 'o> -> World -> bool) (observation : Observation<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let world =
                    if pred evt world then
                        let eventTrace = "Observation.filter" :: evt.Trace
                        World.publish5<'a, Simulant> World.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Map an observation by the given 'mapper' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] map (mapper : Event<'a, 'o> -> World -> 'b) (observation : Observation<'a, 'o>) : Observation<'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let eventTrace = "Observation.map" :: evt.Trace
                let world = World.publish5<'b, Simulant> World.sortSubscriptionsNone (mapper evt world) subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track4
        (tracker : 'c -> Event<'a, 'o> -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (observation : Observation<'a, 'o>) :
        Observation<'b, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let state = World.getCallbackState callbackKey world
                let (state, tracked) = tracker state evt world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked then
                        let eventTrace = "Observation.track4" :: evt.Trace
                        let eventData = transformer state
                        World.publish5<'b, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track2
        (tracker : 'a -> Event<'a, 'o> -> World -> 'a * bool)
        (observation : Observation<'a, 'o>) :
        Observation<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let optState = World.getCallbackState callbackKey world
                let state = match optState with Some state -> state | None -> evt.Data
                let (state, tracked) = tracker state evt world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked then
                        let eventTrace = "Observation.track2" :: evt.Trace
                        World.publish5<'a, Simulant> World.sortSubscriptionsNone state subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let [<DebuggerHidden; DebuggerStepThrough>] track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (observation : Observation<'a, 'o>) :
        Observation<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun evt world ->
                let state = World.getCallbackState callbackKey world
                let (state, tracked) = tracker state world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked then
                        let eventTrace = "Observation.track" :: evt.Trace
                        World.publish5<'a, Simulant> World.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribePlus handleEvent observation world =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let (address, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let world = World.subscribe5<'a, 'o> subscriptionKey handleEvent address observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        let observation = { Observer = observation.Observer; Subscribe = subscribe }
        observation.Subscribe world |> _bc

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure.
    let [<DebuggerHidden; DebuggerStepThrough>] subscribe handleEvent observation world =
        subscribePlus handleEvent observation world |> snd

    /// Terminate an observation when an event at the given address is raised.
    let [<DebuggerHidden; DebuggerStepThrough>] until (eventAddress : unit Address) (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        let subscribe = fun world ->
            let eventKey = World.makeSubscriptionKey ()
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> ^ Name.make ^ acstring subscriptionKey
            let (eventAddress', unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = World.subscribe5 eventKey handleEvent eventAddress observation.Observer world
            let subscription = fun evt world ->
                let eventTrace = "Observation.until" :: evt.Trace
                let world = World.publish5<'a, Simulant> World.sortSubscriptionsNone evt.Data subscriptionAddress eventTrace evt.Publisher world
                (Cascade, world)
            let world = World.subscribe5<'a, 'o> subscriptionKey subscription eventAddress' observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Terminate an observation when the observer is removed from the world.
    let [<DebuggerHidden; DebuggerStepThrough>] lifetime (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        let removingEventAddress = ftoa<unit> !!(typeof<'o>.Name + "/Removing") ->>- observation.Observer.SimulantAddress
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
    let [<DebuggerHidden; DebuggerStepThrough>] scan4 (f : 'b -> Event<'a, 'o> -> World -> 'b) g s (observation : Observation<'a, 'o>) : Observation<'c, 'o> =
        track4 (fun b a w -> (f b a w, true)) g s observation
        
    /// Scan over an observation, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan2 (f : 'a -> Event<'a, 'o> -> World -> 'a) (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        track2 (fun a a2 w -> (f a a2 w, true)) observation
        
    /// Scan over an observation, accumulating state.
    let [<DebuggerHidden; DebuggerStepThrough>] scan (f : 'b -> Event<'a, 'o> -> World -> 'b) s (observation : Observation<'a, 'o>) : Observation<'b, 'o> =
        scan4 f id s observation

    /// Transform an observation into a running average of its event's numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline average (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            observation

    /// Transform an observation into a running map from its event's data to keys as defined by 'f'.
    let [<DebuggerHidden; DebuggerStepThrough>] organize f (observation : Observation<'a, 'o>) : Observation<('a * 'b) option * Map<'b, 'a>, 'o> =
        scan
            (fun (_, m) a world ->
                let b = f a world
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            observation

    /// Transform an observation into a running set of its event's unique data as defined by 'by'.
    let [<DebuggerHidden; DebuggerStepThrough>] groupBy by (observation : Observation<'a, 'o>) : Observation<'b * bool * 'b Set, 'o> =
        scan
            (fun (_, _, set) a world ->
                let b = by a world
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            observation

    /// Transform an observation into a running set of its event's unique data.
    let [<DebuggerHidden; DebuggerStepThrough>] group (observation : Observation<'a, 'o>) : Observation<'a * bool * 'a Set, 'o> =
        groupBy (fun a _ -> a.Data) observation

    /// Transform an observation into a running sum of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline sumN observation = scan2 (fun n a _ -> n + a.Data) observation

    /// Transform an observation into a running product of its data.
    let [<DebuggerHidden; DebuggerStepThrough>] inline productN observation = scan2 (fun n a _ -> n * a.Data) observation
    
    /// Transform an observation of pairs into its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] toFst observation = map (fun a _ -> fst a.Data) observation
    
    /// Transform an observation of pairs into its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] toSnd observation = map (fun a _ -> snd a.Data) observation
    
    /// Transform an observation's pairs by a mapping of its fst values.
    let [<DebuggerHidden; DebuggerStepThrough>] withFst mapper observation = map (fun a _ -> (mapper ^ fst a.Data, snd a.Data)) observation
    
    /// Transform an observation of pairs by a mapping of its snd values.
    let [<DebuggerHidden; DebuggerStepThrough>] withSnd mapper observation = map (fun a _ -> (fst a.Data, mapper ^ snd a.Data)) observation
    
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
    let [<DebuggerHidden; DebuggerStepThrough>] choose (observation : Observation<'a option, 'o>) =
        observation |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    
    /// Transform an observation into a running maximum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] max observation = scan2 (fun n a _ -> if n < a.Data then a.Data else n) observation
    
    /// Transform an observation into a running minimum of it numeric data.
    let [<DebuggerHidden; DebuggerStepThrough>] min observation = scan2 (fun n a _ -> if a.Data < n then a.Data else n) observation

    /// Filter out the events with non-unique data as defined by 'by' from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] distinctBy by observation = observation |> organize by |> toFst |> choose
    
    /// Filter out the events with non-unique data from an observation.
    let [<DebuggerHidden; DebuggerStepThrough>] distinct observation = distinctBy (fun a -> a.Data) observation

    (* Special Combinators *)

    /// Take events from an observation only while World.isTicking evaluates to true.
    let [<DebuggerHidden; DebuggerStepThrough>] isTicking _ world = World.isTicking world

    /// Take events from an observation only when the observer is selected in the world (see
    /// documentation for World.isAddressSelected for what this means (it's very useful!)).
    let [<DebuggerHidden; DebuggerStepThrough>] isObserverSelected evt world = World.isSimulantSelected evt.Subscriber world

    /// Take events from an observation only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an observation only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let [<DebuggerHidden; DebuggerStepThrough>] isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

    /// Take only one event from an observation per game update.
    let [<DebuggerHidden; DebuggerStepThrough>] noMoreThanOncePerUpdate observation =
        observation |> organize (fun _ world -> World.getUpdateCount world) |> toFst |> choose

    /// Filter out simulant change events that do not relate to those returned by 'valueGetter'.
    let [<DebuggerHidden; DebuggerStepThrough>] simulantValue (valueGetter : World -> 'b) (observation : Observation<'a SimulantChangeData, 'o>) =
        filter (fun a world ->
            let oldValue = valueGetter a.Data.OldWorld
            let newValue = valueGetter world
            oldValue <> newValue)
            observation

[<AutoOpen>]
module ObservationModule =
    open Observation

    /// Pipe-right arrow that provides special precedence for observations.
    let (-|>) = (|>)

    /// Make an observation of the observer's change events.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-- ) (simulant : 'a, valueGetter : World -> 'b) (observer : 'o) =
        let changeEventAddress = ftoa<'a SimulantChangeData> !!(typeof<'a>.Name + "/Change") ->>- simulant.SimulantAddress
        observe changeEventAddress observer |> simulantValue valueGetter

    /// Make an observation of one of the observer's change events per frame.
    let [<DebuggerHidden; DebuggerStepThrough>] (/--) (simulant, valueGetter) observer =
        (simulant, valueGetter) *-- observer |> noMoreThanOncePerUpdate

    /// Propagate the event data of an observation to a value in the observing simulant when the
    /// observer exists (doing nothing otherwise).
    let [<DebuggerHidden; DebuggerStepThrough>] (-->) observation valueSetter =
        subscribe (fun a world ->
            let world =
                if World.containsSimulant a.Subscriber world
                then valueSetter a.Data world
                else world
            (Cascade, world))
            observation

    // Propagate a value from the given source simulant to a value in the given destination simulant.
    let [<DebuggerHidden; DebuggerStepThrough>] ( *-> ) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) *-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world

    // Propagate a value from the given source simulant to a value in the given destination simulant, but with frame-based cycle-breaking.
    let [<DebuggerHidden; DebuggerStepThrough>] (/->) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) /-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world