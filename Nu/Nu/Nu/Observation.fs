// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open LanguagePrimitives
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module ObservationModule =

    /// An observation in the functional reactive style.
    /// TODO: I bet there's either a monad or arrow in here...
    type [<ReferenceEquality>] Observation<'a, 'o when 'o :> Simulant> =
        { Observer : 'o
          Subscribe : World -> 'a Address * (World -> World) * World }
        static member make<'a> observer subscribe =
            { Observer = observer; Subscribe = subscribe }

module Observation =

    (* Primitive Combinators *)

    /// Make an observation from an observer address and an event address.
    let observe<'a, 'o when 'o :> Simulant> (eventAddress : 'a Address) (observer : 'o) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let unsubscribe = fun world -> World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.Publisher world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'product
    /// form', which is defined as a pair of the data of the combined events. Think of it as 'zip'
    /// for event streams.
    let product (eventAddress : 'b Address) (observation : Observation<'a, 'o>) : Observation<'a * 'b, 'o> =
        // TODO: implement this with callback state instead of the rat's nest of subscriptions.
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<'a * 'b> <| acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let subscription' = fun event' world ->
                    let eventData = (event.Data, event'.Data)
                    let world = World.publish<'a * 'b, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.Publisher world
                    let world = World.unsubscribe subscriptionKey' world
                    (Cascade, world)
                let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observation.Observer world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observation.Observer world
            (subscriptionAddress'', unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Combine an observation with the events from the given address. Combination is in 'sum
    /// form', which is defined as an Either of the data of the combined events, where only data
    /// from the most recent event is available at a time.
    let sum (eventAddress : 'b Address) (observation : Observation<'a, 'o>) : Observation<Either<'a, 'b>, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observation.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> <| acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let eventData = Left event.Data
                let world = World.publish<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.Publisher world
                (Cascade, world)
            let subscription' = fun event world ->
                let eventData = Right event.Data
                let world = World.publish<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.Publisher world
                (Cascade, world)
            let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observation.Observer world
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observation.Observer world
            (subscriptionAddress'', unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Filter an observation's events by 'pred'.
    let filter (pred : Event<'a, 'o> -> World -> bool) (observation : Observation<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world =
                    if pred event world
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Map an observation's events by the 'mapper' procedure.
    let map (mapper : Event<'a, 'o> -> World -> 'b) (observation : Observation<'a, 'o>) : Observation<'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'b, Simulant> World.sortSubscriptionsNone (mapper event world) subscriptionAddress event.Publisher world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let track4
        (tracker : 'c -> Event<'a, 'o> -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (observation : Observation<'a, 'o>) :
        Observation<'b, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let state = World.getCallbackState callbackKey world
                let (state, tracked) = tracker state event world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked
                    then World.publish<'b, Simulant> World.sortSubscriptionsNone (transformer state) subscriptionAddress event.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let track2
        (tracker : 'a -> Event<'a, 'o> -> World -> 'a * bool)
        (observation : Observation<'a, 'o>) :
        Observation<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let optState = World.getCallbackState callbackKey world
                let state = match optState with Some state -> state | None -> event.Data
                let (state, tracked) = tracker state event world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone state subscriptionAddress event.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// TODO: document!
    let track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (observation : Observation<'a, 'o>) :
        Observation<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let state = World.getCallbackState callbackKey world
                let (state, tracked) = tracker state world
                let world = World.addCallbackState callbackKey state world
                let world =
                    if tracked
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.Publisher world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let subscribeWithUnsub handleEvent observation world =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (address, unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let world = World.subscribe<'a, 'o> subscriptionKey handleEvent address observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        let observation = { Observer = observation.Observer; Subscribe = subscribe }
        observation.Subscribe world |> _bc

    /// Subscribe to an observation, handling each event with the given 'handleEvent' procedure.
    let subscribe handleEvent observation world =
        subscribeWithUnsub handleEvent observation world |> snd

    /// Terminate an observation when an event at the given address is raised.
    let until eventAddress (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        let subscribe = fun world ->
            let eventKey = World.makeSubscriptionKey ()
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress', unsubscribe, world) = observation.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = World.subscribe eventKey handleEvent eventAddress observation.Observer world
            let subscription = fun event world ->
                let world = World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.Publisher world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress' observation.Observer world
            (subscriptionAddress, unsubscribe, world)
        { Observer = observation.Observer; Subscribe = subscribe }

    /// Terminate an observation when the observer is removed from the world.
    let lifetime (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        let removingEventAddress = stoa<unit> (typeof<'o>.Name + "/" + "Removing") ->>- observation.Observer.SimulantAddress
        until removingEventAddress observation

    /// Subscribe to an observation until the observer is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let monitorWithUnsub eventAddress observation world =
        (observation |> lifetime |> subscribeWithUnsub eventAddress) world

    /// Subscribe to an observation until the observer is removed from the world.
    let monitor eventAddress observation world =
        monitorWithUnsub eventAddress observation world |> snd
    
    (* Advanced Combinators *)

    /// Scan over an observation, accumulating state.
    let scan4 (f : 'b -> Event<'a, 'o> -> World -> 'b) g s (observation : Observation<'a, 'o>) : Observation<'c, 'o> =
        track4 (fun b a w -> (f b a w, true)) g s observation
        
    /// Scan over an observation, accumulating state.
    let scan2 (f : 'a -> Event<'a, 'o> -> World -> 'a) (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        track2 (fun a a2 w -> (f a a2 w, true)) observation
        
    /// Scan over an observation, accumulating state.
    let scan (f : 'b -> Event<'a, 'o> -> World -> 'b) s (observation : Observation<'a, 'o>) : Observation<'b, 'o> =
        scan4 f id s observation

    /// Transform an observation into a running average of it event's numeric data.
    let inline average (observation : Observation<'a, 'o>) : Observation<'a, 'o> =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            observation

    /// Transform an observation into a running map from its event's data to keys as defined by 'f'.
    let organize f (observation : Observation<'a, 'o>) : Observation<('a * 'b) option * Map<'b, 'a>, 'o> =
        scan
            (fun (_, m) a world ->
                let b = f a world
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            observation

    /// Transform an observation into a running set of its event's unique data as defined by 'by'.
    let groupBy by (observation : Observation<'a, 'o>) : Observation<'b * bool * 'b Set, 'o> =
        scan
            (fun (_, _, set) a world ->
                let b = by a world
                if Set.contains b set
                then (b, false, set)
                else (b, true, Set.add b set))
            (Unchecked.defaultof<'b>, false, Set.empty)
            observation

    /// Transform an observation into a running set of its event's unique data.
    let group (observation : Observation<'a, 'o>) : Observation<'a * bool * 'a Set, 'o> =
        groupBy (fun a _ -> a.Data) observation

    /// Transform an observation into a running sum of its data.
    let inline sumN observation = scan2 (fun n a _ -> n + a.Data) observation

    /// Transform an observation into a running product of its data.
    let inline productN observation = scan2 (fun n a _ -> n * a.Data) observation
    
    /// Transform an observation of pairs into its fst values.
    let toFst observation = map (fun a _ -> fst a.Data) observation
    
    /// Transform an observation of pairs into its snd values.
    let toSnd observation = map (fun a _ -> snd a.Data) observation
    
    /// Transform an observation's pairs by a mapping of its fst values.
    let withFst mapper observation = map (fun a _ -> (mapper <| fst a.Data, snd a.Data)) observation
    
    /// Transform an observation of pairs by a mapping of its snd values.
    let withSnd mapper observation = map (fun a _ -> (fst a.Data, mapper <| snd a.Data)) observation
    
    /// Transform an observation by duplicating its data into pairs.
    let duplicate observation = map (fun a _ -> (a.Data, a.Data)) observation
    
    /// Take only the first n events from an observation.
    let take n observation = track (fun m _ -> (m + 1, m < n)) 0 observation
    
    /// Skip the first n events in an observation.
    let skip n observation = track (fun m _ -> (m + 1, m >= n)) 0 observation
    
    /// Take only the first event from an observation.
    let head observation = take 1 observation
    
    /// Skip the first event of an observation.
    let tail observation = skip 1 observation
    
    /// Take only the nth event from an observation.
    let nth n observation = observation |> skip n |> head
    
    /// Take only the first event from an observation that satisfies 'p'.
    let search p observation = observation |> filter p |> head
    
    /// Filter out the None data values from an observation and strip the Some constructor from
    /// the remaining values.
    let choose (observation : Observation<'a option, 'o>) = observation |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    
    /// Transform an observation into a running maximum of it numeric data.
    let max observation = scan2 (fun n a _ -> if n < a.Data then a.Data else n) observation
    
    /// Transform an observation into a running minimum of it numeric data.
    let min observation = scan2 (fun n a _ -> if a.Data < n then a.Data else n) observation

    /// Filter out the events with non-unique data as defined by 'by' from an observation.
    let distinctBy by observation = observation |> organize by |> toFst |> choose
    
    /// Filter out the events with non-unique data from an observation.
    let distinct observation = distinctBy (fun a -> a.Data) observation

    (* Special Combinators *)

    /// Take events from an observation only while World.isGamePlaying evaluates to true.
    let isGamePlaying _ world = World.isGamePlaying world

    /// Take events from an observation only while World.isPhysicsRunning evaluates to true.
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    
    /// Take events from an observation only when the observer is selected in the world (see
    /// documentation for World.isAddressSelected for what this means (it's very useful!)).
    let isSelected event world = World.isSimulantSelected event.Subscriber world

    /// Take events from an observation only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an observation only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

    /// Take only one event from an observation per game tick.
    let noMoreThanOncePerTick observation =
        observation |> organize (fun _ world -> world.State.TickTime) |> toFst |> choose

    /// Filter out world state change events that do not relate to those returned by 'valueGetter'.
    let worldStateValue (valueGetter : WorldState -> 'b) (observation : Observation<WorldStateChangeData, 'o>) =
        filter (fun a world ->
            let oldValue = valueGetter a.Data.OldWorldState
            let newValue = valueGetter (World.getState world)
            oldValue <> newValue)
            observation

    /// Filter out simulant change events that do not relate to those returned by 'valueGetter'.
    let simulantValue (valueGetter : World -> 'b) (observation : Observation<'a SimulantChangeData, 'o>) =
        filter (fun a world ->
            let oldValue = valueGetter a.Data.OldWorld
            let newValue = valueGetter world
            oldValue <> newValue)
            observation

[<AutoOpen>]
module ObservationOperatorsModule =
    open Observation

    /// Pipe-right arrow that provides special precedence for observations.
    let (-|>) = (|>)

    /// Pipe-right fat-arrow that provides special precedence for observations.
    let (=|>) = (|>)

    /// Make an observation of the world state's change events.
    let ( *== ) valueGetter observer =
        observe WorldStateChangeEventAddress observer |>
        worldStateValue valueGetter

    /// Make an observation of one of the world state's change events per frame.
    let (/==) valueGetter observer =
        valueGetter *== observer |>
        noMoreThanOncePerTick

    /// Make an observation of the observer's change events.
    let ( *-- ) (simulant : 'a, valueGetter : World -> 'b) (observer : 'o) =
        let simulantChangeEventAddress = stoa<'a SimulantChangeData> (typeof<'a>.Name + "/Change")
        let changeEventAddress = simulantChangeEventAddress ->>- simulant.SimulantAddress
        observe changeEventAddress observer |>
        simulantValue valueGetter

    /// Make an observation of one of the observer's change events per frame.
    let (/--) (simulant, valueGetter) observer =
        (simulant, valueGetter) *-- observer |>
        noMoreThanOncePerTick

    /// Propagate the event data of an observation to a value in the world's state.
    let (==>) observation valueSetter =
        subscribe (fun a world -> (Cascade, World.updateState (valueSetter a.Data world) world)) observation

    /// Propagate the event data of an observation to a value in the observing simulant when the
    /// observer exists (doing nothing otherwise).
    let (-->) observation valueSetter =
        subscribe (fun a world ->
            let world =
                if World.containsSimulant a.Subscriber world
                then valueSetter a.Data world
                else world
            (Cascade, world))
            observation

    /// Propagate a value from the world's state to another value in the world's state.
    let ( *==> ) (valueGetter : WorldState -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        valueGetter *== Game ==> fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue

    /// Propagate a value from the world's state to another value in the world's state, but with frame-based cycle-breaking.
    let (/==>) (valueGetter : WorldState -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        valueGetter /== Game ==> fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue

    /// Propagate a value from the world's state to a value in the given simulant.
    let ( *=-> ) (valueGetter : WorldState -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        valueGetter *== destination --> fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue world

    /// Propagate a value from the world's state to a value in the given simulant, but with frame-based cycle-breaking.
    let (/=->) (valueGetter : WorldState -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        valueGetter /== destination --> fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue world

    /// Propagate a value from the given simulant to a value in the world's state.
    let ( *-=> ) (source : 'a, valueGetter : World -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        (source, valueGetter) *-- Game ==> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue

    /// Propagate a value from the given simulant to a value in the world's state, but with frame-based cycle-breaking.
    let (/-=>) (source : 'a, valueGetter : World -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        (source, valueGetter) /-- Game ==> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue

    // Propagate a value from the given source simulant to a value in the given destination simulant.
    let ( *--> ) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) *-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world

    // Propagate a value from the given source simulant to a value in the given destination simulant, but with frame-based cycle-breaking.
    let (/-->) (source : 'a, valueGetter : World -> 'b) (destination : 'o, valueSetter : 'b -> World -> World) =
        (source, valueGetter) /-- destination --> fun _ world -> let sourceValue = valueGetter world in valueSetter sourceValue world