namespace Nu
open System
open LanguagePrimitives
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module EventStreamModule =

    /// An event stream in the functional reactive style.
    /// TODO: I bet there's either a monad or arrow in here...
    type [<ReferenceEquality>] EventStream<'a, 'o> =
        { ObserverAddress : 'o Address
          Subscribe : World -> 'a Address * (World -> World) * World }
        static member make<'a> observerAddress subscribe =
            { ObserverAddress = observerAddress; Subscribe = subscribe }

module EventStream =

    (* Primitive Combinators *)

    /// Make an event stream from an observer address and an event address.
    let observe<'a, 'o when 'o :> Simulant> (eventAddress : 'a Address) (observerAddress : 'o Address) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let unsubscribe = fun world -> World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observerAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observerAddress; Subscribe = subscribe }

    /// Handle events in an event stream with the given 'handleEvent' procedure.
    let using handleEvent (stream : EventStream<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (address, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let world = World.subscribe<'a, 'o> subscriptionKey handleEvent address stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Combine an event stream with the events from the given address. Combination is in 'product
    /// form', which is defined as a pair of the data of the combined events. Think of it as 'zip'
    /// for event streams.
    let product (eventAddress : 'b Address) (stream : EventStream<'a, 'o>) : EventStream<'a * 'b, 'o> =
        // TODO: implement this with callback state instead of the rat's nest of subscriptions.
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<'a * 'b> <| acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let subscription' = fun event' world ->
                    let eventData = (event.Data, event'.Data)
                    let world = World.publish<'a * 'b, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                    let world = World.unsubscribe subscriptionKey' world
                    (Cascade, world)
                let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' stream.ObserverAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress stream.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Combine an event stream with the events from the given address. Combination is in 'sum
    /// form', which is defined as an Either of the data of the combined events, where only data
    /// from the most recent event is available at a time.
    let sum (eventAddress : 'b Address) (stream : EventStream<'a, 'o>) : EventStream<Either<'a, 'b>, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = stream.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ntoa<Either<'a, 'b>> <| acstring subscriptionKey''
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let eventData = Left event.Data
                let world = World.publish<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                (Cascade, world)
            let subscription' = fun event world ->
                let eventData = Right event.Data
                let world = World.publish<Either<'a, 'b>, Simulant> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' stream.ObserverAddress world
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress stream.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Filter event stream by the 'pred' procedure.
    let filter (pred : Event<'a, 'o> -> World -> bool) (stream : EventStream<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world =
                    if pred event world
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Map event stream by the 'mapper' procedure.
    let map (mapper : Event<'a, 'o> -> World -> 'b) (stream : EventStream<'a, 'o>) : EventStream<'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'b, Simulant> World.sortSubscriptionsNone (mapper event world) subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    let track4
        (tracker : 'c -> Event<'a, 'o> -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (stream : EventStream<'a, 'o>) :
        EventStream<'b, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
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
                    then World.publish<'b, Simulant> World.sortSubscriptionsNone (transformer state) subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    let track2
        (tracker : 'a -> Event<'a, 'o> -> World -> 'a * bool)
        (stream : EventStream<'a, 'o>) :
        EventStream<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
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
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone state subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    let track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (stream : EventStream<'a, 'o>) :
        EventStream<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = stream.Subscribe world
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
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Subscribe to an event stream, returning both an unsubscription procedure as well as the
    /// world as augmented with said subscription.
    let subscribeWithUnsub2 stream world =
        stream.Subscribe world |> _bc

    /// Subscribe to an event stream.
    let subscribe2 stream world =
        subscribeWithUnsub2 stream world |> snd

    /// Subscribe an event stream, handling each event with the given 'handleEvent' procedure,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let subscribeWithUnsub handleEvent stream world =
        stream |> using handleEvent |> subscribeWithUnsub2 <| world

    /// Subscribe an event stream, handling each event with the given 'handleEvent' procedure.
    let subscribe handleEvent stream world =
        subscribeWithUnsub handleEvent stream world |> snd

    /// Terminate an event stream when an event at the given address is raised.
    let until eventAddress (stream : EventStream<'a, 'o>) : EventStream<'a, 'o> =
        let subscribe = fun world ->
            let eventKey = World.makeSubscriptionKey ()
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress', unsubscribe, world) = stream.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = World.subscribe eventKey handleEvent eventAddress stream.ObserverAddress world
            let subscription = fun event world ->
                let world = World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress' stream.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = stream.ObserverAddress; Subscribe = subscribe }

    /// Terminate an event stream when the observing simulant is removed from the world.
    let lifetime (stream : EventStream<'a, 'o>) : EventStream<'a, 'o> =
        until (RemovingEventAddress ->>- stream.ObserverAddress) stream

    /// Subscribe to an event stream until the observing simulant is removed from the world,
    /// returning both an unsubscription procedure as well as the world as augmented with said
    /// subscription.
    let monitorWithUnsub eventAddress stream world =
        (stream |> lifetime |> subscribeWithUnsub eventAddress) world

    /// Subscribe an event stream until the observing simulant is removed from the world.
    let monitor eventAddress stream world =
        monitorWithUnsub eventAddress stream world |> snd
    
    (* Advanced Combinators *)

    /// Scan over an event stream, accumulating state.
    let scan4 (f : 'b -> Event<'a, 'o> -> World -> 'b) g s (stream : EventStream<'a, 'o>) : EventStream<'c, 'o> =
        track4 (fun b a w -> (f b a w, true)) g s stream
        
    /// Scan over an event stream, accumulating state.
    let scan2 (f : 'a -> Event<'a, 'o> -> World -> 'a) (stream : EventStream<'a, 'o>) : EventStream<'a, 'o> =
        track2 (fun a a2 w -> (f a a2 w, true)) stream
        
    /// Scan over an event stream, accumulating state.
    let scan (f : 'b -> Event<'a, 'o> -> World -> 'b) s (stream : EventStream<'a, 'o>) : EventStream<'b, 'o> =
        scan4 f id s stream

    /// Transform an event stream into a running average of it event's numeric data.
    let inline average (stream : EventStream<'a, 'o>) : EventStream<'a, 'o> =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            stream

    /// Transform an event stream into a running map from its event's data to keys as defined by 'f'.
    let organize f (stream : EventStream<'a, 'o>) : EventStream<('a * 'b) option * Map<'b, 'a>, 'o> =
        scan
            (fun (_, m) a w ->
                let b = f a w
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            stream

    /// Transform an event stream into a running set of its event's unique data as defined by 'by'.
    let groupBy by (stream : EventStream<'a, 'o>) : EventStream<'b * bool * 'b Set, 'o> =
        scan
            (fun (_, _, s) a w ->
                let b = by a w
                if Set.contains b s
                then (b, false, s)
                else (b, true, Set.add b s))
            (Unchecked.defaultof<'b>, false, Set.empty)
            stream

    /// Transform an event stream into a running set of its event's unique data.
    let group (stream : EventStream<'a, 'o>) : EventStream<'a * bool * 'a Set, 'o> =
        groupBy (fun a _ -> a.Data) stream

    /// Transform an event stream into a running sum of its data.
    let inline sumN stream = scan2 (fun n a _ -> n + a.Data) stream

    /// Transform an event stream into a running product of its data.
    let inline productN stream = scan2 (fun n a _ -> n * a.Data) stream
    
    /// Transform an event stream of pairs into its fst values.
    let toFst stream = map (fun a _ -> fst a.Data) stream
    
    /// Transform an event stream of pairs into its snd values.
    let toSnd stream = map (fun a _ -> snd a.Data) stream
    
    /// Transform an event stream's pairs by a mapping of its fst values.
    let withFst mapper stream = map (fun a _ -> (mapper <| fst a.Data, snd a.Data)) stream
    
    /// Transform an event stream of pairs by a mapping of its snd values.
    let withSnd mapper stream = map (fun a _ -> (fst a.Data, mapper <| snd a.Data)) stream
    
    /// Transform an event stream by duplicating its data into pairs.
    let duplicate stream = map (fun a _ -> (a.Data, a.Data)) stream
    
    /// Take only the first n events from an event stream.
    let take n stream = track (fun m _ -> (m + 1, m < n)) 0 stream
    
    /// Skip the first n events in an event stream.
    let skip n stream = track (fun m _ -> (m + 1, m >= n)) 0 stream
    
    /// Take only the first event from an event stream.
    let head stream = take 1 stream
    
    /// Skip the first event of an event stream.
    let tail stream = skip 1 stream
    
    /// Take only the nth event from an event stream.
    let nth n stream = stream |> skip n |> head
    
    /// Take only the first event from an event stream that satisfies 'p'.
    let search p stream = stream |> filter p |> head
    
    /// Filter out the None data values from an event stream and strip the Some constructor from
    /// the remaining values.
    let choose (stream : EventStream<'a option, 'o>) = stream |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    
    /// Transform an event stream into a running maximum of it numeric data.
    let max stream = scan2 (fun n a _ -> if n < a.Data then a.Data else n) stream
    
    /// Transform an event stream into a running minimum of it numeric data.
    let min stream = scan2 (fun n a _ -> if a.Data < n then a.Data else n) stream

    /// Filter out the events with non-unique data as defined by 'by' from an event stream.
    let distinctBy by stream = stream |> organize by |> toFst |> choose
    
    /// Filter out the events with non-unique data from an event stream.
    let distinct stream = distinctBy (fun a -> a.Data) stream

    (* Special Combinators *)

    /// Take events from an event stream only while World.isGamePlaying returns true.
    let isGamePlaying _ world = World.isGamePlaying world

    /// Take events from an event stream only while World.isPhysicsRunning returns true.
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    
    /// Take events from an event stream only when the observer is selected in the world (see
    /// documentation for World.isAddressSelected for what this means (it's very useful!)).
    let isSelected event world = World.isAddressSelected event.SubscriberAddress world
    
    /// Take events from an event stream only when the currently selected screen is idling (that
    /// is, there is no screen transition in progress).
    let isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    
    /// Take events from an event stream only when the currently selected screen is transitioning
    /// (that is, there is a screen transition in progress).
    let isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

    /// Take only one event from an event stream per game tick.
    let noMoreThanOncePerTick stream =
        stream |> organize (fun _ w -> w.State.TickTime) |> toFst |> choose

    /// Propagate the event data of an event stream to a value in the world's state.
    let updateWorldStateValue valueSetter stream =
        subscribe (fun a w -> (Cascade, World.updateState (valueSetter a w) w)) stream

    /// Propagate the event data of an event stream to a value in the observer.
    let updateOptSimulantValue valueSetter stream =
        subscribe (fun a w -> (Cascade, World.updateOptSimulant (valueSetter a w) a.SubscriberAddress w)) stream

    let worldStateValue (valueGetter : WorldState -> 'b) (stream : EventStream<WorldStateChangeData, 'o>) =
        filter (fun a w ->
            let oldValue = valueGetter a.Data.OldWorldState
            let newValue = valueGetter (World.getState w)
            oldValue <> newValue)
            stream

    let simulantValue (valueGetter : 'a -> 'b) (stream : EventStream<'a SimulantChangeData, 'o>) =
        filter (fun a w ->
            let oldValue = valueGetter a.Data.OldSimulant
            let newValue = valueGetter (World.getSimulant (Address.changeType<Simulant, 'a> a.PublisherAddress) w)
            oldValue <> newValue)
            stream

    let observeWorldStateValue valueGetter observerAddress =
        observe WorldStateChangeEventAddress observerAddress |>
        worldStateValue valueGetter

    let observeWorldStateValueCyclic valueGetter observerAddress =
        observeWorldStateValue valueGetter observerAddress |>
        noMoreThanOncePerTick

    let observeSimulantValue (valueGetter : 'a -> 'b) (simulantAddress : 'a Address) (observerAddress : 'o Address) =
        let changeEventAddress = Address.changeType<Simulant SimulantChangeData, 'a SimulantChangeData> SimulantChangeEventAddress ->>- simulantAddress
        observe changeEventAddress observerAddress |>
        simulantValue valueGetter

    let observeSimulantValueCyclic valueGetter simulantAddress observerAddress =
        observeSimulantValue valueGetter simulantAddress observerAddress |>
        noMoreThanOncePerTick

[<AutoOpen>]
module EventStreamOperatorsModule =

    /// Propagate a value from the world's state to another value in the world's state.
    let (%->) (valueGetter : WorldState -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        EventStream.observeWorldStateValue valueGetter GameAddress |>
        EventStream.updateWorldStateValue (fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue)

    /// Propagate a value from the world's state to another value in the world's state, but with frame-based cycle-breaking.
    let (%/>) (valueGetter : WorldState -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        EventStream.observeWorldStateValueCyclic valueGetter GameAddress |>
        EventStream.updateWorldStateValue (fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue)

    /// Propagate a value from the world's state to a value in the given simulant.
    let (+->) (valueGetter : WorldState -> 'b) (destinationAddress : 'o Address, valueSetter : 'b -> 'o -> 'o) =
        EventStream.observeWorldStateValue valueGetter destinationAddress |>
        EventStream.updateOptSimulantValue (fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue)

    /// Propagate a value from the world's state to a value in the given simulant, but with frame-based cycle-breaking.
    let (+/>) (valueGetter : WorldState -> 'b) (destinationAddress : 'o Address, valueSetter : 'b -> 'o -> 'o) =
        EventStream.observeWorldStateValueCyclic valueGetter destinationAddress |>
        EventStream.updateOptSimulantValue (fun _ world -> let sourceValue = valueGetter world.State in valueSetter sourceValue)

    /// Propagate a value from the given simulant to a value in the world's state.
    let ( *-> ) (sourceAddress : 'a Address, valueGetter : 'a -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        EventStream.observeSimulantValueCyclic valueGetter sourceAddress GameAddress |>
        EventStream.updateWorldStateValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)

    /// Propagate a value from the given simulant to a value in the world's state, but with frame-based cycle-breaking.
    let ( */> ) (sourceAddress : 'a Address, valueGetter : 'a -> 'b) (valueSetter : 'b -> WorldState -> WorldState) =
        EventStream.observeSimulantValueCyclic valueGetter sourceAddress GameAddress |>
        EventStream.updateWorldStateValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)

    // Propagate a value from the given source simulant to a value in the given destination simulant.
    let (-->) (sourceAddress : 'a Address, valueGetter : 'a -> 'b) (destinationAddress : 'o Address, valueSetter : 'b -> 'o -> 'o) =
        EventStream.observeSimulantValue valueGetter sourceAddress destinationAddress |>
        EventStream.updateOptSimulantValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)

    // Propagate a value from the given source simulant to a value in the given destination simulant, but with frame-based cycle-breaking.
    let (-/>) (sourceAddress : 'a Address, valueGetter : 'a -> 'b) (destinationAddress : 'o Address, valueSetter : 'b -> 'o -> 'o) =
        EventStream.observeSimulantValueCyclic valueGetter sourceAddress destinationAddress |>
        EventStream.updateOptSimulantValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)