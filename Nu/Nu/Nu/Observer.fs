namespace Nu
open System
open LanguagePrimitives
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module ObservableModule =

    /// An observable event in the reactive style.
    /// TODO: I bet there's either a monad or arrow in here...
    type [<ReferenceEquality>] Observable<'a, 'o> =
        { ObserverAddress : 'o Address
          Subscribe : World -> 'a Address * (World -> World) * World }
        static member make<'a> observerAddress subscribe =
            { ObserverAddress = observerAddress; Subscribe = subscribe }

module Observer =

    (* Primitive Combinators *)

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

    let using handleEvent (observable : Observable<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (address, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let world = World.subscribe<'a, 'o> subscriptionKey handleEvent address observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    // TODO: implement this with callback state instead of the rat's nest of subscriptions.
    let product (eventAddress : 'b Address) (observable : Observable<'a, 'o>) : Observable<'a * 'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observable.Subscribe world
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
                let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observable.ObserverAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observable.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let sum (eventAddress : 'b Address) (observable : Observable<'a, 'o>) : Observable<Either<'a, 'b>, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observable.Subscribe world
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
            let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observable.ObserverAddress world
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observable.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let filter (pred : Event<'a, 'o> -> World -> bool) (observable : Observable<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world =
                    if pred event world
                    then World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let map (mapper : Event<'a, 'o> -> World -> 'b) (observable : Observable<'a, 'o>) : Observable<'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'b, Simulant> World.sortSubscriptionsNone (mapper event world) subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let track4
        (tracker : 'c -> Event<'a, 'o> -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (observable : Observable<'a, 'o>) :
        Observable<'b, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'b> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
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
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let track2
        (tracker : 'a -> Event<'a, 'o> -> World -> 'a * bool)
        (observable : Observable<'a, 'o>) :
        Observable<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
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
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (observable : Observable<'a, 'o>) :
        Observable<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
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
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let subscribeWithUnsub2 observable world =
        observable.Subscribe world |> _bc

    let subscribe2 observable world =
        subscribeWithUnsub2 observable world |> snd

    let subscribeWithUnsub handleEvent observable world =
        observable |> using handleEvent |> subscribeWithUnsub2 <| world

    let subscribe handleEvent observable world =
        subscribeWithUnsub handleEvent observable world |> snd

    let until eventAddress (observable : Observable<'a, 'o>) : Observable<'a, 'o> =
        let subscribe = fun world ->
            let eventKey = World.makeSubscriptionKey ()
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ntoa<'a> <| acstring subscriptionKey
            let (eventAddress', unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe eventKey world
            let handleEvent = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = World.subscribe eventKey handleEvent eventAddress observable.ObserverAddress world
            let subscription = fun event world ->
                let world = World.publish<'a, Simulant> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress' observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let lifetime (observable : Observable<'a, 'o>) : Observable<'a, 'o> =
        until (RemovingEventAddress ->>- observable.ObserverAddress) observable

    let monitorWithUnsub eventAddress observable world =
        (observable |> lifetime |> subscribeWithUnsub eventAddress) world

    let monitor eventAddress observable world =
        monitorWithUnsub eventAddress observable world |> snd
    
    (* Advanced Combinators *)

    let scan4 (f : 'b -> Event<'a, 'o> -> World -> 'b) g s (o : Observable<'a, 'o>) : Observable<'c, 'o> =
        track4 (fun b a w -> (f b a w, true)) g s o

    let scan2 (f : 'a -> Event<'a, 'o> -> World -> 'a) (o : Observable<'a, 'o>) : Observable<'a, 'o> =
        track2 (fun a a2 w -> (f a a2 w, true)) o

    let scan (f : 'b -> Event<'a, 'o> -> World -> 'b) s (o : Observable<'a, 'o>) : Observable<'b, 'o> =
        scan4 f id s o

    let inline average (observable : Observable<'a, 'o>) : Observable<'a, 'o> =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            observable

    let organize f (observable : Observable<'a, 'o>) : Observable<('a * 'b) option * Map<'b, 'a>, 'o> =
        scan
            (fun (_, m) a w ->
                let b = f a w
                if Map.containsKey b m
                then (None, m)
                else (Some (a.Data, b), Map.add b a.Data m))
            (None, Map.empty)
            observable

    let groupBy by (observable : Observable<'a, 'o>) : Observable<'b * bool * 'b Set, 'o> =
        scan
            (fun (_, _, s) a w ->
                let b = by a w
                if Set.contains b s
                then (b, false, s)
                else (b, true, Set.add b s))
            (Unchecked.defaultof<'b>, false, Set.empty)
            observable

    let group (observable : Observable<'a, 'o>) : Observable<'a * bool * 'a Set, 'o> =
        groupBy (fun a _ -> a.Data) observable

    let pairwise (observable : Observable<'a, 'o>) : Observable<'a * 'a, 'o> =
        track4
            (fun (o, _) a _ -> ((o, a.Data), Option.isSome o))
            (fun (o, c) -> (Option.get o, c))
            (None, Unchecked.defaultof<'a>)
            observable

    let inline sumN o = scan2 (fun n a _ -> n + a.Data) o
    let inline productN o = scan2 (fun n a _ -> n * a.Data) o
    let toFst o = map (fun a _ -> fst a.Data) o
    let toSnd o = map (fun a _ -> snd a.Data) o
    let withFst mapper o = map (fun a _ -> (mapper <| fst a.Data, snd a.Data)) o
    let withSnd mapper o = map (fun a _ -> (fst a.Data, mapper <| snd a.Data)) o
    let duplicate o = map (fun a _ -> (a.Data, a.Data)) o
    let take n o = track (fun m _ -> (m + 1, m < n)) 0 o
    let skip n o = track (fun m _ -> (m + 1, m >= n)) 0 o
    let head o = take 1 o
    let tail o = skip 1 o
    let nth n o = o |> skip n |> head
    let search p o = o |> filter p |> head
    let choose (o : Observable<'a option, 'o>) = o |> filter (fun opt _ -> Option.isSome opt.Data) |> map (fun a _ -> Option.get a.Data)
    let max o = scan2 (fun n a _ -> if n < a.Data then a.Data else n) o
    let min o = scan2 (fun n a _ -> if a.Data < n then a.Data else n) o
    let distinctBy by o = o |> organize by |> toFst |> choose
    let distinct o = distinctBy (fun a -> a.Data) o

    (* Special Combinators *)

    let isGamePlaying _ world = World.isGamePlaying world
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    let isSelected event world = World.isAddressSelected event.SubscriberAddress world
    let isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    let isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world

    let noMoreThanOncePerTick o =
        o |> organize (fun _ w -> w.State.TickTime) |> toFst |> choose

    let simulantValue<'a, 's, 't when 'a : equality and 's :> Simulant and 't :> Simulant>
        (valueGetter : 's -> 'a)
        (o : Observable<'s SimulantChangeData, 't>) =
        filter (fun a w ->
            let oldValue = valueGetter a.Data.OldSimulant
            let newValue = valueGetter (World.getSimulant (Address.changeType<Simulant, 's> a.PublisherAddress) w)
            oldValue <> newValue)
            o

    let observeSimulantValue<'a, 's, 't when 'a : equality and 's :> Simulant and 't :> Simulant>
        (valueGetter : 's -> 'a)
        (simulantAddress : 's Address)
        (observerAddress : 't Address) =
        let changeEventAddress = Address.changeType<Simulant SimulantChangeData, 's SimulantChangeData> SimulantChangeEventAddress ->>- simulantAddress
        let o = observe<'s SimulantChangeData, 't> changeEventAddress observerAddress
        simulantValue valueGetter o

    let observeSimulantValueCyclic valueGetter simulantAddress observerAddress =
        observeSimulantValue valueGetter simulantAddress observerAddress |> noMoreThanOncePerTick

    let updateOptSimulantValue valueSetter o =
        subscribe (fun a w -> (Cascade, World.updateOptSimulant (valueSetter a w) a.SubscriberAddress w)) o

    let updateSimulantValue valueSetter o =
        subscribe (fun a w -> (Cascade, World.updateSimulant (valueSetter a w) a.SubscriberAddress w)) o

    let propagateSimulantValue<'a, 's, 't when 'a : equality and 's :> Simulant and 't :> Simulant>
        (sourceAddress : 's Address)
        (valueGetter : 's -> 'a)
        (destinationAddress : 't Address)
        (valueSetter : 'a -> 't -> 't) =
        let o = observeSimulantValue valueGetter sourceAddress destinationAddress
        updateSimulantValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)
            o

    let propagateSimulantValueCyclic<'a, 's, 't when 'a : equality and 's :> Simulant and 't :> Simulant>
        (sourceAddress : 's Address)
        (valueGetter : 's -> 'a)
        (destinationAddress : 't Address)
        (valueSetter : 'a -> 't -> 't) =
        let o = observeSimulantValueCyclic valueGetter sourceAddress destinationAddress
        updateSimulantValue (fun _ world ->
            let sourceSimulant = World.getSimulant sourceAddress world
            let sourceSimulantValue = valueGetter sourceSimulant
            valueSetter sourceSimulantValue)
            o

    let inline (-->) (sourceAddress, valueGetter) (destinationAddress, valueSetter) =
        propagateSimulantValue sourceAddress valueGetter destinationAddress valueSetter

    let inline (-|>) (sourceAddress, valueGetter) (destinationAddress, valueSetter) =
        propagateSimulantValueCyclic sourceAddress valueGetter destinationAddress valueSetter