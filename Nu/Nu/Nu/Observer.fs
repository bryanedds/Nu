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
    /// TODO: I bet there's a monad in here somewhere...
    type [<ReferenceEquality>] Observable<'a, 'o> =
        { ObserverAddress : 'o Address
          Subscribe : World -> 'a Address * (World -> World) * World }
        static member make<'a> observerAddress subscribe =
            { ObserverAddress = observerAddress; Subscribe = subscribe }

module Observer =

    (* Primitive Combinators *)

    let observe<'a, 'o> (eventAddress : 'a Address) (observerAddress : 'o Address) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
            let unsubscribe = fun world -> World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'a, obj> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observerAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observerAddress; Subscribe = subscribe }

    let using handleEvent (observable : Observable<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
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
            let subscriptionAddress'' = ltoa<'a * 'b> [acstring subscriptionKey'']
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let subscription' = fun event' world ->
                    let eventData = (event.Data, event'.Data)
                    let world = World.publish<'a * 'b, obj> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                    let world = World.unsubscribe subscriptionKey' world
                    (Cascade, world)
                let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observable.ObserverAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observable.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    // NOTE: this could also be called coproduct.
    let sum (eventAddress : 'b Address) (observable : Observable<'a, 'o>) : Observable<Either<'a, 'b>, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionKey'' = World.makeSubscriptionKey ()
            let (subscriptionAddress, unsubscribe, world) = observable.Subscribe world
            let subscriptionAddress' = eventAddress
            let subscriptionAddress'' = ltoa<Either<'a, 'b>> [acstring subscriptionKey'']
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let eventData = Left event.Data
                let world = World.publish<Either<'a, 'b>, obj> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                (Cascade, world)
            let subscription' = fun event world ->
                let eventData = Right event.Data
                let world = World.publish<Either<'a, 'b>, obj> World.sortSubscriptionsNone eventData subscriptionAddress'' event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'b, 'o> subscriptionKey' subscription' subscriptionAddress' observable.ObserverAddress world
            let world = World.subscribe<'a, 'o> subscriptionKey subscription subscriptionAddress observable.ObserverAddress world
            (subscriptionAddress'', unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let filter (pred : 'a Event -> World -> bool) (observable : Observable<'a, 'o>) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world =
                    if pred event world
                    then World.publish<'a, obj> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let map (mapper : 'a Event -> World -> 'b) (observable : Observable<'a, 'o>) : Observable<'b, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'b> [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'b, obj> World.sortSubscriptionsNone (mapper event world) subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let track4
        (tracker : 'c -> 'a Event -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (observable : Observable<'a, 'o>) :
        Observable<'b, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'b> [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription =
                fun event world ->
                    let state = World.getCallbackState callbackKey world
                    let (state, tracked) = tracker state event world
                    let world = World.addCallbackState callbackKey state world
                    let world =
                        if tracked
                        then World.publish<'b, obj> World.sortSubscriptionsNone (transformer state) subscriptionAddress event.PublisherAddress world
                        else world
                    (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let track2
        (tracker : 'a -> 'a Event -> World -> 'a * bool)
        (observable : Observable<'a, 'o>) :
        Observable<'a, 'o> =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription =
                fun event world ->
                    let optState = World.getCallbackState callbackKey world
                    let state = match optState with Some state -> state | None -> event.Data
                    let (state, tracked) = tracker state event world
                    let world = World.addCallbackState callbackKey state world
                    let world =
                        if tracked
                        then World.publish<'a, obj> World.sortSubscriptionsNone state subscriptionAddress event.PublisherAddress world
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
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription =
                fun event world ->
                    let state = World.getCallbackState callbackKey world
                    let (state, tracked) = tracker state world
                    let world = World.addCallbackState callbackKey state world
                    let world =
                        if tracked
                        then World.publish<'a, obj> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                        else world
                    (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let subscribe2 world observable =
        observable.Subscribe world |> _bc

    let subscribe handleEvent world observable =
        observable |> using handleEvent |> subscribe2 world

    let lifetime (observable : Observable<'a, 'o>) : Observable<'a, 'o> =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionAddress = ltoa<'a> [acstring subscriptionKey]
            let subscriptionAddress' = RemovingEventAddress ->>- observable.ObserverAddress
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let handleRemoving = fun _ world -> let world = unsubscribe world in (Cascade, world)
            let world = World.subscribe subscriptionKey' handleRemoving subscriptionAddress' observable.ObserverAddress world
            let subscription = fun event world ->
                let world = World.publish<'a, obj> World.sortSubscriptionsNone event.Data subscriptionAddress event.PublisherAddress world
                (Cascade, world)
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (subscriptionAddress, unsubscribe, world)
        { ObserverAddress = observable.ObserverAddress; Subscribe = subscribe }

    let monitor eventAddress world observable =
        observable |> lifetime |> subscribe eventAddress world

    (* Advanced Combinators *)

    let scan4 (f : 'b -> 'a Event -> World -> 'b) g s (o : Observable<'a, 'o>) : Observable<'c, 'o> =
        track4 (fun b a w -> (f b a w, true)) g s o

    let scan2 (f : 'a -> 'a Event -> World -> 'a) (o : Observable<'a, 'o>) : Observable<'a, 'o> =
        track2 (fun a a2 w -> (f a a2 w, true)) o

    let scan (f : 'b -> 'a Event -> World -> 'b) s (o : Observable<'a, 'o>) : Observable<'b, 'o> =
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

    let organize (observable : Observable<'a, 'o>) : Observable<'a option * 'a Set, 'o> =
        scan
            (fun (_, s) a _ ->
                if Set.contains a.Data s
                then (None, s)
                else (Some a.Data, Set.add a.Data s))
            (None, Set.empty)
            observable

    let group (observable : Observable<'a, 'o>) : Observable<'a * bool * 'a Set, 'o> =
        scan
            (fun (_, _, s) a _ ->
                if Set.contains a.Data s
                then (a.Data, false, s)
                else (a.Data, true, Set.add a.Data s))
            (Unchecked.defaultof<'a>, false, Set.empty)
            observable
    
    let pairwise (observable : Observable<'a, 'o>) : Observable<'a * 'a, 'o> =
        track4
            (fun (o, _) a _ -> ((o, a.Data), Option.isSome o))
            (fun (o, c) -> (Option.get o, c))
            (None, Unchecked.defaultof<'a>)
            observable

    let inline sumOf o = scan2 (fun m n _ -> m + n) o
    let inline productOf o = scan2 (fun m n _ -> m * n) o
    let take n o = track (fun m _ -> (m + 1, m < n)) 0 o
    let skip n o = track (fun m _ -> (m + 1, m >= n)) 0 o
    let head o = take 1 o
    let tail o = skip 1 o
    let nth n o = o |> skip n |> head
    let search p o = o |> filter p |> head
    let choose (o : Observable<'a option, 'o>) = o |> filter (fun o _ -> Option.isSome o.Data) |> map (fun a _ -> Option.get a.Data)
    let max o = scan2 (fun m n _ -> if m < n.Data then n.Data else m) o
    let min o = scan2 (fun m n _ -> if n.Data < m then n.Data else m) o
    let distinct o = o |> choose |> map (fun a _ -> fst a.Data) |> organize

    (* Map Combinators *)

    // TODO - more of these?

    (* Filter Combinators *)

    let isGamePlaying _ world = World.isGamePlaying world
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    let isSelected event world = World.isAddressSelected event.SubscriberAddress world
    let isSelectedScreenIdling _ world = World.isSelectedScreenIdling world
    let isSelectedScreenTransitioning _ world = World.isSelectedScreenTransitioning world