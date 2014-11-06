namespace Nu
open System
open LanguagePrimitives
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module ReactModule =

    /// An observable event in the reactive style.
    /// TODO: I bet there's a monad in here somewhere...
    type [<ReferenceEquality>] 'a Observable =
        { Subscribe : World -> Address * (World -> World) * World
          TypeCarrier : 'a -> unit }

        static member make<'a> subscribe typeCarrier =
            { Subscribe = subscribe; TypeCarrier = typeCarrier }

module React =

    (* Primitive Combinators *)

    (*

    let lifetime (observable : 'a Observable) : 'a Observable =
        let subscriptionKey = World.makeSubscriptionKey ()
        let handleEvent = fun _ world ->
            let world = World.unsubscribe subscriptionKey world
            let world = observable.Unsubscribe world
            (Cascade, world)
        let subscribe = fun world ->
            let world = World.subscribe subscriptionKey (RemovingEventAddress + observable.ReactAddress) observable.ReactAddress handleEvent world
            observable.Subscribe world
        let unsubscribe = fun world -> let world = World.unsubscribe subscriptionKey world in observable.Unsubscribe world
        Observable.make observable.ReactAddress observable.HandleEvent subscribe unsubscribe

    *)

    let from<'a> eventAddress =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let unsubscribe = fun world -> World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'a> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress event.Data world
                (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a) -> () }

    // TODO: is there a better name for this?
    let using handleEvent (observable : 'a Observable) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let (address, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let world = World.subscribe<'a> subscriptionKey address subscriptionAddress handleEvent world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a) -> () }

    let product eventAddress (observable : 'a Observable) : ('a * 'b) Observable =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let subscriptionAddress' = !+ [acstring subscriptionKey']
            let (eventAddress', unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let subscription' = fun event' world ->
                    let eventData = (event.Data, event'.Data)
                    let world = World.publish<'a * 'b> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress eventData world
                    (Cascade, world)
                let world = World.subscribe<'b> subscriptionKey' eventAddress' subscriptionAddress' subscription' world
                (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a * 'b) -> () }

    // TODO: implement this properly
    let sum eventAddress (observable : 'a Observable) : Either<'a, 'b> Observable =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionKey' = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let subscriptionAddress' = !+ [acstring subscriptionKey']
            let (eventAddress', unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = unsubscribe world
                let world = World.unsubscribe subscriptionKey world
                World.unsubscribe subscriptionKey' world
            let subscription = fun event world ->
                let subscription' = fun event' world ->
                    let eventData = (event.Data, event'.Data)
                    let world = World.publish<'a * 'b> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress eventData world
                    (Cascade, world)
                let world = World.subscribe<'b> subscriptionKey' eventAddress' subscriptionAddress' subscription' world
                (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : Either<'a, 'b>) -> () }

    let filter (pred : 'a Event -> World -> bool) (observable : 'a Observable) =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world =
                    if pred event world
                    then World.publish<'a> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress event.Data world
                    else world
                (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a) -> () }

    let map (mapper : 'a Event -> World -> 'b) (observable : 'a Observable) : 'b Observable =
        let subscribe = fun world ->
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world -> let world = unsubscribe world in World.unsubscribe subscriptionKey world
            let subscription = fun event world ->
                let world = World.publish<'b> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress (mapper event world) world
                (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'b) -> () }

    let track4
        (tracker : 'c -> 'a Event -> World -> 'c * bool)
        (transformer : 'c -> 'b)
        (state : 'c)
        (observable : 'a Observable) :
        'b Observable =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
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
                        then World.publish<'b> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress (transformer state) world
                        else world
                    (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'b) -> () }

    let track2
        (tracker : 'a -> 'a Event -> World -> 'a * bool)
        (observable : 'a Observable) :
        'a Observable =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey None world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
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
                        then World.publish<'a> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress state world
                        else world
                    (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a) -> () }

    let track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (observable : 'a Observable) :
        'a Observable =
        let subscribe = fun world ->
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey state world
            let subscriptionKey = World.makeSubscriptionKey ()
            let subscriptionAddress = !+ [acstring subscriptionKey]
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
                        then World.publish<'a> World.sortSubscriptionsNone subscriptionAddress subscriptionAddress event.Data world
                        else world
                    (Cascade, world)
            let world = World.subscribe<'a> subscriptionKey eventAddress subscriptionAddress subscription world
            (subscriptionAddress, unsubscribe, world)
        { Subscribe = subscribe; TypeCarrier = fun (_ : 'a) -> () }

    let subscribe2 world observable =
        observable.Subscribe world |> _bc

    let subscribe eventAddress world observable =
        observable |> using eventAddress |> subscribe2 world

    (* Advanced Combinators *)

    let scan4 (f : 'b -> 'a Event -> World -> 'b) g s (o : 'a Observable) : 'c Observable =
        track4 (fun b a w -> (f b a w, true)) g s o

    let scan2 (f : 'a -> 'a Event -> World -> 'a) (o : 'a Observable) : 'a Observable =
        track2 (fun a a2 w -> (f a a2 w, true)) o

    let scan (f : 'b -> 'a Event -> World -> 'b) s (o : 'a Observable) : 'b Observable =
        scan4 f id s o

    let augment f s o =
        track (fun b w -> (f b w, true)) s o

    let inline average (observable : 'a Observable) : 'a Observable =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a.Data
                let d = d + one ()
                (n / d, n, d))
            Triple.fst
            (zero (), zero (), zero ())
            observable

    let organize (observable : 'a Observable) : ('a option * 'a Set) Observable =
        scan
            (fun (_, s) a _ ->
                if Set.contains a.Data s
                then (None, s)
                else (Some a.Data, Set.add a.Data s))
            (None, Set.empty)
            observable

    let group (observable : 'a Observable) : ('a * bool * 'a Set) Observable =
        scan
            (fun (_, _, s) a _ ->
                if Set.contains a.Data s
                then (a.Data, false, s)
                else (a.Data, true, Set.add a.Data s))
            (Unchecked.defaultof<'a>, false, Set.empty)
            observable
    
    let pairwise (observable : 'a Observable) : ('a * 'a) Observable =
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
    let choose (o : 'a option Observable) = o |> filter (fun o _ -> Option.isSome o.Data) |> map (fun a _ -> Option.get a.Data)
    let mapi o = augment (fun i _ -> i + 1) 0 o
    let max o = scan2 (fun m n _ -> if m < n.Data then n.Data else m) o
    let min o = scan2 (fun m n _ -> if n.Data < m then n.Data else m) o
    let distinct o = organize <| map (fun a _ -> fst a.Data) ^^ choose o

    (* Map Combinators *)

    let unwrap<'s, 'd> event (_ : World) = Event.unwrapASDE<'s, 'd> event
    let unwrapASD<'s, 'd> event (_ : World) = Event.unwrap<'s, 'd> event
    let unwrapASE<'s, 'd> event (_ : World) = Event.unwrapASE<'s, 'd> event
    let unwrapADE<'d> event (_ : World) = Event.unwrapADE<'d> event
    let unwrapAS<'s, 'd> event (_ : World) = Event.unwrapAS<'s, 'd> event
    let unwrapAD<'d> event (_ : World) = Event.unwrapAD<'d> event
    let unwrapAE event (_ : World) = Event.unwrapAE event
    let unwrapSD<'s, 'd> event (_ : World) = Event.unwrapSD<'s, 'd> event
    let unwrapSE<'s, 'd> event (_ : World) = Event.unwrapSE<'s, 'd> event
    let unwrapDE<'d> event (_ : World) = Event.unwrapDE<'d> event
    let unwrapA event (_ : World) = Event.unwrapA event
    let unwrapS<'s, 'd> event (_ : World) = Event.unwrapS<'s, 'd> event
    let unwrapD<'d> event (_ : World) = Event.unwrapD<'d> event

    (* Filter Combinators *)

    let isGamePlaying _ world = World.isGamePlaying world
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    //let isSelected event world = World.isAddressSelected event.ObservableAddress world