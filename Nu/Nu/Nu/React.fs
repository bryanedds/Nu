namespace Nu
open System
open LanguagePrimitives
open Prime
open Nu.Constants

[<AutoOpen>]
module ReactModule =

    type [<ReferenceEquality>] 'a Reactor =
        { ReactAddress : Address
          Handler : 'a -> World -> EventHandling * World
          Unsubscriber : World -> World
          World : World }

        static member make<'a> subscriberAddress (handler : 'a -> World -> EventHandling * World) unsubscriber world =
            { ReactAddress = subscriberAddress; Handler = handler; Unsubscriber = unsubscriber; World = world }

module React =

    (* Subscribing Combinators *)

    let track4
        (tracker : 'c -> 'a -> World -> 'c * bool)
        (tranformer : 'c -> 'b)
        (state : 'c)
        (reactor : 'b Reactor) :
        'a Reactor =
        let key = World.makeCallbackKey ()
        let world = World.addCallbackState key state reactor.World
        let unsubscriber = fun world -> let world = World.removeCallbackState key world in reactor.Unsubscriber world
        let handler = fun value world ->
            let state = World.getCallbackState key world
            let (state, tracked) = tracker state value world
            let world = World.addCallbackState key state world
            if tracked then reactor.Handler (tranformer state) world
            else (Propagate, world)
        Reactor.make reactor.ReactAddress handler unsubscriber world

    let track2
        (tracker : 'a -> 'a -> World -> 'a * bool)
        (reactor : 'a Reactor) :
        'a Reactor =
        let key = World.makeCallbackKey ()
        let world = World.addCallbackState key None reactor.World
        let unsubscriber = fun world -> let world = World.removeCallbackState key world in reactor.Unsubscriber world
        let handler = fun value world ->
            let optState = World.getCallbackState key world
            let state = match optState with Some state -> state | None -> value
            let (state, tracked) = tracker state value world
            let world = World.addCallbackState key (Some state) world
            if tracked then reactor.Handler state world
            else (Propagate, world)
        Reactor.make reactor.ReactAddress handler unsubscriber world

    let track
        (tracker : 'b -> World -> 'b * bool)
        (state : 'b)
        (reactor : 'a Reactor) :
        'a Reactor =
        let key = World.makeCallbackKey ()
        let world = World.addCallbackState key state reactor.World
        let unsubscriber = fun world -> let world = World.removeCallbackState key world in reactor.Unsubscriber world
        let handler = fun value world ->
            let state = World.getCallbackState key world
            let (state, tracked) = tracker state world
            let world = World.addCallbackState key state world
            if tracked then reactor.Handler value world
            else (Propagate, world)
        Reactor.make reactor.ReactAddress handler unsubscriber world

    let zip eventName (reactor : ('a * Event) Reactor) : 'a Reactor =
        let key = World.makeSubscriptionKey ()
        let unsubscriber = fun world -> World.unsubscribe key world
        let handler = fun event world ->
            let key = World.makeSubscriptionKey ()
            let handler3 = fun event2 world -> let world = unsubscriber world in reactor.Handler (event, event2) world
            let world = World.subscribe key eventName reactor.ReactAddress (CustomSub handler3) world
            (Propagate, world)
        let unsubscriber world = let world = unsubscriber world in reactor.Unsubscriber world
        Reactor.make reactor.ReactAddress handler unsubscriber reactor.World

    let choice eventName (reactor : Either<Event, 'a> Reactor) : 'a Reactor =
        let key = World.makeSubscriptionKey ()
        let handlerRight = fun value world -> reactor.Handler (Right value) world
        let handlerLeft = fun value world -> reactor.Handler (Left value) world
        let world = World.subscribe key eventName reactor.ReactAddress (CustomSub handlerLeft) reactor.World
        let unsubscriber world = let world = World.unsubscribe key world in reactor.Unsubscriber world
        Reactor.make reactor.ReactAddress handlerRight unsubscriber world

    let lifetime (reactor : 'a Reactor) : 'a Reactor =
        let key = World.makeSubscriptionKey ()
        let handler = fun _ world ->
            let world = World.unsubscribe key world
            let world = reactor.Unsubscriber world
            (Propagate, world)
        let world = World.subscribe key (RemovingEventName + reactor.ReactAddress) reactor.ReactAddress (CustomSub handler) reactor.World
        let unsubscriber = fun world -> let world = World.unsubscribe key world in reactor.Unsubscriber world
        Reactor.make reactor.ReactAddress reactor.Handler unsubscriber world

    let subscribe eventName (reactor : Event Reactor) : Event Reactor =
        let key = World.makeSubscriptionKey ()
        let unsubscriber = fun world -> let world = World.unsubscribe key world in reactor.Unsubscriber world
        let world = World.subscribe key eventName reactor.ReactAddress (CustomSub reactor.Handler) reactor.World
        Reactor.make reactor.ReactAddress reactor.Handler unsubscriber world

    let observe eventName (reactor : Event Reactor) : Event Reactor =
        lifetime ^^ subscribe eventName reactor

    (* Primitive Combinators *)
    
    let map (mapper : 'a -> World -> 'b) (reactor : 'b Reactor) : 'a Reactor =
        let handler = fun value world -> reactor.Handler (mapper value world) world
        Reactor.make reactor.ReactAddress handler reactor.Unsubscriber reactor.World

    let mapFst (mapper : 'a -> World -> 'b) (reactor : ('b * 'c) Reactor) : ('a * 'c) Reactor =
        map (fun (a, c) world -> (mapper a world, c)) reactor

    let mapSnd (mapper : 'a -> World -> 'b) (reactor : ('c * 'b) Reactor) : ('c * 'a) Reactor =
        map (fun (c, a) world -> (c, mapper a world)) reactor

    let mapOverLeft (mapper : 'a -> World -> 'b) (reactor : Either<'b, 'c> Reactor) : Either<'a, 'c> Reactor =
        map
            (fun either world ->
                match either with
                | Right c -> Right c
                | Left a -> Left <| mapper a world)
            reactor

    let mapOverRight (mapper : 'a -> World -> 'b) (reactor : Either<'c, 'b> Reactor) : Either<'c, 'a> Reactor =
        map
            (fun either world ->
                match either with
                | Right a -> Right <| mapper a world
                | Left c -> Left c)
            reactor

    let filter pred (reactor : 'a Reactor) : 'a Reactor =
        let handler = fun (value : 'a) world ->
            if pred value world then reactor.Handler value world
            else (Propagate, world)
        Reactor.make reactor.ReactAddress handler reactor.Unsubscriber reactor.World

    let augment f s r = track (fun b w -> (f b w, true)) s r
    let scan4 (f : 'b -> 'a -> World -> 'b) g s (r : 'c Reactor) = track4 (fun c a w -> (f c a w, true)) g s r
    let scan2 f r = track2 (fun a a2 w -> (f a a2 w, true)) r
    let scan (f : 'b -> 'a -> World -> 'b) s r = scan4 f id s r

    (* Advanced Combinators *)

    let inline average (reactor : 'a Reactor) : 'a Reactor =
        scan4
            (fun (_ : 'a, n : 'a, d : 'a) a _ ->
                let n = n + a
                let d = d + GenericOne
                (n / d, n, d))
            Triple.fst
            (GenericZero, GenericZero, GenericZero)
            reactor

    let organize (reactor : ('a option * ('a Set)) Reactor) : 'a Reactor =
        scan
            (fun (_, s) a _ ->
                if Set.contains a s
                then (None, s)
                else (Some a, Set.add a s))
            (None, Set.empty)
            reactor

    let group (reactor : ('a * bool * ('a Set)) Reactor) : 'a Reactor =
        scan
            (fun (_, _, s) a _ ->
                if Set.contains a s
                then (a, false, s)
                else (a, true, Set.add a s))
            (Unchecked.defaultof<'a>, false, Set.empty)
            reactor
    
    let pairwise (r : ('a * 'a) Reactor) : 'a Reactor =
        track4
            (fun (o, _) a _ -> ((o, a), Option.isSome o))
            (fun (o, c) -> (Option.get o, c))
            (None, Unchecked.defaultof<'a>)
            r

    let inline sum r = scan2 (fun m n _ -> m + n) r
    let take n r = track (fun m _ -> (m + 1, m < n)) 0 r
    let skip n r = track (fun m _ -> (m + 1, m >= n)) 0 r
    let head r = take 1 r
    let tail r = skip 1 r
    let nth n r = skip n ^^ head r
    let search p r = filter p ^^ head r
    let choose r = filter (fun o _ -> Option.isSome o) r
    let mapi r = augment (fun i _ -> i + 1) 0 r
    let max r = scan2 (fun m n _ -> if m < n then n else m) r
    let min r = scan2 (fun m n _ -> if n < m then n else m) r
    let distinct r = organize ^^ map (fun a _ -> fst a) ^^ choose r

    (* Map Combinators *)

    let unwrap<'s, 'd> event (_ : World) = Event.unwrapASDE<'s, 'd> event
    let unwrapASD<'s, 'd> event (_ : World) = Event.unwrap<'s, 'd> event
    let unwrapASE<'s> event (_ : World) = Event.unwrapASE<'s> event
    let unwrapADE<'d> event (_ : World) = Event.unwrapADE<'d> event
    let unwrapAS<'s> event (_ : World) = Event.unwrapAS<'s> event
    let unwrapAD<'d> event (_ : World) = Event.unwrapAD<'d> event
    let unwrapAE event (_ : World) = Event.unwrapAE event
    let unwrapSD<'s, 'd> event (_ : World) = Event.unwrapSD<'s, 'd> event
    let unwrapSE<'s> event (_ : World) = Event.unwrapSE<'s> event
    let unwrapDE<'d> event (_ : World) = Event.unwrapDE<'d> event
    let unwrapA event (_ : World) = Event.unwrapA event
    let unwrapS<'s> event (_ : World) = Event.unwrapS<'s> event
    let unwrapD<'d> event (_ : World) = Event.unwrapD<'d> event
    let unwrapV event (_ : World) : 'd = UserData.get ^^ Event.unwrapD event

    (* Filter Combinators *)

    let isGamePlaying _ world = World.isGamePlaying world
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    let isSelected event world = World.isAddressSelected event.ReactAddress world

    (* Initializing Combinator *)

    let unto subscriberAddress handler world = Reactor.make subscriberAddress handler id world