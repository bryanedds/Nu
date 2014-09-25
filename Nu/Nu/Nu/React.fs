namespace Nu
open System
open Prime
open Nu.Constants

module React =

    type [<ReferenceEquality>] 'a Reactor =
        { SubscriberAddress : Address
          Handler : 'a -> World -> EventHandling * World
          Unsubscriber : World -> World
          World : World }

        static member make<'a> subscriberAddress (handler : 'a -> World -> EventHandling * World) unsubscriber world =
            { SubscriberAddress = subscriberAddress; Handler = handler; Unsubscriber = unsubscriber; World = world }

    // event unwrap combinators
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

    // other common combinators
    let isGamePlaying _ world = World.isGamePlaying world
    let isPhysicsRunning _ world = World.isPhysicsRunning world
    let isSelected event world = World.isAddressSelected event.SubscriberAddress world

    let map (mapper : 'a -> World -> 'b) (reactor : 'b Reactor) : 'a Reactor =
        let handler = fun value world -> reactor.Handler (mapper value world) world
        Reactor.make reactor.SubscriberAddress handler reactor.Unsubscriber reactor.World

    let filter pred (reactor : 'a Reactor) : 'a Reactor =
        let handler = fun (value : 'a) world ->
            if pred value world then reactor.Handler value world
            else (Propagate, world)
        Reactor.make reactor.SubscriberAddress handler reactor.Unsubscriber reactor.World

    let mapFst (mapper : 'a -> World -> 'b) (reactor : ('b * 'u) Reactor) : ('a * 'u) Reactor =
        map (fun (a, u) world -> (mapper a world, u)) reactor

    let mapSnd (mapper : 'a -> World -> 'b) (reactor : ('u * 'b) Reactor) : ('u * 'a) Reactor =
        map (fun (u, a) world -> (u, mapper a world)) reactor

    let mapLeft (mapper : 'a -> World -> 'b) (reactor : Either<'b, 'u> Reactor) : Either<'a, 'u> Reactor =
        map
            (fun either world ->
                match either with
                | Right u -> Right u
                | Left a -> Left <| mapper a world)
            reactor

    let mapRight (mapper : 'a -> World -> 'b) (reactor : Either<'u, 'b> Reactor) : Either<'u, 'a> Reactor =
        map
            (fun either world ->
                match either with
                | Right a -> Right <| mapper a world
                | Left u -> Left u)
            reactor

    let fold (folder : 'a -> 'b -> World -> 'a) (state : 'a) (reactor : 'a Reactor) : 'b Reactor =
        let key = Guid.NewGuid ()
        let world = World.addCallbackState key state reactor.World
        let unsubscriber = fun world -> let world = World.removeCallbackState key world in reactor.Unsubscriber world
        let handler = fun (value : 'b) world ->
            let state = World.getCallbackState key world
            let state = folder state value world
            let world = World.addCallbackState key state world
            reactor.Handler state world
        Reactor.make reactor.SubscriberAddress handler unsubscriber world

    let andWith eventName (reactor : ('a * Event) Reactor) : 'a Reactor =
        let key = Guid.NewGuid ()
        let unsubscriber = fun world -> World.unsubscribe key world
        let handler = fun event world ->
            let key = Guid.NewGuid ()
            let handler3 = fun event2 world -> let world = unsubscriber world in reactor.Handler (event, event2) world
            let world = World.subscribe key eventName reactor.SubscriberAddress (CustomSub handler3) world
            (Propagate, world)
        let unsubscriber world = let world = unsubscriber world in reactor.Unsubscriber world
        Reactor.make reactor.SubscriberAddress handler unsubscriber reactor.World

    let orWith eventName (reactor : Either<Event, 'a> Reactor) : 'a Reactor =
        let key = Guid.NewGuid ()
        let handlerRight = fun value world -> reactor.Handler (Right value) world
        let handlerLeft = fun value world -> reactor.Handler (Left value) world
        let world = World.subscribe key eventName reactor.SubscriberAddress (CustomSub handlerLeft) reactor.World
        let unsubscriber world = let world = World.unsubscribe key world in reactor.Unsubscriber world
        Reactor.make reactor.SubscriberAddress handlerRight unsubscriber world
    
    let lifetime (reactor : 'a Reactor) : 'a Reactor =
        let key = Guid.NewGuid ()
        let handler = fun _ world ->
            let world = World.unsubscribe key world
            let world = reactor.Unsubscriber world
            (Propagate, world)
        let world = World.subscribe key (RemovingEventName + reactor.SubscriberAddress) reactor.SubscriberAddress (CustomSub handler) reactor.World
        let unsubscriber = fun world -> let world = World.unsubscribe key world in reactor.Unsubscriber world
        Reactor.make reactor.SubscriberAddress reactor.Handler unsubscriber world

    let using subscriberAddress handler world =
        Reactor.make subscriberAddress handler (fun world -> world) world

    let subscribeCombinator eventName (reactor : Event Reactor) : Event Reactor =
        let key = Guid.NewGuid ()
        let unsubscriber = fun world -> let world = World.unsubscribe key world in reactor.Unsubscriber world
        let world = World.subscribe key eventName reactor.SubscriberAddress (CustomSub reactor.Handler) reactor.World
        Reactor.make reactor.SubscriberAddress reactor.Handler unsubscriber world

    let observeCombinator eventName (reactor : Event Reactor) : Event Reactor =
        lifetime ^^ subscribeCombinator eventName ^^ reactor

    let subscribe eventName reactor =
        (subscribeCombinator eventName reactor).World

    let observe eventName reactor =
        (observeCombinator eventName reactor).World