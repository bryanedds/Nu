namespace Nu
open Nu

[<AutoOpen>]
module DesyncModule =

    // NOTE: sadly, I think this is not a true monadic bind due to a lack of a 'b type generalization...
    let internal desyncBind (m : 'a list) (f : unit -> 'a list) : 'a list = m @ f ()
    let internal desyncReturn op = [op]
    let internal desyncReturnFrom op = op
    let internal desyncZero () = []
    let rec internal desyncCombine m n = m @ n

    /// The desynchronous computation expression builder.
    type DesyncBuilder () =
        member this.Bind (m, f) = desyncBind m f
        member this.Return op = desyncReturn op
        member this.ReturnFrom op = desyncReturnFrom op
        member this.Zero () = desyncZero ()
        member this.Combine (m, n) = desyncCombine m n

    /// The global desync builder instance.
    let desync = DesyncBuilder ()

module Desync =

    /// Invoke an operation in the context of desynchronization.
    let call op = desync.Return op

    /// Skip in the context of desynchronization.
    let skip () = desyncZero ()

    /// Pass in the context of desynchronization.
    let pass () = call World.handleAsPass

    /// Loop in the context of desynchronization.
    let loop (ts : int seq) (f : int -> 'a list) : 'a list =
        Seq.fold (fun m t -> m @ f t) [] ts

    let private run4 makeSubscription (observable : Observable<'a, 'o>) desync world =
        if not <| List.isEmpty desync then
            let callbackKey = World.makeCallbackKey ()
            let world = World.addCallbackState callbackKey desync world
            let subscriptionKey = World.makeSubscriptionKey ()
            let (eventAddress, unsubscribe, world) = observable.Subscribe world
            let unsubscribe = fun world ->
                let world = World.removeCallbackState callbackKey world
                let world = unsubscribe world
                World.unsubscribe subscriptionKey world
            let subscription = makeSubscription unsubscribe callbackKey
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (unsubscribe, world)
        else (id, world)

    let run6 shouldAdvance advance eventHandling (observable : Observable<'a, 'o>) desync world =
        let makeSubscription unsubscribe callbackKey =
            fun event world ->
                let desync = World.getCallbackState callbackKey world
                match desync with
                | [] -> failwith "Invalid desync value in-flight."
                | head :: tail ->
                    let world = advance event head world
                    let world =
                        if shouldAdvance event then // perhaps also pass world?
                            if not <| List.isEmpty tail then World.addCallbackState callbackKey tail world
                            else unsubscribe world
                        else world
                    (eventHandling, world)
        run4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event and specify its event
    /// handling approach.
    let runWithEventSpecifyingHandling shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> EventHandling * World) list) world =
        let makeSubscription unsubscribe callbackKey =
            fun event world ->
                let desync = World.getCallbackState callbackKey world
                match desync with
                | [] -> failwith "Invalid desync value in-flight."
                | head :: tail ->
                    let (eventHandling, world) = head (event, world)
                    let world =
                        if shouldAdvance event then // perhaps also pass world?
                            if not <| List.isEmpty tail then World.addCallbackState callbackKey tail world
                            else unsubscribe world
                        else world
                    (eventHandling, world)
        run4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Cascade.
    let runWithEventCascading shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        run6 shouldAdvance (fun event desync world -> desync event world) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Resolve.
    let runWithEventResolving shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        run6 shouldAdvance (fun event desync world -> desync event world) Resolve observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runCascading shouldAdvance (observable : Observable<'a, 'o>) (desync : (World -> World) list) world =
        run6 shouldAdvance (fun _ desync world -> desync world) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runResolving shouldAdvance (observable : Observable<'a, 'o>) (desync : (World -> World) list) world =
        run6 shouldAdvance (fun _ desync world -> desync world) Resolve observable desync world