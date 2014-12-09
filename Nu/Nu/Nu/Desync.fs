namespace Nu
open Prime
open Nu

module Unsync =

    type [<NoComparison; NoEquality>] Unsync<'s, 'a> =
        Unsync of ('s -> 's * Either<Unsync<'s, 'a>, 'a>)

    let step (m : Unsync<'s, 'a>) (s : 's) : 's * Either<Unsync<'s, 'a>, 'a> =
        match m with Unsync f -> f s

    let rec run (m : Unsync<'s, 'a>) (s : 's) : ('s * 'a) =
        match step m s with
        | (s', Left m') -> run m' s'
        | (s', Right v) -> (s', v)

    let get : Unsync<'s, 's> =
        Unsync (fun s -> (s, Right s))

    let put (s : 's) : Unsync<'s, unit> =
        Unsync (fun _ -> (s, Right ()))

    let wait m : Unsync<'s, 'a> =
        Unsync (fun s -> (s, Left m))

    let returnM (a : 'a) : Unsync<'s, 'a> =
        Unsync (fun s -> (s, Right a))
        
    let rec bind (m : Unsync<'s, 'a>) (cont : 'a -> Unsync<'s, 'b>) : Unsync<'s, 'b> =
        Unsync (fun s ->
            match step m s with
            | (s', Left m') -> (s', Left (bind m' cont))
            | (s', Right v) -> step (cont v) s')

    type UnsyncBuilder () =

        member this.Return op = returnM op
        member this.Bind (m, cont) = bind m cont

    let unsync =
        UnsyncBuilder ()

    let pass () =
        wait <| returnM ()

    let lift expr =
        unsync {
            let! s = get
            let s = expr s
            do! put s }

    let respond expr =
        wait <| lift expr

    let rec loop state advance pred expr =
        if pred state then
            let state = advance state
            unsync {
                do! expr state
                do! loop state advance pred expr }
        else returnM ()

    /// Loop in the context of desynchronization.
    //let loop (ts : 't seq) (cont : 't -> 'a list) : 'a list =
    //    Seq.fold (fun ops t -> ops @ cont t) [] ts

    let private runDesync4 makeSubscription (observable : Observable<'a, 'o>) (desync : Unsync<World, unit>) world =
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

    let runDesync4' eventHandling (observable : Observable<'a, 'o>) desync world =
        let makeSubscription unsubscribe callbackKey =
            fun _ world ->
                let desync = World.getCallbackState callbackKey world
                let (world, desyncResult) = step desync world
                match desyncResult with
                | Left desyncNext ->
                    let world = World.addCallbackState callbackKey desyncNext world
                    (eventHandling, world)
                | Right () ->
                    let world = unsubscribe world
                    (eventHandling, world)
        runDesync4 makeSubscription observable desync world

    (*
    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event and specify its event
    /// handling approach.
    let runDesyncReferencingEventsSpecifyingHandling shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> EventHandling * World) list) world =
        let makeSubscription unsubscribe callbackKey =
            fun event world ->
                let desync = World.getCallbackState callbackKey world
                match desync with
                | [] -> failwith "Invalid desync value in-flight."
                | head :: tail ->
                    let (eventHandling, world) = head event world
                    let world =
                        if shouldAdvance event then // perhaps also pass world?
                            if not <| List.isEmpty tail then World.addCallbackState callbackKey tail world
                            else unsubscribe world
                        else world
                    (eventHandling, world)
        runDesync4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Cascade.
    let runDesyncReferencingEventsAssumingCascade shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        runDesync6 shouldAdvance (fun event desync world -> desync event world) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Resolve.
    let runDesyncReferencingEventsAssumingResolve shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        runDesync6 shouldAdvance (fun event desync world -> desync event world) Resolve observable desync world*)

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runDesyncAssumingCascade (observable : Observable<'a, 'o>) desync world =
        runDesync4' (*fun _ desync world -> desync world*) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runDesyncAssumingResolve (observable : Observable<'a, 'o>) desync world =
        runDesync4' (*fun _ desync world -> desync world*) Resolve observable desync world

(*[<AutoOpen>]
module DesyncModule =

    // NOTE: sadly, I think this is not a true monadic bind due to a lack of a 'b type generalization...
    let internal desyncBind (ops : 'a list) (cont : unit -> 'a list) : 'a list = ops @ cont ()
    let internal desyncReturn op = [op]
    let internal desyncReturnFrom op = op
    let internal desyncZero () = []
    let rec internal desyncCombine ops n = ops @ n

    /// The desynchronous computation expression builder.
    type DesyncBuilder () =
        member this.Bind (ops, cont) = desyncBind ops cont
        member this.Return op = desyncReturn op
        member this.ReturnFrom op = desyncReturnFrom op
        member this.Zero () = desyncZero ()
        member this.Combine (ops, n) = desyncCombine ops n

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
    let loop (ts : 't seq) (cont : 't -> 'a list) : 'a list =
        Seq.fold (fun ops t -> ops @ cont t) [] ts

    let private runDesync4 makeSubscription (observable : Observable<'a, 'o>) desync world =
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

    let runDesync6 shouldAdvance advance eventHandling (observable : Observable<'a, 'o>) desync world =
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
        runDesync4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event and specify its event
    /// handling approach.
    let runDesyncReferencingEventsSpecifyingHandling shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> EventHandling * World) list) world =
        let makeSubscription unsubscribe callbackKey =
            fun event world ->
                let desync = World.getCallbackState callbackKey world
                match desync with
                | [] -> failwith "Invalid desync value in-flight."
                | head :: tail ->
                    let (eventHandling, world) = head event world
                    let world =
                        if shouldAdvance event then // perhaps also pass world?
                            if not <| List.isEmpty tail then World.addCallbackState callbackKey tail world
                            else unsubscribe world
                        else world
                    (eventHandling, world)
        runDesync4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Cascade.
    let runDesyncReferencingEventsAssumingCascade shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        runDesync6 shouldAdvance (fun event desync world -> desync event world) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to reference its source event without specifying its
    /// event handling approach by assuming Resolve.
    let runDesyncReferencingEventsAssumingResolve shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event -> World -> World) list) world =
        runDesync6 shouldAdvance (fun event desync world -> desync event world) Resolve observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runDesyncAssumingCascade shouldAdvance (observable : Observable<'a, 'o>) (desync : (World -> World) list) world =
        runDesync6 shouldAdvance (fun _ desync world -> desync world) Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runDesyncAssumingResolve shouldAdvance (observable : Observable<'a, 'o>) (desync : (World -> World) list) world =
        runDesync6 shouldAdvance (fun _ desync world -> desync world) Resolve observable desync world*)