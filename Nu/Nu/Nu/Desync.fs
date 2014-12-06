namespace Nu
open Nu

[<AutoOpen>]
module DesyncModule =

    // Desynchronizes a series of operations.
    type 'a Desync =
        { Operation : 'a
          OptNext : 'a Desync option }

    // essentially the desync constructor
    let call op =
        { Operation = op; OptNext = None }

    // a desync constructor with a no-op

    let pass () =
        call World.handleAsPass

    // this works, but is not really bind due to no 'b type...
    let bind (m : 'a Desync) (f : unit -> 'a Desync) : 'a Desync =
        { Operation = m.Operation; OptNext = Some <| f () }

    // seems like it would be hard to get this wrong...
    let returnFrom op =
        op

    // probably wrong, but had to define this to get use of `for` closer to compiling (tho it still doesn't)
    //let forM (ts : int seq) (f : int -> 'a Desync) : 'a Desync seq =
    //    Seq.map (fun t -> f t) ts

    // probably wrong, but had to define this to get use of `for` closer to compiling (tho it still doesn't)
    let forM (ts : int seq) (f : int -> 'a Desync) : 'a Desync =
        let folded =
            Seq.fold
                (fun optM t ->
                    match optM with
                    | Some m -> Some { Operation = m.Operation; OptNext = Some <| f t }
                    | None -> None)
                None
                ts
        Option.get folded

    // probably useless, but had to define this to get use of `for` closer to compiling (tho it still doesn't)
    let rec combine m n =
        match m.OptNext with
        | Some next ->
            match next.OptNext with
            | None -> { Operation = next.Operation; OptNext = Some n }
            | Some next2 -> { Operation = next.Operation; OptNext = Some <| combine next2 n }
        | None -> n

    type DesyncBuilder () =

        member this.Bind (m, f) = bind m f
        member this.Return op = call op
        member this.ReturnFrom op = returnFrom op
        member this.For (ops, f) = forM ops f
        member this.Combine (m, n) = combine m n
        member this.Yield op = call op
        member this.Zero () = pass ()

    let desync =
        DesyncBuilder ()

[<RequireQualifiedAccess>]
module Desync =

    /// Integrates desync with Nu's purely functional event system.
    let run shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event * World -> EventHandling * World) Desync) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey desync world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observable.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let subscription =
            fun event world ->
                let desync = World.getCallbackState callbackKey world
                let (eventHandling, world) = desync.Operation (event, world)
                let world =
                    if shouldAdvance event then // TODO: perhaps also pass world?
                        match desync.OptNext with
                        | Some next -> World.addCallbackState callbackKey next world
                        | None -> unsubscribe world
                    else world
                (eventHandling, world)
        let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
        (unsubscribe, world)