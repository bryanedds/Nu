namespace Nu
open Nu

[<AutoOpen>]
module DesyncModule =

    // desync zero
    let zero () = []

    // essentially the desync constructor
    let returnM op = [op]

    // seems like it would be hard to get this wrong...
    let returnFrom op = op

    // this works, but is not really bind due to no 'b type...
    let bind (m : 'a list) (f : unit -> 'a list) : 'a list =
        m @ f ()

    let forM (ts : int seq) (f : int -> 'a list) : 'a list =
        Seq.fold (fun m t -> m @ f t) [] ts

    let rec combine m n =
        m @ n

    let call op =
        returnM op

    // NOTE: this is NOT general to Desync!
    let pass () =
        call (fun (_, world) -> (Cascade, world))

    type DesyncBuilder () =

        member this.Bind (m, f) = bind m f
        member this.Return op = returnM op
        member this.ReturnFrom op = returnFrom op
        member this.For (ops, f) = forM ops f
        member this.Combine (m, n) = combine m n
        member this.Yield op = returnM op
        member this.Zero () = zero ()
        member this.Delay f = (fun x -> f x)

    let desync =
        DesyncBuilder ()

[<RequireQualifiedAccess>]
module Desync =

    /// Integrates desync with Nu's purely functional event system.
    let run shouldAdvance (observable : Observable<'a, 'o>) (desync : ('a Event * World -> EventHandling * World) list) world =
        if not <| List.isEmpty desync then
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
            let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
            (unsubscribe, world)
        else (id, world)