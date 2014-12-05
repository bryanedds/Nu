namespace Nu
open Nu

[<AutoOpen>]
module DesyncModule =

    // let d = desync { for i in 0 .. 3 do! snd; do! moveEnemy 0; do! moveEnemy 1; do! movePlayer }
    // let (unsubscribe, world) = run ... d world

    type [<NoEquality; NoComparison>] 'a Desync =
        { Operation : 'a
          OptNext : 'a Desync option }

    let bind (m : 'a Desync) (f : 'a -> 'a Desync) : 'a Desync =
        { Operation = m.Operation; OptNext = Some <| f m.Operation }

    let pass (_, world) =
        (Cascade, world)

    let call op =
        { Operation = op; OptNext = None }

    let returnFrom op =
        op

    type DesyncBuilder () =

        member this.Bind (m, f) = bind m f
        member this.ReturnFrom op = returnFrom op

    let desync = DesyncBuilder ()

[<RequireQualifiedAccess>]
module Desync =

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