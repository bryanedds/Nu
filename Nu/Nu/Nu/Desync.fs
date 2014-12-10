namespace Nu
open Prime
open Prime.Desync
open Nu

module Desync =

    let updateByEntity address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let entity = World.getEntity address world
            let world = expr entity world
            do! set world }

    let updateEntityW address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let entity = World.getEntity address world
            let entity = expr entity world
            do! setBy <| World.setEntity address entity }

    let updateEntity address expr : Desync<'e, World, unit> =
        updateEntityW address <| flip (fun _ -> expr)

    let updateByGroup address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let group = World.getGroup address world
            let world = expr group world
            do! set world }

    let updateGroupW address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let group = World.getGroup address world
            let group = expr group world
            do! setBy <| World.setGroup address group }

    let updateGroup address expr : Desync<'e, World, unit> =
        updateGroupW address <| flip (fun _ -> expr)

    let updateByScreen address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let screen = World.getScreen address world
            let world = expr screen world
            do! set world }

    let updateScreenW address expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let screen = World.getScreen address world
            let screen = expr screen world
            do! setBy <| World.setScreen address screen }

    let updateScreen address expr : Desync<'e, World, unit> =
        updateScreenW address <| flip (fun _ -> expr)

    let updateByGame expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let game = World.getGame world
            let world = expr game world
            do! set world }

    let updateGameW expr : Desync<'e, World, unit> =
        desync {
            let! world = get
            let game = World.getGame world
            let game = expr game world
            do! setBy <| World.setGame game }

    let updateGame expr : Desync<'e, World, unit> =
        updateGameW <| flip (fun _ -> expr)

    let private runDesync4 makeSubscription (observable : Observable<'a, 'o>) (desync : Desync<'a Event, World, unit>) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey (fun (_ : 'a Event) -> desync) world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observable.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let subscription = makeSubscription unsubscribe callbackKey
        let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
        (unsubscribe, world)

    let runDesync4' eventHandling (observable : Observable<'a, 'o>) (desync : Desync<'a Event, World, unit>) world =
        let makeSubscription unsubscribe callbackKey =
            fun event world ->
                let desync = World.getCallbackState callbackKey world : 'a Event -> Desync<'a Event, World, unit>
                let (world, advanceResult) = advance desync event world
                match advanceResult with
                | Left desyncNext ->
                    let world = World.addCallbackState callbackKey desyncNext world
                    (eventHandling, world)
                | Right () ->
                    let world = unsubscribe world
                    (eventHandling, world)
        runDesync4 makeSubscription observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runDesyncAssumingCascade (observable : Observable<'a, 'o>) desync world =
        runDesync4' Cascade observable desync world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runDesyncAssumingResolve (observable : Observable<'a, 'o>) desync world =
        runDesync4' Resolve observable desync world