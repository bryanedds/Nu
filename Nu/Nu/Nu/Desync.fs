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

    let private runDesync4 eventHandling (desync : Desync<Event<'a, 'o>, World, unit>) (observable : Observable<'a, 'o>) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey (fun (_ : Event<'a, 'o>) -> desync) world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observable.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let advance = fun event world ->
            let desync = World.getCallbackState callbackKey world : Event<'a, 'o> -> Desync<Event<'a, 'o>, World, unit>
            let (world, advanceResult) = advance desync event world
            match advanceResult with
            | Left desyncNext -> World.addCallbackState callbackKey desyncNext world
            | Right () -> unsubscribe world
        let subscription = fun event world ->
            let world = advance event world
            (eventHandling, world)
        let world = advance Unchecked.defaultof<Event<'a, 'o>> world
        let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observable.ObserverAddress world
        (unsubscribe, world)

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runDesyncAssumingCascade desync (observable : Observable<'a, 'o>) world =
        runDesync4 Cascade desync observable world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runDesyncAssumingResolve desync (observable : Observable<'a, 'o>) world =
        runDesync4 Resolve desync observable world