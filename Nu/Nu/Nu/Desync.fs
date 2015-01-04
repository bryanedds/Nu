namespace Nu
open Prime
open Prime.Desync
open Nu

module Desync =

    let updateOptSimulantW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateOptSimulantW expr address } // TODO: see if these can be more concise

    let updateOptSimulant expr address : Desync<'r, 'e, World, unit> =
        updateOptSimulantW (flip (fun _ -> expr)) address

    let updateByOptSimulant expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByOptSimulant expr address }

    let updateSimulantW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateSimulantW expr address }

    let updateSimulant expr address : Desync<'r, 'e, World, unit> =
        updateSimulantW (flip (fun _ -> expr)) address

    let updateBySimulant expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateBySimulant expr address }

    let updateOptEntityW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateOptEntityW expr address }

    let updateOptEntity expr address : Desync<'r, 'e, World, unit> =
        updateOptEntityW (flip (fun _ -> expr)) address

    let updateByOptEntity expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByOptEntity expr address }

    let updateEntityW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateEntityW expr address }

    let updateEntity expr address : Desync<'r, 'e, World, unit> =
        updateEntityW (flip (fun _ -> expr)) address

    let updateByEntity expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByEntity expr address }

    let updateOptGroupW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateOptGroupW expr address }

    let updateOptGroup expr address : Desync<'r, 'e, World, unit> =
        updateOptGroupW (flip (fun _ -> expr)) address

    let updateByOptGroup expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByOptGroup expr address }

    let updateGroupW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateGroupW expr address }

    let updateGroup expr address : Desync<'r, 'e, World, unit> =
        updateGroupW (flip (fun _ -> expr)) address

    let updateByGroup expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByGroup expr address }

    let updateOptScreenW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateOptScreenW expr address }

    let updateOptScreen expr address : Desync<'r, 'e, World, unit> =
        updateOptScreenW (flip (fun _ -> expr)) address

    let updateByOptScreen expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByOptScreen expr address }

    let updateScreenW expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateScreenW expr address }

    let updateScreen expr address : Desync<'r, 'e, World, unit> =
        updateScreenW (flip (fun _ -> expr)) address

    let updateByScreen expr address : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByScreen expr address }

    let updateGameW expr : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateGameW expr }

    let updateGame expr : Desync<'r, 'e, World, unit> =
        updateGameW <| flip (fun _ -> expr)

    let updateByGame expr : Desync<'r, 'e, World, unit> =
        desync { do! update <| World.updateByGame expr }

    let private runDesync4 eventHandling (desync : Desync<World, Event<'a, 'o>, World, unit>) (observation : Observation<'a, 'o>) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey desync world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observation.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let advance = fun event world ->
            let desync = World.getCallbackState callbackKey world : Desync<World, Event<'a, 'o>, World, unit>
            advanceDesync
                world
                (fun world f -> World.addCallbackState callbackKey (f event) world)
                (fun world () -> unsubscribe world)
                desync
        let subscription = fun event world ->
            let world = advance event world
            (eventHandling, world)
        let world = World.subscribe<'a, 'o> subscriptionKey subscription eventAddress observation.ObserverAddress world
        (unsubscribe, world)

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Cascade.
    let runDesyncAssumingCascade desync (observation : Observation<'a, 'o>) world =
        runDesync4 Cascade desync observation world

    /// Run the given desynchronized process on top of Nu's event system.
    /// Allows each desynchronized operation to run without referencing its source event, and
    /// without specifying its event handling approach by assuming Resolve.
    let runDesyncAssumingResolve desync (observation : Observation<'a, 'o>) world =
        runDesync4 Resolve desync observation world