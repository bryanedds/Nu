namespace Nu
open FSharpx
open Prime
open Prime.Desync
open Nu
open Nu.Constants
open Nu.WorldConstants

module Desync =

    let updateSimulantW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateSimulantW expr address }

    let updateSimulant expr address : Desync<'e, World, unit> =
        updateSimulantW (flip (fun _ -> expr)) address

    let updateBySimulant expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateBySimulant expr address }

    let updateGameW expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateGameW expr }

    let updateGame expr : Desync<'e, World, unit> =
        updateGameW <| flip (fun _ -> expr)

    let updateByGame expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateByGame expr }

    let updateScreenW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateScreenW expr address }

    let updateScreen expr address : Desync<'e, World, unit> =
        updateScreenW (flip (fun _ -> expr)) address

    let updateByScreen expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByScreen expr address }

    let updateGroupW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateGroupW expr address }

    let updateGroup expr address : Desync<'e, World, unit> =
        updateGroupW (flip (fun _ -> expr)) address

    let updateByGroup expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByGroup expr address }

    let updateEntityW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateEntityW expr address }

    let updateEntity expr address : Desync<'e, World, unit> =
        updateEntityW (flip (fun _ -> expr)) address

    let updateByEntity expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByEntity expr address }

    let updateByLensW expr lens =
        desync { do! update (fun (world : World) -> Lens.updateS expr lens world) }

    let updateByLens expr lens =
        updateByLensW (flip (fun _ -> expr)) lens

    let private runDesync4 eventHandling (desync : Desync<Event<'a, 'o>, World, unit>) (observation : Observation<'a, 'o>) world =
        let callbackKey = World.makeCallbackKey ()
        let world = World.addCallbackState callbackKey (fun (_ : Event<'a, 'o>) -> desync) world
        let subscriptionKey = World.makeSubscriptionKey ()
        let (eventAddress, unsubscribe, world) = observation.Subscribe world
        let unsubscribe = fun world ->
            let world = World.removeCallbackState callbackKey world
            let world = unsubscribe world
            World.unsubscribe subscriptionKey world
        let advance = fun event world ->
            let desync = World.getCallbackState callbackKey world : Event<'a, 'o> -> Desync<Event<'a, 'o>, World, unit>
            let (world, advanceResult) = advanceDesync desync event world
            match advanceResult with
            | Right () -> unsubscribe world
            | Left desyncNext -> World.addCallbackState callbackKey desyncNext world
        let subscription = fun event world ->
            let world = advance event world
            (eventHandling, world)
        let world = advance Unchecked.defaultof<Event<'a, 'o>> world
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