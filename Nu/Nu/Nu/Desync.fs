namespace Nu
open FSharpx
open Prime
open Prime.Desync
open Nu
open Nu.Constants
open Nu.WorldConstants
module Desync =

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

    /// Update the state of the world and the world.
    let updateStateAndW expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateStateAndW expr }

    /// Update the state of the world.
    let updateStateW expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateStateW expr }

    /// Update the state of the world.
    let updateState expr : Desync<'e, World, unit> =
        updateStateW (flip (fun _ -> expr))

    /// Update the world by its state.
    let updateByState expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateByState expr }

    /// Update a simulant at the given address with the given 'updater' procedure.
    let updateSimulantAndW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateSimulantAndW expr address }

    /// Update a simulant with the given 'updater' procedure at the given address.
    let updateSimulantW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateSimulantW expr address }

    /// Update a simulant with the given 'updater' procedure at the given address.
    let updateSimulant expr address : Desync<'e, World, unit> =
        updateSimulantW (flip (fun _ -> expr)) address

    /// Update the world with the given 'updater' procedure that uses the simulant at given
    /// address in its computation.
    let updateBySimulant expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateBySimulant expr address }

    /// Update the game with the given 'updater' procedure.
    let updateGameAndW expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateGameAndW expr }

    /// Update the game with the given 'updater' procedure.
    let updateGameW expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateGameW expr }

    /// Update the game with the given 'updater' procedure.
    let updateGame expr : Desync<'e, World, unit> =
        updateGameW <| flip (fun _ -> expr)

    /// Update the world with the given 'updater' procedure that uses the game in its
    /// computation.
    let updateByGame expr : Desync<'e, World, unit> =
        desync { do! update <| World.updateByGame expr }

    /// Update a screen at the given address with the given 'updater' procedure.
    let updateScreenAndW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateScreenAndW expr address }

    /// Update a screen with the given 'updater' procedure at the given address.
    let updateScreenW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateScreenW expr address }

    /// Update a screen with the given 'updater' procedure at the given address.
    let updateScreen expr address : Desync<'e, World, unit> =
        updateScreenW (flip (fun _ -> expr)) address

    /// Update the world with the given 'updater' procedure that uses the screen at given
    /// address in its computation.
    let updateByScreen expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByScreen expr address }

    /// Update a group at the given address and the world with the given 'updater' procedure.
    let updateGroupW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateGroupW expr address }

    /// Update a group with the given 'updater' procedure at the given address.
    let updateGroupAndW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateGroupAndW expr address }

    /// Update a group with the given 'updater' procedure at the given address.
    let updateGroup expr address : Desync<'e, World, unit> =
        updateGroupW (flip (fun _ -> expr)) address

    /// Update the world with the given 'updater' procedure that uses the group at given
    /// address in its computation.
    let updateByGroup expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByGroup expr address }

    /// Update an entity at the given address and the world with the given 'updater' procedure.
    let updateEntityAndW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateEntityAndW expr address }

    /// Update an entity with the given 'updater' procedure at the given address.
    let updateEntityW expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateEntityW expr address }

    /// Update an entity with the given 'updater' procedure at the given address.
    let updateEntity expr address : Desync<'e, World, unit> =
        updateEntityW (flip (fun _ -> expr)) address

    /// Update the world with the given 'updater' procedure that uses the entity at given
    /// address in its computation.
    let updateByEntity expr address : Desync<'e, World, unit> =
        desync { do! update <| World.updateByEntity expr address }

    /// Update a lensed value at the given address and the world with the given 'updater' procedure.
    let updateLensedAndW expr lens =
        desync { do! update (fun (world : World) -> Lens.update expr (lens @-> World.lens) world) }

    /// Update a lensed value with the given 'updater' procedure at the given address.
    let updateLensedW expr lens =
        desync { do! update (fun (world : World) -> Lens.updateS expr lens world) }

    /// Update a lensed value with the given 'updater' procedure at the given address.
    let updateLensed expr lens =
        updateLensedW (flip (fun _ -> expr)) lens

    /// Update the world with the given 'updater' procedure that uses the lensed value at given
    /// address in its computation.
    let updateByLensed expr lens =
        desync { do! update (fun (world : World) -> expr (Lens.get world lens) world) }