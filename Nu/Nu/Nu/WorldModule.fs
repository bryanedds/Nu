// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open FSharpx.Collections
open OpenTK
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module World =

    (* F# reach-arounds... *)

    let mutable rebuildEntityTree =
        Unchecked.defaultof<Screen -> World -> Entity QuadTree>

    (* Subsystems *)

    let internal getSubsystemMap world =
        Subsystems.getSubsystemMap world.Subsystems

    let internal getSubsystem<'s when 's :> Subsystem> name world =
        Subsystems.getSubsystem<'s> name world.Subsystems

    let internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
        Subsystems.getSubsystemBy<'s, 't> by name world.Subsystems

    let internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
        { world with Subsystems = Subsystems.setSubsystem<'s> subsystem name world.Subsystems }

    let internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
        { world with Subsystems = Subsystems.updateSubsystem<'s, World> updater name world.Subsystems world }

    let internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
        { world with Subsystems = Subsystems.updateSubsystems updater world.Subsystems world }

    let internal clearSubsystemsMessages world =
        { world with Subsystems = Subsystems.clearSubsystemsMessages world.Subsystems world }

    (* Callbacks *)

    let internal getTasklets world =
        Eventable.getTasklets<World> world

    let internal clearTasklets world =
        Eventable.clearTasklets<World> world

    let internal restoreTasklets tasklets world =
        Eventable.restoreTasklets<World> tasklets world

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet world =
        Eventable.addTasklet<World> tasklet world

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets world =
        Eventable.addTasklets<World> tasklets world

    /// Get callback subscriptions.
    let getSubscriptions world =
        Eventable.getSubscriptions<World> world

    /// Get callback unsubscriptions.
    let getUnsubscriptions world =
        Eventable.getUnsubscriptions<World> world

    /// Add callback state to the world.
    let addCallbackState key state world =
        Eventable.addCallbackState<'s, World> key state world

    /// Remove callback state from the world.
    let removeCallbackState key world =
        Eventable.removeCallbackState<World> key world

    /// Get callback state from the world.
    let getCallbackState<'s> key world =
        Eventable.getCallbackState<'s, World> key world

    (* Components *)

    /// Get the facets of the world.
    let getFacets world =
        Components.getFacets world.Components

    /// Get the entity dispatchers of the world.
    let getEntityDispatchers world =
        Components.getEntityDispatchers world.Components

    /// Get the group dispatchers of the world.
    let getGroupDispatchers world =
        Components.getGroupDispatchers world.Components

    /// Get the screen dispatchers of the world.
    let getScreenDispatchers world =
        Components.getScreenDispatchers world.Components

    /// Get the game dispatchers of the world.
    let getGameDispatchers world =
        Components.getGameDispatchers world.Components

    (* Publishing *)

    /// TODO: document.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter<World>) eventAddress world =
        Eventable.getSubscriptionsSorted publishSorter eventAddress world

    /// TODO: document.
    let getSubscriptionsSorted3 (publishSorter : SubscriptionSorter<World>) eventAddress world =
        Eventable.getSubscriptionsSorted3 publishSorter eventAddress world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) (world : World) =
        Eventable.sortSubscriptionsBy by subscriptions world

    /// Sort subscriptions by their place in the world's simulant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions (world : World) =
        Eventable.sortSubscriptionsByHierarchy subscriptions world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (world : World) =
        Eventable.sortSubscriptionsNone subscriptions world

    /// Publish an event, using the given getSubscriptions and publishSorter procedures to arrange the order to which subscriptions are published.
    let publish6<'a, 'p when 'p :> Simulant> getSubscriptions publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish6<'a, 'p, World> getSubscriptions publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event, using the given publishSorter procedure to arrange the order to which subscriptions are published.
    let publish5<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish5<'a, 'p, World> publishSorter eventData eventAddress eventTrace publisher world

    /// Publish an event.
    let publish<'a, 'p when 'p :> Simulant> (eventData : 'a) (eventAddress : 'a Address) eventTrace (publisher : 'p) world =
        Eventable.publish<'a, 'p, World> eventData eventAddress eventTrace publisher world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
        Eventable.unsubscribe<World> subscriptionKey world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribePlus5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribePlus<'a, 's, World> subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribe5<'a, 's, World> subscriptionKey subscription eventAddress subscriber world

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.subscribe<'a, 's, World> subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.monitorPlus<'a, 's, World> subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant.
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's, World>) (eventAddress : 'a Address) (subscriber : 's) world =
        Eventable.monitor<'a, 's, World> subscription eventAddress subscriber world

    (* WorldState *)

    let private getState world =
        world.State

    let private setStateWithoutEvent state world =
        { world with State = state }

    let private setState state world =
        let oldWorld = world
        let world = setStateWithoutEvent state world
        publish { WorldStateChangeData.OldWorld = oldWorld } Events.WorldStateChange ["World.setState"] Simulants.Game world

    /// Get the tick rate.
    let getTickRate world =
        WorldState.getTickRate world.State

    /// Get the tick rate as a floating-point value.
    let getTickRateF world =
        WorldState.getTickRateF world.State

    /// Set the tick rate without waiting for the end of the current update. Only use
    /// this if you need it and understand the engine internals well enough to know the
    /// consequences.
    let setTickRateImmediately tickRate world =
        setState (WorldState.setTickRateImmediately tickRate world.State) world

    /// Set the tick rate.
    let rec setTickRate tickRate world =
        addTasklet { ScheduledTime = getTickTime world; Operation = fun world -> setTickRateImmediately tickRate world } world

    /// Reset the tick time to 0.
    and resetTickTime world =
        addTasklet { ScheduledTime = getTickTime world; Operation = fun world -> setState (WorldState.resetTickTime world.State) world } world

    /// Get the world's tick time.
    and getTickTime world =
        WorldState.getTickTime world.State

    /// Query that the world is ticking.
    let isTicking world =
        WorldState.isTicking world.State

    let internal updateTickTime world =
        setStateWithoutEvent (WorldState.updateTickTime world.State) world

    /// Get the world's update count.
    let getUpdateCount world =
        WorldState.getUpdateCount world.State

    let internal incrementUpdateCount world =
        setStateWithoutEvent (WorldState.incrementUpdateCount world.State) world

    /// Get the the liveness state of the engine.
    let getLiveness world =
        WorldState.getLiveness world.State

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit world =
        setState (WorldState.exit world.State) world

    /// Get a value from the camera used to view the world.
    let getCameraBy by world =
        WorldState.getCameraBy by world.State

    /// Get the camera used to view the world.
    let getCamera world =
        WorldState.getCamera world.State

    /// Update the camera used to view the world.
    let updateCamera updater world =
        setState (WorldState.updateCamera updater world.State) world

    /// Get the current destination screen if a screen transition is currently underway.
    let getOptScreenTransitionDestination world =
        world.GameState.OptScreenTransitionDestination

    let internal setOptScreenTransitionDestination destination world =
        let gameState = { world.GameState with OptScreenTransitionDestination = destination }
        { world with GameState = gameState }

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        WorldState.getAssetMetadataMap world.State

    /// Get the opt entity cache.
    let internal getOptEntityCache world =
        WorldState.getOptEntityCache world.State

    let internal setAssetMetadataMap assetMetadataMap world =
        setState (WorldState.setAssetMetadataMap assetMetadataMap world.State) world

    let internal getOverlayer world =
        WorldState.getOverlayer world.State

    let internal setOverlayer overlayer world =
        setState (WorldState.setOverlayer overlayer world.State) world

    let internal getOverlayRouter world =
        WorldState.getOverlayRouter world.State

    /// Get the user state of the world, casted to 'u.
    let getUserState world : 'u =
        WorldState.getUserState world.State

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) world =
        setState (WorldState.updateUserState updater world.State) world

    (* Built-in Event Handlers *)

    /// Ignore all handled events.
    let handleAsPass<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Cascade, world)

    /// Swallow all handled events.
    let handleAsSwallow<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, world)
        
    /// Handle event by exiting app.
    let handleAsExit<'a, 's when 's :> Simulant> (_ : Event<'a, 's>) (world : World) =
        (Resolve, exit world)

    (* EntityState *)

    let private optEntityStateKeyEquality 
        (entityAddress : Entity Address, world : World)
        (entityAddress2 : Entity Address, world2 : World) =
        refEq entityAddress entityAddress2 && refEq world world2

    let private optEntityGetFreshKeyAndValue entity world =
        let optEntityState = Vmap.tryFind entity.EntityAddress world.EntityStates
        ((entity.EntityAddress, world), optEntityState)

    let private optEntityStateFinder entity world =
        KeyedCache.getValue
            optEntityStateKeyEquality
            (fun () -> optEntityGetFreshKeyAndValue entity world)
            (entity.EntityAddress, world)
            (getOptEntityCache world)

    let private entityStateSetter entityState entity world =
#if DEBUG
        if not ^ Vmap.containsKey entity.EntityAddress world.EntityStates then
            failwith ^ "Cannot set the state of a non-existent entity '" + symstring entity.EntityAddress + "'"
#endif
        let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
        { world with EntityStates = entityStates }

    let private entityStateAdder entityState entity world =
        let screenDirectory =
            match Address.getNames entity.EntityAddress with
            | [screenName; groupName; entityName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (screenAddress, groupDirectory) ->
                    match Vmap.tryFind groupName groupDirectory with
                    | Some (groupAddress, entityDirectory) ->
                        let entityDirectory = Vmap.add entityName entity.EntityAddress entityDirectory
                        let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot add entity '" + symstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith ^ "Cannot add entity '" + symstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith ^ "Invalid entity address '" + symstring entity.EntityAddress + "'."
        let entityStates = Vmap.add entity.EntityAddress entityState world.EntityStates
        { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

    let private entityStateRemover entity world =
        let screenDirectory =
            match Address.getNames entity.EntityAddress with
            | [screenName; groupName; entityName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (screenAddress, groupDirectory) ->
                    match Vmap.tryFind groupName groupDirectory with
                    | Some (groupAddress, entityDirectory) ->
                        let entityDirectory = Vmap.remove entityName entityDirectory
                        let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None -> failwith ^ "Cannot remove entity '" + symstring entity.EntityAddress + "' from non-existent group."
                | None -> failwith ^ "Cannot remove entity '" + symstring entity.EntityAddress + "' from non-existent screen."
            | _ -> failwith ^ "Invalid entity address '" + symstring entity.EntityAddress + "'."
        let entityStates = Vmap.remove entity.EntityAddress world.EntityStates
        { world with ScreenDirectory = screenDirectory; EntityStates = entityStates }

    let internal getEntityStateBoundsMax entityState =
        // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
        match entityState.Rotation with
        | 0.0f ->
            let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
            boundsOverflow // no need to transform is unrotated
        | _ ->
            let boundsOverflow = Math.makeBoundsOverflow entityState.Position entityState.Size entityState.Overflow
            let position = boundsOverflow.Xy
            let size = Vector2 (boundsOverflow.Z, boundsOverflow.W) - position
            let center = position + size * 0.5f
            let corner = position + size
            let centerToCorner = corner - center
            let quaternion = Quaternion.FromAxisAngle (Vector3.UnitZ, Constants.Math.DegreesToRadiansF * 45.0f)
            let newSizeOver2 = Vector2 (Vector2.Transform (centerToCorner, quaternion)).Y
            let newPosition = center - newSizeOver2
            let newSize = newSizeOver2 * 2.0f
            Vector4 (newPosition.X, newPosition.Y, newPosition.X + newSize.X, newPosition.Y + newSize.Y)

    let internal publishEntityChange entityState (entity : Entity) oldWorld world =
        if entityState.PublishChanges then
            publish
                { Simulant = entity; OldWorld = oldWorld }
                (Events.EntityChange ->- entity)
                ["World.publishEntityChange"]
                entity
                world
        else world

    let inline internal getOptEntityState entity world =
        optEntityStateFinder entity world

    let internal getEntityState (entity : Entity) world =
        match getOptEntityState entity world with
        | Some entityState -> entityState
        | None -> failwith ^ "Could not find entity with address '" + symstring entity + "'."

    let internal addEntityState entityState entity world =
        entityStateAdder entityState entity world

    let internal removeEntityState entity world =
        entityStateRemover entity world

    let inline internal setEntityStateWithoutEvent entityState entity world =
        entityStateSetter entityState entity world

    let internal setEntityState entityState (entity : Entity) world =
        let oldWorld = world
        let world = entityStateSetter entityState entity world
        publishEntityChange entityState entity oldWorld world

    let internal updateEntityStateWithoutEvent updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityStateWithoutEvent entityState entity world

    let internal updateEntityState updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityState entityState entity world

    let getEntityBoundsMax entity world =
        let entityState = getEntityState entity world
        getEntityStateBoundsMax entityState

    (* GroupState *)

    let private groupStateSetter groupState group world =
#if DEBUG
        if not ^ Vmap.containsKey group.GroupAddress world.GroupStates then
            failwith ^ "Cannot set the state of a non-existent group '" + symstring group.GroupAddress + "'"
#endif
        let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
        { world with GroupStates = groupStates }

    let private groupStateAdder groupState group world =
        let screenDirectory =
            match Address.getNames group.GroupAddress with
            | [screenName; groupName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (screenAddress, groupDirectory) ->
                    match Vmap.tryFind groupName groupDirectory with
                    | Some (groupAddress, entityDirectory) ->
                        let groupDirectory = Vmap.add groupName (groupAddress, entityDirectory) groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                    | None ->
                        let entityDirectory = Vmap.makeEmpty ()
                        let groupDirectory = Vmap.add groupName (group.GroupAddress, entityDirectory) groupDirectory
                        Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                | None -> failwith ^ "Cannot add group '" + symstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith ^ "Invalid group address '" + symstring group.GroupAddress + "'."
        let groupStates = Vmap.add group.GroupAddress groupState world.GroupStates
        { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let private groupStateRemover group world =
        let screenDirectory =
            match Address.getNames group.GroupAddress with
            | [screenName; groupName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (screenAddress, groupDirectory) ->
                    let groupDirectory = Vmap.remove groupName groupDirectory
                    Vmap.add screenName (screenAddress, groupDirectory) world.ScreenDirectory
                | None -> failwith ^ "Cannot remove group '" + symstring group.GroupAddress + "' from non-existent screen."
            | _ -> failwith ^ "Invalid group address '" + symstring group.GroupAddress + "'."
        let groupStates = Vmap.remove group.GroupAddress world.GroupStates
        { world with ScreenDirectory = screenDirectory; GroupStates = groupStates }

    let inline internal getOptGroupState group world =
        Vmap.tryFind group.GroupAddress world.GroupStates

    let internal getGroupState group world =
        match getOptGroupState group world with
        | Some groupState -> groupState
        | None -> failwith ^ "Could not find group with address '" + symstring group + "'."

    let internal addGroupState groupState group world =
        groupStateAdder groupState group world

    let internal removeGroupState group world =
        groupStateRemover group world

    let inline internal setGroupStateWithoutEvent groupState group world =
        groupStateSetter groupState group world

    let internal setGroupState groupState group world =
        let oldWorld = world
        let world = groupStateSetter groupState group world
        if groupState.PublishChanges then
            publish
                { Simulant = group; OldWorld = oldWorld }
                (Events.GroupChange ->- group)
                ["World.setGroupState"]
                group
                world
        else world

    let internal updateGroupState updater group world =
        let groupState = getGroupState group world
        let groupState = updater groupState
        setGroupState groupState group world

    (* ScreenState *)

    let private screenStateSetter screenState screen world =
#if DEBUG
        if not ^ Vmap.containsKey screen.ScreenAddress world.ScreenStates then
            failwith ^ "Cannot set the state of a non-existent screen '" + symstring screen.ScreenAddress + "'"
#endif
        let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
        { world with ScreenStates = screenStates }

    let private screenStateAdder screenState screen world =
        let screenDirectory =
            match Address.getNames screen.ScreenAddress with
            | [screenName] ->
                match Vmap.tryFind screenName world.ScreenDirectory with
                | Some (_, groupDirectory) ->
                    // NOTE: this is logically a redundant operation...
                    Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
                | None ->
                    let groupDirectory = Vmap.makeEmpty ()
                    Vmap.add screenName (screen.ScreenAddress, groupDirectory) world.ScreenDirectory
            | _ -> failwith ^ "Invalid screen address '" + symstring screen.ScreenAddress + "'."
        let screenStates = Vmap.add screen.ScreenAddress screenState world.ScreenStates
        { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let private screenStateRemover screen world =
        let screenDirectory =
            match Address.getNames screen.ScreenAddress with
            | [screenName] -> Vmap.remove screenName world.ScreenDirectory
            | _ -> failwith ^ "Invalid screen address '" + symstring screen.ScreenAddress + "'."
        let screenStates = Vmap.remove screen.ScreenAddress world.ScreenStates
        { world with ScreenDirectory = screenDirectory; ScreenStates = screenStates }

    let inline internal getOptScreenState screen world =
        Vmap.tryFind screen.ScreenAddress world.ScreenStates

    let internal getScreenState (screen : Screen) world =
        match getOptScreenState screen world with
        | Some screenState -> screenState
        | None -> failwith ^ "Could not find screen with address '" + symstring screen + "'."

    let internal addScreenState screenState screen world =
        screenStateAdder screenState screen world

    let internal removeScreenState screen world =
        screenStateRemover screen world

    let inline internal setScreenStateWithoutEvent screenState screen world =
        screenStateSetter screenState screen world

    let internal setScreenState screenState screen world =
        let oldWorld = world
        let world = screenStateSetter screenState screen world
        if screenState.PublishChanges then
            publish
                { Simulant = screen; OldWorld = oldWorld }
                (Events.ScreenChange ->- screen)
                ["World.setScreenState"]
                screen
                world
        else world

    let internal updateScreenState updater screen world =
        let screenState = getScreenState screen world
        let screenState = updater screenState
        setScreenState screenState screen world

    let internal updateEntityInEntityTree entity oldWorld world =

        // OPTIMIZATION: attempt to avoid constructing a screen address on each call to decrease address hashing
        // OPTIMIZATION: assumes a valid entity address with List.head on its names
        let screen =
            match world.GameState.OptSelectedScreen with
            | Some screen when screen.ScreenName = List.head ^ Address.getNames entity.EntityAddress -> screen
            | Some _ | None -> entity |> etog |> gtos

        // proceed with updating entity in entity tree
        let screenState = getScreenState screen world
        let entityTree =
            MutantCache.mutateMutant
                (fun () -> rebuildEntityTree screen oldWorld)
                (fun entityTree ->
                    let oldEntityState = getEntityState entity oldWorld
                    let oldEntityBoundsMax = getEntityStateBoundsMax oldEntityState
                    let entityState = getEntityState entity world
                    let entityBoundsMax = getEntityStateBoundsMax entityState
                    QuadTree.updateElement
                        (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityBoundsMax
                        (entityState.Omnipresent || entityState.ViewType = Absolute) entityBoundsMax
                        entity entityTree
                    entityTree)
                screenState.EntityTreeNp
        let screenState = { screenState with EntityTreeNp = entityTree }
        setScreenStateWithoutEvent screenState screen world

    let internal updateEntityStatePlus updater entity world =
        let oldWorld = world
        let world = updateEntityStateWithoutEvent updater entity world
        let world = updateEntityInEntityTree entity oldWorld world
        publishEntityChange (getEntityState entity world) entity oldWorld world

    (* GameState *)

    let internal getGameState world =
        world.GameState

    let internal setGameState gameState world =
        let oldWorld = world
        let world = { world with GameState = gameState }
        if gameState.PublishChanges then
            publish
                { Simulant = Simulants.Game; OldWorld = oldWorld }
                (Events.GameChange ->- Simulants.Game)
                ["World.setGameState"]
                Simulants.Game
                world
        else world

    let internal updateGameState updater world =
        let gameState = getGameState world
        let gameState = updater gameState
        setGameState gameState world