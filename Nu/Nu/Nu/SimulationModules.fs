// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx.Collections
open OpenTK
open Prime
open Nu

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module TransitionDescriptor =

    /// Make a screen transition descriptor.
    let make transitionType =
        { TransitionType = transitionType
          TransitionLifetime = 0L
          OptDissolveImage = None }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GameState =

    /// Make a game state value.
    let make dispatcher =
        { Id = Core.makeId ()
          OptSelectedScreen = None
          PublishChanges = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ScreenState =

    /// Make a screen state value.
    let make optSpecialization optName dispatcher =
        let id = Core.makeId ()
        let screenState =
            { Id = id
              Name = match optName with Some name -> name | None -> acstring id
              OptSpecialization = optSpecialization 
              TransitionStateNp = IdlingState
              TransitionTicksNp = 0L // TODO: roll this field into Incoming/OutcomingState values
              Incoming = TransitionDescriptor.make Incoming
              Outgoing = TransitionDescriptor.make Outgoing
              PublishChanges = true
              Persistent = true
              CreationTimeStampNp = Core.getTimeStamp ()
              DispatcherNp = dispatcher
              EntityTreeNp = Unchecked.defaultof<Entity QuadTree MutantCache>
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }
        let quadTree = QuadTree.make Constants.Engine.EntityTreeDepth Constants.Engine.EntityTreeBounds
        { screenState with EntityTreeNp = MutantCache.make Operators.id quadTree }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module GroupState =

    /// Make a group state value.
    let make optSpecialization optName dispatcher =
        let id = Core.makeId ()
        { GroupState.Id = id
          Name = match optName with Some name -> name | None -> acstring id
          OptSpecialization = optSpecialization
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}
      
[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EntityState =

    /// Make an entity state value.
    let make optSpecialization optName optOverlayName dispatcher =
        let id = Core.makeId ()
        { Id = id
          Name = match optName with Some name -> name | None -> acstring id
          OptSpecialization = optSpecialization
          Position = Vector2.Zero
          Depth = 0.0f
          Size = Constants.Engine.DefaultEntitySize
          Rotation = 0.0f
          Visible = true
          ViewType = Relative
          Omnipresent = false
          PublishChanges = true
          Persistent = true
          CreationTimeStampNp = Core.getTimeStamp ()
          DispatcherNp = dispatcher
          FacetNames = []
          FacetsNp = []
          OptOverlayName = optOverlayName
          Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true }}

[<AutoOpen>]
module SimulationOperators =

    /// Convert a name to a screen's proxy.
    let (!>) screenName = Screen.proxy ^ ntoa screenName

    /// Convert a name to a screen's proxy.
    let ntos screenName = !> screenName

    /// Convert a group's proxy to an entity's by appending the entity's name at the end.
    let gtoe (group : Group) entityName = group => entityName

    /// Convert a screen's proxy to a group's by appending the group's name at the end.
    let stog (screen : Screen) groupName = screen => groupName

    /// Convert an entity's proxy to a group's by removing the entity's name from the end.
    let etog (entity : Entity) = !< entity

    /// Convert a group's proxy to a screen's by removing the group's name from the end.
    let gtos group = Screen.proxy ^ Address.take<Group, Screen> 1 group.GroupAddress

[<RequireQualifiedAccess>]
module Simulants =

    let Game = { GameAddress = Address.empty }
    let DefaultScreen = !> Constants.Engine.DefaultScreenName
    let DefaultGroup = DefaultScreen => Constants.Engine.DefaultGroupName
    let DefaultEntity = DefaultGroup => Constants.Engine.DefaultEntityName

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module World =

    (* F# reach-arounds... *)

    let mutable rebuildEntityTree =
        Unchecked.defaultof<Screen -> World -> World * Entity QuadTree>

    (* Publishing *)

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> (HashIdentity.FromFunctions Address<obj>.hash Address<obj>.equals)

    let internal sortFstDesc (priority : single, _) (priority2 : single, _) =
        // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
        if priority > priority2 then -1
        elif priority < priority2 then 1
        else 0

    let private getAnyEventAddresses eventAddress =
        // OPTIMIZATION: uses memoization.
        if not ^ Address.isEmpty eventAddress then
            let anyEventAddressesKey = Address.allButLast eventAddress
            match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
            | (true, anyEventAddresses) -> anyEventAddresses
            | (false, _) ->
                let eventAddressNameKeys = Address.getNameKeys eventAddress
                let anyEventAddressNameKeys = Address.getNameKeys Events.Any
                let anyEventAddresses =
                    [for i in 0 .. List.length eventAddressNameKeys - 1 do
                        let subNameKeys = List.take i eventAddressNameKeys @ anyEventAddressNameKeys
                        yield ktoa subNameKeys]
                AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                anyEventAddresses
        else failwith "Event name cannot be empty."

    let private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry rQueue) world : (single * SubscriptionEntry) list =
        List.foldBack
            (fun (key, simulant : Simulant, subscription) subscriptions ->
                let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                let subscription = (priority, (key, simulant, subscription))
                subscription :: subscriptions)
            subscriptions
            []

    let private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
        let anyEventAddresses = getAnyEventAddresses eventAddress
        let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
        let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        publishSorter subList world

    let private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
        let boxableSubscription = fun (event : obj) world ->
            try subscription (event :?> Event<'a, 's>) world
            with
            | :? InvalidCastException ->
                // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                // up an event type parameter for some form of World.publish or subscribe.
                reraise ()
            | _ -> reraise ()
        box boxableSubscription

    let private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
        (subscriber : Simulant) (publisher : 'p) (eventAddress : 'a Address) (eventData : 'a) subscription world =
        let event =
            { Subscriber = subscriber :?> 's
              Publisher = publisher :> Simulant
              EventAddress = eventAddress
              Data = eventData }
        let callableSubscription = unbox<BoxableSubscription> subscription
        let result = callableSubscription event world
        Some result

    /// Make a key used to track an unsubscription with a subscription.
    let makeSubscriptionKey () = Guid.NewGuid ()

    /// Make a callback key used to track callback states.
    let makeCallbackKey () = Guid.NewGuid ()

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
        let subscriptions = getSortableSubscriptions by subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    /// Sort subscriptions by their place in the world's simulant hierarchy.
    let sortSubscriptionsByHierarchy subscriptions world =
        sortSubscriptionsBy
            (fun _ _ -> Constants.Engine.EntityPublishingPriority)
            subscriptions
            world

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
        subscriptions

    /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
    let publish5<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
        let objEventAddress = atooa eventAddress
        let subscriptions = getSubscriptionsSorted publishSorter objEventAddress world
        let (_, world) =
            List.foldWhile
                (fun (eventHandling, world) (_, subscriber : Simulant, subscription) ->
                    if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                        (match world.State.Liveness with Running -> true | Exiting -> false) then
                        match Address.getNameKeys subscriber.SimulantAddress with
                        | [] -> publishEvent<'a, 'p, Game> subscriber publisher eventAddress eventData subscription world
                        | [_] -> publishEvent<'a, 'p, Screen> subscriber publisher eventAddress eventData subscription world
                        | [_; _] -> publishEvent<'a, 'p, Group> subscriber publisher eventAddress eventData subscription world
                        | [_; _; _] -> publishEvent<'a, 'p, Entity> subscriber publisher eventAddress eventData subscription world
                        | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                    else None)
                (Cascade, world)
                subscriptions
        world

    /// Publish an event.
    let publish<'a, 'p when 'p :> Simulant>
        (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
        publish5 sortSubscriptionsByHierarchy eventData eventAddress publisher world

    /// Unsubscribe from an event.
    let unsubscribe subscriptionKey world =
        match Map.tryFind subscriptionKey world.Callbacks.Unsubscriptions with
        | Some (eventAddress, subscriber) ->
            match Map.tryFind eventAddress world.Callbacks.Subscriptions with
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriber', _) ->
                            subscriptionKey' = subscriptionKey &&
                            subscriber' = subscriber)
                        subscriptionList
                let subscriptions = 
                    match subscriptionList with
                    | [] -> Map.remove eventAddress world.Callbacks.Subscriptions
                    | _ -> Map.add eventAddress subscriptionList world.Callbacks.Subscriptions
                let unsubscriptions = Map.remove subscriptionKey world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            | None -> world
        | None -> world

    /// Subscribe to an event using the given subscriptionKey, and be provided with an unsubscription callback.
    let subscribePlus5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        if not ^ Address.isEmpty eventAddress then
            let objEventAddress = atooa eventAddress
            let subscriptions =
                let subscriptionEntry = (subscriptionKey, subscriber :> Simulant, boxSubscription subscription)
                match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
            let unsubscriptions = Map.add subscriptionKey (objEventAddress, subscriber :> Simulant) world.Callbacks.Unsubscriptions
            let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
            let world = { world with Callbacks = callbacks }
            (unsubscribe subscriptionKey, world)
        else failwith "Event name cannot be empty."

    /// Subscribe to an event, and be provided with an unsubscription callback.
    let subscribePlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus5 (makeSubscriptionKey ()) subscription eventAddress subscriber world

    /// Subscribe to an event using the given subscriptionKey.
    let subscribe5<'a, 's when 's :> Simulant>
        subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribePlus5 subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world |> snd

    /// Subscribe to an event.
    let subscribe<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        subscribe5 (makeSubscriptionKey ()) subscription eventAddress subscriber world

    /// Keep active a subscription for the lifetime of a simulant, and be provided with an unsubscription callback.
    let monitorPlus<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        if not ^ Address.isEmpty subscriber.SimulantAddress then
            let monitorKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe5<'a, 's> monitorKey subscription eventAddress subscriber world
            let unsubscribe = fun world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe monitorKey world
                world
            let subscription' = fun _ world -> (Cascade, unsubscribe world)
            let removingEventAddress = ftoa<unit> (typeof<'s>.Name + "/Removing") ->>- subscriber.SimulantAddress
            let world = subscribe5<unit, 's> removalKey subscription' removingEventAddress subscriber world
            (unsubscribe, world)
        else failwith "Cannot monitor events with an anonymous subscriber."

    /// Keep active a subscription for the lifetime of a simulant.
    let monitor<'a, 's when 's :> Simulant>
        (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
        monitorPlus subscription eventAddress subscriber world |> snd

    (* Subsystems *)

    let internal getSubsystem<'s when 's :> Subsystem> name world =
        Map.find name world.Subsystems.SubsystemMap :?> 's

    let internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
        let subsystem = getSubsystem<'s> name world
        by subsystem

    let internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
        let subsystems = { SubsystemMap = Map.add name (subsystem :> Subsystem) world.Subsystems.SubsystemMap }
        { world with Subsystems = subsystems }

    let internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
        let subsystem = getSubsystem<'s> name world
        let subsystem = updater subsystem world
        setSubsystem subsystem name world

    let internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
        Map.fold
            (fun world name subsystem -> let subsystem = updater subsystem world in setSubsystem subsystem name world)
            world
            world.Subsystems.SubsystemMap

    let internal clearSubsystemsMessages world =
        updateSubsystems (fun is _ -> is.ClearMessages ()) world

    (* Callbacks *)

    let internal clearTasklets world =
        let callbacks = { world.Callbacks with Tasklets = Queue.empty }
        { world with Callbacks = callbacks }

    let internal restoreTasklets (tasklets : Tasklet Queue) world =
        let callbacks = { world.Callbacks with Tasklets = Queue.ofSeq ^ Seq.append (world.Callbacks.Tasklets :> Tasklet seq) (tasklets :> Tasklet seq) }
        { world with Callbacks = callbacks }

    /// Add a tasklet to be executed by the engine at the scheduled time.
    let addTasklet tasklet world =
        let callbacks = { world.Callbacks with Tasklets = Queue.conj tasklet world.Callbacks.Tasklets }
        { world with Callbacks = callbacks }

    /// Add multiple tasklets to be executed by the engine at the scheduled times.
    let addTasklets tasklets world =
        let callbacks = { world.Callbacks with Tasklets = Queue.ofSeq ^ Seq.append (tasklets :> Tasklet seq) (world.Callbacks.Tasklets :> Tasklet seq) }
        { world with Callbacks = callbacks }

    /// Add callback state to the world.
    let addCallbackState key state world =
        let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Remove callback state from the world.
    let removeCallbackState key world =
        let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
        { world with Callbacks = callbacks }

    /// Get callback state from the world.
    let getCallbackState<'a> key world =
        let state = Map.find key world.Callbacks.CallbackStates
        state :?> 'a

    (* Components *)

    /// Get the entity dispatchers of the world.
    let getEntityDispatchers world =
        world.Components.EntityDispatchers

    /// Get the group dispatchers of the world.
    let getGroupDispatchers world =
        world.Components.GroupDispatchers

    /// Get the screen dispatchers of the world.
    let getScreenDispatchers world =
        world.Components.ScreenDispatchers

    /// Get the entity dispatchers of the world.
    let getGameDispatchers world =
        world.Components.GameDispatchers

    /// Get the entity dispatchers of the world.
    let getFacets world =
        world.Components.Facets

    (* WorldState *)

    let private getState world =
        world.State

    let private setStateWithoutEvent state world =
        { world with State = state }

    let private setState state world =
        let oldWorld = world
        let world = setStateWithoutEvent state world
        publish { WorldStateChangeData.OldWorld = oldWorld } Events.WorldStateChange Simulants.Game world

    /// Get the world's tick rate.
    let getTickRate world =
        world.State.TickRate

    /// Get the world's tick rate as a floating-point value.
    let getTickRateF world =
        single ^ getTickRate world

    /// Set the world's tick rate without waiting for the end of the current update. Only use
    /// this if you need it and understand the engine internals well enough to know the
    /// consequences.
    let setTickRateImmediately tickRate world =
        let state = { world.State with TickRate = tickRate }
        setState state world

    /// Set the world's tick rate.
    let rec setTickRate tickRate world =
        let tasklet =
            { ScheduledTime = getTickTime world
              Operation = fun world -> setTickRateImmediately tickRate world }
        addTasklet tasklet world

    /// Get the world's tick time.
    and getTickTime world =
        world.State.TickTime

    /// Query that the world is ticking.
    let isTicking world =
        getTickRate world <> 0L

    let internal updateTickTime world =
        let state = { world.State with TickTime = getTickTime world + getTickRate world }
        setStateWithoutEvent state world

    /// Get the world's update count.
    let getUpdateCount world =
        world.State.UpdateCount

    let internal incrementUpdateCount world =
        let state = { world.State with UpdateCount = inc ^ getUpdateCount world }
        setStateWithoutEvent state world

    /// Get the the liveness state of the world.
    let getLiveness world =
        world.State.Liveness

    /// Place the engine into a state such that the app will exit at the end of the current update.
    let exit world =
        let state = { world.State with Liveness = Exiting }
        setState state world

    /// Get the a value from the camera used to view the world.
    let getCameraBy by world =
        by world.State.Camera

    /// Get the camera used to view the world.
    let getCamera world =
        getCameraBy id world

    let private setCamera camera world =
        let state = { world.State with Camera = camera }
        setState state world

    /// Update the camera used to view the world.
    let updateCamera updater world =
        let camera = updater ^ getCamera world
        setCamera camera world

    /// Get the current destination screen if a screen transition is currently underway.
    let getOptScreenTransitionDestination world =
        world.State.OptScreenTransitionDestination

    let internal setOptScreenTransitionDestination destination world =
        let state = { world.State with OptScreenTransitionDestination = destination }
        setState state world

    /// Get the asset metadata map.
    let getAssetMetadataMap world =
        world.State.AssetMetadataMap

    let internal setAssetMetadataMap assetMetadataMap world =
        let state = { world.State with AssetMetadataMap = assetMetadataMap }
        setState state world

    let internal setOverlayer overlayer world =
        let state = { world.State with Overlayer = overlayer }
        setState state world

    /// Get the user state of the world, casted to 'u.
    let getUserState world : 'u =
        world.State.UserState :?> 'u

    /// Set the user state of the world.
    let setUserState (userState : 'u) world =
        let state = { world.State with UserState = userState }
        setState state world

    /// Update the user state of the world.
    let updateUserState (updater : 'u -> 'v) world =
        let state = getUserState world
        let state = updater state
        setUserState state world

    (* EntityState *)

    let private optEntityStateKeyEquality 
        (entityAddress : Entity Address, world : World)
        (entityAddress2 : Entity Address, world2 : World) =
        entityAddress == entityAddress2 && world == world2

    let private optEntityGetFreshKeyAndValue entity world =
        let optEntityState =
            match Address.getNameKeys entity.EntityAddress with
            | [screenNameKey; groupNameKey; entityNameKey] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (_, entityStateMap) -> Map.tryFind entityNameKey.Name entityStateMap
                    | None -> None
                | None -> None
            | _ -> failwith ^ "Invalid entity address '" + acstring entity.EntityAddress + "'."
        ((entity.EntityAddress, world), optEntityState)

    let private optEntityStateFinder entity world =
        KeyedCache.getValue
            optEntityStateKeyEquality
            (fun () -> optEntityGetFreshKeyAndValue entity world)
            (entity.EntityAddress, world)
            world.State.OptEntityCache

    let private entityStateAdder (entityState : EntityState) entity world =
        match Address.getNameKeys entity.EntityAddress with
        | [screenNameKey; groupNameKey; entityNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (screenState, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (groupState, entityStateMap) ->
                    let entityStateMap = Map.add entityNameKey.Name entityState entityStateMap
                    let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None -> failwith ^ "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
            | None -> failwith ^ "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
        | _ -> failwith ^ "Invalid entity address '" + acstring entity.EntityAddress + "'."

    let private entityStateRemover entity world =
        match Address.getNameKeys entity.EntityAddress with
        | [screenNameKey; groupNameKey; entityNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (screenState, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (groupState, entityStateMap) ->
                    let entityStateMap = Map.remove entityNameKey.Name entityStateMap
                    let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None -> world
            | None -> world
        | _ -> failwith ^ "Invalid entity address '" + acstring entity.EntityAddress + "'."

    let internal getEntityStateMaxBounds entityState =
        // TODO: get up off yer arse and write an algorithm for tight-fitting bounds...
        match entityState.Rotation with
        | 0.0f ->
            Vector4 (entityState.Position.X, entityState.Position.Y, entityState.Position.X + entityState.Size.X, entityState.Position.Y + entityState.Size.Y)
        | _ ->
            let center = entityState.Position + entityState.Size * 0.5f
            let corner = entityState.Position + entityState.Size
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
                entity
                world
        else world

    let internal getEntityStateMap group world =
        match Address.getNameKeys group.GroupAddress with
        | [screenNameKey; groupNameKey] ->
            let (_, screenStateMap) = world.SimulantStates
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (_, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (_, entityStateMap) -> entityStateMap
                | None -> Map.empty
            | None -> Map.empty
        | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."

    let inline internal getOptEntityState entity world =
        optEntityStateFinder entity world

    let internal getEntityState (entity : Entity) world =
        match getOptEntityState entity world with
        | Some entityState -> entityState
        | None -> failwith ^ "Could not find entity with address '" + acstring entity + "'."

    let inline internal setEntityStateWithoutEvent entityState entity world =
        entityStateAdder entityState entity world

    let internal setOptEntityStateWithoutEvent optEntityState entity world =
        match optEntityState with 
        | Some entityState -> entityStateAdder entityState entity world
        | None -> entityStateRemover entity world

    let internal setEntityState entityState (entity : Entity) world =
        let oldWorld = world
        let world = entityStateAdder entityState entity world
        publishEntityChange entityState entity oldWorld world

    let internal updateEntityStateWithoutEvent updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityStateWithoutEvent entityState entity world

    let internal updateEntityState updater entity world =
        let entityState = getEntityState entity world
        let entityState = updater entityState
        setEntityState entityState entity world

    let getEntityMaxBounds entity world =
        let entityState = getEntityState entity world
        getEntityStateMaxBounds entityState

    (* GroupState *)

    let private optGroupStateFinder group world =
        match Address.getNameKeys group.GroupAddress with
        | [screenNameKey; groupNameKey] ->
            let (_, screenStateMap) = world.SimulantStates
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (_, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (groupState, _) -> Some groupState
                | None -> None
            | None -> None
        | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."

    let private groupStateAdder (groupState : GroupState) group world =
        match Address.getNameKeys group.GroupAddress with
        | [screenNameKey; groupNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (screenState, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (_, entityStateMap) ->
                    let groupStateMap = Map.add groupNameKey.Name (groupState, entityStateMap) groupStateMap
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None ->
                    let groupStateMap = Map.add groupNameKey.Name (groupState, Map.empty) groupStateMap
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
            | None -> failwith ^ "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
        | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."

    let private groupStateRemover group world =
        match Address.getNameKeys group.GroupAddress with
        | [screenNameKey; groupNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (screenState, groupStateMap) ->
                match Map.tryFind groupNameKey.Name groupStateMap with
                | Some (_, entityStateMap) ->
                    if Map.isEmpty entityStateMap then
                        let groupStateMap = Map.remove groupNameKey.Name groupStateMap
                        let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    else failwith ^ "Cannot remove group " + acstring group.GroupAddress + ", which still contains entities."
                | None -> world
            | None -> world
        | _ -> failwith ^ "Invalid group address '" + acstring group.GroupAddress + "'."

    let internal getGroupStateMap screen world =
        match Address.getNameKeys screen.ScreenAddress with
        | [screenNameKey] ->
            let (_, screenStateMap) = world.SimulantStates
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (_, groupStateMap) -> groupStateMap
            | None -> Map.empty
        | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."

    let inline internal getOptGroupState group world =
        optGroupStateFinder group world

    let internal getGroupState (group : Group) world =
        match getOptGroupState group world with
        | Some groupState -> groupState
        | None -> failwith ^ "Could not find group with address '" + acstring group + "'."

    let inline internal setGroupStateWithoutEvent groupState group world =
        groupStateAdder groupState group world

    let internal setOptGroupStateWithoutEvent optGroupState group world =
        match optGroupState with 
        | Some groupState -> groupStateAdder groupState group world
        | None -> groupStateRemover group world

    let internal setGroupState groupState group world =
        let oldWorld = world
        let world = groupStateAdder groupState group world
        if groupState.PublishChanges then
            publish
                { Simulant = group; OldWorld = oldWorld }
                (Events.GroupChange ->- group)
                group
                world
        else world

    let internal updateGroupState updater group world =
        let groupState = getGroupState group world
        let groupState = updater groupState
        setGroupState groupState group world

    (* ScreenState *)

    let private optScreenStateFinder screen world =
        match Address.getNameKeys screen.ScreenAddress with
        | [screenNameKey] ->
            let (_, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (screenState, _) -> Some screenState
            | None -> None
        | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."

    let private screenStateAdder (screenState : ScreenState) screen world =
        match Address.getNameKeys screen.ScreenAddress with
        | [screenNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (_, groupStateMap) ->
                let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                { world with SimulantStates = (gameState, screenStateMap) }
            | None ->
                let screenStateMap = Map.add screenNameKey.Name (screenState, Map.empty) screenStateMap
                { world with SimulantStates = (gameState, screenStateMap) }
        | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."

    let private screenStateRemover screen world =
        match Address.getNameKeys screen.ScreenAddress with
        | [screenNameKey] ->
            let (gameState, screenStateMap) = world.SimulantStates 
            match Map.tryFind screenNameKey.Name screenStateMap with
            | Some (_, groupStateMap) ->
                if Map.isEmpty groupStateMap then
                    let screenStateMap = Map.remove screenNameKey.Name screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                else failwith ^ "Cannot remove screen " + acstring screen.ScreenAddress + ", which still contains groups."
            | None -> world
        | _ -> failwith ^ "Invalid screen address '" + acstring screen.ScreenAddress + "'."

    let inline internal getScreenStateMap world =
        snd world.SimulantStates

    let inline internal getOptScreenState screen world =
        optScreenStateFinder screen world

    let internal getScreenState (screen : Screen) world =
        match getOptScreenState screen world with
        | Some screenState -> screenState
        | None -> failwith ^ "Could not find screen with address '" + acstring screen + "'."

    let inline internal setScreenStateWithoutEvent screenState screen world =
        screenStateAdder screenState screen world

    let internal setOptScreenStateWithoutEvent optScreenState screen world =
        match optScreenState with
        | Some screenState -> screenStateAdder screenState screen world
        | None -> screenStateRemover screen world

    let internal setScreenState screenState screen world =
        let oldWorld = world
        let world = screenStateAdder screenState screen world
        if screenState.PublishChanges then
            publish
                { Simulant = screen; OldWorld = oldWorld }
                (Events.ScreenChange ->- screen)
                screen
                world
        else world

    let internal updateScreenState updater screen world =
        let screenState = getScreenState screen world
        let screenState = updater screenState
        setScreenState screenState screen world

    let internal updateEntityInEntityTree entity oldWorld world =
        let screen = entity |> etog |> gtos
        let screenState = getScreenState screen world
        let (world, entityTree) =
            MutantCache.mutateMutant
                (fun () -> rebuildEntityTree screen oldWorld)
                (fun world entityTree ->
                    let oldEntityState = getEntityState entity oldWorld
                    let oldEntityMaxBounds = getEntityStateMaxBounds oldEntityState
                    let entityState = getEntityState entity world
                    let entityMaxBounds = getEntityStateMaxBounds entityState
                    QuadTree.updateElement
                        (oldEntityState.Omnipresent || oldEntityState.ViewType = Absolute) oldEntityMaxBounds
                        (entityState.Omnipresent || entityState.ViewType = Absolute) entityMaxBounds
                        entity entityTree
                    (world, entityTree))
                world
                screenState.EntityTreeNp
        let screenState = { screenState with EntityTreeNp = entityTree }
        setScreenStateWithoutEvent screenState screen world

    let internal updateEntityStatePlus updater entity world =
        let oldWorld = world
        let world = updateEntityStateWithoutEvent updater entity world
        let world = updateEntityInEntityTree entity oldWorld world
        publishEntityChange (getEntityState entity world) entity oldWorld world

    (* GameState *)

    let rec internal getGameStateMap world =
        let gameState = getGameState world
        let screenStateMap = getScreenStateMap world
        (gameState, screenStateMap)

    and internal getGameState world : GameState =
        fst world.SimulantStates

    let internal setGameState gameState world =
        let oldWorld = world
        let screenStateMap = getScreenStateMap world
        let world = { world with SimulantStates = (gameState, screenStateMap) }
        if gameState.PublishChanges then
            publish
                { OldWorld = oldWorld; Simulant = Simulants.Game }
                (Events.GameChange ->- Simulants.Game)
                Simulants.Game
                world
        else world

    let internal updateGameState updater world =
        let gameState = getGameState world
        let gameState = updater gameState
        setGameState gameState world