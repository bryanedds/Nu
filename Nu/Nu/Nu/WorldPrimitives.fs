// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldPrimitivesModule =

    let private AnyEventAddressesCache =
        Dictionary<obj Address, obj Address list> HashIdentity.Structural

    type World with

        (* Publishing *)

        static member internal sortFstDesc (priority : single, _) (priority2 : single, _) =
            // OPTIMIZATION: priority parameter is annotated as 'single' to decrease GC pressure.
            if priority > priority2 then -1
            elif priority < priority2 then 1
            else 0

        static member private getAnyEventAddresses eventAddress =
            // OPTIMIZATION: uses memoization.
            if not <| Address.isEmpty eventAddress then
                let anyEventAddressesKey = Address.allButLast eventAddress
                match AnyEventAddressesCache.TryGetValue anyEventAddressesKey with
                | (true, anyEventAddresses) -> anyEventAddresses
                | (false, _) ->
                    let eventAddressList = eventAddress.Names
                    let anyEventAddressList = AnyEventAddress.Names
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressList - 1 do
                            let subNameList = List.take i eventAddressList @ anyEventAddressList
                            yield Address.make subNameList]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry rQueue) world : (single * SubscriptionEntry) list =
            List.fold
                (fun subscriptions (key, simulant : Simulant, subscription) ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, simulant, subscription))
                    subscription :: subscriptions)
                []
                subscriptions

        static member private getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress world =
            let anyEventAddresses = World.getAnyEventAddresses eventAddress
            let optSubLists = List.map (fun anyEventAddress -> Map.tryFind anyEventAddress world.Callbacks.Subscriptions) anyEventAddresses
            let optSubLists = Map.tryFind eventAddress world.Callbacks.Subscriptions :: optSubLists
            let subLists = List.definitize optSubLists
            let subList = List.concat subLists
            publishSorter subList world

        static member private boxSubscription<'a, 's when 's :> Simulant> (subscription : Subscription<'a, 's>) =
            let boxableSubscription = fun (event : obj) world ->
                try subscription (event :?> Event<'a, 's>) world
                with
                | :? InvalidCastException ->
                    // NOTE: If you've reached this exception, then you've probably inadvertantly mixed
                    // up an event type parameter for some form of World.publish or subscribe.
                    reraise ()
                | _ -> reraise ()
            box boxableSubscription

        static member private publishEvent<'a, 'p, 's when 'p :> Simulant and 's :> Simulant>
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
        static member makeSubscriptionKey () = Guid.NewGuid ()

        /// Make a callback key used to track callback states.
        static member makeCallbackKey () = Guid.NewGuid ()

        /// Get an entity's picking priority.
        static member getEntityPickingPriority entity world =
            let entityState = World.getEntityState entity world
            let dispatcher = entityState.DispatcherNp
            dispatcher.GetPickingPriority entity entityState.Depth world

        /// Sort subscriptions using categorization via the 'by' procedure.
        static member sortSubscriptionsBy by (subscriptions : SubscriptionEntry list) world =
            let subscriptions = World.getSortableSubscriptions by subscriptions world
            let subscriptions = List.sortWith World.sortFstDesc subscriptions
            List.map snd subscriptions

        /// Sort subscriptions by their editor picking priority.
        static member sortSubscriptionsByPickingPriority subscriptions world =
            World.sortSubscriptionsBy World.getEntityPickingPriority subscriptions world

        /// Sort subscriptions by their place in the world's simulant hierarchy.
        static member sortSubscriptionsByHierarchy subscriptions world =
            World.sortSubscriptionsBy
                (fun _ _ -> EntityPublishingPriority)
                subscriptions
                world

        /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
        static member sortSubscriptionsNone (subscriptions : SubscriptionEntry list) (_ : World) =
            subscriptions

        /// Publish an event, using the given publishSorter procedure to arranging the order to which subscriptions are published.
        static member publish<'a, 'p when 'p :> Simulant> publishSorter (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            let objEventAddress = atooa eventAddress
            let subscriptions = World.getSubscriptionsSorted publishSorter objEventAddress world
            let (_, world) =
                List.foldWhile
                    (fun (eventHandling, world) (_, subscriber : Simulant, subscription) ->
                        if  (match eventHandling with Cascade -> true | Resolve -> false) &&
                            (match world.State.Liveness with Running -> true | Exiting -> false) then
                            match subscriber.SimulantAddress.Names with
                            | [] -> World.publishEvent<'a, 'p, Game> subscriber publisher eventAddress eventData subscription world
                            | [_] -> World.publishEvent<'a, 'p, Screen> subscriber publisher eventAddress eventData subscription world
                            | [_; _] -> World.publishEvent<'a, 'p, Group> subscriber publisher eventAddress eventData subscription world
                            | [_; _; _] -> World.publishEvent<'a, 'p, Entity> subscriber publisher eventAddress eventData subscription world
                            | _ -> failwith "Unexpected match failure in 'Nu.World.publish.'"
                        else None)
                    (Cascade, world)
                    subscriptions
            world

        /// Publish an event.
        static member publish4<'a, 'p when 'p :> Simulant>
            (eventData : 'a) (eventAddress : 'a Address) (publisher : 'p) world =
            World.publish World.sortSubscriptionsByHierarchy eventData eventAddress publisher world

        /// Subscribe to an event.
        static member subscribe<'a, 's when 's :> Simulant>
            subscriptionKey (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty eventAddress then
                let objEventAddress = atooa eventAddress
                let subscriptions =
                    let subscriptionEntry = (subscriptionKey, subscriber :> Simulant, World.boxSubscription subscription)
                    match Map.tryFind objEventAddress world.Callbacks.Subscriptions with
                    | Some subscriptionEntries -> Map.add objEventAddress (subscriptionEntry :: subscriptionEntries) world.Callbacks.Subscriptions
                    | None -> Map.add objEventAddress [subscriptionEntry] world.Callbacks.Subscriptions
                let unsubscriptions = Map.add subscriptionKey (objEventAddress, subscriber :> Simulant) world.Callbacks.Unsubscriptions
                let callbacks = { world.Callbacks with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }
                { world with Callbacks = callbacks }
            else failwith "Event name cannot be empty."

        /// Subscribe to an event.
        static member subscribe4<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            World.subscribe (World.makeSubscriptionKey ()) subscription eventAddress subscriber world

        /// Unsubscribe from an event.
        static member unsubscribe subscriptionKey world =
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

        /// Keep active a subscription for the lifetime of a simulant.
        static member monitor<'a, 's when 's :> Simulant>
            (subscription : Subscription<'a, 's>) (eventAddress : 'a Address) (subscriber : 's) world =
            if not <| Address.isEmpty subscriber.SimulantAddress then
                let monitorKey = World.makeSubscriptionKey ()
                let removalKey = World.makeSubscriptionKey ()
                let world = World.subscribe<'a, 's> monitorKey subscription eventAddress subscriber world
                let subscription' = fun _ world ->
                    let world = World.unsubscribe removalKey world
                    let world = World.unsubscribe monitorKey world
                    (Cascade, world)
                let removingEventAddress = stoa<unit> (typeof<'s>.Name + "/" + "Removing") ->>- subscriber.SimulantAddress
                World.subscribe<unit, 's> removalKey subscription' removingEventAddress subscriber world
            else failwith "Cannot monitor events with an anonymous subscriber."

        (* Entity *)

        static member private optEntityStateFinder entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) -> Map.tryFind entityName entityStateMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateAdder (entityState : EntityState) entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.add entityName entityState entityStateMap
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateRemover entity world =
            match entity.EntityAddress.Names with
            | [screenName; groupName; entityName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, entityStateMap) ->
                        let entityStateMap = Map.remove entityName entityStateMap
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member internal getEntityStateMap group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) -> entityStateMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member internal getEntityState (entity : Entity) world =
            Option.get ^ World.getOptEntityState entity world

        static member internal setEntityStateWithoutEvent entityState entity world =
            World.entityStateAdder entityState entity world

        static member internal setOptEntityStateWithoutEvent optEntityState entity world =
            match optEntityState with 
            | Some entityState -> World.entityStateAdder entityState entity world
            | None -> World.entityStateRemover entity world

        static member internal setEntityState entityState (entity : Entity) world =
            let oldWorld = world
            let world = World.entityStateAdder entityState entity world
            if entityState.PublishChanges then
                World.publish4
                    { Simulant = entity; OldWorld = oldWorld }
                    (EntityChangeEventAddress ->>- entity.EntityAddress)
                    entity
                    world
            else world

        static member internal updateEntityState updater entity world =
            let entityState = World.getEntityState entity world
            let entityState = updater entityState
            World.setEntityState entityState entity world

        (* Group *)

        static member private optGroupStateFinder group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (groupState, _) -> Some groupState
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateAdder (groupState : GroupState) group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) ->
                        let groupStateMap = Map.add groupName (groupState, entityStateMap) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    | None ->
                        let groupStateMap = Map.add groupName (groupState, Map.empty) groupStateMap
                        let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                | None -> failwith <| "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateRemover group world =
            match group.GroupAddress.Names with
            | [screenName; groupName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, groupStateMap) ->
                    match Map.tryFind groupName groupStateMap with
                    | Some (_, entityStateMap) ->
                        if Map.isEmpty entityStateMap then
                            let groupStateMap = Map.remove groupName groupStateMap
                            let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                            { world with SimulantStates = (gameState, screenStateMap) }
                        else failwith <| "Cannot remove group " + acstring group.GroupAddress + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getGroupStateMap screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) -> groupStateMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getOptGroupState group world =
            World.optGroupStateFinder group world

        static member internal getGroupState group world : GroupState =
            Option.get ^ World.getOptGroupState group world

        static member internal setGroupStateWithoutEvent groupState group world =
            World.groupStateAdder groupState group world

        static member internal setOptGroupStateWithoutEvent optGroupState group world =
            match optGroupState with 
            | Some groupState -> World.groupStateAdder groupState group world
            | None -> World.groupStateRemover group world

        static member internal setGroupState groupState group world =
            let oldWorld = world
            let world = World.groupStateAdder groupState group world
            if groupState.PublishChanges then
                World.publish4
                    { Simulant = group; OldWorld = oldWorld }
                    (GroupChangeEventAddress ->>- group.GroupAddress)
                    group
                    world
            else world

        static member internal updateGroupState updater group world =
            let groupState = World.getGroupState group world
            let groupState = updater groupState
            World.setGroupState groupState group world

        (* Screen *)

        static member private optScreenStateFinder screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (screenState, _) -> Some screenState
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateAdder (screenState : ScreenState) screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    let screenStateMap = Map.add screenName (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None ->
                    let screenStateMap = Map.add screenName (screenState, Map.empty) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateRemover screen world =
            match screen.ScreenAddress.Names with
            | [screenName] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenName screenStateMap with
                | Some (_, groupStateMap) ->
                    if Map.isEmpty groupStateMap then
                        let screenStateMap = Map.remove screenName screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    else failwith <| "Cannot remove screen " + acstring screen.ScreenAddress + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getScreenStateMap world =
            snd world.SimulantStates

        static member internal getOptScreenState screen world =
            World.optScreenStateFinder screen world

        static member internal getScreenState screen world : ScreenState =
            Option.get ^ World.getOptScreenState screen world

        static member internal setScreenStateWithoutEvent screenState screen world =
            World.screenStateAdder screenState screen world

        static member internal setOptScreenStateWithoutEvent optScreenState screen world =
            match optScreenState with
            | Some screenState -> World.screenStateAdder screenState screen world
            | None -> World.screenStateRemover screen world

        static member internal setScreenState screenState screen world =
            let oldWorld = world
            let world = World.screenStateAdder screenState screen world
            if screenState.PublishChanges then
                World.publish4
                    { Simulant = screen; OldWorld = oldWorld }
                    (ScreenChangeEventAddress ->>- screen.ScreenAddress)
                    screen
                    world
            else world

        static member internal updateScreenState updater screen world =
            let screenState = World.getScreenState screen world
            let screenState = updater screenState
            World.setScreenState screenState screen world

        (* Game *)

        static member internal getGameStateMap world =
            let gameState = World.getGameState world
            let screenStateMap = World.getScreenStateMap world
            (gameState, screenStateMap)

        static member internal getGameState world : GameState =
            fst world.SimulantStates

        static member internal setGameState gameState world =
            let oldWorld = world
            let screenStateMap = World.getScreenStateMap world
            let world = { world with SimulantStates = (gameState, screenStateMap) }
            if gameState.PublishChanges then
                World.publish4
                    { OldWorld = oldWorld; Simulant = Game }
                    (GameChangeEventAddress ->>- Game.GameAddress)
                    Game
                    world
            else world

        static member internal updateGameState updater world =
            let gameState = World.getGameState world
            let gameState = updater gameState
            World.setGameState gameState world

[<AutoOpen>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member internal toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member internal toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Query that the given mouse button is down.
        static member isMouseButtonDown mouseButton (_ : World) =
            MouseState.isButtonDown mouseButton

        /// Get the position of the mouse.
        static member getMousePosition (_ : World) =
            MouseState.getPosition ()

        /// Get the position of the mouse in floating-point coordinates.
        static member getMousePositionF (_ : World) =
            MouseState.getPositionF ()

        /// Query that the given keyboard key is down.
        static member isKeyboardKeyDown scanCode (_ : World) =
            KeyboardState.isKeyDown scanCode

        // TODO: implement isKeyboardModifierActive.

[<AutoOpen>]
module WorldSubsystemsModule =

    type World with

        static member internal getSubsystem<'s when 's :> Subsystem> name world =
            Map.find name world.Subsystems :?> 's

        static member internal getSubsystemBy<'s, 't when 's :> Subsystem> by name world : 't =
            let subsystem = World.getSubsystem<'s> name world
            by subsystem

        static member internal setSubsystem<'s when 's :> Subsystem> (subsystem : 's) name world =
            let subsystems = Map.add name (subsystem :> Subsystem) world.Subsystems
            { world with Subsystems = subsystems }

        static member internal updateSubsystem<'s when 's :> Subsystem> (updater : 's -> World -> 's) name world =
            let subsystem = World.getSubsystem<'s> name world
            let subsystem = updater subsystem world
            World.setSubsystem subsystem name world

        static member internal updateSubsystems (updater : Subsystem -> World -> Subsystem) world =
            Map.fold
                (fun world name subsystem -> let subsystem = updater subsystem world in World.setSubsystem subsystem name world)
                world
                world.Subsystems

        static member internal clearSubsystemsMessages world =
            World.updateSubsystems (fun is _ -> is.ClearMessages ()) world

        /// Add a physics message to the world.
        static member addPhysicsMessage (message : PhysicsMessage) world =
            World.updateSubsystem (fun is _ -> is.EnqueueMessage message) IntegratorSubsystemName world

        /// Add a rendering message to the world.
        static member addRenderMessage (message : RenderMessage) world =
            World.updateSubsystem (fun rs _ -> rs.EnqueueMessage message) RendererSubsystemName world

        /// Add an audio message to the world.
        static member addAudioMessage (message : AudioMessage) world =
            World.updateSubsystem (fun aps _ -> aps.EnqueueMessage message) AudioPlayerSubsystemName world

[<AutoOpen>]
module WorldCallbacksModule =

    type World with

        static member internal clearTasks world =
            let callbacks = { world.Callbacks with Tasks = [] }
            { world with Callbacks = callbacks }

        static member internal restoreTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = world.Callbacks.Tasks @ tasks }
            { world with Callbacks = callbacks }

        /// Add a task to be executed by the engine at the specified task tick.
        static member addTask task world =
            let callbacks = { world.Callbacks with Tasks = task :: world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add multiple task to be executed by the engine at the specified task tick.
        static member addTasks tasks world =
            let callbacks = { world.Callbacks with Tasks = tasks @ world.Callbacks.Tasks }
            { world with Callbacks = callbacks }

        /// Add callback state to the world.
        static member addCallbackState key state world =
            let callbacks = { world.Callbacks with CallbackStates = Map.add key (state :> obj) world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Remove callback state from the world.
        static member removeCallbackState key world =
            let callbacks = { world.Callbacks with CallbackStates = Map.remove key world.Callbacks.CallbackStates }
            { world with Callbacks = callbacks }

        /// Get callback state from the world.
        static member getCallbackState<'a> key world =
            let state = Map.find key world.Callbacks.CallbackStates
            state :?> 'a

[<AutoOpen>]
module WorldStateModule =

    type World with

        /// Get the state of the world.
        static member getState world =
            world.State

        static member private setState state world =
            let oldState = world.State
            let world = { world with State = state }
            World.publish4 { OldWorldState = oldState } WorldStateChangeEventAddress Game world

        /// Update the world state, taking the world as an argument.
        static member updateStateW updater world =
            let state = updater world.State world
            World.setState state world

        /// Update the world state.
        static member updateState updater world =
            World.updateStateW (fun state _ -> updater state) world

        /// Update the world by its state.
        static member updateByState updater world : World =
            updater world.State world

        /// Get the world's tick time.
        static member getTickTime world =
            world.State.TickTime

        static member internal incrementTickTime world =
            let state = { world.State with TickTime = world.State.TickTime + 1L }
            World.setState state world

        /// Get the the liveness state of the world.
        static member getLiveness world =
            world.State.Liveness

        /// Place the engine into a state such that the app will exit at the end of the current tick.
        static member exit world =
            let state = { world.State with Liveness = Exiting }
            World.setState state world

        /// Query that the engine is in game-playing mode.
        static member isGamePlaying world =
            Interactivity.isGamePlaying world.State.Interactivity

        /// Query that the physics system is running.
        static member isPhysicsRunning world =
            Interactivity.isPhysicsRunning world.State.Interactivity

        /// Get the interactivity state of the world.
        static member getInteractivity world =
            world.State.Interactivity

        /// Set the level of the world's interactivity.
        static member setInteractivity interactivity world =
            let state = { world.State with Interactivity = interactivity }
            World.setState state world

        /// Update the the level of the world's interactivity.
        static member updateInteractivity updater world =
            let interactivity = updater <| World.getInteractivity world
            World.setInteractivity interactivity world
            
        /// Get the a value from the camera used to view the world.
        static member getCameraBy by world =
            by world.State.Camera

        /// Get the camera used to view the world.
        static member getCamera world =
            World.getCameraBy id world

        static member private setCamera camera world =
            let state = { world.State with Camera = camera }
            World.setState state world

        /// Update the camera used to view the world.
        static member updateCamera updater world =
            let camera = updater <| World.getCamera world
            World.setCamera camera world

        /// Get the current destination screen if a screen transition is currently underway.
        static member getOptScreenTransitionDestination world =
            world.State.OptScreenTransitionDestination

        static member internal setOptScreenTransitionDestination destination world =
            let state = { world.State with OptScreenTransitionDestination = destination }
            World.setState state world

        /// Get the asset metadata map.
        static member getAssetMetadataMap world =
            world.State.AssetMetadataMap

        static member internal setAssetMetadataMap assetMetadataMap world =
            let state = { world.State with AssetMetadataMap = assetMetadataMap }
            World.setState state world

        static member internal setOverlayer overlayer world =
            let state = { world.State with Overlayer = overlayer }
            World.setState state world

        /// Get the user state of the world, casted to 'u.
        static member getUserState world : 'u =
            world.State.UserState :?> 'u

        static member private setUserState (userState : 'u) world =
            let state = { world.State with UserState = userState }
            World.setState state world

        /// Update the user state of the world.
        static member updateUserState (updater : 'u -> 'v) world =
            let state = World.getUserState world
            let state = updater state
            World.setUserState state world