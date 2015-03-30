// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open FSharpx
open FSharpx.Collections
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
                    let eventAddressNameKeys = eventAddress.NameKeys
                    let anyEventAddressNameKeys = AnyEventAddress.NameKeys
                    let anyEventAddresses =
                        [for i in 0 .. List.length eventAddressNameKeys - 1 do
                            let subNameKeys = List.take i eventAddressNameKeys @ anyEventAddressNameKeys
                            yield ktoa subNameKeys]
                    AnyEventAddressesCache.Add (anyEventAddressesKey, anyEventAddresses)
                    anyEventAddresses
            else failwith "Event name cannot be empty."

        static member private getSortableSubscriptions getEntityPublishingPriority (subscriptions : SubscriptionEntry rQueue) world : (single * SubscriptionEntry) list =
            List.foldBack
                (fun (key, simulant : Simulant, subscription) subscriptions ->
                    let priority = simulant.GetPublishingPriority getEntityPublishingPriority world
                    let subscription = (priority, (key, simulant, subscription))
                    subscription :: subscriptions)
                subscriptions
                []

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
                            match subscriber.SimulantAddress.NameKeys with
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

        static member private optEntityStateKeyEquality 
            (entityAddress : EntityState Address, world : World)
            (entityAddress2 : EntityState Address, world2 : World) =
            entityAddress === entityAddress2 && world === world2

        static member private optEntityGetFreshKeyAndValue entity world =
            let optEntityState =
                match entity.EntityAddress.NameKeys with
                | [screenNameKey; groupNameKey; entityNameKey] ->
                    let (_, screenStateMap) = world.SimulantStates 
                    match Map.tryFind screenNameKey.Name screenStateMap with
                    | Some (_, groupStateMap) ->
                        match Map.tryFind groupNameKey.Name groupStateMap with
                        | Some (_, entityStateMap) -> Map.tryFind entityNameKey.Name entityStateMap
                        | None -> None
                    | None -> None
                | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."
            ((entity.EntityAddress, world), optEntityState)

        static member private optEntityStateFinder entity world =
            KeyedCache.getValue
                World.optEntityStateKeyEquality
                (fun () -> World.optEntityGetFreshKeyAndValue entity world)
                (entity.EntityAddress, world)
                world.State.OptEntityCache

        static member private entityStateAdder (entityState : EntityState) entity world =
            match entity.EntityAddress.NameKeys with
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
                    | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent group."
                | None -> failwith <| "Cannot add entity '" + acstring entity.EntityAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member private entityStateRemover entity world =
            match entity.EntityAddress.NameKeys with
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
            | _ -> failwith <| "Invalid entity address '" + acstring entity.EntityAddress + "'."

        static member internal getEntityStateMap group world =
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (_, entityStateMap) -> entityStateMap
                    | None -> Map.empty
                | None -> Map.empty
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getOptEntityState entity world =
            World.optEntityStateFinder entity world

        static member internal getEntityState (entity : Entity) world =
            (World.optEntityStateFinder entity world).Value // OPTIMIZATION: getting entity state as directly as possible

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
            match group.GroupAddress.NameKeys with
            | [screenNameKey; groupNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    match Map.tryFind groupNameKey.Name groupStateMap with
                    | Some (groupState, _) -> Some groupState
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateAdder (groupState : GroupState) group world =
            match group.GroupAddress.NameKeys with
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
                | None -> failwith <| "Cannot add group '" + acstring group.GroupAddress + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member private groupStateRemover group world =
            match group.GroupAddress.NameKeys with
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
                        else failwith <| "Cannot remove group " + acstring group.GroupAddress + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring group.GroupAddress + "'."

        static member internal getGroupStateMap screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (_, screenStateMap) = world.SimulantStates
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) -> groupStateMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getOptGroupState group world =
            World.optGroupStateFinder group world

        static member internal getGroupState group world =
            (World.optGroupStateFinder group world).Value // OPTIMIZATION: getting entity state as directly as possible

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
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (_, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (screenState, _) -> Some screenState
                | None -> None
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateAdder (screenState : ScreenState) screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    let screenStateMap = Map.add screenNameKey.Name (screenState, groupStateMap) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
                | None ->
                    let screenStateMap = Map.add screenNameKey.Name (screenState, Map.empty) screenStateMap
                    { world with SimulantStates = (gameState, screenStateMap) }
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member private screenStateRemover screen world =
            match screen.ScreenAddress.NameKeys with
            | [screenNameKey] ->
                let (gameState, screenStateMap) = world.SimulantStates 
                match Map.tryFind screenNameKey.Name screenStateMap with
                | Some (_, groupStateMap) ->
                    if Map.isEmpty groupStateMap then
                        let screenStateMap = Map.remove screenNameKey.Name screenStateMap
                        { world with SimulantStates = (gameState, screenStateMap) }
                    else failwith <| "Cannot remove screen " + acstring screen.ScreenAddress + ", which still contains groups."
                | None -> world
            | _ -> failwith <| "Invalid screen address '" + acstring screen.ScreenAddress + "'."

        static member internal getScreenStateMap world =
            snd world.SimulantStates

        static member internal getOptScreenState screen world =
            World.optScreenStateFinder screen world

        static member internal getScreenState screen world =
            (World.optScreenStateFinder screen world).Value // OPTIMIZATION: getting entity state as directly as possible

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