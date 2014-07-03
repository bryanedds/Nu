namespace Nu
open System
open System.Collections.Generic
open FSharpx
open FSharpx.Lens.Operators
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuConstants

// TODO: when module exportation is implemented in F# - http://fslang.uservoice.com/forums/245727-f-language/suggestions/5688199-allow-re-exporting-from-modules
// make this module exported from World and rename to WorldPrimsModule.

[<AutoOpen>]
module WorldPrimsModule =

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : int * int
          TileSize : int * int
          TileSizeF : Vector2
          TileMapSize : int * int
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : int * int }

    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : int * int
          OptTileSetTile : TmxTilesetTile option
          TilePosition : int * int }

[<RequireQualifiedAccess>]
module WorldPrims =

    (* Message functions. *)

    /// Make a subscription key.
    let makeSubscriptionKey =
        Guid.NewGuid

    let mutable publish =
        Unchecked.defaultof<SubscriptionSorter -> Address -> Address -> MessageData -> World -> World>

    let mutable publish4 =
        Unchecked.defaultof<Address -> Address -> MessageData -> World -> World>

    let mutable subscribe =
        Unchecked.defaultof<Guid -> Address -> Address -> Subscription -> World -> World>

    let mutable subscribe4 =
        Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>

    let mutable unsubscribe =
        Unchecked.defaultof<Guid -> World -> World>

    let mutable withSubscription =
        Unchecked.defaultof<Address -> Address -> Subscription -> (World -> World) -> World -> World>

    let mutable observe =
        Unchecked.defaultof<Address -> Address -> Subscription -> World -> World>

    (* Entity functions. *)

    let private optEntityFinder (address : Address) world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None -> None
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None -> None
            | Some entityMap -> Map.tryFind (List.at 2 address) entityMap

    let private entityAdder (address : Address) world (child : Entity) =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None ->
            let entityMap = Map.singleton (List.at 2 address) child
            let groupMap = Map.singleton (List.at 1 address) entityMap
            { world with Entities = Map.add (List.at 0 address) groupMap world.Entities }
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None ->
                let entityMap = Map.singleton (List.at 2 address) child
                let groupMap = Map.add (List.at 1 address) entityMap groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap world.Entities }
            | Some entityMap ->
                let entityMap = Map.add (List.at 2 address) child entityMap
                let groupMap = Map.add (List.at 1 address) entityMap groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap world.Entities }

    let private entityRemover (address : Address) world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Entities
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let optEntityMap = Map.tryFind (List.at 1 address) groupMap
            match optEntityMap with
            | None -> world
            | Some entityMap ->
                let entityMap = Map.remove (List.at 2 address) entityMap
                let groupMap = Map.add (List.at 1 address) entityMap groupMap
                { world with Entities = Map.add (List.at 0 address) groupMap world.Entities }

    let private worldEntity address =
        { Get = fun world -> Option.get <| optEntityFinder address world
          Set = fun entity world -> entityAdder address world entity }

    let private worldOptEntity address =
        { Get = fun world -> optEntityFinder address world
          Set = fun optEntity world -> match optEntity with None -> entityRemover address world | Some entity -> entityAdder address world entity }

    let private worldEntities address =
        { Get = fun world ->
            match address with
            | [screenStr; groupStr] ->
                match Map.tryFind screenStr world.Entities with
                | None -> Map.empty
                | Some groupMap ->
                    match Map.tryFind groupStr groupMap with
                    | None -> Map.empty
                    | Some entityMap -> entityMap
            | _ -> failwith <| "Invalid entity address '" + addrToStr address + "'."
          Set = fun entities world ->
            match address with
            | [screenStr; groupStr] ->
                match Map.tryFind screenStr world.Entities with
                | None -> { world with Entities = Map.add screenStr (Map.singleton groupStr entities) world.Entities }
                | Some groupMap ->
                    match Map.tryFind groupStr groupMap with
                    | None -> { world with Entities = Map.add screenStr (Map.add groupStr entities groupMap) world.Entities }
                    | Some entityMap -> { world with Entities = Map.add screenStr (Map.add groupStr (Map.addMany (Map.toSeq entities) entityMap) groupMap) world.Entities }
            | _ -> failwith <| "Invalid entity address '" + addrToStr address + "'." }

    let getEntity address world = get world <| worldEntity address
    let setEntity address entity world = set entity world <| worldEntity address
    let withEntity fn address world = Sim.withSimulant worldEntity fn address world
    let withEntityAndWorld fn address world = Sim.withSimulantAndWorld worldEntity fn address world

    let getOptEntity address world = get world <| worldOptEntity address
    let containsEntity address world = Option.isSome <| getOptEntity address world
    let private setOptEntity address optEntity world = set optEntity world <| worldOptEntity address
    let tryWithEntity fn address world = Sim.tryWithSimulant worldOptEntity worldEntity fn address world
    let tryWithEntityAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptEntity worldEntity fn address world
    
    let getEntities address world = get world <| worldEntities address
    let private setEntities address entities world = set entities world <| worldEntities address

    let registerEntity address (entity : Entity) world =
        entity.Register (address, world)

    let unregisterEntity address world =
        let entity = getEntity address world
        entity.Unregister (address, world)

    let removeEntityImmediate (address : Address) world =
        let world = publish4 (RemovingEvent @ address) address NoData world
        let world = unregisterEntity address world
        setOptEntity address None world

    let removeEntity address world =
        let task =
            { Time = world.Ticks
              Operation = fun world -> if containsEntity address world then removeEntityImmediate address world else world }
        { world with Tasks = task :: world.Tasks }

    let clearEntitiesImmediate (address : Address) world =
        let entities = getEntities address world
        Map.fold
            (fun world entityName _ -> removeEntityImmediate (address @ [entityName]) world)
            world
            entities

    let clearEntities (address : Address) world =
        let entities = getEntities address world
        Map.fold
            (fun world entityName _ -> removeEntity (address @ [entityName]) world)
            world
            entities

    let removeEntitiesImmediate (screenAddress : Address) entityNames world =
        List.fold
            (fun world entityName -> removeEntityImmediate (screenAddress @ [entityName]) world)
            world
            entityNames

    let removeEntities (screenAddress : Address) entityNames world =
        List.fold
            (fun world entityName -> removeEntity (screenAddress @ [entityName]) world)
            world
            entityNames

    let addEntity address entity world =
        let world =
            match getOptEntity address world with
            | None -> world
            | Some _ -> removeEntityImmediate address world
        let world = setEntity address entity world
        let world = registerEntity address entity world
        publish4 (AddedEvent @ address) address NoData world

    let addEntities groupAddress entities world =
        List.fold
            (fun world entity -> addEntity (addrstr groupAddress entity.Name) entity world)
            world
            entities

    (* Group functions. *)

    let private optGroupFinder (address : Address) world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind (List.at 1 address) groupMap

    let private groupAdder (address : Address) world child =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None ->
            { world with Groups = Map.singleton (List.at 0 address) <| Map.singleton (List.at 1 address) child }
        | Some groupMap ->
            let groupMap = Map.add (List.at 1 address) child groupMap
            { world with Groups = Map.add (List.at 0 address) groupMap world.Groups }

    let private groupRemover (address : Address) world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let groupMap = Map.remove (List.at 1 address) groupMap
            { world with Groups = Map.add (List.at 0 address) groupMap world.Groups }

    let private worldGroup address =
        { Get = fun world -> Option.get <| optGroupFinder address world
          Set = fun group world -> groupAdder address world group }

    let private worldOptGroup address =
        { Get = fun world -> optGroupFinder address world
          Set = fun optGroup world -> match optGroup with None -> groupRemover address world | Some group -> groupAdder address world group }

    let private worldGroups address =
        { Get = fun world ->
            match address with
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'."
          Set = fun groups world ->
            match address with
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | None -> { world with Groups = Map.add screenStr groups world.Groups }
                | Some groupMap -> { world with Groups = Map.add screenStr (Map.addMany (Map.toSeq groups) groupMap) world.Groups }
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'." }
            
    let getGroup address world = get world <| worldGroup address
    let setGroup address group world = set group world <| worldGroup address
    let withGroup fn address world = Sim.withSimulant worldGroup fn address world
    let withGroupAndWorld fn address world = Sim.withSimulantAndWorld worldGroup fn address world

    let getOptGroup address world = get world <| worldOptGroup address
    let containsGroup address world = Option.isSome <| getOptGroup address world
    let private setOptGroup address optGroup world = set optGroup world <| worldOptGroup address
    let tryWithGroup fn address world = Sim.tryWithSimulant worldOptGroup worldGroup fn address world
    let tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptGroup worldGroup fn address world
    
    let getGroups address world = get world <| worldGroups address
    let private setGroups address groups world = set groups world <| worldGroups address

    let registerGroup address (group : Group) world =
        group.Register (address, world)

    let unregisterGroup address world =
        let group = getGroup address world
        group.Unregister (address, world)

    let removeGroupImmediate (address : Address) world =
        let world = publish4 (RemovingEvent @ address) address NoData world
        let world = unregisterGroup address world
        let world = clearEntitiesImmediate address world
        setOptGroup address None world

    let removeGroup address world =
        let task =
            { Time = world.Ticks
              Operation = fun world -> if containsGroup address world then removeGroupImmediate address world else world }
        { world with Tasks = task :: world.Tasks }

    let clearGroupsImmediate (address : Address) world =
        let groups = getGroups address world
        Map.fold
            (fun world groupName _ -> removeGroupImmediate (address @ [groupName]) world)
            world
            groups

    let clearGroups (address : Address) world =
        let groups = getGroups address world
        Map.fold
            (fun world groupName _ -> removeGroup (address @ [groupName]) world)
            world
            groups

    let removeGroupsImmediate (screenAddress : Address) groupNames world =
        List.fold
            (fun world groupName -> removeGroupImmediate (screenAddress @ [groupName]) world)
            world
            groupNames

    let removeGroups (screenAddress : Address) groupNames world =
        List.fold
            (fun world groupName -> removeGroup (screenAddress @ [groupName]) world)
            world
            groupNames

    let addGroup address (group : Group) entities world =
        let world =
            match getOptGroup address world with
            | None -> world
            | Some _ -> removeGroupImmediate address world
        let world = setGroup address group world
        let world = addEntities address entities world
        let world = registerGroup address group world
        publish4 (AddedEvent @ address) address NoData world

    let addGroups screenAddress groupDescriptors world =
        List.fold
            (fun world (groupName, group, entities) -> addGroup (screenAddress @ [groupName]) group entities world)
            world
            groupDescriptors

    (* Screen functions. *)
    
    let private optScreenFinder (address : Address) world =
        Map.tryFind (List.at 0 address) world.Screens

    let private screenAdder (address : Address) world child =
        { world with Screens = Map.add (List.at 0 address) child world.Screens }

    let private screenRemover (address : Address) world =
        { world with Screens = Map.remove (List.at 0 address) world.Screens }

    let private worldScreen address =
        { Get = fun world -> Option.get <| optScreenFinder address world
          Set = fun screen world -> screenAdder address world screen }

    let private worldOptScreen address =
        { Get = fun world -> optScreenFinder address world
          Set = fun optScreen world -> match optScreen with None -> screenRemover address world | Some screen -> screenAdder address world screen }

    let private worldScreens address =
        { Get = fun world ->
            match address with
            | [] -> world.Screens
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'."
          Set = fun screens world ->
            match address with
            | [] -> { world with Screens = Map.addMany (Map.toSeq screens) world.Screens }
            | _ -> failwith <| "Invalid screen address '" + addrToStr address + "'." }

    let worldScreenIncoming address = worldScreen address >>| Screen.screenIncoming
    let worldScreenOutgoing address = worldScreen address >>| Screen.screenOutgoing

    let getScreen address world = get world <| worldScreen address
    let setScreen address screen world = set screen world <| worldScreen address
    let withScreen fn address world = Sim.withSimulant worldScreen fn address world
    let withScreenAndWorld fn address world = Sim.withSimulantAndWorld worldScreen fn address world
    
    let getOptScreen address world = get world <| worldOptScreen address
    let containsScreen address world = Option.isSome <| getOptScreen address world
    let private setOptScreen address optScreen world = set optScreen world <| worldOptScreen address
    let tryWithScreen fn address world = Sim.tryWithSimulant worldOptScreen worldScreen fn address world
    let tryWithScreenAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptScreen worldScreen fn address world

    let getScreens address world = get world <| worldScreens address
    let private setScreens address screens world = set screens world <| worldScreens address

    let registerScreen address (screen : Screen) world =
        screen.Register (address, world)

    let unregisterScreen address world =
        let screen = getScreen address world
        screen.Unregister (address, world)

    let removeScreenImmediate (address : Address) world =
        let world = publish4 (RemovingEvent @ address) address NoData world
        let world = clearGroupsImmediate address world
        let world = unregisterScreen address world
        setOptScreen address None world

    let removeScreen address world =
        let task =
            { Time = world.Ticks
              Operation = fun world -> if containsScreen address world then removeScreenImmediate address world else world }
        { world with Tasks = task :: world.Tasks }

    let addScreen address screen groupDescriptors world =
        let world =
            match getOptScreen address world with
            | None -> world
            | Some _ -> removeScreenImmediate address world
        let world = setScreen address screen world
        let world = addGroups address groupDescriptors world
        let world = registerScreen address screen world
        publish4 (AddedEvent @ address) address NoData world

    (* Game functions. *)

    let private worldGame =
        { Get = fun world -> world.Game
          Set = fun game world -> { world with Game = game }}

    let private worldOptSelectedScreenAddress =
        worldGame >>| Game.gameOptSelectedScreenAddress

    let private worldOptSelectedScreen =
        { Get = fun world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddress
            match optSelectedScreenAddress with
            | None -> None
            | Some selectedScreenAddress -> get world <| worldOptScreen selectedScreenAddress
          Set = fun screen world ->
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddress
            match optSelectedScreenAddress with
            | None -> failwith "Cannot set a non-existent screen."
            | Some selectedScreenAddress -> set screen.Value world <| worldScreen selectedScreenAddress }

    let private worldSelectedScreenAddress =
        { Get = fun world -> Option.get <| get world worldOptSelectedScreenAddress
          Set = fun address world -> set (Some address) world worldOptSelectedScreenAddress }

    let private worldSelectedScreen =
        { Get = fun world -> Option.get <| get world worldOptSelectedScreen
          Set = fun screen world -> set (Some screen) world worldOptSelectedScreen }

    let getSelectedScreenAddress world = get world worldSelectedScreenAddress
    let setSelectedScreenAddress address world = set address world worldSelectedScreenAddress
    let getOptSelectedScreenAddress world = get world worldOptSelectedScreenAddress
    let setOptSelectedScreenAddress optAddress world = set optAddress world worldOptSelectedScreenAddress

    let getSelectedScreen world = get world worldSelectedScreen
    let setSelectedScreen screen world = set screen world worldSelectedScreen
    let getOptSelectedScreen world = get world worldOptSelectedScreen
    let setOptSelectedScreen optScreen world = set optScreen world worldOptSelectedScreen

    (* Normal functions. *)

    let private ScreenTransitionKeys =
        (makeSubscriptionKey (), makeSubscriptionKey ())

    let private SplashScreenTickKey =
        makeSubscriptionKey ()

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        NuMath.initTypeConverters ()
        Audio.initTypeConverters ()
        Rendering.initTypeConverters ()

    let isAddressSelected (address : Address) world =
        let optScreenAddress = getOptSelectedScreenAddress world
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead
    
    let handleEventAsSwallow (world : World) =
        (Handled, world)

    let handleEventAsExit (world : World) =
        (Handled, { world with Liveness = Exiting })

    let private setScreenStatePlus address state world =
        // TODO: add swallowing for other types of input as well (keys, joy buttons, etc.)
        let world = withScreen (fun screen -> { screen with State = state }) address world
        match state with
        | IdlingState ->
            world |>
                unsubscribe -<| fst ScreenTransitionKeys |>
                unsubscribe -<| snd ScreenTransitionKeys
        | IncomingState | OutgoingState ->
            world |>
                subscribe (fst ScreenTransitionKeys) (DownMouseEvent @ AnyEvent) address SwallowSub |>
                subscribe (snd ScreenTransitionKeys) (UpMouseEvent @ AnyEvent) address SwallowSub

    let selectScreen destination world =
        let world = setScreenStatePlus destination IncomingState world
        setOptSelectedScreenAddress (Some destination) world

    let transitionScreen destination world =
        match getOptSelectedScreenAddress world with
        | None ->
            trace "Program Error: Could not handle screen transition due to no selected screen."
            { world with Liveness = Exiting }
        | Some selectedScreenAddress ->
            let subscriptionKey = makeSubscriptionKey ()
            let sub = CustomSub (fun _ world ->
                let world = unsubscribe subscriptionKey world
                let world = selectScreen destination world
                (Unhandled, world))
            let world = setScreenStatePlus selectedScreenAddress OutgoingState world
            subscribe subscriptionKey (FinishOutgoingEvent @ selectedScreenAddress) selectedScreenAddress sub world

    let private updateTransition1 (transition : Transition) =
        if transition.TransitionTicks = transition.TransitionLifetime then (true, { transition with TransitionTicks = 0L })
        else (false, { transition with TransitionTicks = transition.TransitionTicks + 1L })

    let internal updateTransition update world =
        let world =
            match getOptSelectedScreenAddress world with
            | None -> world
            | Some selectedScreenAddress ->
                let selectedScreen = getScreen selectedScreenAddress world
                match selectedScreen.State with
                | IncomingState ->
                    let world =
                        if selectedScreen.Incoming.TransitionTicks <> 0L then world
                        else publish4 (SelectEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                    match world.Liveness with
                    | Exiting -> world
                    | Running ->
                        let world =
                            if selectedScreen.Incoming.TransitionTicks <> 0L then world
                            else publish4 (StartIncomingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                        match world.Liveness with
                        | Exiting -> world
                        | Running ->
                            let (finished, incoming) = updateTransition1 selectedScreen.Incoming
                            let selectedScreen = { selectedScreen with Incoming = incoming }
                            let world = setScreen selectedScreenAddress selectedScreen world
                            let world = setScreenStatePlus selectedScreenAddress (if finished then IdlingState else IncomingState) world
                            if not finished then world
                            else publish4 (FinishIncomingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                | OutgoingState ->
                    let world =
                        if selectedScreen.Outgoing.TransitionTicks <> 0L then world
                        else publish4 (StartOutgoingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                    match world.Liveness with
                    | Exiting -> world
                    | Running ->
                        let (finished, outgoing) = updateTransition1 selectedScreen.Outgoing
                        let selectedScreen = { selectedScreen with Outgoing = outgoing }
                        let world = setScreen selectedScreenAddress selectedScreen world
                        let world = setScreenStatePlus selectedScreenAddress (if finished then IdlingState else OutgoingState) world
                        if not finished then world
                        else
                            let world = publish4 (DeselectEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                            match world.Liveness with
                            | Exiting -> world
                            | Running -> publish4 (FinishOutgoingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                | IdlingState -> world
        match world.Liveness with
        | Exiting -> world
        | Running -> update world

    let rec private handleSplashScreenIdleTick idlingTime ticks message world =
        let world = unsubscribe SplashScreenTickKey world
        if ticks < idlingTime then
            let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime -<| incL ticks
            let world = subscribe SplashScreenTickKey message.Event message.Subscriber subscription world
            (Unhandled, world)
        else
            match getOptSelectedScreenAddress world with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (Handled, { world with Liveness = Exiting })
            | Some selectedScreenAddress ->
                let world = setScreenStatePlus selectedScreenAddress OutgoingState world
                (Unhandled, world)

    let internal handleSplashScreenIdle idlingTime message world =
        let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime 0L
        let world = subscribe SplashScreenTickKey TickEvent message.Subscriber subscription world
        (Handled, world)

    let handleEventAsScreenTransitionFromSplash destination world =
        let world = selectScreen destination world
        (Unhandled, world)

    let handleEventAsScreenTransition destination world =
        let world = transitionScreen destination world
        (Unhandled, world)

    let private sortFstDesc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let getSimulant (address : Address) world =
        match address with
        | [] -> Game <| world.Game
        | [_] as screenAddress -> Screen <| getScreen screenAddress world
        | [_; _] as groupAddress -> Group <| getGroup groupAddress world
        | [_; _; _] as entityAddress -> Entity <| getEntity entityAddress world
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let getOptSimulant (address : Address) world =
        match address with
        | [] -> Some <| Game world.Game
        | [_] as screenAddress -> Option.map Screen <| getOptScreen screenAddress world
        | [_; _] as groupAddress -> Option.map Group <| getOptGroup groupAddress world
        | [_; _; _] as entityAddress -> Option.map Entity <| getOptEntity entityAddress world
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let getPublishingPriority getEntityPublishingPriority simulant world =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> getEntityPublishingPriority entity world

    let getSubscriptionSortables getEntityPublishingPriority subscriptions world =
        let optSimulants =
            List.map
                (fun (key, address, subscription) ->
                    let optSimulant = getOptSimulant address world
                    Option.map (fun simulant -> (getPublishingPriority getEntityPublishingPriority simulant world, (key, address, subscription))) optSimulant)
                subscriptions
        List.definitize optSimulants

    let sortSubscriptionsBy getEntityPublishingPriority (subscriptions : SubscriptionEntry list) world =
        let subscriptions = getSubscriptionSortables getEntityPublishingPriority subscriptions world
        let subscriptions = List.sortWith sortFstDesc subscriptions
        List.map snd subscriptions

    let sortSubscriptionsByPickingPriority subscriptions world =
        sortSubscriptionsBy (fun (entity : Entity) world -> entity.GetPickingPriority world) subscriptions world

    let sortSubscriptionsByHierarchy (subscriptions : SubscriptionEntry list) world =
        sortSubscriptionsBy (fun _ _ -> EntityPublishingPriority) subscriptions world

    /// Publish a message for the given event.
    let rec publishDefinition publishSort event publisher messageData world =
        let events = List.collapseLeft event
        let optSubLists = List.map (fun event -> Map.tryFind (event @ AnyEvent) world.Subscriptions) events
        let optSubLists = Map.tryFind event world.Subscriptions :: optSubLists
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        let subListSorted = publishSort subList world
        let (_, world) =
            List.foldWhile
                (fun (messageHandled, world) (_, subscriber, subscription) ->
                    let message = { Event = event; Publisher = publisher; Subscriber = subscriber; Data = messageData }
                    if messageHandled = Handled || world.Liveness = Exiting then None
                    else
                        let result =
                            match subscription with
                            | ExitSub -> handleEventAsExit world
                            | SwallowSub -> handleEventAsSwallow world
                            | ScreenTransitionSub destination -> handleEventAsScreenTransition destination world
                            | ScreenTransitionFromSplashSub destination -> handleEventAsScreenTransitionFromSplash destination world
                            | CustomSub fn ->
                                match getOptSimulant message.Subscriber world with
                                | None -> (Unhandled, world)
                                | Some _ -> fn message world
                        Some result)
                (Unhandled, world)
                subListSorted
        world

    /// Publish a message for the given event.
    and publish4Definition event publisher messageData world =
        publish sortSubscriptionsByHierarchy event publisher messageData world

    /// Subscribe to messages for the given event.
    and subscribeDefinition subscriptionKey event subscriber subscription world =
        let subscriptions = 
            match Map.tryFind event world.Subscriptions with
            | None -> Map.add event [(subscriptionKey, subscriber, subscription)] world.Subscriptions
            | Some subscriptionList -> Map.add event ((subscriptionKey, subscriber, subscription) :: subscriptionList) world.Subscriptions
        let unsubscriptions = Map.add subscriptionKey (event, subscriber) world.Unsubscriptions
        { world with Subscriptions = subscriptions; Unsubscriptions = unsubscriptions }

    /// Subscribe to messages for the given event.
    and subscribe4Definition event subscriber subscription world =
        subscribe (makeSubscriptionKey ()) event subscriber subscription world

    /// Unsubscribe to messages for the given event.
    and unsubscribeDefinition subscriptionKey world =
        match Map.tryFind subscriptionKey world.Unsubscriptions with
        | None -> world // TODO: consider failure signal
        | Some (event, subscriber) ->
            match Map.tryFind event world.Subscriptions with
            | None -> world // TODO: consider failure signal
            | Some subscriptionList ->
                let subscriptionList =
                    List.remove
                        (fun (subscriptionKey', subscriber', _) -> subscriptionKey' = subscriptionKey && subscriber' = subscriber)
                        subscriptionList
                let subscriptions = Map.add event subscriptionList world.Subscriptions
                { world with Subscriptions = subscriptions }

    /// Execute a procedure within the context of a given subscription for the given event.
    and withSubscriptionDefinition event subscriber subscription procedure world =
        let subscriptionKey = makeSubscriptionKey ()
        let world = subscribe subscriptionKey event subscriber subscription world
        let world = procedure world
        unsubscribe subscriptionKey world

    /// Subscribe to messages for the given event during the lifetime of the subscriber.
    and observeDefinition event subscriber subscription world =
        if List.isEmpty subscriber then
            debug "Cannot observe events with an anonymous subscriber."
            world
        else
            let observationKey = makeSubscriptionKey ()
            let removalKey = makeSubscriptionKey ()
            let world = subscribe observationKey event subscriber subscription world
            let sub = CustomSub (fun _ world ->
                let world = unsubscribe removalKey world
                let world = unsubscribe observationKey world
                (Unhandled, world))
            subscribe removalKey (RemovingEvent @ subscriber) subscriber sub world

    publish <- publishDefinition
    publish4 <- publish4Definition
    subscribe <- subscribeDefinition
    subscribe4 <- subscribe4Definition
    unsubscribe <- unsubscribeDefinition
    withSubscription <- withSubscriptionDefinition
    observe <- observeDefinition