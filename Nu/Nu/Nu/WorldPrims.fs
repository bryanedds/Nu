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

    (* Tiling functions. *)

    let makeTileMapData tileMapAsset world =
        let (_, _, map) = Metadata.getTileMapMetadata tileMapAsset.TileMapAssetName tileMapAsset.PackageName world.AssetMetadataMap
        let mapSize = (map.Width, map.Height)
        let tileSize = (map.TileWidth, map.TileHeight)
        let tileSizeF = Vector2 (single <| fst tileSize, single <| snd tileSize)
        let tileMapSize = (fst mapSize * fst tileSize, snd mapSize * snd tileSize)
        let tileMapSizeF = Vector2 (single <| fst tileMapSize, single <| snd tileMapSize)
        let tileSet = map.Tilesets.[0] // MAGIC_VALUE: I'm not sure how to properly specify this
        let optTileSetWidth = tileSet.Image.Width
        let optTileSetHeight = tileSet.Image.Height
        let tileSetSize = (optTileSetWidth.Value / fst tileSize, optTileSetHeight.Value / snd tileSize)
        { Map = map; MapSize = mapSize; TileSize = tileSize; TileSizeF = tileSizeF; TileMapSize = tileMapSize; TileMapSizeF = tileMapSizeF; TileSet = tileSet; TileSetSize = tileSetSize }

    let makeTileData (tileMap : Entity) tmd (tl : TmxLayer) tileIndex =
        let mapRun = fst tmd.MapSize
        let tileSetRun = fst tmd.TileSetSize
        let (i, j) = (tileIndex % mapRun, tileIndex / mapRun)
        let tile = tl.Tiles.[tileIndex]
        let gid = tile.Gid - tmd.TileSet.FirstGid
        let gidPosition = gid * fst tmd.TileSize
        let gid2 = (gid % tileSetRun, gid / tileSetRun)
        let tileMapPosition = tileMap.Position
        let tilePosition = (
            int tileMapPosition.X + fst tmd.TileSize * i,
            int tileMapPosition.Y - snd tmd.TileSize * (j + 1)) // subtraction for right-handedness
        let optTileSetTile = Seq.tryFind (fun (item : TmxTilesetTile) -> tile.Gid - 1 = item.Id) tmd.TileSet.Tiles
        { Tile = tile; I = i; J = j; Gid = gid; GidPosition = gidPosition; Gid2 = gid2; TilePosition = tilePosition; OptTileSetTile = optTileSetTile }

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

    let private entityFinder (address : Address) world (child : Entity) =
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

    let worldEntity address =
        { Get = fun world -> Option.get <| optEntityFinder address world
          Set = fun entity world -> entityFinder address world entity }

    let worldOptEntity address =
        { Get = fun world -> optEntityFinder address world
          Set = fun optEntity world -> match optEntity with None -> entityRemover address world | Some entity -> entityFinder address world entity }

    let worldEntities address =
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
    let setOptEntity address optEntity world = set optEntity world <| worldOptEntity address
    let withOptEntity fn address world = Sim.withOptSimulant worldOptEntity fn address world
    let withOptEntityAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptEntity fn address world

    let tryWithEntity fn address world = Sim.tryWithSimulant worldOptEntity worldEntity fn address world
    let tryWithEntityAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptEntity worldEntity fn address world
    
    let getEntities address world = get world <| worldEntities address
    let setEntities address entities world = set entities world <| worldEntities address

    let registerEntity address (entity : Entity) world =
        entity.Register (address, world)

    let unregisterEntity address world =
        let entity = getEntity address world
        entity.Unregister (address, world)

    let removeEntity address world =
        let world = unregisterEntity address world
        setOptEntity address None world

    let clearEntities (address : Address) world =
        let entities = getEntities address world
        Map.fold
            (fun world entityName _ -> removeEntity (address @ [entityName]) world)
            world
            entities

    let removeEntities (screenAddress : Address) entityNames world =
        List.fold
            (fun world entityName -> removeEntity (screenAddress @ [entityName]) world)
            world
            entityNames

    let addEntity address entity world =
        let world =
            match getOptEntity address world with
            | None -> world
            | Some _ -> removeEntity address world
        let world = setEntity address entity world
        registerEntity address entity world

    let addEntities groupAddress entities world =
        List.fold
            (fun world entity -> addEntity (addrstr groupAddress entity.Name) entity world)
            world
            entities

    let private sortFstAsc (priority, _) (priority2, _) =
        if priority = priority2 then 0
        elif priority > priority2 then -1
        else 1

    let pickingSort entities =
        let priorities = List.map Entity.getPickingPriority entities
        let prioritiesAndEntities = List.zip priorities entities
        let prioritiesAndEntitiesSorted = List.sortWith sortFstAsc prioritiesAndEntities
        List.map snd prioritiesAndEntitiesSorted

    let tryPickEntity position entities camera =
        let entitiesSorted = pickingSort entities
        List.tryFind
            (fun entity ->
                let positionEntity = Entity.mouseToEntity position camera entity
                let transform = Entity.getTransform entity
                let picked = NuMath.isInBox3 positionEntity transform.Position transform.Size
                picked)
            entitiesSorted

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

    let worldGroup address =
        { Get = fun world -> Option.get <| optGroupFinder address world
          Set = fun group world -> groupAdder address world group }

    let worldOptGroup address =
        { Get = fun world -> optGroupFinder address world
          Set = fun optGroup world -> match optGroup with None -> groupRemover address world | Some group -> groupAdder address world group }

    let worldGroups address =
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
    let setOptGroup address optGroup world = set optGroup world <| worldOptGroup address
    let withOptGroup fn address world = Sim.withOptSimulant worldOptGroup fn address world
    let withOptGroupAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptGroup fn address world

    let tryWithGroup fn address world = Sim.tryWithSimulant worldOptGroup worldGroup fn address world
    let tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptGroup worldGroup fn address world
    
    let getGroups address world = get world <| worldGroups address
    let setGroups address groups world = set groups world <| worldGroups address

    let registerGroup address entities (group : Group) world =
        group.Register (address, entities, world)

    let unregisterGroup address world =
        let group = getGroup address world
        group.Unregister (address, world)

    let removeGroup address world =
        let world = unregisterGroup address world
        setOptGroup address None world

    let clearGroups (address : Address) world =
        let groups = getGroups address world
        Map.fold
            (fun world groupName _ -> removeGroup (address @ [groupName]) world)
            world
            groups

    let removeGroups (screenAddress : Address) groupNames world =
        List.fold
            (fun world groupName -> removeGroup (screenAddress @ [groupName]) world)
            world
            groupNames

    let addGroup address (group : Group) entities world =
        let world =
            match getOptGroup address world with
            | None -> world
            | Some _ -> removeGroup address world
        let world = setGroup address group world
        registerGroup address entities group world

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

    let worldScreen address =
        { Get = fun world -> Option.get <| optScreenFinder address world
          Set = fun screen world -> screenAdder address world screen }

    let worldOptScreen address =
        { Get = fun world -> optScreenFinder address world
          Set = fun optScreen world -> match optScreen with None -> screenRemover address world | Some screen -> screenAdder address world screen }

    let worldScreens address =
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
    let setOptScreen address optScreen world = set optScreen world <| worldOptScreen address
    let withOptScreen fn address world = Sim.withOptSimulant worldOptScreen fn address world
    let withOptScreenAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptScreen fn address world

    let tryWithScreen fn address world = Sim.tryWithSimulant worldOptScreen worldScreen fn address world
    let tryWithScreenAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptScreen worldScreen fn address world

    let getScreens address world = get world <| worldScreens address
    let setScreens address screens world = set screens world <| worldScreens address

    let registerScreen address (screen : Screen) groupDescriptors world =
        screen.Register (address, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = getScreen address world
        screen.Unregister (address, world)

    let removeScreen address world =
        let world = unregisterScreen address world
        setOptScreen address None world

    let addScreen address screen groupDescriptors world =
        let world =
            match getOptScreen address world with
            | None -> world
            | Some _ -> removeScreen address world
        let world = setScreen address screen world
        registerScreen address screen groupDescriptors world

    (* Game functions. *)

    let worldGame =
        { Get = fun world -> world.Game
          Set = fun game world -> { world with Game = game }}

    let worldOptSelectedScreenAddress =
        worldGame >>| Game.gameOptSelectedScreenAddress

    let worldOptSelectedScreen =
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

    let getOptSelectedScreenAddress world = get world worldOptSelectedScreenAddress
    let setOptSelectedScreenAddress optAddress world = set optAddress world worldOptSelectedScreenAddress

    let getOptSelectedScreen world = get world worldOptSelectedScreen
    let setOptSelectedScreen optScreen world = set optScreen world worldOptSelectedScreen

    (* Normal functions. *)

    /// Initialize Nu's various type converters.
    /// Must be called for reflection to work in Nu.
    let initTypeConverters () =
        NuMath.initTypeConverters ()
        Audio.initTypeConverters ()
        Rendering.initTypeConverters ()

    let private getSimulant address world =
        match address with
        | [] -> Game <| world.Game
        | [_] as screenAddress -> Screen <| getScreen screenAddress world
        | [_; _] as groupAddress -> Group <| getGroup groupAddress world
        | [_; _; _] as entityAddress -> Entity <| getEntity entityAddress world
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let private getOptSimulant address world =
        match address with
        | [] -> Some <| Game world.Game
        | [_] as screenAddress -> Option.map Screen <| getOptScreen screenAddress world
        | [_; _] as groupAddress -> Option.map Group <| getOptGroup groupAddress world
        | [_; _; _] as entityAddress -> Option.map Entity <| getOptEntity entityAddress world
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let private isAddressSelected address world =
        let optScreenAddress = getOptSelectedScreenAddress world
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: _, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let private getPublishingPriority simulant =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> Entity.getPickingPriority entity

    let private getSubscriptionDescriptors subscriptions world =
        let optSimulants =
            List.map
                (fun (address, subscription) ->
                    let optSimulant = getOptSimulant address world
                    Option.map (fun simulant -> (getPublishingPriority simulant, (address, subscription))) optSimulant)
                subscriptions
        List.definitize optSimulants

    let private subscriptionSort subscriptions world =
        let subscriptionDescriptors = getSubscriptionDescriptors subscriptions world
        let subscriptionDescriptors = List.sortWith sortFstAsc subscriptionDescriptors
        List.map snd subscriptionDescriptors

    /// Publish a message for the given event.
    let rec publish event publisher messageData world =
        let events = List.collapseLeft event
        let optSubLists = List.map (fun event -> Map.tryFind event world.Subscriptions) events
        let subLists = List.definitize optSubLists
        let subList = List.concat subLists
        let subListSorted = subscriptionSort subList world
        let (_, liveness, world) =
            List.foldWhile
                (fun (messageHandled, liveness, world) (subscriber, subscription) ->
                    let message = { Event = event; Publisher = publisher; Subscriber = subscriber; Data = messageData }
                    if messageHandled = Handled || liveness = Exiting then None
                    elif not <| isAddressSelected subscriber world then Some (messageHandled, liveness, world)
                    else
                        let result =
                            match subscription with
                            | ExitSub -> handleEventAsExit world
                            | SwallowSub -> handleEventAsSwallow world
                            | ScreenTransitionSub destination -> handleEventAsScreenTransition destination world
                            | ScreenTransitionFromSplashSub destination -> handleEventAsScreenTransitionFromSplash destination world
                            | CustomSub fn ->
                                match getOptSimulant message.Subscriber world with
                                | None -> (Unhandled, liveness, world)
                                | Some _ -> fn message world
                        Some result)
                (Unhandled, Running, world)
                subListSorted
        (liveness, world)

    /// Subscribe to messages for the given event.
    and subscribe event subscriber subscription world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind event subs
        { world with
            Subscriptions =
                match optSubList with
                | None -> Map.add event [(subscriber, subscription)] subs
                | Some subList -> Map.add event ((subscriber, subscription) :: subList) subs }

    /// Unsubscribe to messages for the given event.
    and unsubscribe event subscriber world =
        let subs = world.Subscriptions
        let optSubList = Map.tryFind event subs
        match optSubList with
        | None -> world
        | Some subList ->
            let subList = List.remove (fun (address, _) -> address = subscriber) subList
            let subscriptions = Map.add event subList subs
            { world with Subscriptions = subscriptions }

    /// Execute a procedure within the context of a given subscription for the given event.
    and withSubscription event subscription subscriber procedure world =
        let world = subscribe event subscriber subscription world
        let world = procedure world
        unsubscribe event subscriber world
    
    and handleEventAsSwallow (world : World) =
        (Handled, Running, world)

    and handleEventAsExit (world : World) =
        (Handled, Exiting, world)

    and private setScreenStatePlus address state world =
        let world = withScreen (fun screen -> { screen with State = state }) address world
        match state with
        | IdlingState ->
            world |>
                unsubscribe DownMouseLeftEvent address |>
                unsubscribe UpMouseLeftEvent address
        | IncomingState | OutgoingState ->
            world |>
                subscribe DownMouseLeftEvent address SwallowSub |>
                subscribe UpMouseLeftEvent address SwallowSub

    and transitionScreen destination world =
        let world = setScreenStatePlus destination IncomingState world
        setOptSelectedScreenAddress (Some destination) world

    and private updateTransition1 (transition : Transition) =
        if transition.TransitionTicks = transition.TransitionLifetime then (true, { transition with TransitionTicks = 0L })
        else (false, { transition with TransitionTicks = transition.TransitionTicks + 1L })

    and internal updateTransition update world =
        let (liveness, world) =
            match getOptSelectedScreenAddress world with
            | None -> (Running, world)
            | Some selectedScreenAddress ->
                let selectedScreen = getScreen selectedScreenAddress world
                match selectedScreen.State with
                | IncomingState ->
                    let (finished, incoming) = updateTransition1 selectedScreen.Incoming
                    let selectedScreen = { selectedScreen with Incoming = incoming }
                    let world = setScreen selectedScreenAddress selectedScreen world
                    let world = setScreenStatePlus selectedScreenAddress (if finished then IdlingState else IncomingState) world
                    let (liveness, world) =
                        if not finished then (Running, world)
                        else publish (FinishedIncomingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                    match liveness with
                    | Exiting -> (liveness, world)
                    | Running ->
                        if selectedScreen.Incoming.TransitionTicks <> 0L then (liveness, world)
                        else publish (SelectedEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                | OutgoingState ->
                    let (finished, outgoing) = updateTransition1 selectedScreen.Outgoing
                    let selectedScreen = { selectedScreen with Outgoing = outgoing }
                    let world = setScreen selectedScreenAddress selectedScreen world
                    let world = setScreenStatePlus selectedScreenAddress (if finished then IdlingState else OutgoingState) world
                    if not finished then (Running, world)
                    else
                        let (liveness, world) = publish (DeselectedEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                        match liveness with
                        | Exiting -> (liveness, world)
                        | Running -> publish (FinishedOutgoingEvent @ selectedScreenAddress) selectedScreenAddress NoData world
                | IdlingState -> (Running, world)
        match liveness with
        | Exiting -> (liveness, world)
        | Running -> update world

    and private handleSplashScreenIdleTick idlingTime ticks message world =
        let world = unsubscribe message.Event message.Subscriber world
        if ticks < idlingTime then
            let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime -<| incL ticks
            let world = subscribe message.Event message.Subscriber subscription world
            (Unhandled, Running, world)
        else
            match getOptSelectedScreenAddress world with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (Handled, Exiting, world)
            | Some selectedScreenAddress ->
                let world = setScreenStatePlus selectedScreenAddress OutgoingState world
                (Unhandled, Running, world)

    and internal handleSplashScreenIdle idlingTime message world =
        let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime 0L
        let world = subscribe TickEvent message.Subscriber subscription world
        (Handled, Running, world)

    and private handleFinishedScreenOutgoing destination message world =
        let world = unsubscribe message.Event message.Subscriber world
        let world = transitionScreen destination world
        (Handled, Running, world)

    and handleEventAsScreenTransitionFromSplash destination world =
        let world = transitionScreen destination world
        (Unhandled, Running, world)

    and handleEventAsScreenTransition destination world =
        match getOptSelectedScreenAddress world with
        | None ->
            trace "Program Error: Could not handle screen transition due to no selected screen."
            (Handled, Exiting, world)
        | Some selectedScreenAddress ->
            let sub = CustomSub (fun _ world ->
                let world = unsubscribe (NuConstants.FinishedOutgoingEvent @ selectedScreenAddress) selectedScreenAddress world
                let world = transitionScreen destination world
                (Unhandled, Running, world))
            let world = setScreenStatePlus selectedScreenAddress OutgoingState world
            let world = subscribe (NuConstants.FinishedOutgoingEvent @ selectedScreenAddress) selectedScreenAddress sub world
            (Unhandled, Running, world)

    let removeEntityPlus address world =
        let world = removeEntity address world
        publish (RemovedEvent @ address) address NoData world

    let addEntityPlus address entity world =
        let world = addEntity address entity world
        publish (AddedEvent @ address) address NoData world