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

    let withEntity fn address world = Sim.withSimulant worldEntity fn address world
    let withEntityAndWorld fn address world = Sim.withSimulantAndWorld worldEntity fn address world

    let withOptEntity fn address world = Sim.withOptSimulant worldOptEntity fn address world
    let withOptEntityAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptEntity fn address world

    let tryWithEntity fn address world = Sim.tryWithSimulant worldOptEntity worldEntity fn address world
    let tryWithEntityAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptEntity worldEntity fn address world

    let registerEntity address (entity : Entity) world =
        entity.Register (address, world)

    let unregisterEntity address world =
        let entity = get world <| worldEntity address
        entity.Unregister (address, world)

    let removeEntity address world =
        let world = unregisterEntity address world
        set None world <| worldOptEntity address

    let removeEntities address world =
        let entities = get world <| worldEntities address
        Map.fold
            (fun world entityName _ -> removeEntity (address @ [entityName]) world)
            world
            entities

    let addEntity address entity world =
        let world =
            match get world <| worldOptEntity address with
            | None -> world
            | Some _ -> removeEntity address world
        let world = set entity world <| worldEntity address
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

    let withGroup fn address world = Sim.withSimulant worldGroup fn address world
    let withGroupAndWorld fn address world = Sim.withSimulantAndWorld worldGroup fn address world

    let withOptGroup fn address world = Sim.withOptSimulant worldOptGroup fn address world
    let withOptGroupAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptGroup fn address world

    let tryWithGroup fn address world = Sim.tryWithSimulant worldOptGroup worldGroup fn address world
    let tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptGroup worldGroup fn address world

    let registerGroup address entities (group : Group) world =
        group.Register (address, entities, world)

    let unregisterGroup address world =
        let group = get world <| worldGroup address
        group.Unregister (address, world)

    let removeGroup address world =
        let world = unregisterGroup address world
        set None world <| worldOptGroup address

    let removeGroups address world =
        let groups = get world <| worldGroups address
        Map.fold
            (fun world groupName _ -> removeGroup (address @ [groupName]) world)
            world
            groups

    let addGroup address (group : Group) entities world =
        let world =
            match get world <| worldOptGroup address with
            | None -> world
            | Some _ -> removeGroup address world
        let world = set group world <| worldGroup address
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

    let withScreen fn address world = Sim.withSimulant worldScreen fn address world
    let withScreenAndWorld fn address world = Sim.withSimulantAndWorld worldScreen fn address world

    let withOptScreen fn address world = Sim.withOptSimulant worldOptScreen fn address world
    let withOptScreenAndWorld fn address world = Sim.withOptSimulantAndWorld worldOptScreen fn address world

    let tryWithScreen fn address world = Sim.tryWithSimulant worldOptScreen worldScreen fn address world
    let tryWithScreenAndWorld fn address world = Sim.tryWithSimulantAndWorld worldOptScreen worldScreen fn address world

    let registerScreen address (screen : Screen) groupDescriptors world =
        screen.Register (address, groupDescriptors, world)

    let unregisterScreen address world =
        let screen = get world <| worldScreen address
        screen.Unregister (address, world)

    let removeScreen address world =
        let world = unregisterScreen address world
        set None world <| worldOptScreen address

    let addScreen address screen groupDescriptors world =
        let world =
            match get world <| worldOptScreen address with
            | None -> world
            | Some _ -> removeScreen address world
        let world = set screen world <| worldScreen address
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
        | [_] as screenAddress -> Screen <| get world -<| worldScreen screenAddress
        | [_; _] as groupAddress -> Group <| get world -<| worldGroup groupAddress
        | [_; _; _] as entityAddress -> Entity <| get world -<| worldEntity entityAddress
        | _ -> failwith <| "Invalid simulant address '" + addrToStr address + "'."

    let private getSubscribedSimulants subscriptions world =
        List.map (fun (address, _) -> getSimulant address world) subscriptions

    let private isAddressSelected address world =
        let optScreenAddress = get world worldOptSelectedScreenAddress
        match (address, optScreenAddress) with
        | ([], _) -> true
        | (_, None) -> false
        | (_, Some []) -> false
        | (addressHead :: addressTail, Some (screenAddressHead :: _)) -> addressHead = screenAddressHead

    let private getPublishingPriority simulant =
        match simulant with
        | Game _ -> GamePublishingPriority
        | Screen _ -> ScreenPublishingPriority
        | Group _ -> GroupPublishingPriority
        | Entity entity -> Entity.getPickingPriority entity

    let private subscriptionSort subscriptions world =
        let simulants = getSubscribedSimulants subscriptions world
        let priorities = List.map getPublishingPriority simulants
        let prioritiesAndSubscriptions = List.zip priorities subscriptions
        let prioritiesAndSubscriptionsSorted = List.sortWith sortFstAsc prioritiesAndSubscriptions
        List.map snd prioritiesAndSubscriptionsSorted

    /// Publish a message for the given event.
    let rec publish event publisher message world =
        let optSubList = Map.tryFind event world.Subscriptions
        match optSubList with
        | None -> (true, world)
        | Some subList ->
            let subListSorted = subscriptionSort subList world
            let (_, keepRunning, world) =
                List.foldWhile
                    (fun (message, keepRunning, world) (subscriber, subscription) ->
                        if message.Handled || not keepRunning then None
                        elif not <| isAddressSelected subscriber world then Some (message, keepRunning, world)
                        else
                            let result =
                                match subscription with
                                | ExitSub -> handleEventAsExit message world
                                | SwallowSub -> handleEventAsSwallow message world
                                | ScreenTransitionSub destination -> handleEventAsScreenTransition destination message world
                                | CustomSub fn -> fn event publisher subscriber message world
                            Some result)
                    (message, true, world)
                    subListSorted
            (keepRunning, world)

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
    
    and handleEventAsSwallow message (world : World) =
        (Message.handle message, true, world)

    and handleEventAsExit message (world : World) =
        (Message.handle message, false, world)

    and private getScreenState address world =
        let screen = get world <| worldScreen address
        screen.State

    and private setScreenState address state world =
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
        let world = setScreenState destination IncomingState world
        set (Some destination) world worldOptSelectedScreenAddress

    and private updateTransition1 (transition : Transition) =
        if transition.TransitionTicks = transition.TransitionLifetime then ({ transition with TransitionTicks = 0 }, true)
        else ({ transition with TransitionTicks = transition.TransitionTicks + 1 }, false)

    and internal updateTransition update world =
        let (keepRunning, world) =
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddress
            match optSelectedScreenAddress with
            | None -> (true, world)
            | Some selectedScreenAddress ->
                let screenState = getScreenState selectedScreenAddress world
                match screenState with
                | IncomingState ->
                    // TODO: remove duplication with below
                    let selectedScreen = get world <| worldScreen selectedScreenAddress
                    let incoming = get selectedScreen Screen.screenIncoming
                    let (incoming, finished) = updateTransition1 incoming
                    let selectedScreen = set incoming selectedScreen Screen.screenIncoming
                    let world = set selectedScreen world <| worldScreen selectedScreenAddress
                    let world = setScreenState selectedScreenAddress (if finished then IdlingState else IncomingState) world
                    if not finished then (true, world)
                    else
                        publish
                            (FinishedIncomingEvent @ selectedScreenAddress)
                            selectedScreenAddress
                            { Handled = false; Data = NoData }
                            world
                | OutgoingState ->
                    let selectedScreen = get world <| worldScreen selectedScreenAddress
                    let outgoing = get selectedScreen Screen.screenOutgoing
                    let (outgoing, finished) = updateTransition1 outgoing
                    let selectedScreen = set outgoing selectedScreen Screen.screenOutgoing
                    let world = set selectedScreen world <| worldScreen selectedScreenAddress
                    let world = setScreenState selectedScreenAddress (if finished then IdlingState else OutgoingState) world
                    if not finished then (true, world)
                    else
                        publish
                            (FinishedOutgoingEvent @ selectedScreenAddress)
                            selectedScreenAddress
                            { Handled = false; Data = NoData }
                            world
                | IdlingState -> (true, world)
        if keepRunning then update world
            else (keepRunning, world)

    and private handleSplashScreenIdleTick idlingTime ticks event publisher subscriber message world =
        let world = unsubscribe event subscriber world
        if ticks < idlingTime then
            let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime -<| incI ticks
            let world = subscribe event subscriber subscription world
            (message, true, world)
        else
            let optSelectedScreenAddress = get world worldOptSelectedScreenAddress
            match optSelectedScreenAddress with
            | None ->
                trace "Program Error: Could not handle splash screen tick due to no selected screen."
                (message, false, world)
            | Some selectedScreenAddress ->
                let world = setScreenState selectedScreenAddress OutgoingState world
                (message, true, world)

    and internal handleSplashScreenIdle idlingTime event publisher subscriber message world =
        let subscription = CustomSub <| handleSplashScreenIdleTick idlingTime 0
        let world = subscribe TickEvent subscriber subscription world
        (Message.handle message, true, world)

    and private handleFinishedScreenOutgoing destination event publisher subscriber message world =
        let world = unsubscribe event subscriber world
        let world = transitionScreen destination world
        (Message.handle message, true, world)

    and handleEventAsScreenTransition destination message world =
        let world = transitionScreen destination world
        (Message.handle message, true, world)