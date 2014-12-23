namespace InfinityRpg
open System
open System.IO
open SDL2
open OpenTK
open Prime
open Prime.Desync
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open Nu.Desync
open AStar
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module GameplayDispatcherModule =

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type Screen with
    
        member screen.ContentRandState = screen?ContentRandState : uint64
        static member setContentRandState (value : uint64) (screen : Screen) = screen?ContentRandState <- value
        member screen.OngoingRandState = screen?OngoingRandState : uint64
        static member setOngoingRandState (value : uint64) (screen : Screen) = screen?OngoingRandState <- value
        member screen.ShallLoadGame = screen?ShallLoadGame : bool
        static member setShallLoadGame (value : bool) (screen : Screen) = screen?ShallLoadGame <- value

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        // hud addresses
        static let getHudAddress gameplayAddress = satoga gameplayAddress HudName
        static let getHudHaltAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudHaltName
        static let getHudSaveGameAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudSaveGameName
        static let getHudFeelerAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudFeelerName
        static let getHudDetailUpAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailUpName
        static let getHudDetailRightAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailRightName
        static let getHudDetailDownAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailDownName
        static let getHudDetailLeftAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailLeftName
        
        // hud entities
        static let getHudHalt gameplayAddress world = World.getEntity (getHudHaltAddress gameplayAddress) world
        static let setHudHalt field gameplayAddress world = World.setEntity field (getHudHaltAddress gameplayAddress) world

        // scene addresses
        static let getSceneAddress gameplayAddress = satoga gameplayAddress SceneName
        static let getFieldAddress gameplayAddress = gatoea (getSceneAddress gameplayAddress) FieldName
        static let getPlayerAddress gameplayAddress = gatoea (getSceneAddress gameplayAddress) PlayerName
        static let getCharacterAddress (character : Entity) gameplayAddress = gatoea (getSceneAddress gameplayAddress) character.Name
        static let getCharacterAddresses characters gameplayAddress = List.map (fun character -> getCharacterAddress character gameplayAddress) characters

        // scene entities
        static let getField gameplayAddress world = World.getEntity (getFieldAddress gameplayAddress) world
        static let getOptCharacterAtPosition position gameplayAddress world =
            let characters = world |> World.getEntities (getSceneAddress gameplayAddress) |> Seq.filter (Entity.dispatchesAs typeof<CharacterDispatcher>)
            Seq.tryFind (fun (character : Entity) -> character.Position = position) characters
        //static let getCharacterAtPosition position gameplayAddress world = Option.get <| getOptCharacterAtPosition position gameplayAddress world
        static let getOptCharacterInDirection position direction gameplayAddress world = getOptCharacterAtPosition (position + dtovf direction) gameplayAddress world
        static let getCharacterInDirection position direction gameplayAddress world = Option.get <| getOptCharacterInDirection position direction gameplayAddress world
        static let getPlayer gameplayAddress world = World.getEntity (getPlayerAddress gameplayAddress) world
        static let getEnemies gameplayAddress world = World.getEntities (getSceneAddress gameplayAddress) world |> Seq.filter (Entity.dispatchesAs typeof<EnemyDispatcher>) |> List.ofSeq
        static let getParticipants gameplayAddress world = (getField gameplayAddress world, getPlayer gameplayAddress world, getEnemies gameplayAddress world)

        static let makeField rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
            let field = Entity.setFieldMapNp fieldMap field
            let field = Entity.setSize (Entity.getQuickSize field world) field
            let field = Entity.setPersistent false field
            (field, rand)

        static let makeEnemies rand world =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 1
            List.fold
                (fun (enemies, rand) i ->
                    let enemyPosition = single i * TileSize * 2.0f
                    let enemy = World.makeEntity typeof<EnemyDispatcher>.Name None world
                    let enemy = Entity.setDepth CharacterDepth enemy
                    let enemy = Entity.setPosition enemyPosition enemy
                    let enemy = Entity.setCharacterAnimationSheet GoopyImage enemy
                    (Map.add enemy.Name enemy enemies, rand))
                (Map.empty, rand)
                [0 .. enemyCount - 1]

        static let walk positive current destination =
            let walkSpeed = if positive then CharacterWalkSpeed else -CharacterWalkSpeed
            let next = current + walkSpeed
            let delta = if positive then destination - next else next - destination
            if delta < CharacterWalkSpeed then (destination, WalkFinished) else (next, WalkContinuing)

        static let getCharacterAnimationStateByActionBegin tickTime characterPosition characterAnimationState actionDescriptor =
            let currentDirection = characterAnimationState.Direction
            let direction = ActionDescriptor.getActionDirection characterPosition currentDirection actionDescriptor
            { characterAnimationState with
                Direction = direction
                AnimationType = CharacterAnimationActing
                StartTime = tickTime }

        static let getCharacterAnimationStateByActionEnd tickTime characterAnimationState =
            { characterAnimationState with
                AnimationType = CharacterAnimationFacing
                StartTime = tickTime }

        static let tryGetNavigationPath touchPosition occupationMap (character : Entity) =
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find (vftovm touchPosition) nodes
            let currentNode = Map.find (vftovm character.Position) nodes
            let optNavigationPath =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun n -> 0.0f))
            match optNavigationPath with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let cancelNavigation (character : Entity) =
            let characterActivity = character.ActivityState
            let characterActivity =
                match characterActivity with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with OptNavigationPath = None }
            Entity.setActivityState characterActivity character

        static let anyTurnsInProgress2 (player : Entity) enemies =
            player.ActivityState <> NoActivity ||
            List.exists (fun (enemy : Entity) -> enemy.DesiredTurn <> NoTurn || enemy.ActivityState <> NoActivity) enemies

        static let anyTurnsInProgress gameplayAddress world =
            let player = getPlayer gameplayAddress world
            let enemies = getEnemies gameplayAddress world
            anyTurnsInProgress2 player enemies

        static let updateCharacterByNavigation navigationDescriptor characterAddress world =
            let character = World.getEntity characterAddress world
            let (character, walkState) =
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                let (newPosition, walkState) =
                    let walkOrigin = vmtovf walkDescriptor.WalkOriginM
                    let walkVector = dtovf walkDescriptor.WalkDirection
                    let walkDestination = walkOrigin + walkVector
                    match walkDescriptor.WalkDirection with
                    | Upward -> let (newY, arrival) = walk true character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
                    | Rightward -> let (newX, arrival) = walk true character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
                    | Downward -> let (newY, arrival) = walk false character.Position.Y walkDestination.Y in (Vector2 (character.Position.X, newY), arrival)
                    | Leftward -> let (newX, arrival) = walk false character.Position.X walkDestination.X in (Vector2 (newX, character.Position.Y), arrival)
                let characterAnimationState = { character.CharacterAnimationState with Direction = walkDescriptor.WalkDirection }
                let character = Entity.setPosition newPosition character
                let character = Entity.setCharacterAnimationState characterAnimationState character
                (character, walkState)
            let character =
                match walkState with
                | WalkFinished ->
                    match navigationDescriptor.OptNavigationPath with
                    | Some [] -> failwith "NavigationPath should never be empty here."
                    | Some (_ :: []) -> Entity.setActivityState NoActivity character
                    | Some (currentNode :: navigationPath) ->
                        let walkDescriptor =
                            { WalkDirection = vmtod <| (List.head navigationPath).PositionM - currentNode.PositionM
                              WalkOriginM = vftovm character.Position }
                        let navigationDescriptor = { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                        Entity.setActivityState (Navigation navigationDescriptor) character
                    | None -> Entity.setActivityState NoActivity character
                | WalkContinuing -> character
            World.setEntity character characterAddress world

        static let updateCharacterByAction actionDescriptor characterAddress world =
            let tickTime = world.State.TickTime
            let character = World.getEntity characterAddress world
            let character =
                if actionDescriptor.ActionTicks = 0L then
                    character |>
                        Entity.setCharacterAnimationState (getCharacterAnimationStateByActionBegin tickTime character.Position character.CharacterAnimationState actionDescriptor) |>
                        Entity.setActivityState (Action <| ActionDescriptor.incActionTicks actionDescriptor)
                elif actionDescriptor.ActionTicks > 0L && actionDescriptor.ActionTicks < ActionTicksMax then
                    character |>
                        Entity.setActivityState (Action <| ActionDescriptor.incActionTicks actionDescriptor)
                else
                    character |>
                        Entity.setActivityState NoActivity |>
                        Entity.setCharacterAnimationState (getCharacterAnimationStateByActionEnd tickTime character.CharacterAnimationState)
            World.setEntity character characterAddress world

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents =
            match character.ActivityState with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM (vftovm character.Position) occupationMap
                if Set.contains direction openDirections then
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = vftovm character.Position }
                    NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
                else
                    let targetPosition = character.Position + dtovf direction
                    if List.exists (fun (opponent : Entity) -> opponent.Position = targetPosition) opponents
                    then makeAttackTurn <| vftovm targetPosition
                    else NoTurn

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents =
            if character.ActivityState = NoActivity then
                match tryGetNavigationPath touchPosition occupationMap character with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | _ ->
                        let characterPositionM = vftovm character.Position
                        let walkDirection = vmtod <| (List.head navigationPath).PositionM - characterPositionM
                        let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                        NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                | None ->
                    let targetPosition = touchPosition |> vftovm |> vmtovf
                    if Math.arePositionsAdjacent targetPosition character.Position then
                        if List.exists (fun (opponent : Entity) -> opponent.Position = targetPosition) opponents
                        then makeAttackTurn <| vftovm targetPosition
                        else NoTurn
                    else NoTurn
            else NoTurn

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) rand =
            match enemy.ControlType with
            | Player ->
                debug <| "Invalid ControlType '" + acstring enemy.ControlType + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                let nextPlayerPosition =
                    match player.ActivityState with
                    | Action _ -> player.Position
                    | Navigation navigationDescriptor -> NavigationDescriptor.nextPosition navigationDescriptor
                    | NoActivity -> player.Position
                if Math.arePositionsAdjacent enemy.Position nextPlayerPosition then
                    let enemyTurn = makeAttackTurn <| vftovm nextPlayerPosition
                    (enemyTurn, rand)
                else
                    let (randResult, rand) = Rand.nextIntUnder 4 rand
                    let direction = Direction.fromInt randResult
                    let enemyTurn = determineCharacterTurnFromDirection direction occupationMap enemy [player]
                    (enemyTurn, rand)
            | Uncontrolled -> (NoTurn, rand)

        static let determineDesiredEnemyTurns occupationMap player enemies rand =
            let (_, enemyTurns, rand) =
                List.foldBack
                    (fun (enemy : Entity) (occupationMap, enemyTurns, rand) ->
                        let (enemyTurn, rand) = determineDesiredEnemyTurn occupationMap player enemy rand
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn enemy occupationMap
                        (occupationMap, enemyTurn :: enemyTurns, rand))
                    enemies
                    (occupationMap, [], rand)
            (enemyTurns, rand)

        static let determinePlayerTurnFromTouch touchPosition gameplayAddress world =
            let (field, player, enemies) = getParticipants gameplayAddress world
            if not <| anyTurnsInProgress2 player enemies then
                let touchPositionW = Camera.mouseToWorld Relative touchPosition world.Camera
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm player.Position) field.FieldMapNp.FieldTiles enemies
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies player enemies with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.OptNavigationPath |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction gameplayAddress world =
            let (field, player, enemies) = getParticipants gameplayAddress world
            if not <| anyTurnsInProgress2 player enemies then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                determineCharacterTurnFromDirection direction occupationMapWithEnemies player enemies
            else NoTurn

        static let determinePlayerTurnFromInput playerInput gameplayAddress world =
            match playerInput with
            | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition gameplayAddress world
            | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction gameplayAddress world
            | NoInput -> NoTurn

        static let determinePlayerTurn gameplayAddress world =
            let player = getPlayer gameplayAddress world
            match player.ActivityState with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if player.Position = vmtovf walkDescriptor.WalkOriginM then
                    let field = getField gameplayAddress world
                    let enemies = getEnemies gameplayAddress world
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyActionActivities enemies =
            List.foldBack
                (fun (enemy : Entity) precedingEnemyActivities ->
                    let enemyActivity =
                        let noPrecedingEnemyActionActivity = List.notExists ActivityState.isActing precedingEnemyActivities
                        let noCurrentEnemyActionActivity = List.notExists (fun (enemy : Entity) -> ActivityState.isActing enemy.ActivityState) enemies
                        if noPrecedingEnemyActionActivity && noCurrentEnemyActionActivity then
                            match enemy.DesiredTurn with
                            | ActionTurn actionDescriptor -> Action actionDescriptor
                            | NavigationTurn _ -> NoActivity
                            | CancelTurn -> NoActivity
                            | NoTurn -> NoActivity
                        else NoActivity
                    enemyActivity :: precedingEnemyActivities)
                enemies
                []

        static let determineEnemyNavigationActivities enemies =
            List.foldBack
                (fun (enemy : Entity) enemyActivities ->
                    let noCurrentEnemyActionActivity = List.notExists (fun (enemy : Entity) -> ActivityState.isActing enemy.ActivityState) enemies
                    let enemyActivity =
                        if noCurrentEnemyActionActivity then
                            match enemy.DesiredTurn with
                            | ActionTurn _ -> NoActivity
                            | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                            | CancelTurn -> NoActivity
                            | NoTurn -> NoActivity
                        else NoActivity
                    enemyActivity :: enemyActivities)
                enemies
                []

        static let runCharacterReaction actionDescriptor initiatorAddress gameplayAddress world =
            // TODO: implement animations
            let initiator = World.getEntity initiatorAddress world
            if actionDescriptor.ActionTicks = ActionTicksMax then
                let reactor = getCharacterInDirection initiator.Position initiator.CharacterAnimationState.Direction gameplayAddress world
                let reactorAddress = getCharacterAddress reactor gameplayAddress
                let reactorDamage = initiator.PowerBuff * 5.0f - reactor.ShieldBuff |> int
                let reactorHitPoints = reactor.HitPoints - reactorDamage
                let reactor = Entity.setHitPoints reactorHitPoints reactor
                let world = World.setEntity reactor reactorAddress world
                if reactor.HitPoints <= 0 then
                    if reactor.Name = PlayerName
                    then World.transitionScreen TitleAddress world
                    else snd <| World.removeEntity reactor reactorAddress world
                else world
            else world

        static let runCharacterNavigation newNavigationDescriptor characterAddress gameplayAddress world =
            let desync = desync {
                do! updateEntity (Entity.setActivityState <| Navigation newNavigationDescriptor) characterAddress
                do! during
                        (fun world ->
                            match World.getEntityBy (fun entity -> entity.ActivityState) characterAddress world with
                            | Navigation nd -> newNavigationDescriptor.WalkDescriptor.WalkOriginM = nd.WalkDescriptor.WalkOriginM
                            | Action _ | NoActivity -> false) <|
                        desync {
                            do! update <| fun world ->
                                let navigationDescriptor =
                                    match World.getEntityBy (fun entity -> entity.ActivityState) characterAddress world with
                                    | Navigation navigationDescriptor -> navigationDescriptor
                                    | _ -> failwith "Unexpected match failure in InfinityRpg.GameplayDispatcherModule.runCharacterNavigation."
                                updateCharacterByNavigation navigationDescriptor characterAddress world
                            do! next () }}
            let obs = observe TickEventAddress characterAddress |> until (DeselectEventAddress ->>- gameplayAddress)
            snd <| runDesyncAssumingCascade desync obs world

        static let runCharacterAction newActionDescriptor characterAddress gameplayAddress world =
            // NOTE: currently just implements attack
            let desync = desync {
                do! updateEntity (Entity.setActivityState <| Action newActionDescriptor) characterAddress
                do! during
                        (fun world ->
                            let activityState = World.getEntityBy (fun entity -> entity.ActivityState) characterAddress world 
                            ActivityState.isActing activityState) <|
                        desync {
                            do! update <| fun world ->
                                let actionDescriptor =
                                    match World.getEntityBy (fun entity -> entity.ActivityState) characterAddress world  with
                                    | Action actionDescriptor -> actionDescriptor
                                    | _ -> failwithumf ()
                                let world = updateCharacterByAction actionDescriptor characterAddress world
                                runCharacterReaction actionDescriptor characterAddress gameplayAddress world
                            do! next () }}
            let obs = observe TickEventAddress characterAddress |> until (DeselectEventAddress ->>- gameplayAddress)
            snd <| runDesyncAssumingCascade desync obs world

        static let runCharacterNoActivity characterAddress world =
            World.updateEntity (Entity.setActivityState NoActivity) characterAddress world

        static let runCharacterActivity newActivity characterAddress gameplayAddress world =
            match newActivity with
            | Action newActionDescriptor -> runCharacterAction newActionDescriptor characterAddress gameplayAddress world
            | Navigation newNavigationDescriptor -> runCharacterNavigation newNavigationDescriptor characterAddress gameplayAddress world
            | NoActivity -> runCharacterNoActivity characterAddress world

        static let tryRunEnemyActivity gameplayAddress world newActivity enemyAddress =
            if newActivity <> NoActivity then
                let enemy = World.getEntity enemyAddress world
                let enemy = Entity.setDesiredTurn NoTurn enemy
                let world = World.setEntity enemy enemyAddress world
                runCharacterActivity newActivity enemyAddress gameplayAddress world
            else world

        static let runEnemyNavigationActivities enemyNavigationActivities enemyAddresses gameplayAddress world =
            if List.exists ActivityState.isNavigating enemyNavigationActivities
            then List.fold2 (tryRunEnemyActivity gameplayAddress) world enemyNavigationActivities enemyAddresses
            else world

        static let runEnemyActivities enemyActionActivities enemyNavigationActivities enemyAddresses gameplayAddress world =
            let anyEnemyActionActivity = List.exists ActivityState.isActing enemyActionActivities
            let newEnemyActivities = if anyEnemyActionActivity then enemyActionActivities else enemyNavigationActivities
            List.fold2 (tryRunEnemyActivity gameplayAddress) world newEnemyActivities enemyAddresses

        static let runPlayerTurn playerTurn gameplayAddress world =

            // construct occupation map
            let occupationMap =
                let field = getField gameplayAddress world
                let enemies = getEnemies gameplayAddress world
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn field.FieldMapNp.FieldTiles enemies playerTurn

            // determine player activity
            let optNewPlayerActivity =
                match playerTurn with
                | ActionTurn actionDescriptor -> Some <| Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Some <| Navigation navigationDescriptor
                | CancelTurn -> Some NoActivity
                | NoTurn -> None

            // run player activity
            let world =
                match optNewPlayerActivity with
                | Some newPlayerActivity ->
                    let playerAddress = getPlayerAddress gameplayAddress
                    runCharacterActivity newPlayerActivity playerAddress gameplayAddress world
                | None -> world

            // determine (and set) enemy desired turns if applicable
            let world =
                match optNewPlayerActivity with
                | Some (Action _)
                | Some (Navigation _) ->
                    let gameplay = World.getScreen gameplayAddress world
                    let rand = Rand.make gameplay.OngoingRandState
                    let player = getPlayer gameplayAddress world
                    let enemies = getEnemies gameplayAddress world
                    let (enemyDesiredTurns, rand) = determineDesiredEnemyTurns occupationMap player enemies rand
                    let enemies = List.map2 Entity.setDesiredTurn enemyDesiredTurns enemies
                    let world = World.setEntities (getSceneAddress gameplayAddress) enemies world
                    let gameplay = Screen.setOngoingRandState (Rand.getState rand) gameplay
                    World.setScreen gameplay gameplayAddress world
                | Some NoActivity
                | None -> world

            // run enemy activities in accordance with the player's current activity
            let world =
                let player = getPlayer gameplayAddress world
                let enemies = getEnemies gameplayAddress world
                match player.ActivityState with
                | Action _ -> world
                | Navigation _ ->
                    let enemyAddresses = getCharacterAddresses enemies gameplayAddress
                    let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies
                    runEnemyNavigationActivities newEnemyNavigationActivities enemyAddresses gameplayAddress world
                | NoActivity ->
                    let enemyAddresses = getCharacterAddresses enemies gameplayAddress
                    let newEnemyActionActivities = determineEnemyActionActivities enemies
                    let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies
                    runEnemyActivities newEnemyActionActivities newEnemyNavigationActivities enemyAddresses gameplayAddress world

            // teh world
            world

        static let tryRunPlayerTurn playerInput gameplayAddress world =
            if not <| anyTurnsInProgress gameplayAddress world then
                let hudSaveGameAddress = getHudSaveGameAddress gameplayAddress
                let hudHaltAddress = getHudHaltAddress gameplayAddress
                let playerAddress = getPlayerAddress gameplayAddress
                let desync = desync {
                    do! updateEntity (Entity.setEnabled false) hudSaveGameAddress
                    do! loop 0 inc (fun i world -> i = 0 || anyTurnsInProgress gameplayAddress world) <| fun i -> desync {
                        let! event = nextE ()
                        do! match event.Data with
                            | Left _ -> updateEntity cancelNavigation playerAddress
                            | Right _ ->
                                if i = 0
                                then updateBy
                                        (determinePlayerTurnFromInput playerInput gameplayAddress)
                                        (fun playerTurn -> runPlayerTurn playerTurn gameplayAddress)
                                else updateBy
                                        (determinePlayerTurn gameplayAddress)
                                        (fun playerTurn -> runPlayerTurn playerTurn gameplayAddress) }
                    do! updateEntity (Entity.setEnabled true) hudSaveGameAddress }
                let obs =
                    observe (ClickEventAddress ->>- hudHaltAddress) gameplayAddress |>
                    sum TickEventAddress |>
                    until (DeselectEventAddress ->>- gameplayAddress)
                snd <| runDesyncAssumingCascade desync obs world
            else world

        static let handlePlayerChange event world =
            let gameplayAddress = event.SubscriberAddress
            let player = getPlayer gameplayAddress world
            let hudHalt = getHudHalt gameplayAddress world
            let isPlayerNavigatingPath = ActivityState.isNavigatingPath player.ActivityState
            let hudHalt = Entity.setEnabled isPlayerNavigatingPath hudHalt
            let world = setHudHalt hudHalt gameplayAddress world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let playerInput = TouchInput event.Data
            let world = tryRunPlayerTurn playerInput event.SubscriberAddress world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let playerInput = DetailInput direction
            let world = tryRunPlayerTurn playerInput event.SubscriberAddress world
            (Cascade, world)

        static let handleNewGame gameplay gameplayAddress world =

            // get common addresses
            let sceneAddress = getSceneAddress gameplayAddress

            // generate non-deterministic random numbers
            let sysrandom = Random ()
            let contentSeedState = uint64 <| sysrandom.Next ()
            let ongoingSeedState = uint64 <| sysrandom.Next ()

            // initialize gameplay screen
            let gameplay = Screen.setContentRandState contentSeedState gameplay
            let gameplay = Screen.setOngoingRandState ongoingSeedState gameplay
            let world = World.setScreen gameplay gameplayAddress world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field
            let (field, rand) = makeField rand world

            // make player
            let player = World.makeEntity typeof<PlayerDispatcher>.Name (Some PlayerName) world
            let player = Entity.setDepth CharacterDepth player
            let player = Entity.setPublishChanges true player

            // make enemies
            let (enemies, _) = makeEnemies rand world
            let world = snd <| World.addEntities enemies sceneAddress world

            // make scene hierarchy
            let entityMap = Map.ofList [(field.Name, field); (player.Name, player)]
            let scene = World.makeGroup typeof<GroupDispatcher>.Name (Some SceneName) world
            let sceneHierarchy = (scene, entityMap)

            // add scene hierarchy to world
            snd <| World.addGroup sceneHierarchy sceneAddress world

        static let handleLoadGame gameplay gameplayAddress world =

            // get common addresses
            let sceneAddress = getSceneAddress gameplayAddress

            // get and initialize gameplay screen from read
            let gameplayHierarchy = World.readScreenHierarchyFromFile SaveFilePath world
            let (gameplayFromRead, groupHierarchies) = gameplayHierarchy
            let gameplay = Screen.setContentRandState gameplayFromRead.ContentRandState gameplay
            let gameplay = Screen.setOngoingRandState gameplayFromRead.OngoingRandState gameplay
            let world = World.setScreen gameplay gameplayAddress world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field frome rand (field is not serialized, but generated deterministically with ContentRandState)
            let (field, _) = makeField rand world

            // find scene hierarchy and add field to it
            let sceneHierarchy = Map.find SceneName groupHierarchies
            let (scene, entities) = sceneHierarchy
            let entityMap = Map.add field.Name field entities
            let sceneHierarchy = (scene, entityMap)

            // add scene hierarchy to world
            snd <| World.addGroup sceneHierarchy sceneAddress world

        static let handleSelectGameplay event world =
            let (gameplay : Screen, gameplayAddress) = World.unwrapSA event world
            let world =
                // NOTE: doing a File.Exists then loading the file is dangerous since the file can
                // always be deleted / moved between the two operations!
                if gameplay.ShallLoadGame && File.Exists SaveFilePath
                then handleLoadGame gameplay gameplayAddress world
                else handleNewGame gameplay gameplayAddress world
            (Cascade, world)
            
        static let handleClickSaveGame event world =
            let gameplayAddress = World.unwrapA event world
            let gameplayHierarchy = World.getScreenHierarchy gameplayAddress world
            World.writeScreenHierarchyToFile SaveFilePath gameplayHierarchy world
            (Cascade, world)

        static let handleDeselectGameplay event world =
            let gameplayAddress = World.unwrapA event world
            let sceneAddress = getSceneAddress gameplayAddress
            let scene = World.getGroup sceneAddress world
            let world = snd <| World.removeGroup scene sceneAddress world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? ShallLoadGame false]

        override dispatcher.Register (gameplay, gameplayAddress, world) =
            let world =
                world |>
                (observe (EntityChangeEventAddress ->>- getPlayerAddress gameplayAddress) gameplayAddress |> subscribe handlePlayerChange) |>
                (observe (TouchEventAddress ->>- getHudFeelerAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor handleTouchFeeler) |>
                (observe (DownEventAddress ->>- getHudDetailUpAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail Upward)) |>
                (observe (DownEventAddress ->>- getHudDetailRightAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail Rightward)) |>
                (observe (DownEventAddress ->>- getHudDetailDownAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail Downward)) |>
                (observe (DownEventAddress ->>- getHudDetailLeftAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail Leftward)) |>
                (World.subscribe4 (SelectEventAddress ->>- gameplayAddress) gameplayAddress handleSelectGameplay) |>
                (World.subscribe4 (ClickEventAddress ->>- getHudSaveGameAddress gameplayAddress) gameplayAddress handleClickSaveGame) |>
                (World.subscribe4 (DeselectEventAddress ->>- gameplayAddress) gameplayAddress handleDeselectGameplay)
            (gameplay, world)