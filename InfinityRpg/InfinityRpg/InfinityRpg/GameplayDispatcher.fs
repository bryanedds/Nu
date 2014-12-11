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
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module GameplayDispatcherModule =

    type Screen with
    
        member screen.ContentRandState = screen?ContentRandState : uint64
        static member setContentRandState (value : uint64) (screen : Screen) = screen?ContentRandState <- value
        member screen.OngoingRandState = screen?OngoingRandState : uint64
        static member setOngoingRandState (value : uint64) (screen : Screen) = screen?OngoingRandState <- value
        member screen.ShallLoadGame = screen?ShallLoadGame : bool
        static member setShallLoadGame (value : bool) (screen : Screen) = screen?ShallLoadGame <- value

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        static let getHudAddress gameplayAddress = satoga gameplayAddress HudName
        static let getHudSaveGameAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudSaveGameName
        static let getHudFeelerAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudFeelerName
        static let getHudDetailUpAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailUpName
        static let getHudDetailRightAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailRightName
        static let getHudDetailDownAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailDownName
        static let getHudDetailLeftAddress gameplayAddress = gatoea (getHudAddress gameplayAddress) HudDetailLeftName

        static let getSceneAddress gameplayAddress = satoga gameplayAddress SceneName
        static let getFieldAddress gameplayAddress = gatoea (getSceneAddress gameplayAddress) FieldName 
        static let getPlayerAddress gameplayAddress = gatoea (getSceneAddress gameplayAddress) PlayerName 
        static let getField gameplayAddress world = World.getEntity (getFieldAddress gameplayAddress) world
        static let setField gameplayAddress field world = World.setEntity (getFieldAddress gameplayAddress) field world
        static let getPlayer gameplayAddress world = World.getEntity (getPlayerAddress gameplayAddress) world
        static let setPlayer gameplayAddress player world = World.setEntity (getPlayerAddress gameplayAddress) player world
        static let getEnemies gameplayAddress world = World.getEntities (getSceneAddress gameplayAddress) world |> Seq.filter (Entity.dispatchesAs typeof<EnemyDispatcher>)
        static let setEnemies gameplayAddress enemies world = World.setEntities (getSceneAddress gameplayAddress) enemies world

        static let getParticipants gameplayAddress world =
            (getField gameplayAddress world, getPlayer gameplayAddress world, List.ofSeq <| getEnemies gameplayAddress world)
        
        static let setParticipants gameplayAddress field player enemies world =
            world |> setField gameplayAddress field |> setPlayer gameplayAddress player |> setEnemies gameplayAddress enemies

        static let makeField rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
            let field = Entity.setFieldMapNp fieldMap field
            let field = Entity.setSize (Entity.getQuickSize field world) field
            let field = Entity.setPersistent false field
            (field, rand)

        static let makeEnemies world rand =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 0
            List.fold
                (fun (enemies, rand) positionM ->
                    let enemyPosition = single positionM * TileSize * 2.0f
                    let enemy = World.makeEntity typeof<EnemyDispatcher>.Name None world
                    let enemy = Entity.setDepth CharacterDepth enemy
                    let enemy = Entity.setPosition enemyPosition enemy
                    let enemy = Entity.setCharacterAnimationSheet ZommieImage enemy
                    (Map.add enemy.Name enemy enemies, rand))
                (Map.empty, rand)
                [0 .. enemyCount - 1]

        static let handleNewGame gameplayAddress gameplay world =

            // get common addresses
            let sceneAddress = getSceneAddress gameplayAddress

            // generate non-deterministic random numbers
            let sysrandom = Random ()
            let contentSeedState = uint64 <| sysrandom.Next ()
            let ongoingSeedState = uint64 <| sysrandom.Next ()

            // initialize gameplay screen
            let gameplay = Screen.setContentRandState contentSeedState gameplay
            let gameplay = Screen.setOngoingRandState ongoingSeedState gameplay
            let world = World.setScreen gameplayAddress gameplay world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field
            let (field, rand) = makeField rand world

            // make player
            let player = World.makeEntity typeof<PlayerDispatcher>.Name (Some PlayerName) world
            let player = Entity.setDepth CharacterDepth player

            // make enemies
            let (enemies, _) = makeEnemies world rand
            let world = snd <| World.addEntities sceneAddress enemies world

            // make scene hierarchy
            let entityMap = Map.ofList [(field.Name, field); (player.Name, player)]
            let scene = World.makeGroup typeof<GroupDispatcher>.Name (Some SceneName) world
            let sceneHierarchy = (scene, entityMap)

            // add scene hierarchy to world
            snd <| World.addGroup sceneAddress sceneHierarchy world

        static let handleLoadGame gameplayAddress gameplay world =

            // get common addresses
            let sceneAddress = getSceneAddress gameplayAddress

            // get and initialize gameplay screen from read
            let gameplayHierarchy = World.readScreenHierarchyFromFile SaveFilePath world
            let (gameplayFromRead, groupHierarchies) = gameplayHierarchy
            let gameplay = Screen.setContentRandState gameplayFromRead.ContentRandState gameplay
            let gameplay = Screen.setOngoingRandState gameplayFromRead.OngoingRandState gameplay
            let world = World.setScreen gameplayAddress gameplay world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field
            let (field, _) = makeField rand world

            // find scene hierarchy and add field to it
            let sceneHierarchy = Map.find SceneName groupHierarchies
            let (scene, entities) = sceneHierarchy
            let entityMap = Map.add field.Name field entities
            let sceneHierarchy = (scene, entityMap)

            // add scene hierarchy to world
            snd <| World.addGroup sceneAddress sceneHierarchy world

        static let handleSelectGameplay event world =
            let (gameplayAddress, gameplay : Screen) = World.unwrapAS event world
            let world =
                // NOTE: doing a File.Exists then loading the file is dangerous since the file can
                // always be deleted / moved between the two operations!
                if gameplay.ShallLoadGame && File.Exists SaveFilePath
                then handleLoadGame gameplayAddress gameplay world
                else handleNewGame gameplayAddress gameplay world
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
            let world = snd <| World.removeGroup sceneAddress scene world
            (Cascade, world)

        static let anyTurnsInProgress2 (player : Entity) enemies =
            player.ActivityState <> NoActivity ||
            List.exists (fun (enemy : Entity) -> enemy.DesiredTurn <> NoTurn || enemy.ActivityState <> NoActivity) enemies

        static let anyTurnsInProgress gameplayAddress world =
            let (_, player, enemies) = getParticipants gameplayAddress world
            anyTurnsInProgress2 player enemies

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) rand =
            match enemy.ControlType with
            | Player ->
                debug <| "Invalid ControlType '" + acstring enemy.ControlType + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                if Math.arePositionsAdjacent enemy.Position player.Position then
                    let enemyTurn = makeAttackTurn player.Position
                    (enemyTurn, rand)
                else
                    let (randResult, rand) = Rand.nextIntUnder 4 rand
                    let direction = Direction.fromInt randResult
                    let enemyTurn = CharacterActivity.determineTurnFromDirection direction occupationMap enemy [player]
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

        static let advanceEnemyActivities enemies =
            List.map CharacterActivity.advanceActivity enemies

        static let determinePlayerTurnFromTouch touchPosition gameplayAddress world =
            let (field, player, enemies) = getParticipants gameplayAddress world
            if not <| anyTurnsInProgress2 player enemies then
                let touchPositionW = Camera.mouseToWorld Relative touchPosition world.Camera
                let playerPositionM = Vector2i (Vector2.Divide (player.Position, TileSize))
                let occupationMapWithAdjacentEnemies = OccupationMap.makeFromFieldTilesAndAdjacentCharacters playerPositionM field.FieldMapNp.FieldTiles enemies
                match CharacterActivity.determineTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies player enemies with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let firstNavigationNode = List.head <| Option.get ^| navigationDescriptor.OptNavigationPath
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                    if Map.find firstNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction gameplayAddress world =
            let (field, player, enemies) = getParticipants gameplayAddress world
            if not <| anyTurnsInProgress2 player enemies then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                CharacterActivity.determineTurnFromDirection direction occupationMapWithEnemies player enemies
            else NoTurn

        static let determinePlayerTurnFromInput playerInput gameplayAddress world =
            match playerInput with
            | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition gameplayAddress world
            | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction gameplayAddress world
            | NoInput -> NoTurn

        static let determinePlayerTurnFromNavigationState navigationDescriptor gameplayAddress world =
            let (field, player, enemies) = getParticipants gameplayAddress world
            let walkDescriptor = navigationDescriptor.WalkDescriptor
            let playerPositionM = Vector2i (Vector2.Divide (player.Position, TileSize))
            if playerPositionM = walkDescriptor.WalkOriginM then
                let walkDestinationM = walkDescriptor.WalkOriginM + Direction.toVector2i walkDescriptor.WalkDirection
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters field.FieldMapNp.FieldTiles enemies
                if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                else NavigationTurn navigationDescriptor
            else NavigationTurn navigationDescriptor

        static let determinePlayerTurn playerInput gameplayAddress world =
            match CharacterActivity.determineTurnFromActivityState <| getPlayer gameplayAddress world with
            | ActionTurn _ as actionTurn -> actionTurn
            | NavigationTurn navigationDescriptor -> determinePlayerTurnFromNavigationState navigationDescriptor gameplayAddress world
            | CancelTurn -> CancelTurn
            | NoTurn -> determinePlayerTurnFromInput playerInput gameplayAddress world

        static let determineEnemyActionActivities enemies =
            List.foldBack
                (fun (enemy : Entity) precedingEnemyActivities ->
                    let enemyActivity =
                        let noPrecedingEnemyActionActivity =
                            List.notExists ActivityState.isActing precedingEnemyActivities
                        let noCurrentEnemyActionActivity =
                            List.notExists (fun (enemy : Entity) -> ActivityState.isActing enemy.ActivityState) enemies
                        if  noPrecedingEnemyActionActivity &&
                            noCurrentEnemyActionActivity then
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
                    let noCurrentEnemyActionActivity =
                        List.notExists
                            (fun (enemy : Entity) -> ActivityState.isActing enemy.ActivityState)
                            enemies
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

        static let tryUpdateEnemyActivity newActivity enemy =
            if newActivity <> NoActivity then
                let enemy = Entity.setActivityState newActivity enemy
                Entity.setDesiredTurn NoTurn enemy
            else enemy

        static let updateEnemyNaviagationActivities enemies =
            let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies
            let anyEnemyNavigationActivity = List.exists ActivityState.isNavigating newEnemyNavigationActivities
            if anyEnemyNavigationActivity then List.map2 tryUpdateEnemyActivity newEnemyNavigationActivities enemies
            else enemies

        static let updateEnemyActivities enemies =
            let newEnemyActionActivities = determineEnemyActionActivities enemies 
            let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies
            let anyEnemyActionActivity = List.exists ActivityState.isActing newEnemyActionActivities
            let newEnemyActivities = if anyEnemyActionActivity then newEnemyActionActivities else newEnemyNavigationActivities
            List.map2 tryUpdateEnemyActivity newEnemyActivities enemies

        static let advanceCharacters gameplayAddress playerTurn world =

            // construct gameplay context
            let gameplay = World.getScreen gameplayAddress world
            let rand = Rand.make gameplay.OngoingRandState

            // construct local context
            let (field, player, enemies) = getParticipants gameplayAddress world
            let occupationMap = OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn field.FieldMapNp.FieldTiles enemies playerTurn

            // determine player activity state
            let optNewPlayerActivity =
                match playerTurn with
                | ActionTurn actionDescriptor -> Some <| Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Some <| Navigation navigationDescriptor
                | CancelTurn -> Some NoActivity
                | NoTurn -> None

            // update player activity
            let player =
                match optNewPlayerActivity with
                | Some newPlayerActivity -> Entity.setActivityState newPlayerActivity player
                | None -> player

            // determine (and set) enemy desired turns when applicable
            let (enemies, rand) =
                match optNewPlayerActivity with
                | Some newPlayerActivity ->
                    if newPlayerActivity <> NoActivity then
                        let (enemyDesiredTurns, rand) = determineDesiredEnemyTurns occupationMap player enemies rand
                        let enemies = List.map2 Entity.setDesiredTurn enemyDesiredTurns enemies
                        (enemies, rand)
                    else (enemies, rand)
                | None -> (enemies, rand)

            // update enemy activities in accordance with the player's current activity
            let enemies =
                match player.ActivityState with
                | Action _ -> enemies
                | Navigation _ -> updateEnemyNaviagationActivities enemies
                | NoActivity -> updateEnemyActivities enemies

            // advance player activity
            let player = CharacterActivity.advanceActivity player

            // advance enemies activities
            let enemies = advanceEnemyActivities enemies
            
            // update local context
            let world = setParticipants gameplayAddress field player enemies world

            // update gameplay context
            let gameplay = Screen.setOngoingRandState (Rand.getState rand) gameplay
            World.setScreen gameplayAddress gameplay world

        static let tryAdvanceTurn playerInput gameplayAddress world =
            if not <| anyTurnsInProgress gameplayAddress world then
                let playerTurn = determinePlayerTurn playerInput gameplayAddress world
                if playerTurn <> NoTurn then
                    let world = advanceCharacters gameplayAddress playerTurn world
                    let advancer = desync {
                        let hudSaveGameAddress = getHudSaveGameAddress gameplayAddress
                        do! updateEntity hudSaveGameAddress <| Entity.setEnabled false
                        do! next ()
                        do! during (anyTurnsInProgress gameplayAddress) (desync {
                            do! next ()
                            do! updateBy
                                    (determinePlayerTurn NoInput gameplayAddress)
                                    (advanceCharacters gameplayAddress) })
                        do! updateEntity HudSaveGameAddress <| Entity.setEnabled true }
                    let tickObs = observe TickEventAddress gameplayAddress |> until (DeselectEventAddress ->>- gameplayAddress)
                    snd <| runDesyncAssumingCascade tickObs advancer world
                else world
            else world

        static let handleTouchFeeler event world =
            let gameplayAddress = World.unwrapA event world
            let playerInput = TouchInput event.Data
            let world = tryAdvanceTurn playerInput gameplayAddress world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let gameplayAddress = World.unwrapA event world
            let playerInput = DetailInput direction
            let world = tryAdvanceTurn playerInput gameplayAddress world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? ShallLoadGame false]

        override dispatcher.Register (gameplayAddress, gameplay, world) =
            let world = observe (TouchEventAddress ->>- getHudFeelerAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailUpAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail North) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailRightAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail East) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailDownAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail South) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailLeftAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail West) world |> snd
            let world = World.subscribe4 handleSelectGameplay (SelectEventAddress ->>- gameplayAddress) gameplayAddress world
            let world = World.subscribe4 handleClickSaveGame (ClickEventAddress ->>- getHudSaveGameAddress gameplayAddress) gameplayAddress world
            let world = World.subscribe4 handleDeselectGameplay (DeselectEventAddress ->>- gameplayAddress) gameplayAddress world
            (gameplay, world)