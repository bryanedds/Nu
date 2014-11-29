namespace InfinityRpg
open System
open System.IO
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observer
open InfinityRpg
open InfinityRpg.Constants

[<AutoOpen>]
module GameplayDispatcherModule =

    type Screen with
    
        member screen.ContentRandState = screen?ContentRandState : uint64
        static member setContentRandState (value : uint64) (screen : Screen) = screen?ContentRandState <- value
        member screen.OngoingRandState = screen?OngoingRandState : uint64
        static member setOngoingRandState (value : uint64) (screen : Screen) = screen?OngoingRandState <- value
        member screen.PlayerTurnInput = screen?PlayerTurnInput : PlayerTurnInput
        static member setPlayerTurnInput (value : PlayerTurnInput) (screen : Screen) = screen?PlayerTurnInput <- value

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
            (getField gameplayAddress world, List.ofSeq <| getEnemies gameplayAddress world, getPlayer gameplayAddress world)
        
        static let setParticipants gameplayAddress field enemies player world =
            world |> setField gameplayAddress field |> setEnemies gameplayAddress enemies |> setPlayer gameplayAddress player

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
            let enemyCount = randResult + 5
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

        static let handleNewGame event world =

            // get common addresses
            let gameplayAddress = World.unwrapA event world
            let sceneAddress = getSceneAddress gameplayAddress

            // generate non-deterministic random numbers
            let sysrandom = Random ()
            let contentSeedState = uint64 <| sysrandom.Next ()
            let ongoingSeedState = uint64 <| sysrandom.Next ()

            // get and initialize gameplay screen
            let gameplay = World.getScreen gameplayAddress world
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
            let world = snd <| World.addGroup sceneAddress sceneHierarchy world
            (Cascade, world)

        static let handleLoadGame event world =

            // get common addresses
            let gameplayAddress = World.unwrapA event world
            let sceneAddress = getSceneAddress gameplayAddress

            // get and initialize gameplay screen from read
            let gameplayHierarchy = World.readScreenHierarchyFromFile SaveFilePath world
            let (gameplayFromRead, groupHierarchies) = gameplayHierarchy
            let gameplay = World.getScreen gameplayAddress world
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
            let world = snd <| World.addGroup sceneAddress sceneHierarchy world
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

        static let determineEnemyTurn occupationMap (enemy : Entity) rand =
            match enemy.ControlType with
            | Player ->
                debug <| "Invalid ControlType '" + acstring enemy.ControlType + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                let (randResult, rand) = Rand.nextIntUnder 4 rand
                let walkDirection = Direction.fromInt randResult
                let enemyTurn = CharacterActivity.determineTurnFromDirection walkDirection occupationMap enemy
                (enemyTurn, rand)
            | Uncontrolled -> (NoTurn, rand)

        static let determineEnemyTurns occupationMap enemies rand =
            let (_, enemyTurns, rand) =
                List.foldBack
                    (fun (enemy : Entity) (occupationMap, enemyTurns, rand) ->
                        let (enemyTurn, rand) = determineEnemyTurn occupationMap enemy rand
                        let occupationMap = OccupationMap.transferByTurn enemyTurn enemy occupationMap
                        (occupationMap, enemyTurn :: enemyTurns, rand))
                    enemies
                    (occupationMap, [], rand)
            (enemyTurns, rand)

        static let setEnemyActivityFromTurn enemyTurn enemy =
            match enemyTurn with
            | NavigationTurn navigationDescriptor -> Entity.setActivityState (Navigation navigationDescriptor) enemy
            | CancelTurn -> Entity.setActivityState NoActivity enemy
            | NoTurn -> Entity.setActivityState NoActivity enemy

        static let setEnemyActivitiesFromTurns enemyTurns enemies =
            List.map2 setEnemyActivityFromTurn enemyTurns enemies

        static let advanceEnemyNavigations enemies =
            List.map CharacterActivity.advanceNavigation enemies

        static let isTurnProgressing enemies player =
            CharacterActivity.anyActivitiesInProgress (player :: enemies)

        static let determinePlayerTurn playerTurnInput occupationMapWithAdjacentEnemies occupationMapWithEnemies camera enemies player =

            // determine player turn from their current activity
            match CharacterActivity.determineTurnFromActivityState player with
            | NoTurn ->

                // determine player turn from gameplay input
                match playerTurnInput with
                | Touch touchPosition ->
                    if not <| isTurnProgressing enemies player then
                        let touchPositionW = Camera.mouseToWorld Relative touchPosition camera
                        let playerTurn = CharacterActivity.determineTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies player
                        match playerTurn with
                        | NavigationTurn navigationDescriptor as navigationTurn ->
                            let firstNavigationNode = List.head <| Option.get ^| navigationDescriptor.OptNavigationPath
                            if Map.find firstNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                            else navigationTurn
                        | CancelTurn -> CancelTurn
                        | NoTurn -> NoTurn
                    else NoTurn

                // determine player turn
                | DetailNavigation direction ->
                    if not <| isTurnProgressing enemies player
                    then CharacterActivity.determineTurnFromDirection direction occupationMapWithEnemies player
                    else NoTurn

                // no turn
                | NoInput -> NoTurn

            // navigate if not blocked, cancel otherwise
            | NavigationTurn navigationDescriptor as navigationTurn ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                let playerPositionM = Vector2i (Vector2.Divide (player.Position, TileSize))
                if playerPositionM = walkDescriptor.WalkOriginM then
                    let walkDestinationM = walkDescriptor.WalkOriginM + Direction.toVector2i walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                else navigationTurn
            
            // probably should never reach this, but forward value if so
            | CancelTurn -> CancelTurn

        static let tryAdvanceCharacters playerTurn occupationMap enemies player rand =

            // try to set character activities
            let (enemies, player, rand) =
                match playerTurn with
                | NavigationTurn navigationDescriptor ->

                    // determine enemy turns
                    let (enemyTurns, rand) = determineEnemyTurns occupationMap enemies rand

                    // ->
                    // -> any intermediate processing of turns goes here
                    // ->

                    // set player activity
                    let player = Entity.setActivityState (Navigation navigationDescriptor) player

                    // set enemy activities
                    let enemies = setEnemyActivitiesFromTurns enemyTurns enemies
                    (enemies, player, rand)

                // cancel player activity
                | CancelTurn -> (enemies, Entity.setActivityState NoActivity player, rand)

                // no new activity
                | NoTurn -> (enemies, player, rand)

            // advance player
            let player = CharacterActivity.advanceNavigation player

            // advance enemies
            let enemies = advanceEnemyNavigations enemies
            (enemies, player, rand)

        static let handleTick event world =

            // construct context
            let (gameplayAddress, gameplay : Screen) = World.unwrapAS event world
            let (field, enemies, player) = getParticipants gameplayAddress world
            let rand = Rand.make gameplay.OngoingRandState
            let playerPositionM = Vector2i (Vector2.Divide (player.Position, TileSize))
            let occupationMapWithoutEnemies = OccupationMap.makeFromFieldTiles field.FieldMapNp.FieldTiles
            let occupationMapWithAdjacentEnemies = List.fold (flip <| OccupationMap.occupyByAdjacentCharacter playerPositionM) occupationMapWithoutEnemies enemies
            let occupationMapWithEnemies = List.fold (flip OccupationMap.occupyByCharacter) occupationMapWithoutEnemies enemies

            // determine player turn from input
            let playerTurn = determinePlayerTurn gameplay.PlayerTurnInput occupationMapWithAdjacentEnemies occupationMapWithEnemies world.Camera enemies player
            let occupationMapWithEveryone = OccupationMap.occupyByTurn playerTurn occupationMapWithEnemies
            let gameplay = Screen.setPlayerTurnInput NoInput gameplay
            let world = World.setScreen gameplayAddress gameplay world

            // try advance and update participants
            let (enemies, player, rand) = tryAdvanceCharacters playerTurn occupationMapWithEveryone enemies player rand
            let world = setParticipants gameplayAddress field enemies player world

            // update ongoing rand state
            let gameplay = Screen.setOngoingRandState (Rand.getState rand) gameplay
            let world = World.setScreen gameplayAddress gameplay world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let (gameplayAddress, gameplay) = World.unwrapAS event world
            let gameplay = Screen.setPlayerTurnInput (Touch event.Data) gameplay
            let world = World.setScreen gameplayAddress gameplay world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let (gameplayAddress, gameplay) = World.unwrapAS event world
            let gameplay = Screen.setPlayerTurnInput (DetailNavigation direction) gameplay
            let world = World.setScreen gameplayAddress gameplay world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? PlayerTurnInput NoInput]

        override dispatcher.Register (gameplayAddress, gameplay, world) =
            let world = observe TickEventAddress gameplayAddress |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- getHudFeelerAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (TouchEventAddress ->>- getHudFeelerAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailUpAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail North) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailRightAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail East) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailDownAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail South) world |> snd
            let world = observe (DownEventAddress ->>- getHudDetailLeftAddress gameplayAddress) gameplayAddress |> filter isSelected |> monitor (handleDownDetail West) world |> snd
            let world = observe ClickTitleNewGameEventAddress gameplayAddress |> subscribe handleNewGame world |> snd
            let world = observe ClickTitleLoadGameEventAddress gameplayAddress |> filter (fun _ _ -> File.Exists SaveFilePath) |> subscribe handleLoadGame world |> snd
            let world = World.subscribe4 handleClickSaveGame (ClickEventAddress ->>- getHudSaveGameAddress gameplayAddress) gameplayAddress world
            let world = World.subscribe4 handleDeselectGameplay (DeselectEventAddress ->>- gameplayAddress) gameplayAddress world
            (gameplay, world)