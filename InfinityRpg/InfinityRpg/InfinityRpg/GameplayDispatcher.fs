namespace InfinityRpg
open System
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

        static let getField world =
            World.getEntity FieldAddress world

        static let getEnemies world =
            World.getEntities SceneAddress world |>
            Seq.filter (Entity.dispatchesAs typeof<EnemyDispatcher>)

        static let getPlayer world =
            World.getEntity PlayerAddress world

        static let getGameplay world =
            World.getScreen GameplayAddress world

        static let getParticipants world =
            (getField world, List.ofSeq <| getEnemies world, getPlayer world, getGameplay world)

        static let makeField rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
            let field = Entity.setFieldMapNp fieldMap field
            let field = Entity.setSize (Entity.getQuickSize field world) field
            let field = Entity.setPersistent false field
            (field, rand)

        static let handleNewGame _ world =

            // get and initialize gameplay screen
            let sysrandom = Random ()
            let contentSeedState = uint64 <| sysrandom.Next ()
            let ongoingSeedState = uint64 <| sysrandom.Next ()
            let gameplay = World.getScreen GameplayAddress world
            let gameplay = Screen.setContentRandState contentSeedState gameplay
            let gameplay = Screen.setOngoingRandState ongoingSeedState gameplay
            let world = World.setScreen GameplayAddress gameplay world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field
            let (field, rand) = makeField rand world

            // make player
            let player = World.makeEntity typeof<PlayerDispatcher>.Name (Some PlayerName) world
            let player = Entity.setDepth CharacterDepth player

            // make enemies
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 5
            let (enemies, rand) =
                List.fold
                    (fun (enemies, rand) positionM ->
                        let enemyPosition = single positionM * TileSize * 2.0f
                        let enemy = World.makeEntity typeof<EnemyDispatcher>.Name None world
                        let enemy = Entity.setDepth CharacterDepth enemy
                        let enemy = Entity.setPosition enemyPosition enemy
                        let enemy = Entity.setCharacterAnimationSheet ZommieImage enemy
                        (enemy :: enemies, rand))
                    ([], rand)
                    [0 .. enemyCount - 1]
            let enemyMap = Map.ofListBy (fun (enemy : Entity) -> enemy.Name, enemy) enemies
            let world = snd <| World.addEntities SceneAddress enemyMap world

            // make scene hierarchy
            let entities = Map.ofList [(field.Name, field); (player.Name, player)]
            let scene = World.makeGroup typeof<GroupDispatcher>.Name (Some SceneName) world
            let sceneHierarchy = (scene, entities)

            // add scene hierarchy to world
            let world = snd <| World.addGroup SceneAddress sceneHierarchy world
            (Cascade, world)

        static let handleLoadGame _ world =

            // read in gameplay screen
            let gameplayHierarchy = World.readScreenHierarchyFromFile SaveFilePath world
            let (gameplayFromRead, groupHierarchies) = gameplayHierarchy
            let gameplay = World.getScreen GameplayAddress world
            let gameplay = Screen.setContentRandState gameplayFromRead.ContentRandState gameplay
            let gameplay = Screen.setOngoingRandState gameplayFromRead.OngoingRandState gameplay
            let world = World.setScreen GameplayAddress gameplay world

            // make rand from gameplay
            let rand = Rand.make gameplay.ContentRandState

            // make field
            let (field, rand) = makeField rand world

            // find scene hierarchy and add field to it
            let sceneHierarchy = Map.find SceneName groupHierarchies
            let (scene, entities) = sceneHierarchy
            let entities = Map.add field.Name field entities
            let sceneHierarchy = (scene, entities)

            // add scene hierarchy to world
            let world = snd <| World.addGroup SceneAddress sceneHierarchy world
            (Cascade, world)

        static let handleClickSaveGame _ world =
            let gameplayHierarchy = World.getScreenHierarchy GameplayAddress world
            World.writeScreenHierarchyToFile SaveFilePath gameplayHierarchy world
            (Cascade, world)

        static let handleDeselectGameplay _ world =
            let scene = World.getGroup SceneAddress world
            let world = snd <| World.removeGroup SceneAddress scene world
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

        static let determineEnemyTurns playerTurn occupationMap enemies rand =
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

        static let advanceEnemyNavigations field enemies =
            List.map CharacterActivity.advanceNavigation enemies

        static let isTurnProgressing player enemies =
            CharacterActivity.anyActivitiesInProgress (player :: enemies)

        static let determinePlayerTurn playerTurnInput occupationMapWithAdjacentEnemies occupationMapWithEnemies camera enemies player =

            // determine player turn from their current activity
            match CharacterActivity.determineTurnFromActivityState player with
            | NoTurn ->

                // determine player turn from gameplay input
                match playerTurnInput with
                | Touch touchPosition ->
                    if not <| isTurnProgressing player enemies then
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
                    if not <| isTurnProgressing player enemies
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

        static let handleTick _ world =

            // construct context
            let (field, enemies, player, gameplay) = getParticipants world
            let rand = Rand.make gameplay.OngoingRandState
            let playerPositionM = Vector2i (Vector2.Divide (player.Position, TileSize))
            let occupationMapWithoutEnemies = OccupationMap.makeFromFieldTiles field.FieldMapNp.FieldTiles
            let occupationMapWithAdjacentEnemies = List.fold (flip <| OccupationMap.occupyByAdjacentCharacter playerPositionM) occupationMapWithoutEnemies enemies
            let occupationMapWithEnemies = List.fold (flip OccupationMap.occupyByCharacter) occupationMapWithoutEnemies enemies

            // determine player turn from input
            let playerTurn = determinePlayerTurn gameplay.PlayerTurnInput occupationMapWithAdjacentEnemies occupationMapWithEnemies world.Camera enemies player
            let occupationMapWithEveryone = OccupationMap.occupyByTurn playerTurn occupationMapWithEnemies
            let gameplay = Screen.setPlayerTurnInput NoInput gameplay
            let world = World.setScreen GameplayAddress gameplay world

            // try to advance characters
            let (world, rand) =

                // try to set character activities
                let (player, enemies, rand) =
                    match playerTurn with
                    | NavigationTurn navigationDescriptor ->

                        // determine enemy turns
                        let (enemyTurns, rand) = determineEnemyTurns playerTurn occupationMapWithEveryone enemies rand

                        // ->
                        // -> any intermediate processing of turns goes here
                        // ->

                        // set player activity
                        let player = Entity.setActivityState (Navigation navigationDescriptor) player

                        // set enemy activities
                        let enemies = setEnemyActivitiesFromTurns enemyTurns enemies
                        (player, enemies, rand)

                    // cancel player activity
                    | CancelTurn -> (Entity.setActivityState NoActivity player, enemies, rand)

                    // no new activity
                    | NoTurn -> (player, enemies, rand)

                // advance player
                let player = CharacterActivity.advanceNavigation player
                let world = World.setEntity PlayerAddress player world

                // advance enemies
                let enemies = advanceEnemyNavigations field enemies
                let world = World.setEntities SceneAddress enemies world
                (world, rand)

            // update ongoing rand state
            let gameplay = Screen.setOngoingRandState (Rand.getState rand) gameplay
            let world = World.setScreen GameplayAddress gameplay world

            // cascade
            (Cascade, world)

        static let handleTouchFeeler event world =
            let gameplay = Screen.setPlayerTurnInput (Touch event.Data) (getGameplay world)
            let world = World.setScreen GameplayAddress gameplay world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let gameplay = Screen.setPlayerTurnInput (DetailNavigation direction) (getGameplay world)
            let world = World.setScreen GameplayAddress gameplay world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? PlayerTurnInput NoInput]

        override dispatcher.Register (address, screen, world) =
            if address <> GameplayAddress then failwith "Invalid address for GameplayDispatcher screen."
            let world = observe TickEventAddress address |> filter isSelected |> monitor handleTick world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (TouchEventAddress ->>- HudFeelerAddress) address |> filter isSelected |> monitor handleTouchFeeler world |> snd
            let world = observe (DownEventAddress ->>- HudDetailUpAddress) address |> filter isSelected |> monitor (handleDownDetail North) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailRightAddress) address |> filter isSelected |> monitor (handleDownDetail East) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailDownAddress) address |> filter isSelected |> monitor (handleDownDetail South) world |> snd
            let world = observe (DownEventAddress ->>- HudDetailLeftAddress) address |> filter isSelected |> monitor (handleDownDetail West) world |> snd
            let world = observe ClickTitleNewGameEventAddress GameAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleNewGame world |> snd
            let world = observe ClickTitleLoadGameEventAddress GameAddress |> product (SelectEventAddress ->>- GameplayAddress) |> subscribe handleLoadGame world |> snd
            let world = World.subscribe4 handleClickSaveGame ClickHudSaveGameEventAddress GameAddress world
            let world = World.subscribe4 handleDeselectGameplay (DeselectEventAddress ->>- GameplayAddress) GameAddress world
            (screen, world)