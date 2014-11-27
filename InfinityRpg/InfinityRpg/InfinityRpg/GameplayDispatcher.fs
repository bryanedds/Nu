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
        member screen.InputPlayerTurn = screen?InputPlayerTurn : TurnDescriptor
        static member setInputPlayerTurn (value : TurnDescriptor) (screen : Screen) = screen?InputPlayerTurn <- value

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        static let getField world =
            World.getEntity FieldAddress world

        static let getEnemies world =
            World.getEntities SceneAddress world |>
            Seq.filter (Entity.dispatchesAs typeof<EnemyDispatcher>)

        static let getPlayer world =
            World.getEntity PlayerAddress world

        static let getParticipants world =
            (getField world, List.ofSeq <| getEnemies world, getPlayer world, World.getScreen GameplayAddress world)

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
            let (enemyCount, rand) = Rand.nextIntUnder 10 rand
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

        static let queryEnemyTurn field (enemy : Entity) rand =
            match enemy.ControlType with
            | Player ->
                debug <| "Invalid ControlType '" + acstring enemy.ControlType + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                let (randResult, rand) = Rand.nextIntUnder 4 rand
                let walkDirection = Direction.fromInt randResult
                let enemyTurn = CharacterActivity.queryTurnOnDirection walkDirection field enemy
                (enemyTurn, rand)
            | Uncontrolled -> (NoTurn, rand)

        static let queryEnemyTurns field enemies rand =
            //let (enemyTurnsRev, rand) =
                List.foldBack
                    (fun (enemy : Entity) (enemyTurns, rand) ->
                        let (enemyTurn, rand) = queryEnemyTurn field enemy rand
                        (enemyTurn :: enemyTurns, rand))
                    enemies
                    ([], rand)
            //let enemyTurns = List.rev enemyTurnsRev
            //(enemyTurns, rand)

        static let setEnemyTurns enemyTurns enemies =
            List.map2
                (fun enemyTurn enemy ->
                    match enemyTurn with
                    | NavigationTurn navigationDescriptor -> Entity.setActivityState (Navigation navigationDescriptor) enemy
                    | NoTurn -> Entity.setActivityState NoActivity enemy)
                enemyTurns
                enemies

        static let advanceEnemyNavigations field enemies =
            List.map (CharacterActivity.advanceNavigation field) enemies

        static let isTurnInProgress player enemies (gameplay : Screen) =
            match gameplay.InputPlayerTurn with
            | NavigationTurn _ -> true
            | NoTurn -> List.notExists (fun (character : Entity) -> character.ActivityState <> NoActivity) (player :: enemies)

        static let handleTick _ world =

            // get participants
            let (field, enemies, player, gameplay) = getParticipants world

            // make rand from gameplay
            let rand = Rand.make gameplay.OngoingRandState

            // get and update player turn from gameplay
            let playerTurn =
                match gameplay.InputPlayerTurn with
                | NavigationTurn _ as playerTurn -> playerTurn
                | NoTurn -> CharacterActivity.getNavigationTurn player
            let gameplay = Screen.setInputPlayerTurn NoTurn gameplay
            let world = World.setScreen GameplayAddress gameplay world

            let (world, rand) =

                let (player, enemies, rand) =
                    match playerTurn with
                    | NavigationTurn navigationDescriptor ->
                    
                        // get enemy turns
                        let (enemyTurns, rand) = queryEnemyTurns field enemies rand

                        // -> any intermediate processing of turns goes here <-

                        // set player turn
                        let player = Entity.setActivityState (Navigation navigationDescriptor) player

                        // set enemy turns
                        let enemies = setEnemyTurns enemyTurns enemies
                        (player, enemies, rand)

                    | NoTurn -> (player, enemies, rand)
            
                // advance player
                let player = CharacterActivity.advanceNavigation field player
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

            // pull in context
            let touchPosition : Vector2 = World.unwrapD event world
            let touchPositionW = Camera.mouseToWorld Relative touchPosition world.Camera
            let (field, enemies, player, gameplay) = getParticipants world
            
            // update player turn
            let playerTurn =
                if isTurnInProgress player enemies gameplay
                then CharacterActivity.touch touchPositionW field player
                else NoTurn
            let gameplay = Screen.setInputPlayerTurn playerTurn gameplay
            let world = World.setScreen GameplayAddress gameplay world
            
            // cascade
            (Cascade, world)

        static let handleDownDetail direction event world =
            
            // grab participants
            let (field, enemies, player, gameplay) = getParticipants world

            // update player turn from gameplay
            let playerTurn =
                if isTurnInProgress player enemies gameplay
                then CharacterActivity.queryTurnOnDirection direction field player
                else NoTurn
            let gameplay = Screen.setInputPlayerTurn playerTurn gameplay
            let world = World.setScreen GameplayAddress gameplay world

            // cascade
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? InputPlayerTurn NoTurn]

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