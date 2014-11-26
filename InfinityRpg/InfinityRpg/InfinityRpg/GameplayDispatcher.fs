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
            (getField world, List.ofSeq <| getEnemies world, getPlayer world)

        static let tryChangeEnemyActivityToWalk field (enemy : Entity) rand =
            match enemy.ControlType with
            | Player ->
                debug <| "Invalid ControlType '" + acstring enemy.ControlType + "' for enemy"
                (enemy, rand)
            | Chaos ->
                let (randResult, rand) = Rand.nextIntUnder 4 rand
                let walkDirection = Direction.fromInt randResult
                let enemy = snd <| CharacterActivity.tryChangeActivityToNavigationByDirection walkDirection field enemy
                (enemy, rand)
            | Uncontrolled -> (enemy, rand)

        static let tryChangeEnemyActivitiesToWalk field enemies rand =
            List.fold
                (fun (enemies, rand) (enemy : Entity) ->
                    let (enemy, rand) = tryChangeEnemyActivityToWalk field enemy rand
                    (enemy :: enemies, rand))
                ([], rand)
                enemies

        static let advanceEnemyWalk playerActivityReport field (enemy : Entity) rand =
            let wasNavigating = ActivityState.isNavigating enemy.ActivityState
            let enemy = snd <| CharacterActivity.advanceNavigation field enemy
            match playerActivityReport with
            | TurnTaken ->
                let justFinishedNavigation = wasNavigating && ActivityState.isNavigating enemy.ActivityState
                if justFinishedNavigation then tryChangeEnemyActivityToWalk field enemy rand
                else (enemy, rand)
            | NoTurnTaken -> (enemy, rand)

        static let advanceEnemiesWalk playerActivityReport field enemies rand =
            List.fold
                (fun (enemies, rand) (enemy : Entity) ->
                    let (enemy, rand) = advanceEnemyWalk playerActivityReport field enemy rand
                    (enemy :: enemies, rand))
                ([], rand)
                enemies

        static let canPlayerAct enemies =
            Seq.notExists (fun (enemy : Entity) -> enemy.ActivityState <> NoActivity) enemies

        static let handleTick _ world =

            // get participants
            let (field, enemies, player) = getParticipants world

            // advance player - player always goes first in each turn
            let (turnReport, player) =
                let optWalkDirection =
                    if World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_RIGHT) world then Some East
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_LEFT) world then Some West
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_UP) world then Some North
                    elif World.isKeyboardKeyDown (int SDL.SDL_Scancode.SDL_SCANCODE_DOWN) world then Some South
                    else None
                let (turnReport, player) =
                    match optWalkDirection with
                    | Some walkDirection ->
                        if canPlayerAct enemies
                        then CharacterActivity.tryChangeActivityToNavigationByDirection walkDirection field player
                        else (NoTurnTaken, player)
                    | None -> (NoTurnTaken, player)
                let (turnReport2, player) = CharacterActivity.advanceNavigation field player
                (TurnReport.join turnReport turnReport2, player)
            let world = World.setEntity PlayerAddress player world

            // advance enemies
            let world =
                let (enemies, rand) =
                    let rand = Rand.make world.Game?RandState
                    let (enemies, rand) =
                        match turnReport with
                        | TurnTaken -> tryChangeEnemyActivitiesToWalk field enemies rand
                        | NoTurnTaken -> (enemies, rand)
                    advanceEnemiesWalk turnReport field enemies rand
                let world = World.setEntities SceneAddress enemies world
                let game = world.Game?RandState <- Rand.getState rand
                World.setGame game world

            // cascade
            (Cascade, world)

        static let handleTouchFeeler event world =

            // pull in context
            let touchPosition : Vector2 = World.unwrapD event world
            let touchPositionW = Camera.mouseToWorld Relative touchPosition world.Camera
            let (field, enemies, player) = getParticipants world
            
            // advance player
            let canPlayerAct = canPlayerAct enemies
            let player = if canPlayerAct then CharacterActivity.touch touchPositionW field player else player
            let world = World.setEntity PlayerAddress player world
            
            // advance enemies
            let world =
                if canPlayerAct then
                    let rand = Rand.make <| world.Game?RandState
                    let (enemies, rand) = tryChangeEnemyActivitiesToWalk field enemies rand
                    let world = World.setEntities SceneAddress enemies world
                    let game = world.Game?RandState <- Rand.getState rand
                    World.setGame game world
                else world

            // cascade
            (Cascade, world)

        static let handleDownDetail direction event world =
            
            // grab participants
            let (field, enemies, player) = getParticipants world

            // advance player
            let (playerTurnReport, player) =
                if canPlayerAct enemies
                then CharacterActivity.tryChangeActivityToNavigationByDirection direction field player
                else (NoTurnTaken, player)
            let world = World.setEntity PlayerAddress player world

            // advance enemies
            let world =
                match playerTurnReport with
                | TurnTaken ->
                    let (enemies, rand) = tryChangeEnemyActivitiesToWalk field (List.ofSeq enemies) (Rand.make <| world.Game?RandState)
                    let world = World.setEntities SceneAddress enemies world
                    let game = world.Game?RandState <- Rand.getState rand
                    World.setGame game world
                | NoTurnTaken -> world

            // cascade
            (Cascade, world)

        static let makeField rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let field = World.makeEntity typeof<FieldDispatcher>.Name (Some FieldName) world
            let field = Entity.setFieldMapNp fieldMap field
            let field = Entity.setSize (Entity.getQuickSize field world) field
            let field = Entity.setPersistent false field
            (field, rand)

        static let handleNewGame _ world =

            // make random seed
            let systemRandomSeedState = uint64 <| (new Random ()).Next ()
            let rand = Rand.make systemRandomSeedState

            // make field
            let (field, rand) = makeField rand world

            // make player
            let player = World.makeEntity typeof<PlayerDispatcher>.Name (Some PlayerName) world
            let player = Entity.setDepth CharacterDepth player

            // make enemies
            let rand = Rand.make world.Game?RandState
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

            // update game rand state
            let game = world.Game?RandState <- Rand.getState rand
            let world = World.setGame game world
            (Cascade, world)

        static let handleLoadGame _ world =

            // replace game value
            let gameHierarchy = World.readGameFromFile SaveFilePath world
            let (game, screenHierarchy) = gameHierarchy
            let world = World.setGame game world

            // get rand from game
            let rand = Rand.make world.Game?RandState

            // make field
            let (field, rand) = makeField rand world

            // find scene hierarchy and add field to it
            let sceneHierarchy = Map.find SceneName <| snd ^| Map.find GameplayName screenHierarchy
            let (scene, entities) = sceneHierarchy
            let entities = Map.add field.Name field entities
            let sceneHierarchy = (scene, entities)

            // add scene hierarchy to world
            let world = snd <| World.addGroup SceneAddress sceneHierarchy world

            // update game rand state
            let game = world.Game?RandState <- Rand.getState rand
            let world = World.setGame game world
            (Cascade, world)

        static let handleClickSaveGame _ world =
            let gameHierarchy = World.getGameHierarchy world
            World.writeGameToFile SaveFilePath gameHierarchy world
            (Cascade, world)

        static let handleDeselectGameplay _ world =
            let scene = World.getGroup SceneAddress world
            let world = snd <| World.removeGroup SceneAddress scene world
            (Cascade, world)

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