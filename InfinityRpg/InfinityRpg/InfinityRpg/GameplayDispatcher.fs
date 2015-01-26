namespace InfinityRpg
open System
open System.IO
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants
open Nu.Observation
open Nu.Chain
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

        member this.GetContentRandState world : uint64 = (this.GetXtension world)?ContentRandState
        member this.SetContentRandState (value : uint64) world = this.UpdateXtension (fun xtension -> xtension?ContentRandState <- value) world
        member this.GetOngoingRandState world : uint64 = (this.GetXtension world)?OngoingRandState
        member this.SetOngoingRandState (value : uint64) world = this.UpdateXtension (fun xtension -> xtension?OngoingRandState <- value) world
        member this.GetShallLoadGame world : bool = (this.GetXtension world)?ShallLoadGame
        member this.SetShallLoadGame (value : bool) world = this.UpdateXtension (fun xtension -> xtension?ShallLoadGame <- value) world

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        (* Hud Proxies *)

        static let proxyHud gameplay = Group.proxy <| satoga gameplay.ScreenAddress HudName
        static let proxyHudHalt gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudHaltName
        static let proxyHudSaveGame gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudSaveGameName
        static let proxyHudFeeler gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudFeelerName
        static let proxyHudDetailUp gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudDetailUpName
        static let proxyHudDetailRight gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudDetailRightName
        static let proxyHudDetailDown gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudDetailDownName
        static let proxyHudDetailLeft gameplay = Entity.proxy <| gatoea (proxyHud gameplay).GroupAddress HudDetailLeftName

        (* Scene Proxies *)

        static let proxyScene gameplay =
            Group.proxy <| satoga gameplay.ScreenAddress SceneName
        
        static let proxyField gameplay =
            Entity.proxy <| gatoea (proxyScene gameplay).GroupAddress FieldName
        
        static let proxyCharacters gameplay world =
            let entities = World.proxyEntities (proxyScene gameplay) world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs typeof<CharacterDispatcher> world) entities

        static let proxyOptCharacterAtPosition position gameplay world =
            let characters = proxyCharacters gameplay world
            Seq.tryFind (fun (character : Entity) -> character.GetPosition world = position) characters
        
        static let proxyOptCharacterInDirection position direction gameplay world =
            proxyOptCharacterAtPosition (position + dtovf direction) gameplay world
        
        static let proxyCharacterInDirection position direction gameplay world =
            Option.get <| proxyOptCharacterInDirection position direction gameplay world

        static let proxyPlayer gameplay =
            Entity.proxy <| gatoea (proxyScene gameplay).GroupAddress PlayerName

        static let proxyEnemies gameplay world =
            let entities = World.proxyEntities (proxyScene gameplay) world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs typeof<EnemyDispatcher> world) entities

        (* End of Proxies *)

        static let createField scene rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let (field, world) = World.createEntity typeof<FieldDispatcher>.Name (Some FieldName) scene world
            let world = field.SetFieldMapNp fieldMap world
            let world = field.SetSize (World.getEntityQuickSize field world) world
            let world = field.SetPersistent false world
            (field, rand, world)

        static let createEnemies scene rand world =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 1
            List.fold
                (fun (enemies, rand, world) i ->
                    let enemyPosition = single i * TileSize * 2.0f
                    let (enemy, world) = World.createEntity typeof<EnemyDispatcher>.Name None scene world
                    let world = enemy.SetDepth CharacterDepth world
                    let world = enemy.SetPosition enemyPosition world
                    let world = enemy.SetCharacterAnimationSheet GoopyImage world
                    (enemy :: enemies, rand, world))
                ([], rand, world)
                [0 .. enemyCount - 1]

        static let walk3 positive current destination =
            let walkSpeed = if positive then CharacterWalkSpeed else -CharacterWalkSpeed
            let next = current + walkSpeed
            let delta = if positive then destination - next else next - destination
            if delta < CharacterWalkSpeed then (destination, WalkFinished) else (next, WalkContinuing)

        static let walk walkDescriptor (position : Vector2) =
            let walkOrigin = vmtovf walkDescriptor.WalkOriginM
            let walkVector = dtovf walkDescriptor.WalkDirection
            let walkDestination = walkOrigin + walkVector
            match walkDescriptor.WalkDirection with
            | Upward -> let (newY, arrival) = walk3 true position.Y walkDestination.Y in (Vector2 (position.X, newY), arrival)
            | Rightward -> let (newX, arrival) = walk3 true position.X walkDestination.X in (Vector2 (newX, position.Y), arrival)
            | Downward -> let (newY, arrival) = walk3 false position.Y walkDestination.Y in (Vector2 (position.X, newY), arrival)
            | Leftward -> let (newX, arrival) = walk3 false position.X walkDestination.X in (Vector2 (newX, position.Y), arrival)

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

        static let tryGetNavigationPath touchPosition occupationMap (character : Entity) world =
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find (vftovm touchPosition) nodes
            let currentNode = Map.find (vftovm <| character.GetPosition world) nodes
            let optNavigationPath =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun n -> 0.0f))
            match optNavigationPath with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let isPlayerNavigatingPath gameplay world =
            let player = proxyPlayer gameplay
            ActivityState.isNavigatingPath <| player.GetActivityState world

        static let cancelNavigation (character : Entity) world =
            let characterActivity =
                match character.GetActivityState world with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with OptNavigationPath = None }
            character.SetActivityState characterActivity world

        static let anyTurnsInProgress2 (player : Entity) enemies world =
            player.GetActivityState world <> NoActivity ||
            Seq.exists
                (fun (enemy : Entity) -> enemy.GetDesiredTurn world <> NoTurn || enemy.GetActivityState world <> NoActivity)
                enemies

        static let anyTurnsInProgress gameplay world =
            let player = proxyPlayer gameplay
            let enemies = proxyEnemies gameplay world
            anyTurnsInProgress2 player enemies world

        static let updateCharacterByWalk walkDescriptor (character : Entity) world =
            let (newPosition, walkState) = walk walkDescriptor <| character.GetPosition world
            let world = character.SetPosition newPosition world
            let characterAnimationState = { character.GetCharacterAnimationState world with Direction = walkDescriptor.WalkDirection }
            let world = character.SetCharacterAnimationState characterAnimationState world
            (world, walkState)

        static let updateCharacterByWalkState walkState navigationDescriptor (character : Entity) world =
            match walkState with
            | WalkFinished ->
                match navigationDescriptor.OptNavigationPath with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) -> character.SetActivityState NoActivity world
                | Some (currentNode :: navigationPath) ->
                    let walkDirection = vmtod <| (List.head navigationPath).PositionM - currentNode.PositionM
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm <| character.GetPosition world }
                    let navigationDescriptor = { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                    character.SetActivityState (Navigation navigationDescriptor) world
                | None -> character.SetActivityState NoActivity world
            | WalkContinuing -> world

        static let updateCharacterByNavigation navigationDescriptor character world =
            let (world, walkState) = updateCharacterByWalk navigationDescriptor.WalkDescriptor character world
            updateCharacterByWalkState walkState navigationDescriptor character world

        static let updateCharacterByAction actionDescriptor (character : Entity) world =
            if actionDescriptor.ActionTicks = 0L then
                world |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionBegin world.State.TickTime (character.GetPosition world) (character.GetCharacterAnimationState world) actionDescriptor) |>
                    character.SetActivityState (Action <| ActionDescriptor.incActionTicks actionDescriptor)
            elif actionDescriptor.ActionTicks > 0L && actionDescriptor.ActionTicks < ActionTicksMax then
                world |>
                    character.SetActivityState (Action <| ActionDescriptor.incActionTicks actionDescriptor)
            else
                world |>
                    character.SetActivityState NoActivity |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionEnd world.State.TickTime (character.GetCharacterAnimationState world))

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents world =
            match character.GetActivityState world with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM (vftovm <| character.GetPosition world) occupationMap
                if Set.contains direction openDirections then
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = vftovm <| character.GetPosition world }
                    NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
                else
                    let targetPosition = character.GetPosition world + dtovf direction
                    if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                    then makeAttackTurn <| vftovm targetPosition
                    else NoTurn

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents world =
            if character.GetActivityState world = NoActivity then
                match tryGetNavigationPath touchPosition occupationMap character world with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | _ ->
                        let characterPositionM = vftovm <| character.GetPosition world
                        let walkDirection = vmtod <| (List.head navigationPath).PositionM - characterPositionM
                        let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                        NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                | None ->
                    let targetPosition = touchPosition |> vftovm |> vmtovf
                    if Math.arePositionsAdjacent targetPosition <| character.GetPosition world then
                        if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                        then makeAttackTurn <| vftovm targetPosition
                        else NoTurn
                    else NoTurn
            else NoTurn

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) rand world =
            match enemy.GetControlType world with
            | Player ->
                debug <| "Invalid ControlType '" + acstring (enemy.GetControlType world) + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                let nextPlayerPosition =
                    match player.GetActivityState world with
                    | Action _ -> player.GetPosition world
                    | Navigation navigationDescriptor -> NavigationDescriptor.nextPosition navigationDescriptor
                    | NoActivity -> player.GetPosition world
                if Math.arePositionsAdjacent (enemy.GetPosition world) nextPlayerPosition then
                    let enemyTurn = makeAttackTurn <| vftovm nextPlayerPosition
                    (enemyTurn, rand)
                else
                    let (randResult, rand) = Rand.nextIntUnder 4 rand
                    let direction = Direction.fromInt randResult
                    let enemyTurn = determineCharacterTurnFromDirection direction occupationMap enemy [player] world
                    (enemyTurn, rand)
            | Uncontrolled -> (NoTurn, rand)

        static let determineDesiredEnemyTurns occupationMap player enemies rand world =
            let (_, enemyTurns, rand) =
                List.foldBack
                    (fun (enemy : Entity) (occupationMap, enemyTurns, rand) ->
                        let (enemyTurn, rand) = determineDesiredEnemyTurn occupationMap player enemy rand world
                        let occupationMap = OccupationMap.transferByDesiredTurn enemyTurn (enemy.GetPosition world) occupationMap
                        (occupationMap, enemyTurn :: enemyTurns, rand))
                    (List.ofSeq enemies)
                    (occupationMap, [], rand)
            (enemyTurns, rand)

        static let determinePlayerTurnFromTouch touchPosition gameplay world =
            let field = proxyField gameplay
            let fieldMap = field.GetFieldMapNp world
            let player = proxyPlayer gameplay
            let enemies = proxyEnemies gameplay world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not <| anyTurnsInProgress2 player enemies world then
                let touchPositionW = World.getCameraBy (Camera.mouseToWorld Relative touchPosition) world
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm <| player.GetPosition world) fieldMap.FieldTiles enemyPositions
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies player enemies world with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.OptNavigationPath |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction gameplay world =
            let field = proxyField gameplay
            let fieldMap = field.GetFieldMapNp world
            let player = proxyPlayer gameplay
            let enemies = proxyEnemies gameplay world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not <| anyTurnsInProgress2 player enemies world then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                determineCharacterTurnFromDirection direction occupationMapWithEnemies player enemies world
            else NoTurn

        static let determinePlayerTurnFromInput playerInput address world =
            match playerInput with
            | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition address world
            | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction address world
            | NoInput -> NoTurn

        static let determinePlayerTurn gameplay world =
            let player = proxyPlayer gameplay
            match player.GetActivityState world with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if player.GetPosition world = vmtovf walkDescriptor.WalkOriginM then
                    let field = proxyField gameplay
                    let fieldMap = field.GetFieldMapNp world
                    let enemies = proxyEnemies gameplay world
                    let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    let walkDestinationM = walkDescriptor.WalkOriginM + dtovm walkDescriptor.WalkDirection
                    if Map.find walkDestinationM occupationMapWithEnemies then CancelTurn
                    else NavigationTurn navigationDescriptor
                else NoTurn
            | NoActivity -> NoTurn

        static let determineEnemyActionActivities enemies world =
            List.foldBack
                (fun (enemy : Entity) precedingEnemyActivities ->
                    let enemyActivity =
                        let noPrecedingEnemyActionActivity = Seq.notExists ActivityState.isActing precedingEnemyActivities
                        let noCurrentEnemyActionActivity = Seq.notExists (fun (enemy : Entity) -> ActivityState.isActing (enemy.GetActivityState world)) enemies
                        if noPrecedingEnemyActionActivity && noCurrentEnemyActionActivity then
                            match enemy.GetDesiredTurn world with
                            | ActionTurn actionDescriptor -> Action actionDescriptor
                            | NavigationTurn _ -> NoActivity
                            | CancelTurn -> NoActivity
                            | NoTurn -> NoActivity
                        else NoActivity
                    enemyActivity :: precedingEnemyActivities)
                (List.ofSeq enemies)
                []

        static let determineEnemyNavigationActivities enemies world =
            List.foldBack
                (fun (enemy : Entity) enemyActivities ->
                    let noCurrentEnemyActionActivity =
                        Seq.notExists (fun (enemy : Entity) -> ActivityState.isActing <| enemy.GetActivityState world) enemies
                    let enemyActivity =
                        if noCurrentEnemyActionActivity then
                            match enemy.GetDesiredTurn world with
                            | ActionTurn _ -> NoActivity
                            | NavigationTurn navigationDescriptor -> Navigation navigationDescriptor
                            | CancelTurn -> NoActivity
                            | NoTurn -> NoActivity
                        else NoActivity
                    enemyActivity :: enemyActivities)
                (List.ofSeq enemies)
                []

        static let runCharacterReaction actionDescriptor (initiator : Entity) gameplay world =
            // TODO: implement animations
            if actionDescriptor.ActionTicks = ActionTicksMax then
                let reactor =
                    proxyCharacterInDirection
                        (initiator.GetPosition world)
                        (initiator.GetCharacterAnimationState world).Direction
                        gameplay
                        world
                let reactorDamage = initiator.GetPowerBuff world * 5.0f - reactor.GetShieldBuff world |> int
                let reactorHitPoints = reactor.GetHitPoints world - reactorDamage
                let world = reactor.SetHitPoints reactorHitPoints world
                if reactor.GetHitPoints world <= 0 then
                    if reactor.GetName world = PlayerName
                    then World.transitionScreen Title world
                    else World.destroyEntity reactor world
                else world
            else world

        static let runCharacterNavigation newNavigationDescriptor (character : Entity) gameplay world =
            let chain = chain {
                do! update ^ character.SetActivityState ^ Navigation newNavigationDescriptor
                do! during (fun world ->
                    match character.GetActivityState world with
                    | Navigation navigationDescriptor ->
                        newNavigationDescriptor.WalkDescriptor.WalkOriginM =
                            navigationDescriptor.WalkDescriptor.WalkOriginM
                    | Action _ -> false
                    | NoActivity -> false) ^ chain {
                    do! update ^ fun world ->
                        let navigationDescriptor =
                            match character.GetActivityState world with
                            | Navigation navigationDescriptor -> navigationDescriptor
                            | _ -> failwith "Unexpected match failure in InfinityRpg.GameplayDispatcherModule.runCharacterNavigation."
                        updateCharacterByNavigation navigationDescriptor character world
                    do! pass }}
            let observation = observe TickEventAddress character |> until (DeselectEventAddress ->>- gameplay.ScreenAddress)
            snd <| runAssumingCascade chain observation world

        static let runCharacterAction newActionDescriptor (character : Entity) gameplay world =
            // NOTE: currently just implements attack
            let chain = chain {
                do! update ^ character.SetActivityState ^ Action newActionDescriptor
                do! during (fun world -> ActivityState.isActing <| character.GetActivityState world) ^ chain {
                    do! update ^ fun world ->
                        let actionDescriptor =
                            match character.GetActivityState world  with
                            | Action actionDescriptor -> actionDescriptor
                            | _ -> failwithumf ()
                        let world = updateCharacterByAction actionDescriptor character world
                        runCharacterReaction actionDescriptor character gameplay world
                    do! pass }}
            let observation = observe TickEventAddress character |> until (DeselectEventAddress ->>- gameplay.ScreenAddress)
            snd <| runAssumingCascade chain observation world

        static let runCharacterNoActivity (character : Entity) world =
            character.SetActivityState NoActivity world

        static let runCharacterActivity newActivity character gameplay world =
            match newActivity with
            | Action newActionDescriptor -> runCharacterAction newActionDescriptor character gameplay world
            | Navigation newNavigationDescriptor -> runCharacterNavigation newNavigationDescriptor character gameplay world
            | NoActivity -> runCharacterNoActivity character world

        static let tryRunEnemyActivity gameplay world newActivity (enemy : Entity) =
            if newActivity <> NoActivity then
                let world = enemy.SetDesiredTurn NoTurn world
                runCharacterActivity newActivity enemy gameplay world
            else world

        static let runEnemyNavigationActivities enemyNavigationActivities enemies gameplay world =
            if Seq.exists ActivityState.isNavigating enemyNavigationActivities
            then Seq.fold2 (tryRunEnemyActivity gameplay) world enemyNavigationActivities enemies
            else world

        static let runEnemyActivities enemyActionActivities enemyNavigationActivities enemies gameplay world =
            let anyEnemyActionActivity = Seq.exists ActivityState.isActing enemyActionActivities
            let newEnemyActivities = if anyEnemyActionActivity then enemyActionActivities else enemyNavigationActivities
            Seq.fold2 (tryRunEnemyActivity gameplay) world newEnemyActivities enemies
            
        static let runPlayerTurn playerTurn gameplay world =

            // construct occupation map
            let occupationMap =
                let field = proxyField gameplay
                let fieldMap = field.GetFieldMapNp world
                let enemies = proxyEnemies gameplay world
                let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions playerTurn

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
                    let player = proxyPlayer gameplay
                    runCharacterActivity newPlayerActivity player gameplay world
                | None -> world

            // determine (and set) enemy desired turns if applicable
            let world =
                // TODO: raise the level of abstraction up here (use addresses only instead of raw entity values)
                match optNewPlayerActivity with
                | Some (Action _)
                | Some (Navigation _) ->
                    let rand = Rand.make <| gameplay.GetOngoingRandState world
                    let player = proxyPlayer gameplay
                    let enemies = proxyEnemies gameplay world
                    let (enemyDesiredTurns, rand) = determineDesiredEnemyTurns occupationMap player enemies rand world
                    let world = Seq.fold2 (fun world (enemy : Entity) turn -> enemy.SetDesiredTurn turn world) world enemies enemyDesiredTurns
                    gameplay.SetOngoingRandState (Rand.getState rand) world
                | Some NoActivity
                | None -> world

            // run enemy activities in accordance with the player's current activity
            let world =
                let player = proxyPlayer gameplay
                let enemies = proxyEnemies gameplay world
                match player.GetActivityState world with
                | Action _ -> world
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActionActivities = determineEnemyActionActivities enemies world
                    let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies world
                    if List.exists ActivityState.isActing newEnemyActionActivities then
                        let world = runEnemyActivities newEnemyActionActivities newEnemyNavigationActivities enemies gameplay world
                        cancelNavigation player world
                    else runEnemyNavigationActivities newEnemyNavigationActivities enemies gameplay world

            // teh world
            world

        static let tryRunPlayerTurn playerInput gameplay world =
            if not <| anyTurnsInProgress gameplay world then
                let hudSaveGame = proxyHudSaveGame gameplay
                let hudHalt = proxyHudHalt gameplay
                let player = proxyPlayer gameplay
                let chain = chain {
                    do! update ^ hudSaveGame.SetEnabled false
                    do! loop 0 inc (fun i world -> i = 0 || anyTurnsInProgress gameplay world) ^ fun i -> chain {
                        let! event = next
                        do! match event.Data with
                            | Right _ -> chain {
                                let! playerTurn =
                                    if i = 0
                                    then getBy <| determinePlayerTurnFromInput playerInput gameplay
                                    else getBy <| determinePlayerTurn gameplay
                                do! update <| runPlayerTurn playerTurn gameplay }
                            | Left _ -> chain {
                                do! update ^ cancelNavigation player }}
                    do! update ^ hudSaveGame.SetEnabled true }
                let observation =
                    observe (ClickEventAddress ->>- hudHalt.EntityAddress) gameplay |>
                    sum TickEventAddress |>
                    until (DeselectEventAddress ->>- gameplay.ScreenAddress)
                snd <| runAssumingCascade chain observation world
            else world

        static let handlePlayerChange event world =
            let gameplay = event.Subscriber : Screen
            let hudHalt = proxyHudHalt gameplay
            let playerNavigatingPath = isPlayerNavigatingPath gameplay world
            let world = hudHalt.SetEnabled playerNavigatingPath world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let playerInput = TouchInput event.Data
            let world = tryRunPlayerTurn playerInput event.Subscriber world
            (Cascade, world)

        static let handleDownDetail direction event world =
            let playerInput = DetailInput direction
            let world = tryRunPlayerTurn playerInput event.Subscriber world
            (Cascade, world)

        static let handleNewGame (gameplay : Screen) world =

            // generate non-deterministic random numbers
            let sysrandom = Random ()
            let contentSeedState = uint64 <| sysrandom.Next ()
            let ongoingSeedState = uint64 <| sysrandom.Next ()

            // initialize gameplay screen
            let world = gameplay.SetContentRandState contentSeedState world
            let world = gameplay.SetOngoingRandState ongoingSeedState world

            // make scene group
            let (scene, world) = World.createGroup typeof<GroupDispatcher>.Name (Some SceneName) gameplay world

            // make rand from gameplay
            let rand = Rand.make <| gameplay.GetContentRandState world

            // make field
            let (rand, world) = _bc <| createField scene rand world

            // make player
            let (player, world) = World.createEntity typeof<PlayerDispatcher>.Name (Some PlayerName) scene world
            let world = player.SetDepth CharacterDepth world

            // make enemies
            __c <| createEnemies scene rand world

        static let handleLoadGame (gameplay : Screen) world =

            // get common proxies
            let scene = proxyScene gameplay

            // get and initialize gameplay screen from read
            let contentRandState = gameplay.GetContentRandState world
            let ongoingRandState = gameplay.GetOngoingRandState world
            let world = snd <| World.readScreenFromFile SaveFilePath (Some GameplayName) world
            let world = gameplay.SetContentRandState contentRandState world
            let world = gameplay.SetOngoingRandState ongoingRandState world
            let world = gameplay.SetTransitionStateNp IncomingState world

            // make rand from gameplay
            let rand = Rand.make <| gameplay.GetContentRandState world

            // make field from rand (field is not serialized, but generated deterministically with ContentRandState)
            __c <| createField scene rand world

        static let handleSelectTitle _ world =
            let world = World.playSong DefaultTimeToFadeOutSongMs 1.0f ButterflyGirlSong world
            (Cascade, world)

        static let handleSelectGameplay event  world =
            let gameplay = event.Subscriber : Screen
            let world =
                // NOTE: doing a File.Exists then loading the file is dangerous since the file can
                // always be deleted / moved between the two operations!
                if gameplay.GetShallLoadGame world && File.Exists SaveFilePath
                then handleLoadGame gameplay world
                else handleNewGame gameplay world
            let world = World.playSong DefaultTimeToFadeOutSongMs 1.0f HerosVengeanceSong world
            (Cascade, world)

        static let handleClickSaveGame event world =
            let gameplay = event.Subscriber
            World.writeScreenToFile SaveFilePath gameplay world
            (Cascade, world)

        static let handleDeselectGameplay event world =
            let scene = proxyScene event.Subscriber
            let world = World.destroyGroup scene world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? ShallLoadGame false]

        override dispatcher.Register gameplay world =
            world |>
                (observe (EntityChangeEventAddress ->>- (proxyPlayer gameplay).EntityAddress) gameplay |> subscribe handlePlayerChange) |>
                (observe (TouchEventAddress ->>- (proxyHudFeeler gameplay).EntityAddress) gameplay |> filter isObserverSelected |> monitor handleTouchFeeler) |>
                (observe (DownEventAddress ->>- (proxyHudDetailUp gameplay).EntityAddress) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Upward)) |>
                (observe (DownEventAddress ->>- (proxyHudDetailRight gameplay).EntityAddress) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Rightward)) |>
                (observe (DownEventAddress ->>- (proxyHudDetailDown gameplay).EntityAddress) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Downward)) |>
                (observe (DownEventAddress ->>- (proxyHudDetailLeft gameplay).EntityAddress) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Leftward)) |>
                (World.subscribe4 handleSelectTitle (SelectEventAddress ->>- Title.ScreenAddress) gameplay) |>
                (World.subscribe4 handleSelectGameplay (SelectEventAddress ->>- gameplay.ScreenAddress) gameplay) |>
                (World.subscribe4 handleClickSaveGame (ClickEventAddress ->>- (proxyHudSaveGame gameplay).EntityAddress) gameplay) |>
                (World.subscribe4 handleDeselectGameplay (DeselectEventAddress ->>- gameplay.ScreenAddress) gameplay)