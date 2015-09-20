namespace InfinityRpg
open System
open System.IO
open SDL2
open OpenTK
open Prime
open Nu
open Nu.Observation
open Nu.Chain
open AStar
open InfinityRpg

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

        static let proxyCharacters world =
            let entities = World.proxyEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs typeof<CharacterDispatcher> world) entities

        static let proxyOptCharacterAtPosition position world =
            let characters = proxyCharacters world
            Seq.tryFind (fun (character : Entity) -> character.GetPosition world = position) characters
        
        static let proxyOptCharacterInDirection position direction world =
            proxyOptCharacterAtPosition (position + dtovf direction) world
        
        static let proxyCharacterInDirection position direction world =
            Option.get ^ proxyOptCharacterInDirection position direction world

        static let proxyEnemies world =
            let entities = World.proxyEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs typeof<EnemyDispatcher> world) entities

        static let makeAttackTurn targetPositionM =
            ActionTurn
                { ActionTicks = 0L
                  ActionOptTargetPositionM = Some targetPositionM
                  ActionDataName = Constants.InfinityRpg.AttackName }

        static let createField scene rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make Constants.Assets.FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let (field, world) = World.createEntity typeof<FieldDispatcher>.Name None (Some ^ Simulants.Field.EntityName) scene world
            let world = field.SetFieldMapNp fieldMap world
            let world = field.SetSize (World.getEntityQuickSize field world) world
            let world = field.SetPersistent false world
            (field, rand, world)

        static let createEnemies scene rand world =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 1
            List.fold
                (fun (enemies, rand, world) i ->
                    let enemyPosition = single i * Constants.Layout.TileSize * 2.0f
                    let (enemy, world) = World.createEntity typeof<EnemyDispatcher>.Name None None scene world
                    let world = enemy.SetDepth Constants.Layout.CharacterDepth world
                    let world = enemy.SetPosition enemyPosition world
                    let world = enemy.SetCharacterAnimationSheet Constants.Assets.GoopyImage world
                    (enemy :: enemies, rand, world))
                ([], rand, world)
                [0 .. enemyCount - 1]

        static let walk3 positive current destination =
            let walkSpeed = if positive then Constants.Layout.CharacterWalkSpeed else -Constants.Layout.CharacterWalkSpeed
            let next = current + walkSpeed
            let delta = if positive then destination - next else next - destination
            if delta < Constants.Layout.CharacterWalkSpeed then (destination, WalkFinished) else (next, WalkContinuing)

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
            let currentNode = Map.find (vftovm ^ character.GetPosition world) nodes
            let optNavigationPath =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun n -> 0.0f))
            match optNavigationPath with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let isPlayerNavigatingPath world =
            ActivityState.isNavigatingPath ^ Simulants.Player.GetActivityState world

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

        static let anyTurnsInProgress world =
            let enemies = proxyEnemies world
            anyTurnsInProgress2 Simulants.Player enemies world

        static let updateCharacterByWalk walkDescriptor (character : Entity) world =
            let (newPosition, walkState) = walk walkDescriptor ^ character.GetPosition world
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
                    let walkDirection = vmtod ^ (List.head navigationPath).PositionM - currentNode.PositionM
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm ^ character.GetPosition world }
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
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionBegin (World.getTickTime world) (character.GetPosition world) (character.GetCharacterAnimationState world) actionDescriptor) |>
                    character.SetActivityState (Action ^ ActionDescriptor.updateActionTicks (World.getTickRate world) actionDescriptor)
            elif actionDescriptor.ActionTicks > 0L && actionDescriptor.ActionTicks < Constants.InfinityRpg.ActionTicksMax then
                world |>
                    character.SetActivityState (Action ^ ActionDescriptor.updateActionTicks (World.getTickRate world) actionDescriptor)
            else
                world |>
                    character.SetActivityState NoActivity |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionEnd (World.getTickTime world) (character.GetCharacterAnimationState world))

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents world =
            match character.GetActivityState world with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM (vftovm ^ character.GetPosition world) occupationMap
                if Set.contains direction openDirections then
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = vftovm ^ character.GetPosition world }
                    NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = None }
                else
                    let targetPosition = character.GetPosition world + dtovf direction
                    if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                    then makeAttackTurn ^ vftovm targetPosition
                    else NoTurn

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents world =
            if character.GetActivityState world = NoActivity then
                match tryGetNavigationPath touchPosition occupationMap character world with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | _ ->
                        let characterPositionM = vftovm ^ character.GetPosition world
                        let walkDirection = vmtod ^ (List.head navigationPath).PositionM - characterPositionM
                        let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                        NavigationTurn { WalkDescriptor = walkDescriptor; OptNavigationPath = Some navigationPath }
                | None ->
                    let targetPosition = touchPosition |> vftovm |> vmtovf
                    if Math.arePositionsAdjacent targetPosition ^ character.GetPosition world then
                        if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                        then makeAttackTurn ^ vftovm targetPosition
                        else NoTurn
                    else NoTurn
            else NoTurn

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) rand world =
            match enemy.GetControlType world with
            | Player ->
                debug ^ "Invalid ControlType '" + acstring (enemy.GetControlType world) + "' for enemy"
                (NoTurn, rand)
            | Chaos ->
                let nextPlayerPosition =
                    match player.GetActivityState world with
                    | Action _ -> player.GetPosition world
                    | Navigation navigationDescriptor -> NavigationDescriptor.nextPosition navigationDescriptor
                    | NoActivity -> player.GetPosition world
                if Math.arePositionsAdjacent (enemy.GetPosition world) nextPlayerPosition then
                    let enemyTurn = makeAttackTurn ^ vftovm nextPlayerPosition
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

        static let determinePlayerTurnFromTouch touchPosition world =
            let fieldMap = Simulants.Field.GetFieldMapNp world
            let enemies = proxyEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not ^ anyTurnsInProgress2 Simulants.Player enemies world then
                let touchPositionW = World.getCameraBy (Camera.mouseToWorld Relative touchPosition) world
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm ^ Simulants.Player.GetPosition world) fieldMap.FieldTiles enemyPositions
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies Simulants.Player enemies world with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.OptNavigationPath |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction world =
            let fieldMap = Simulants.Field.GetFieldMapNp world
            let enemies = proxyEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not ^ anyTurnsInProgress2 Simulants.Player enemies world then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                determineCharacterTurnFromDirection direction occupationMapWithEnemies Simulants.Player enemies world
            else NoTurn

        static let determinePlayerTurnFromInput playerInput world =
            match playerInput with
            | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition world
            | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction world
            | NoInput -> NoTurn

        static let determinePlayerTurn world =
            match Simulants.Player.GetActivityState world with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if Simulants.Player.GetPosition world = vmtovf walkDescriptor.WalkOriginM then
                    let fieldMap = Simulants.Field.GetFieldMapNp world
                    let enemies = proxyEnemies world
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
                        Seq.notExists (fun (enemy : Entity) -> ActivityState.isActing ^ enemy.GetActivityState world) enemies
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

        static let runCharacterReaction actionDescriptor (initiator : Entity) world =
            // TODO: implement animations
            if actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax then
                let reactor =
                    proxyCharacterInDirection
                        (initiator.GetPosition world)
                        (initiator.GetCharacterAnimationState world).Direction
                        world
                let reactorDamage = initiator.GetPowerBuff world * 5.0f - reactor.GetShieldBuff world |> int
                let reactorHitPoints = reactor.GetHitPoints world - reactorDamage
                let world = reactor.SetHitPoints reactorHitPoints world
                if reactor.GetHitPoints world <= 0 then
                    if reactor.GetName world = Simulants.Player.EntityName
                    then World.transitionScreen Simulants.Title world
                    else World.destroyEntity reactor world
                else world
            else world

        static let runCharacterNavigation newNavigationDescriptor (character : Entity) world =
            let chain = chain {
                do! update ^ character.SetActivityState ^ Navigation newNavigationDescriptor
                do! during (fun world ->
                    match character.GetActivityState world with
                    | Navigation navigationDescriptor -> newNavigationDescriptor.WalkDescriptor.WalkOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM
                    | Action _ -> false
                    | NoActivity -> false) ^ chain {
                    do! update ^ fun world ->
                        let navigationDescriptor =
                            match character.GetActivityState world with
                            | Navigation navigationDescriptor -> navigationDescriptor
                            | _ -> failwith "Unexpected match failure in InfinityRpg.GameplayDispatcherModule.runCharacterNavigation."
                        updateCharacterByNavigation navigationDescriptor character world
                    do! pass }}
            let observation = character |> observe Events.Update |> until (Events.Deselect ->- Simulants.Gameplay)
            runAssumingCascade chain observation world |> snd

        static let runCharacterAction newActionDescriptor (character : Entity) world =
            // NOTE: currently just implements attack
            let chain = chain {
                do! update ^ character.SetActivityState ^ Action newActionDescriptor
                do! during (ActivityState.isActing << character.GetActivityState) ^ chain {
                    do! update ^ fun world ->
                        let actionDescriptor =
                            match character.GetActivityState world with
                            | Action actionDescriptor -> actionDescriptor
                            | _ -> failwithumf ()
                        let world = updateCharacterByAction actionDescriptor character world
                        runCharacterReaction actionDescriptor character world
                    do! pass }}
            let observation = character |> observe Events.Update |> until (Events.Deselect ->- Simulants.Gameplay)
            runAssumingCascade chain observation world |> snd

        static let runCharacterNoActivity (character : Entity) world =
            character.SetActivityState NoActivity world

        static let runCharacterActivity newActivity character world =
            match newActivity with
            | Action newActionDescriptor -> runCharacterAction newActionDescriptor character world
            | Navigation newNavigationDescriptor -> runCharacterNavigation newNavigationDescriptor character world
            | NoActivity -> runCharacterNoActivity character world

        static let tryRunEnemyActivity world newActivity (enemy : Entity) =
            if newActivity <> NoActivity then
                let world = enemy.SetDesiredTurn NoTurn world
                runCharacterActivity newActivity enemy world
            else world

        static let runEnemyNavigationActivities enemyNavigationActivities enemies world =
            if Seq.exists ActivityState.isNavigating enemyNavigationActivities
            then Seq.fold2 tryRunEnemyActivity world enemyNavigationActivities enemies
            else world

        static let runEnemyActivities enemyActionActivities enemyNavigationActivities enemies world =
            let anyEnemyActionActivity = Seq.exists ActivityState.isActing enemyActionActivities
            let newEnemyActivities = if anyEnemyActionActivity then enemyActionActivities else enemyNavigationActivities
            Seq.fold2 tryRunEnemyActivity world newEnemyActivities enemies
            
        static let runPlayerTurn playerTurn world =

            // construct occupation map
            let occupationMap =
                let fieldMap = Simulants.Field.GetFieldMapNp world
                let enemies = proxyEnemies world
                let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions playerTurn

            // determine player activity
            let optNewPlayerActivity =
                match playerTurn with
                | ActionTurn actionDescriptor -> Some ^ Action actionDescriptor
                | NavigationTurn navigationDescriptor -> Some ^ Navigation navigationDescriptor
                | CancelTurn -> Some NoActivity
                | NoTurn -> None

            // run player activity
            let world =
                match optNewPlayerActivity with
                | Some newPlayerActivity -> runCharacterActivity newPlayerActivity Simulants.Player world
                | None -> world

            // determine (and set) enemy desired turns if applicable
            let world =
                match optNewPlayerActivity with
                | Some (Action _)
                | Some (Navigation _) ->
                    let rand = Rand.make ^ Simulants.Gameplay.GetOngoingRandState world
                    let enemies = proxyEnemies world
                    let (enemyDesiredTurns, rand) = determineDesiredEnemyTurns occupationMap Simulants.Player enemies rand world
                    let world = Seq.fold2 (fun world (enemy : Entity) turn -> enemy.SetDesiredTurn turn world) world enemies enemyDesiredTurns
                    Simulants.Gameplay.SetOngoingRandState (Rand.getState rand) world
                | Some NoActivity
                | None -> world

            // run enemy activities in accordance with the player's current activity
            let world =
                let enemies = proxyEnemies world
                match Simulants.Player.GetActivityState world with
                | Action _ -> world
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActionActivities = determineEnemyActionActivities enemies world
                    let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies world
                    if List.exists ActivityState.isActing newEnemyActionActivities then
                        let world = runEnemyActivities newEnemyActionActivities newEnemyNavigationActivities enemies world
                        cancelNavigation Simulants.Player world
                    else runEnemyNavigationActivities newEnemyNavigationActivities enemies world

            // teh world
            world

        static let tryRunPlayerTurn playerInput world =
            if not ^ anyTurnsInProgress world then
                let chain = chain {
                    do! update ^ Simulants.HudSaveGame.SetEnabled false
                    do! loop 0 inc (fun i world -> i = 0 || anyTurnsInProgress world) ^ fun i -> chain {
                        let! event = next
                        do! match event.Data with
                            | Right _ -> chain {
                                let! playerTurn =
                                    if i = 0
                                    then getBy ^ determinePlayerTurnFromInput playerInput
                                    else getBy ^ determinePlayerTurn
                                do! update ^ runPlayerTurn playerTurn }
                            | Left _ -> chain {
                                do! update ^ cancelNavigation Simulants.Player }}
                    do! update ^ Simulants.HudSaveGame.SetEnabled true }
                let observation =
                    Simulants.Gameplay |>
                    observe (Events.Click ->- Simulants.HudHalt) |>
                    sum Events.Update |>
                    until (Events.Deselect ->- Simulants.Gameplay)
                runAssumingCascade chain observation world |> snd
            else world

        static let handlePlayerChange _ world =
            let playerNavigatingPath = isPlayerNavigatingPath world
            let world = Simulants.HudHalt.SetEnabled playerNavigatingPath world
            (Cascade, world)

        static let handleTouchFeeler event world =
            let playerInput = TouchInput event.Data
            let world = tryRunPlayerTurn playerInput world
            (Cascade, world)

        static let handleDownDetail direction _ world =
            let playerInput = DetailInput direction
            let world = tryRunPlayerTurn playerInput world
            (Cascade, world)

        static let handleNewGame world =

            // generate non-deterministic random numbers
            let sysrandom = Random ()
            let contentSeedState = uint64 ^ sysrandom.Next ()
            let ongoingSeedState = uint64 ^ sysrandom.Next ()

            // initialize gameplay screen
            let world = Simulants.Gameplay.SetContentRandState contentSeedState world
            let world = Simulants.Gameplay.SetOngoingRandState ongoingSeedState world

            // make scene group
            let (scene, world) = World.createGroup typeof<GroupDispatcher>.Name None (Some ^ Simulants.Scene.GroupName) Simulants.Gameplay world

            // make rand from gameplay
            let rand = Rand.make ^ Simulants.Gameplay.GetContentRandState world

            // make field
            let (rand, world) = _bc ^ createField scene rand world

            // make player
            let (player, world) = World.createEntity typeof<PlayerDispatcher>.Name None (Some ^ Simulants.Player.EntityName) scene world
            let world = player.SetDepth Constants.Layout.CharacterDepth world

            // make enemies
            __c ^ createEnemies scene rand world

        static let handleLoadGame world =

            // get and initialize gameplay screen from read
            let world = World.readScreenFromFile Constants.FilePaths.SaveFile (Some ^ Simulants.Gameplay.ScreenName) world |> snd
            let world = Simulants.Gameplay.SetTransitionStateNp IncomingState world

            // make rand from gameplay
            let rand = Rand.make ^ Simulants.Gameplay.GetContentRandState world

            // make field from rand (field is not serialized, but generated deterministically with ContentRandState)
            __c ^ createField Simulants.Scene rand world

        static let handleSelectTitle _ world =
            let world = World.playSong Constants.Audio.DefaultTimeToFadeOutSongMs 1.0f Constants.Assets.ButterflyGirlSong world
            (Cascade, world)

        static let handleSelectGameplay _ world =
            let world =
                // NOTE: doing a File.Exists then loading the file is dangerous since the file can
                // always be deleted / moved between the two operations!
                if Simulants.Gameplay.GetShallLoadGame world && File.Exists Constants.FilePaths.SaveFile
                then handleLoadGame world
                else handleNewGame world
            let world = World.playSong Constants.Audio.DefaultTimeToFadeOutSongMs 1.0f Constants.Assets.HerosVengeanceSong world
            (Cascade, world)

        static let handleClickSaveGame event world =
            let gameplay = event.Subscriber
            World.writeScreenToFile Constants.FilePaths.SaveFile gameplay world
            (Cascade, world)

        static let handleDeselectGameplay _ world =
            let world = World.destroyGroup Simulants.Scene world
            (Cascade, world)

        static member FieldDefinitions =
            [define? ContentRandState Rand.DefaultSeedState
             define? OngoingRandState Rand.DefaultSeedState
             define? ShallLoadGame false]

        override dispatcher.Register (gameplay, world) =
            world |>
                (observe (Events.EntityChange ->- Simulants.Player) gameplay |> subscribe handlePlayerChange) |>
                (observe (Events.Touch ->- Simulants.HudFeeler) gameplay |> filter isObserverSelected |> monitor handleTouchFeeler) |>
                (observe (Events.Down ->- Simulants.HudDetailUp) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Upward)) |>
                (observe (Events.Down ->- Simulants.HudDetailRight) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Rightward)) |>
                (observe (Events.Down ->- Simulants.HudDetailDown) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Downward)) |>
                (observe (Events.Down ->- Simulants.HudDetailLeft) gameplay |> filter isObserverSelected |> monitor (handleDownDetail Leftward)) |>
                (World.subscribe handleSelectTitle (Events.Select ->- Simulants.Title) gameplay) |>
                (World.subscribe handleSelectGameplay (Events.Select ->- gameplay) gameplay) |>
                (World.subscribe handleClickSaveGame (Events.Click ->- Simulants.HudSaveGame) gameplay) |>
                (World.subscribe handleDeselectGameplay (Events.Deselect ->- gameplay) gameplay)