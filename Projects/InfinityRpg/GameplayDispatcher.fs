namespace InfinityRpg
open System
open System.IO
open OpenTK
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcherModule =

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type Screen with

        member this.GetContentRandState = this.Get Property? ContentRandState
        member this.SetContentRandState = this.Set Property? ContentRandState
        member this.ContentRandState = Lens.make<uint64, World> Property? ContentRandState this.GetContentRandState this.SetContentRandState this
        member this.GetOngoingRandState = this.Get Property? OngoingRandState
        member this.SetOngoingRandState = this.Set Property? OngoingRandState
        member this.OngoingRandState = Lens.make<uint64, World> Property? OngoingRandState this.GetOngoingRandState this.SetOngoingRandState this
        member this.GetShallLoadGame = this.Get Property? ShallLoadGame
        member this.SetShallLoadGame = this.Set Property? ShallLoadGame
        member this.ShallLoadGame = Lens.make<bool, World> Property? ShallLoadGame this.GetShallLoadGame this.SetShallLoadGame this

    type GameplayDispatcher () =
        inherit ScreenDispatcher ()

        static let getCharacters world =
            let entities = World.getEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs<CharacterDispatcher> world) entities

        static let tryGetCharacterAtPosition position world =
            let characters = getCharacters world
            Seq.tryFind (fun (character : Entity) -> character.GetPosition world = position) characters
        
        static let tryGetCharacterInDirection position direction world =
            tryGetCharacterAtPosition (position + dtovf direction) world
        
        static let getCharacterInDirection position direction world =
            Option.get (tryGetCharacterInDirection position direction world)

        static let getEnemies world =
            let entities = World.getEntities Simulants.Scene world
            Seq.filter (fun (entity : Entity) -> entity.DispatchesAs<EnemyDispatcher> world) entities

        static let makeAttackTurn targetPositionM =
            ActionTurn
                { ActionTicks = 0L
                  ActionTargetPositionMOpt = Some targetPositionM
                  ActionDataName = Constants.InfinityRpg.AttackName }

        static let createField scene rand world =
            let pathEdgesM = [(Vector2i (1, 10), Vector2i (20, 10))]
            let (fieldMap, rand) = FieldMap.make Assets.FieldTileSheetImage (Vector2i 22) pathEdgesM rand
            let (field, world) = World.createEntity<FieldDispatcher> (Some Simulants.Field.EntityName) DefaultOverlay scene world
            let world = field.SetFieldMapNp fieldMap world
            let world = field.SetSize (field.GetQuickSize world) world
            let world = field.SetPersistent false world
            (field, rand, world)

        static let createEnemies scene rand world =
            let (randResult, rand) = Rand.nextIntUnder 5 rand
            let enemyCount = randResult + 1
            List.fold
                (fun (enemies, rand, world) i ->
                    let enemyPosition = single i * Constants.Layout.TileSize * 2.0f
                    let (enemy, world) = World.createEntity<EnemyDispatcher> None DefaultOverlay scene world
                    let world = enemy.SetPosition enemyPosition world
                    let world = enemy.SetDepth Constants.Layout.CharacterDepth world
                    let world = enemy.SetCharacterAnimationSheet Assets.GoopyImage world
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

        static let getCharacterAnimationStateByActionBegin tickTime characterPosition characterAnimationState (actionDescriptor : ActionDescriptor) =
            let currentDirection = characterAnimationState.Direction
            let direction = actionDescriptor.ComputeActionDirection characterPosition currentDirection
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
            let currentNode = Map.find (vftovm (character.GetPosition world)) nodes
            let navigationPathOpt =
                AStar.FindPath (
                    currentNode,
                    goalNode,
                    (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                    (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        static let isPlayerNavigatingPath world =
            (Simulants.Player.GetCharacterActivityState world).IsNavigatingPath

        static let cancelNavigation (character : Entity) world =
            let characterActivity =
                match character.GetCharacterActivityState world with
                | Action _ as action -> action
                | NoActivity -> NoActivity
                | Navigation navDescriptor -> Navigation { navDescriptor with NavigationPathOpt = None }
            character.SetCharacterActivityState characterActivity world

        static let anyTurnsInProgress2 (player : Entity) enemies world =
            player.GetCharacterActivityState world <> NoActivity ||
            Seq.exists
                (fun (enemy : Entity) -> enemy.GetDesiredTurn world <> NoTurn || enemy.GetCharacterActivityState world <> NoActivity)
                enemies

        static let anyTurnsInProgress world =
            let enemies = getEnemies world
            anyTurnsInProgress2 Simulants.Player enemies world

        static let updateCharacterByWalk walkDescriptor (character : Entity) world =
            let (newPosition, walkState) = walk walkDescriptor (character.GetPosition world)
            let world = character.SetPosition newPosition world
            let characterAnimationState = { character.GetCharacterAnimationState world with Direction = walkDescriptor.WalkDirection }
            let world = character.SetCharacterAnimationState characterAnimationState world
            (walkState, world)

        static let updateCharacterByWalkState walkState navigationDescriptor (character : Entity) world =
            match walkState with
            | WalkFinished ->
                match navigationDescriptor.NavigationPathOpt with
                | Some [] -> failwith "NavigationPath should never be empty here."
                | Some (_ :: []) -> character.SetCharacterActivityState NoActivity world
                | Some (currentNode :: navigationPath) ->
                    let walkDirection = vmtod ((List.head navigationPath).PositionM - currentNode.PositionM)
                    let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = vftovm (character.GetPosition world) }
                    let navigationDescriptor = { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath }
                    character.SetCharacterActivityState (Navigation navigationDescriptor) world
                | None -> character.SetCharacterActivityState NoActivity world
            | WalkContinuing -> world

        static let updateCharacterByNavigation navigationDescriptor character world =
            let (walkState, world) = updateCharacterByWalk navigationDescriptor.WalkDescriptor character world
            updateCharacterByWalkState walkState navigationDescriptor character world

        static let updateCharacterByAction actionDescriptor (character : Entity) world =
            if actionDescriptor.ActionTicks = 0L then
                world |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionBegin (World.getTickTime world) (character.GetPosition world) (character.GetCharacterAnimationState world) actionDescriptor) |>
                    character.SetCharacterActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks })
            elif actionDescriptor.ActionTicks > 0L && actionDescriptor.ActionTicks < Constants.InfinityRpg.ActionTicksMax then
                world |>
                    character.SetCharacterActivityState (Action { actionDescriptor with ActionTicks = inc actionDescriptor.ActionTicks })
            else
                world |>
                    character.SetCharacterActivityState NoActivity |>
                    character.SetCharacterAnimationState (getCharacterAnimationStateByActionEnd (World.getTickTime world) (character.GetCharacterAnimationState world))

        static let determineCharacterTurnFromDirection direction occupationMap (character : Entity) opponents world =
            match character.GetCharacterActivityState world with
            | Action _ -> NoTurn
            | Navigation _ -> NoTurn
            | NoActivity ->
                let openDirections = OccupationMap.getOpenDirectionsAtPositionM (vftovm (character.GetPosition world)) occupationMap
                if Set.contains direction openDirections then
                    let walkDescriptor = { WalkDirection = direction; WalkOriginM = vftovm (character.GetPosition world) }
                    NavigationTurn { WalkDescriptor = walkDescriptor; NavigationPathOpt = None }
                else
                    let targetPosition = character.GetPosition world + dtovf direction
                    if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                    then makeAttackTurn (vftovm targetPosition)
                    else NoTurn

        static let determineCharacterTurnFromTouch touchPosition occupationMap (character : Entity) opponents world =
            if character.GetCharacterActivityState world = NoActivity then
                match tryGetNavigationPath touchPosition occupationMap character world with
                | Some navigationPath ->
                    match navigationPath with
                    | [] -> NoTurn
                    | _ ->
                        let characterPositionM = vftovm (character.GetPosition world)
                        let walkDirection = vmtod ((List.head navigationPath).PositionM - characterPositionM)
                        let walkDescriptor = { WalkDirection = walkDirection; WalkOriginM = characterPositionM }
                        NavigationTurn { WalkDescriptor = walkDescriptor; NavigationPathOpt = Some navigationPath }
                | None ->
                    let targetPosition = touchPosition |> vftovm |> vmtovf
                    if Math.arePositionsAdjacent targetPosition (character.GetPosition world) then
                        if Seq.exists (fun (opponent : Entity) -> opponent.GetPosition world = targetPosition) opponents
                        then makeAttackTurn (vftovm targetPosition)
                        else NoTurn
                    else NoTurn
            else NoTurn

        static let determineDesiredEnemyTurn occupationMap (player : Entity) (enemy : Entity) rand world =
            match (enemy.GetCharacterState world).ControlType with
            | PlayerControlled as controlType ->
                Log.debug ("Invalid ControlType '" + scstring controlType + "' for enemy.")
                (NoTurn, rand)
            | Chaos ->
                let nextPlayerPosition =
                    match player.GetCharacterActivityState world with
                    | Action _ -> player.GetPosition world
                    | Navigation navigationDescriptor -> navigationDescriptor.NextPosition
                    | NoActivity -> player.GetPosition world
                if Math.arePositionsAdjacent (enemy.GetPosition world) nextPlayerPosition then
                    let enemyTurn = makeAttackTurn (vftovm nextPlayerPosition)
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
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let touchPositionW = World.mouseToWorld Relative touchPosition world
                let occupationMapWithAdjacentEnemies =
                    OccupationMap.makeFromFieldTilesAndAdjacentCharacters
                        (vftovm (Simulants.Player.GetPosition world))
                        fieldMap.FieldTiles
                        enemyPositions
                match determineCharacterTurnFromTouch touchPositionW occupationMapWithAdjacentEnemies Simulants.Player enemies world with
                | ActionTurn _ as actionTurn -> actionTurn
                | NavigationTurn navigationDescriptor as navigationTurn ->
                    let headNavigationNode = navigationDescriptor.NavigationPathOpt |> Option.get |> List.head
                    let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                    if Map.find headNavigationNode.PositionM occupationMapWithEnemies then CancelTurn
                    else navigationTurn
                | CancelTurn -> CancelTurn
                | NoTurn -> NoTurn
            else NoTurn

        static let determinePlayerTurnFromDetailNavigation direction world =
            let fieldMap = Simulants.Field.GetFieldMapNp world
            let enemies = getEnemies world
            let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
            if not (anyTurnsInProgress2 Simulants.Player enemies world) then
                let occupationMapWithEnemies = OccupationMap.makeFromFieldTilesAndCharacters fieldMap.FieldTiles enemyPositions
                determineCharacterTurnFromDirection direction occupationMapWithEnemies Simulants.Player enemies world
            else NoTurn

        static let determinePlayerTurnFromInput playerInput world =
            match playerInput with
            | TouchInput touchPosition -> determinePlayerTurnFromTouch touchPosition world
            | DetailInput direction -> determinePlayerTurnFromDetailNavigation direction world
            | NoInput -> NoTurn

        static let determinePlayerTurn world =
            match Simulants.Player.GetCharacterActivityState world with
            | Action _ -> NoTurn
            | Navigation navigationDescriptor ->
                let walkDescriptor = navigationDescriptor.WalkDescriptor
                if Simulants.Player.GetPosition world = vmtovf walkDescriptor.WalkOriginM then
                    let fieldMap = Simulants.Field.GetFieldMapNp world
                    let enemies = getEnemies world
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
                        let noPrecedingEnemyActionActivity = Seq.notExists (fun (state : CharacterActivityState) -> state.IsActing) precedingEnemyActivities
                        let noCurrentEnemyActionActivity = Seq.notExists (fun (enemy : Entity) -> (enemy.GetCharacterActivityState world).IsActing) enemies
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
                        Seq.notExists (fun (enemy : Entity) -> (enemy.GetCharacterActivityState world).IsActing) enemies
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
                    getCharacterInDirection
                        (initiator.GetPosition world)
                        (initiator.GetCharacterAnimationState world).Direction
                        world
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let world = reactor.CharacterState.Update (fun state -> { state with HitPoints = state.HitPoints - reactorDamage }) world
                if reactor.CharacterState.GetBy (fun state -> state.HitPoints <= 0) world then
                    if reactor.GetName world = Simulants.Player.EntityName
                    then World.transitionScreen (!! "Title") world
                    else World.destroyEntity reactor world
                else world
            else world

        static let runCharacterNavigation newNavigationDescriptor (character : Entity) world =
            let chain = chain {
                do! Chain.update (character.SetCharacterActivityState (Navigation newNavigationDescriptor))
                do! Chain.during (fun world ->
                    match character.GetCharacterActivityState world with
                    | Navigation navigationDescriptor -> newNavigationDescriptor.WalkDescriptor.WalkOriginM = navigationDescriptor.WalkDescriptor.WalkOriginM
                    | Action _ -> false
                    | NoActivity -> false) $ chain {
                    do! Chain.update $ fun world ->
                        let navigationDescriptor =
                            match character.GetCharacterActivityState world with
                            | Navigation navigationDescriptor -> navigationDescriptor
                            | _ -> failwithumf ()
                        updateCharacterByNavigation navigationDescriptor character world
                    do! Chain.pass }}
            let stream =
                Stream.until
                    (Stream.make Simulants.Gameplay.DeselectEvent)
                    (Stream.make character.UpdateEvent)
            Chain.runAssumingCascade chain stream world |> snd

        static let runCharacterAction newActionDescriptor (character : Entity) world =
            // NOTE: currently just implements attack
            let chain = chain {
                do! Chain.update (character.SetCharacterActivityState (Action newActionDescriptor))
                do! Chain.during (character.CharacterActivityState.GetBy (fun state -> state.IsActing)) $ chain {
                    do! Chain.update $ fun world ->
                        let actionDescriptor =
                            match character.GetCharacterActivityState world with
                            | Action actionDescriptor -> actionDescriptor
                            | _ -> failwithumf ()
                        let world = updateCharacterByAction actionDescriptor character world
                        runCharacterReaction actionDescriptor character world
                    do! Chain.pass }}
            let stream =
                Stream.until
                    (Stream.make Simulants.Gameplay.DeselectEvent)
                    (Stream.make character.UpdateEvent)
            Chain.runAssumingCascade chain stream world |> snd

        static let runCharacterNoActivity (character : Entity) world =
            character.SetCharacterActivityState NoActivity world

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
            if Seq.exists (fun (state : CharacterActivityState) -> state.IsNavigating) enemyNavigationActivities
            then Seq.fold2 tryRunEnemyActivity world enemyNavigationActivities enemies
            else world

        static let runEnemyActivities enemyActionActivities enemyNavigationActivities enemies world =
            let anyEnemyActionActivity = Seq.exists (fun (state : CharacterActivityState) -> state.IsActing) enemyActionActivities
            let newEnemyActivities = if anyEnemyActionActivity then enemyActionActivities else enemyNavigationActivities
            Seq.fold2 tryRunEnemyActivity world newEnemyActivities enemies
            
        static let runPlayerTurn playerTurn world =

            // construct occupation map
            let occupationMap =
                let fieldMap = Simulants.Field.GetFieldMapNp world
                let enemies = getEnemies world
                let enemyPositions = Seq.map (fun (enemy : Entity) -> enemy.GetPosition world) enemies
                OccupationMap.makeFromFieldTilesAndCharactersAndDesiredTurn fieldMap.FieldTiles enemyPositions playerTurn

            // determine player activity
            let newPlayerActivityOpt =
                match playerTurn with
                | ActionTurn actionDescriptor -> Some (Action actionDescriptor)
                | NavigationTurn navigationDescriptor -> Some (Navigation navigationDescriptor)
                | CancelTurn -> Some NoActivity
                | NoTurn -> None

            // run player activity
            let world =
                match newPlayerActivityOpt with
                | Some newPlayerActivity -> runCharacterActivity newPlayerActivity Simulants.Player world
                | None -> world

            // determine (and set) enemy desired turns if applicable
            let world =
                match newPlayerActivityOpt with
                | Some (Action _)
                | Some (Navigation _) ->
                    let rand = Rand.makeFromSeedState (Simulants.Gameplay.GetOngoingRandState world)
                    let enemies = getEnemies world
                    let (enemyDesiredTurns, rand) = determineDesiredEnemyTurns occupationMap Simulants.Player enemies rand world
                    let world = Seq.fold2 (fun world (enemy : Entity) turn -> enemy.SetDesiredTurn turn world) world enemies enemyDesiredTurns
                    Simulants.Gameplay.SetOngoingRandState (Rand.getState rand) world
                | Some NoActivity
                | None -> world

            // run enemy activities in accordance with the player's current activity
            let world =
                let enemies = getEnemies world
                match Simulants.Player.GetCharacterActivityState world with
                | Action _ -> world
                | Navigation _ 
                | NoActivity ->
                    let newEnemyActionActivities = determineEnemyActionActivities enemies world
                    let newEnemyNavigationActivities = determineEnemyNavigationActivities enemies world
                    if List.exists (fun (state : CharacterActivityState) -> state.IsActing) newEnemyActionActivities then
                        let world = runEnemyActivities newEnemyActionActivities newEnemyNavigationActivities enemies world
                        cancelNavigation Simulants.Player world
                    else runEnemyNavigationActivities newEnemyNavigationActivities enemies world

            // fin
            world

        static let tryRunPlayerTurn playerInput world =
            if not (anyTurnsInProgress world) then
                let chain = chain {
                    do! Chain.update $ Simulants.HudSaveGame.SetEnabled false
                    do! Chain.loop 0 inc (fun i world -> i = 0 || anyTurnsInProgress world) $ fun i -> chain {
                        let! evt = Chain.next
                        do! match evt.Data with
                            | Right _ -> chain {
                                let! playerTurn =
                                    if i = 0
                                    then Chain.getBy (determinePlayerTurnFromInput playerInput)
                                    else Chain.getBy determinePlayerTurn
                                do! Chain.update (runPlayerTurn playerTurn) }
                            | Left _ -> chain {
                                do! Chain.update (cancelNavigation Simulants.Player) }}
                    do! Chain.update (Simulants.HudSaveGame.SetEnabled true) }
                let stream =
                    Stream.until
                        (Stream.make Simulants.Gameplay.DeselectEvent)
                        (Stream.sum
                            (Stream.make Simulants.HudHalt.ClickEvent)
                            (Stream.make Simulants.Player.UpdateEvent))
                Chain.runAssumingCascade chain stream world |> snd
            else world

        static let handlePlayerActivityChange _ world =
            let playerNavigatingPath = isPlayerNavigatingPath world
            Simulants.HudHalt.SetEnabled playerNavigatingPath world

        static let handleTouchFeeler evt world =
            let playerInput = TouchInput evt.Data
            tryRunPlayerTurn playerInput world

        static let handleDownDetail direction _ world =
            let playerInput = DetailInput direction
            tryRunPlayerTurn playerInput world

        static let handleNewGame world =

            // generate non-deterministic random numbers
            let sysrandom = System.Random ()
            let contentSeedState = uint64 (sysrandom.Next ())
            let ongoingSeedState = uint64 (sysrandom.Next ())

            // initialize gameplay screen
            let world = Simulants.Gameplay.SetContentRandState contentSeedState world
            let world = Simulants.Gameplay.SetOngoingRandState ongoingSeedState world

            // make scene layer
            let (scene, world) = World.createLayer (Some Simulants.Scene.LayerName) Simulants.Gameplay world

            // make rand from gameplay
            let rand = Rand.makeFromSeedState (Simulants.Gameplay.GetContentRandState world)

            // make field
            let (rand, world) = _bc (createField scene rand world)

            // make player
            let (player, world) = World.createEntity<PlayerDispatcher> (Some Simulants.Player.EntityName) DefaultOverlay scene world
            let world = player.SetDepth Constants.Layout.CharacterDepth world

            // make enemies
            __c (createEnemies scene rand world)

        static let handleLoadGame world =

            // get and initialize gameplay screen from read
            let world = World.readScreenFromFile Assets.SaveFilePath (Some Simulants.Gameplay.ScreenName) world |> snd
            let world = Simulants.Gameplay.SetTransitionState IncomingState world

            // make rand from gameplay
            let rand = Rand.makeFromSeedState (Simulants.Gameplay.GetContentRandState world)

            // make field from rand (field is not serialized, but generated deterministically with ContentRandState)
            __c (createField Simulants.Scene rand world)

        static let handleSelectTitle _ world =
            World.playSong Constants.Audio.DefaultTimeToFadeOutSongMs 1.0f Assets.ButterflyGirlSong world

        static let handleSelectGameplay _ world =
            let world =
                // NOTE: doing a File.Exists then loading the file is dangerous since the file can
                // always be deleted / moved between the two operations!
                if Simulants.Gameplay.GetShallLoadGame world && File.Exists Assets.SaveFilePath
                then handleLoadGame world
                else handleNewGame world
            World.playSong Constants.Audio.DefaultTimeToFadeOutSongMs 1.0f Assets.HerosVengeanceSong world

        static let handleClickSaveGame evt world =
            let gameplay = evt.Subscriber
            World.writeScreenToFile Assets.SaveFilePath gameplay world
            world

        static let handleDeselectGameplay _ world =
            World.destroyLayer Simulants.Scene world

        static member Properties =
            [define Screen.ContentRandState Rand.DefaultSeedState
             define Screen.OngoingRandState Rand.DefaultSeedState
             define Screen.ShallLoadGame false]

        override dispatcher.Register (gameplay, world) =
            let world = Stream.make Simulants.Player.CharacterActivityState.ChangeEvent |> Stream.subscribe handlePlayerActivityChange gameplay $ world
            let world = Stream.make Simulants.HudFeeler.TouchEvent |> Stream.isSimulantSelected Simulants.HudFeeler |> Stream.monitor handleTouchFeeler gameplay $ world
            let world = Stream.make Simulants.HudDetailUp.DownEvent |> Stream.isSimulantSelected Simulants.HudDetailUp |> Stream.monitor (handleDownDetail Upward) gameplay $ world
            let world = Stream.make Simulants.HudDetailRight.DownEvent |> Stream.isSimulantSelected Simulants.HudDetailRight |> Stream.monitor (handleDownDetail Rightward) gameplay $ world
            let world = Stream.make Simulants.HudDetailDown.DownEvent |> Stream.isSimulantSelected Simulants.HudDetailDown |> Stream.monitor (handleDownDetail Downward) gameplay $ world
            let world = Stream.make Simulants.HudDetailLeft.DownEvent |> Stream.isSimulantSelected Simulants.HudDetailLeft |> Stream.monitor (handleDownDetail Leftward) gameplay $ world
            let world = World.monitor handleSelectTitle (!! "Title" : Screen).SelectEvent gameplay world
            let world = World.monitor handleSelectGameplay gameplay.SelectEvent gameplay world
            let world = World.monitor handleClickSaveGame Simulants.HudSaveGame.ClickEvent gameplay world
            let world = World.monitor handleDeselectGameplay gameplay.DeselectEvent gameplay world
            world