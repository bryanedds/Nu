namespace InfinityRpg
open System
open System.Numerics
open System.IO
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

[<AutoOpen>]
module GameplayDispatcher =

    type [<StructuralEquality; NoComparison>] PlayerInput =
        | TouchInput of Vector2
        | DetailInput of Direction
        | NoInput

    type [<StructuralEquality; NoComparison>] GameplayMessage =
        | FinishTurns of CharacterIndex list
        | TickTurns of CharacterIndex list
        | BeginTurns of CharacterIndex list
        | RunCharacterActivation
        | MakeEnemyMoves
        | TryContinuePlayerNavigation
        | TryMakePlayerMove of PlayerInput
        | TryHaltPlayer
        | TransitionMap of Direction
        | HandleMapChange of PlayerInput
        | StartGameplay

    type [<NoEquality; NoComparison>] GameplayCommand =
        | HandlePlayerInput of PlayerInput
        | SaveGame
        | ListenKeyboard
        | Update
        | PostUpdate
        | Nop

    type Screen with
        member this.GetGameplay = this.GetModel<Gameplay>
        member this.SetGameplay = this.SetModel<Gameplay>
        member this.Gameplay = this.Model<Gameplay> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

        static let tryGetNavigationPath (_ : CharacterIndex) positionM gameplay =
            let fieldTiles = gameplay.Field.FieldMapNp.FieldTiles
            let characterPositions = gameplay.Chessboard.CharacterCoordinates |> Map.toValueList |> List.map (fun positionM -> vmtovf positionM)
            let currentPositionM = Gameplay.getCoordinates PlayerIndex gameplay
            let occupationMap = OccupationMap.makeFromFieldTilesAndAdjacentCharacters currentPositionM fieldTiles characterPositions
            let nodes = OccupationMap.makeNavigationNodes occupationMap
            let goalNode = Map.find positionM nodes
            let currentNode = Map.find currentPositionM nodes
            let navigationPathOpt =
                AStar.FindPath
                    (currentNode, goalNode,
                     (fun n n2 -> if n2.PositionM.Y <> n.PositionM.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                     (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

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
            | Upward -> let (newY, arrival) = walk3 true position.Y walkDestination.Y in (v2 position.X newY, arrival)
            | Rightward -> let (newX, arrival) = walk3 true position.X walkDestination.X in (v2 newX position.Y, arrival)
            | Downward -> let (newY, arrival) = walk3 false position.Y walkDestination.Y in (v2 position.X newY, arrival)
            | Leftward -> let (newX, arrival) = walk3 false position.X walkDestination.X in (v2 newX position.Y, arrival)
        
        override this.Channel (_, _) =
            [Simulants.Gameplay.SelectEvent => msg StartGameplay
             Simulants.Gameplay.UpdateEvent => cmd Update
             Simulants.Gameplay.PostUpdateEvent => cmd PostUpdate]

        override this.Message (gameplay, message, _, world) =
            
            match message with
            | FinishTurns indices ->
                let updater index gameplay =
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnFinishing ->
                        match Gameplay.getCharacterActivityState index gameplay with
                        | Action actionDescriptor ->
                            let gameplay = Gameplay.finishMove index gameplay
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = Gameplay.getCharacterState reactorIndex gameplay
                            let characterAnimationState = Gameplay.getCharacterAnimationState index gameplay
                            let gameplay = Gameplay.updateCharacterAnimationState index (constant (characterAnimationState.Facing (World.getTickTime world))) gameplay
                            if reactorState.HitPoints <= 0 then
                                match reactorIndex with
                                | PlayerIndex -> Gameplay.updateCharacterState reactorIndex (constant {reactorState with ControlType = Uncontrolled}) gameplay // TODO: reimplement screen transition
                                | EnemyIndex _ -> Gameplay.removeEnemy reactorIndex gameplay
                            else gameplay
                        | Navigation navigationDescriptor ->
                            let gameplay = Gameplay.setCharacterPositionToCoordinates index gameplay
                            if index.IsEnemy then
                                Gameplay.finishMove index gameplay
                            else gameplay
                        | _ -> failwith "TurnStatus is TurnFinishing; CharacterActivityState should not be NoActivity"
                    | _ -> failwith "non-finishing turns should be filtered out by this point"
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                just gameplay
            
            | TickTurns indices ->
                let updater index gameplay =
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnProgressing ->
                        match Gameplay.getCharacterActivityState index gameplay with
                        | Action actionDescriptor ->
                            let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                            let reactorState = Gameplay.getCharacterState reactorIndex gameplay
                            let gameplay = 
                                if actionDescriptor.ActionTicks = Constants.InfinityRpg.ReactionTick then
                                    if reactorState.HitPoints <= 0 then
                                        let reactorCharacterAnimationState = Gameplay.getCharacterAnimationState reactorIndex gameplay
                                        Gameplay.updateCharacterAnimationState reactorIndex (constant reactorCharacterAnimationState.Slain) gameplay
                                    else gameplay
                                else gameplay
                            if actionDescriptor.ActionTicks = Constants.InfinityRpg.ActionTicksMax
                            then Gameplay.updateTurnStatus index (constant TurnFinishing) gameplay
                            else Gameplay.updateCharacterActivityState index (constant (Action actionDescriptor.Inc)) gameplay
                        | Navigation navigationDescriptor ->
                            let (newPosition, walkState) = Gameplay.getPosition index gameplay |> walk navigationDescriptor.WalkDescriptor
                            let gameplay = Gameplay.updatePosition index (constant newPosition) gameplay
                            match walkState with
                            | WalkFinished -> Gameplay.updateTurnStatus index (constant TurnFinishing) gameplay
                            | WalkContinuing -> gameplay
                        | NoActivity -> failwith "TurnStatus is TurnProgressing; CharacterActivityState should not be NoActivity"
                    | _ -> gameplay
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                let indices = List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) = TurnFinishing) indices
                withMsg (FinishTurns indices) gameplay
            
            | BeginTurns indices ->
                let updater index gameplay =
                    let characterAnimationState = Gameplay.getCharacterAnimationState index gameplay
                    match (Gameplay.getTurnStatus index gameplay) with
                    | TurnBeginning ->
                        let characterAnimationState =
                            match (Gameplay.getCharacterActivityState index gameplay) with
                            | Action actionDescriptor ->
                                let reactorIndex = Option.get actionDescriptor.ActionTargetIndexOpt
                                let characterPosition = Gameplay.getPosition index gameplay
                                let direction = ActionDescriptor.computeActionDirection characterPosition (Gameplay.getCoordinates reactorIndex gameplay)
                                CharacterAnimationState.makeAction (World.getTickTime world) direction
                            | Navigation navigationDescriptor ->
                                characterAnimationState.UpdateDirection navigationDescriptor.WalkDescriptor.WalkDirection
                            | _ -> failwith "TurnStatus is TurnBeginning; CharacterActivityState should not be NoActivity"
                        let gameplay = Gameplay.updateCharacterAnimationState index (constant characterAnimationState) gameplay
                        Gameplay.updateTurnStatus index (constant TurnProgressing) gameplay // "TurnProgressing" for normal animation; "TurnFinishing" for roguelike mode
                    | _ -> gameplay
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withMsg (TickTurns indices) gameplay
            
            | RunCharacterActivation ->
                let gameplay = // NOTE: player's turn is converted to activity at the beginning of the round, activating the observable playback of his move
                    if gameplay.Player.TurnStatus = TurnPending then Gameplay.updateTurnStatus PlayerIndex (constant TurnBeginning) gameplay else gameplay
                let indices = // NOTE: enemies are activated at the same time during player movement, or after player's action has finished playback
                    Gameplay.getEnemyIndices gameplay |> List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) <> Idle)
                let gameplay =
                    if (List.exists (fun x -> (Gameplay.getTurnStatus x gameplay) = TurnPending) indices) then
                        match gameplay.Player.CharacterActivityState with
                        | Action _ -> gameplay
                        | Navigation _ 
                        | NoActivity ->
                            Gameplay.forEachIndex (fun index gameplay -> Gameplay.updateTurnStatus index (constant TurnBeginning) gameplay) indices gameplay
                    else gameplay
                let indices = List.filter (fun x -> (Gameplay.getTurnStatus x gameplay) <> TurnPending) indices
                let indices =
                    match gameplay.Player.TurnStatus with
                    | Idle -> indices
                    | _ -> PlayerIndex :: indices
                withMsg (BeginTurns indices) gameplay

            | MakeEnemyMoves ->
                let indices = Gameplay.getEnemyIndices gameplay
                let attackerOpt =
                    List.tryFind (fun x ->
                        Math.arePositionMsAdjacent
                            (Gameplay.getCoordinates x gameplay)
                            (Gameplay.getCoordinates PlayerIndex gameplay))
                        indices
                let updater =
                    (fun index gameplay ->
                        let characterState = Gameplay.getCharacterState index gameplay
                        let enemyMoveOpt =
                            match characterState.ControlType with
                            | Chaos ->
                                if characterState.HitPoints > 0 then
                                    match attackerOpt with
                                    | Some attackerIndex ->
                                        if index = attackerIndex
                                        then Some (Attack PlayerIndex)
                                        else None
                                    | None ->
                                        let openDirections = Gameplay.getCoordinates index gameplay |> gameplay.Chessboard.OpenDirections
                                        let direction = Gen.random1 4 |> Direction.fromInt
                                        if List.exists (fun x -> x = direction) openDirections
                                        then Some (Step direction)
                                        else None
                                else None
                            | _ -> None
                        match enemyMoveOpt with
                        | Some move ->
                            let gameplay = Gameplay.addMove index move gameplay
                            let gameplay = Gameplay.activateCharacter index gameplay
                            Gameplay.applyMove index gameplay
                        | None -> gameplay)
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withMsg RunCharacterActivation gameplay

            | TryContinuePlayerNavigation ->
                let playerMoveOpt =
                    match gameplay.Player.TurnStatus with
                    | TurnFinishing ->
                        match Gameplay.getCurrentMove PlayerIndex gameplay with
                        | Travel path ->
                            match path with
                            | _ :: [] -> None
                            | _ :: navigationPath ->
                                let targetPositionM = (List.head navigationPath).PositionM
                                if List.exists (fun x -> x = targetPositionM) gameplay.Chessboard.AvailableCoordinates
                                then Some (Travel navigationPath)
                                else None
                            | [] -> failwithumf ()
                        | _ -> None
                    | _ -> None
                let gameplay =
                    match gameplay.Player.TurnStatus with
                    | TurnFinishing -> Gameplay.finishMove PlayerIndex gameplay
                    | _ -> gameplay
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.activateCharacter PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ ->
                    if not (Gameplay.anyTurnsInProgress gameplay) then withCmd ListenKeyboard gameplay else withMsg RunCharacterActivation gameplay
            
            | TryMakePlayerMove playerInput ->
                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                let targetCoordinatesOpt =
                    match playerInput with
                    | TouchInput touchPosition -> Some (World.mouseToWorld false touchPosition world |> vftovm)
                    | DetailInput direction -> Some (currentCoordinates + dtovm direction)
                    | NoInput -> None
                let playerMoveOpt =
                    match targetCoordinatesOpt with
                    | Some coordinates ->
                        if Math.arePositionMsAdjacent coordinates currentCoordinates then
                            let openDirections = gameplay.Chessboard.OpenDirections currentCoordinates
                            let direction = Math.directionToTarget currentCoordinates coordinates
                            let opponents = Gameplay.getOpponentIndices PlayerIndex gameplay
                            if List.exists (fun x -> x = direction) openDirections
                            then Some (Step direction)
                            elif List.exists (fun index -> (Gameplay.getCoordinates index gameplay) = coordinates) opponents then
                                let targetIndex = Gameplay.getIndexByCoordinates coordinates gameplay
                                Some (Attack targetIndex)
                            else None
                        else
                            match tryGetNavigationPath PlayerIndex coordinates gameplay with
                            | Some navigationPath ->
                                match navigationPath with
                                | [] -> None
                                | _ -> Some (Travel navigationPath)
                            | None -> None
                    | None -> None
                
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.activateCharacter PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ -> just gameplay

            | TryHaltPlayer ->
                match Map.tryFind PlayerIndex gameplay.Chessboard.CurrentMoves with
                | Some _ ->
                    let gameplay = Gameplay.truncatePlayerPath gameplay
                    just gameplay
                | None -> just gameplay

            | TransitionMap direction ->
                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                let newCoordinates =
                    match direction with
                    | Upward -> currentCoordinates.WithY 0
                    | Rightward -> currentCoordinates.WithX 0
                    | Downward -> currentCoordinates.WithY (Constants.Layout.FieldUnitSizeM.Y - 1)
                    | Leftward -> currentCoordinates.WithX (Constants.Layout.FieldUnitSizeM.X - 1)
                let gameplay = Gameplay.clearEnemies gameplay
                let gameplay = Gameplay.clearPickups gameplay
                let gameplay = Gameplay.yankPlayer newCoordinates gameplay
                let gameplay = Gameplay.transitionMap direction gameplay
                let gameplay = Gameplay.setFieldMap (FieldMap.makeFromFieldMapUnit gameplay.MapModeler.Current) gameplay
                let gameplay = Gameplay.makeEnemies 4 gameplay
                just gameplay

            | HandleMapChange playerInput ->
                let msg =
                    match playerInput with
                    | DetailInput direction ->
                        let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                        let targetOutside =
                            match direction with
                            | Upward -> currentCoordinates.Y = Constants.Layout.FieldUnitSizeM.Y - 1
                            | Rightward -> currentCoordinates.X = Constants.Layout.FieldUnitSizeM.X - 1
                            | Downward -> currentCoordinates.Y = 0
                            | Leftward -> currentCoordinates.X = 0
                        if targetOutside && gameplay.MapModeler.PossibleInDirection direction
                        then TransitionMap direction
                        else TryMakePlayerMove playerInput
                    | _ -> TryMakePlayerMove playerInput
                withMsg msg gameplay
            
            | StartGameplay ->
                if gameplay.ShallLoadGame && File.Exists Assets.SaveFilePath then
                    let gameplayStr = File.ReadAllText Assets.SaveFilePath
                    let gameplay = scvalue<Gameplay> gameplayStr
                    just gameplay
                else
                    let gameplay = Gameplay.initial
                    let gameplay = Gameplay.setFieldMap (FieldMap.makeFromFieldMapUnit gameplay.MapModeler.Current) gameplay
                    let gameplay = Gameplay.makePlayer gameplay
                    let gameplay = Gameplay.makeEnemies 4 gameplay
                    just gameplay

        override this.Command (gameplay, command, _, world) =

            match command with
            | HandlePlayerInput playerInput ->
                if not (Gameplay.anyTurnsInProgress gameplay) then
                    match gameplay.Player.CharacterState.ControlType with
                    | PlayerControlled -> withMsg (HandleMapChange playerInput) world
                    | _ -> just world
                else just world

            | SaveGame ->
                let gameplayStr = scstring gameplay
                File.WriteAllText (Assets.SaveFilePath, gameplayStr)
                just world

            | ListenKeyboard ->
                if KeyboardState.isKeyDown KeyboardKey.Up then withCmd (HandlePlayerInput (DetailInput Upward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd (HandlePlayerInput (DetailInput Rightward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd (HandlePlayerInput (DetailInput Downward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd (HandlePlayerInput (DetailInput Leftward)) world
                else just world
            
            | Update ->
                if (Gameplay.anyTurnsInProgress gameplay) then withMsg TryContinuePlayerNavigation world
                else withCmd ListenKeyboard world

            | PostUpdate ->
                let playerCenter = Simulants.Player.GetCenter world
                let eyeCenter =
                    if Simulants.Field.Exists world then
                        let eyeSize = World.getEyeSize world
                        let eyeCornerNegative = playerCenter - eyeSize * 0.5f
                        let eyeCornerPositive = playerCenter + eyeSize * 0.5f
                        let fieldCornerNegative = Simulants.Field.GetPosition world
                        let fieldCornerPositive = Simulants.Field.GetPosition world + Simulants.Field.GetSize world
                        let fieldBoundsNegative = fieldCornerNegative + eyeSize * 0.5f
                        let fieldBoundsPositive = fieldCornerPositive - eyeSize * 0.5f
                        let eyeCenterX =
                            if eyeCornerNegative.X < fieldCornerNegative.X then fieldBoundsNegative.X
                            elif eyeCornerPositive.X > fieldCornerPositive.X then fieldBoundsPositive.X
                            else playerCenter.X
                        let eyeCenterY =
                            if eyeCornerNegative.Y < fieldCornerNegative.Y then fieldBoundsNegative.Y
                            elif eyeCornerPositive.Y > fieldCornerPositive.Y then fieldBoundsPositive.Y
                            else playerCenter.Y
                        v2 eyeCenterX eyeCenterY
                    else playerCenter
                let world = World.setEyeCenter eyeCenter world
                just world

            | Nop ->
                just world

        override this.Content (gameplay, screen) =

            // scene layer
            [Content.layerIfScreenSelected screen (fun _ _ ->
                Content.layer Simulants.Scene.Name []

                    [Content.entity<FieldDispatcher> Simulants.Field.Name
                       [Entity.Field <== gameplay --> fun gameplay -> gameplay.Field]

                     Content.entities gameplay
                        (fun gameplay ->
                            let generator k _ = Pickup.makeHealth k
                            Map.toListBy generator gameplay.Chessboard.PickupItems)
                        constant
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Pickup <== pickup])

                     Content.entitiesIndexedBy gameplay
                        (fun gameplay -> gameplay.Enemies) constant
                        (fun character -> match character.Index with EnemyIndex i -> i | _ -> failwithumf ())
                        (fun index character _ -> Content.entity<CharacterDispatcher> ("Enemy+" + scstring index) [Entity.Character <== character])

                     Content.entity<CharacterDispatcher> Simulants.Player.Name
                       [Entity.Character <== gameplay --> fun gameplay -> gameplay.Player]])

             // hud layer
             Content.layer Simulants.Hud.Name []

                [Content.button Simulants.HudSaveGame.Name
                    [Entity.Position == v2 88.0f -184.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "SaveGameUp"; Entity.DownImage == asset "Gui" "SaveGameDown"
                     Entity.Enabled <== gameplay --> fun gameplay -> if Gameplay.anyTurnsInProgress gameplay then false else true
                     Entity.ClickEvent ==> cmd SaveGame]

                 Content.button Simulants.HudHalt.Name
                    [Entity.Position == v2 88.0f -112.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "HaltUp"; Entity.DownImage == asset "Gui" "HaltDown"
                     Entity.Enabled <== gameplay --> fun gameplay ->
                        match gameplay.Player.CharacterActivityState with
                        | Navigation nav -> nav.MultiRoundContext
                        | _ -> false
                     Entity.ClickEvent ==> msg TryHaltPlayer]

                 Content.button Simulants.HudBack.Name
                    [Entity.Position == v2 88.0f -256.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "BackUp"; Entity.DownImage == asset "Gui" "BackDown"]

                 Content.label Gen.name
                    [Entity.Position == v2 -448.0f -240.0f; Entity.Size == v2 224.0f 224.0f; Entity.Depth == 9.0f
                     Entity.LabelImage == asset "Gui" "DetailBacking"]

                 Content.button Simulants.HudDetailUpward.Name
                    [Entity.Position == v2 -368.0f -88.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailUpwardUp"; Entity.DownImage == asset "Gui" "DetailUpwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Upward))]

                 Content.button Simulants.HudDetailRightward.Name
                    [Entity.Position == v2 -296.0f -160.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailRightwardUp"; Entity.DownImage == asset "Gui" "DetailRightwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Rightward))]

                 Content.button Simulants.HudDetailDownward.Name
                    [Entity.Position == v2 -368.0f -232.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailDownwardUp"; Entity.DownImage == asset "Gui" "DetailDownwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Downward))]

                 Content.button Simulants.HudDetailLeftward.Name
                    [Entity.Position == v2 -440.0f -160.0f; Entity.Size == v2 64.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "DetailLeftwardUp"; Entity.DownImage == asset "Gui" "DetailLeftwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Leftward))]

                 Content.feeler Simulants.HudFeeler.Name
                    [Entity.Position == v2 -480.0f -272.0f; Entity.Size == v2 960.0f 544.0f; Entity.Depth == 9.0f
                     Entity.TouchEvent ==|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))]]]
