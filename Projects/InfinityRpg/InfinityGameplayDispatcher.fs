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
        | BeginTurns
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

        static let tryGetNavigationPath (_ : CharacterIndex) coordinates gameplay =
            let fieldTiles = gameplay.Field.FieldMapNp.FieldTiles
            let characterPositions = gameplay.Chessboard.OccupiedSpaces |> List.map (fun coordinates -> vctovf coordinates)
            let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
            let navigationMap = NavigationMap.makeFromFieldTilesAndAdjacentCharacters currentCoordinates fieldTiles characterPositions
            let nodes = NavigationMap.makeNavigationNodes navigationMap
            let goalNode = Map.find coordinates nodes
            let currentNode = Map.find currentCoordinates nodes
            let navigationPathOpt =
                AStar.FindPath
                    (currentNode,
                     goalNode,
                     (fun n n2 -> if n2.Coordinates.Y <> n.Coordinates.Y then 2.0f else 1.0f), // prefer horizontal walk to vertical for predictability
                     (fun _ -> 0.0f))
            match navigationPathOpt with
            | null -> None
            | navigationPath -> Some (navigationPath |> List.ofSeq |> List.rev |> List.tail)

        override this.Channel (_, _) =
            [Simulants.Gameplay.SelectEvent => msg StartGameplay
             Simulants.Gameplay.UpdateEvent => cmd Update
             Simulants.Gameplay.PostUpdateEvent => cmd PostUpdate]

        override this.Message (gameplay, message, _, world) =
            
            match message with
            | FinishTurns indices ->
                let updater index gameplay =
                    let characterTurn = Gameplay.getCharacterTurn index gameplay
                    match characterTurn.TurnStatus with
                    | TurnFinishing ->
                        match characterTurn.TurnType with
                        | AttackTurn ->
                            let gameplay = Gameplay.finishMove index gameplay
                            let reactorIndex = Option.get characterTurn.ReactorOpt
                            let reactorState = Gameplay.getCharacter reactorIndex gameplay
                            let gameplay =
                                if reactorIndex = PlayerIndex then
                                    Gameplay.refreshPlayerPuppetHitPoints gameplay
                                else gameplay
                            if reactorState.HitPoints <= 0 then
                                match reactorIndex with
                                | PlayerIndex -> Gameplay.updateCharacter reactorIndex (Character.updateControlType (constant Uncontrolled)) gameplay // TODO: reimplement screen transition
                                | EnemyIndex _ -> Gameplay.removeEnemy reactorIndex gameplay
                            else gameplay
                        | WalkTurn _ -> if index.IsEnemy then Gameplay.finishMove index gameplay else gameplay
                    | _ -> failwith "non-finishing turns should be filtered out by this point"
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                just gameplay
            
            | TickTurns indices ->
                let updater index gameplay =
                    let characterTurn = Gameplay.getCharacterTurn index gameplay
                    match characterTurn.TurnStatus with
                    | TurnTicking tickCount ->
                        let gameplay =
                            match characterTurn.TurnType with
                            | AttackTurn ->
                                if tickCount = Constants.InfinityRpg.ActionTicksMax
                                then Gameplay.setCharacterTurnStatus index TurnFinishing gameplay
                                else gameplay
                            | WalkTurn _ ->
                                if tickCount = dec (int64 Constants.InfinityRpg.CharacterWalkSteps)
                                then Gameplay.setCharacterTurnStatus index TurnFinishing gameplay
                                else gameplay
                        Gameplay.updateCharacterTurn index Turn.incTickCount gameplay
                    | _ -> gameplay
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                let indices = List.filter (fun x -> (Gameplay.getCharacterTurn x gameplay).TurnStatus = TurnFinishing) indices
                withMsg (FinishTurns indices) gameplay
            
            | BeginTurns ->
                let updater index gameplay =
                    let characterTurn = Gameplay.getCharacterTurn index gameplay
                    match characterTurn.TurnStatus with
                    | TurnBeginning ->
                        let gameplay =
                            match characterTurn.TurnType with
                            | AttackTurn -> Gameplay.updateCharacterTurn index (Turn.updateStartTick (constant (World.getTickTime world))) gameplay
                            | WalkTurn _ -> gameplay
                        Gameplay.setCharacterTurnStatus index (TurnTicking 0L) gameplay // "TurnTicking" for normal animation; "TurnFinishing" for roguelike mode
                    | _ -> gameplay
                let gameplay = Gameplay.updatePuppeteer (Puppeteer.runTurnCoordination) gameplay
                let indices = Gameplay.getCharacterIndices gameplay |> List.filter (fun x -> Gameplay.turnInProgress x gameplay) |> List.filter (fun x -> (Gameplay.getCharacterTurn x gameplay).TurnStatus <> TurnPending)
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withMsg (TickTurns indices) gameplay
            
            | MakeEnemyMoves ->
                let indices = Gameplay.getEnemyIndices gameplay
                let attackerOpt =
                    List.tryFind (fun x ->
                        Math.areCoordinatesAdjacent
                            (Gameplay.getCoordinates x gameplay)
                            (Gameplay.getCoordinates PlayerIndex gameplay))
                        indices
                let updater =
                    (fun index gameplay ->
                        let character = Gameplay.getCharacter index gameplay
                        let enemyMoveOpt =
                            match character.ControlType with
                            | Chaos ->
                                if character.HitPoints > 0 then
                                    match attackerOpt with
                                    | Some attackerIndex when attackerIndex = index ->
                                        Some (Attack PlayerIndex)
                                    | _ ->
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
                withMsg BeginTurns gameplay

            | TryContinuePlayerNavigation ->
                let playerTurnOpt = Gameplay.tryGetCharacterTurn PlayerIndex gameplay
                let playerMoveOpt =
                    match playerTurnOpt with
                    | Some playerTurn ->
                        match playerTurn.TurnStatus with
                        | TurnFinishing ->
                            match Gameplay.getCharacterMove PlayerIndex gameplay with
                            | Travel path ->
                                match path with
                                | _ :: [] -> None
                                | _ :: navigationPath ->
                                    let targetCoordinates = (List.head navigationPath).Coordinates
                                    if List.exists (fun x -> x = targetCoordinates) gameplay.Chessboard.UnoccupiedSpaces
                                    then Some (Travel navigationPath)
                                    else None
                                | [] -> failwithumf ()
                            | _ -> None
                        | _ -> None
                    | None -> None
                let gameplay =
                    match playerTurnOpt with
                    | Some playerTurn ->
                        match playerTurn.TurnStatus with
                        | TurnFinishing -> Gameplay.finishMove PlayerIndex gameplay
                        | _ -> gameplay
                    | None -> gameplay
                match playerMoveOpt with
                | Some move ->
                    let gameplay = Gameplay.addMove PlayerIndex move gameplay
                    let gameplay = Gameplay.activateCharacter PlayerIndex gameplay
                    let gameplay = Gameplay.applyMove PlayerIndex gameplay
                    withMsg MakeEnemyMoves gameplay
                | _ ->
                    if not (Gameplay.anyTurnsInProgress gameplay)
                    then withCmd ListenKeyboard gameplay
                    else withMsg BeginTurns gameplay
            
            | TryMakePlayerMove playerInput ->
                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                let targetCoordinatesOpt =
                    match playerInput with
                    | TouchInput touchPosition -> Some (World.mouseToWorld false touchPosition world |> vftovc)
                    | DetailInput direction -> Some (currentCoordinates + dtovc direction)
                    | NoInput -> None
                let playerMoveOpt =
                    match targetCoordinatesOpt with
                    | Some coordinates ->
                        if Math.areCoordinatesAdjacent coordinates currentCoordinates then
                            let openDirections = gameplay.Chessboard.OpenDirections currentCoordinates
                            let direction = Math.directionToTarget currentCoordinates coordinates
                            let opponents = Gameplay.getOpponentIndices PlayerIndex gameplay
                            if List.exists (fun x -> x = direction) openDirections then Some (Step direction)
                            elif List.exists (fun index -> (Gameplay.getCoordinates index gameplay) = coordinates) opponents then
                                let targetIndex = Gameplay.getIndexByCoordinates coordinates gameplay
                                Some (Attack targetIndex)
                            else None
                        else
                            match tryGetNavigationPath PlayerIndex coordinates gameplay with
                            | Some navigationPath ->
                                match navigationPath with
                                | _ :: _ -> Some (Travel navigationPath)
                                | [] -> None
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
                match Map.tryFind PlayerIndex gameplay.CharacterMoves with
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
                    | Downward -> currentCoordinates.WithY (Constants.Layout.FieldMapSizeC.Y - 1)
                    | Leftward -> currentCoordinates.WithX (Constants.Layout.FieldMapSizeC.X - 1)
                let gameplay = Gameplay.clearEnemies gameplay
                let gameplay = Gameplay.clearPickups gameplay
                let gameplay = Gameplay.relocateCharacter PlayerIndex newCoordinates gameplay
                let gameplay = Gameplay.transitionMap direction gameplay
                let gameplay = Gameplay.resetFieldMap (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
                let gameplay = Gameplay.makeEnemies 4 gameplay
                just gameplay

            | HandleMapChange playerInput ->
                let msg =
                    match playerInput with
                    | DetailInput direction ->
                        let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                        let targetOutside =
                            match direction with
                            | Upward -> currentCoordinates.Y = Constants.Layout.FieldMapSizeC.Y - 1
                            | Rightward -> currentCoordinates.X = Constants.Layout.FieldMapSizeC.X - 1
                            | Downward -> currentCoordinates.Y = 0
                            | Leftward -> currentCoordinates.X = 0
                        if targetOutside && gameplay.MetaMap.PossibleInDirection direction
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
                    let gameplay = Gameplay.resetFieldMap (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
                    let gameplay = Gameplay.makeEnemies 4 gameplay
                    just gameplay

        override this.Command (gameplay, command, _, world) =

            match command with
            | HandlePlayerInput playerInput ->
                if not (Gameplay.anyTurnsInProgress gameplay) then
                    match (Gameplay.getCharacter PlayerIndex gameplay).ControlType with
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
                if (Gameplay.anyTurnsInProgress gameplay)
                then withMsg TryContinuePlayerNavigation world
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

                    // field
                    [Content.entity<FieldDispatcher> Simulants.Field.Name
                       [Entity.Field <== gameplay --> fun gameplay -> gameplay.Field]

                     // pickups
                     Content.entitiesUntracked gameplay
                        (fun gameplay -> gameplay.Chessboard.PickupSpaces)
                        (fun pickups _ -> Map.toListBy (fun positionM _ -> Pickup.makeHealth positionM) pickups)
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Size == Constants.Layout.TileSize; Entity.Pickup <== pickup])

                     // characters
                     Content.entitiesTrackedByFst gameplay
                        (fun gameplay -> (gameplay.Chessboard.Characters, gameplay.Puppeteer))
                        (fun (characters, puppeteer) _ -> Puppeteer.generatePositionsAndAnimationStates characters puppeteer)
                        (fun index characterData _ ->
                            let name = match index with 0 -> Simulants.Player.Name | _ -> "Enemy+" + scstring index
                            Content.entity<CharacterDispatcher> name
                                [Entity.Position <== characterData --> fun (position, _) -> position
                                 Entity.CharacterAnimationSheet <== characterData --> fun _ -> match index with 0 -> Assets.PlayerImage | _ -> Assets.GoopyImage // TODO: pull this from data
                                 Entity.CharacterAnimationState <== characterData --> fun (_, characterAnimationState) -> characterAnimationState])])

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
                     Entity.Enabled <== gameplay --> fun gameplay -> Gameplay.isPlayerTraveling gameplay
                     Entity.ClickEvent ==> msg TryHaltPlayer]

                 Content.button Simulants.HudBack.Name
                    [Entity.Position == v2 88.0f -256.0f; Entity.Size == v2 384.0f 64.0f; Entity.Depth == 10.0f
                     Entity.UpImage == asset "Gui" "BackUp"; Entity.DownImage == asset "Gui" "BackDown"]

                 Content.text Gen.name
                    [Entity.Position == v2 -440.0f 200.0f; Entity.Depth == 9.0f
                     Entity.Text <== gameplay --> fun gameplay ->
                        "HP: " + scstring gameplay.Puppeteer.PlayerPuppetState.HitPoints]

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
                    [Entity.Position == v2 -480.0f -270.0f; Entity.Size == v2 960.0f 540.0f; Entity.Depth == 9.0f
                     Entity.TouchEvent ==|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))]]]
