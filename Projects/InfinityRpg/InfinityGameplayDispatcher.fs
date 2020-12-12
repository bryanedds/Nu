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
        | TickTurns
        | BeginTurns
        | MakeEnemyAttack
        | MakeEnemiesWalk
        | MakeEnemyMoves
        | TryTransitionRound
        | TryMakePlayerMove of PlayerInput
        | SkipPlayerTurn
        | HaltPlayer
        | TransitionMap of Direction
        | HandleMapChange of PlayerInput
        | Initialize
        | Update

    type [<NoEquality; NoComparison>] GameplayCommand =
        | DisplayTurnEffects of CharacterIndex list
        | HandlePlayerInput of PlayerInput
        | ListenKeyboard
        | TrackPlayer
        | Save
        | Nop

    type Screen with
        member this.GetGameplay = this.GetModel<Gameplay>
        member this.SetGameplay = this.SetModel<Gameplay>
        member this.Gameplay = this.Model<Gameplay> ()

    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

        static let tryGetNavigationPath coordinates gameplay =
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
            [Simulants.Gameplay.SelectEvent => msg Initialize
             Simulants.Gameplay.UpdateEvent => msg Update
             Simulants.Gameplay.PostUpdateEvent => cmd TrackPlayer]

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
                            match characterTurn.GetReactor with
                            | ReactingCharacter reactorIndex ->
                                let reactorState = Gameplay.getCharacter reactorIndex gameplay
                                let gameplay =
                                    if reactorIndex = PlayerIndex
                                    then Gameplay.refreshPlayerPuppetHitPoints gameplay
                                    else gameplay
                                if reactorState.HitPoints <= 0 then
                                    match reactorIndex with
                                    | PlayerIndex -> Gameplay.updateCharacter reactorIndex (Character.updateControlType (constant Uncontrolled)) gameplay // TODO: reimplement screen transition
                                    | EnemyIndex _ -> Gameplay.removeCharacter reactorIndex gameplay
                                else gameplay
                            | ReactingProp coordinates -> Gameplay.removeLongGrass coordinates gameplay
                        | WalkTurn _ -> Gameplay.finishMove index gameplay
                    | _ -> failwith "non-finishing turns should be filtered out by this point"
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                just gameplay
            
            | TickTurns ->
                let updater index gameplay =
                    let characterTurn = Gameplay.getCharacterTurn index gameplay
                    match characterTurn.TurnStatus with
                    | TurnTicking ->
                        let tickCount = gameplay.Time - characterTurn.StartTick
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
                        gameplay
                    | _ -> gameplay
                let indices = Puppeteer.getActingCharacters gameplay.Puppeteer
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                let indices = List.filter (fun x -> (Gameplay.getCharacterTurn x gameplay).TurnStatus = TurnFinishing) indices
                withMsg (FinishTurns indices) gameplay

            | BeginTurns ->
                let updater index gameplay =
                    let characterTurn = Gameplay.getCharacterTurn index gameplay
                    match characterTurn.TurnStatus with
                    | TurnBeginning -> Gameplay.setCharacterTurnStatus index TurnTicking gameplay // "TurnTicking" for normal animation; "TurnFinishing" for roguelike mode
                    | _ -> gameplay
                let indices = Puppeteer.getActingCharacters gameplay.Puppeteer
                let gameplay = Gameplay.forEachIndex updater indices gameplay
                withCmd (DisplayTurnEffects indices) gameplay

            | MakeEnemyAttack ->
                let gameplay =
                    let index = gameplay.Round.AttackingEnemyGroup.Head
                    let gameplay =
                        if (Gameplay.getCharacter PlayerIndex gameplay).IsAlive // TODO: create Gameplay.isPlayerAlive function.
                        then Gameplay.makeMove gameplay.Time index (Attack (ReactingCharacter PlayerIndex)) gameplay
                        else gameplay
                    Gameplay.removeHeadFromAttackingEnemyGroup gameplay
                withMsg BeginTurns gameplay

            | MakeEnemiesWalk ->
                let updater =
                    (fun index gameplay ->
                        let character = Gameplay.getCharacter index gameplay
                        match character.ControlType with
                        | Chaos ->
                            let openDirections = Gameplay.getCoordinates index gameplay |> gameplay.Chessboard.OpenDirections
                            let direction = Gen.random1 4 |> Direction.ofInt
                            if List.exists (fun x -> x = direction) openDirections
                            then Gameplay.makeMove gameplay.Time index (Step direction) gameplay
                            else gameplay
                        | _ -> gameplay)
                        
                let gameplay = Gameplay.forEachIndex updater gameplay.Round.WalkingEnemyGroup gameplay
                let gameplay = Gameplay.removeWalkingEnemyGroup gameplay
                withMsg BeginTurns gameplay
            
            | MakeEnemyMoves ->
                let adjacentEnemies = Gameplay.getEnemyIndices gameplay |> List.filter (fun x -> Gameplay.areCharactersAdjacent x PlayerIndex gameplay)
                let gameplay = Gameplay.addAttackingEnemyGroup adjacentEnemies gameplay
                let gameplay = Gameplay.createWalkingEnemyGroup gameplay
                
                match gameplay.Round.TryGetPlayerMove with
                | Some move ->
                    match move with
                    | Step _
                    | Travel _ -> withMsg MakeEnemiesWalk gameplay
                    | _ -> withMsg BeginTurns gameplay
                | None -> withMsg MakeEnemiesWalk gameplay

            | TryTransitionRound ->
                let gameplay =
                    match gameplay.Round.PlayerContinuity with
                    | AutomaticNavigation path ->
                        match path with
                        | _ :: [] -> gameplay
                        | _ :: navigationPath ->
                            let targetCoordinates = (List.head navigationPath).Coordinates
                            if List.exists (fun x -> x = targetCoordinates) gameplay.Chessboard.UnoccupiedSpaces
                            then Gameplay.makeMove gameplay.Time PlayerIndex (Travel navigationPath) gameplay
                            else gameplay
                        | [] -> failwithumf ()
                    | _ -> gameplay
                
                if Map.exists (fun k _ -> k = PlayerIndex) gameplay.Round.CharacterMoves then
                    withMsg MakeEnemyMoves gameplay
                else
                    let gameplay = Gameplay.updateRound (Round.updatePlayerContinuity (constant NoContinuity)) gameplay
                    withCmd ListenKeyboard gameplay
            
            | TryMakePlayerMove playerInput ->
                let time = gameplay.Time
                let currentCoordinates = Gameplay.getCoordinates PlayerIndex gameplay
                let targetCoordinatesOpt =
                    match playerInput with
                    | TouchInput touchPosition -> Some (World.mouseToWorld false touchPosition world |> vftovc)
                    | DetailInput direction -> Some (currentCoordinates + dtovc direction)
                    | NoInput -> None
                let gameplay =
                    match targetCoordinatesOpt with
                    | Some coordinates ->
                        if Math.areCoordinatesAdjacent coordinates currentCoordinates then
                            if Chessboard.spaceExists coordinates gameplay.Chessboard then
                                match Chessboard.tryGetOccupantAtCoordinates coordinates gameplay.Chessboard with
                                | Some occupant ->
                                    match occupant with
                                    | OccupyingCharacter character -> Gameplay.makeMove time PlayerIndex (Attack (ReactingCharacter character.CharacterIndex)) gameplay
                                    | OccupyingProp _ -> Gameplay.makeMove time PlayerIndex (Attack (ReactingProp coordinates)) gameplay
                                | None ->
                                    let direction = Math.directionToTarget currentCoordinates coordinates
                                    Gameplay.makeMove time PlayerIndex (Step direction) gameplay
                            else gameplay
                        else
                            match tryGetNavigationPath coordinates gameplay with
                            | Some navigationPath ->
                                match navigationPath with
                                | _ :: _ -> Gameplay.makeMove time PlayerIndex (Travel navigationPath) gameplay
                                | [] -> gameplay
                            | None -> gameplay
                    | None -> gameplay
                
                if Map.exists (fun k _ -> k = PlayerIndex) gameplay.Round.CharacterMoves then
                    withMsg MakeEnemyMoves gameplay
                else just gameplay

            | SkipPlayerTurn ->
                let gameplay = Gameplay.updateRound (Round.updatePlayerContinuity (constant Waiting)) gameplay
                withMsg MakeEnemyMoves gameplay
            
            | HaltPlayer ->
                let gameplay = Gameplay.truncatePlayerPath gameplay
                just gameplay

            | TransitionMap direction ->
                let gameplay = Gameplay.clearEnemies gameplay
                let gameplay = Gameplay.clearPickups gameplay
                let gameplay = Gameplay.transitionMap direction gameplay
                let gameplay = Gameplay.populateFieldMap gameplay
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
                        if targetOutside && gameplay.MetaMap.PossibleInDirection direction && gameplay.MetaMap.OnPathBoundary currentCoordinates
                        then TransitionMap direction
                        else TryMakePlayerMove playerInput
                    | _ -> TryMakePlayerMove playerInput
                withMsg msg gameplay
            
            | Initialize ->
                if gameplay.ShallLoadGame && File.Exists Assets.Global.SaveFilePath then
                    let gameplayStr = File.ReadAllText Assets.Global.SaveFilePath
                    let gameplay = scvalue<Gameplay> gameplayStr
                    just gameplay
                else
                    let gameplay = Gameplay.initial
                    let gameplay = Gameplay.resetFieldMapWithPlayer (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
                    let gameplay = Gameplay.populateFieldMap gameplay
                    just gameplay

            | Update ->
                let gameplay = Gameplay.advanceTime gameplay
                match gameplay.Round.RoundStatus with
                | RunningCharacterMoves -> withMsg TickTurns gameplay
                | MakingEnemyAttack -> withMsg MakeEnemyAttack gameplay
                | MakingEnemiesWalk -> withMsg MakeEnemiesWalk gameplay
                | FinishingRound -> withMsg TryTransitionRound gameplay
                | NoRound -> withCmd ListenKeyboard gameplay

        override this.Command (gameplay, command, _, world) =

            match command with
            | DisplayTurnEffects indices ->
                let world =
                    match Puppeteer.tryGetCharacterTurn PlayerIndex gameplay.Puppeteer with
                    | Some turn ->
                        match turn.TurnType with
                        | AttackTurn ->
                            if turn.StartTick = gameplay.Time then
                                let effect = Effects.makeSwordStrikeEffect turn.Direction
                                let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Scene world
                                let world = entity.SetEffect effect world
                                let world = entity.SetSize (v2Dup 144.0f) world
                                let world = entity.SetPosition ((vctovf turn.OriginCoordinates) - Constants.Layout.TileSize) world
                                let world = entity.SetElevation Constants.Layout.EffectElevation world
                                entity.SetSelfDestruct true world
                            else world
                        | _ -> world
                    | None -> world
                withMsg TickTurns world
            
            | HandlePlayerInput playerInput ->
                if not gameplay.Round.InProgress then
                    match (Gameplay.getCharacter PlayerIndex gameplay).ControlType with
                    | PlayerControlled -> withMsg (HandleMapChange playerInput) world
                    | _ -> just world
                else just world

            | Save ->
                let gameplayStr = scstring gameplay
                File.WriteAllText (Assets.Global.SaveFilePath, gameplayStr)
                just world

            | ListenKeyboard ->
                if KeyboardState.isKeyDown KeyboardKey.Up then withCmd (HandlePlayerInput (DetailInput Upward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd (HandlePlayerInput (DetailInput Rightward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd (HandlePlayerInput (DetailInput Downward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd (HandlePlayerInput (DetailInput Leftward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Space then withMsg SkipPlayerTurn world
                else just world

            | TrackPlayer ->
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
                     Content.entities gameplay
                        (fun gameplay -> gameplay.Chessboard.PickupSpaces)
                        (fun pickups _ -> pickups |> Map.toSeqBy (fun positionM _ -> Pickup.makeHealth positionM) |> Map.indexed)
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Size == Constants.Layout.TileSize; Entity.Pickup <== pickup])

                     // props
                     Content.entities gameplay
                        (fun gameplay -> (gameplay.Chessboard.PropSpaces, gameplay.Puppeteer, gameplay.Time))
                        (fun (props, puppeteer, time) _ -> Puppeteer.getPropMap props puppeteer time)
                        (fun index prop _ -> Content.entity<PropDispatcher> ("Prop+" + scstring index) [Entity.Size == Constants.Layout.TileSize; Entity.Prop <== prop])

                     // characters
                     Content.entities gameplay
                        (fun gameplay -> (gameplay.Chessboard.Characters, gameplay.Puppeteer, gameplay.Time))
                        (fun (characters, puppeteer, time) _ -> Puppeteer.getCharacterMap characters puppeteer time)
                        (fun index character _ ->
                            let name =
                                match index with
                                | 0 -> Simulants.Player.Name
                                | _ -> "Enemy+" + scstring index
                            Content.entity<CharacterDispatcher> name
                                [Entity.CharacterAnimationSheet <== character --> fun (_, _, _) -> match index with 0 -> Assets.Gameplay.PlayerImage | _ -> Assets.Gameplay.GoopyImage // TODO: pull this from data
                                 Entity.CharacterAnimationState <== character --> fun (_, characterAnimationState, _) -> characterAnimationState
                                 Entity.CharacterAnimationTime <== character --> fun (_, _, time) -> time
                                 Entity.Position <== character --> fun (position, _, _) -> position])])

             // hud layer
             Content.layer Simulants.Hud.Name []

                [Content.button Simulants.HudHalt.Name
                    [Entity.Position == v2 184.0f -144.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Halt"
                     Entity.Enabled <== gameplay --> fun gameplay -> gameplay.Round.IsPlayerTraveling
                     Entity.ClickEvent ==> msg HaltPlayer]

                 Content.button Simulants.HudSaveGame.Name
                    [Entity.Position == v2 184.0f -200.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Save Game"
                     Entity.Enabled <== gameplay --> fun gameplay -> if gameplay.Round.InProgress then false else true
                     Entity.ClickEvent ==> cmd Save]

                 Content.button Simulants.HudBack.Name
                    [Entity.Position == v2 184.0f -256.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Back"]

                 Content.text Gen.name
                    [Entity.Position == v2 -440.0f 200.0f; Entity.Elevation == 9.0f
                     Entity.Text <== gameplay --> fun gameplay ->
                        "HP: " + scstring gameplay.Puppeteer.PlayerPuppetState.HitPoints]

                 Content.label Gen.name
                    [Entity.Position == v2 -447.0f -240.0f; Entity.Size == v2 168.0f 168.0f; Entity.Elevation == 9.0f
                     Entity.LabelImage == asset "Gui" "DetailBacking"]

                 Content.button Simulants.HudDetailUpward.Name
                    [Entity.Position == v2 -387.0f -126.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailUpwardUp"; Entity.DownImage == asset "Gui" "DetailUpwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Upward))]

                 Content.button Simulants.HudDetailRightward.Name
                    [Entity.Position == v2 -336.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailRightwardUp"; Entity.DownImage == asset "Gui" "DetailRightwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Rightward))]

                 Content.button Simulants.HudDetailDownward.Name
                    [Entity.Position == v2 -387.0f -234.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailDownwardUp"; Entity.DownImage == asset "Gui" "DetailDownwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Downward))]

                 Content.button Simulants.HudDetailLeftward.Name
                    [Entity.Position == v2 -438.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailLeftwardUp"; Entity.DownImage == asset "Gui" "DetailLeftwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DetailInput Leftward))]

                 Content.button Simulants.HudWait.Name
                    [Entity.Position == v2 -387.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "W"
                     Entity.Enabled <== gameplay --> fun gameplay -> if gameplay.Round.InProgress then false else true
                     Entity.ClickEvent ==> msg SkipPlayerTurn]
                 
                 Content.feeler Simulants.HudFeeler.Name
                    [Entity.Position == v2 -480.0f -270.0f; Entity.Size == v2 960.0f 540.0f; Entity.Elevation == 9.0f
                     Entity.TouchEvent ==|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))]]]