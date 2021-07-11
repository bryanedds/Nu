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
        | DirectionInput of Direction
        | TurnSkipInput
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
        | HandleSelectionInput of PlayerInput
        | EnterSelectionMode
        | Initialize
        | Update
        | Nil

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

        override this.Channel (_, _) =
            [Simulants.Gameplay.Screen.SelectEvent => msg Initialize
             Simulants.Gameplay.Screen.UpdateEvent => msg Update
             Simulants.Gameplay.Screen.PostUpdateEvent => cmd TrackPlayer]

        override this.Message (gameplay, message, _, world) =
            
            match message with
            | FinishTurns indices ->
                let gameplay =
                    List.fold (fun gameplay index ->
                        let characterTurn = Gameboard.getCharacterTurn index gameplay.Gameboard
                        match characterTurn.TurnStatus with
                        | TurnFinishing ->
                            match characterTurn.TurnType with
                            | AttackTurn _ ->
                                let gameplay = Gameplay.finishMove index gameplay
                                match characterTurn.TurnReactionOpt with
                                | Some (CharacterReaction characterIndex) ->
                                    match Gameboard.tryGetCharacter characterIndex gameplay.Gameboard with
                                    | Some character ->
                                        if character.HitPoints <= 0 then
                                            match character.CharacterIndex with
                                            | PlayerIndex -> Gameplay.updateInputMode (constant DisabledInputMode) gameplay // TODO: reimplement screen transition
                                            | EnemyIndex _ -> Gameplay.tryKillCharacter character.CharacterIndex gameplay
                                        else gameplay
                                    | None -> gameplay
                                | Some (PickupReaction _) -> gameplay
                                | Some (PropReaction coordinates) -> Gameplay.tryCutGrass coordinates gameplay
                                | None -> gameplay
                            | WalkTurn _ -> Gameplay.finishMove index gameplay
                        | _ -> failwith "Non-finishing turns should be filtered out by this point.")
                        gameplay
                        indices
                just gameplay
            
            | TickTurns ->
                let actingCharacters = Gameboard.getActingCharacterIndices gameplay.Gameboard
                let gameplay =
                    List.fold (fun gameplay index ->
                        let characterTurn = Gameboard.getCharacterTurn index gameplay.Gameboard
                        match characterTurn.TurnStatus with
                        | TurnTicking ->
                            let tickCount = gameplay.Time - characterTurn.StartTime
                            match characterTurn.TurnType with
                            | AttackTurn _ ->
                                if tickCount = Constants.Gameplay.ActionTicksMax
                                then Gameplay.tryUpdateCharacterTurnStatus index (constant TurnFinishing) gameplay
                                else gameplay
                            | WalkTurn _ ->
                                if tickCount = dec (int64 Constants.Gameplay.CharacterWalkSteps)
                                then Gameplay.tryUpdateCharacterTurnStatus index (constant TurnFinishing) gameplay
                                else gameplay
                        | _ -> gameplay)
                        gameplay
                        actingCharacters
                let finishingCharacters = List.filter (fun index -> (Gameboard.getCharacterTurn index gameplay.Gameboard).TurnStatus = TurnFinishing) actingCharacters
                withMsg (FinishTurns finishingCharacters) gameplay

            | BeginTurns ->
                let characterIndices = Gameboard.getActingCharacterIndices gameplay.Gameboard
                let gameplay =
                    List.fold (fun gameplay index ->
                        let characterTurn = Gameboard.getCharacterTurn index gameplay.Gameboard
                        match characterTurn.TurnStatus with
                        | TurnBeginning -> Gameplay.tryUpdateCharacterTurnStatus index (constant TurnTicking) gameplay // "TurnTicking" for normal animation; "TurnFinishing" for roguelike mode
                        | _ -> gameplay)
                        gameplay
                        characterIndices
                withCmd (DisplayTurnEffects characterIndices) gameplay

            | MakeEnemyAttack ->
                let index = gameplay.Round.AttackingEnemyGroup.Head
                let gameplay =
                    match Gameboard.tryGetCharacter PlayerIndex gameplay.Gameboard with
                    | Some character when character.IsAlive ->
                        Gameplay.tryAddMove gameplay.Time index (Attack (CharacterReaction PlayerIndex)) gameplay
                    | Some _ | None -> gameplay
                let gameplay = Gameplay.removeHeadFromAttackingEnemyGroup gameplay
                withMsg BeginTurns gameplay

            | MakeEnemiesWalk ->
                let gameplay =
                    List.fold (fun gameplay index ->
                        match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
                        | Some coordinates ->
                            let openDirections = Gameboard.getOpenDirections coordinates gameplay.Gameboard
                            let direction = Gen.random1 4 |> Direction.ofInt
                            if Set.contains direction openDirections
                            then Gameplay.tryAddMove gameplay.Time index (Step direction) gameplay
                            else gameplay
                        | None -> gameplay)
                        gameplay
                        gameplay.Round.WalkingEnemyGroup
                let gameplay = Gameplay.removeWalkingEnemyGroup gameplay
                withMsg BeginTurns gameplay

            | MakeEnemyMoves ->
                let enemyIndices = Gameboard.getEnemyIndices gameplay.Gameboard
                let adjacentEnemies = List.filter (fun x -> Gameplay.areCharactersAdjacent x PlayerIndex gameplay) enemyIndices
                let gameplay = Gameplay.addAttackingEnemyGroup adjacentEnemies gameplay
                let gameplay = Gameplay.createWalkingEnemyGroup gameplay
                match Round.tryGetPlayerMove gameplay.Round with
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
                            let unoccupiedSpaces = Gameboard.getUnoccupiedSpaces gameplay.Gameboard
                            if Set.contains targetCoordinates unoccupiedSpaces
                            then Gameplay.tryAddMove gameplay.Time PlayerIndex (Travel navigationPath) gameplay
                            else gameplay
                        | [] -> failwithumf ()
                    | _ -> gameplay
                if Map.containsKey PlayerIndex gameplay.Round.CharacterMoves then
                    withMsg MakeEnemyMoves gameplay
                else
                    let gameplay = Gameplay.updateRound (Round.updatePlayerContinuity (constant NoContinuity)) gameplay
                    withCmd ListenKeyboard gameplay
            
            | TryMakePlayerMove playerInput ->
                let time = gameplay.Time
                match Gameboard.tryGetCharacterCoordinates PlayerIndex gameplay.Gameboard with
                | Some currentCoordinates ->
                    let targetCoordinatesOpt =
                        match playerInput with
                        | TouchInput touchPosition -> Some (World.mouseToWorld false touchPosition world |> vftovc)
                        | DirectionInput direction -> Some (currentCoordinates + dtovc direction)
                        | _ -> None
                    let gameplay =
                        match targetCoordinatesOpt with
                        | Some targetCoordinates ->
                            if Math.areCoordinatesAdjacent targetCoordinates currentCoordinates then
                                if Set.contains targetCoordinates gameplay.Gameboard.Spaces then
                                    match Map.tryFind targetCoordinates gameplay.Gameboard.Characters with
                                    | Some character -> Gameplay.tryAddMove time PlayerIndex (Attack (CharacterReaction character.CharacterIndex)) gameplay
                                    | None ->
                                        match Map.tryFind targetCoordinates gameplay.Gameboard.Props with
                                        | Some _ -> Gameplay.tryAddMove time PlayerIndex (Attack (PropReaction targetCoordinates)) gameplay
                                        | None ->
                                            let direction = Math.directionToTarget currentCoordinates targetCoordinates
                                            Gameplay.tryAddMove time PlayerIndex (Step direction) gameplay
                                else gameplay
                            else
                                let navigationPathOpt =
                                    let navigationMap = Gameboard.getNavigationMap currentCoordinates gameplay.Gameboard
                                    if Map.containsKey targetCoordinates navigationMap
                                    then NavigationMap.tryMakeNavigationPath currentCoordinates targetCoordinates navigationMap
                                    else None
                                match navigationPathOpt with
                                | Some navigationPath ->
                                    match navigationPath with
                                    | _ :: _ -> Gameplay.tryAddMove time PlayerIndex (Travel navigationPath) gameplay
                                    | [] -> gameplay
                                | None -> gameplay
                        | None -> gameplay
                    if Map.containsKey PlayerIndex gameplay.Round.CharacterMoves
                    then withMsg MakeEnemyMoves gameplay
                    else just gameplay
                | None -> just gameplay

            | SkipPlayerTurn ->
                let gameplay = Gameplay.updateRound (Round.updatePlayerContinuity (constant Waiting)) gameplay
                withMsg MakeEnemyMoves gameplay
            
            | HaltPlayer ->
                let gameplay = Gameplay.tryInterruptPlayer gameplay
                just gameplay

            | TransitionMap direction ->
                let gameplay = Gameplay.clearProps gameplay
                let gameplay = Gameplay.clearEnemies gameplay
                let gameplay = Gameplay.clearPickups gameplay
                let gameplay = Gameplay.transitionFieldMap direction gameplay
                let gameplay = Gameplay.makeProps gameplay
                let gameplay = Gameplay.makeEnemies (Gen.random2 1 6) gameplay
                just gameplay

            | HandleMapChange playerInput ->
                let msg =
                    match playerInput with
                    | DirectionInput direction ->
                        match Gameboard.tryGetCharacterCoordinates PlayerIndex gameplay.Gameboard with
                        | Some currentCoordinates ->
                            let targetOutside =
                                match direction with
                                | Upward -> currentCoordinates.Y = Constants.Layout.FieldMapSizeC.Y - 1
                                | Rightward -> currentCoordinates.X = Constants.Layout.FieldMapSizeC.X - 1
                                | Downward -> currentCoordinates.Y = 0
                                | Leftward -> currentCoordinates.X = 0
                            let possibleInDirection = MetaMap.possibleInDirection direction gameplay.MetaMap
                            let onPathBoundary = MetaMap.onPathBoundary currentCoordinates gameplay.MetaMap
                            if targetOutside && possibleInDirection && onPathBoundary then TransitionMap direction else TryMakePlayerMove playerInput
                        | None -> Nil
                    | _ -> TryMakePlayerMove playerInput
                withMsg msg gameplay
            
            | HandleSelectionInput playerInput ->
                match playerInput with
                | TouchInput touchPosition ->
                    let gameplay = Gameplay.updateInputMode (constant NormalInputMode) gameplay
                    let targetCoordinates = World.mouseToWorld false touchPosition world |> vftovc
                    if Set.contains targetCoordinates gameplay.Gameboard.Spaces then
                        match Gameboard.tryGetCharacterAtCoordinates targetCoordinates gameplay.Gameboard with
                        | Some character when character.IsEnemy ->
                            let gameplay = Gameplay.tryAddMove gameplay.Time PlayerIndex (Shoot (CharacterReaction character.CharacterIndex)) gameplay
                            withMsg MakeEnemyMoves gameplay
                        | Some _ | None -> just gameplay
                    else just gameplay 
                | _ -> just gameplay
            
            | EnterSelectionMode ->
                let gameplay = Gameplay.updateInputMode (constant SelectionInputMode) gameplay
                just gameplay
            
            | Initialize ->
                if gameplay.ShallLoadGame && File.Exists Assets.Global.SaveFilePath then
                    let gameplayStr = File.ReadAllText Assets.Global.SaveFilePath
                    let gameplay = scvalue<Gameplay> gameplayStr
                    just gameplay
                else
                    let gameplay = Gameplay.initial
                    let gameplay = Gameplay.resetFieldMapWithPlayer (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
                    let gameplay = Gameplay.makeProps gameplay
                    let gameplay = Gameplay.makeEnemies (Gen.random2 1 6) gameplay
                    just gameplay

            | Update ->
                let gameplay = Gameplay.advanceTime gameplay
                match Round.getRoundState gameplay.Round with
                | RunningCharacterMoves -> withMsg TickTurns gameplay
                | MakingEnemyAttack -> withMsg MakeEnemyAttack gameplay
                | MakingEnemiesWalk -> withMsg MakeEnemiesWalk gameplay
                | FinishingRound -> withMsg TryTransitionRound gameplay
                | NoRound -> withCmd ListenKeyboard gameplay

            | Nil ->
                just gameplay

        override this.Command (gameplay, command, _, world) =

            match command with
            | DisplayTurnEffects _ ->
                let world =
                    match Gameboard.tryGetCharacterTurn PlayerIndex gameplay.Gameboard with
                    | Some turn ->
                        match turn.TurnType with
                        | AttackTurn attackType ->
                            if turn.StartTime = gameplay.Time then
                                match attackType with
                                | NormalAttack ->
                                    let effect = Effects.makeSwordStrikeEffect turn.Direction
                                    let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Gameplay.Scene.Group world
                                    let world = entity.SetEffect effect world
                                    let world = entity.SetSize (v2Dup 144.0f) world
                                    let world = entity.SetPosition ((vctovf turn.OriginCoordinates) - Constants.Layout.TileSize) world
                                    let world = entity.SetElevation Constants.Layout.EffectElevation world
                                    entity.SetSelfDestruct true world
                                | MissileAttack ->
                                    match turn.TurnReactionOpt with
                                    | Some (CharacterReaction characterIndex) ->
                                        match Gameboard.tryGetCharacterCoordinates characterIndex gameplay.Gameboard with
                                        | Some characterCoordinates ->
                                            let effect = Effects.makeMagicMissileImpactEffect ()
                                            let (entity, world) = World.createEntity<EffectDispatcher> None DefaultOverlay Simulants.Gameplay.Scene.Group world
                                            let world = entity.SetEffect effect world
                                            let world = entity.SetSize Constants.Layout.TileSize world
                                            let world = entity.SetPosition (vctovf characterCoordinates) world
                                            let world = entity.SetElevation Constants.Layout.EffectElevation world
                                            entity.SetSelfDestruct true world
                                        | None -> world
                                    | _ -> world
                            else world
                        | _ -> world
                    | None -> world
                withMsg TickTurns world
            
            | HandlePlayerInput playerInput ->
                if Round.notInProgress gameplay.Round then
                    match gameplay.InputMode with
                    | NormalInputMode ->
                        let msg =
                            match playerInput with
                            | TurnSkipInput -> SkipPlayerTurn
                            | _ -> HandleMapChange playerInput
                        withMsg msg world
                    | SelectionInputMode -> withMsg (HandleSelectionInput playerInput) world
                    | DisabledInputMode -> just world
                else just world

            | Save ->
                let gameplayStr = scstring gameplay
                try File.WriteAllText (Assets.Global.SaveFilePath, gameplayStr)
                with exn -> Log.debug ("Failed to write save file at '" + Assets.Global.SaveFilePath + "' due to: " + scstring exn)
                just world

            | ListenKeyboard ->
                if KeyboardState.isKeyDown KeyboardKey.Up then withCmd (HandlePlayerInput (DirectionInput Upward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Right then withCmd (HandlePlayerInput (DirectionInput Rightward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Down then withCmd (HandlePlayerInput (DirectionInput Downward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Left then withCmd (HandlePlayerInput (DirectionInput Leftward)) world
                elif KeyboardState.isKeyDown KeyboardKey.Space then withCmd (HandlePlayerInput TurnSkipInput) world
                else just world

            | TrackPlayer ->
                let playerCenter = Simulants.Gameplay.Scene.Player.GetCenter world
                let fieldBounds = Simulants.Gameplay.Scene.Field.GetBounds world
                let world = World.setEyeCenter playerCenter world
                let world = World.constrainEyeBounds fieldBounds world
                just world

            | Nop ->
                just world

        override this.Content (gameplay, screen) =

            // scene group
            [Content.groupIfScreenSelected screen (fun _ _ ->
                Content.group Simulants.Gameplay.Scene.Group.Name []

                    // field
                    [Content.entity<FieldDispatcher> Simulants.Gameplay.Scene.Field.Name
                       [Entity.Field <== gameplay --> fun gameplay -> gameplay.Field]

                     // pickups
                     Content.entities gameplay
                        (fun gameplay _ -> gameplay.Gameboard.Pickups)
                        (fun pickups _ -> pickups |> Map.toSeqBy (fun positionM pickupType -> Pickup.ofPickupType pickupType positionM) |> Map.indexed)
                        (fun index pickup _ -> Content.entity<PickupDispatcher> ("Pickup+" + scstring index) [Entity.Size == Constants.Layout.TileSize; Entity.Pickup <== pickup])

                     // props
                     Content.entities gameplay
                        (fun gameplay _ -> (gameplay.Gameboard.Props, gameplay.Gameboard, gameplay.Time))
                        (fun (props, puppeteer, time) _ -> Gameboard.getPropMap props puppeteer time)
                        (fun index prop _ -> Content.entity<PropDispatcher> ("Prop+" + scstring index) [Entity.Size == Constants.Layout.TileSize; Entity.Prop <== prop])

                     // characters
                     Content.entities gameplay
                        (fun gameplay _ -> (gameplay.Gameboard.Characters, gameplay.Gameboard, gameplay.Time))
                        (fun (characters, puppeteer, time) _ -> Gameboard.getCharacterMap characters puppeteer time)
                        (fun index character _ ->
                            let name =
                                match index with
                                | 0 -> Simulants.Gameplay.Scene.Player.Name
                                | _ -> "Enemy+" + scstring index
                            Content.entity<CharacterDispatcher> name
                                [Entity.CharacterAnimationSheet <== character --> fun (_, _, _) -> match index with 0 -> Assets.Gameplay.PlayerImage | _ -> Assets.Gameplay.GoopyImage // TODO: pull this from data
                                 Entity.CharacterAnimationState <== character --> fun (_, characterAnimationState, _) -> characterAnimationState
                                 Entity.CharacterAnimationTime <== character --> fun (_, _, time) -> time
                                 Entity.Position <== character --> fun (position, _, _) -> position])])

             // gui group
             Content.group Simulants.Gameplay.Gui.Group.Name []

                [// back button
                 Content.button Simulants.Gameplay.Gui.Back.Name
                    [Entity.Position == v2 184.0f -256.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Back"]

                 // halt button
                 Content.button Gen.name
                    [Entity.Position == v2 184.0f -144.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Halt"
                     Entity.Enabled <== gameplay --> fun gameplay -> gameplay.Round.IsPlayerTraveling
                     Entity.ClickEvent ==> msg HaltPlayer]

                 // save button
                 Content.button Gen.name
                    [Entity.Position == v2 184.0f -200.0f; Entity.Size == v2 288.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "Save Game"
                     Entity.Enabled <== gameplay --> fun gameplay -> not (Round.inProgress gameplay.Round) && gameplay.InputMode = NormalInputMode
                     Entity.ClickEvent ==> cmd Save]

                 // HP
                 Content.text Gen.name
                    [Entity.Position == v2 -440.0f 200.0f; Entity.Elevation == 9.0f
                     Entity.Text <== gameplay --> fun gameplay -> "HP: " + scstring (Gameboard.getPlayer gameplay.Gameboard).HitPoints]

                 // detail backdrop
                 Content.label Gen.name
                    [Entity.Position == v2 -447.0f -240.0f; Entity.Size == v2 168.0f 168.0f; Entity.Elevation == 9.0f
                     Entity.LabelImage == asset "Gui" "DetailBackdrop"]

                 // detail up
                 Content.button Gen.name
                    [Entity.Position == v2 -387.0f -126.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailUpwardUp"; Entity.DownImage == asset "Gui" "DetailUpwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DirectionInput Upward))]

                 // detail right
                 Content.button Gen.name
                    [Entity.Position == v2 -336.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailRightwardUp"; Entity.DownImage == asset "Gui" "DetailRightwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DirectionInput Rightward))]

                 // detail down
                 Content.button Gen.name
                    [Entity.Position == v2 -387.0f -234.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailDownwardUp"; Entity.DownImage == asset "Gui" "DetailDownwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DirectionInput Downward))]

                 // detail left
                 Content.button Gen.name
                    [Entity.Position == v2 -438.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.UpImage == asset "Gui" "DetailLeftwardUp"; Entity.DownImage == asset "Gui" "DetailLeftwardDown"
                     Entity.ClickSoundOpt == None
                     Entity.ClickEvent ==> cmd (HandlePlayerInput (DirectionInput Leftward))]

                 // wait button
                 Content.button Gen.name
                    [Entity.Position == v2 -387.0f -177.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f
                     Entity.Text == "W"
                     Entity.Enabled <== gameplay --> fun gameplay -> if Round.inProgress gameplay.Round then false else true
                     Entity.ClickEvent ==> cmd (HandlePlayerInput TurnSkipInput)]

                 // item bar
                 Content.panel Gen.name
                    [Entity.Position == v2 400.0f 200.0f; Entity.Size == v2 48.0f 48.0f; Entity.Elevation == 10.0f]
                        [Content.entities gameplay
                           (fun gameplay _ -> gameplay.Inventory)
                           (fun inventory _ -> if Inventory.containsItem (Special MagicMissile) inventory then Map.singleton 0 () else Map.empty )
                           (fun _ _ _ ->
                               Content.button "MagicMissileButton"
                                   [Entity.PositionLocal == v2Zero; Entity.Size == v2 48.0f 48.0f; Entity.ElevationLocal == 1.0f
                                    Entity.UpImage == asset "Gameplay" "MagicMissile"; Entity.DownImage == asset "Gameplay" "MagicMissile"
                                    Entity.ClickEvent ==> msg EnterSelectionMode])]

                 // input feeler
                 Content.feeler Gen.name
                    [Entity.Position == v2 -480.0f -270.0f; Entity.Size == v2 960.0f 540.0f; Entity.Elevation == 9.0f
                     Entity.TouchEvent ==|> fun evt -> cmd (HandlePlayerInput (TouchInput evt.Data))]]]