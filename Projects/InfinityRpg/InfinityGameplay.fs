namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] MapModeler =
    { FieldMapUnits : Map<Vector2i, FieldMapUnit>
      CurrentFieldOffset : Vector2i }

    static member empty =
        { FieldMapUnits = Map.empty
          CurrentFieldOffset = v2iZero }

    member this.AddFieldMapUnit fieldMapUnit =
        let fieldMapUnits = Map.add fieldMapUnit.OffsetCount fieldMapUnit this.FieldMapUnits
        { this with FieldMapUnits = fieldMapUnits; CurrentFieldOffset = fieldMapUnit.OffsetCount }

    member this.Current =
        this.FieldMapUnits.[this.CurrentFieldOffset]

    member this.OffsetInDirection direction =
        this.CurrentFieldOffset + dtovm direction
    
    member this.ExistsInDirection direction =
        Map.containsKey (this.OffsetInDirection direction) this.FieldMapUnits
    
    member this.NextOffset =
        if this.Current.IsHorizontal
        then this.OffsetInDirection Rightward
        else this.OffsetInDirection Upward
    
    member this.NextOffsetInDirection direction =
        this.NextOffset = this.OffsetInDirection direction

    member this.PossibleInDirection direction =
        this.ExistsInDirection direction || this.NextOffsetInDirection direction
    
    member this.MoveCurrent direction =
        { this with CurrentFieldOffset = this.OffsetInDirection direction }
    
    member this.MakeFieldMapUnit =
        this.AddFieldMapUnit (FieldMapUnit.make (Some this.Current))
    
    member this.Transition direction =
        if this.ExistsInDirection direction
        then this.MoveCurrent direction
        else this.MakeFieldMapUnit
    
    static member make =
        MapModeler.empty.AddFieldMapUnit (FieldMapUnit.make None)

type SingleRoundMove =
    | Step of Direction
    | Attack of CharacterIndex

type [<NoComparison>] Move =
    | SingleRoundMove of SingleRoundMove
    | MultiRoundMove of NavigationNode list

    member this.MakeTurn positionM =
        match this with
        | SingleRoundMove singleRoundMove ->
            match singleRoundMove with
            | Step direction -> Turn.makeNavigation None positionM direction
            | Attack index -> Turn.makeAttack index
        | MultiRoundMove path ->
            let direction = Math.directionToTarget positionM path.Head.PositionM
            Turn.makeNavigation (Some path) positionM direction

type [<ReferenceEquality; NoComparison>] Chessboard =
    { PassableCoordinates : Map<Vector2i, PickupType Option>
      CharacterCoordinates : Map<CharacterIndex, Vector2i>
      CurrentMoves : Map<CharacterIndex, Move> }

    static member empty =
        { PassableCoordinates = Map.empty
          CharacterCoordinates = Map.empty
          CurrentMoves = Map.empty }

    member this.EnemyCoordinates =
        Map.filter (fun (k : CharacterIndex) _ -> k.IsEnemy) this.CharacterCoordinates

    member this.PlayerCoordinates =
        Map.filter (fun (k : CharacterIndex) _ -> not k.IsEnemy) this.CharacterCoordinates
    
    member this.PickupItems =
        Map.filter (fun _ v -> v <> None) this.PassableCoordinates

    member this.EnemyCount =
        this.EnemyCoordinates.Count

    member this.PickupCount =
        this.PickupItems.Count
    
    member this.AvailableCoordinates =
        let occupiedCoordinates = Map.toValueSeq this.CharacterCoordinates
        let passableCoordinates = Map.toKeyList this.PassableCoordinates
        List.except occupiedCoordinates passableCoordinates

    member this.OpenDirections coordinates =
        List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovm d))) this.AvailableCoordinates) [Upward; Rightward; Downward; Leftward]
    
    static member updatePassableCoordinates newValue chessboard =
        { chessboard with PassableCoordinates = newValue }
    
    static member updateCharacterCoordinates newValue chessboard =
        { chessboard with CharacterCoordinates = newValue }

    static member updateCurrentMoves newValue chessboard =
        { chessboard with CurrentMoves = newValue }

    static member characterExists index chessboard =
        Map.exists (fun k _ -> k = index) chessboard.CharacterCoordinates
    
    static member pickupAtCoordinates coordinates chessboard =
        match chessboard.PassableCoordinates.[coordinates] with
        | Some _ -> true
        | None -> false
    
    static member updateCoordinatesValue newValue coordinates chessboard =
        let passableCoordinates = Map.add coordinates newValue chessboard.PassableCoordinates
        Chessboard.updatePassableCoordinates passableCoordinates chessboard
    
    static member clearPickups _ _ chessboard =
        let passableCoordinates = Map.map (fun _ _ -> None) chessboard.PassableCoordinates
        Chessboard.updatePassableCoordinates passableCoordinates chessboard
    
    // used for both adding and relocating
    static member placeCharacter index coordinates (chessboard : Chessboard) =
        if List.exists (fun x -> x = coordinates) chessboard.AvailableCoordinates then
            let characterCoordinates = Map.add index coordinates chessboard.CharacterCoordinates
            Chessboard.updateCharacterCoordinates characterCoordinates chessboard
        else failwith "character placement failed; coordinates unavailable"

    static member removeCharacter index _ chessboard =
        let characterCoordinates = Map.remove index chessboard.CharacterCoordinates
        Chessboard.updateCharacterCoordinates characterCoordinates chessboard
    
    static member clearEnemies _ _ (chessboard : Chessboard) =
        Chessboard.updateCharacterCoordinates chessboard.PlayerCoordinates chessboard
    
    static member addMove index move chessboard =
        let currentMoves = Map.add index move chessboard.CurrentMoves
        Chessboard.updateCurrentMoves currentMoves chessboard
    
    static member setPassableCoordinates _ fieldMap chessboard =
        let passableCoordinates = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> None)
        Chessboard.updatePassableCoordinates passableCoordinates chessboard                    

type [<ReferenceEquality; NoComparison>] Gameplay =
    { MapModeler : MapModeler
      Chessboard : Chessboard
      ShallLoadGame : bool
      Field : Field
      Pickups : Pickup list
      Enemies : Character list
      Player : Character }

    static member initial =
        { MapModeler = MapModeler.make
          Chessboard = Chessboard.empty
          ShallLoadGame = false
          Field = Field.initial
          Pickups = []
          Enemies = []
          Player = Character.initial }

    member this.PickupCount =
        this.Pickups.Length

    member this.EnemyCount =
        this.Enemies.Length
    
    static member updateMapModeler newValue gameplay =
        { gameplay with MapModeler = newValue }
    
    static member updateField newValue gameplay =
        { gameplay with Field = newValue }

    static member updatePickups newValue gameplay =
        { gameplay with Pickups = newValue }
    
    static member updateEnemies newValue gameplay =
        { gameplay with Enemies = newValue }

    static member updatePlayer newValue gameplay =
        { gameplay with Player = newValue }
    
    static member getCharacters gameplay =
        gameplay.Player :: gameplay.Enemies
    
    static member pickupAtCoordinates coordinates gameplay =
        gameplay.Pickups |> List.exists (fun pickup -> pickup.Position = vmtovf coordinates)

    static member characterExists index gameplay =
        Gameplay.getCharacters gameplay |> List.exists (fun gameplay -> gameplay.Index = index)
    
    static member tryGetCharacterByIndex index gameplay =
        Gameplay.getCharacters gameplay |> List.tryFind (fun gameplay -> gameplay.Index = index)
    
    static member getCharacterByIndex index gameplay =
        Gameplay.tryGetCharacterByIndex index gameplay |> Option.get

    static member getTurn index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).Turn

    static member getCharacterState index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).CharacterState
    
    static member getTurnStatus index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).TurnStatus
    
    static member getCharacterActivityState index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).CharacterActivityState

    static member getCharacterAnimationState index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).CharacterAnimationState
    
    static member getPosition index gameplay =
        (Gameplay.getCharacterByIndex index gameplay).Position
    
    static member getEnemyIndices gameplay =
        List.map (fun gameplay -> gameplay.Index) gameplay.Enemies

    static member getOpponentIndices index gameplay =
        match index with
        | PlayerIndex -> Gameplay.getEnemyIndices gameplay
        | _ -> [PlayerIndex]
    
    static member getEnemyTurns gameplay =
        List.map (fun enemy -> enemy.Turn) gameplay.Enemies
    
    static member getCharacterTurns gameplay =
        Gameplay.getCharacters gameplay |> List.map (fun character -> character.TurnStatus)
    
    static member anyTurnsInProgress gameplay =
        Gameplay.getCharacterTurns gameplay |> List.exists (fun turnStatus -> turnStatus <> Idle)
    
    static member saveImpossible gameplay =
        Gameplay.anyTurnsInProgress gameplay ||
        KeyboardState.isKeyDown KeyboardKey.Up ||
        KeyboardState.isKeyDown KeyboardKey.Right ||
        KeyboardState.isKeyDown KeyboardKey.Down ||
        KeyboardState.isKeyDown KeyboardKey.Left
    
    static member updateCharacterBy updater index newValue gameplay =
        match index with
        | PlayerIndex ->
            let player = updater newValue gameplay.Player
            Gameplay.updatePlayer player gameplay
        | EnemyIndex _ as index ->
            let enemies =
                gameplay.Enemies |>
                List.map (fun enemy -> if enemy.Index = index then updater newValue enemy else enemy)
            Gameplay.updateEnemies enemies gameplay
    
    static member updateTurn index newValue gameplay =
        Gameplay.updateCharacterBy Character.updateTurn index newValue gameplay

    static member updateCharacterState index newValue gameplay =
        Gameplay.updateCharacterBy Character.updateCharacterState index newValue gameplay
    
    static member updateTurnStatus index newValue gameplay =
        Gameplay.updateCharacterBy Character.updateTurnStatus index newValue gameplay
    
    static member updateCharacterActivityState index newValue gameplay =
        Gameplay.updateCharacterBy Character.updateCharacterActivityState index newValue gameplay

    static member updateCharacterAnimationState index newValue gameplay =
        Gameplay.updateCharacterBy Character.updateCharacterAnimationState index newValue gameplay

    static member updatePosition index newValue gameplay =
        Gameplay.updateCharacterBy Character.updatePosition index newValue gameplay
    
    static member updateEnemiesBy updater newValues gameplay =
        let enemies = List.map2 (fun newValue gameplay -> updater newValue gameplay) newValues gameplay.Enemies
        Gameplay.updateEnemies enemies gameplay

    static member updateEnemyActivityStates newValues gameplay =
        Gameplay.updateEnemiesBy Character.updateCharacterActivityState newValues gameplay
    
    static member getCoordinates index gameplay =
        gameplay.Chessboard.CharacterCoordinates.[index]

    static member getIndexByCoordinates coordinates gameplay =
        Map.findKey (fun _ x -> x = coordinates) gameplay.Chessboard.CharacterCoordinates

    static member getCurrentMove index gameplay =
        gameplay.Chessboard.CurrentMoves.[index]
    
    static member createPlayer gameplay =
        let coordinates = Gameplay.getCoordinates PlayerIndex gameplay
        let player = Character.makePlayer coordinates
        Gameplay.updatePlayer player gameplay

    // a basic sync mechanism that relies on never adding and removing *at the same time*
    static member syncLists (gameplay : Gameplay) =
        let chessboard = gameplay.Chessboard
        let gameplay =
            if gameplay.PickupCount <> chessboard.PickupCount then
                let pickups =
                    if gameplay.PickupCount > chessboard.PickupCount then
                        List.filter (fun (pickup : Pickup) -> Chessboard.pickupAtCoordinates (vftovm pickup.Position) chessboard) gameplay.Pickups
                    else 
                        let generator k _ = Pickup.makeHealth k
                        let pickups = Map.filter (fun k _ -> not (Gameplay.pickupAtCoordinates k gameplay)) chessboard.PickupItems |> Map.toListBy generator
                        pickups @ gameplay.Pickups
                Gameplay.updatePickups pickups gameplay
            else gameplay

        if gameplay.EnemyCount <> chessboard.EnemyCount then
            let enemies =
                if gameplay.EnemyCount > chessboard.EnemyCount then
                    List.filter (fun (character : Character) -> Chessboard.characterExists character.Index chessboard) gameplay.Enemies
                else
                    let generator k v = Character.makeEnemy k v
                    let enemies = Map.filter (fun k _ -> not (Gameplay.characterExists k gameplay)) chessboard.EnemyCoordinates |> Map.toListBy generator
                    enemies @ gameplay.Enemies
            Gameplay.updateEnemies enemies gameplay
        else gameplay

    // if updater takes index, index is arg1; if updater takes coordinates, coordinates is arg2
    static member updateChessboardBy updater arg1 arg2 gameplay =
        let chessboard = updater arg1 arg2 gameplay.Chessboard
        let gameplay = { gameplay with Chessboard = chessboard }
        Gameplay.syncLists gameplay
    
    static member relocateCharacter index coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.placeCharacter index coordinates gameplay
    
    static member addMove index (move : Move) gameplay =
        Gameplay.updateChessboardBy Chessboard.addMove index move gameplay
    
    static member addHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.updateCoordinatesValue (Some Health) coordinates gameplay

    static member removeHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.updateCoordinatesValue None coordinates gameplay
    
    static member clearPickups gameplay =
        Gameplay.updateChessboardBy Chessboard.clearPickups () () gameplay
    
    static member removeEnemy index gameplay =
        let coordinates = Gameplay.getCoordinates index gameplay
        let gameplay = Gameplay.addHealth coordinates gameplay
        Gameplay.updateChessboardBy Chessboard.removeCharacter index () gameplay

    static member clearEnemies gameplay =
        Gameplay.updateChessboardBy Chessboard.clearEnemies () () gameplay

    static member unpackMove index gameplay =
        let move = Gameplay.getCurrentMove index gameplay
        let turn = Gameplay.getCoordinates index gameplay |> move.MakeTurn
        let gameplay = Gameplay.updateTurn index turn gameplay
        Gameplay.updateTurnStatus index TurnPending gameplay
    
    static member finishMove index gameplay =
        let gameplay = Gameplay.updateTurn index NoTurn gameplay
        let gameplay = Gameplay.updateCharacterActivityState index NoActivity gameplay
        Gameplay.updateTurnStatus index Idle gameplay
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay = Gameplay.updateCharacterState index { gameplay.Player.CharacterState with HitPoints = 30 } gameplay
            Gameplay.removeHealth coordinates gameplay
        | _ -> gameplay
    
    static member applyStep index direction gameplay =
        let coordinates = (Gameplay.getCoordinates index gameplay) + dtovm direction
        let gameplay =
            if Chessboard.pickupAtCoordinates coordinates gameplay.Chessboard then
                Gameplay.tryPickupHealth index coordinates gameplay
            else gameplay
        Gameplay.relocateCharacter index coordinates gameplay
    
    static member applyAttack reactorIndex gameplay =
        let reactorDamage = 4 // NOTE: just hard-coding damage for now
        let reactorState = Gameplay.getCharacterState reactorIndex gameplay
        Gameplay.updateCharacterState reactorIndex { reactorState with HitPoints = reactorState.HitPoints - reactorDamage } gameplay
    
    static member stopTraveler reactorIndex gameplay =
        match Gameplay.getTurn reactorIndex gameplay with
        | NavigationTurn navigationDescriptor ->
            Gameplay.updateTurn reactorIndex (NavigationTurn navigationDescriptor.CancelNavigation) gameplay
        | _ -> gameplay
    
    static member applyMove index gameplay =
        let move = Gameplay.getCurrentMove index gameplay
        match move with
        | SingleRoundMove singleRoundMove ->
            match singleRoundMove with
            | Step direction -> Gameplay.applyStep index direction gameplay
            | Attack reactorIndex ->
                let gameplay = Gameplay.applyAttack reactorIndex gameplay
                Gameplay.stopTraveler reactorIndex gameplay
        | MultiRoundMove path ->
            match path with
            | head :: _ ->
                let currentCoordinates = Gameplay.getCoordinates index gameplay
                let direction = Math.directionToTarget currentCoordinates head.PositionM
                Gameplay.applyStep index direction gameplay
            | [] -> failwithumf ()
    
    static member activateCharacter index gameplay =
        let activity = CharacterActivityState.makeFromTurn (Gameplay.getTurn index gameplay)
        let gameplay = Gameplay.updateCharacterActivityState index activity gameplay
        Gameplay.updateTurnStatus PlayerIndex TurnBeginning gameplay
    
    static member setFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboardBy Chessboard.setPassableCoordinates () fieldMap gameplay
        let field = { FieldMapNp = fieldMap }
        Gameplay.updateField field gameplay

    static member transitionMap direction gameplay =
        let mapModeler = gameplay.MapModeler.Transition direction
        Gameplay.updateMapModeler mapModeler gameplay

    static member setCharacterPositionToCoordinates index gameplay =
        let position = Gameplay.getCoordinates index gameplay |> vmtovf
        Gameplay.updatePosition index position gameplay
    
    static member yankPlayer coordinates gameplay =
        let gameplay = Gameplay.relocateCharacter PlayerIndex coordinates gameplay
        Gameplay.setCharacterPositionToCoordinates PlayerIndex gameplay
    
    static member makePlayer gameplay =
        let gameplay = Gameplay.updateChessboardBy Chessboard.placeCharacter PlayerIndex v2iZero gameplay
        Gameplay.createPlayer gameplay

    static member makeEnemy index gameplay =
        let availableCoordinates = gameplay.Chessboard.AvailableCoordinates
        let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
        Gameplay.updateChessboardBy Chessboard.placeCharacter index coordinates gameplay

    static member makeEnemies quantity gameplay =
        let quantity = quantity - 1
        let rec recursion count gameplay =
            let gameplay = Gameplay.makeEnemy (EnemyIndex count) gameplay
            if count = quantity then gameplay
            else recursion (count + 1) gameplay
        recursion 0 gameplay
    
    static member forEachIndex updater indices gameplay =
        let rec recursion (indices : CharacterIndex list) gameplay =
            if indices.Length = 0 then gameplay
            else
                let index = indices.Head
                let characterOpt = Gameplay.tryGetCharacterByIndex index gameplay
                let gameplay =
                    match characterOpt with
                    | None -> gameplay
                    | Some _ -> updater index gameplay
                recursion indices.Tail gameplay
        recursion indices gameplay