namespace InfinityRpg
open System
open System.Numerics
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

type [<NoComparison>] Move =
    | Step of Direction
    | Attack of CharacterIndex
    | Travel of NavigationNode list

    member this.TruncatePath =
        match this with
        | Travel (head :: _) -> Travel [head]
        | _ -> this

type FieldSpace =
    { CharacterOpt : (CharacterIndex * CharacterState) Option
      PickupItemOpt : PickupType Option }

    member this.GetCharacter =
        match this.CharacterOpt with Some character -> character | None -> failwithumf ()
    
    member this.GetCharacterIndex =
        match this.CharacterOpt with Some (index, _) -> index | None -> failwithumf ()
    
    member this.ContainsCharacter =
        match this.CharacterOpt with Some _ -> true | None -> false

    static member containsEnemy fieldSpace =
        match fieldSpace.CharacterOpt with
        | None -> false // more efficient order
        | Some (i, _) -> i.IsEnemy
    
    static member containsSpecifiedCharacter index fieldSpace =
        match fieldSpace.CharacterOpt with
        | None -> false // more efficient order
        | Some (i, _) -> i = index

    member this.ContainsPickup =
        match this.PickupItemOpt with Some _ -> true | None -> false
    
    static member containsPickup (fieldSpace : FieldSpace) =
        fieldSpace.ContainsPickup

    static member updateCharacterOpt updater fieldSpace =
        { fieldSpace with CharacterOpt = updater fieldSpace.CharacterOpt }
    
    static member updatePickupItemOpt updater fieldSpace =
        { fieldSpace with PickupItemOpt = updater fieldSpace.PickupItemOpt }
    
    static member addCharacter character fieldSpace =
        FieldSpace.updateCharacterOpt (constant (Some character)) fieldSpace

    static member updateCharacterStateInternal updater characterOpt =
        match characterOpt with
        | Some character -> Some (fst character, updater (snd character))
        | None -> failwithumf ()
    
    static member updateCharacterState updater fieldSpace =
        FieldSpace.updateCharacterOpt (FieldSpace.updateCharacterStateInternal updater) fieldSpace
    
    static member removeCharacter fieldSpace =
        FieldSpace.updateCharacterOpt (constant None) fieldSpace
    
    static member addHealth fieldSpace =
        FieldSpace.updatePickupItemOpt (constant (Some Health)) fieldSpace
    
    static member removePickup fieldSpace =
        FieldSpace.updatePickupItemOpt (constant None) fieldSpace
    
    static member empty =
        { CharacterOpt = None
          PickupItemOpt = None }

type [<ReferenceEquality; NoComparison>] Chessboard =
    { PassableCoordinates : Map<Vector2i, FieldSpace>
      CurrentMoves : Map<CharacterIndex, Move> }

    static member empty =
        { PassableCoordinates = Map.empty
          CurrentMoves = Map.empty }

    member this.EnemyCoordinates =
        Map.filter (fun _ v -> FieldSpace.containsEnemy v ) this.PassableCoordinates

    member this.PickupItems =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsPickup) this.PassableCoordinates

    member this.EnemyCount =
        this.EnemyCoordinates.Count

    member this.PickupCount =
        this.PickupItems.Count
    
    member this.OccupiedCoordinates =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsCharacter) this.PassableCoordinates |> Map.toKeyList
    
    member this.AvailableCoordinates =
        Map.filter (fun _ (v : FieldSpace) -> not v.ContainsCharacter) this.PassableCoordinates |> Map.toKeyList

    member this.OpenDirections coordinates =
        List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovm d))) this.AvailableCoordinates) [Upward; Rightward; Downward; Leftward]
    
    static member updatePassableCoordinates updater chessboard =
        { chessboard with PassableCoordinates = updater chessboard.PassableCoordinates }
    
    static member updateCurrentMoves updater chessboard =
        { chessboard with CurrentMoves = updater chessboard.CurrentMoves }

    static member characterExists index chessboard =
        Map.exists (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.PassableCoordinates
    
    static member pickupAtCoordinates coordinates chessboard =
        chessboard.PassableCoordinates.[coordinates].ContainsPickup
    
    static member updateByCoordinatesInternal coordinates updater (passableCoordinates : Map<Vector2i, FieldSpace>) =
        Map.add coordinates (updater passableCoordinates.[coordinates]) passableCoordinates

    static member updateByPredicateInternal predicate updater (passableCoordinates : Map<Vector2i, FieldSpace>) =
        Map.map (fun _ v -> if predicate v then updater v else v) passableCoordinates
    
    static member updateByCoordinates coordinates updater chessboard =
        Chessboard.updatePassableCoordinates (Chessboard.updateByCoordinatesInternal coordinates updater) chessboard
    
    static member updateByPredicate predicate updater chessboard =
        Chessboard.updatePassableCoordinates (Chessboard.updateByPredicateInternal predicate updater) chessboard
    
    static member addHealth _ coordinates chessboard =
        Chessboard.updateByCoordinates coordinates FieldSpace.addHealth chessboard

    static member removePickup _ coordinates chessboard =
        Chessboard.updateByCoordinates coordinates FieldSpace.removePickup chessboard
    
    static member clearPickups _ _ chessboard =
        Chessboard.updateByPredicate FieldSpace.containsPickup FieldSpace.removePickup chessboard
    
    static member getCharacterCoordinates index chessboard =
        Map.findKey (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.PassableCoordinates

    static member getCharacterAtCoordinates coordinates chessboard =
        chessboard.PassableCoordinates.[coordinates].GetCharacter
    
    static member getCharacterState index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.getCharacterAtCoordinates coordinates chessboard |> snd
    
    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addCharacter character) chessboard

    static member updateCharacterState index updater chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates (FieldSpace.updateCharacterState updater) chessboard
    
    static member removeCharacter index _ chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates FieldSpace.removeCharacter chessboard
    
    static member relocateCharacter index coordinates (chessboard : Chessboard) =
        let oldCoordinates = Chessboard.getCharacterCoordinates index chessboard
        let character = Chessboard.getCharacterAtCoordinates oldCoordinates chessboard
        let chessboard = Chessboard.updateByCoordinates oldCoordinates FieldSpace.removeCharacter chessboard
        Chessboard.addCharacter character coordinates chessboard
    
    static member clearEnemies _ _ (chessboard : Chessboard) =
        Chessboard.updateByPredicate FieldSpace.containsEnemy FieldSpace.removeCharacter chessboard
    
    static member addMove index move chessboard =
        let currentMoves = Map.add index move chessboard.CurrentMoves
        Chessboard.updateCurrentMoves (constant currentMoves) chessboard
    
    static member removeMove index _ chessboard =
        let currentMoves = Map.remove index chessboard.CurrentMoves
        Chessboard.updateCurrentMoves (constant currentMoves) chessboard
    
    static member truncatePlayerPath _ _ chessboard =
        let move = chessboard.CurrentMoves.[PlayerIndex].TruncatePath
        Chessboard.addMove PlayerIndex move chessboard
    
    static member setPassableCoordinates _ fieldMap chessboard =
        let passableCoordinates = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> FieldSpace.empty)
        Chessboard.updatePassableCoordinates (constant passableCoordinates) chessboard

    static member transitionMap _ fieldMap chessboard =
        let playerCoordinates = Chessboard.getCharacterCoordinates PlayerIndex chessboard
        let player = Chessboard.getCharacterAtCoordinates playerCoordinates chessboard
        let chessboard = Chessboard.setPassableCoordinates () fieldMap chessboard
        Chessboard.addCharacter player playerCoordinates chessboard

type [<ReferenceEquality; NoComparison>] Gameplay =
    { MapModeler : MapModeler
      Chessboard : Chessboard
      ShallLoadGame : bool
      Field : Field
      CharacterTurns : Turn list
      LastActionStart : int64
      Enemies : Character list
      Player : Character }

    static member initial =
        { MapModeler = MapModeler.make
          Chessboard = Chessboard.empty
          ShallLoadGame = false
          Field = Field.initial
          CharacterTurns = []
          LastActionStart = 0L
          Enemies = []
          Player = Character.initial }

    member this.EnemyCount =
        this.Enemies.Length
    
    static member updateMapModeler updater gameplay =
        { gameplay with MapModeler = updater gameplay.MapModeler }
    
    static member updateField updater gameplay =
        { gameplay with Field = updater gameplay.Field }

    static member updateCharacterTurns updater gameplay =
        { gameplay with CharacterTurns = updater gameplay.CharacterTurns }
    
    static member updateLastActionStart updater gameplay =
        { gameplay with LastActionStart = updater gameplay.LastActionStart }

    static member updateEnemies updater gameplay =
        { gameplay with Enemies = updater gameplay.Enemies }

    static member updatePlayer updater gameplay =
        { gameplay with Player = updater gameplay.Player }
    
    static member getCharacters gameplay =
        gameplay.Player :: gameplay.Enemies
    
    static member characterExists index gameplay =
        Gameplay.getCharacters gameplay |> List.exists (fun gameplay -> gameplay.Index = index)
    
    static member tryGetCharacterByIndex index gameplay =
        Gameplay.getCharacters gameplay |> List.tryFind (fun gameplay -> gameplay.Index = index)
    
    static member getCharacterByIndex index gameplay =
        Gameplay.tryGetCharacterByIndex index gameplay |> Option.get

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
    
    static member anyTurnsInProgress gameplay = 
        List.notEmpty gameplay.CharacterTurns
    
    static member updateCharacterBy by index updater gameplay =
        match index with
        | PlayerIndex ->
            let player = by updater gameplay.Player
            Gameplay.updatePlayer (constant player) gameplay
        | EnemyIndex _ as index ->
            let enemies =
                gameplay.Enemies |>
                List.map (fun enemy -> if enemy.Index = index then by updater enemy else enemy)
            Gameplay.updateEnemies (constant enemies) gameplay
    
    static member updateCharacterAnimationState index updater gameplay =
        Gameplay.updateCharacterBy Character.updateCharacterAnimationState index updater gameplay

    static member updatePosition index updater gameplay =
        Gameplay.updateCharacterBy Character.updatePosition index updater gameplay
    
    static member getCoordinates index gameplay =
        Chessboard.getCharacterCoordinates index gameplay.Chessboard

    static member getIndexByCoordinates coordinates gameplay =
        Chessboard.getCharacterAtCoordinates coordinates gameplay.Chessboard |> fst

    static member getCharacterState index gameplay =
        Chessboard.getCharacterState index gameplay.Chessboard
    
    static member getCurrentMove index gameplay =
        gameplay.Chessboard.CurrentMoves.[index]
    
    static member tryGetCharacterTurn index gameplay =
        List.tryFind (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member getCharacterTurn index gameplay =
        List.find (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member turnInProgress index gameplay =
        List.exists (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member isPlayerAttacking gameplay =
        match Gameplay.tryGetCharacterTurn PlayerIndex gameplay with
        | Some turn -> turn.TurnType = AttackTurn
        | None -> false

    static member isPlayerTraveling gameplay =
        match Gameplay.tryGetCharacterTurn PlayerIndex gameplay with
        | Some turn ->
            match turn.TurnType with
            | WalkTurn multiRoundContext -> multiRoundContext
            | _ -> false
        | None -> false
    
    static member updateCharacterTurn index updater gameplay =
        Gameplay.updateCharacterTurns (fun turns -> List.map (fun x -> if x.Actor = index then updater x else x) turns) gameplay
    
    static member setCharacterTurnStatus index status gameplay =
        Gameplay.updateCharacterTurn index (Turn.updateTurnStatus (constant status)) gameplay
    
    static member createPlayer gameplay =
        let coordinates = Gameplay.getCoordinates PlayerIndex gameplay
        let player = Character.makePlayer coordinates
        Gameplay.updatePlayer (constant player) gameplay

    // a basic sync mechanism that relies on never adding and removing *at the same time*
    static member syncLists (gameplay : Gameplay) =
        let chessboard = gameplay.Chessboard
        if gameplay.EnemyCount <> chessboard.EnemyCount then
            let enemies =
                if gameplay.EnemyCount > chessboard.EnemyCount then
                    List.filter (fun (character : Character) -> Chessboard.characterExists character.Index chessboard) gameplay.Enemies
                else
                    let generator k (v : FieldSpace) = Character.makeEnemy v.GetCharacterIndex k
                    let enemies = Map.filter (fun _ (v : FieldSpace) -> not (Gameplay.characterExists v.GetCharacterIndex gameplay)) chessboard.EnemyCoordinates |> Map.toListBy generator
                    enemies @ gameplay.Enemies
            Gameplay.updateEnemies (constant enemies) gameplay
        else gameplay

    // if updater takes index, index is arg1; if updater takes coordinates, coordinates is arg2
    static member updateChessboardBy updater arg1 arg2 gameplay =
        let chessboard = updater arg1 arg2 gameplay.Chessboard
        let gameplay = { gameplay with Chessboard = chessboard }
        Gameplay.syncLists gameplay
    
    static member updateCharacterState index updater gameplay =
        Gameplay.updateChessboardBy Chessboard.updateCharacterState index updater gameplay

    static member relocateCharacter index coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.relocateCharacter index coordinates gameplay
    
    static member addMove index (move : Move) gameplay =
        Gameplay.updateChessboardBy Chessboard.addMove index move gameplay

    static member removeMove index gameplay =
        Gameplay.updateChessboardBy Chessboard.removeMove index () gameplay
    
    static member truncatePlayerPath gameplay =
        Gameplay.updateChessboardBy Chessboard.truncatePlayerPath () () gameplay
    
    static member addHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.addHealth () coordinates gameplay

    static member removeHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.removePickup () coordinates gameplay
    
    static member clearPickups gameplay =
        Gameplay.updateChessboardBy Chessboard.clearPickups () () gameplay
    
    static member removeEnemy index gameplay =
        let coordinates = Gameplay.getCoordinates index gameplay
        let gameplay = Gameplay.addHealth coordinates gameplay
        Gameplay.updateChessboardBy Chessboard.removeCharacter index () gameplay

    static member clearEnemies gameplay =
        Gameplay.updateChessboardBy Chessboard.clearEnemies () () gameplay

    static member finishMove index gameplay =
        let gameplay = Gameplay.updateCharacterTurns (fun turns -> List.filter (fun x -> x.Actor <> index) turns) gameplay
        Gameplay.removeMove index gameplay
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay = Gameplay.updateCharacterState index (CharacterState.updateHitPoints (constant 30)) gameplay
            Gameplay.removeHealth coordinates gameplay
        | _ -> gameplay
    
    static member applyStep index direction gameplay =
        let coordinates = (Gameplay.getCoordinates index gameplay) + dtovm direction
        let gameplay = Gameplay.updateCharacterState index (CharacterState.updateFacingDirection (constant direction)) gameplay
        let gameplay =
            if Chessboard.pickupAtCoordinates coordinates gameplay.Chessboard then
                Gameplay.tryPickupHealth index coordinates gameplay
            else gameplay
        Gameplay.relocateCharacter index coordinates gameplay
    
    static member applyAttack index reactorIndex gameplay =
        let reactorDamage = 4 // NOTE: just hard-coding damage for now
        let coordinates = Gameplay.getCoordinates index gameplay
        let reactorCoordinates = Gameplay.getCoordinates reactorIndex gameplay
        let direction = Math.directionToTarget coordinates reactorCoordinates
        let gameplay = Gameplay.updateCharacterState index (CharacterState.updateFacingDirection (constant direction)) gameplay
        Gameplay.updateCharacterState reactorIndex (CharacterState.updateHitPoints (fun x -> x - reactorDamage)) gameplay
    
    static member stopTravelingPlayer reactorIndex gameplay =
        if reactorIndex = PlayerIndex then Gameplay.truncatePlayerPath gameplay else gameplay
    
    static member applyMove index gameplay =
        let move = Gameplay.getCurrentMove index gameplay
        match move with
        | Step direction -> Gameplay.applyStep index direction gameplay
        | Attack reactorIndex ->
            let gameplay = Gameplay.applyAttack index reactorIndex gameplay
            Gameplay.stopTravelingPlayer reactorIndex gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                let currentCoordinates = Gameplay.getCoordinates index gameplay
                let direction = Math.directionToTarget currentCoordinates head.PositionM
                Gameplay.applyStep index direction gameplay
            | [] -> failwithumf ()
    
    static member activateCharacter index gameplay =
        let move = Gameplay.getCurrentMove index gameplay
        let coordinates = Gameplay.getCoordinates index gameplay
        
        let turn =
            match move with
            | Step direction -> Turn.makeWalk index false coordinates direction
            | Attack reactorIndex ->
                let direction = Gameplay.getCoordinates reactorIndex gameplay |> Math.directionToTarget coordinates
                Turn.makeAttack index reactorIndex coordinates direction
            | Travel path ->
                let direction = Math.directionToTarget coordinates path.Head.PositionM
                Turn.makeWalk index true coordinates direction

        Gameplay.updateCharacterTurns (fun x -> turn :: x) gameplay

    static member setFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboardBy Chessboard.setPassableCoordinates () fieldMap gameplay
        let field = { FieldMapNp = fieldMap }
        Gameplay.updateField (constant field) gameplay

    static member resetFieldMap fieldMap gameplay = // TODO: get rid of this redundency
        let gameplay = Gameplay.updateChessboardBy Chessboard.transitionMap () fieldMap gameplay
        let field = { FieldMapNp = fieldMap }
        Gameplay.updateField (constant field) gameplay
    
    static member transitionMap direction gameplay =
        let mapModeler = gameplay.MapModeler.Transition direction
        Gameplay.updateMapModeler (constant mapModeler) gameplay

    static member setCharacterPositionToCoordinates index gameplay =
        let position = Gameplay.getCoordinates index gameplay |> vmtovf
        Gameplay.updatePosition index (constant position) gameplay
    
    static member yankPlayer coordinates gameplay =
        let gameplay = Gameplay.relocateCharacter PlayerIndex coordinates gameplay
        Gameplay.setCharacterPositionToCoordinates PlayerIndex gameplay
    
    static member makePlayer gameplay =
        let gameplay = Gameplay.updateChessboardBy Chessboard.addCharacter (PlayerIndex, CharacterState.makePlayer) v2iZero gameplay
        Gameplay.createPlayer gameplay

    static member makeEnemy index gameplay =
        let availableCoordinates = gameplay.Chessboard.AvailableCoordinates
        let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
        Gameplay.updateChessboardBy Chessboard.addCharacter (index, CharacterState.makeEnemy) coordinates gameplay

    static member makeEnemies quantity gameplay =
        let rec recursion count gameplay =
            if count = quantity then gameplay
            else Gameplay.makeEnemy (EnemyIndex count) gameplay |> recursion (count + 1)
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