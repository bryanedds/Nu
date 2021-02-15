namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type InputMode =
    | NormalInputMode
    | SelectionInputMode
    | DisabledInputMode

/// NOTE: continuity here is the player's ability to perform operations across multiple rounds without processing
/// NoRound in between. rounds are thus linked together preventing the game from alternating between "round on" and
/// "round off" states. in other words, PlayerContinuity effectively tells the game whether to advance the round cycle
/// or turn it off. this is what prevents things like a save button blinking on and off during player navigation and
/// waiting.
type [<ReferenceEquality; NoComparison>] PlayerContinuity =
    | ManualNavigation
    | AutomaticNavigation of NavigationNode list
    | Waiting
    | NoContinuity

type [<StructuralEquality; NoComparison>] Move =
    | Step of Direction
    | Travel of NavigationNode list
    | Attack of Reactor
    | Shoot of Reactor

type RoundState =
    | RunningCharacterMoves
    | MakingEnemyAttack
    | MakingEnemiesWalk
    | FinishingRound
    | NoRound

/// TODO: turn this into an abstract data type.
type [<ReferenceEquality; NoComparison>] Round =
    { PlayerContinuity : PlayerContinuity
      CharacterMoves : Map<CharacterIndex, Move>
      WalkingEnemyGroup : CharacterIndex list
      AttackingEnemyGroup : CharacterIndex list }
      
      member this.IsPlayerContinuity =
          match this.PlayerContinuity with
          | NoContinuity -> false
          | _ -> true
      
      member this.IsPlayerTraveling =
          match this.PlayerContinuity with
          | AutomaticNavigation _ -> true
          | _ -> false
    
    static member updatePlayerContinuity updater round =
        { round with PlayerContinuity = updater round.PlayerContinuity }
    
    static member updateCharacterMoves updater round =
        { round with CharacterMoves = updater round.CharacterMoves }
    
    static member updateWalkingEnemyGroup updater round =
        { round with WalkingEnemyGroup = updater round.WalkingEnemyGroup }
    
    static member updateAttackingEnemyGroup updater round =
        { round with AttackingEnemyGroup = updater round.AttackingEnemyGroup }
    
    static member getRoundState (round : Round) =
        if Map.notEmpty round.CharacterMoves then RunningCharacterMoves
        elif List.notEmpty round.AttackingEnemyGroup then MakingEnemyAttack
        elif List.notEmpty round.WalkingEnemyGroup then MakingEnemiesWalk
        elif round.IsPlayerContinuity then FinishingRound
        else NoRound
    
    static member tryGetPlayerMove (round : Round) =
        Map.tryFind PlayerIndex round.CharacterMoves
    
    static member inProgress (round : Round) =
        Map.notEmpty round.CharacterMoves ||
        List.notEmpty round.WalkingEnemyGroup ||
        List.notEmpty round.AttackingEnemyGroup ||
        round.IsPlayerContinuity
    
    static member notInProgress round =
        not $ Round.inProgress round
    
    static member addMove index move round =
        Round.updateCharacterMoves (Map.add index move) round

    static member removeMove index round =
        Round.updateCharacterMoves (Map.remove index) round

    static member addWalkingEnemyGroup group round =
        Round.updateWalkingEnemyGroup (constant group) round

    static member removeWalkingEnemyGroup round =
        Round.updateWalkingEnemyGroup (constant []) round

    static member addAttackingEnemyGroup group round =
        Round.updateAttackingEnemyGroup (constant group) round

    static member removeHeadFromAttackingEnemyGroup round =
        Round.updateAttackingEnemyGroup List.tail round

    static member empty =
        { PlayerContinuity = NoContinuity
          CharacterMoves = Map.empty
          WalkingEnemyGroup = []
          AttackingEnemyGroup = [] }

/// TODO: turn this into an abstract data type.
type [<ReferenceEquality; NoComparison>] Gameplay =
    { Time : int64
      InputMode : InputMode
      ShallLoadGame : bool
      MetaMap : MetaMap
      Field : Field
      Chessboard : Chessboard
      Puppeteer : Puppeteer
      Round : Round
      Inventory : Inventory }

    (* Updaters *)
    
    static member updateInputMode updater gameplay =
        { gameplay with InputMode = updater gameplay.InputMode }
    
    static member updateMetaMap updater gameplay =
        { gameplay with MetaMap = updater gameplay.MetaMap }
    
    static member updateField updater gameplay =
        { gameplay with Field = updater gameplay.Field }
    
    static member updateChessboard updater gameplay =
        { gameplay with Chessboard = updater gameplay.Chessboard }
    
    static member updatePuppeteer updater gameplay =
        { gameplay with Puppeteer = updater gameplay.Puppeteer }
    
    static member updateRound updater gameplay =
        { gameplay with Round = updater gameplay.Round }
    
    static member updateInventory updater gameplay =
        { gameplay with Inventory = updater gameplay.Inventory }
    
    static member tryUpdateCharacterTurn index updater gameplay =
        Gameplay.updatePuppeteer (Puppeteer.tryUpdateCharacterTurn index updater) gameplay
    
    static member tryUpdateCharacterTurnStatus index status gameplay =
        Gameplay.tryUpdateCharacterTurn index (Turn.updateTurnStatus status) gameplay
    
    static member tryUpdateCharacter index updater gameplay =
        Gameplay.updateChessboard (Chessboard.tryUpdateCharacter index updater) gameplay

    static member tryRelocateCharacter index coordinates gameplay =
        Gameplay.updateChessboard (Chessboard.tryRelocateCharacter index coordinates) gameplay

    (* Accessors *)
    
    static member areCharactersAdjacent index index2 gameplay =
        Math.areCoordinatesAdjacent
            (Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard |> Option.get) // assume character exists
            (Chessboard.tryGetCharacterCoordinates index2 gameplay.Chessboard |> Option.get) // assume character exists

    (* FieldMap Commands *)
    
    static member resetFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.setFieldSpaces fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member resetFieldMapWithPlayer fieldMap gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.transitionMap fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member transitionFieldMap direction gameplay =
        let player = Chessboard.tryGetCharacter PlayerIndex gameplay.Chessboard |> Option.get // assume player exists
        let gameplay = Gameplay.updateChessboard (Chessboard.tryRemoveCharacter PlayerIndex) gameplay
        let gameplay = Gameplay.updateMetaMap (MetaMap.transition direction) gameplay
        let gameplay = Gameplay.resetFieldMap (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
        let newCoordinates =
            match direction with
            | Upward
            | Rightward -> gameplay.MetaMap.Current.PathStart
            | Downward
            | Leftward -> gameplay.MetaMap.Current.PathEnd
        Gameplay.updateChessboard (Chessboard.addCharacter player newCoordinates) gameplay

    (* Interaction Commands *)

    static member createWalkingEnemyGroup gameplay =
        let enemyIndices = Chessboard.getEnemyIndices gameplay.Chessboard
        let group = List.except gameplay.Round.AttackingEnemyGroup enemyIndices
        Gameplay.updateRound (Round.addWalkingEnemyGroup group) gameplay
    
    static member removeWalkingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeWalkingEnemyGroup gameplay
    
    static member addAttackingEnemyGroup group gameplay =
        Gameplay.updateRound (Round.addAttackingEnemyGroup group) gameplay

    static member removeHeadFromAttackingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeHeadFromAttackingEnemyGroup gameplay
    
    static member addMove index (move : Move) gameplay =
        Gameplay.updateRound (Round.addMove index move) gameplay

    static member removeMove index gameplay =
        Gameplay.updateRound (Round.removeMove index) gameplay
    
    static member finishMove index gameplay =
        let gameplay = Gameplay.updatePuppeteer (Puppeteer.removeCharacterTurn index) gameplay
        Gameplay.removeMove index gameplay

    static member tryAddMove time index move gameplay =
        match Chessboard.tryGetCharacter index gameplay.Chessboard with
        | Some character when character.IsAlive ->
            let gameplay = Gameplay.addMove index move gameplay
            let gameplay = Gameplay.tryActivateCharacter time index gameplay
            let gameplay = Gameplay.tryApplyMove index gameplay
            if index = PlayerIndex then
                match move with
                | Step _ -> Gameplay.updateRound (Round.updatePlayerContinuity (constant ManualNavigation)) gameplay
                | Travel path -> Gameplay.updateRound (Round.updatePlayerContinuity (constant (AutomaticNavigation path))) gameplay
                | _ -> gameplay
            else gameplay
        | Some _ | None -> gameplay
    
    static member tryInterruptPlayer gameplay =
        if gameplay.Round.IsPlayerTraveling then
            Gameplay.updateRound (fun round ->
                Round.updatePlayerContinuity (fun playerContinuity ->
                    match playerContinuity with
                    | AutomaticNavigation (head :: _) -> AutomaticNavigation [head]
                    | _ -> playerContinuity)
                    round)
                gameplay
        else gameplay
    
    static member tryCutGrass coordinates gameplay =
        match Map.tryFind coordinates gameplay.Chessboard.Props with
        | Some prop when prop = LongGrass ->
            let gameplay = Gameplay.updateChessboard (Chessboard.removeProp coordinates) gameplay
            if Gen.random1 10 = 0 then Gameplay.makeRandomPickup coordinates gameplay else gameplay
        | Some _ | None -> gameplay
    
    static member tryKillCharacter index gameplay =
        match Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard with
        | Some coordinates ->
            let gameplay = Gameplay.updateChessboard (Chessboard.tryRemoveCharacter index) gameplay
            match index with
            | EnemyIndex _ ->
                let gameplay = if Gen.randomb then Gameplay.updateChessboard (Chessboard.addPickup Health coordinates) gameplay else gameplay
                let gameplay = Gameplay.updateRound (Round.updateWalkingEnemyGroup (List.filter (fun character -> Chessboard.doesCharacterExist character gameplay.Chessboard))) gameplay
                let gameplay = Gameplay.updateRound (Round.updateAttackingEnemyGroup (List.filter (fun character -> Chessboard.doesCharacterExist character gameplay.Chessboard))) gameplay
                gameplay
            | PlayerIndex -> gameplay
        | None -> gameplay

    (* Advance Commands *)
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay =
                match Chessboard.tryGetPickup coordinates gameplay.Chessboard with
                | Some Health -> Gameplay.tryUpdateCharacter index (Character.updateHitPoints (fun x -> x + 15)) gameplay
                | Some (Item item) -> Gameplay.updateInventory (Inventory.tryAddItem item) gameplay
                | None -> gameplay
            Gameplay.updateChessboard (Chessboard.removePickup coordinates) gameplay
        | _ -> gameplay
    
    static member tryApplyStep index direction gameplay =
        match Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard with
        | Some coordinates ->
            let coordinates = coordinates + dtovc direction
            let gameplay = Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
            let gameplay =
                if Map.containsKey coordinates gameplay.Chessboard.Pickups
                then Gameplay.tryPickupHealth index coordinates gameplay
                else gameplay
            Gameplay.tryRelocateCharacter index coordinates gameplay
        | None -> gameplay
    
    static member tryApplyAttack index reactor gameplay =
        match Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard with
        | Some coordinates ->
            match reactor with
            | ReactingCharacter reactorIndex ->
                let reactorDamage = 4 // NOTE: just hard-coding damage for now
                let reactorCoordinates = Chessboard.tryGetCharacterCoordinates reactorIndex gameplay.Chessboard |> Option.get // we know it's there - we just got it
                let direction = Math.directionToTarget coordinates reactorCoordinates
                let gameplay = Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
                let gameplay = Gameplay.tryUpdateCharacter reactorIndex (Character.updateHitPoints (fun hp -> hp - reactorDamage)) gameplay
                if reactorIndex = PlayerIndex then Gameplay.tryInterruptPlayer gameplay else gameplay
            | ReactingProp reactorCoordinates ->
                let direction = Math.directionToTarget coordinates reactorCoordinates
                Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
            | ReactingPickup _ ->
                gameplay
        | None -> gameplay
    
    static member tryApplyMove index gameplay =
        let move = gameplay.Round.CharacterMoves.[index]
        match move with
        | Step direction ->
            Gameplay.tryApplyStep index direction gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                match Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard with
                | Some coordinates ->
                    let direction = Math.directionToTarget coordinates head.Coordinates
                    Gameplay.tryApplyStep index direction gameplay
                | None -> gameplay
            | [] -> gameplay
        | Attack reactor ->
            Gameplay.tryApplyAttack index reactor gameplay
        | Shoot reactor ->
            let gameplay = Gameplay.updateInventory (Inventory.removeItem (Special MagicMissile)) gameplay
            Gameplay.tryApplyAttack index reactor gameplay
    
    static member tryActivateCharacter time index gameplay =
        let move = gameplay.Round.CharacterMoves.[index]
        match Chessboard.tryGetCharacterCoordinates index gameplay.Chessboard with
        | Some coordinates ->
            let turn =
                match move with
                | Step direction ->
                    Turn.makeWalk time index false coordinates direction
                | Travel path ->
                    let direction = Math.directionToTarget coordinates path.Head.Coordinates
                    Turn.makeWalk time index true coordinates direction
                | Attack reactor ->
                    let direction =
                        match reactor with
                        | ReactingCharacter reactorIndex -> Chessboard.tryGetCharacterCoordinates reactorIndex gameplay.Chessboard |> Option.get |> Math.directionToTarget coordinates // assume reactor exists
                        | ReactingPickup _ -> failwithumf ()
                        | ReactingProp reactorCoordinates -> Math.directionToTarget coordinates reactorCoordinates
                    Turn.makeAttack time index false reactor coordinates direction
                | Shoot reactor ->
                    let direction =
                        match reactor with
                        | ReactingCharacter reactorIndex -> Chessboard.tryGetCharacterCoordinates reactorIndex gameplay.Chessboard |> Option.get |> Math.directionToTarget coordinates // assume reactor exists
                        | ReactingPickup _ -> failwithumf ()
                        | ReactingProp _ -> failwithumf ()
                    Turn.makeAttack time index true reactor coordinates direction
            Gameplay.updatePuppeteer (Puppeteer.addCharacterTurn turn) gameplay
        | None -> gameplay

    static member advanceTime gameplay =
        { gameplay with Time = inc gameplay.Time }

    (* Population Commands *)
    
    static member makeRandomPickup coordinates gameplay =
        let pickup = if Gen.randomb then Health else (Item (Special MagicMissile))
        Gameplay.updateChessboard (Chessboard.addPickup pickup coordinates) gameplay

    static member makeLongGrass coordinates gameplay =
        Gameplay.updateChessboard (Chessboard.addProp LongGrass coordinates) gameplay

    static member makeLongGrasses gameplay =
        let mapBounds = v4iBounds v2iZero (Constants.Layout.FieldMapSizeC - v2iOne)
        let predicate1 coordinates = Math.isPointInBoundsI coordinates mapBounds && Map.find coordinates gameplay.Field.FieldMap.FieldTiles = FieldMap.GrassTile
        let predicate2 coordinates = FieldMap.hasAtLeastNAdjacentTiles 2 coordinates FieldMap.TreeTile mapBounds gameplay.Field.FieldMap.FieldTiles
        let unoccupiedSpaces = Chessboard.getUnoccupiedSpaces gameplay.Chessboard
        Set.fold (fun gameplay coordinates ->
            if predicate1 coordinates && predicate2 coordinates
            then Gameplay.makeLongGrass coordinates gameplay
            else gameplay)
            gameplay
            unoccupiedSpaces
    
    static member makeProps gameplay =
        Gameplay.makeLongGrasses gameplay
    
    static member makeEnemy index gameplay =
        let unoccupiedSpaces = Chessboard.getUnoccupiedSpaces gameplay.Chessboard
        let coordinates = Seq.item (Gen.random1 (Set.count unoccupiedSpaces)) unoccupiedSpaces
        Gameplay.updateChessboard (Chessboard.addCharacter (Character.makeEnemy index) coordinates) gameplay
    
    static member makeEnemies enemyCount gameplay =
        Seq.fold
            (fun gameplay index -> Gameplay.makeEnemy (EnemyIndex index) gameplay)
            gameplay
            [0 .. dec enemyCount]

    static member clearPickups gameplay =
        Gameplay.updateChessboard Chessboard.clearPickups gameplay

    static member clearProps gameplay =
        Gameplay.updateChessboard Chessboard.clearProps gameplay

    static member clearEnemies gameplay =
        Gameplay.updateChessboard Chessboard.clearEnemies gameplay

    (* Constructors *)

    static member initial =
        let field = Field.initial
        let chessboard = Chessboard.make field.FieldMap
        { Time = 0L
          InputMode = NormalInputMode
          ShallLoadGame = false
          MetaMap = MetaMap.initial
          Field = field
          Chessboard = chessboard
          Puppeteer = Puppeteer.empty
          Round = Round.empty
          Inventory = Inventory.initial }