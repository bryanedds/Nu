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

    member this.NotNormalInput =
        match this with
        | NormalInputMode -> false
        | _ -> true

type RoundStatus =
    | RunningCharacterMoves
    | MakingEnemyAttack
    | MakingEnemiesWalk
    | FinishingRound
    | NoRound

// NOTE: continuity here is the player's ability to perform operations across multiple rounds without processing
// NoRound in between. rounds are thus linked together preventing the game from alternating between "round on" and
// "round off" states. in other words, PlayerContinuity effectively tells the game whether to advance the round cycle
// or turn it off. this is what prevents things like a save button blinking on and off during player navigation and
// waiting.
type [<ReferenceEquality; NoComparison>] PlayerContinuity =
    | ManualNavigation
    | AutomaticNavigation of NavigationNode list
    | Waiting
    | NoContinuity

    static member truncatePath playerContinuity =
        match playerContinuity with
        | AutomaticNavigation (head :: _) -> AutomaticNavigation [head]
        | _ -> failwithumf ()

type [<StructuralEquality; NoComparison>] Move =
    | Step of Direction
    | Travel of NavigationNode list
    | Attack of Reactor
    | Shoot of Reactor

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

    static member tryGetPlayerMove (round : Round) =
        Map.tryFind PlayerIndex round.CharacterMoves
    
    static member inProgress (round : Round) =
        Map.notEmpty round.CharacterMoves || List.notEmpty round.WalkingEnemyGroup || List.notEmpty round.AttackingEnemyGroup || round.IsPlayerContinuity
    
    static member notInProgress round =
        not $ Round.inProgress round
    
    static member getRoundStatus (round : Round) =
        if Map.notEmpty round.CharacterMoves then RunningCharacterMoves
        elif List.notEmpty round.AttackingEnemyGroup then MakingEnemyAttack
        elif List.notEmpty round.WalkingEnemyGroup then MakingEnemiesWalk
        elif round.IsPlayerContinuity then FinishingRound
        else NoRound
    
    static member updatePlayerContinuity updater round =
        { round with PlayerContinuity = updater round.PlayerContinuity }
    
    static member updateCharacterMoves updater round =
        { round with CharacterMoves = updater round.CharacterMoves }
    
    static member updateWalkingEnemyGroup updater round =
        { round with WalkingEnemyGroup = updater round.WalkingEnemyGroup }
    
    static member updateAttackingEnemyGroup updater round =
        { round with AttackingEnemyGroup = updater round.AttackingEnemyGroup }
    
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

    static member areCharactersAdjacent index1 index2 gameplay =
        Math.areCoordinatesAdjacent (Chessboard.getCharacterCoordinates index1 gameplay.Chessboard) (Chessboard.getCharacterCoordinates index2 gameplay.Chessboard)
    
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
    
    static member addMove index (move : Move) gameplay =
        Gameplay.updateRound (Round.addMove index move) gameplay

    static member removeMove index gameplay =
        Gameplay.updateRound (Round.removeMove index) gameplay
    
    static member truncatePlayerPath gameplay =
        Gameplay.updateRound (Round.updatePlayerContinuity PlayerContinuity.truncatePath) gameplay
    
    static member updateCharacterTurn index updater gameplay =
        Gameplay.updatePuppeteer (Puppeteer.updateCharacterTurn index updater) gameplay
    
    static member setCharacterTurnStatus index status gameplay =
        Gameplay.updateCharacterTurn index (Turn.updateTurnStatus (constant status)) gameplay
    
    static member updateCharacter index updater gameplay =
        Gameplay.updateChessboard (Chessboard.updateCharacter index updater) gameplay

    static member relocateCharacter index coordinates gameplay =
        Gameplay.updateChessboard (Chessboard.relocateCharacter index coordinates) gameplay
    
    static member refreshWalkingEnemyGroup gameplay =
        Gameplay.updateRound (Round.updateWalkingEnemyGroup (List.filter (fun character -> Chessboard.doesCharacterExist character gameplay.Chessboard))) gameplay
    
    static member refreshAttackingEnemyGroup gameplay =
        Gameplay.updateRound (Round.updateAttackingEnemyGroup (List.filter (fun character -> Chessboard.doesCharacterExist character gameplay.Chessboard))) gameplay
    
    static member removeCharacter index gameplay =
        let coordinates = Chessboard.getCharacterCoordinates index gameplay.Chessboard
        let gameplay = Gameplay.updateChessboard (Chessboard.removeCharacter index) gameplay
        match index with
        | EnemyIndex _ ->
            let gameplay =
                if Gen.randomb
                then Gameplay.updateChessboard (Chessboard.addPickup Health coordinates) gameplay
                else gameplay
            let gameplay = Gameplay.refreshWalkingEnemyGroup gameplay
            Gameplay.refreshAttackingEnemyGroup gameplay
        | PlayerIndex -> gameplay

    static member clearEnemies gameplay =
        Gameplay.updateChessboard Chessboard.clearEnemies gameplay
    
    static member clearPickups gameplay =
        Gameplay.updateChessboard Chessboard.clearPickups gameplay
    
    static member clearProps gameplay =
        Gameplay.updateChessboard Chessboard.clearProps gameplay

    static member addRandomPickup coordinates gameplay =
        let pickup = if Gen.randomb then Health else (Item (Special MagicMissile))
        Gameplay.updateChessboard (Chessboard.addPickup pickup coordinates) gameplay
    
    static member removeLongGrass coordinates gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.removeProp coordinates) gameplay
        if Gen.random1 10 = 0
        then Gameplay.addRandomPickup coordinates gameplay
        else gameplay
    
    static member finishMove index gameplay =
        let gameplay = Gameplay.updatePuppeteer (Puppeteer.removeCharacterTurn index) gameplay
        Gameplay.removeMove index gameplay
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay =
                match Chessboard.getPickup coordinates gameplay.Chessboard with
                | Health ->
                    let gameplay = Gameplay.updateCharacter index (Character.updateHitPoints (fun x -> x + 15)) gameplay
                    Gameplay.refreshPlayerPuppetHitPoints gameplay
                | Item item -> Gameplay.updateInventory (Inventory.tryAddItem item) gameplay
            Gameplay.updateChessboard (Chessboard.removePickup coordinates) gameplay
        | _ -> gameplay
    
    static member applyStep index direction gameplay =
        let coordinates = (Chessboard.getCharacterCoordinates index gameplay.Chessboard) + dtovc direction
        let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
        let gameplay =
            if Chessboard.isPickupAtCoordinates coordinates gameplay.Chessboard then
                Gameplay.tryPickupHealth index coordinates gameplay
            else gameplay
        Gameplay.relocateCharacter index coordinates gameplay
    
    static member stopTravelingPlayer reactorIndex gameplay =
        if reactorIndex = PlayerIndex && gameplay.Round.IsPlayerTraveling then Gameplay.truncatePlayerPath gameplay else gameplay
    
    static member applyAttack index reactor gameplay =
        let coordinates = Chessboard.getCharacterCoordinates index gameplay.Chessboard
        match reactor with
        | ReactingCharacter reactorIndex ->
            let reactorDamage = 4 // NOTE: just hard-coding damage for now
            let reactorCoordinates = Chessboard.getCharacterCoordinates reactorIndex gameplay.Chessboard
            let direction = Math.directionToTarget coordinates reactorCoordinates
            let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
            let gameplay = Gameplay.updateCharacter reactorIndex (Character.updateHitPoints (fun x -> x - reactorDamage)) gameplay
            Gameplay.stopTravelingPlayer reactorIndex gameplay
        | ReactingProp reactorCoordinates ->
            let direction = Math.directionToTarget coordinates reactorCoordinates
            Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
    
    static member applyMove index gameplay =
        let move = gameplay.Round.CharacterMoves.[index]
        match move with
        | Step direction -> Gameplay.applyStep index direction gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                let currentCoordinates = Chessboard.getCharacterCoordinates index gameplay.Chessboard
                let direction = Math.directionToTarget currentCoordinates head.Coordinates
                Gameplay.applyStep index direction gameplay
            | [] -> failwithumf ()
        | Attack reactor -> Gameplay.applyAttack index reactor gameplay
        | Shoot reactor ->
            let gameplay = Gameplay.updateInventory (Inventory.removeItem (Special MagicMissile)) gameplay
            Gameplay.applyAttack index reactor gameplay
    
    static member activateCharacter time index gameplay =
        let move = gameplay.Round.CharacterMoves.[index]
        let coordinates = Chessboard.getCharacterCoordinates index gameplay.Chessboard
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
                    | ReactingCharacter reactorIndex -> Chessboard.getCharacterCoordinates reactorIndex gameplay.Chessboard |> Math.directionToTarget coordinates
                    | ReactingProp reactorCoordinates -> Math.directionToTarget coordinates reactorCoordinates
                Turn.makeAttack time index false reactor coordinates direction
            | Shoot reactor ->
                let direction =
                    match reactor with
                    | ReactingCharacter reactorIndex -> Chessboard.getCharacterCoordinates reactorIndex gameplay.Chessboard |> Math.directionToTarget coordinates
                    | ReactingProp _ -> failwithumf ()
                Turn.makeAttack time index true reactor coordinates direction
        Gameplay.updatePuppeteer (Puppeteer.addCharacterTurn turn) gameplay

    static member advanceTime gameplay =
        { gameplay with Time = inc gameplay.Time }

    static member makeMove time index move gameplay =
        if (Chessboard.getCharacter index gameplay.Chessboard).IsAlive then
            let gameplay = Gameplay.addMove index move gameplay
            let gameplay = Gameplay.activateCharacter time index gameplay
            let gameplay = Gameplay.applyMove index gameplay
            if index = PlayerIndex then
                match move with
                | Step _ -> Gameplay.updateRound (Round.updatePlayerContinuity (constant ManualNavigation)) gameplay
                | Travel path -> Gameplay.updateRound (Round.updatePlayerContinuity (constant (AutomaticNavigation path))) gameplay
                | _ -> gameplay
            else gameplay
        else gameplay
    
    static member addAttackingEnemyGroup group gameplay =
        Gameplay.updateRound (Round.addAttackingEnemyGroup group) gameplay

    static member removeHeadFromAttackingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeHeadFromAttackingEnemyGroup gameplay

    static member createWalkingEnemyGroup gameplay =
        let enemyIndices = Chessboard.getEnemyIndices gameplay.Chessboard
        let group = List.except gameplay.Round.AttackingEnemyGroup enemyIndices
        Gameplay.updateRound (Round.addWalkingEnemyGroup group) gameplay

    static member removeWalkingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeWalkingEnemyGroup gameplay
    
    static member resetFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.setFieldSpaces fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member resetFieldMapWithPlayer fieldMap gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.transitionMap fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member transitionMap direction gameplay =
        let player = Chessboard.getCharacter PlayerIndex gameplay.Chessboard
        let gameplay = Gameplay.removeCharacter PlayerIndex gameplay
        let gameplay = Gameplay.updateMetaMap (MetaMap.transition direction) gameplay
        let gameplay = Gameplay.resetFieldMap (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
        let newCoordinates =
            match direction with
            | Upward
            | Rightward -> gameplay.MetaMap.Current.PathStart
            | Downward
            | Leftward -> gameplay.MetaMap.Current.PathEnd
        Gameplay.updateChessboard (Chessboard.addCharacter player newCoordinates) gameplay

    static member makeLongGrass gameplay =
        let mapBounds = v4iBounds v2iZero (Constants.Layout.FieldMapSizeC - v2iOne)
        let predicate1 coordinates = Math.isPointInBoundsI coordinates mapBounds && Map.find coordinates gameplay.Field.FieldMapNp.FieldTiles = FieldMap.GrassTile
        let predicate2 coordinates = FieldMap.hasAtLeastNAdjacentTiles 2 coordinates FieldMap.TreeTile mapBounds gameplay.Field.FieldMapNp.FieldTiles
        let unoccupiedSpaces = Chessboard.getUnoccupiedSpaces gameplay.Chessboard
        Set.fold (fun gameplay space ->
            if predicate1 space && predicate2 space
            then Gameplay.updateChessboard (Chessboard.addProp LongGrass space) gameplay
            else gameplay)
            gameplay
            unoccupiedSpaces
    
    static member makeEnemy index gameplay =
        let unoccupiedSpaces = Chessboard.getUnoccupiedSpaces gameplay.Chessboard
        let coordinates = Seq.item (Gen.random1 (Set.count unoccupiedSpaces)) unoccupiedSpaces
        Gameplay.updateChessboard (Chessboard.addCharacter (Character.makeEnemy index) coordinates) gameplay

    static member makeEnemies quantity gameplay =
        Seq.fold
            (fun gameplay index -> Gameplay.makeEnemy (EnemyIndex index) gameplay)
            gameplay
            [0 .. dec quantity]
    
    static member populateFieldMap gameplay =
        let gameplay = Gameplay.makeLongGrass gameplay
        let enemyCount = 1 + Gen.random1 5
        Gameplay.makeEnemies enemyCount gameplay

    // NOTE: this function should never need to be exposed publicly. The Gameplay types should be responsible for any
    // required syncing of its internal fields
    static member refreshPlayerPuppetHitPoints gameplay =
        let player = Chessboard.getCharacter PlayerIndex gameplay.Chessboard
        Gameplay.updatePuppeteer (Puppeteer.updatePlayerPuppetHitPoints (constant player.HitPoints)) gameplay

    static member initial =
        let field = Field.initial
        let chessboard = Chessboard.make field.FieldMapNp
        { Time = 0L
          InputMode = NormalInputMode
          ShallLoadGame = false
          MetaMap = MetaMap.initial
          Field = field
          Chessboard = chessboard
          Puppeteer = chessboard |> Chessboard.getCharacter PlayerIndex |> Puppeteer.make
          Round = Round.empty
          Inventory = Inventory.initial }