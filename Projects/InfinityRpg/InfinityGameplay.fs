namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<NoComparison>] Move =
    | Step of Direction
    | Attack of CharacterIndex
    | Travel of NavigationNode list

    member this.TruncatePath =
        match this with
        | Travel (head :: _) -> Travel [head]
        | _ -> this

type [<ReferenceEquality; NoComparison>] Round =
    { CharacterMoves : Map<CharacterIndex, Move>
      WalkingEnemyGroup : CharacterIndex list
      AttackingEnemyGroup : CharacterIndex list }

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
        { CharacterMoves = Map.empty
          WalkingEnemyGroup = []
          AttackingEnemyGroup = [] }

type [<ReferenceEquality; NoComparison>] Gameplay =
    { ShallLoadGame : bool
      MetaMap : MetaMap
      Field : Field
      Chessboard : Chessboard
      Puppeteer : Puppeteer
      Round : Round }

    static member initial =
        let field = Field.initial
        let chessboard = Chessboard.init field.FieldMapNp
        { ShallLoadGame = false
          MetaMap = MetaMap.make
          Field = field
          Chessboard = chessboard
          Puppeteer = Puppeteer.init <| Chessboard.getCharacter PlayerIndex chessboard
          Round = Round.empty }

    static member getEnemyIndices gameplay =
        gameplay.Chessboard.EnemyIndices

    static member getCharacterIndices gameplay =
        PlayerIndex :: gameplay.Chessboard.EnemyIndices
    
    static member getOpponentIndices index gameplay =
        match index with
        | PlayerIndex -> Gameplay.getEnemyIndices gameplay
        | _ -> [PlayerIndex]
    
    static member getCoordinates index gameplay =
        Chessboard.getCharacterCoordinates index gameplay.Chessboard

    static member getCharacter index gameplay =
        Chessboard.getCharacter index gameplay.Chessboard
    
    static member getIndexByCoordinates coordinates gameplay =
        let character = Chessboard.getCharacterAtCoordinates coordinates gameplay.Chessboard
        character.CharacterIndex
    
    static member getCharacterMove index gameplay =
        gameplay.Round.CharacterMoves.[index]
    
    static member tryGetCharacterTurn index gameplay =
        Puppeteer.tryGetCharacterTurn index gameplay.Puppeteer
    
    static member getCharacterTurn index gameplay =
        Puppeteer.getCharacterTurn index gameplay.Puppeteer
    
    static member turnInProgress index gameplay =
        Puppeteer.turnInProgress index gameplay.Puppeteer
    
    static member anyTurnsInProgress gameplay = 
        gameplay.Puppeteer.AnyTurnsInProgress
    
    static member isPlayerTraveling gameplay =
        match Gameplay.tryGetCharacterTurn PlayerIndex gameplay with
        | Some turn ->
            match turn.TurnType with
            | WalkTurn multiRoundContext -> multiRoundContext
            | _ -> false
        | None -> false
    
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
    
    static member addMove index (move : Move) gameplay =
        Gameplay.updateRound (Round.addMove index move) gameplay

    static member removeMove index gameplay =
        Gameplay.updateRound (Round.removeMove index) gameplay
    
    static member truncatePlayerPath gameplay =
        let move = gameplay.Round.CharacterMoves.[PlayerIndex].TruncatePath
        Gameplay.addMove PlayerIndex move gameplay
    
    static member updateCharacterTurn index updater gameplay =
        Gameplay.updatePuppeteer (Puppeteer.updateCharacterTurn index updater) gameplay
    
    static member setCharacterTurnStatus index status gameplay =
        Gameplay.updateCharacterTurn index (Turn.updateTurnStatus (constant status)) gameplay
    
    static member updateCharacter index updater gameplay =
        Gameplay.updateChessboard (Chessboard.updateCharacter index updater) gameplay

    static member relocateCharacter index coordinates gameplay =
        Gameplay.updateChessboard (Chessboard.relocateCharacter index coordinates) gameplay
    
    static member removeEnemy index gameplay =
        match index with
        | EnemyIndex _ ->
            let coordinates = Gameplay.getCoordinates index gameplay
            let gameplay = Gameplay.updateChessboard (Chessboard.addPickup Health coordinates) gameplay
            Gameplay.updateChessboard (Chessboard.removeCharacter index) gameplay
        | PlayerIndex -> failwithumf ()

    static member clearEnemies gameplay =
        Gameplay.updateChessboard Chessboard.clearEnemies gameplay

    static member clearPickups gameplay =
        Gameplay.updateChessboard Chessboard.clearPickups gameplay

    static member refreshPlayerPuppetHitPoints gameplay =
        let player = Gameplay.getCharacter PlayerIndex gameplay
        Gameplay.updatePuppeteer (Puppeteer.updatePlayerPuppetHitPoints (constant player.HitPoints)) gameplay
    
    static member finishMove index gameplay =
        let gameplay = Gameplay.updatePuppeteer (Puppeteer.removeCharacterTurn index) gameplay
        Gameplay.removeMove index gameplay
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay = Gameplay.updateCharacter index (Character.updateHitPoints (constant 30)) gameplay
            let gameplay = Gameplay.refreshPlayerPuppetHitPoints gameplay
            Gameplay.updateChessboard (Chessboard.removePickup coordinates) gameplay
        | _ -> gameplay
    
    static member applyStep index direction gameplay =
        let coordinates = (Gameplay.getCoordinates index gameplay) + dtovc direction
        let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
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
        let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
        Gameplay.updateCharacter reactorIndex (Character.updateHitPoints (fun x -> x - reactorDamage)) gameplay
    
    static member stopTravelingPlayer reactorIndex gameplay =
        if reactorIndex = PlayerIndex then Gameplay.truncatePlayerPath gameplay else gameplay
    
    static member applyMove index gameplay =
        let move = Gameplay.getCharacterMove index gameplay
        match move with
        | Step direction -> Gameplay.applyStep index direction gameplay
        | Attack reactorIndex ->
            let gameplay = Gameplay.applyAttack index reactorIndex gameplay
            Gameplay.stopTravelingPlayer reactorIndex gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                let currentCoordinates = Gameplay.getCoordinates index gameplay
                let direction = Math.directionToTarget currentCoordinates head.Coordinates
                Gameplay.applyStep index direction gameplay
            | [] -> failwithumf ()
    
    static member activateCharacter index gameplay =
        let move = Gameplay.getCharacterMove index gameplay
        let coordinates = Gameplay.getCoordinates index gameplay
        let turn =
            match move with
            | Step direction -> Turn.makeWalk index false coordinates direction
            | Attack reactorIndex ->
                let direction = Gameplay.getCoordinates reactorIndex gameplay |> Math.directionToTarget coordinates
                Turn.makeAttack index reactorIndex coordinates direction
            | Travel path ->
                let direction = Math.directionToTarget coordinates path.Head.Coordinates
                Turn.makeWalk index true coordinates direction
        Gameplay.updatePuppeteer (Puppeteer.addCharacterTurn turn) gameplay

    static member makeMove index move gameplay =
        if (Gameplay.getCharacter index gameplay).IsAlive then
            let gameplay = Gameplay.addMove index move gameplay
            let gameplay = Gameplay.activateCharacter index gameplay
            Gameplay.applyMove index gameplay
        else gameplay
    
    static member addAttackingEnemyGroup group gameplay =
        Gameplay.updateRound (Round.addAttackingEnemyGroup group) gameplay

    static member removeHeadFromAttackingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeHeadFromAttackingEnemyGroup gameplay
    
    static member createWalkingEnemyGroup gameplay =
        let group = Gameplay.getEnemyIndices gameplay |> List.except gameplay.Round.AttackingEnemyGroup
        Gameplay.updateRound (Round.addWalkingEnemyGroup group) gameplay

    static member removeWalkingEnemyGroup gameplay =
        Gameplay.updateRound Round.removeWalkingEnemyGroup gameplay
    
    static member resetFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboard (Chessboard.transitionMap fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member transitionMap direction gameplay =
        Gameplay.updateMetaMap (MetaMap.transition direction) gameplay

    static member makeEnemy index gameplay =
        let availableCoordinates = gameplay.Chessboard.UnoccupiedSpaces
        let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
        Gameplay.updateChessboard (Chessboard.addCharacter (Character.makeEnemy index) coordinates) gameplay

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
                let gameplay = updater index gameplay
                recursion indices.Tail gameplay
        recursion indices gameplay