namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type [<StructuralEquality; NoComparison>] InputMode =
    | NormalInputMode
    | SelectionInputMode
    | DisabledInputMode

type [<StructuralEquality; NoComparison>] Action =
    | Step of Direction
    | Travel of NavigationNode list
    | Attack of TurnReaction
    | Shoot of TurnReaction

type [<StructuralEquality; NoComparison>] Navigation =
    | Idling // no navigation
    | Waiting // skipping turn
    | ManualNavigation
    | AutomaticNavigation of NavigationNode list

type [<StructuralEquality; NoComparison>] Progression =
    | ActionsIssued
    | EnemiesAttacking
    | EnemiesWalking
    | ProgressionFinishing
    | ProgressionFinished

/// TODO: turn this into an abstract data type.
/// TODO: keep cleaning up this shitty progression code.
type [<ReferenceEquality; NoComparison>] Gameplay =
    { Time : int64
      InputMode : InputMode
      ShallLoadGame : bool
      Inventory : Inventory
      MetaMap : MetaMap
      Field : Field
      Gameboard : Gameboard
      ActionsIssued : Map<CharacterIndex, Action>
      WalkingEnemies : CharacterIndex list
      AttackingEnemies : CharacterIndex list
      PlayerNavigation : Navigation }

    (* Properties *)

    /// NOTE: continuity here is the player's ability to perform operations across multiple rounds without processing
    /// RoundFinished in between. rounds are thus linked together preventing the game from alternating between "round
    /// on" and "round off" states. In other words, PlayerContinuity effectively tells the game whether to advance the
    /// round cycle or turn it off. This is what prevents things like a save button blinking on and off during player
    /// navigation and waiting.
    member this.IsPlayerContinuing =
        match this.PlayerNavigation with
        | Idling -> false
        | _ -> true

    member this.IsPlayerTraveling =
        match this.PlayerNavigation with
        | AutomaticNavigation _ -> true
        | _ -> false

    (* Updaters *)
    
    static member updateInputMode updater gameplay =
        { gameplay with InputMode = updater gameplay.InputMode }
    
    static member updateMetaMap updater gameplay =
        { gameplay with MetaMap = updater gameplay.MetaMap }
    
    static member updateField updater gameplay =
        { gameplay with Field = updater gameplay.Field }
    
    static member updateGameboard updater gameplay =
        { gameplay with Gameboard = updater gameplay.Gameboard }
    
    static member updateInventory updater gameplay =
        { gameplay with Inventory = updater gameplay.Inventory }
    
    static member updateActionsIssued updater gameplay =
        { gameplay with ActionsIssued = updater gameplay.ActionsIssued }

    static member updatePlayerNavigation updater gameplay =
        { gameplay with PlayerNavigation = updater gameplay.PlayerNavigation }
    
    static member updateWalkingEnemies updater gameplay =
        { gameplay with WalkingEnemies = updater gameplay.WalkingEnemies }
    
    static member updateAttackingEnemies updater gameplay =
        { gameplay with AttackingEnemies = updater gameplay.AttackingEnemies }
    
    static member tryUpdateCharacterTurn index updater gameplay =
        Gameplay.updateGameboard (Gameboard.tryUpdateCharacterTurn index updater) gameplay
    
    static member tryUpdateCharacterTurnStatus index status gameplay =
        Gameplay.tryUpdateCharacterTurn index (Turn.updateTurnStatus status) gameplay
    
    static member tryUpdateCharacter index updater gameplay =
        Gameplay.updateGameboard (Gameboard.tryUpdateCharacter index updater) gameplay

    static member tryRelocateCharacter index coordinates gameplay =
        Gameplay.updateGameboard (Gameboard.tryRelocateCharacter index coordinates) gameplay

    (* Accessors *)
    
    static member areCharactersAdjacent index index2 gameplay =
        Math.areCoordinatesAdjacent
            (Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard |> Option.get) // assume character exists
            (Gameboard.tryGetCharacterCoordinates index2 gameplay.Gameboard |> Option.get) // assume character exists

    static member getProgression (gameplay : Gameplay) =
        if Map.notEmpty gameplay.ActionsIssued then ActionsIssued
        elif List.notEmpty gameplay.AttackingEnemies then EnemiesAttacking
        elif List.notEmpty gameplay.WalkingEnemies then EnemiesWalking
        elif gameplay.IsPlayerContinuing then ProgressionFinishing
        else ProgressionFinished
    
    static member inProgress gameplay =
        Gameplay.getProgression gameplay <> ProgressionFinished
    
    static member tryGetPlayerAction (gameplay : Gameplay) =
        Map.tryFind PlayerIndex gameplay.ActionsIssued

    (* FieldMap Commands *)
    
    static member resetFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateGameboard (Gameboard.setFieldSpaces fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member resetFieldMapWithPlayer fieldMap gameplay =
        let gameplay = Gameplay.updateGameboard (Gameboard.transitionMap fieldMap) gameplay
        Gameplay.updateField (Field.setFieldMap fieldMap) gameplay
    
    static member transitionFieldMap direction gameplay =
        let player = Gameboard.tryGetCharacter PlayerIndex gameplay.Gameboard |> Option.get // assume player exists
        let gameplay = Gameplay.updateGameboard (Gameboard.tryRemoveCharacter PlayerIndex) gameplay
        let gameplay = Gameplay.updateMetaMap (MetaMap.transition direction) gameplay
        let gameplay = Gameplay.resetFieldMap (FieldMap.makeFromMetaTile gameplay.MetaMap.Current) gameplay
        let newCoordinates =
            match direction with
            | Upward | Rightward -> gameplay.MetaMap.Current.PathStart
            | Downward | Leftward -> gameplay.MetaMap.Current.PathEnd
        Gameplay.updateGameboard (Gameboard.addCharacter player newCoordinates) gameplay

    (* Interaction Commands *)
    
    static member issueAction index action gameplay =
        Gameplay.updateActionsIssued (Map.add index action) gameplay

    static member finishAction index gameplay =
        let gameplay = Gameplay.updateGameboard (Gameboard.removeCharacterTurn index) gameplay
        Gameplay.updateActionsIssued (Map.remove index) gameplay

    static member tryIssueAction time index action gameplay =
        match Gameboard.tryGetCharacter index gameplay.Gameboard with
        | Some character when character.IsAlive ->
            let gameplay = Gameplay.issueAction index action gameplay
            let gameplay = Gameplay.tryActivateCharacter time index gameplay
            let gameplay = Gameplay.tryApplyCharacterAction index gameplay
            if index = PlayerIndex then
                match action with
                | Step _ -> Gameplay.updatePlayerNavigation (constant ManualNavigation) gameplay
                | Travel path -> Gameplay.updatePlayerNavigation (constant (AutomaticNavigation path)) gameplay
                | _ -> gameplay
            else gameplay
        | _ -> gameplay

    static member addWalkingEnemies enemies gameplay =
        Gameplay.updateWalkingEnemies (constant enemies) gameplay

    static member removeWalkingEnemies gameplay =
        Gameplay.updateWalkingEnemies (constant []) gameplay

    static member addAttackingEnemies group gameplay =
        Gameplay.updateAttackingEnemies (constant group) gameplay

    static member removeHeadFromAttackingEnemies gameplay =
        Gameplay.updateAttackingEnemies List.tail gameplay
    
    static member tryInterruptPlayer (gameplay : Gameplay) =
        if gameplay.IsPlayerTraveling then
            Gameplay.updatePlayerNavigation (fun navigation ->
                match navigation with
                | AutomaticNavigation (head :: _) -> AutomaticNavigation [head]
                | _ -> navigation)
                gameplay
        else gameplay
    
    static member tryCutGrass coordinates gameplay =
        match Map.tryFind coordinates gameplay.Gameboard.Props with
        | Some prop when prop = LongGrass ->
            let gameplay = Gameplay.updateGameboard (Gameboard.removeProp coordinates) gameplay
            if Gen.random1 10 = 0 then Gameplay.makeRandomPickup coordinates gameplay else gameplay
        | Some _ | None -> gameplay
    
    static member tryKillCharacter index gameplay =
        match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
        | Some coordinates ->
            let gameplay = Gameplay.updateGameboard (Gameboard.tryRemoveCharacter index) gameplay
            match index with
            | EnemyIndex _ ->
                let gameplay = if Gen.randomb then Gameplay.updateGameboard (Gameboard.addPickup Health coordinates) gameplay else gameplay
                let gameplay = Gameplay.updateWalkingEnemies (List.filter (fun character -> Gameboard.doesCharacterExist character gameplay.Gameboard)) gameplay
                let gameplay = Gameplay.updateAttackingEnemies (List.filter (fun character -> Gameboard.doesCharacterExist character gameplay.Gameboard)) gameplay
                gameplay
            | PlayerIndex -> gameplay
        | None -> gameplay

    (* Advance Commands *)
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay =
                match Gameboard.tryGetPickup coordinates gameplay.Gameboard with
                | Some Health -> Gameplay.tryUpdateCharacter index (Character.updateHitPoints (fun x -> x + 15)) gameplay
                | Some (Item item) -> Gameplay.updateInventory (Inventory.tryAddItem item) gameplay
                | None -> gameplay
            Gameplay.updateGameboard (Gameboard.removePickup coordinates) gameplay
        | _ -> gameplay
    
    static member tryApplyStep index direction gameplay =
        match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
        | Some coordinates ->
            let coordinates = coordinates + dtovc direction
            let gameplay = Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
            let gameplay =
                if Map.containsKey coordinates gameplay.Gameboard.Pickups
                then Gameplay.tryPickupHealth index coordinates gameplay
                else gameplay
            Gameplay.tryRelocateCharacter index coordinates gameplay
        | None -> gameplay
    
    static member tryApplyAttack index reaction gameplay =
        match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
        | Some coordinates ->
            match reaction with
            | CharacterReaction characterIndex ->
                let damage = 4 // NOTE: just hard-coding damage for now
                let characterCoordinates = Gameboard.tryGetCharacterCoordinates characterIndex gameplay.Gameboard |> Option.get // we know it's there - we just got it
                let direction = Math.directionToTarget coordinates characterCoordinates
                let gameplay = Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
                let gameplay = Gameplay.tryUpdateCharacter characterIndex (Character.updateHitPoints (fun hp -> hp - damage)) gameplay
                if characterIndex = PlayerIndex then Gameplay.tryInterruptPlayer gameplay else gameplay
            | PropReaction propCoordinates ->
                let direction = Math.directionToTarget coordinates propCoordinates
                Gameplay.tryUpdateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
            | PickupReaction _ -> gameplay
        | None -> gameplay
    
    static member tryApplyCharacterAction index gameplay =
        match gameplay.ActionsIssued.[index] with // assume index is valid
        | Step direction ->
            Gameplay.tryApplyStep index direction gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
                | Some coordinates ->
                    let direction = Math.directionToTarget coordinates head.Coordinates
                    Gameplay.tryApplyStep index direction gameplay
                | None -> gameplay
            | [] -> gameplay
        | Attack reaction ->
            Gameplay.tryApplyAttack index reaction gameplay
        | Shoot reaction ->
            let gameplay = Gameplay.updateInventory (Inventory.removeItem (Special MagicMissile)) gameplay
            Gameplay.tryApplyAttack index reaction gameplay
    
    static member tryActivateCharacter time index gameplay =
        let action = gameplay.ActionsIssued.[index] // assume index is valid
        match Gameboard.tryGetCharacterCoordinates index gameplay.Gameboard with
        | Some coordinates ->
            let turn =
                match action with
                | Step direction ->
                    Turn.makeWalk time index false coordinates direction
                | Travel path ->
                    let direction = Math.directionToTarget coordinates path.Head.Coordinates
                    Turn.makeWalk time index true coordinates direction
                | Attack reaction ->
                    let direction =
                        match reaction with
                        | CharacterReaction characterIndex -> Gameboard.tryGetCharacterCoordinates characterIndex gameplay.Gameboard |> Option.get |> Math.directionToTarget coordinates // assume character exists
                        | PickupReaction _ -> failwithumf ()
                        | PropReaction propCoordinates -> Math.directionToTarget coordinates propCoordinates
                    Turn.makeAttack time index NormalAttack reaction coordinates direction
                | Shoot reaction ->
                    let direction =
                        match reaction with
                        | CharacterReaction characterIndex -> Gameboard.tryGetCharacterCoordinates characterIndex gameplay.Gameboard |> Option.get |> Math.directionToTarget coordinates // assume character exists
                        | PickupReaction _ -> failwithumf ()
                        | PropReaction _ -> failwithumf ()
                    Turn.makeAttack time index MissileAttack reaction coordinates direction
            Gameplay.updateGameboard (Gameboard.addCharacterTurn turn) gameplay
        | None -> gameplay

    static member advanceTime gameplay =
        { gameplay with Time = inc gameplay.Time }

    (* Population Commands *)
    
    static member makeRandomPickup coordinates gameplay =
        let pickup = if Gen.randomb then Health else (Item (Special MagicMissile))
        Gameplay.updateGameboard (Gameboard.addPickup pickup coordinates) gameplay

    static member makeLongGrass coordinates gameplay =
        Gameplay.updateGameboard (Gameboard.addProp LongGrass coordinates) gameplay

    static member makeLongGrasses gameplay =
        let mapBounds = v4iBounds v2iZero (Constants.Gameplay.FieldMapSizeC - v2iOne)
        let predicate1 coordinates = Math.isPointInBoundsI coordinates mapBounds && Map.find coordinates gameplay.Field.FieldMap.FieldTiles = FieldMap.GrassTile
        let predicate2 coordinates = FieldMap.hasAtLeastNAdjacentTiles 2 coordinates FieldMap.TreeTile mapBounds gameplay.Field.FieldMap.FieldTiles
        let unoccupiedSpaces = Gameboard.getUnoccupiedSpaces gameplay.Gameboard
        Set.fold (fun gameplay coordinates ->
            if predicate1 coordinates && predicate2 coordinates
            then Gameplay.makeLongGrass coordinates gameplay
            else gameplay)
            gameplay
            unoccupiedSpaces
    
    static member makeProps gameplay =
        Gameplay.makeLongGrasses gameplay
    
    static member makeEnemy index gameplay =
        let unoccupiedSpaces = Gameboard.getUnoccupiedSpaces gameplay.Gameboard
        let coordinates = Seq.item (Gen.random1 (Set.count unoccupiedSpaces)) unoccupiedSpaces
        Gameplay.updateGameboard (Gameboard.addCharacter (Character.makeEnemy index) coordinates) gameplay
    
    static member makeEnemies enemyCount gameplay =
        Seq.fold
            (fun gameplay index -> Gameplay.makeEnemy (EnemyIndex index) gameplay)
            gameplay
            [0 .. dec enemyCount]

    static member clearPickups gameplay =
        Gameplay.updateGameboard Gameboard.clearPickups gameplay

    static member clearProps gameplay =
        Gameplay.updateGameboard Gameboard.clearProps gameplay

    static member clearEnemies gameplay =
        Gameplay.updateGameboard Gameboard.clearEnemies gameplay

    (* Constructors *)

    static member initial =
        let field = Field.initial
        { Time = 0L
          InputMode = NormalInputMode
          ShallLoadGame = false
          Inventory = Inventory.initial
          MetaMap = MetaMap.initial
          Field = field
          Gameboard = Gameboard.make field.FieldMap
          ActionsIssued = Map.empty
          WalkingEnemies = []
          AttackingEnemies = []
          PlayerNavigation = Idling }