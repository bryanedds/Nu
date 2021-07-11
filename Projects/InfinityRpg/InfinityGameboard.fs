namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type [<StructuralEquality; NoComparison>] AttackType =
    | NormalAttack
    | MissileAttack

type [<StructuralEquality; NoComparison>] TurnType =
    | WalkTurn of bool
    | AttackTurn of AttackType
    member this.IsWalking = match this with WalkTurn _ -> true | _ -> false
    member this.IsAttacking = match this with AttackTurn _ -> true | _ -> false

type [<StructuralEquality; NoComparison>] TurnStatus =
    | TurnBeginning
    | TurnTicking
    | TurnFinishing

type [<StructuralEquality; NoComparison>] TurnReaction =
    | CharacterReaction of CharacterIndex
    | PickupReaction of Vector2i
    | PropReaction of Vector2i

type [<ReferenceEquality; NoComparison>] Turn =
    { TurnType : TurnType
      TurnStatus : TurnStatus
      TurnReactionOpt : TurnReaction option
      CharacterIndex : CharacterIndex
      OriginCoordinates : Vector2i
      Direction : Direction
      StartTime : int64 }

    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member updateStartTime updater (turn : Turn) =
        { turn with StartTime = updater turn.StartTime }
    
    static member hasCharacterReaction characterIndex turn =
        match turn.TurnReactionOpt with
        | Some reaction ->
            match reaction with
            | CharacterReaction index -> index = characterIndex
            | _ -> false
        | None -> false
    
    static member computeCharacterPosition time turn =
        match turn.TurnType with
        | WalkTurn _ ->
            match turn.TurnStatus with
            | TurnBeginning -> vctovf turn.OriginCoordinates
            | TurnTicking ->
                let tickCount = int (time - turn.StartTime + 1L)
                let offset = Constants.Gameplay.CharacterWalkStep * int tickCount
                let offsetVector = dtovfScaled turn.Direction (single offset)
                vctovf turn.OriginCoordinates + offsetVector
            | TurnFinishing -> turn.OriginCoordinates + dtovc turn.Direction |> vctovf
        | _ -> vctovf turn.OriginCoordinates
    
    static member toCharacterAnimationState turn =
        let animationType =
            match turn.TurnType with
            | AttackTurn attackTurnType ->
                match turn.TurnStatus with
                | TurnBeginning
                | TurnTicking -> match attackTurnType with NormalAttack -> CharacterAnimationActing | MissileAttack -> CharacterAnimationSpecial
                | _ -> CharacterAnimationFacing
            | WalkTurn _ -> CharacterAnimationFacing
        CharacterAnimationState.make turn.StartTime animationType turn.Direction

    static member makeWalk time index multiRoundContext originC direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnBeginning
          TurnReactionOpt = None
          CharacterIndex = index
          OriginCoordinates = originC
          Direction = direction
          StartTime = time }

    static member makeAttack time index magicMissile reaction originC direction  =
        { TurnType = AttackTurn magicMissile
          TurnStatus = TurnBeginning
          TurnReactionOpt = Some reaction
          CharacterIndex = index
          OriginCoordinates = originC
          Direction = direction
          StartTime = time }

/// TODO: turn this into an abstract data type.
type [<ReferenceEquality; NoComparison>] Gameboard =
    { Spaces : Vector2i Set
      Pickups : Map<Vector2i, PickupType>
      Props : Map<Vector2i, PropType>
      Characters : Map<Vector2i, Character>
      CharacterTurns : Turn list }

    member this.AnyTurnsInProgress =
        List.notEmpty this.CharacterTurns
    
    static member updateSpaces updater gameboard =
        { gameboard with Spaces = updater gameboard.Spaces }

    static member updatePickupSpaces updater gameboard =
        { gameboard with Pickups = updater gameboard.Pickups }

    static member updatePropSpaces updater gameboard =
        { gameboard with Props = updater gameboard.Props }
    
    static member updateCharacterSpaces updater gameboard =
        { gameboard with Characters = updater gameboard.Characters }

    static member getOccupiedSpaces gameboard =
        let characterSpaces = gameboard.Characters |> Map.toKeySeq |> Set.ofSeq
        let propSpaces = gameboard.Props |> Map.toKeySeq |> Set.ofSeq
        Set.union characterSpaces propSpaces

    static member getUnoccupiedSpaces gameboard =
        let occupiedSpaces = Gameboard.getOccupiedSpaces gameboard
        Set.difference gameboard.Spaces occupiedSpaces

    static member getOpenDirections coordinates gameboard =
        let unoccupiedSpaces = Gameboard.getUnoccupiedSpaces gameboard
        [Upward; Rightward; Downward; Leftward] |>
        Set.ofList |>
        Set.filter (fun direction -> Set.exists (fun coordinates2 -> coordinates2 = coordinates + dtovc direction) unoccupiedSpaces) 

    static member getEnemySpaces gameboard =
        Map.filter
            (fun _ (character : Character) -> character.CharacterIndex.IsEnemy)
            gameboard.Characters

    static member getEnemies gameboard =
        let enemySpaces = Gameboard.getEnemySpaces gameboard
        Map.toValueList enemySpaces

    static member getEnemyIndices gameboard =
        let enemies = Gameboard.getEnemies gameboard
        List.map (fun (enemy : Character) -> enemy.CharacterIndex) enemies

    static member tryGetCharacterCoordinates index gameboard =
        Map.tryFindKey (fun _ (character : Character) -> character.CharacterIndex = index) gameboard.Characters

    static member tryGetCharacterAtCoordinates coordinates gameboard =
        Map.tryFind coordinates gameboard.Characters

    static member tryGetCharacter index gameboard =
        match Gameboard.tryGetCharacterCoordinates index gameboard with
        | Some coordinates -> Gameboard.tryGetCharacterAtCoordinates coordinates gameboard
        | None -> None
        
    static member tryGetPlayer (gameboard : Gameboard) =
        Gameboard.tryGetCharacter PlayerIndex gameboard
        
    static member getPlayer (gameboard : Gameboard) =
        match Gameboard.tryGetPlayer gameboard with
        | Some player -> player
        | None -> failwithumf ()

    static member tryGetPickup coordinates gameboard =
        Map.tryFind coordinates gameboard.Pickups

    static member getNavigationMap currentCoordinates gameboard =
        let navigableSpaces coordinates =
            not
                (Map.containsKey coordinates gameboard.Characters &&
                 (coordinates = currentCoordinates + dtovc Upward ||
                  coordinates = currentCoordinates + dtovc Rightward ||
                  coordinates = currentCoordinates + dtovc Downward ||
                  coordinates = currentCoordinates + dtovc Leftward))
        gameboard.Spaces |>
        Set.removeMany (Map.toKeyList gameboard.Props) |>
        Set.filter navigableSpaces |>
        Map.ofSeqBy (fun space -> (space, false))

    static member doesCharacterExist index gameboard =
        Map.exists
            (fun _ (character : Character) -> character.CharacterIndex = index)
            gameboard.Characters

    static member addCharacter character coordinates (gameboard : Gameboard) =
        Gameboard.updateCharacterSpaces (Map.add coordinates character) gameboard

    static member tryUpdateCharacter index updater gameboard =
        match Gameboard.tryGetCharacterCoordinates index gameboard with
        | Some coordinates ->
            let character = Gameboard.tryGetCharacterAtCoordinates coordinates gameboard |> Option.get // we know it's there - we just got it
            Gameboard.updateCharacterSpaces (Map.add coordinates (updater character)) gameboard
        | None -> gameboard

    static member tryRemoveCharacter index gameboard =
        match Gameboard.tryGetCharacterCoordinates index gameboard with
        | Some coordinates -> Gameboard.updateCharacterSpaces (Map.remove coordinates) gameboard
        | None -> gameboard

    static member tryRelocateCharacter index coordinates (gameboard : Gameboard) =
        match Gameboard.tryGetCharacterCoordinates index gameboard with
        | Some oldCoordinates ->
            let character = Gameboard.tryGetCharacterAtCoordinates oldCoordinates gameboard |> Option.get // we know it's there - we just got it
            let gameboard = Gameboard.updateCharacterSpaces (Map.remove oldCoordinates) gameboard
            Gameboard.addCharacter character coordinates gameboard
        | None -> gameboard

    static member clearEnemies (gameboard : Gameboard) =
        Gameboard.updateCharacterSpaces (Map.filter (fun _ character -> character.IsAlly)) gameboard

    static member addProp prop coordinates gameboard =
        Gameboard.updatePropSpaces (Map.add coordinates prop) gameboard

    static member removeProp coordinates gameboard =
        Gameboard.updatePropSpaces (Map.remove coordinates) gameboard

    static member clearProps gameboard =
        Gameboard.updatePropSpaces (constant Map.empty) gameboard

    static member addPickup pickup coordinates gameboard =
        Gameboard.updatePickupSpaces (Map.add coordinates pickup) gameboard

    static member removePickup coordinates gameboard =
        Gameboard.updatePickupSpaces (Map.remove coordinates) gameboard

    static member clearPickups gameboard =
        Gameboard.updatePickupSpaces (constant Map.empty) gameboard

    static member setFieldSpaces fieldMap gameboard =
        let spaces =
            fieldMap.FieldTiles |>
            Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |>
            Map.toKeySeq |>
            Set.ofSeq
        Gameboard.updateSpaces (constant spaces) gameboard

    static member transitionMap fieldMap gameboard =
        match Gameboard.tryGetCharacterCoordinates PlayerIndex gameboard with
        | Some coordinates ->
            let player = Gameboard.tryGetCharacterAtCoordinates coordinates gameboard |> Option.get // we know it's there - we just got it
            let gameboard = Gameboard.setFieldSpaces fieldMap gameboard
            Gameboard.addCharacter player coordinates gameboard
        | None -> gameboard

    static member tryGetCharacterTurn index gameboard =
        List.tryFind (fun x -> x.CharacterIndex = index) gameboard.CharacterTurns

    static member tryGetOpponentTurn index gameboard =
        List.tryFind (Turn.hasCharacterReaction index) gameboard.CharacterTurns
    
    static member tryGetAttackingEnemyTurn gameboard =
        List.tryFind (fun x -> x.CharacterIndex.IsEnemy && x.TurnType.IsAttacking) gameboard.CharacterTurns
    
    static member getCharacterTurn index gameboard =
        List.find (fun x -> x.CharacterIndex = index) gameboard.CharacterTurns
    
    static member getCharacterTurnIsInProgress index gameboard =
        List.exists (fun x -> x.CharacterIndex = index) gameboard.CharacterTurns

    static member getActingCharacterIndices gameboard =
        List.map (fun x -> x.CharacterIndex) gameboard.CharacterTurns
    
    static member updateCharacterTurns updater gameboard =
        { gameboard with CharacterTurns = updater gameboard.CharacterTurns }

    static member tryUpdateCharacterTurn index updater gameboard =
        Gameboard.updateCharacterTurns (fun turns ->
            List.map (fun character ->
                if character.CharacterIndex = index
                then updater character
                else character)
                turns)
            gameboard

    static member getPropMap props gameboard time =
        let getProp coordinates _ =
            let animationType =
                match Gameboard.tryGetCharacterTurn PlayerIndex gameboard with
                | Some turn ->
                    match turn.TurnReactionOpt with
                    | Some reaction ->
                        match reaction with
                        | CharacterReaction _ -> PropAnimationStanding
                        | PickupReaction _ -> PropAnimationStanding
                        | PropReaction propCoordinates ->
                            if propCoordinates = coordinates then
                                match turn.TurnStatus with
                                | TurnBeginning -> PropAnimationStanding
                                | TurnTicking ->
                                    if time - turn.StartTime + 1L < Constants.Gameplay.ReactionTick
                                    then PropAnimationStanding
                                    else PropAnimationDestroyed
                                | TurnFinishing -> PropAnimationDestroyed
                            else PropAnimationStanding
                    | None -> PropAnimationStanding
                | None -> PropAnimationStanding
            Prop.makeLongGrass coordinates animationType
        props |>
        Map.toSeqBy getProp |>
        Map.indexed

    static member getCharacterMap characters gameboard time =
        let getCharacterEntry coordinates (character : Character) =
            let index = match character.CharacterIndex with PlayerIndex -> 0 | EnemyIndex i -> inc i
            let turnOpt = Gameboard.tryGetCharacterTurn character.CharacterIndex gameboard
            let position = match turnOpt with Some turn -> Turn.computeCharacterPosition time turn | None -> vctovf coordinates
            let characterAnimationState =
                match turnOpt with
                | None ->
                    let animationType =
                        if character.IsDead then
                            match Gameboard.tryGetOpponentTurn character.CharacterIndex gameboard with
                            | Some attackerTurn ->
                                match attackerTurn.TurnStatus with
                                | TurnBeginning -> CharacterAnimationFacing
                                | TurnTicking ->
                                    if time - attackerTurn.StartTime + 1L < Constants.Gameplay.ReactionTick
                                    then CharacterAnimationFacing
                                    else CharacterAnimationSlain
                                | TurnFinishing -> CharacterAnimationSlain
                            | None -> CharacterAnimationSlain
                        else CharacterAnimationFacing
                    CharacterAnimationState.make time animationType character.FacingDirection
                | Some turn -> Turn.toCharacterAnimationState turn
            (index, (position, characterAnimationState, time))
        characters |>
        Map.toSeqBy getCharacterEntry |>
        Map.ofSeq

    static member addCharacterTurn turn gameboard =
        Gameboard.updateCharacterTurns (List.cons turn) gameboard

    static member removeCharacterTurn index gameboard =
        Gameboard.updateCharacterTurns (List.filter (fun turn -> turn.CharacterIndex <> index)) gameboard

    static member empty =
        { Spaces = Set.empty
          Pickups = Map.empty
          Props = Map.empty
          Characters = Map.empty
          CharacterTurns = [] }

    static member make fieldMap =
        let gameboard = Gameboard.setFieldSpaces fieldMap Gameboard.empty
        Gameboard.addCharacter (Character.makePlayer ()) v2iZero gameboard