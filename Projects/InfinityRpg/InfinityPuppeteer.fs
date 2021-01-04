namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

// TODO: see if we can get rid of or generalize this type.
type [<ReferenceEquality; NoComparison>] Reactor =
    | ReactingCharacter of CharacterIndex
    | ReactingPickup of Vector2i
    | ReactingProp of Vector2i

type [<ReferenceEquality; NoComparison>] TurnType =
    | WalkTurn of bool
    | AttackTurn of bool
    member this.IsWalking = match this with WalkTurn _ -> true | _ -> false
    member this.IsAttacking = match this with AttackTurn _ -> true | _ -> false

type [<ReferenceEquality; NoComparison>] TurnStatus =
    | TurnBeginning
    | TurnTicking
    | TurnFinishing

type [<ReferenceEquality; NoComparison>] Turn =
    { TurnType : TurnType
      TurnStatus : TurnStatus
      CharacterIndex : CharacterIndex
      ReactorOpt : Reactor option
      OriginCoordinates : Vector2i
      Direction : Direction
      StartTick : int64 }

    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member updateStartTick updater turn =
        { turn with StartTick = updater turn.StartTick }
    
    static member tryGetReactingCharacterIndex turn =
        match turn.ReactorOpt with
        | Some (ReactingCharacter characterIndex) -> Some characterIndex
        | _ -> None
    
    static member hasParticularReactor reactorIndex turn =
        match turn.ReactorOpt with
        | Some reactor ->
            match reactor with
            | ReactingCharacter index -> index = reactorIndex
            | _ -> false
        | None -> false
    
    static member calculatePosition time turn =
        match turn.TurnType with
        | WalkTurn _ ->
            match turn.TurnStatus with
            | TurnBeginning -> vctovf turn.OriginCoordinates
            | TurnTicking ->
                let tickCount = int (time - turn.StartTick + 1L)
                let offset = Constants.Gameplay.CharacterWalkStep * int tickCount
                let offsetVector = dtovfScaled turn.Direction (single offset)
                vctovf turn.OriginCoordinates + offsetVector
            | TurnFinishing -> turn.OriginCoordinates + dtovc turn.Direction |> vctovf
        | _ -> vctovf turn.OriginCoordinates
    
    static member toCharacterAnimationState turn =
        let animationType =
            match turn.TurnType with
            | AttackTurn magicMissile ->
                match turn.TurnStatus with
                | TurnBeginning
                | TurnTicking -> if magicMissile then CharacterAnimationSpecial else CharacterAnimationActing
                | _ -> CharacterAnimationFacing
            | WalkTurn _ -> CharacterAnimationFacing
        CharacterAnimationState.make turn.StartTick animationType turn.Direction

    static member makeWalk time index multiRoundContext originC direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnBeginning
          CharacterIndex = index
          ReactorOpt = None
          OriginCoordinates = originC
          Direction = direction
          StartTick = time }

    static member makeAttack time index magicMissile reactor originC direction  =
        { TurnType = AttackTurn magicMissile
          TurnStatus = TurnBeginning
          CharacterIndex = index
          ReactorOpt = Some reactor
          OriginCoordinates = originC
          Direction = direction
          StartTick = time }

// TODO: consider folding this type into Chessboard.
type [<ReferenceEquality; NoComparison>] Puppeteer =
    { CharacterTurns : Turn list }

    member this.AnyTurnsInProgress =
        List.notEmpty this.CharacterTurns

    static member tryGetCharacterTurn index puppeteer =
        List.tryFind (fun x -> x.CharacterIndex = index) puppeteer.CharacterTurns

    static member tryGetOpponentTurn index puppeteer =
        List.tryFind (Turn.hasParticularReactor index) puppeteer.CharacterTurns
    
    static member tryGetAttackingEnemyTurn puppeteer =
        List.tryFind (fun x -> x.CharacterIndex.IsEnemy && x.TurnType.IsAttacking) puppeteer.CharacterTurns
    
    static member getCharacterTurn index puppeteer =
        List.find (fun x -> x.CharacterIndex = index) puppeteer.CharacterTurns
    
    static member getCharacterTurnIsInProgress index puppeteer =
        List.exists (fun x -> x.CharacterIndex = index) puppeteer.CharacterTurns

    static member getActingCharacterIndices puppeteer =
        List.map (fun x -> x.CharacterIndex) puppeteer.CharacterTurns
    
    static member updateCharacterTurns updater puppeteer =
        { puppeteer with CharacterTurns = updater puppeteer.CharacterTurns }

    static member tryUpdateCharacterTurn index updater puppeteer =
        Puppeteer.updateCharacterTurns (fun turns ->
            List.map (fun character ->
                if character.CharacterIndex = index
                then updater character
                else character)
                turns)
            puppeteer

    static member getPropMap props puppeteer time =
        let getProp coordinates _ =
            let animationType =
                match Puppeteer.tryGetCharacterTurn PlayerIndex puppeteer with
                | Some turn ->
                    match turn.ReactorOpt with
                    | Some reactor ->
                        match reactor with
                        | ReactingCharacter _ -> PropAnimationStanding
                        | ReactingPickup _ -> PropAnimationStanding
                        | ReactingProp propCoords ->
                            if propCoords = coordinates then
                                match turn.TurnStatus with
                                | TurnBeginning -> PropAnimationStanding
                                | TurnTicking ->
                                    if time - turn.StartTick + 1L < Constants.Gameplay.ReactionTick
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

    static member getCharacterMap characters puppeteer time =
        let getCharacterEntry coordinates (character : Character) =
            let index = match character.CharacterIndex with PlayerIndex -> 0 | EnemyIndex i -> inc i
            let turnOpt = Puppeteer.tryGetCharacterTurn character.CharacterIndex puppeteer
            let position = match turnOpt with Some turn -> Turn.calculatePosition time turn | None -> vctovf coordinates
            let characterAnimationState =
                match turnOpt with
                | None ->
                    let animationType =
                        if character.IsDead then
                            match Puppeteer.tryGetOpponentTurn character.CharacterIndex puppeteer with
                            | Some attackerTurn ->
                                match attackerTurn.TurnStatus with
                                | TurnBeginning -> CharacterAnimationFacing
                                | TurnTicking ->
                                    if time - attackerTurn.StartTick + 1L < Constants.Gameplay.ReactionTick
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

    static member addCharacterTurn turn puppeteer =
        Puppeteer.updateCharacterTurns (fun x -> turn :: x) puppeteer

    static member removeCharacterTurn index puppeteer =
        Puppeteer.updateCharacterTurns (fun turns -> List.filter (fun x -> x.CharacterIndex <> index) turns) puppeteer

    static member empty =
        { CharacterTurns = [] }