namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type TurnType =
    | WalkTurn of bool
    | AttackTurn

type TurnStatus =
    | TurnPending
    | TurnBeginning
    | TurnTicking of int64
    | TurnFinishing

    static member incTickCount turnStatus =
        match turnStatus with
        | TurnTicking tickCount -> TurnTicking (inc tickCount)
        | _ -> turnStatus

type Turn =
    { TurnType : TurnType
      TurnStatus : TurnStatus
      Actor : CharacterIndex
      ReactorOpt : CharacterIndex option
      OriginCoordinates : Vector2i
      Direction : Direction
      StartTick : int64 }
    
    static member calculatePosition turn =
        match turn.TurnType with
        | WalkTurn _ ->
            match turn.TurnStatus with
            | TurnPending
            | TurnBeginning -> vctovf turn.OriginCoordinates
            | TurnTicking tickCount ->
                let offset = Constants.InfinityRpg.CharacterWalkStep * int tickCount
                let offsetVector = dtovfScaled turn.Direction (single offset)
                vctovf turn.OriginCoordinates + offsetVector
            | TurnFinishing -> turn.OriginCoordinates + dtovc turn.Direction |> vctovf
        | _ -> vctovf turn.OriginCoordinates
    
    static member toCharacterAnimationState turn =
        let animationType =
            match turn.TurnType with
            | AttackTurn ->
                match turn.TurnStatus with
                | TurnBeginning
                | TurnTicking _ -> CharacterAnimationActing
                | _ -> CharacterAnimationFacing
            | WalkTurn _ -> CharacterAnimationFacing
        CharacterAnimationState.make turn.StartTick animationType turn.Direction

    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member updateStartTick updater turn =
        { turn with StartTick = updater turn.StartTick }

    static member incTickCount turn =
        Turn.updateTurnStatus TurnStatus.incTickCount turn
    
    static member makeWalk index multiRoundContext originC direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = None
          OriginCoordinates = originC
          Direction = direction
          StartTick = 0L }

    static member makeAttack index targetIndex originC direction =
        { TurnType = AttackTurn
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = Some targetIndex
          OriginCoordinates = originC
          Direction = direction
          StartTick = 0L }

type [<ReferenceEquality; NoComparison>] Puppeteer =
    { CharacterTurns : Turn list }

    static member initial =
        { CharacterTurns = [] }

    static member tryGetCharacterTurn index puppeteer =
        List.tryFind (fun x -> x.Actor = index) puppeteer.CharacterTurns

    static member tryGetOpponentTurn index puppeteer =
        List.tryFind (fun x -> x.ReactorOpt = Some index) puppeteer.CharacterTurns
    
    static member getCharacterTurn index puppeteer =
        List.find (fun x -> x.Actor = index) puppeteer.CharacterTurns
    
    static member turnInProgress index puppeteer =
        List.exists (fun x -> x.Actor = index) puppeteer.CharacterTurns

    member this.AnyTurnsInProgress = 
        List.notEmpty this.CharacterTurns
    
    static member updateCharacterTurns updater puppeteer =
        { puppeteer with CharacterTurns = updater puppeteer.CharacterTurns }

    static member addCharacterTurn turn puppeteer =
        Puppeteer.updateCharacterTurns (fun x -> turn :: x) puppeteer

    static member updateCharacterTurn index updater puppeteer =
        Puppeteer.updateCharacterTurns (fun turns -> List.map (fun x -> if x.Actor = index then updater x else x) turns) puppeteer

    static member removeCharacterTurn index puppeteer =
        Puppeteer.updateCharacterTurns (fun turns -> List.filter (fun x -> x.Actor <> index) turns) puppeteer

    static member generatePositionsAndAnimationStates characters puppeteer =
        let generator coordinates character =
            let index = match character.CharacterIndex with PlayerIndex -> 0 | EnemyIndex i -> inc i
            let turnOpt = Puppeteer.tryGetCharacterTurn character.CharacterIndex puppeteer
            let position = match turnOpt with Some turn -> Turn.calculatePosition turn | None -> vctovf coordinates
            let characterAnimationState =
                match turnOpt with
                | None ->
                    let animationType =
                        if not character.IsAlive then
                            match Puppeteer.tryGetOpponentTurn character.CharacterIndex puppeteer with
                            | Some attackerTurn ->
                                match attackerTurn.TurnStatus with
                                | TurnPending
                                | TurnBeginning -> CharacterAnimationFacing
                                | TurnTicking tickCount ->
                                    if tickCount < Constants.InfinityRpg.ReactionTick
                                    then CharacterAnimationFacing
                                    else CharacterAnimationSlain
                                | TurnFinishing -> CharacterAnimationSlain
                            | None -> CharacterAnimationSlain
                        else CharacterAnimationFacing
                    CharacterAnimationState.make 0L animationType character.FacingDirection
                | Some turn -> Turn.toCharacterAnimationState turn
            (index, (position, characterAnimationState))
        Map.toListBy generator characters
                
