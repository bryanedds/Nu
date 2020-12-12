namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type [<ReferenceEquality; NoComparison>] PuppetState =
    { HitPoints : int }

    static member updateHitPoints updater (puppetState : PuppetState) =
        { puppetState with HitPoints = updater puppetState.HitPoints }
    
    static member makeFromCharacter (character : Character) =
        { HitPoints = character.HitPoints }

type [<ReferenceEquality; NoComparison>] TurnType =
    | WalkTurn of bool
    | AttackTurn

    member this.IsWalking =
        match this with
        | WalkTurn _ -> true
        | _ -> false

type [<ReferenceEquality; NoComparison>] TurnStatus =
    | TurnBeginning
    | TurnTicking
    | TurnFinishing

type [<ReferenceEquality; NoComparison>] Reactor =
    | ReactingCharacter of CharacterIndex
    | ReactingProp of Vector2i

type [<ReferenceEquality; NoComparison>] Turn =
    { TurnType : TurnType
      TurnStatus : TurnStatus
      Actor : CharacterIndex
      ReactorOpt : Reactor option
      OriginCoordinates : Vector2i
      Direction : Direction
      StartTick : int64 }
    
    member this.GetReactor =
        match this.ReactorOpt with
        | Some reactor -> reactor
        | None -> failwithumf ()
    
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
                | TurnTicking -> CharacterAnimationActing
                | _ -> CharacterAnimationFacing
            | WalkTurn _ -> CharacterAnimationFacing
        CharacterAnimationState.make turn.StartTick animationType turn.Direction

    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member updateStartTick updater turn =
        { turn with StartTick = updater turn.StartTick }

    static member makeWalk time index multiRoundContext originC direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnBeginning
          Actor = index
          ReactorOpt = None
          OriginCoordinates = originC
          Direction = direction
          StartTick = time }

    static member makeAttack time index reactor originC direction =
        { TurnType = AttackTurn
          TurnStatus = TurnBeginning
          Actor = index
          ReactorOpt = Some reactor
          OriginCoordinates = originC
          Direction = direction
          StartTick = time }

type [<ReferenceEquality; NoComparison>] Puppeteer =
    { CharacterTurns : Turn list
      PlayerPuppetState : PuppetState }

    member this.AnyTurnsInProgress = 
        List.notEmpty this.CharacterTurns

    static member tryGetCharacterTurn index puppeteer =
        List.tryFind (fun x -> x.Actor = index) puppeteer.CharacterTurns

    static member tryGetOpponentTurn index puppeteer =
        List.tryFind (Turn.hasParticularReactor index) puppeteer.CharacterTurns
    
    static member tryGetAttackingEnemyTurn puppeteer =
        List.tryFind (fun x -> x.Actor.IsEnemy && x.TurnType = AttackTurn) puppeteer.CharacterTurns
    
    static member getCharacterTurn index puppeteer =
        List.find (fun x -> x.Actor = index) puppeteer.CharacterTurns
    
    static member turnInProgress index puppeteer =
        List.exists (fun x -> x.Actor = index) puppeteer.CharacterTurns

    static member getActingCharacters puppeteer =
        List.map (fun x -> x.Actor) puppeteer.CharacterTurns
    
    static member updateCharacterTurns updater puppeteer =
        { puppeteer with CharacterTurns = updater puppeteer.CharacterTurns }

    static member updatePlayerPuppetState updater puppeteer =
        { puppeteer with PlayerPuppetState = updater puppeteer.PlayerPuppetState }

    static member addCharacterTurn turn puppeteer =
        Puppeteer.updateCharacterTurns (fun x -> turn :: x) puppeteer

    static member updateCharacterTurn index updater puppeteer =
        Puppeteer.updateCharacterTurns (fun turns -> List.map (fun x -> if x.Actor = index then updater x else x) turns) puppeteer

    static member removeCharacterTurn index puppeteer =
        Puppeteer.updateCharacterTurns (fun turns -> List.filter (fun x -> x.Actor <> index) turns) puppeteer

    static member updatePlayerPuppetHitPoints updater puppeteer =
        Puppeteer.updatePlayerPuppetState (PuppetState.updateHitPoints updater) puppeteer

    static member getPropMap props puppeteer time =
        let generator coordinates _ =
            let animationType =
                match Puppeteer.tryGetCharacterTurn PlayerIndex puppeteer with
                | Some turn ->
                    match turn.ReactorOpt with
                    | Some reactor ->
                        match reactor with
                        | ReactingProp propCoords ->
                            if propCoords = coordinates then
                                match turn.TurnStatus with
                                | TurnBeginning -> PropAnimationStanding
                                | TurnTicking ->
                                    if time - turn.StartTick + 1L < Constants.InfinityRpg.ReactionTick
                                    then PropAnimationStanding
                                    else PropAnimationDestroyed
                                | TurnFinishing -> PropAnimationDestroyed
                            else PropAnimationStanding
                        | _ -> PropAnimationStanding
                    | None -> PropAnimationStanding
                | None -> PropAnimationStanding
            Prop.makeLongGrass coordinates animationType
        props |>
        Map.toSeqBy generator |>
        Map.indexed
    
    static member getCharacterMap characters puppeteer time =
        let generator coordinates character =
            let index = match character.CharacterIndex with PlayerIndex -> 0 | EnemyIndex i -> inc i
            let turnOpt = Puppeteer.tryGetCharacterTurn character.CharacterIndex puppeteer
            let position = match turnOpt with Some turn -> Turn.calculatePosition time turn | None -> vctovf coordinates
            let characterAnimationState =
                match turnOpt with
                | None ->
                    let animationType =
                        if not character.IsAlive then
                            match Puppeteer.tryGetOpponentTurn character.CharacterIndex puppeteer with
                            | Some attackerTurn ->
                                match attackerTurn.TurnStatus with
                                | TurnBeginning -> CharacterAnimationFacing
                                | TurnTicking ->
                                    if time - attackerTurn.StartTick + 1L < Constants.InfinityRpg.ReactionTick
                                    then CharacterAnimationFacing
                                    else CharacterAnimationSlain
                                | TurnFinishing -> CharacterAnimationSlain
                            | None -> CharacterAnimationSlain
                        else CharacterAnimationFacing
                    CharacterAnimationState.make time animationType character.FacingDirection
                | Some turn -> Turn.toCharacterAnimationState turn
            (index, (position, characterAnimationState, time))
        characters |>
        Map.toListBy generator |>
        Map.ofSeq

    static member init (player : Character) =
        { CharacterTurns = []
          PlayerPuppetState = PuppetState.makeFromCharacter player }