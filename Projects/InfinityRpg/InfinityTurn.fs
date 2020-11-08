namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type [<CustomEquality; NoComparison>] NavigationNode =
    { Coordinates : Vector2i
      mutable Neighbors : NavigationNode list } // OPTIMIZATION: has to be mutable to be efficiently populated.

    interface NavigationNode IHasNeighbors with
        member this.Neighbors = this.Neighbors :> _ seq

    interface NavigationNode IEquatable with
        member this.Equals that =
            this.Coordinates = that.Coordinates

    override this.Equals that =
        match that with
        | :? NavigationNode as that -> this.Coordinates = that.Coordinates
        | _ -> false

    override this.GetHashCode () =
        this.Coordinates.GetHashCode ()

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
                let offset = (int Constants.InfinityRpg.CharacterWalkResolution) * (int tickCount)
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

    static member turnsToCharacterAnimationState characterIndex (character : Character) characterTurns =
        match List.tryFind (fun turn -> turn.Actor = characterIndex) characterTurns with
        | None ->
            let animationType =
                if not character.IsAlive then
                    match List.tryFind (fun (turn : Turn) -> turn.ReactorOpt = Some characterIndex) characterTurns with
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

type [<ReferenceEquality; NoComparison>] PuppetMaster =
    { CharacterTurns : Turn list }

    static member initial =
        { CharacterTurns = [] }

    static member tryGetCharacterTurn index puppetMaster =
        List.tryFind (fun x -> x.Actor = index) puppetMaster.CharacterTurns

    static member tryGetOpponentTurn index puppetMaster =
        List.tryFind (fun x -> x.ReactorOpt = Some index) puppetMaster.CharacterTurns
    
    static member getCharacterTurn index puppetMaster =
        List.find (fun x -> x.Actor = index) puppetMaster.CharacterTurns
    
    static member turnInProgress index puppetMaster =
        List.exists (fun x -> x.Actor = index) puppetMaster.CharacterTurns

    member this.AnyTurnsInProgress = 
        List.notEmpty this.CharacterTurns
    
    static member updateCharacterTurns updater puppetMaster =
        { puppetMaster with CharacterTurns = updater puppetMaster.CharacterTurns }

    static member addCharacterTurn turn puppetMaster =
        PuppetMaster.updateCharacterTurns (fun x -> turn :: x) puppetMaster

    static member updateCharacterTurn index updater puppetMaster =
        PuppetMaster.updateCharacterTurns (fun turns -> List.map (fun x -> if x.Actor = index then updater x else x) turns) puppetMaster

    static member removeCharacterTurn index puppetMaster =
        PuppetMaster.updateCharacterTurns (fun turns -> List.filter (fun x -> x.Actor <> index) turns) puppetMaster

    static member generatePositionsAndAnimationStates characters puppetMaster =
        let generator coordinates character =
            let index = match character.CharacterIndex with PlayerIndex -> 0 | EnemyIndex i -> inc i
            let turnOpt = PuppetMaster.tryGetCharacterTurn character.CharacterIndex puppetMaster
            let position = match turnOpt with Some turn -> Turn.calculatePosition turn | None -> vctovf coordinates
            let characterAnimationState =
                match turnOpt with
                | None ->
                    let animationType =
                        if not character.IsAlive then
                            match PuppetMaster.tryGetOpponentTurn character.CharacterIndex puppetMaster with
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
                
