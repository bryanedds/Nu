namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

type CharacterIndex =
    | EnemyIndex of int
    | PlayerIndex

    member this.IsEnemy =
        match this with EnemyIndex _ -> true | PlayerIndex -> false

    member this.IsAlly =
        not this.IsEnemy

type [<CustomEquality; NoComparison>] NavigationNode =
    { PositionM : Vector2i
      mutable Neighbors : NavigationNode list } // OPTIMIZATION: has to be mutable to be efficiently populated.

    interface NavigationNode IHasNeighbors with
        member this.Neighbors = this.Neighbors :> _ seq

    interface NavigationNode IEquatable with
        member this.Equals that =
            this.PositionM = that.PositionM

    override this.Equals that =
        match that with
        | :? NavigationNode as that -> this.PositionM = that.PositionM
        | _ -> false

    override this.GetHashCode () =
        this.PositionM.GetHashCode ()

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
      OriginM : Vector2i
      Direction : Direction
      StartTick : int64 }
    
    static member calculatePosition turn =
        match turn.TurnType with
        | WalkTurn _ ->
            match turn.TurnStatus with
            | TurnPending
            | TurnBeginning -> vmtovf turn.OriginM
            | TurnTicking tickCount ->
                let offset = (int Constants.InfinityRpg.CharacterWalkResolution) * (int tickCount)
                let offsetVector = dtovfBy turn.Direction offset
                vmtovf turn.OriginM + offsetVector
            | TurnFinishing -> turn.OriginM + dtovm turn.Direction |> vmtovf
        | _ -> vmtovf turn.OriginM
    
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

    static member turnsToCharacterAnimationState characterIndex (characterState : CharacterState) characterTurns =
        match List.tryFind (fun turn -> turn.Actor = characterIndex) characterTurns with
        | None ->
            let animationType =
                if not characterState.IsAlive then
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
            CharacterAnimationState.make 0L animationType characterState.FacingDirection
        | Some turn -> Turn.toCharacterAnimationState turn
    
    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member updateStartTick updater turn =
        { turn with StartTick = updater turn.StartTick }

    static member incTickCount turn =
        Turn.updateTurnStatus TurnStatus.incTickCount turn
    
    static member makeWalk index multiRoundContext originM direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = None
          OriginM = originM
          Direction = direction
          StartTick = 0L }

    static member makeAttack index targetIndex originM direction =
        { TurnType = AttackTurn
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = Some targetIndex
          OriginM = originM
          Direction = direction
          StartTick = 0L }