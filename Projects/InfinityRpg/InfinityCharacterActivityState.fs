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
      Direction : Direction }
    
    static member updateTurnStatus updater turn =
        { turn with TurnStatus = updater turn.TurnStatus }

    static member incTickCount turn =
        Turn.updateTurnStatus TurnStatus.incTickCount turn
    
    static member makeWalk index multiRoundContext originM direction =
        { TurnType = WalkTurn multiRoundContext
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = None
          OriginM = originM
          Direction = direction }

    static member makeAttack index targetIndex originM direction =
        { TurnType = AttackTurn
          TurnStatus = TurnPending
          Actor = index
          ReactorOpt = Some targetIndex
          OriginM = originM
          Direction = direction } 