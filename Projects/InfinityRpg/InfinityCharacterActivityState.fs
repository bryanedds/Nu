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

type WalkState =
    | WalkFinished
    | WalkContinuing

type [<ReferenceEquality; NoComparison>] WalkDescriptor =
    { WalkDirection : Direction
      WalkOriginM : Vector2i }

    member this.NextPositionM =
        this.WalkOriginM + dtovm this.WalkDirection

    static member make origin direction =
        { WalkDirection = direction
          WalkOriginM = origin }

type [<ReferenceEquality; NoComparison>] NavigationDescriptor =
    { WalkDescriptor : WalkDescriptor
      MultiRoundContext : bool }

    member this.NextPositionM =
        this.WalkDescriptor.NextPositionM 

    member this.NextPositionI =
        this.NextPositionM |> vmtovi

    member this.NextPosition =
        this.NextPositionI |> vitovf
    
    static member make multiRoundContext origin direction =
        { WalkDescriptor = WalkDescriptor.make origin direction
          MultiRoundContext = multiRoundContext }

type [<ReferenceEquality; NoComparison>] ActionDescriptor =
    { ActionTargetIndexOpt : CharacterIndex option
      ActionDataName : string }

    static member computeActionDirection currentPosition targetPositionM =
        targetPositionM - vftovm currentPosition |> vmtod

type TurnStatus =
    | TurnPending
    | TurnBeginning
    | TurnTicking of int64
    | TurnFinishing
    | Idle

    static member incTickCount turnStatus =
        match turnStatus with
        | TurnTicking tickCount -> TurnTicking (inc tickCount)
        | _ -> turnStatus

type [<NoComparison>] CharacterActivityState =
    | Action of ActionDescriptor
    | Navigation of NavigationDescriptor
    | NoActivity

    member this.IsActing =
        match this with
        | Action _ -> true
        | Navigation _ | NoActivity -> false

    member this.IsNotActing =
        not this.IsActing

    member this.IsNavigating =
        match this with
        | Action _ | NoActivity -> false
        | Navigation _ -> true

    member this.IsNotNavigating =
        not this.IsNavigating

    member this.IsNavigatingPath =
        match this with
        | Navigation navigationDescriptor -> navigationDescriptor.MultiRoundContext
        | Action _ | NoActivity -> false

    static member makeAttack index =
        Action
            { ActionTargetIndexOpt = Some index
              ActionDataName = Constants.InfinityRpg.AttackName }

    static member makeNavigation multiRoundContext origin direction =
        Navigation (NavigationDescriptor.make multiRoundContext origin direction)