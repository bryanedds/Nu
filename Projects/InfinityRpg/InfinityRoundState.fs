namespace InfinityRpg
open System
open Prime
open Nu
open InfinityRpg

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

type [<StructuralEquality; NoComparison>] RoundProgress =
    | CharactersActing
    | EnemiesAttacking
    | EnemiesWalking
    | RoundFinishing
    | RoundFinished

/// TODO: turn this into an abstract data type?
type [<ReferenceEquality; NoComparison>] RoundState =
    { CharacterActions : Map<CharacterIndex, Action>
      WalkingEnemyGroup : CharacterIndex list
      AttackingEnemyGroup : CharacterIndex list
      PlayerNavigation : Navigation }

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
    
    static member updatePlayerNavigation updater roundState =
        { roundState with PlayerNavigation = updater roundState.PlayerNavigation }
    
    static member updateCharacterActions updater roundState =
        { roundState with CharacterActions = updater roundState.CharacterActions }
    
    static member updateWalkingEnemyGroup updater roundState =
        { roundState with WalkingEnemyGroup = updater roundState.WalkingEnemyGroup }
    
    static member updateAttackingEnemyGroup updater roundState =
        { roundState with AttackingEnemyGroup = updater roundState.AttackingEnemyGroup }
    
    static member getRoundProgress (roundState : RoundState) =
        if Map.notEmpty roundState.CharacterActions then CharactersActing
        elif List.notEmpty roundState.AttackingEnemyGroup then EnemiesAttacking
        elif List.notEmpty roundState.WalkingEnemyGroup then EnemiesWalking
        elif roundState.IsPlayerContinuing then RoundFinishing
        else RoundFinished
    
    static member inProgress roundState =
        RoundState.getRoundProgress roundState <> RoundFinished
    
    static member tryGetPlayerAction (roundState : RoundState) =
        Map.tryFind PlayerIndex roundState.CharacterActions
    
    static member addCharacterAction index action roundState =
        RoundState.updateCharacterActions (Map.add index action) roundState

    static member removeCharacterAction index roundState =
        RoundState.updateCharacterActions (Map.remove index) roundState

    static member addWalkingEnemyGroup group roundState =
        RoundState.updateWalkingEnemyGroup (constant group) roundState

    static member removeWalkingEnemyGroup roundState =
        RoundState.updateWalkingEnemyGroup (constant []) roundState

    static member addAttackingEnemyGroup group roundState =
        RoundState.updateAttackingEnemyGroup (constant group) roundState

    static member removeHeadFromAttackingEnemyGroup roundState =
        RoundState.updateAttackingEnemyGroup List.tail roundState

    static member empty =
        { CharacterActions = Map.empty
          WalkingEnemyGroup = []
          AttackingEnemyGroup = []
          PlayerNavigation = Idling }