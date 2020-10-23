namespace InfinityRpg
open System
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] Character =
    { Index : CharacterIndex
      CharacterState : CharacterState
      TurnStatus : TurnStatus
      CharacterActivityState : CharacterActivityState
      CharacterAnimationState : CharacterAnimationState
      CharacterAnimationSheet : Image AssetTag
      Position : Vector2 }

    static member initial =
        { Index = PlayerIndex
          CharacterState = CharacterState.empty
          TurnStatus = Idle
          CharacterActivityState = NoActivity
          CharacterAnimationState = CharacterAnimationState.initial
          CharacterAnimationSheet = Assets.PlayerImage
          Position = v2Zero }

    static member updateCharacterState updater (character : Character) =
        { character with CharacterState = updater character.CharacterState }
    
    static member updateTurnStatus updater (character : Character) =
        { character with TurnStatus = updater character.TurnStatus }
    
    static member updateCharacterActivityState updater (character : Character) =
        { character with CharacterActivityState = updater character.CharacterActivityState }

    static member updateCharacterAnimationState updater (character : Character) =
        { character with CharacterAnimationState = updater character.CharacterAnimationState }

    static member updatePosition updater (character : Character) =
        { character with Position = updater character.Position }
    
    static member makePlayer positionM =
        let characterState = { CharacterState.empty with HitPoints = 30; ControlType = PlayerControlled }
        { Character.initial with
            CharacterState = characterState
            Position = vmtovf positionM }

    static member makeEnemy index positionM =
        let characterState = { CharacterState.empty with HitPoints = 10; ControlType = Chaos }
        { Character.initial with
            Index = index
            CharacterState = characterState
            CharacterAnimationSheet = Assets.GoopyImage
            Position = vmtovf positionM }