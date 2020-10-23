namespace InfinityRpg
open System
open System.Numerics
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

    static member updateCharacterState newValue (character : Character) =
        { character with CharacterState = newValue }
    
    static member updateTurnStatus newValue (character : Character) =
        { character with TurnStatus = newValue }
    
    static member updateCharacterActivityState newValue (character : Character) =
        { character with CharacterActivityState = newValue }

    static member updateCharacterAnimationState newValue (character : Character) =
        { character with CharacterAnimationState = newValue }

    static member updatePosition newValue (character : Character) =
        { character with Position = newValue }
    
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