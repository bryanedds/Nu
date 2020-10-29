namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] Character =
    { Index : CharacterIndex
      TurnStatus : TurnStatus
      CharacterActivityState : CharacterActivityState
      CharacterAnimationState : CharacterAnimationState
      CharacterAnimationSheet : Image AssetTag
      Position : Vector2 }

    static member initial =
        { Index = PlayerIndex
          TurnStatus = Idle
          CharacterActivityState = NoActivity
          CharacterAnimationState = CharacterAnimationState.initial
          CharacterAnimationSheet = Assets.PlayerImage
          Position = v2Zero }

    static member updateTurnStatus updater (character : Character) =
        { character with TurnStatus = updater character.TurnStatus }
    
    static member updateCharacterActivityState updater (character : Character) =
        { character with CharacterActivityState = updater character.CharacterActivityState }

    static member updateCharacterAnimationState updater (character : Character) =
        { character with CharacterAnimationState = updater character.CharacterAnimationState }

    static member updatePosition updater (character : Character) =
        { character with Position = updater character.Position }
    
    static member makePlayer positionM =
        { Character.initial with
            Position = vmtovf positionM }

    static member makeEnemy index positionM =
        { Character.initial with
            Index = index
            CharacterAnimationSheet = Assets.GoopyImage
            Position = vmtovf positionM }