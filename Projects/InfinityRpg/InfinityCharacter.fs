namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] Character =
    { Index : CharacterIndex
      CharacterAnimationState : CharacterAnimationState
      CharacterAnimationSheet : Image AssetTag
      Position : Vector2 }

    static member initial =
        { Index = PlayerIndex
          CharacterAnimationState = CharacterAnimationState.initial
          CharacterAnimationSheet = Assets.PlayerImage
          Position = v2Zero }

    static member updateCharacterAnimationState updater (character : Character) =
        { character with CharacterAnimationState = updater character.CharacterAnimationState }

    static member updatePosition updater (character : Character) =
        { character with Position = updater character.Position }