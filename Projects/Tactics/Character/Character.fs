namespace Tactics
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Character =

    type [<ReferenceEquality; SymbolicExpansion>] Character =
        private
            { CharacterIndex_ : CharacterIndex
              CharacterType_ : CharacterType
              CharacterState_ : PoiseType
              CharacterAnimationState_ : CharacterAnimationState
              CelSize_ : Vector2 }

        (* CharacterState Properties *)
        member this.Name = CharacterType.getName this.CharacterType_
        member this.CharacterIndex = this.CharacterIndex_
        member this.CharacterType = this.CharacterType_
        member this.PartyIndex = match this.CharacterIndex with AllyIndex index | EnemyIndex index -> index
        member this.Ally = match this.CharacterIndex with AllyIndex _ -> true | EnemyIndex _ -> false
        member this.Enemy = not this.Ally
        member this.CelSize = this.CelSize_

        (* Animation Properties *)
        member this.TimeStart = this.CharacterAnimationState_.StartTime
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction

    let friendly (character : Character) (character2 : Character) =
        CharacterIndex.friendly character.CharacterIndex character2.CharacterIndex

    let getPoiseType character =
        character.CharacterState_

    let getAnimationInset time (character : Character) =
        CharacterAnimationState.inset time character.CelSize_ character.CharacterAnimationState_

    let getAnimationIndex time character =
        CharacterAnimationState.index time character.CharacterAnimationState_

    let getAnimationProgressOpt time character =
        CharacterAnimationState.progressOpt time character.CharacterAnimationState_

    let getAnimationFinished time character =
        CharacterAnimationState.getFinished time character.CharacterAnimationState_

    let face direction character =
        { character with CharacterAnimationState_ = CharacterAnimationState.face direction character.CharacterAnimationState_ }

    let animate time characterAnimationType character =
        { character with CharacterAnimationState_ = CharacterAnimationState.setCharacterAnimationType (Some time) characterAnimationType character.CharacterAnimationState_ }

    let make characterIndex characterType (characterState : PoiseType) animationSheet celSize direction =
        let animationType = IdleAnimation
        let animationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = animationType; Direction = direction }
        { CharacterIndex_ = characterIndex
          CharacterType_ = characterType
          CharacterState_ = characterState
          CharacterAnimationState_ = animationState
          CelSize_ = celSize }

    let empty =
        let characterAnimationState = { StartTime = 0L; AnimationSheet = asset "Field " "Jinn"; CharacterAnimationType = PoiseAnimation Poising; Direction = Downward }
        { CharacterIndex_ = AllyIndex 0
          CharacterType_ = Ally Jinn
          CharacterState_ = Poising
          CharacterAnimationState_ = characterAnimationState
          CelSize_ = v2 48.0f 48.0f }

type Character = Character.Character