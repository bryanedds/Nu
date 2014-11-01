namespace InfinityRpg
open System
open System.ComponentModel
open Nu

[<AutoOpen>]
module CharacterAnimationFacetModule =

    type CharacterAnimationType =
        | CharacterAnimationFacing
        | CharacterAnimationActing

    type CharacterAnimationData =
        { CharacterAnimationData : Map<CharacterAnimationType, AnimationData> }

    let CharacterAnimationFacingData =
        { FrameCount = 2
          FrameStutter = 6 }

    let CharacterAnimationActingData =
        { FrameCount = 2
          FrameStutter = 6 }

    let CharacterAnimationData =
        { CharacterAnimationData =
            Map.ofList
                [(CharacterAnimationFacing, CharacterAnimationFacingData)
                 (CharacterAnimationActing, CharacterAnimationActingData)] }

    type [<TypeConverter (typeof<AlgebraicConverter<CharacterAnimationState>>)>] CharacterAnimationState =
        { CharacterAnimationType : CharacterAnimationType
          CharacterAnimationDirection : Direction
          CharacterAnimationStartTime : int64 }

    type Entity with

        member entity.CharacterAnimationState = entity?CharacterAnimationState : CharacterAnimationState
        static member setCharacterAnimationState (value : CharacterAnimationState) (entity : Entity) = entity?CharacterAnimationState <- value

    type CharacterAnimationFacet () =
        inherit Facet ()

        static member FieldDefinitions =
            [define? CharacterAnimationState { CharacterAnimationType = CharacterAnimationFacing; CharacterAnimationDirection = South; CharacterAnimationStartTime = 0L }]

[<AutoOpen>]
module InfinityRpgModule =

    type InfinityRpgDispatcher () =
        inherit GameDispatcher ()