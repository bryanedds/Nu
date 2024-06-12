// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Avatar =

    type [<ReferenceEquality; SymbolicExpansion>] Avatar =
        { Perimeter : Box3
          CharacterAnimationState : CharacterAnimationState
          CelSize : Vector2 }

        (* Animation Properties *)
        member this.StartTime = this.CharacterAnimationState.StartTime
        member this.AnimationSheet = this.CharacterAnimationState.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState.Direction

    let getAnimationInset time (avatar : Avatar) =
        CharacterAnimationState.inset time avatar.CelSize avatar.CharacterAnimationState

    let getAnimationProgressOpt time avatar =
        CharacterAnimationState.progressOpt time avatar.CharacterAnimationState

    let getAnimationFinished time avatar =
        CharacterAnimationState.getFinished time avatar.CharacterAnimationState

    let mapCharacterAnimationState updater (avatar : Avatar) =
        let characterAnimationState = updater avatar.CharacterAnimationState
        if characterAnimationState =/= avatar.CharacterAnimationState
        then { avatar with CharacterAnimationState = characterAnimationState }
        else avatar

    let setDirection direction (avatar : Avatar) =
        mapCharacterAnimationState (fun state -> { state with Direction = direction }) avatar

    let lookAt bottomOffset5 (avatar : Avatar) =
        let delta = bottomOffset5 - avatar.Perimeter.BottomOffset5
        let direction = Direction.ofVector3 delta
        setDirection direction avatar

    let animate time characterAnimationType avatar =
        mapCharacterAnimationState (fun state -> CharacterAnimationState.setCharacterAnimationType time characterAnimationType state) avatar

    let make bounds animationSheet direction =
        let characterAnimationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = IdleAnimation; MaterializationOpt = None; Direction = direction }
        { Perimeter = bounds
          CharacterAnimationState = characterAnimationState
          CelSize = Constants.Gameplay.CharacterCelSize }

    let empty () =
        let bounds = box3 v3Zero Constants.Gameplay.CharacterSize
        { Perimeter = bounds
          CharacterAnimationState = CharacterAnimationState.empty
          CelSize = Constants.Gameplay.CharacterCelSize }

    let initial () =
        let position = v3 2064.0f 48.0f 0.0f - Constants.Gameplay.CharacterSize.WithY 0.0f * 0.5f
        let bounds = box3 position Constants.Gameplay.CharacterSize
        let characterAnimationState = CharacterAnimationState.initial
        { empty () with
            Perimeter = bounds
            CharacterAnimationState = characterAnimationState }

type Avatar = Avatar.Avatar