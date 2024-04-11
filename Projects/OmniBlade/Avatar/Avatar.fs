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
        private
            { Perimeter_ : Box3
              CharacterAnimationState_ : CharacterAnimationState
              CelSize_ : Vector2 }

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.BottomOffset = this.Perimeter_.Bottom + Constants.Field.CharacterBottomOffset
        member this.Size = this.Perimeter_.Size
        member this.LowerPerimeter = box3 (this.Perimeter_.Min + v3 (this.Perimeter_.Size.X * 0.25f) 0.0f 0.0f) (this.Perimeter_.Size * 0.5f)
        member this.LowerCenter = this.LowerPerimeter.Center

        (* Animation Properties *)
        member this.StartTime = this.CharacterAnimationState_.StartTime
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction
        member this.CelSize = this.CelSize_

    let getAnimationInset time (avatar : Avatar) =
        CharacterAnimationState.inset time avatar.CelSize_ avatar.CharacterAnimationState_

    let getAnimationProgressOpt time avatar =
        CharacterAnimationState.progressOpt time avatar.CharacterAnimationState_

    let getAnimationFinished time avatar =
        CharacterAnimationState.getFinished time avatar.CharacterAnimationState_

    let mapPerimeter updater (avatar : Avatar) =
        let bounds = updater avatar.Perimeter_
        if bounds =/= avatar.Perimeter
        then { avatar with Perimeter_ = bounds }
        else avatar

    let mapCenter updater (avatar : Avatar) =
        mapPerimeter (fun bounds -> bounds.Center |> updater |> bounds.WithCenter) avatar

    let mapBottom updater (avatar : Avatar) =
        mapPerimeter (fun bounds -> bounds.Bottom |> updater |> bounds.WithBottom) avatar

    let mapCharacterAnimationState updater (avatar : Avatar) =
        let characterAnimationState = updater avatar.CharacterAnimationState_
        if characterAnimationState =/= avatar.CharacterAnimationState_
        then { avatar with CharacterAnimationState_ = characterAnimationState }
        else avatar

    let mapDirection updater (avatar : Avatar) =
        mapCharacterAnimationState (fun state -> { state with Direction = updater state.Direction }) avatar

    let lookAt bottomOffset (avatar : Avatar) =
        let delta = bottomOffset - avatar.BottomOffset
        let direction = Direction.ofVector3 delta
        mapDirection (constant direction) avatar

    let animate time characterAnimationType avatar =
        mapCharacterAnimationState (fun state -> CharacterAnimationState.setCharacterAnimationType time characterAnimationType state) avatar

    let make bounds animationSheet direction =
        let characterAnimationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = IdleAnimation; MaterializationOpt = None; Direction = direction }
        { Perimeter_ = bounds
          CharacterAnimationState_ = characterAnimationState
          CelSize_ = Constants.Gameplay.CharacterCelSize }

    let empty () =
        let bounds = box3 v3Zero Constants.Gameplay.CharacterSize
        { Perimeter_ = bounds
          CharacterAnimationState_ = CharacterAnimationState.empty
          CelSize_ = Constants.Gameplay.CharacterCelSize }

    let initial () =
        let position = v3 2064.0f 48.0f 0.0f - Constants.Gameplay.CharacterSize.WithY 0.0f * 0.5f
        let bounds = box3 position Constants.Gameplay.CharacterSize
        let characterAnimationState = CharacterAnimationState.initial
        { empty () with
            Perimeter_ = bounds
            CharacterAnimationState_ = characterAnimationState }

type Avatar = Avatar.Avatar