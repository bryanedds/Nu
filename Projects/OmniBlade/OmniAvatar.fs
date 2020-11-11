// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Avatar =

    type [<ReferenceEquality; NoComparison>] Avatar =
        private
            { BoundsOriginal_ : Vector4
              Bounds_ : Vector4
              AnimationState_ : CharacterAnimationState
              CollidedBodyShapes_ : BodyShapeSource list
              SeparatedBodyShapes_ : BodyShapeSource list
              IntersectedBodyShapes_ : BodyShapeSource list }

        (* Bounds Original Properties *)
        member this.BoundsOriginal = this.BoundsOriginal_
        member this.PositionOriginal = this.BoundsOriginal_.Position
        member this.CenterOriginal = this.BoundsOriginal_.Center
        member this.BottomOriginal = this.BoundsOriginal_.Bottom
        member this.SizeOriginal = this.BoundsOriginal_.Size

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* AnimationState Properties *)
        member this.TimeStart = this.AnimationState_.TimeStart
        member this.AnimationSheet = this.AnimationState_.AnimationSheet
        member this.AnimationCycle = this.AnimationState_.AnimationCycle
        member this.Direction = this.AnimationState_.Direction

        (* Local Properties *)
        member this.CollidedBodyShapes = this.CollidedBodyShapes_
        member this.SeparatedBodyShapes = this.SeparatedBodyShapes_
        member this.IntersectedBodyShapes = this.IntersectedBodyShapes_

    let getAnimationIndex time avatar =
        CharacterAnimationState.index time avatar.AnimationState_

    let getAnimationProgressOpt time avatar =
        CharacterAnimationState.progressOpt time avatar.AnimationState_

    let getAnimationFinished time avatar =
        CharacterAnimationState.getFinished time avatar.AnimationState_

    let updateCollidedBodyShapes updater (avatar : Avatar) =
        let bodyShapes = updater avatar.CollidedBodyShapes_
        if bodyShapes <> avatar.CollidedBodyShapes_
        then { avatar with CollidedBodyShapes_ = bodyShapes }
        else avatar

    let updateSeparatedBodyShapes updater (avatar : Avatar) =
        let bodyShapes = updater avatar.SeparatedBodyShapes_
        if bodyShapes <> avatar.SeparatedBodyShapes_
        then { avatar with SeparatedBodyShapes_ = bodyShapes }
        else avatar

    let updateIntersectedBodyShapes updater (avatar : Avatar) =
        let bodyShapes = updater avatar.IntersectedBodyShapes_
        if bodyShapes <> avatar.IntersectedBodyShapes_
        then { avatar with IntersectedBodyShapes_ = bodyShapes }
        else avatar

    let updateBounds updater (avatar : Avatar) =
        let bounds = updater avatar.Bounds_
        if bounds <> avatar.Bounds
        then { avatar with Bounds_ = bounds }
        else avatar

    let updatePosition updater (avatar : Avatar) =
        updateBounds (fun bounds -> bounds.Position |> updater |> bounds.WithPosition) avatar

    let updateCenter updater (avatar : Avatar) =
        updateBounds (fun bounds -> bounds.Center |> updater |> bounds.WithCenter) avatar

    let updateBottom updater (avatar : Avatar) =
        updateBounds (fun bounds -> bounds.Bottom |> updater |> bounds.WithBottom) avatar

    let updateAnimationState updater (avatar : Avatar) =
        let animationState = updater avatar.AnimationState_
        if animationState <> avatar.AnimationState_
        then { avatar with AnimationState_ = animationState }
        else avatar

    let updateDirection updater (avatar : Avatar) =
        updateAnimationState (fun state -> { state with Direction = updater state.Direction }) avatar

    let animate time cycle avatar =
        updateAnimationState (fun state -> CharacterAnimationState.setCycle (Some time) cycle state) avatar

    let toSymbolizable avatar =
        { avatar with
            CollidedBodyShapes_ = []
            SeparatedBodyShapes_ = []
            IntersectedBodyShapes_ = [] }

    let make bounds animationSheet direction =
        let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = IdleCycle; Direction = direction }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = animationState
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let empty =
        let bounds = v4Bounds v2Zero Constants.Gameplay.CharacterSize
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = CharacterAnimationState.empty
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let initial =
        let position = v2 2752.0f 64.0f - Constants.Gameplay.CharacterSize.WithY 0.0f * 0.5f
        let bounds = v4Bounds position Constants.Gameplay.CharacterSize
        let animationState = CharacterAnimationState.initial
        { empty with
            BoundsOriginal_ = bounds
            Bounds_ = bounds
            AnimationState_ = animationState }

type Avatar = Avatar.Avatar