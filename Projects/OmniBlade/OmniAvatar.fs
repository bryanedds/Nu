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
              CharacterAnimationState_ : CharacterAnimationState
              CelSize_ : Vector2
              CollidedBodyShapes_ : BodyShapeSource list
              SeparatedBodyShapes_ : BodyShapeSource list
              IntersectedBodyShapes_ : BodyShapeSource list }

        (* Bounds Original Properties *)
        member this.BoundsOriginal = this.BoundsOriginal_
        member this.LowerBoundsOriginal = v4Bounds this.BoundsOriginal_.Position (this.BoundsOriginal_.Size.MapY ((*) 0.5f))
        member this.PositionOriginal = this.BoundsOriginal_.Position
        member this.CenterOriginal = this.BoundsOriginal_.Center
        member this.BottomOriginal = this.BoundsOriginal_.Bottom
        member this.SizeOriginal = this.BoundsOriginal_.Size

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.BottomOffset = this.Bounds_.Bottom + Constants.Field.CharacterBottomOffset
        member this.Size = this.Bounds_.Size
        member this.LowerBounds = v4Bounds (this.Bounds_.Position + v2 (this.Bounds_.Size.X * 0.25f) 0.0f) (this.Bounds_.Size * 0.5f)
        member this.LowerCenter = this.LowerBounds.Center

        (* Animation Properties *)
        member this.TimeStart = this.CharacterAnimationState_.TimeStart
        member this.AnimationSheet = this.CharacterAnimationState_.AnimationSheet
        member this.CharacterAnimationType = this.CharacterAnimationState_.CharacterAnimationType
        member this.Direction = this.CharacterAnimationState_.Direction
        member this.CelSize = this.CelSize_

        (* Local Properties *)
        member this.CollidedBodyShapes = this.CollidedBodyShapes_
        member this.SeparatedBodyShapes = this.SeparatedBodyShapes_
        member this.IntersectedBodyShapes = this.IntersectedBodyShapes_

    let getAnimationInset time (avatar : Avatar) =
        CharacterAnimationState.inset time avatar.CelSize_ avatar.CharacterAnimationState_

    let getAnimationProgressOpt time avatar =
        CharacterAnimationState.progressOpt time avatar.CharacterAnimationState_

    let getAnimationFinished time avatar =
        CharacterAnimationState.getFinished time avatar.CharacterAnimationState_

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

    let updateCharacterAnimationState updater (avatar : Avatar) =
        let characterAnimationState = updater avatar.CharacterAnimationState_
        if characterAnimationState <> avatar.CharacterAnimationState_
        then { avatar with CharacterAnimationState_ = characterAnimationState }
        else avatar

    let updateDirection updater (avatar : Avatar) =
        updateCharacterAnimationState (fun state -> { state with Direction = updater state.Direction }) avatar

    let lookAt bottomOffset (avatar : Avatar) =
        let delta = bottomOffset - avatar.BottomOffset
        let direction = Direction.ofVector2 delta
        updateDirection (constant direction) avatar

    let animate time characterAnimationType avatar =
        updateCharacterAnimationState (fun state -> CharacterAnimationState.setCharacterAnimationType (Some time) characterAnimationType state) avatar

    let toSymbolizable avatar =
        { avatar with
            CollidedBodyShapes_ = []
            SeparatedBodyShapes_ = []
            IntersectedBodyShapes_ = [] }

    let make bounds animationSheet direction =
        let characterAnimationState = { TimeStart = 0L; AnimationSheet = animationSheet; CharacterAnimationType = IdleAnimation; Direction = direction }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterAnimationState_ = characterAnimationState
          CelSize_ = Constants.Gameplay.CharacterCelSize
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let empty =
        let bounds = v4Bounds v2Zero Constants.Gameplay.CharacterSize
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          CharacterAnimationState_ = CharacterAnimationState.empty
          CelSize_ = Constants.Gameplay.CharacterCelSize
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let initial =
        let position = v2 2064.0f 48.0f - Constants.Gameplay.CharacterSize.WithY 0.0f * 0.5f
        let bounds = v4Bounds position Constants.Gameplay.CharacterSize
        let characterAnimationState = CharacterAnimationState.initial
        { empty with
            BoundsOriginal_ = bounds
            Bounds_ = bounds
            CharacterAnimationState_ = characterAnimationState }

type Avatar = Avatar.Avatar