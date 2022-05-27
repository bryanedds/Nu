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
            { PerimeterOriginal_ : Box3
              Perimeter_ : Box3
              CharacterAnimationState_ : CharacterAnimationState
              CelSize_ : Vector2
              CollidedBodyShapes_ : BodyShapeSource list
              SeparatedBodyShapes_ : BodyShapeSource list
              IntersectedBodyShapes_ : BodyShapeSource list }

        (* Perimeter Original Properties *)
        member this.PerimeterOriginal = this.PerimeterOriginal_
        member this.LowerPerimeterOriginal = box3 this.PerimeterOriginal_.Position (this.PerimeterOriginal_.Size.MapY ((*) 0.5f))
        member this.PositionOriginal = this.PerimeterOriginal_.Position
        member this.CenterOriginal = this.PerimeterOriginal_.Center
        member this.BottomOriginal = this.PerimeterOriginal_.Bottom
        member this.SizeOriginal = this.PerimeterOriginal_.Size

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Position = this.Perimeter_.Position
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.BottomOffset = this.Perimeter_.Bottom + Constants.Field.CharacterBottomOffset
        member this.Size = this.Perimeter_.Size
        member this.LowerPerimeter = box3 (this.Perimeter_.Position + v3 (this.Perimeter_.Size.X * 0.25f) 0.0f 0.0f) (this.Perimeter_.Size * 0.5f)
        member this.LowerCenter = this.LowerPerimeter.Center

        (* Animation Properties *)
        member this.StartTime = this.CharacterAnimationState_.StartTime
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

    let updatePerimeter updater (avatar : Avatar) =
        let bounds = updater avatar.Perimeter_
        if bounds <> avatar.Perimeter
        then { avatar with Perimeter_ = bounds }
        else avatar

    let updatePosition updater (avatar : Avatar) =
        updatePerimeter (fun bounds -> bounds.Position |> updater |> bounds.WithPosition) avatar

    let updateCenter updater (avatar : Avatar) =
        updatePerimeter (fun bounds -> bounds.Center |> updater |> bounds.WithCenter) avatar

    let updateBottom updater (avatar : Avatar) =
        updatePerimeter (fun bounds -> bounds.Bottom |> updater |> bounds.WithBottom) avatar

    let updateCharacterAnimationState updater (avatar : Avatar) =
        let characterAnimationState = updater avatar.CharacterAnimationState_
        if characterAnimationState <> avatar.CharacterAnimationState_
        then { avatar with CharacterAnimationState_ = characterAnimationState }
        else avatar

    let updateDirection updater (avatar : Avatar) =
        updateCharacterAnimationState (fun state -> { state with Direction = updater state.Direction }) avatar

    let lookAt bottomOffset (avatar : Avatar) =
        let delta = bottomOffset - avatar.BottomOffset
        let direction = Direction.ofVector3 delta
        updateDirection (constant direction) avatar

    let animate time characterAnimationType avatar =
        updateCharacterAnimationState (fun state -> CharacterAnimationState.setCharacterAnimationType (Some time) characterAnimationType state) avatar

    let toSymbolizable avatar =
        { avatar with
            CollidedBodyShapes_ = []
            SeparatedBodyShapes_ = []
            IntersectedBodyShapes_ = [] }

    let make bounds animationSheet direction =
        let characterAnimationState = { StartTime = 0L; AnimationSheet = animationSheet; CharacterAnimationType = IdleAnimation; Direction = direction }
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterAnimationState_ = characterAnimationState
          CelSize_ = Constants.Gameplay.CharacterCelSize
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let empty =
        let bounds = box3 v3Zero Constants.Gameplay.CharacterSize
        { PerimeterOriginal_ = bounds
          Perimeter_ = bounds
          CharacterAnimationState_ = CharacterAnimationState.empty
          CelSize_ = Constants.Gameplay.CharacterCelSize
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let initial =
        let position = v3 2064.0f 48.0f 0.0f - Constants.Gameplay.CharacterSize.WithY 0.0f * 0.5f
        let bounds = box3 position Constants.Gameplay.CharacterSize
        let characterAnimationState = CharacterAnimationState.initial
        { empty with
            PerimeterOriginal_ = bounds
            Perimeter_ = bounds
            CharacterAnimationState_ = characterAnimationState }

type Avatar = Avatar.Avatar