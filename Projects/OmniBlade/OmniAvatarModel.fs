namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module AvatarModel =

    type [<ReferenceEquality; NoComparison>] AvatarModel =
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

    let updateCollidedBodyShapes updater (avatar : AvatarModel) =
        { avatar with CollidedBodyShapes_ = updater avatar.CollidedBodyShapes_ }

    let updateSeparatedBodyShapes updater (avatar : AvatarModel) =
        { avatar with SeparatedBodyShapes_ = updater avatar.SeparatedBodyShapes_ }

    let updateIntersectedBodyShapes updater (avatar : AvatarModel) =
        { avatar with IntersectedBodyShapes_ = updater avatar.IntersectedBodyShapes_ }

    let updateBounds updater (avatar : AvatarModel) =
        { avatar with Bounds_ = updater avatar.Bounds_ }

    let updatePosition updater (avatar : AvatarModel) =
        { avatar with Bounds_ = avatar.Position |> updater |> avatar.Bounds.WithPosition }

    let updateCenter updater (avatar : AvatarModel) =
        { avatar with Bounds_ = avatar.Center |> updater |> avatar.Bounds.WithCenter }

    let updateBottom updater (avatar : AvatarModel) =
        { avatar with Bounds_ = avatar.Bottom |> updater |> avatar.Bounds.WithBottom }

    let updateDirection updater (avatar : AvatarModel) =
        { avatar with AnimationState_ = { avatar.AnimationState_ with Direction = updater avatar.AnimationState_.Direction }}

    let animate time cycle avatar =
        { avatar with AnimationState_ = CharacterAnimationState.setCycle (Some time) cycle avatar.AnimationState_ }

    let make bounds animationSheet direction =
        let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = IdleCycle; Direction = direction }
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = animationState
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

    let empty =
        let bounds = v4Bounds (v2Dup 128.0f) Constants.Gameplay.CharacterSize
        { BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = CharacterAnimationState.empty
          CollidedBodyShapes_ = []
          SeparatedBodyShapes_ = []
          IntersectedBodyShapes_ = [] }

type AvatarModel = AvatarModel.AvatarModel