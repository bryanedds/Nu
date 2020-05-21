namespace OmniBlade
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module AvatarModel =

    type [<CustomEquality; NoComparison>] AvatarModel =
        private
            { Dirty_ : Guid
              BoundsOriginal_ : Vector4
              Bounds_ : Vector4
              AnimationState_ : CharacterAnimationState
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
        member this.IntersectedBodyShapes = this.IntersectedBodyShapes_

        (* Equals *)
        override this.GetHashCode () = hash this.Dirty_
        override this.Equals thatObj = match thatObj with :? AvatarModel as that -> this.Dirty_ = that.Dirty_ | _ -> false

    let getAnimationIndex time avatar =
        CharacterAnimationState.index time avatar.AnimationState_

    let getAnimationProgressOpt time avatar =
        CharacterAnimationState.progressOpt time avatar.AnimationState_

    let getAnimationFinished time avatar =
        CharacterAnimationState.getFinished time avatar.AnimationState_

    let updateIntersectedBodyShapes updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; IntersectedBodyShapes_ = updater avatar.IntersectedBodyShapes_ }

    let updateBounds updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; Bounds_ = updater avatar.Bounds_ }

    let updatePosition updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; Bounds_ = avatar.Position |> updater |> avatar.Bounds.WithPosition }

    let updateCenter updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; Bounds_ = avatar.Center |> updater |> avatar.Bounds.WithCenter }

    let updateBottom updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; Bounds_ = avatar.Bottom |> updater |> avatar.Bounds.WithBottom }

    let updateDirection updater (avatar : AvatarModel) =
        { avatar with Dirty_ = Gen.id; AnimationState_ = { avatar.AnimationState_ with Direction = updater avatar.AnimationState_.Direction }}

    let animate time cycle avatar =
        { avatar with Dirty_ = Gen.id; AnimationState_ = CharacterAnimationState.setCycle (Some time) cycle avatar.AnimationState_ }

    let make bounds animationSheet direction =
        let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = IdleCycle; Direction = direction }
        { Dirty_ = Gen.id
          BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = animationState
          IntersectedBodyShapes_ = [] }

    let empty =
        let bounds = v4Bounds (v2Dup 128.0f) Constants.Gameplay.CharacterSize
        { Dirty_ = Gen.id
          BoundsOriginal_ = bounds
          Bounds_ = bounds
          AnimationState_ = CharacterAnimationState.empty
          IntersectedBodyShapes_ = [] }

type AvatarModel = AvatarModel.AvatarModel