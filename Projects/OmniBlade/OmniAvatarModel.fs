namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module AvatarModel =

    type [<ReferenceEquality; NoComparison>] AvatarModel =
        private
            { AnimationState : CharacterAnimationState
              IntersectedBodyShapes_ : BodyShapeSource list
              BoundsOriginal_ : Vector4
              Bounds_ : Vector4 }

        (* AnimationState Properties *)
        member this.TimeStart = this.AnimationState.TimeStart
        member this.AnimationSheet = this.AnimationState.AnimationSheet
        member this.AnimationCycle = this.AnimationState.AnimationCycle
        member this.Direction = this.AnimationState.Direction

        (* Local Properties *)
        member this.IntersectedBodyShapes = this.IntersectedBodyShapes_

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

        static member getAnimationIndex time avatar =
            CharacterAnimationState.index time avatar.AnimationState

        static member getAnimationProgressOpt time avatar =
            CharacterAnimationState.progressOpt time avatar.AnimationState

        static member getAnimationFinished time avatar =
            CharacterAnimationState.getFinished time avatar.AnimationState

        static member updateIntersectedBodyShapes updater (avatar : AvatarModel) =
            { avatar with IntersectedBodyShapes_ = updater avatar.IntersectedBodyShapes_ }

        static member updateBounds updater (avatar : AvatarModel) =
            { avatar with Bounds_ = updater avatar.Bounds_ }

        static member updatePosition updater (avatar : AvatarModel) =
            { avatar with Bounds_ = avatar.Position |> updater |> avatar.Bounds.WithPosition }

        static member updateCenter updater (avatar : AvatarModel) =
            { avatar with Bounds_ = avatar.Center |> updater |> avatar.Bounds.WithCenter }

        static member updateBottom updater (avatar : AvatarModel) =
            { avatar with Bounds_ = avatar.Bottom |> updater |> avatar.Bounds.WithBottom }

        static member updateDirection updater (avatar : AvatarModel) =
            { avatar with AnimationState = { avatar.AnimationState with Direction = updater avatar.AnimationState.Direction }}

        static member animate time cycle avatar =
            { avatar with AnimationState = CharacterAnimationState.setCycle (Some time) cycle avatar.AnimationState }

        static member make animationSheet direction bounds =
            let animationState = { TimeStart = 0L; AnimationSheet = animationSheet; AnimationCycle = IdleCycle; Direction = direction }
            { AnimationState = animationState
              IntersectedBodyShapes_ = []
              BoundsOriginal_ = bounds
              Bounds_ = bounds }

type AvatarModel = AvatarModel.AvatarModel