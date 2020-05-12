namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module PropModel =

    type [<ReferenceEquality; NoComparison>] PropModel =
        private
            { PropData_ : PropData
              Bounds_ : Vector4
              Depth_ : single }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.PropData = this.PropData_
        member this.Depth = this.Depth_

        static member updateBounds updater (model : PropModel) =
            { model with Bounds_ = updater model.Bounds_ }

        static member updatePosition updater (model : PropModel) =
            { model with Bounds_ = model.Position |> updater |> model.Bounds.WithPosition }

        static member updateCenter updater (model : PropModel) =
            { model with Bounds_ = model.Center |> updater |> model.Bounds.WithCenter }

        static member updateBottom updater (model : PropModel) =
            { model with Bounds_ = model.Bottom |> updater |> model.Bounds.WithBottom }

        static member make propData bounds depth =
            { PropData_ = propData
              Bounds_ = bounds
              Depth_ = depth }

type PropModel = PropModel.PropModel