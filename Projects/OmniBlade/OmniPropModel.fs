namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module PropModel =

    type [<ReferenceEquality; NoComparison>] PropModel =
        private
            { Bounds_ : Vector4
              Depth_ : single
              Advents_ : Advent Set
              Props_ : PropData }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.Depth = this.Depth_
        member this.Advents = this.Advents_
        member this.Props = this.Props_

        static member updateBounds updater (model : PropModel) =
            { model with Bounds_ = updater model.Bounds_ }

        static member updatePosition updater (model : PropModel) =
            { model with Bounds_ = model.Position |> updater |> model.Bounds.WithPosition }

        static member updateCenter updater (model : PropModel) =
            { model with Bounds_ = model.Center |> updater |> model.Bounds.WithCenter }

        static member updateBottom updater (model : PropModel) =
            { model with Bounds_ = model.Bottom |> updater |> model.Bounds.WithBottom }

        static member make bounds depth advents props =
            { Bounds_ = bounds
              Depth_ = depth
              Advents_ = advents
              Props_ = props }

type PropModel = PropModel.PropModel