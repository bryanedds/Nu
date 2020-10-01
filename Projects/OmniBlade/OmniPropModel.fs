namespace OmniBlade
open Prime
open Nu

[<RequireQualifiedAccess>]
module PropModel =

    type [<StructuralEquality; NoComparison>] PropModel =
        private
            { Bounds_ : Vector4
              Depth_ : single
              Advents_ : Advent Set
              PropData_ : PropData
              PropState_ : PropState
              PropId_ : int }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.Depth = this.Depth_
        member this.Advents = this.Advents_
        member this.PropData = this.PropData_
        member this.PropState = this.PropState_
        member this.PropId = this.PropId_

    let updateBounds updater (model : PropModel) =
        { model with Bounds_ = updater model.Bounds_ }

    let updatePosition updater (model : PropModel) =
        { model with Bounds_ = model.Position |> updater |> model.Bounds.WithPosition }

    let updateCenter updater (model : PropModel) =
        { model with Bounds_ = model.Center |> updater |> model.Bounds.WithCenter }

    let updateBottom updater (model : PropModel) =
        { model with Bounds_ = model.Bottom |> updater |> model.Bounds.WithBottom }

    let updatePropState updater (model : PropModel) =
        { model with PropState_ = updater model.PropState_ }

    let make bounds depth advents propData propState propId =
        { Bounds_ = bounds
          Depth_ = depth
          Advents_ = advents
          PropData_ = propData
          PropState_ = propState
          PropId_ = propId }

    let empty =
        { Bounds_ = v4Bounds v2Zero Constants.Gameplay.TileSize
          Depth_ = 0.0f
          Advents_ = Set.empty
          PropData_ = PropData.empty
          PropState_ = NilState
          PropId_ = 0 }

type PropModel = PropModel.PropModel