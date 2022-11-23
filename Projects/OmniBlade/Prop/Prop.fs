// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type [<NoComparison>] PropState =
    | DoorState of bool
    | SwitchState of bool
    | CharacterState of Color * CharacterAnimationState
    | SpriteState of Image AssetTag * Color * Blend * Color * Flip * bool
    | NilState

[<RequireQualifiedAccess>]
module Prop =

    [<Syntax
        ("", "", "", "", "",
         Constants.PrettyPrinter.DefaultThresholdMin,
         Constants.PrettyPrinter.DetailedThresholdMax)>]
    type [<NoComparison>] Prop =
        private
            { Perimeter_ : Box3
              Elevation_ : single
              Advents_ : Advent Set
              PointOfInterest_ : Vector3
              PropData_ : PropData
              PropState_ : PropState
              PropId_ : int }

        (* Perimeter Properties *)
        member this.Perimeter = this.Perimeter_
        member this.Position = this.Perimeter_.Position
        member this.Center = this.Perimeter_.Center
        member this.Bottom = this.Perimeter_.Bottom
        member this.BottomInset = this.Perimeter_.Bottom + Constants.Field.CharacterBottomOffset
        member this.Size = this.Perimeter_.Size

        (* Local Properties *)
        member this.Elevation = this.Elevation_
        member this.Advents = this.Advents_
        member this.PointOfInterest = this.PointOfInterest_
        member this.PropData = this.PropData_
        member this.PropState = this.PropState_
        member this.PropId = this.PropId_

    let updatePerimeter updater (prop : Prop) =
        { prop with Perimeter_ = updater prop.Perimeter_ }

    let updatePosition updater (prop : Prop) =
        { prop with Perimeter_ = prop.Position |> updater |> prop.Perimeter.WithPosition }

    let updateCenter updater (prop : Prop) =
        { prop with Perimeter_ = prop.Center |> updater |> prop.Perimeter.WithCenter }

    let updateBottom updater (prop : Prop) =
        { prop with Perimeter_ = prop.Bottom |> updater |> prop.Perimeter.WithBottom }

    let updateAdvents updater (prop : Prop) =
        { prop with Advents_ = updater prop.Advents_ }

    let updatePointOfInterest updater (prop : Prop) =
        { prop with PointOfInterest_ = updater prop.PointOfInterest_ }

    let updatePropState updater (prop : Prop) =
        { prop with PropState_ = updater prop.PropState_ }

    let make bounds elevation advents pointOfInterest propData propState propId =
        { Perimeter_ = bounds
          Elevation_ = elevation
          Advents_ = advents
          PointOfInterest_ = pointOfInterest
          PropData_ = propData
          PropState_ = propState
          PropId_ = propId }

    let empty =
        { Perimeter_ = box3 v3Zero Constants.Gameplay.TileSize
          Elevation_ = 0.0f
          Advents_ = Set.empty
          PointOfInterest_ = v3Zero
          PropData_ = EmptyProp
          PropState_ = NilState
          PropId_ = 0 }

type Prop = Prop.Prop