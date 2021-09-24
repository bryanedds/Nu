// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type [<NoEquality; NoComparison>] PropState =
    | PortalState of Vector4 * bool
    | DoorState of bool
    | ChestState of Vector4 * Guid
    | SwitchState of bool
    | SealState of bool
    | CharacterState of Vector4 * CharacterType * CharacterAnimationState * Color * Color * bool
    | NpcState of NpcType * Direction * Color * Color * bool
    | ShopkeepState of bool
    | NilState

[<RequireQualifiedAccess>]
module Prop =

    type [<ReferenceEquality; NoComparison>] Prop =
        private
            { Bounds_ : Vector4
              Elevation_ : single
              Advents_ : Advent Set
              PropData_ : PropData
              PropState_ : PropState
              PropId_ : int }

        (* Bounds Properties *)
        member this.Bounds = this.Bounds_
        member this.Position = this.Bounds_.Position
        member this.Center = this.Bounds_.Center
        member this.Bottom = this.Bounds_.Bottom
        member this.BottomInset = this.Bounds_.Bottom + Constants.Field.CharacterBottomOffset
        member this.Size = this.Bounds_.Size

        (* Local Properties *)
        member this.Elevation = this.Elevation_
        member this.Advents = this.Advents_
        member this.PropData = this.PropData_
        member this.PropState = this.PropState_
        member this.PropId = this.PropId_

    let updateBounds updater (prop : Prop) =
        { prop with Bounds_ = updater prop.Bounds_ }

    let updatePosition updater (prop : Prop) =
        { prop with Bounds_ = prop.Position |> updater |> prop.Bounds.WithPosition }

    let updateCenter updater (prop : Prop) =
        { prop with Bounds_ = prop.Center |> updater |> prop.Bounds.WithCenter }

    let updateBottom updater (prop : Prop) =
        { prop with Bounds_ = prop.Bottom |> updater |> prop.Bounds.WithBottom }

    let updatePropState updater (prop : Prop) =
        { prop with PropState_ = updater prop.PropState_ }

    let make bounds elevation advents propData propState propId =
        { Bounds_ = bounds
          Elevation_ = elevation
          Advents_ = advents
          PropData_ = propData
          PropState_ = propState
          PropId_ = propId }

    let empty =
        { Bounds_ = v4Bounds v2Zero Constants.Gameplay.TileSize
          Elevation_ = 0.0f
          Advents_ = Set.empty
          PropData_ = EmptyProp
          PropState_ = NilState
          PropId_ = 0 }

type Prop = Prop.Prop