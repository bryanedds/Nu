﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OmniBlade
open System
open System.Numerics
open Prime
open Nu

type PropState =
    | DoorState of bool
    | CharacterState of Color * CharacterAnimationState
    | SpriteState of Image AssetTag * Color * Blend * Color * Flip * bool
    | NilState

[<RequireQualifiedAccess>]
module Prop =

    type [<SymbolicExpansion>] Prop =
        { Perimeter : Box3
          Elevation : single
          PropData : PropData
          PropState : PropState
          PropId : int }

    let mapPropState mapper (prop : Prop) =
        { prop with PropState = mapper prop.PropState }

    let make bounds elevation propData propState propId =
        { Perimeter = bounds
          Elevation = elevation
          PropData = propData
          PropState = propState
          PropId = propId }

    let empty =
        { Perimeter = box3 v3Zero Constants.Gameplay.TileSize
          Elevation = 0.0f
          PropData = EmptyProp
          PropState = NilState
          PropId = 0 }

type Prop = Prop.Prop