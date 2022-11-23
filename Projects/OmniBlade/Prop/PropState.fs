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