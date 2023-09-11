// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Numerics
open Prime
open Nu
open Nu.Gaia

type DragEntityState =
    | DragEntityPosition2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : single * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyeCenter2d of Vector2 * Vector2
    | DragEyeInactive

type [<SymbolicExpansion>] GaiaState =
    { ProjectDllPath : string
      EditModeOpt : string option
      UseImperativeExecution : bool }
    static member defaultState =
        { ProjectDllPath = ""
          EditModeOpt = None
          UseImperativeExecution = false }