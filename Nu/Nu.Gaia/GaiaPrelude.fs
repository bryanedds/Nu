// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Numerics
open Prime
open Nu

type DragEntityState =
    | DragEntityPosition2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : single * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyeCenter2d of Vector2 * Vector2
    | DragEyeInactive

type [<SymbolicExpansion>] GaiaState =
    { ProjectDllPath : string
      ProjectEditModeOpt : string option
      ProjectImperativeExecution : bool }
    static member make dllPath editModeOpt imperativeExecution =
        { ProjectDllPath = dllPath
          ProjectEditModeOpt = editModeOpt
          ProjectImperativeExecution = imperativeExecution }
    static member defaultState =
        GaiaState.make "" None false