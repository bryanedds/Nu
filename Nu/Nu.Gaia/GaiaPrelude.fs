// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Diagnostics
open System.IO
open System.Numerics
open ImGuiNET
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

type SavedState =
    { ProjectFilePath : string
      EditModeOpt : string option
      UseImperativeExecution : bool }
    static member defaultState =
        { ProjectFilePath = ""
          EditModeOpt = None
          UseImperativeExecution = false }