// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu.Gaia
open OpenTK
open System
open System.Collections.Generic
open Prime
open Nu
open Nu.Gaia
open Nu.Gaia.Design

type WorldChanger = World -> World

type WorldChangers = WorldChanger List

type DragEntityState =
    | DragEntityPosition of Vector2 * Vector2 * Entity
    | DragEntityRotation of Vector2 * Vector2 * Entity
    | DragEntityNone

type DragCameraState =
    | DragCameraPosition of Vector2 * Vector2
    | DragCameraNone

type EditorState =
    { TargetDir : string
      RightClickPosition : Vector2
      DragEntityState : DragEntityState
      DragCameraState : DragCameraState
      PastWorlds : World list
      FutureWorlds : World list
      SelectedLayer : Layer }