// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu
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
      SelectedGroup : Group
      FilePaths : Map<Group Address, string>
      RefreshHierarchyViewRequested : bool } // HACK: make sure hierarchy view isn't updated more than once per frame.

type SavedState =
    { BinaryFilePath : string
      UseGameplayScreen : bool
      UseImperativeExecution : bool }

/// Globals needed to sync Nu with WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let EditorGuid = Gen.id
    let mutable Form = Unchecked.defaultof<GaiaForm>
    let mutable World = Unchecked.defaultof<World>
    let mutable Screen = Simulants.DefaultScreen
    let mutable PastWorlds : World list = []
    let mutable FutureWorlds : World list = []
    let mutable WorldChangers = WorldChangers ()
    let mutable SelectEntity : Entity -> GaiaForm -> World -> unit = Unchecked.defaultof<_>

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        PastWorlds <- pastWorld :: PastWorlds
        FutureWorlds <- []