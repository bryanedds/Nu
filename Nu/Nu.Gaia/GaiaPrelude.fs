// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Gaia.Design

type WorldChanger = World -> World
type WorldChangers = WorldChanger List

type DragEntityState =
    | DragEntityPosition2d of MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityPosition3d of EntityDragOffset : Vector3 * EntityPlane : Plane3 * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyePosition2d of Vector2 * Vector2
    | DragEyeInactive

type EditorState =
    { TargetDir : string
      mutable RightClickPosition : Vector2
      mutable DragEntityState : DragEntityState
      mutable DragEyeState : DragEyeState
      mutable OtherSnaps : single * single * single
      mutable SelectedGroup : Group
      mutable FilePaths : Map<Group Address, string>
      mutable RefreshHierarchyViewRequested : bool // HACK: make sure hierarchy view isn't updated more than once per frame.
      mutable PastWorlds : World list
      mutable FutureWorlds : World list }

type SavedState =
    { BinaryFilePath : string
      UseGameplayScreen : bool
      UseImperativeExecution : bool }

/// Globals needed to sync Nu with WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let mutable Screen = Simulants.Default.Screen
    let mutable World = Unchecked.defaultof<World>
    let mutable EditorState = Unchecked.defaultof<EditorState>
    let mutable Form = Unchecked.defaultof<GaiaForm>
    let mutable WorldChangers = WorldChangers ()
    let mutable SelectEntity : Entity -> GaiaForm -> World -> unit = Unchecked.defaultof<_>

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        EditorState.PastWorlds <- pastWorld :: EditorState.PastWorlds
        EditorState.FutureWorlds <- []