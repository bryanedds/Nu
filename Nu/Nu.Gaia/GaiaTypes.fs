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
      RightClickPosition : Vector2
      DragEntityState : DragEntityState
      DragEyeState : DragEyeState
      OtherSnaps : single * single * single
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
    let mutable Screen = Simulants.Default.Screen
    let mutable PastWorlds : World list = []
    let mutable FutureWorlds : World list = []
    let mutable WorldChangers = WorldChangers ()
    let mutable SelectEntity : Entity -> GaiaForm -> World -> unit = Unchecked.defaultof<_>

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        PastWorlds <- pastWorld :: PastWorlds
        FutureWorlds <- []