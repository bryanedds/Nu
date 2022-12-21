// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu
open Nu.Gaia.Design

type Updater = World -> World
type Updaters = Updater List

type DragEntityState =
    | DragEntityPosition2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityRotation2d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : Vector2 * Entity : Entity
    | DragEntityPosition3d of Time : DateTimeOffset * EntityDragOffset : Vector3 * EntityPlane : Plane3 * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyePosition2d of Vector2 * Vector2
    | DragEyeInactive

type SavedState =
    { BinaryFilePath : string
      EditModeOpt : string option
      UseImperativeExecution : bool }
    static member defaultState =
        { BinaryFilePath = ""
          EditModeOpt = None
          UseImperativeExecution = false }

/// Globals needed to sync Nu with WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let mutable RightClickPosition = v2Zero
    let mutable DragEntityState = DragEntityInactive
    let mutable DragEyeState = DragEyeInactive
    let mutable OtherSnaps = (Constants.Editor.Position3dSnapDefault, Constants.Editor.Degrees3dSnapDefault, Constants.Editor.Scale3dSnapDefault)
    let mutable FilePaths = Map.empty<Group Address, string>
    let mutable RefreshHierarchyViewRequested = false // HACK: make sure hierarchy view isn't updated more than once per frame.
    let mutable PastWorlds = [] : World list
    let mutable FutureWorlds = [] : World list
    let mutable (PreUpdaters, PerUpdaters) = (Updaters (), Updaters ())
    let mutable SelectEntity = Unchecked.defaultof<_> : Entity -> GaiaForm -> World -> unit
    let mutable SelectedScreen = Unchecked.defaultof<Screen> // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable SelectedGroup = Unchecked.defaultof<Group>
    let mutable TargetDir = Unchecked.defaultof<string>
    let mutable Form = Unchecked.defaultof<GaiaForm>
    let mutable World = Unchecked.defaultof<World>

    let init selectEntity screen targetDir form world =
        SelectEntity <- selectEntity
        SelectedScreen <- screen
        SelectedGroup <- Nu.World.getGroups screen world |> Seq.head
        TargetDir <- targetDir
        Form <- form
        World <- world

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        PastWorlds <- pastWorld :: PastWorlds
        FutureWorlds <- []
        pastWorld