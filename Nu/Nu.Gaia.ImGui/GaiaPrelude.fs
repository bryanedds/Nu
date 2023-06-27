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
    | DragEntityPosition3d of Time : DateTimeOffset * EntityDragOffset : Vector3 * EntityPlane : Plane3 * Entity : Entity
    | DragEntityRotation3d of Time : DateTimeOffset * MousePositionWorldOrig : Vector2 * EntityDragOffset : single * EntityDragAxis : Vector3 * Entity : Entity
    | DragEntityInactive

type DragEyeState =
    | DragEyeCenter2d of Vector2 * Vector2
    | DragEyeInactive

type SavedState =
    { AssemblyFilePath : string
      EditModeOpt : string option
      UseImperativeExecution : bool }
    static member defaultState =
        { AssemblyFilePath = ""
          EditModeOpt = None
          UseImperativeExecution = false }

/// Global state and functionality needed to interoperate Nu and WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let mutable private PastWorlds = [] : World list
    let mutable private FutureWorlds = [] : World list
    let mutable World = Unchecked.defaultof<World>

    let pushPastWorld () =
        World <- Nu.World.shelve World
        PastWorlds <- World :: PastWorlds
        FutureWorlds <- []

    let canUndo () =
        List.notEmpty PastWorlds

    let canRedo () =
        List.notEmpty PastWorlds

    let tryUndo world =
        if not (Nu.World.getImperative world) then
            match PastWorlds with
            | pastWorld :: pastWorlds' ->
                let futureWorld = Nu.World.shelve world
                let world = Nu.World.unshelve pastWorld
                PastWorlds <- pastWorlds'
                FutureWorlds <- futureWorld :: FutureWorlds
                World <- world
                (true, world)
            | [] -> (false, world)
        else (false, world)

    let tryRedo world =
        if not (Nu.World.getImperative world) then
            match FutureWorlds with
            | futureWorld :: futureWorlds' ->
                let pastWorld = Nu.World.shelve world
                let world = Nu.World.unshelve futureWorld
                PastWorlds <- pastWorld :: PastWorlds
                FutureWorlds <- futureWorlds'
                World <- world
                (true, world)
            | [] -> (false, world)
        else (false, world)