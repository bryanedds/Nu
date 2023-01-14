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
    | DragEyeCenter2d of Vector2 * Vector2
    | DragEyeInactive

type SavedState =
    { BinaryFilePath : string
      EditModeOpt : string option
      UseImperativeExecution : bool }
    static member defaultState =
        { BinaryFilePath = ""
          EditModeOpt = None
          UseImperativeExecution = false }

/// Global state and functionality needed to interoperate Nu and WinForms.
[<RequireQualifiedAccess>]
module Globals =

    let mutable private pastWorlds = [] : World list
    let mutable private futureWorlds = [] : World list
    let mutable private preUpdaters = Updaters ()
    let mutable private updaters = Updaters ()
    let mutable private selectEntityFn = Unchecked.defaultof<_> : Entity -> GaiaForm -> World -> unit
    let mutable Form = Unchecked.defaultof<GaiaForm>
    let mutable World = Unchecked.defaultof<World>

    let nextPreUpdate updater =
        preUpdaters.Add updater

    let nextUpdate updater =
        updaters.Add updater

    let processPreUpdaters world =
        let preUpdatersCopy = List.ofSeq preUpdaters
        preUpdaters.Clear ()
        List.fold (fun world updater -> updater world) world preUpdatersCopy

    let processUpdaters world =
        let perUpdatersCopy = List.ofSeq updaters
        updaters.Clear ()
        List.fold (fun world updater -> updater world) world perUpdatersCopy

    let pushPastWorld pastWorld =
        let pastWorld = Nu.World.shelve pastWorld
        pastWorlds <- pastWorld :: pastWorlds
        futureWorlds <- []
        pastWorld

    let canUndo () =
        List.notEmpty pastWorlds

    let canRedo () =
        List.notEmpty pastWorlds

    let tryUndo world =
        if not (Nu.World.getImperative world) then
            match pastWorlds with
            | pastWorld :: pastWorlds' ->
                let futureWorld = Nu.World.shelve world
                let world = Nu.World.unshelve pastWorld
                pastWorlds <- pastWorlds'
                futureWorlds <- futureWorld :: futureWorlds
                (true, world)
            | [] -> (false, world)
        else (false, world)

    let tryRedo world =
        if not (Nu.World.getImperative world) then
            match futureWorlds with
            | futureWorld :: futureWorlds' ->
                let pastWorld = Nu.World.shelve world
                let world = Nu.World.unshelve futureWorld
                pastWorlds <- pastWorld :: pastWorlds
                futureWorlds <- futureWorlds'
                (true, world)
            | [] -> (false, world)
        else (false, world)

    let selectEntity =
        selectEntityFn

    let init selectEntity =
        selectEntityFn <- selectEntity