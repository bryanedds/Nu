// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Numerics
open Prime
open Nu
open Nu.Gaia

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

    let mutable private pastWorlds = [] : World list
    let mutable private futureWorlds = [] : World list
    let mutable World = Unchecked.defaultof<World>

    let pushPastWorld () =
        World <- Nu.World.shelve World
        pastWorlds <- World :: pastWorlds
        futureWorlds <- []

    let canUndo () =
        List.notEmpty pastWorlds

    let canRedo () =
        List.notEmpty pastWorlds

    let tryUndo () =
        if not (Nu.World.getImperative World) then
            match pastWorlds with
            | pastWorld :: pastWorlds' ->
                let futureWorld = Nu.World.shelve World
                World <- Nu.World.unshelve pastWorld
                pastWorlds <- pastWorlds'
                futureWorlds <- futureWorld :: futureWorlds
                true
            | [] -> false
        else false

    let tryRedo () =
        if not (Nu.World.getImperative World) then
            match futureWorlds with
            | futureWorld :: futureWorlds' ->
                let pastWorld = Nu.World.shelve World
                World <- Nu.World.unshelve futureWorld
                pastWorlds <- pastWorld :: pastWorlds
                futureWorlds <- futureWorlds'
                true
            | [] -> false
        else false