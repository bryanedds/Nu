// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System

/// IO artifacts passively produced and consumed by Nu.
type [<NoEquality; NoComparison>] View =
    | Render of single * single * obj AssetTag * RenderDescriptor
    | PlaySound of single * Sound AssetTag
    | PlaySong of int * single * Song AssetTag
    | FadeOutSong of int
    | StopSong
    | Tag of string * obj
    | Views of View array

[<RequireQualifiedAccess>]
module View =

    /// Convert a view to an array of zero or more views.
    let toArray view =
        match view with
        | Views views -> views
        | _ -> [|view|]

    /// The empty view.
    let empty = Views [||]
