// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// A tag interface for describing an emitter.
type EmitterDescriptor = interface end

/// IO artifacts passively produced and consumed by Nu.
type [<ReferenceEquality>] View =
    | Render2d of single * single * obj AssetTag * RenderOperation2d
    | Render3d of RenderMessage3d
    | PlaySound of single * Sound AssetTag
    | PlaySong of GameTime * GameTime * GameTime * single * Song AssetTag
    | FadeOutSong of GameTime
    | StopSong
    | SpawnEmitter of string * EmitterDescriptor
    | Tag of string * obj
    | Views of View array
    | ViewsSegmented of View SArray

[<RequireQualifiedAccess>]
module View =

    /// Convert a view to a seq of zero or more views.
    let rec toSeq view =
        seq {
            match view with
            | Views views -> for view in views do yield! toSeq view
            | ViewsSegmented views -> for view in views do yield! toSeq view
            | _ -> yield view }

    /// The empty view.
    let empty = Views [||]