// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open Prime

/// A tag interface for describing an emitter.
type EmitterDescriptor = interface end

/// IO artifacts passively produced and consumed by Nu.
type [<ReferenceEquality>] View =
    | SpriteView of single * single * obj AssetTag * SpriteValue
    | TextView of single * single * obj AssetTag * TextValue
    | Light3dView of Light3dValue
    | BillboardView of BillboardValue
    | StaticModelView of StaticModelValue
    | StaticModelSurfaceView of StaticModelSurfaceValue
    | SpawnEmitter of string * EmitterDescriptor
    | Tag of string * obj
    | Views of View SArray

[<RequireQualifiedAccess>]
module View =

    /// Convert a view to a seq of zero or more views.
    let rec toSeq view =
        seq {
            match view with
            | Views views -> for view in views do yield! toSeq view
            | _ -> yield view }

    /// The empty view.
    let empty = Views SArray.empty