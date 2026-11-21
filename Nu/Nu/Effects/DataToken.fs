// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu
open System
open Prime

/// Data tokens for downstream processing.
type [<ReferenceEquality>] DataToken =
    | SpriteToken of Elevation : single * Horizon : single * Image : AssetTag * SpriteValue : SpriteValue
    | TextToken of Elevation : single * Horizon : single * Font : AssetTag * TextValue : TextValue
    | Light3dToken of Light3dValue : Light3dValue
    | BillboardToken of BillboardValue : BillboardValue
    | StaticModelToken of StaticModelValue : StaticModelValue
    | StaticModelSurfaceToken of StaticModelSurfaceValue : StaticModelSurfaceValue
    | EmitterToken of Name : string * EmitterDescriptor : Particles.EmitterDescriptor
    | TagToken of Name : string * UserDefined : obj
    | DataTokens of DataTokens : DataToken SArray

/// DataToken functions.
[<RequireQualifiedAccess>]
module DataToken =

    /// Flatten a data token to a linear sequence of zero or more data tokens.
    let rec toSeq dataToken =
        seq {
            match dataToken with
            | DataTokens dataTokens -> for token in dataTokens do yield! toSeq token
            | _ -> yield dataToken }

    /// The empty data token.
    let empty = DataTokens SArray.empty