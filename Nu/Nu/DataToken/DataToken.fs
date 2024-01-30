// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// Data tokens for optional downstream processing.
/// NOTE: EffectToken only exists as a way to make effect emitters on emitters work due to issue #141.
/// NOTE: some case fields are objs because the type information isn't available at the union's definition.
type [<ReferenceEquality>] DataToken =
    | SpriteToken of Elevation : single * Horizon : single * Image : AssetTag * SpriteValue : SpriteValue
    | TextToken of Elevation : single * Horizon : single * Font : AssetTag * TextValue : TextValue
    | Light3dToken of Light3dValue : Light3dValue
    | BillboardToken of BillboardValue : BillboardValue
    | StaticModelToken of StaticModelValue : StaticModelValue
    | StaticModelSurfaceToken of StaticModelSurfaceValue : StaticModelSurfaceValue
    | EffectToken of Name : string * EffectDescriptorObj : obj * SliceObj : obj
    | EmitterToken of Name : string * EmitterDescriptorObj : obj
    | TagToken of Name : string * UserDefined : obj
    | DataTokens of DataTokens : DataToken SArray

[<RequireQualifiedAccess>]
module DataToken =

    /// Convert a data token to a seq of zero or more data tokens.
    let rec toSeq dataToken =
        seq {
            match dataToken with
            | DataTokens dataTokens -> for token in dataTokens do yield! toSeq token
            | _ -> yield dataToken }

    /// The empty data token.
    let empty = DataTokens SArray.empty