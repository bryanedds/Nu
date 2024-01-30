// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// Tokenized data and processing requests.
/// NOTE: EffectToken only exists as a way to make effect emitters on emitters work due to issue #141.
/// NOTE: some case fields are objs because the type information isn't available at the union's definition.
type [<ReferenceEquality>] Token =
    | SpriteToken of Elevation : single * Horizon : single * Image : AssetTag * SpriteValue : SpriteValue
    | TextToken of Elevation : single * Horizon : single * Font : AssetTag * TextValue : TextValue
    | Light3dToken of Light3dValue : Light3dValue
    | BillboardToken of BillboardValue : BillboardValue
    | StaticModelToken of StaticModelValue : StaticModelValue
    | StaticModelSurfaceToken of StaticModelSurfaceValue : StaticModelSurfaceValue
    | EffectToken of Name : string * EffectDescriptorObj : obj * SliceObj : obj
    | EmitterToken of Name : string * EmitterDescriptorObj : obj
    | TagToken of Name : string * UserDefined : obj
    | Tokens of Tokens : Token SArray

[<RequireQualifiedAccess>]
module Token =

    /// Convert a token to a seq of zero or more tokens.
    let rec toSeq token =
        seq {
            match token with
            | Tokens tokens -> for token in tokens do yield! toSeq token
            | _ -> yield token }

    /// The empty token.
    let empty = Tokens SArray.empty