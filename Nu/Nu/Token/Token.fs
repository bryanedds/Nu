// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

/// A tag interface for describing an emitter.
type EmitterDescriptor = interface end

/// Tokenized processing requests.
type [<ReferenceEquality>] Token =
    | SpriteToken of single * single * AssetTag * SpriteValue
    | TextToken of single * single * AssetTag * TextValue
    | Light3dToken of Light3dValue
    | BillboardToken of BillboardValue
    | StaticModelToken of StaticModelValue
    | StaticModelSurfaceToken of StaticModelSurfaceValue
    | EmitterToken of string * EmitterDescriptor
    | TagToken of string * obj
    | Tokens of Token SArray

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