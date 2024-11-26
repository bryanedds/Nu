// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open Prime

/// Data tokens for downstream processing.
/// NOTE: EffectToken exists only as a way to make effect emitters on emitters work due to issue #141. If we can cut
/// this dependency then we can place this type definition above all the effects types in its own code folder.
type [<ReferenceEquality>] DataToken =
    | SpriteToken of Elevation : single * Horizon : single * Image : AssetTag * SpriteValue : SpriteValue
    | TextToken of Elevation : single * Horizon : single * Font : AssetTag * TextValue : TextValue
    | Light3dToken of Light3dValue : Light3dValue
    | BillboardToken of BillboardValue : BillboardValue
    | StaticModelToken of StaticModelValue : StaticModelValue
    | StaticModelSurfaceToken of StaticModelSurfaceValue : StaticModelSurfaceValue
    | EmitterToken of Name : string * EmitterDescriptor : Particles.EmitterDescriptor
    | EffectToken of Name : string * EffectDescriptor : Effects.EffectDescriptor * Slice : Effects.Slice
    | TagToken of Name : string * UserDefined : obj
    | DataTokens of DataTokens : DataToken SArray

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