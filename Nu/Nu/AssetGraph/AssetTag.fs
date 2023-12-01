// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System

/// Describes a means for looking up an asset.
type [<StructuralEquality; StructuralComparison; Struct>] 'a AssetTag =
    { PackageName : string
      AssetName : string }

[<RequireQualifiedAccess>]
module AssetTag =

    /// Check two asset tags for equality.
    let inline equals left right =
        left.PackageName = right.PackageName &&
        left.AssetName = right.AssetName

    /// Make an asset tag.
    let make<'a> packageName assetName : 'a AssetTag =
        { PackageName = packageName; AssetName = assetName }

    /// Convert an asset tag to a string pair.
    let toPair<'a> (assetTag : 'a AssetTag) =
        (assetTag.PackageName, assetTag.AssetName)

    /// Make an asset tag from a string pair.
    let ofPair<'a> (packageName, assetName) =
        make<'a> packageName assetName

    /// Convert an asset tag from one type to another.
    let convert<'a, 'b> (assetTag : 'a AssetTag) : 'b AssetTag =
        make<'b> assetTag.PackageName assetTag.AssetName

    /// Convert an asset tag with a specific type to one of obj type.
    let generalize (assetTag : 'a AssetTag) : obj AssetTag =
        convert<'a, obj> assetTag

    /// Convert an asset tag with an obj type to one of a specific type.
    let specialize<'a> (assetTag : obj AssetTag) : 'a AssetTag =
        convert<obj, 'a> assetTag

    /// Infer the internal format of an asset by its name.
    /// TODO: move this somewhere more relevant?
    let inferInternalFormatFromAssetName (assetTag : _ AssetTag) =
        if  assetTag.AssetName.EndsWith "_n" ||
            assetTag.AssetName.EndsWith "_u" ||
            assetTag.AssetName.EndsWith "Normal" ||
            assetTag.AssetName.EndsWith "Uncompressed" then
            Constants.OpenGl.UncompressedTextureFormat
        else Constants.OpenGl.CompressedColorTextureFormat

[<AutoOpen>]
module AssetTagOperators =

    /// Check two asset tags for equality.
    let inline assetEq left right =
        AssetTag.equals left right

    /// Check two asset tags for inequality.
    let inline assetNeq left right =
        not (AssetTag.equals left right)

    /// Make an asset tag.
    let asset<'a> packageName assetName : 'a AssetTag =
        AssetTag.make packageName assetName