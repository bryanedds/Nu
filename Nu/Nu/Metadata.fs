// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.Drawing
open System.IO
open OpenTK
open TiledSharp
open Prime
open Nu

/// Metadata for an asset. Useful to describe various attributes of an asset without having the
/// full asset loaded into memory.
type [<StructuralEquality; NoComparison>] AssetMetadata =
    | TextureMetadata of Vector2i
    | TileMapMetadata of string * AssetTag list * TmxMap
    | SoundMetadata
    | SongMetadata
    | OtherMetadata of obj
    | InvalidMetadata of string

/// A map of asset names to asset metadata.
/// TODO: consider turning this, combined with the functions in the Metadata module, into an ADT.
type AssetMetadataMap = Umap<string, Umap<string, AssetMetadata>>

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

[<RequireQualifiedAccess>]
module Metadata =

    let private getTileSetProperties (tileSet : TmxTileset) =
        let properties = tileSet.Properties
        try { PackageName = properties.["PackageName"]
              AssetName = properties.["ImageAssetName"] }
        with :? KeyNotFoundException ->
            let errorMessage = "TileSet '" + tileSet.Name + "' missing one or more properties (PackageName or AssetName)."
            raise ^ TileSetPropertyNotFoundException errorMessage

    let private generateTextureMetadata asset =
        if not ^ File.Exists asset.FilePath then
            let errorMessage = "Failed to load Bitmap due to missing file '" + asset.FilePath + "'."
            Log.trace errorMessage
            InvalidMetadata errorMessage
        else
            // TODO: find an efficient way to pull metadata from a bitmap file without loading its
            // actual image buffer.
            try use bitmap = new Bitmap (asset.FilePath)
                if bitmap.PixelFormat = Imaging.PixelFormat.Format32bppArgb
                then TextureMetadata ^ Vector2i (bitmap.Width, bitmap.Height)
                else
                    let errorMessage = "Bitmap with invalid format (expecting 32-bit ARGB)."
                    Log.trace errorMessage
                    InvalidMetadata errorMessage
            with _ as exn ->
                let errorMessage = "Failed to load Bitmap '" + asset.FilePath + "' due to: " + scstring exn
                Log.trace errorMessage
                InvalidMetadata errorMessage

    let private generateTileMapMetadata asset =
        try let tmxMap = TmxMap asset.FilePath
            let tileSets = List.ofSeq tmxMap.Tilesets
            let tileSetImages = List.map getTileSetProperties tileSets
            TileMapMetadata (asset.FilePath, tileSetImages, tmxMap)
        with _ as exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            InvalidMetadata errorMessage

    let private generateAssetMetadata asset =
        let extension = Path.GetExtension asset.FilePath
        let metadata =
            match extension with
            | ".bmp"
            | ".png" -> generateTextureMetadata asset
            | ".tmx" -> generateTileMapMetadata asset
            | ".wav" -> SoundMetadata
            | ".ogg" -> SongMetadata
            | _ -> InvalidMetadata ^ "Could not load asset metadata '" + scstring asset + "' due to unknown extension '" + extension + "'."
        (asset.AssetTag.AssetName, metadata)

    let private tryGenerateAssetMetadataSubmap packageName assetGraph =
        match AssetGraph.tryLoadAssetsFromPackage true None packageName assetGraph with
        | Right assets ->
            let submap = assets |> Seq.map generateAssetMetadata |> Umap.ofSeq
            (packageName, submap)
        | Left error ->
            Log.info ^ "Could not load asset metadata for package '" + packageName + "' due to: " + error
            (packageName, Umap.makeEmpty None)

    let private generateAssetMetadataMap2 packageNames assetGraph =
        List.fold
            (fun assetMetadataMap packageName ->
                let (packageName, submap) = tryGenerateAssetMetadataSubmap packageName assetGraph
                Umap.add packageName submap assetMetadataMap)
            (Umap.makeEmpty None)
            packageNames

    /// Generate an asset metadata map from the given asset graph.
    let generateAssetMetadataMap assetGraph =
        let packageNames = AssetGraph.getPackageNames assetGraph
        let assetMetadataMap = generateAssetMetadataMap2 packageNames assetGraph
        assetMetadataMap

    /// Try to get the metadata of the given asset.
    let tryGetMetadata (assetTag : AssetTag) assetMetadataMap =
        let optPackage = Umap.tryFind assetTag.PackageName assetMetadataMap
        match optPackage with
        | Some package ->
            let optAsset = Umap.tryFind assetTag.AssetName package
            match optAsset with
            | Some _ as asset -> asset
            | None -> None
        | None -> None

    /// Try to get the texture metadata of the given asset.
    let tryGetTextureSize assetTag assetMetadataMap =
        let optAsset = tryGetMetadata assetTag assetMetadataMap
        match optAsset with
        | Some (TextureMetadata size) -> Some size
        | None -> None
        | _ -> None

    /// Forcibly get the texture size metadata of the given asset (throwing on failure).
    let getTextureSize assetTag assetMetadataMap =
        Option.get ^ tryGetTextureSize assetTag assetMetadataMap

    /// Try to get the texture size metadata of the given asset.
    let tryGetTextureSizeAsVector2 assetTag assetMetadataMap =
        match tryGetTextureSize assetTag assetMetadataMap with
        | Some size -> Some ^ Vector2 (single size.X, single size.Y)
        | None -> None

    /// Forcibly get the texture size metadata of the given asset (throwing on failure).
    let getTextureSizeAsVector2 assetTag assetMetadataMap =
        Option.get ^ tryGetTextureSizeAsVector2 assetTag assetMetadataMap

    /// Try to get the tile map metadata of the given asset.
    let tryGetTileMapMetadata assetTag assetMetadataMap =
        let optAsset = tryGetMetadata assetTag assetMetadataMap
        match optAsset with
        | Some (TileMapMetadata (filePath, images, tmxMap)) -> Some (filePath, images, tmxMap)
        | None -> None
        | _ -> None

    /// Forcibly get the tile map metadata of the given asset (throwing on failure).
    let getTileMapMetadata assetTag assetMetadataMap =
        Option.get ^ tryGetTileMapMetadata assetTag assetMetadataMap