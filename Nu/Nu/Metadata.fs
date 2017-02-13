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

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

/// Metadata for an asset. Useful to describe various attributes of an asset without having the
/// full asset loaded into memory.
type [<StructuralEquality; NoComparison>] AssetMetadata =
    | TextureMetadata of Vector2i
    | TileMapMetadata of string * AssetTag list * TmxMap
    | SoundMetadata
    | SongMetadata
    | OtherMetadata of obj
    | InvalidMetadata of string

[<AutoOpen>]
module MetadataModule =

    /// Stores metadata for game assets.
    type [<NoEquality; NoComparison>] Metadata =
        private
            { MetadataMap : UMap<string, UMap<string, AssetMetadata>> }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
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
                // TODO: P1: find an efficient way to pull metadata from a bitmap file without loading its actual bitmap.
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

        let private tryGenerateMetadataSubmap packageName assetGraph =
            match AssetGraph.tryLoadAssetsFromPackage true None packageName assetGraph with
            | Right assets ->
                let submap = assets |> Seq.map generateAssetMetadata |> UMap.ofSeq
                (packageName, submap)
            | Left error ->
                Log.info ^ "Could not load asset metadata for package '" + packageName + "' due to: " + error
                (packageName, UMap.makeEmpty None)

        let private makeMetadataMap packageNames assetGraph =
            List.fold
                (fun metadata packageName ->
                    let (packageName, submap) = tryGenerateMetadataSubmap packageName assetGraph
                    UMap.add packageName submap metadata)
                (UMap.makeEmpty None)
                packageNames

        /// Try to get the metadata of the given asset.
        let tryGetMetadata (assetTag : AssetTag) metadata =
            let packageOpt = UMap.tryFind assetTag.PackageName metadata.MetadataMap
            match packageOpt with
            | Some package ->
                let assetOpt = UMap.tryFind assetTag.AssetName package
                match assetOpt with
                | Some _ as asset -> asset
                | None -> None
            | None -> None

        /// Try to get the texture metadata of the given asset.
        let tryGetTextureSize assetTag metadata =
            let assetOpt = tryGetMetadata assetTag metadata
            match assetOpt with
            | Some (TextureMetadata size) -> Some size
            | None -> None
            | _ -> None

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        let getTextureSize assetTag metadata =
            Option.get ^ tryGetTextureSize assetTag metadata

        /// Try to get the texture size metadata of the given asset.
        let tryGetTextureSizeAsVector2 assetTag metadata =
            match tryGetTextureSize assetTag metadata with
            | Some size -> Some ^ Vector2 (single size.X, single size.Y)
            | None -> None

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        let getTextureSizeAsVector2 assetTag metadata =
            Option.get ^ tryGetTextureSizeAsVector2 assetTag metadata

        /// Try to get the tile map metadata of the given asset.
        let tryGetTileMapMetadata assetTag metadata =
            let assetOpt = tryGetMetadata assetTag metadata
            match assetOpt with
            | Some (TileMapMetadata (filePath, images, tmxMap)) -> Some (filePath, images, tmxMap)
            | None -> None
            | _ -> None

        /// Forcibly get the tile map metadata of the given asset (throwing on failure).
        let getTileMapMetadata assetTag metadata =
            Option.get ^ tryGetTileMapMetadata assetTag metadata

        /// Generate metadata from the given asset graph.
        let make assetGraph =
            let packageNames = AssetGraph.getPackageNames assetGraph
            let metadataMap = makeMetadataMap packageNames assetGraph
            { MetadataMap = metadataMap }

        /// The empty metadata.
        let makeEmpty () =
            { MetadataMap = UMap.makeEmpty None }

/// Stores metadata for game assets.
type Metadata = MetadataModule.Metadata