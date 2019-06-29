// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2018.

namespace Nu
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging
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
    | TextureMetadata of Vector2i * PixelFormat
    | TileMapMetadata of string * Image AssetTag list * TmxMap
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
            try AssetTag.make<Image> properties.["PackageName"] properties.["ImageAssetName"]
            with :? KeyNotFoundException ->
                let errorMessage = "TileSet '" + tileSet.Name + "' missing one or more properties (PackageName or AssetName)."
                raise (TileSetPropertyNotFoundException errorMessage)

        let private generateTextureMetadata asset =
            if not (File.Exists asset.FilePath) then
                let errorMessage = "Failed to load Bitmap due to missing file '" + asset.FilePath + "'."
                Log.trace errorMessage
                InvalidMetadata errorMessage
            else
                // TODO: P1: find an efficient way to pull metadata from a bitmap file without loading its actual bitmap.
                try use bitmap = new Bitmap (asset.FilePath)
                    TextureMetadata (Vector2i (bitmap.Width, bitmap.Height), bitmap.PixelFormat)
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
                | _ -> InvalidMetadata ("Could not load asset metadata '" + scstring asset + "' due to unknown extension '" + extension + "'.")
            (asset.AssetTag.AssetName, metadata)

        let private tryGenerateMetadataSubmap packageName assetGraph =
            match AssetGraph.tryLoadAssetsFromPackage true None packageName assetGraph with
            | Right assets ->
                let submap = assets |> List.map generateAssetMetadata |> UMap.makeFromSeq Constants.Metadata.MetadatMapConfig
                (packageName, submap)
            | Left error ->
                Log.info ("Could not load asset metadata for package '" + packageName + "' due to: " + error)
                (packageName, UMap.makeEmpty Constants.Metadata.MetadatMapConfig)

        let private makeMetadataMap packageNames assetGraph =
            List.fold
                (fun metadata packageName ->
                    let (packageName, submap) = tryGenerateMetadataSubmap packageName assetGraph
                    UMap.add packageName submap metadata)
                (UMap.makeEmpty Constants.Metadata.MetadatMapConfig)
                packageNames

        /// Try to get the metadata of the given asset.
        let tryGetMetadata (assetTag : obj AssetTag) metadata =
            match UMap.tryFind assetTag.PackageName metadata.MetadataMap with
            | Some package ->
                match UMap.tryFind assetTag.AssetName package with
                | Some _ as asset -> asset
                | None -> None
            | None -> None

        /// Try to get the texture metadata of the given asset.
        let tryGetTextureSize (assetTag : Image AssetTag) metadata =
            match tryGetMetadata (AssetTag.generalize assetTag) metadata with
            | Some (TextureMetadata (size, _)) -> Some size
            | None -> None
            | _ -> None

        /// Try to get the texture metadata of the given asset.
        let tryGetTextureFormat (assetTag : Image AssetTag) metadata =
            match tryGetMetadata (AssetTag.generalize assetTag) metadata with
            | Some (TextureMetadata (_, format)) -> Some format
            | None -> None
            | _ -> None

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        let getTextureSize assetTag metadata =
            Option.get (tryGetTextureSize assetTag metadata)

        /// Try to get the texture size metadata of the given asset.
        let tryGetTextureSizeF assetTag metadata =
            match tryGetTextureSize assetTag metadata with
            | Some size -> Some (Vector2 (single size.X, single size.Y))
            | None -> None

        /// Forcibly get the texture size metadata of the given asset (throwing on failure).
        let getTextureSizeF assetTag metadata =
            Option.get (tryGetTextureSizeF assetTag metadata)

        /// Try to get the tile map metadata of the given asset.
        let tryGetTileMapMetadata (assetTag : TileMap AssetTag) metadata =
            match tryGetMetadata (AssetTag.generalize assetTag) metadata with
            | Some (TileMapMetadata (filePath, images, tmxMap)) -> Some (filePath, images, tmxMap)
            | None -> None
            | _ -> None

        /// Forcibly get the tile map metadata of the given asset (throwing on failure).
        let getTileMapMetadata assetTag metadata =
            Option.get (tryGetTileMapMetadata assetTag metadata)

        /// Get a copy of the metadata map.
        let getMetadataMap metadata =
            let map =
                metadata.MetadataMap |>
                UMap.toSeq |>
                Seq.map (fun (packageName, map) -> (packageName, map |> UMap.toSeq |> Map.ofSeq)) |>
                Map.ofSeq
            map

        /// Get a map of all the discovered assets.
        let getAssetMap metadata =
            let assetMap =
                getMetadataMap metadata |>
                Map.map (fun _ metadata -> Map.toKeyList metadata)
            assetMap

        /// Generate metadata from the given asset graph.
        let make assetGraph =
            let packageNames = AssetGraph.getPackageNames assetGraph
            let metadataMap = makeMetadataMap packageNames assetGraph
            { MetadataMap = metadataMap }

        /// The empty metadata.
        let makeEmpty () =
            { MetadataMap = UMap.makeEmpty Constants.Metadata.MetadatMapConfig }

/// Stores metadata for game assets.
type Metadata = MetadataModule.Metadata