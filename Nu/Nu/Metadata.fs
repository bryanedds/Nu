// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Imaging
open System.IO
open TiledSharp
open Prime
open Nu

/// Thrown when a tile set property is not found.
exception TileSetPropertyNotFoundException of string

/// Metadata for an asset. Useful to describe various attributes of an asset without having the
/// full asset loaded into memory.
type [<NoEquality; NoComparison>] AssetMetadata =
    | TextureMetadata of Vector2i * PixelFormat
    | TileMapMetadata of string * (TmxTileset * Image AssetTag) array * TmxMap
    | StaticModelMetadata of OpenGL.Hl.PhysicallyBasedStaticModel
    | SoundMetadata
    | SongMetadata
    | OtherMetadata of obj
    | InvalidMetadata of string

[<AutoOpen>]
module TmxExtensions =

    // OPTIMIZATION: cache tileset image assets.
    let ImageAssetsMemo = dictPlus<TmxTileset, Image AssetTag> HashIdentity.Structural []

    type TmxTileset with
        member this.ImageAsset =
            match ImageAssetsMemo.TryGetValue this with
            | (false, _) ->
                let imageAsset =
                    try scvalue<Image AssetTag> this.Properties.["Image"]
                    with :? KeyNotFoundException ->
                        let errorMessage =
                            "Tileset '" + this.Name + "' missing Image property.\n" +
                            "You must add a Custom Property to the tile set called 'Image' and give it an asset value like '[PackageName AssetName]'.\n" +
                            "This will specify where the engine can find the tile set's associated image asset."
                        raise (TileSetPropertyNotFoundException errorMessage)
                ImageAssetsMemo.Add (this, imageAsset)
                imageAsset
            | (true, imageAssets) -> imageAssets

    type TmxMap with
        member this.ImageAssets =
            this.Tilesets |>
            Array.ofSeq |>
            Array.map (fun (tileSet : TmxTileset) -> (tileSet, tileSet.ImageAsset))

[<RequireQualifiedAccess>]
module Metadata =

    /// Stores metadata for game assets.
    type [<NoEquality; NoComparison>] Metadata =
        private
            { MetadataMap : UMap<string, UMap<string, AssetMetadata>> }

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
            let imageAssets = tmxMap.ImageAssets
            TileMapMetadata (asset.FilePath, imageAssets, tmxMap)
        with _ as exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            InvalidMetadata errorMessage

    let private generateStaticModelMetadata asset =
        if not (File.Exists asset.FilePath) then
            let errorMessage = "Failed to load static model due to missing file '" + asset.FilePath + "'."
            Log.trace errorMessage
            InvalidMetadata errorMessage
        else
            use assimp = new Assimp.AssimpContext ()
            match OpenGL.Hl.TryCreatePhysicallyBasedStaticModel (false, asset.FilePath, assimp) with
            | Right model -> StaticModelMetadata model
            | Left error ->
                let errorMessage = "Failed to load static model '" + asset.FilePath + "' due to: " + error
                Log.trace errorMessage
                InvalidMetadata errorMessage

    let private generateAssetMetadata asset =
        let extension = Path.GetExtension asset.FilePath
        let metadata =
            match extension with
            | ".bmp"
            | ".png" -> generateTextureMetadata asset
            | ".tmx" -> generateTileMapMetadata asset
            | ".obj" -> generateStaticModelMetadata asset
            | ".wav" -> SoundMetadata
            | ".ogg" -> SongMetadata
            | _ -> InvalidMetadata ("Could not load asset metadata '" + scstring asset + "' due to unknown extension '" + extension + "'.")
        (asset.AssetTag.AssetName, metadata)

    let private tryGenerateMetadataSubmap imperative packageName assetGraph =
        match AssetGraph.tryLoadAssetsFromPackage true None packageName assetGraph with
        | Right assets ->
            let config = if imperative then Imperative else Functional
            let submap = assets |> List.map generateAssetMetadata |> UMap.makeFromSeq HashIdentity.Structural config
            (packageName, submap)
        | Left error ->
            Log.info ("Could not load asset metadata for package '" + packageName + "' due to: " + error)
            let config = if imperative then Imperative else Functional
            (packageName, UMap.makeEmpty HashIdentity.Structural config)

    let private makeMetadataMap imperative packageNames assetGraph =
        let config = if imperative then Imperative else Functional
        List.fold
            (fun metadata packageName ->
                let (packageName, submap) = tryGenerateMetadataSubmap imperative packageName assetGraph
                UMap.add packageName submap metadata)
            (UMap.makeEmpty HashIdentity.Structural config)
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
        | Some size -> Some (v2 (single size.X) (single size.Y))
        | None -> None

    /// Forcibly get the texture size metadata of the given asset (throwing on failure).
    let getTextureSizeF assetTag metadata =
        Option.get (tryGetTextureSizeF assetTag metadata)

    /// Try to get the tile map metadata of the given asset.
    let tryGetTileMapMetadata (assetTag : TileMap AssetTag) metadata =
        match tryGetMetadata (AssetTag.generalize assetTag) metadata with
        | Some (TileMapMetadata (filePath, imageAssets, tmxMap)) -> Some (filePath, imageAssets, tmxMap)
        | None -> None
        | _ -> None

    /// Forcibly get the tile map metadata of the given asset (throwing on failure).
    let getTileMapMetadata assetTag metadata =
        Option.get (tryGetTileMapMetadata assetTag metadata)

    /// Try to get the static model of the given asset.
    let tryGetStaticModel (assetTag : TileMap AssetTag) metadata =
        match tryGetMetadata (AssetTag.generalize assetTag) metadata with
        | Some (StaticModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the static model of the given asset (throwing on failure).
    let getStaticModel assetTag metadata =
        Option.get (tryGetStaticModel assetTag metadata)

    /// Try to get the bounds metadata of the given asset.
    let tryGetBounds assetTag metadata =
        match tryGetStaticModel assetTag metadata with
        | Some model -> Some model.Bounds
        | None -> None

    /// Forcibly get the bounds metadata of the given asset (throwing on failure).
    let getBounds assetTag metadata =
        Option.get (tryGetBounds assetTag metadata)

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
    let make imperative assetGraph =
        let packageNames = AssetGraph.getPackageNames assetGraph
        let metadataMap = makeMetadataMap imperative packageNames assetGraph
        { MetadataMap = metadataMap }

    /// Make empty metadata.
    let makeEmpty imperative =
        let config = if imperative then Imperative else Functional
        { MetadataMap = UMap.makeEmpty HashIdentity.Structural config }

/// Stores metadata for game assets.
type Metadata = Metadata.Metadata