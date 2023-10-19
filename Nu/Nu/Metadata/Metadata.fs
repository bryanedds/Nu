// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Diagnostics
open System.IO
open TiledSharp
open Prime

(* NOTE: The Metadata folder is placed after the subsystems folders in order to prevent subsystems from accessing
Metadata from another thread. *)

/// Metadata for an asset. Useful to describe various attributes of an asset without having the
/// full asset loaded into memory.
type Metadata =
    | TextureMetadata of Vector2i
    | TileMapMetadata of string * (TmxTileset * Image AssetTag) array * TmxMap
    | StaticModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedStaticModel
    | SoundMetadata
    | SongMetadata
    | OtherMetadata of obj

[<RequireQualifiedAccess>]
module Metadata =

    (* Performance Timers *)
    let private TextureTimer = Stopwatch ()
    let private TmxTimer = Stopwatch ()
    let private FbxTimer = Stopwatch ()
    let private ObjTimer = Stopwatch ()
    let private WavTimer = Stopwatch ()
    let private OggTimer = Stopwatch ()

    let mutable private MetadataPackages :
        UMap<string, UMap<string, Metadata>> = UMap.makeEmpty StringComparer.Ordinal Imperative

    let private tryGenerateTextureMetadata asset =
        if File.Exists asset.FilePath then
            let platform = Environment.OSVersion.Platform
            let fileExtension = Path.GetExtension(asset.FilePath).ToLowerInvariant()
            if  (platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows) &&
                fileExtension <> ".tga" then // NOTE: System.Drawing.Image does not seem to support .tga loading.
                // NOTE: System.Drawing.Image is not, AFAIK, available on non-Windows platforms, so we use a fast path here.
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                use image = Drawing.Image.FromStream (fileStream, false, false)
                Some (TextureMetadata (v2i image.Width image.Height))
            else
                // NOTE: System.Drawing.Image is not, AFAIK, available on non-Windows platforms, so we use a VERY slow path here.
                match OpenGL.Texture.TryCreateImageData (Unchecked.defaultof<OpenGL.InternalFormat>, false, asset.FilePath) with
                | Some (metadata, _, disposer) ->
                    use _ = disposer
                    Some (TextureMetadata (v2i metadata.TextureWidth metadata.TextureHeight))
                | None ->
                    let errorMessage = "Failed to load texture metadata for '" + asset.FilePath + "."
                    Log.trace errorMessage
                    None
        else
            let errorMessage = "Failed to load texture due to missing file '" + asset.FilePath + "'."
            Log.trace errorMessage
            None

    let private tryGenerateTileMapMetadata asset =
        try let tmxMap = TmxMap (asset.FilePath, true)
            let imageAssets = tmxMap.GetImageAssets asset.AssetTag.PackageName
            Some (TileMapMetadata (asset.FilePath, imageAssets, tmxMap))
        with _ as exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            None

    let private tryGenerateStaticModelMetadata asset =
        if File.Exists asset.FilePath then
            let textureMemo = OpenGL.Texture.TextureMemo.make () // unused
            use assimp = new Assimp.AssimpContext ()
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedStaticModel (false, asset.FilePath, Unchecked.defaultof<_>, textureMemo, assimp) with
            | Right model -> Some (StaticModelMetadata model)
            | Left error ->
                let errorMessage = "Failed to load static model '" + asset.FilePath + "' due to: " + error
                Log.trace errorMessage
                None
        else
            let errorMessage = "Failed to load static model due to missing file '" + asset.FilePath + "'."
            Log.trace errorMessage
            None

    let private tryGenerateAssetMetadata asset =
        let extension = Path.GetExtension(asset.FilePath).ToLowerInvariant()
        let metadataOpt =
            match extension with
            | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" ->
                TextureTimer.Start ()
                let metadataOpt = tryGenerateTextureMetadata asset
                TextureTimer.Stop ()
                metadataOpt
            | ".tmx" ->
                TmxTimer.Start ()
                let metadataOpt = tryGenerateTileMapMetadata asset
                TmxTimer.Stop ()
                metadataOpt
            | ".fbx" ->
                FbxTimer.Start ()
                let metadataOpt = tryGenerateStaticModelMetadata asset
                FbxTimer.Stop ()
                metadataOpt
            | ".obj" ->
                ObjTimer.Start ()
                let metadataOpt = tryGenerateStaticModelMetadata asset
                ObjTimer.Stop ()
                metadataOpt
            | ".wav" ->
                WavTimer.Start ()
                let metadataOpt = Some SoundMetadata
                WavTimer.Stop ()
                metadataOpt
            | ".ogg" ->
                OggTimer.Start ()
                let metadataOpt = Some SongMetadata
                OggTimer.Stop ()
                metadataOpt
            | _ -> None
        match metadataOpt with
        | Some metadata -> Some (asset.AssetTag.AssetName, metadata)
        | None -> None

    let private tryGenerateMetadataPackage config packageName assetGraph =
        match AssetGraph.tryCollectAssetsFromPackage None packageName assetGraph with
        | Right assets ->
            let package = assets |> List.map tryGenerateAssetMetadata |> List.definitize |> UMap.makeFromSeq HashIdentity.Structural config
            (packageName, package)
        | Left error ->
            Log.info ("Could not load asset metadata for package '" + packageName + "' due to: " + error)
            (packageName, UMap.makeEmpty HashIdentity.Structural config)

    /// Generate metadata from the given asset graph.
    let generateMetadata imperative assetGraph =
        let config = if imperative then Imperative else Functional
        let packageNames = AssetGraph.getPackageNames assetGraph
        for packageName in packageNames do
            let (packageName, package) = tryGenerateMetadataPackage config packageName assetGraph
            MetadataPackages <- UMap.add packageName package MetadataPackages

    /// Regenerate metadata.
    let regenerateMetadata () =
        let packageNames = MetadataPackages |> Seq.map fst
        let config = UMap.getConfig MetadataPackages
        MetadataPackages <-
            Seq.fold
                (fun metadataPackages packageName ->
                    match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
                    | Right assetGraph ->
                        let (packageName, package) = tryGenerateMetadataPackage config packageName assetGraph
                        match UMap.tryFind packageName metadataPackages with
                        | Some packageExisting -> UMap.add packageName (UMap.addMany (seq package) packageExisting) metadataPackages
                        | None -> UMap.add packageName package metadataPackages
                    | Left error ->
                        Log.info ("Metadata package regeneration failed due to: '" + error)
                        metadataPackages)
                MetadataPackages
                packageNames

    /// Attempt to get the metadata of the given asset.
    let tryGetMetadata (assetTag : obj AssetTag) =
        match UMap.tryFind assetTag.PackageName MetadataPackages with
        | Some package ->
            match UMap.tryFind assetTag.AssetName package with
            | Some _ as asset -> asset
            | None -> None
        | None -> None

    /// Attempt to get the texture metadata of the given asset.
    let tryGetTextureSize (assetTag : Image AssetTag) =
        match tryGetMetadata (AssetTag.generalize assetTag) with
        | Some (TextureMetadata size) -> Some size
        | None -> None
        | _ -> None

    /// Forcibly get the texture size metadata of the given asset (throwing on failure).
    let getTextureSize assetTag =
        Option.get (tryGetTextureSize assetTag)

    /// Attempt to get the texture size metadata of the given asset.
    let tryGetTextureSizeF assetTag =
        match tryGetTextureSize assetTag with
        | Some size -> Some (v2 (single size.X) (single size.Y))
        | None -> None

    /// Forcibly get the texture size metadata of the given asset (throwing on failure).
    let getTextureSizeF assetTag =
        Option.get (tryGetTextureSizeF assetTag)

    /// Attempt to get the tile map metadata of the given asset.
    let tryGetTileMapMetadata (assetTag : TileMap AssetTag) =
        match tryGetMetadata (AssetTag.generalize assetTag) with
        | Some (TileMapMetadata (filePath, imageAssets, tmxMap)) -> Some (filePath, imageAssets, tmxMap)
        | None -> None
        | _ -> None

    /// Forcibly get the tile map metadata of the given asset (throwing on failure).
    let getTileMapMetadata assetTag =
        Option.get (tryGetTileMapMetadata assetTag)

    /// Attempt to get the static model metadata of the given asset.
    let tryGetStaticModelMetadata (assetTag : StaticModel AssetTag) =
        match tryGetMetadata (AssetTag.generalize assetTag) with
        | Some (StaticModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the static model metadata of the given asset (throwing on failure).
    let getStaticModelMetadata assetTag =
        Option.get (tryGetStaticModelMetadata assetTag)

    /// Get a copy of the metadata packages.
    let getMetadataPackages () =
        let map =
            MetadataPackages |>
            UMap.toSeq |>
            Seq.map (fun (packageName, map) -> (packageName, map |> UMap.toSeq |> Map.ofSeq)) |>
            Map.ofSeq
        map

    /// Attempt to get a copy of a metadata package with the given package name.
    let tryGetMetadataPackage packageName =
        match MetadataPackages.TryGetValue packageName with
        | (true, package) -> Some (package |> UMap.toSeq |> Map.ofSeq)
        | (false, _) -> None

    /// Get a map of all metadata's discovered assets.
    let getDiscoveredAssets metadata =
        let sources =
            getMetadataPackages metadata |>
            Map.map (fun _ metadata -> Map.toKeyList metadata)
        sources