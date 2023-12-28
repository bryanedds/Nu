// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open TiledSharp
open Prime

(* NOTE: The Metadata folder is placed after the subsystems folders in order to prevent subsystems from accessing
Metadata from another thread. *)

/// Metadata for an asset. Useful to describe various attributes of an asset without having the
/// full asset loaded into memory.
type Metadata =
    | RawMetadata
    | TextureMetadata of Vector2i
    | TileMapMetadata of string * (TmxTileset * Image AssetTag) array * TmxMap
    | StaticModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | SoundMetadata
    | SongMetadata

[<RequireQualifiedAccess>]
module Metadata =

    let mutable private MetadataPackages :
        UMap<string, UMap<string, DateTime * string * Metadata>> = UMap.makeEmpty StringComparer.Ordinal Imperative

    /// Thread-safe.
    let private tryGenerateRawMetadata (asset : Asset) =
        if File.Exists asset.FilePath
        then Some RawMetadata
        else None

    /// Thread-safe.
    let private tryGenerateTextureMetadata (asset : Asset) =
        if File.Exists asset.FilePath then
            let platform = Environment.OSVersion.Platform
            let fileExtension = PathF.GetExtensionLower asset.FilePath
            if  (platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows) &&
                fileExtension <> ".tga" then // NOTE: System.Drawing.Image does not seem to support .tga loading.
                // NOTE: System.Drawing.Image is not, AFAIK, available on non-Windows platforms, so we use a fast path here.
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                use image = Drawing.Image.FromStream (fileStream, false, false)
                Some (TextureMetadata (v2i image.Width image.Height))
            else
                // NOTE: System.Drawing.Image is not, AFAIK, available on non-Windows platforms, so we use a VERY slow path here.
                match OpenGL.Texture.TryCreateTextureData (Unchecked.defaultof<OpenGL.InternalFormat>, false, asset.FilePath) with
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

    /// Thread-safe.
    let private tryGenerateTileMapMetadata (asset : Asset) =
        try let tmxMap = TmxMap (asset.FilePath, true)
            let imageAssets = tmxMap.GetImageAssets asset.AssetTag.PackageName
            Some (TileMapMetadata (asset.FilePath, imageAssets, tmxMap))
        with exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            None

    /// Thread-safe.
    let private tryGenerateModelMetadata (asset : Asset) =
        if File.Exists asset.FilePath then
            let textureMemo = OpenGL.Texture.TextureMemo.make () // unused
            let assimpSceneMemo = OpenGL.Assimp.AssimpSceneMemo.make () // unused
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedModel (false, asset.FilePath, Unchecked.defaultof<_>, textureMemo, assimpSceneMemo) with
            | Right model ->
                if model.Animated
                then Some (AnimatedModelMetadata model)
                else Some (StaticModelMetadata model)
            | Left error ->
                let errorMessage = "Failed to load model '" + asset.FilePath + "' due to: " + error
                Log.trace errorMessage
                None
        else
            let errorMessage = "Failed to load model due to missing file '" + asset.FilePath + "'."
            Log.trace errorMessage
            None

    /// Thread-safe.
    let private tryGenerateAssetMetadata (asset : Asset) =
        let extension = PathF.GetExtensionLower asset.FilePath
        match extension with
        | ".raw" -> tryGenerateRawMetadata asset
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" -> tryGenerateTextureMetadata asset
        | ".tmx" -> tryGenerateTileMapMetadata asset
        | ".fbx" | ".dae" | ".obj" -> tryGenerateModelMetadata asset
        | ".wav" -> Some SoundMetadata
        | ".ogg" -> Some SongMetadata
        | _ -> None

    let private tryGenerateMetadataPackage config packageName assetGraph =
        match AssetGraph.tryCollectAssetsFromPackage None packageName assetGraph with
        | Right assetsCollected ->
            let package =
                assetsCollected |>
                List.map (fun asset ->
                    vsync {
                        match tryGenerateAssetMetadata asset with
                        | Some metadata ->
                            let lastWriteTime =
                                try File.GetLastWriteTime asset.FilePath
                                with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTime ()
                            return Some (asset.AssetTag.AssetName, (lastWriteTime, asset.FilePath, metadata))
                        | None -> return None }) |>
                Vsync.Parallel |>
                Vsync.RunSynchronously |>
                Array.definitize |>
                UMap.makeFromSeq HashIdentity.Structural config
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

        // reload outdated metadata from collected package
        let config = UMap.getConfig MetadataPackages
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->

            // reload outdated metadata from package
            for packageEntry in AssetGraph.getPackageDescriptors assetGraph do
                let metadataPackageName = packageEntry.Key
                match AssetGraph.tryCollectAssetsFromPackage None metadataPackageName assetGraph with
                | Right assetsCollected ->
                
                    // find or create metadata package
                    let metadataPackage =
                        match UMap.tryFind metadataPackageName MetadataPackages with
                        | Some metadataPackage -> metadataPackage
                        | None ->
                            let metadataPackage = UMap.makeEmpty StringComparer.Ordinal config
                            MetadataPackages <- UMap.add metadataPackageName metadataPackage MetadataPackages
                            metadataPackage

                    // categorize existing assets based on the required action
                    let assetsExisting = metadataPackage
                    let assetsToKeep = Dictionary ()
                    for assetEntry in assetsExisting do
                        let (assetName, (lastWriteTime, filePath, audioAsset)) = assetEntry
                        let lastWriteTime' =
                            try File.GetLastWriteTime filePath
                            with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTime ()
                        if lastWriteTime >= lastWriteTime' then
                            assetsToKeep.Add (assetName, (lastWriteTime, filePath, audioAsset))

                    // categorize assets to load
                    let assetsToLoad = HashSet ()
                    for asset in assetsCollected do
                        if not (assetsToKeep.ContainsKey asset.AssetTag.AssetName) then
                            assetsToLoad.Add asset |> ignore<bool>

                    // load assets
                    let assetsLoaded = Dictionary ()
                    for asset in assetsToLoad do
                        match tryGenerateAssetMetadata asset with
                        | Some audioAsset ->
                            let lastWriteTime =
                                try File.GetLastWriteTime asset.FilePath
                                with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTime ()
                            assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, audioAsset)
                        | None -> ()

                    // insert assets into package
                    let metadataPackage =
                        Seq.fold (fun metadataPackage (assetEntry : KeyValuePair<string, _>) ->
                            let assetName = assetEntry.Key
                            let (lastWriteTime, filePath, audioAsset) = assetEntry.Value
                            UMap.add assetName (lastWriteTime, filePath, audioAsset) metadataPackage)
                            metadataPackage (Seq.append assetsToKeep assetsLoaded)

                    // insert package
                    MetadataPackages <- UMap.add metadataPackageName metadataPackage MetadataPackages

                // handle errors
                | Left error ->
                    Log.info ("Metadata package regeneration failed due to: '" + error)
        | Left error ->
            Log.info ("Metadata regeneration failed due to: '" + error)

    /// Attempt to get the file path of the given asset.
    let tryGetFilePath (assetTag : AssetTag) =
        match UMap.tryFind assetTag.PackageName MetadataPackages with
        | Some package ->
            match UMap.tryFind assetTag.AssetName package with
            | Some (_, filePath, _) -> Some filePath
            | None -> None
        | None -> None

    /// Attempt to get the metadata of the given asset.
    let tryGetMetadata (assetTag : AssetTag) =
        match UMap.tryFind assetTag.PackageName MetadataPackages with
        | Some package ->
            match UMap.tryFind assetTag.AssetName package with
            | Some (_, _, asset) -> Some asset
            | None -> None
        | None -> None

    /// Attempt to get the texture metadata of the given asset.
    let tryGetTextureSize (assetTag : Image AssetTag) =
        match tryGetMetadata assetTag with
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
        match tryGetMetadata assetTag with
        | Some (TileMapMetadata (filePath, imageAssets, tmxMap)) -> Some (filePath, imageAssets, tmxMap)
        | None -> None
        | _ -> None

    /// Forcibly get the tile map metadata of the given asset (throwing on failure).
    let getTileMapMetadata assetTag =
        Option.get (tryGetTileMapMetadata assetTag)

    /// Attempt to get the static model metadata of the given asset.
    let tryGetStaticModelMetadata (assetTag : StaticModel AssetTag) =
        match tryGetMetadata assetTag with
        | Some (StaticModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the static model metadata of the given asset (throwing on failure).
    let getStaticModelMetadata assetTag =
        Option.get (tryGetStaticModelMetadata assetTag)

    /// Attempt to get the animated model metadata of the given asset.
    let tryGetAnimatedModelMetadata (assetTag : AnimatedModel AssetTag) =
        match tryGetMetadata assetTag with
        | Some (AnimatedModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the animated cmodel metadata of the given asset (throwing on failure).
    let getAnimatedModelMetadata assetTag =
        Option.get (tryGetAnimatedModelMetadata assetTag)

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