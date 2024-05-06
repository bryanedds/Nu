// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Buffers.Binary
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open TiledSharp
open Prime

/// A tile map's metadata.
type TileMapMetadata =
    { TileMapImageAssets : struct (TmxTileset * Image AssetTag) array
      TileMap : TmxMap }

/// Metadata for an asset. Useful to describe various attributes of an asset without having the full asset loaded into
/// memory.
type Metadata =
    | RawMetadata
    | TextureMetadata of OpenGL.Texture.TextureMetadata
    | TileMapMetadata of TileMapMetadata
    | StaticModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | SoundMetadata
    | SongMetadata

[<RequireQualifiedAccess>]
module Metadata =

    let mutable private Initialized = false
    let mutable private AssetGraphOpt : AssetGraph option = None
    let private MetadataPackagesLoaded = ConcurrentDictionary<string, ConcurrentDictionary<string, DateTimeOffset * string * Metadata>> ()

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
            if fileExtension = ".dds" then
                let ddsHeader = Array.zeroCreate<byte> 20
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                fileStream.ReadExactly ddsHeader
                let height = BinaryPrimitives.ReadUInt32LittleEndian (ddsHeader.AsSpan (12, 4))
                let width = BinaryPrimitives.ReadUInt32LittleEndian (ddsHeader.AsSpan (16, 4))
                Some (TextureMetadata (OpenGL.Texture.TextureMetadata.make (int width) (int height)))
            elif fileExtension = ".tga" then
                let ddsHeader = Array.zeroCreate<byte> 16
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                fileStream.ReadExactly ddsHeader
                let width = BinaryPrimitives.ReadUInt16LittleEndian (ddsHeader.AsSpan (12, 2))
                let height = BinaryPrimitives.ReadUInt16LittleEndian (ddsHeader.AsSpan (14, 2))
                Some (TextureMetadata (OpenGL.Texture.TextureMetadata.make (int width) (int height)))
            elif platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
                use fileStream = new FileStream (asset.FilePath, FileMode.Open, FileAccess.Read, FileShare.Read)
                use image = Drawing.Image.FromStream (fileStream, false, false)
                Some (TextureMetadata (OpenGL.Texture.TextureMetadata.make image.Width image.Height))
            else
                Log.infoOnce "Slow path used to load texture metadata."
                match OpenGL.Texture.TryCreateTextureData (true, asset.FilePath) with
                | Some textureData ->
                    let metadata = textureData.Metadata
                    textureData.Dispose ()
                    Some (TextureMetadata metadata)
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
            Some (TileMapMetadata { TileMapImageAssets = imageAssets; TileMap = tmxMap })
        with exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            None

    /// Thread-safe.
    let private tryGenerateModelMetadata (asset : Asset) =
        if File.Exists asset.FilePath then
            let textureClient = OpenGL.Texture.TextureClient None // unused. TODO: consider making this opt.
            let sceneClient = OpenGL.PhysicallyBased.PhysicallyBasedSceneClient () // unused. TODO: consider making this opt.
            match sceneClient.TryCreatePhysicallyBasedModel (false, asset.FilePath, OpenGL.PhysicallyBased.PhysicallyBasedMaterial.empty, textureClient) with
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
        | RawExtension _ -> tryGenerateRawMetadata asset
        | ImageExtension _ -> tryGenerateTextureMetadata asset
        | TileMapExtension _ -> tryGenerateTileMapMetadata asset
        | ModelExtension _ -> tryGenerateModelMetadata asset
        | SoundExtension _ -> Some SoundMetadata
        | SongExtension _ -> Some SongMetadata
        | _ -> None

    /// Thread-safe.
    let private tryGenerateMetadataPackage packageName assetGraph =
        match AssetGraph.tryCollectAssetsFromPackage None packageName assetGraph with
        | Right assetsCollected ->
            let package =
                assetsCollected |>
                List.map (fun asset ->
                    vsync {
                        match tryGenerateAssetMetadata asset with
                        | Some metadata ->
                            let lastWriteTime =
                                try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                                with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                            return Some (asset.AssetTag.AssetName, (lastWriteTime, asset.FilePath, metadata))
                        | None -> return None }) |>
                Vsync.Parallel |>
                Vsync.RunSynchronously |>
                Array.definitize |>
                Map.ofArray |>
                Array.ofSeq |>
                fun assets -> ConcurrentDictionary (-1, assets, HashIdentity.Structural)
            package
        | Left error ->
            Log.info ("Could not load asset metadata for package '" + packageName + "' due to: " + error)
            ConcurrentDictionary (-1, Seq.empty, HashIdentity.Structural)

    /// Attempt to load the given metadata package.
    /// Thread-safe.
    let loadMetadataPackage packageName =
        match AssetGraphOpt with
        | Some assetGraph ->
            if  assetGraph.PackageDescriptors.ContainsKey packageName &&
                not (MetadataPackagesLoaded.ContainsKey packageName) then
                let package = tryGenerateMetadataPackage packageName assetGraph
                MetadataPackagesLoaded.TryAdd (packageName, package) |> ignore<bool>
        | None -> ()

    /// Attempt to unload the given metadata package.
    /// Thread-safe.
    let unloadMetadataPackage packageName =
        MetadataPackagesLoaded.Remove packageName

    /// Reload metadata.
    /// Thread-safe.
    let reloadMetadata () =

        // reload outdated metadata from collected package
        match AssetGraph.tryMakeFromFile Assets.Global.AssetGraphFilePath with
        | Right assetGraph ->
            for packageEntry in assetGraph.PackageDescriptors do
                let metadataPackageName = packageEntry.Key
                match MetadataPackagesLoaded.TryGetValue metadataPackageName with
                | (true, metadataPackage) ->
                    match AssetGraph.tryCollectAssetsFromPackage None metadataPackageName assetGraph with
                    | Right assetsCollected ->

                        // categorize existing assets based on the required action
                        let assetsExisting = metadataPackage
                        let assetsToKeep = Dictionary ()
                        for assetEntry in assetsExisting do
                            let assetName = assetEntry.Key
                            let (lastWriteTime, filePath, audioAsset) = assetEntry.Value
                            let lastWriteTime' =
                                try DateTimeOffset (File.GetLastWriteTime filePath)
                                with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
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
                                    try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                                    with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
                                assetsLoaded.[asset.AssetTag.AssetName] <- (lastWriteTime, asset.FilePath, audioAsset)
                            | None -> ()

                        // insert assets into package
                        for assetEntry in Seq.append assetsToKeep assetsLoaded do
                            let assetName = assetEntry.Key
                            let (lastWriteTime, filePath, audioAsset) = assetEntry.Value
                            metadataPackage.TryAdd (assetName, (lastWriteTime, filePath, audioAsset)) |> ignore<bool>

                        // insert package
                        MetadataPackagesLoaded.TryAdd (metadataPackageName, metadataPackage) |> ignore<bool>

                    // handle errors if any
                    | Left error -> Log.info ("Metadata package regeneration failed due to: '" + error)
                | (false, _) -> ()
        | Left error -> Log.info ("Metadata regeneration failed due to: '" + error)

    /// Attempt to get the metadata package containing the given asset, attempt to load it if it isn't already.
    /// Thread-safe.
    let private tryGetMetadataPackage (assetTag : AssetTag) =
        match MetadataPackagesLoaded.TryGetValue assetTag.PackageName with
        | (true, package) -> Some package
        | (false, _) ->
            match AssetGraphOpt with
            | Some assetGraph ->
                if assetGraph.PackageDescriptors.ContainsKey assetTag.PackageName then
                    Log.info ("Loading Metadata package '" + assetTag.PackageName + "' for asset '" + assetTag.AssetName + "' on the fly.")
                    let package = tryGenerateMetadataPackage assetTag.PackageName assetGraph
                    MetadataPackagesLoaded.TryAdd (assetTag.PackageName, package) |> ignore<bool>
                    Some package
                else None
            | None -> None

    /// Get the metadate packages that have been loaded.
    /// NOTE: this is a potentially expensive call as the tree of ConcurrentDictionaries must be copied to avoid
    /// invalidating enumeration in a multi-threaded context.
    /// Thread-safe.
    let getMetadataPackagesLoaded () =
        MetadataPackagesLoaded.ToArray () |>
        Array.map (fun packageEntry -> KeyValuePair (packageEntry.Key, packageEntry.Value.ToArray ()))

    /// Determine that a given asset's metadata exists.
    /// Thread-safe.
    let getMetadataExists (assetTag : AssetTag) =
        match tryGetMetadataPackage assetTag with
        | Some package ->
            match package.TryGetValue assetTag.AssetName with
            | (true, (_, _, _)) -> true
            | (false, _) -> false
        | None -> false

    /// Attempt to get the file path of the given asset.
    /// Thread-safe.
    let tryGetFilePath (assetTag : AssetTag) =
        match tryGetMetadataPackage assetTag with
        | Some package ->
            match package.TryGetValue assetTag.AssetName with
            | (true, (_, filePath, _)) -> Some filePath
            | (false, _) -> None
        | None -> None

    /// Attempt to get the metadata of the given asset.
    /// Thread-safe.
    let tryGetMetadata (assetTag : AssetTag) =
        match tryGetMetadataPackage assetTag with
        | Some package ->
            match package.TryGetValue assetTag.AssetName with
            | (true, (_, _, asset)) -> Some asset
            | (false, _) -> None
        | None -> None

    /// Attempt to get the texture metadata of the given image.
    /// Thread-safe.
    let tryGetTextureMetadata (image : Image AssetTag) =
        match tryGetMetadata image with
        | Some (TextureMetadata metadata) -> Some metadata
        | None -> None
        | _ -> None

    /// Forcibly get the texture metadata of the given image (throwing on failure).
    /// Thread-safe.
    let getTextureMetadata image =
        Option.get (tryGetTextureMetadata image)

    /// Attempt to get the texture size of the given image.
    /// Thread-safe.
    let tryGetTextureSize (image : Image AssetTag) =
        match tryGetTextureMetadata image with
        | Some metadata -> Some (v2i metadata.TextureWidth metadata.TextureHeight)
        | None -> None

    /// Forcibly get the texture size of the given image (throwing on failure).
    /// Thread-safe.
    let getTextureSize image =
        Option.get (tryGetTextureSize image)

    /// Attempt to get the texture size of the given image.
    /// Thread-safe.
    let tryGetTextureSizeF image =
        match tryGetTextureSize image with
        | Some size -> Some (v2 (single size.X) (single size.Y))
        | None -> None

    /// Forcibly get the texture size of the given image (throwing on failure).
    /// Thread-safe.
    let getTextureSizeF image =
        Option.get (tryGetTextureSizeF image)

    /// Attempt to get the metadata of the given tile map.
    /// Thread-safe.
    let tryGetTileMapMetadata (tileMap : TileMap AssetTag) =
        match tryGetMetadata tileMap with
        | Some (TileMapMetadata tileMapMetadata) -> Some tileMapMetadata
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given tile map (throwing on failure).
    /// Thread-safe.
    let getTileMapMetadata tileMap =
        Option.get (tryGetTileMapMetadata tileMap)

    /// Thread-safe.
    let private tryGetModelMetadata model =
        match tryGetMetadata model with
        | Some (StaticModelMetadata model) -> Some model
        | Some (AnimatedModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Thread-safe.
    let private tryGetModelAlbedoImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, albedoTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.BaseColor, 0)
                let mutable (_, albedoTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.Diffuse, 0)
                let albedoTextureSlotFilePath =
                    if isNull albedoTextureSlotA.FilePath then
                        if isNull albedoTextureSlotB.FilePath then ""
                        else albedoTextureSlotB.FilePath
                    else albedoTextureSlotA.FilePath
                let assetName = PathF.GetFileNameWithoutExtension albedoTextureSlotFilePath
                let image = asset model.PackageName assetName
                if getMetadataExists image then Some image else None
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelRoughnessImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, roughnessTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Roughness, 0)
                if isNull roughnessTextureSlot.FilePath then roughnessTextureSlot.FilePath <- "" // ensure not null
                let assetName = PathF.GetFileNameWithoutExtension roughnessTextureSlot.FilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =   albedoImage.AssetName
                        let has_bc =            albedoAssetName.Contains "_bc"
                        let has_d =             albedoAssetName.Contains "_d"
                        let hasBaseColor =      albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =        albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =         albedoAssetName.Contains "Albedo"
                        let g_mAsset =          asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g_m")                   elif has_d then albedoAssetName.Replace ("_d", "_g_m")                  else "")
                        let g_m_aoAsset =       asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g_m_ao")                elif has_d then albedoAssetName.Replace ("_d", "_g_m_ao")               else "")
                        let gAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g")                     elif has_d then albedoAssetName.Replace ("_d", "_g")                    else "")
                        let sAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_s")                     elif has_d then albedoAssetName.Replace ("_d", "_s")                    else "")
                        let rmAsset =           asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "RM")         elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "RM")          elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "RM")           else "")
                        let rmaAsset =          asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "RMA")        elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "RMA")         elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "RMA")          else "")
                        let roughnessAsset =    asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Roughness")  elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Roughness")   elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Roughness")    else "")
                        if getMetadataExists gAsset then Some gAsset
                        elif getMetadataExists sAsset then Some sAsset
                        elif getMetadataExists g_mAsset then Some g_mAsset
                        elif getMetadataExists g_m_aoAsset then Some g_m_aoAsset
                        elif getMetadataExists roughnessAsset then Some roughnessAsset
                        elif getMetadataExists rmAsset then Some rmAsset
                        elif getMetadataExists rmaAsset then Some rmaAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelMetallicImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, metallicTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Metalness, 0)
                if isNull metallicTextureSlot.FilePath then metallicTextureSlot.FilePath <- "" // ensure not null
                let assetName = PathF.GetFileNameWithoutExtension metallicTextureSlot.FilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =   albedoImage.AssetName
                        let has_bc =            albedoAssetName.Contains "_bc"
                        let has_d =             albedoAssetName.Contains "_d"
                        let hasBaseColor =      albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =        albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =         albedoAssetName.Contains "Albedo"
                        let mAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_m")                     elif has_d then albedoAssetName.Replace ("_d", "_m")                    else "")
                        let g_mAsset =          asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g_m")                   elif has_d then albedoAssetName.Replace ("_d", "_g_m")                  else "")
                        let g_m_aoAsset =       asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g_m_ao")                elif has_d then albedoAssetName.Replace ("_d", "_g_m_ao")               else "")
                        let rmAsset =           asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "RM")         elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "RM")          elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "RM")           else "")
                        let rmaAsset =          asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "RMA")        elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "RMA")         elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "RMA")          else "")
                        let metallicAsset =     asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Metallic")   elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Metallic")    elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Metallic")     else "")
                        let metalnessAsset =    asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Metalness")  elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Metalness")   elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Metalness")    else "")
                        if getMetadataExists mAsset then Some mAsset
                        elif getMetadataExists g_mAsset then Some g_mAsset
                        elif getMetadataExists g_m_aoAsset then Some g_m_aoAsset
                        elif getMetadataExists metallicAsset then Some metallicAsset
                        elif getMetadataExists metalnessAsset then Some metalnessAsset
                        elif getMetadataExists rmAsset then Some rmAsset
                        elif getMetadataExists rmaAsset then Some rmaAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelAmbientOcclusionImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, ambientOcclusionTextureSlotA) = material.GetMaterialTexture (Assimp.TextureType.Ambient, 0)
                let mutable (_, ambientOcclusionTextureSlotB) = material.GetMaterialTexture (Assimp.TextureType.AmbientOcclusion, 0)
                let ambientOcclusionTextureSlotFilePath =
                    if isNull ambientOcclusionTextureSlotA.FilePath then
                        if isNull ambientOcclusionTextureSlotB.FilePath then ""
                        else ambientOcclusionTextureSlotB.FilePath
                    else ambientOcclusionTextureSlotA.FilePath
                let assetName = PathF.GetFileNameWithoutExtension ambientOcclusionTextureSlotFilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =       albedoImage.AssetName
                        let has_bc =                albedoAssetName.Contains "_bc"
                        let has_d =                 albedoAssetName.Contains "_d"
                        let hasBaseColor =          albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =            albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =             albedoAssetName.Contains "Albedo"
                        let g_m_aoAsset =           asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_g_m_ao")                        elif has_d then albedoAssetName.Replace ("_d", "_g_m_ao")                       else "")
                        let aoAsset =               asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_ao")                            elif has_d then albedoAssetName.Replace ("_d", "_ao")                           else "")
                        let rmaAsset =              asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "RMA")                elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "RMA")                 elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "RMA")              else "")
                        let ambientOcclusionAsset = asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "AmbientOcclusion")   elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "AmbientOcclusion")    elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "AmbientOcclusion") else "")
                        let aoAsset' =              asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "AO")                 elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "AO")                  elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "AO")               else "")
                        if getMetadataExists aoAsset then Some aoAsset
                        elif getMetadataExists g_m_aoAsset then Some g_m_aoAsset
                        elif getMetadataExists ambientOcclusionAsset then Some ambientOcclusionAsset
                        elif getMetadataExists aoAsset' then Some aoAsset'
                        elif getMetadataExists rmaAsset then Some rmaAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelEmissionImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, emissionTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Emissive, 0)
                if isNull emissionTextureSlot.FilePath then emissionTextureSlot.FilePath <- "" // ensure not null
                let assetName = PathF.GetFileNameWithoutExtension emissionTextureSlot.FilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =   albedoImage.AssetName
                        let has_bc =            albedoAssetName.Contains "_bc"
                        let has_d =             albedoAssetName.Contains "_d"
                        let hasBaseColor =      albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =        albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =         albedoAssetName.Contains "Albedo"
                        let eAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_e")                     elif has_d then albedoAssetName.Replace ("_d", "_e")                    else "")
                        let emissionAsset =     asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Emission")   elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Emission")    elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Emission") else "")
                        if getMetadataExists eAsset then Some eAsset
                        elif getMetadataExists emissionAsset then Some emissionAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelNormalImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, normalTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
                if isNull normalTextureSlot.FilePath then normalTextureSlot.FilePath <- "" // ensure not null
                let assetName = PathF.GetFileNameWithoutExtension normalTextureSlot.FilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =   albedoImage.AssetName
                        let has_bc =            albedoAssetName.Contains "_bc"
                        let has_d =             albedoAssetName.Contains "_d"
                        let hasBaseColor =      albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =        albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =         albedoAssetName.Contains "Albedo"
                        let nAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_n")                 elif has_d then albedoAssetName.Replace ("_d", "_n")                else "")
                        let normalAsset =       asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Normal") elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Normal")  elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Normal") else "")
                        if getMetadataExists nAsset then Some nAsset
                        elif getMetadataExists normalAsset then Some normalAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelHeightImage materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                let mutable (_, heightTextureSlot) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
                if isNull heightTextureSlot.FilePath then heightTextureSlot.FilePath <- "" // ensure not null
                let assetName = PathF.GetFileNameWithoutExtension heightTextureSlot.FilePath
                let image = asset model.PackageName assetName
                if not (getMetadataExists image) then
                    match tryGetModelAlbedoImage materialIndex model with
                    | Some albedoImage ->
                        let albedoAssetName =   albedoImage.AssetName
                        let has_bc =            albedoAssetName.Contains "_bc"
                        let has_d =             albedoAssetName.Contains "_d"
                        let hasBaseColor =      albedoAssetName.Contains "BaseColor"
                        let hasDiffuse =        albedoAssetName.Contains "Diffuse"
                        let hasAlbedo =         albedoAssetName.Contains "Albedo"
                        let hAsset =            asset albedoImage.PackageName (if has_bc then albedoAssetName.Replace ("_bc", "_h")                 elif has_d then albedoAssetName.Replace ("_d", "_h")                else "")
                        let heightAsset =       asset albedoImage.PackageName (if hasBaseColor then albedoAssetName.Replace ("BaseColor", "Height") elif hasDiffuse then albedoAssetName.Replace ("Diffuse", "Height")  elif hasAlbedo  then albedoAssetName.Replace ("Albedo", "Height") else "")
                        if getMetadataExists hAsset then Some hAsset
                        elif getMetadataExists heightAsset then Some heightAsset
                        else None
                    | None -> None
                else Some image
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelTwoSided materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                match material.IgnoreLightMapsOpt with
                | Some ignoreLightMaps -> Some ignoreLightMaps
                | None -> None
            | Some _ | None -> None
        | None -> None

    /// Thread-safe.
    let private tryGetModelNavShape materialIndex model =
        match tryGetModelMetadata model with
        | Some modelMetadata ->
            match modelMetadata.SceneOpt with
            | Some scene when materialIndex >= 0 && materialIndex < scene.Materials.Count ->
                let material = scene.Materials.[materialIndex]
                match material.NavShapeOpt with
                | Some shape -> Some shape
                | None -> None
            | Some _ | None -> None
        | None -> None

    /// Attempt to get the metadata of the given static model.
    /// Thread-safe.
    let tryGetStaticModelMetadata (staticModel : StaticModel AssetTag) =
        match tryGetMetadata staticModel with
        | Some (StaticModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given static model (throwing on failure).
    /// Thread-safe.
    let getStaticModelMetadata staticModel =
        Option.get (tryGetStaticModelMetadata staticModel)

    /// Attempt to get the albedo image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelAlbedoImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelAlbedoImage materialIndex staticModel

    /// Attempt to get the roughness image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelRoughnessImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelRoughnessImage materialIndex staticModel

    /// Attempt to get the metallic image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelMetallicImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelMetallicImage materialIndex staticModel

    /// Attempt to get the ambient occlusion image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelAmbientOcclusionImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelAmbientOcclusionImage materialIndex staticModel

    /// Attempt to get the emission image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelEmissionImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelEmissionImage materialIndex staticModel

    /// Attempt to get the normal image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelNormalImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelNormalImage materialIndex staticModel

    /// Attempt to get the height image asset for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelHeightImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelHeightImage materialIndex staticModel

    /// Attempt to get the two-sided property for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelTwoSided materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelTwoSided materialIndex staticModel

    /// Attempt to get the 3d navigation shape for the given material index and static model.
    /// Thread-safe.
    let tryGetStaticModelNavShape materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelNavShape materialIndex staticModel

    /// Attempt to get the metadata of the given animated model.
    /// Thread-safe.
    let tryGetAnimatedModelMetadata (animatedModel : AnimatedModel AssetTag) =
        match tryGetMetadata animatedModel with
        | Some (AnimatedModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given animated model (throwing on failure).
    /// Thread-safe.
    let getAnimatedModelMetadata animatedModel =
        Option.get (tryGetAnimatedModelMetadata animatedModel)

    /// Attempt to get the albedo image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelAlbedoImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelAlbedoImage materialIndex animatedModel

    /// Attempt to get the roughness image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelRoughnessImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelRoughnessImage materialIndex animatedModel

    /// Attempt to get the metallic image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelMetallicImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelMetallicImage materialIndex animatedModel

    /// Attempt to get the ambient occlusion image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelAmbientOcclusionImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelAmbientOcclusionImage materialIndex animatedModel

    /// Attempt to get the emission image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelEmissionImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelEmissionImage materialIndex animatedModel

    /// Attempt to get the normal image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelNormalImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelNormalImage materialIndex animatedModel

    /// Attempt to get the height image asset for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelHeightImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelHeightImage materialIndex animatedModel

    /// Attempt to get the two-sided property for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelTwoSided materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelTwoSided materialIndex animatedModel

    /// Attempt to get the 3d navigation shape property for the given material index and animated model.
    /// Thread-safe.
    let tryGetAnimatedModelNavShape materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelNavShape materialIndex animatedModel

    /// Initialize metadata to use the given imperative value and asset graph.
    let init assetGraph =
        if not Initialized then
            AssetGraphOpt <- Some assetGraph
            Initialized <- true