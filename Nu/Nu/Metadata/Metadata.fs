// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Buffers.Binary
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
    | TextureMetadata of OpenGL.Texture.TextureMetadata
    | TileMapMetadata of string * struct (TmxTileset * Image AssetTag) array * TmxMap // TODO: create a record for this.
    | StaticModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | AnimatedModelMetadata of OpenGL.PhysicallyBased.PhysicallyBasedModel
    | SoundMetadata
    | SongMetadata

[<RequireQualifiedAccess>]
module Metadata =

    let mutable private MetadataPackages : UMap<string, UMap<string, DateTimeOffset * string * Metadata>> =
        UMap.makeEmpty StringComparer.Ordinal Imperative

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
            Some (TileMapMetadata (asset.FilePath, imageAssets, tmxMap))
        with exn ->
            let errorMessage = "Failed to load TmxMap '" + asset.FilePath + "' due to: " + scstring exn
            Log.trace errorMessage
            None

    /// Thread-safe.
    let private tryGenerateModelMetadata (asset : Asset) =
        if File.Exists asset.FilePath then
            let textureMemo = OpenGL.Texture.TextureMemo None // unused. TODO: consider making this opt.
            let assimpSceneMemo = OpenGL.Assimp.AssimpSceneMemo.make () // unused. TODO: consider making this opt.
            match OpenGL.PhysicallyBased.TryCreatePhysicallyBasedModel (false, asset.FilePath, OpenGL.PhysicallyBased.PhysicallyBasedMaterial.empty, textureMemo, assimpSceneMemo) with
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
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" | ".dds" -> tryGenerateTextureMetadata asset
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
                                try DateTimeOffset (File.GetLastWriteTime asset.FilePath)
                                with exn -> Log.info ("Asset file write time read error due to: " + scstring exn); DateTimeOffset.MinValue.DateTime
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

    /// Determine that a given asset's metadata exists.
    let getMetadataExists (assetTag : AssetTag) =
        match UMap.tryFind assetTag.PackageName MetadataPackages with
        | Some package ->
            match UMap.tryFind assetTag.AssetName package with
            | Some (_, _, _) -> true
            | None -> false
        | None -> false

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

    /// Attempt to get the texture metadata of the given image.
    let tryGetTextureMetadata (image : Image AssetTag) =
        match tryGetMetadata image with
        | Some (TextureMetadata metadata) -> Some metadata
        | None -> None
        | _ -> None

    /// Forcibly get the texture metadata of the given image (throwing on failure).
    let getTextureMetadata image =
        Option.get (tryGetTextureMetadata image)

    /// Attempt to get the texture size of the given image.
    let tryGetTextureSize (image : Image AssetTag) =
        match tryGetTextureMetadata image with
        | Some metadata -> Some (v2i metadata.TextureWidth metadata.TextureHeight)
        | None -> None

    /// Forcibly get the texture size of the given image (throwing on failure).
    let getTextureSize image =
        Option.get (tryGetTextureSize image)

    /// Attempt to get the texture size of the given image.
    let tryGetTextureSizeF image =
        match tryGetTextureSize image with
        | Some size -> Some (v2 (single size.X) (single size.Y))
        | None -> None

    /// Forcibly get the texture size of the given image (throwing on failure).
    let getTextureSizeF image =
        Option.get (tryGetTextureSizeF image)

    /// Attempt to get the metadata of the given tile map.
    let tryGetTileMapMetadata (tileMap : TileMap AssetTag) =
        match tryGetMetadata tileMap with
        | Some (TileMapMetadata (filePath, imageAssets, tmxMap)) -> Some (filePath, imageAssets, tmxMap)
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given tile map (throwing on failure).
    let getTileMapMetadata tileMap =
        Option.get (tryGetTileMapMetadata tileMap)

    let private tryGetModelMetadata model =
        match tryGetMetadata model with
        | Some (StaticModelMetadata model) -> Some model
        | Some (AnimatedModelMetadata model) -> Some model
        | None -> None
        | _ -> None

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
    let tryGetStaticModelMetadata (staticModel : StaticModel AssetTag) =
        match tryGetMetadata staticModel with
        | Some (StaticModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given static model (throwing on failure).
    let getStaticModelMetadata staticModel =
        Option.get (tryGetStaticModelMetadata staticModel)

    /// Attempt to get the albedo image asset for the given material index and static model.
    let tryGetStaticModelAlbedoImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelAlbedoImage materialIndex staticModel

    /// Attempt to get the roughness image asset for the given material index and static model.
    let tryGetStaticModelRoughnessImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelRoughnessImage materialIndex staticModel

    /// Attempt to get the metallic image asset for the given material index and static model.
    let tryGetStaticModelMetallicImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelMetallicImage materialIndex staticModel

    /// Attempt to get the ambient occlusion image asset for the given material index and static model.
    let tryGetStaticModelAmbientOcclusionImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelAmbientOcclusionImage materialIndex staticModel

    /// Attempt to get the emission image asset for the given material index and static model.
    let tryGetStaticModelEmissionImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelEmissionImage materialIndex staticModel

    /// Attempt to get the normal image asset for the given material index and static model.
    let tryGetStaticModelNormalImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelNormalImage materialIndex staticModel

    /// Attempt to get the height image asset for the given material index and static model.
    let tryGetStaticModelHeightImage materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelHeightImage materialIndex staticModel

    /// Attempt to get the two-sided property for the given material index and static model.
    let tryGetStaticModelTwoSided materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelTwoSided materialIndex staticModel

    /// Attempt to get the 3d navigation shape for the given material index and static model.
    let tryGetStaticModelNavShape materialIndex (staticModel : StaticModel AssetTag) =
        tryGetModelNavShape materialIndex staticModel

    /// Attempt to get the metadata of the given animated model.
    let tryGetAnimatedModelMetadata (animatedModel : AnimatedModel AssetTag) =
        match tryGetMetadata animatedModel with
        | Some (AnimatedModelMetadata model) -> Some model
        | None -> None
        | _ -> None

    /// Forcibly get the metadata of the given animated model (throwing on failure).
    let getAnimatedModelMetadata animatedModel =
        Option.get (tryGetAnimatedModelMetadata animatedModel)

    /// Attempt to get the albedo image asset for the given material index and animated model.
    let tryGetAnimatedModelAlbedoImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelAlbedoImage materialIndex animatedModel

    /// Attempt to get the roughness image asset for the given material index and animated model.
    let tryGetAnimatedModelRoughnessImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelRoughnessImage materialIndex animatedModel

    /// Attempt to get the metallic image asset for the given material index and animated model.
    let tryGetAnimatedModelMetallicImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelMetallicImage materialIndex animatedModel

    /// Attempt to get the ambient occlusion image asset for the given material index and animated model.
    let tryGetAnimatedModelAmbientOcclusionImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelAmbientOcclusionImage materialIndex animatedModel

    /// Attempt to get the emission image asset for the given material index and animated model.
    let tryGetAnimatedModelEmissionImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelEmissionImage materialIndex animatedModel

    /// Attempt to get the normal image asset for the given material index and animated model.
    let tryGetAnimatedModelNormalImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelNormalImage materialIndex animatedModel

    /// Attempt to get the height image asset for the given material index and animated model.
    let tryGetAnimatedModelHeightImage materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelHeightImage materialIndex animatedModel

    /// Attempt to get the two-sided property for the given material index and animated model.
    let tryGetAnimatedModelTwoSided materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelTwoSided materialIndex animatedModel

    /// Attempt to get the 3d navigation shape property for the given material index and animated model.
    let tryGetAnimatedModelNavShape materialIndex (animatedModel : AnimatedModel AssetTag) =
        tryGetModelNavShape materialIndex animatedModel