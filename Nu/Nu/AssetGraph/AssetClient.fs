// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

/// Provides asset clients for direct usage.
type AssetClient (textureClient : OpenGL.Texture.TextureClient, cubeMapClient : OpenGL.CubeMap.CubeMapClient, sceneClient : OpenGL.PhysicallyBased.PhysicallyBasedSceneClient) =

    /// The texture client.
    member this.TextureClient = textureClient

    /// The cube map client.
    member this.CubeMapClient = cubeMapClient

    /// The physically-based scene client.
    member this.SceneClient = sceneClient

    /// Preload assets.
    member this.PreloadAssets (is2d, assets : Asset seq) =

        // collect loadable assets
        let textureAssets = List ()
        let cubeMapAssets = List ()
        let assimpSceneAssets = List ()
        for asset in assets do
            match PathF.GetExtensionLower asset.FilePath with
            | ImageExtension _ -> textureAssets.Add asset
            | CubeMapExtension _ -> cubeMapAssets.Add asset
            | ModelExtension _ -> assimpSceneAssets.Add asset
            | _ -> ()

        // instantiate texture data loading ops
        let textureDataLoadOps =
            [for textureAsset in textureAssets do
                vsync {
                    match OpenGL.Texture.TryCreateTextureData (not is2d, textureAsset.FilePath) with
                    | Some textureData -> return Right (textureAsset.FilePath, textureData)
                    | None -> return Left ("Error creating texture data from '" + textureAsset.FilePath + "'") }]

        // run texture data loading ops
        let textureDataArray = textureDataLoadOps |> Vsync.Parallel |> Vsync.RunSynchronously

        // instantiate assimp scene loading ops
        let assimpSceneLoadOps =
            [for assimpSceneAsset in assimpSceneAssets do
                vsync {
                    match AssimpContext.TryGetScene assimpSceneAsset.FilePath with
                    | Right scene -> return Right (assimpSceneAsset.FilePath, scene)
                    | Left error -> return Left error }]

        // upload loaded texture data sequentially
        for textureData in textureDataArray do
            match textureData with
            | Right (filePath, textureData) ->
                let texture =
                    if is2d then
                        let (metadata, textureId) =
                            if OpenGL.Texture.InferFiltered2d filePath
                            then OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, OpenGL.Texture.Uncompressed, textureData)
                            else OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, false, false, OpenGL.Texture.Uncompressed, textureData)
                        OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                    elif textureData.LazyLoadable then
                        let (metadata, textureId) = OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, OpenGL.Texture.InferCompression filePath, textureData)
                        let lazyTexture = new OpenGL.Texture.LazyTexture (filePath, metadata, textureId, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true)
                        textureClient.LazyTextureQueue.Enqueue lazyTexture
                        OpenGL.Texture.LazyTexture lazyTexture
                    else
                        Log.infoOnce "One or more textures for non-2D usage are not streamable; consider using the ConvertToDds refinement with them for more efficient loading."
                        let (metadata, textureId) = OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, true, OpenGL.Texture.InferCompression filePath, textureData)
                        OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId }
                textureClient.Textures.[filePath] <- texture
            | Left error -> Log.info error

        // run assimp scene loading ops
        for assimpScene in assimpSceneLoadOps |> Vsync.Parallel |> Vsync.RunSynchronously do
            match assimpScene with
            | Right (_, _) -> ()
            | Left error -> Log.info error

        // load cube maps directly
        for cubeMap in cubeMapAssets do
            match cubeMap.FilePath |> File.ReadAllLines |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = PathF.GetDirectoryName cubeMap.FilePath
                let faceRightFilePath = dirPath + "/" + faceRightFilePath.Trim ()
                let faceLeftFilePath = dirPath + "/" + faceLeftFilePath.Trim ()
                let faceTopFilePath = dirPath + "/" + faceTopFilePath.Trim ()
                let faceBottomFilePath = dirPath + "/" + faceBottomFilePath.Trim ()
                let faceBackFilePath = dirPath + "/" + faceBackFilePath.Trim ()
                let faceFrontFilePath = dirPath + "/" + faceFrontFilePath.Trim ()
                let cubeMapKey = (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)
                match OpenGL.CubeMap.TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
                | Right cubeMap -> cubeMapClient.CubeMaps.[cubeMapKey] <- cubeMap
                | Left error -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to: " + error)
            | _ -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line.")