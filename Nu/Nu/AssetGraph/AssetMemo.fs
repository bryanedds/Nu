// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open Prime
open Nu

[<RequireQualifiedAccess>]
module AssetMemo =

    /// Memoize assets in parallel.
    let memoizeParallel is2d (assets : Asset seq) (textureMemo : OpenGL.Texture.TextureMemo) (cubeMapMemo : OpenGL.CubeMap.CubeMapMemo) (assimpSceneMemo : OpenGL.Assimp.AssimpSceneMemo) =

        // collect memoizable assets
        let textureAssets = List ()
        let cubeMapAssets = List ()
        let assimpSceneAssets = List ()
        for asset in assets do
            match PathF.GetExtensionLower asset.FilePath with
            | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" | ".dds" -> textureAssets.Add asset
            | ".cbm" -> cubeMapAssets.Add asset
            | ".fbx" | ".dae" | ".obj" -> assimpSceneAssets.Add asset
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
                    use assimp = new Assimp.AssimpContext ()
                    try let scene = assimp.ImportFile (assimpSceneAsset.FilePath, Constants.Assimp.PostProcessSteps)
                        scene.IndexDatasToMetadata () // avoid polluting memory with face data
                        return Right (assimpSceneAsset.FilePath, scene)
                    with exn ->
                        return Left ("Could not load assimp scene from '" + assimpSceneAsset.FilePath + "' due to: " + scstring exn) }]

        // upload loaded texture data sequentially
        for textureData in textureDataArray do
            match textureData with
            | Right (filePath, textureData) ->
                let texture =
                    if is2d then
                        let (metadata, textureId) = OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, false, false, textureData)
                        let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                        OpenGL.Texture.EagerTexture { TextureMetadata = metadata; TextureId = textureId; TextureHandle = textureHandle }
                    else
                        let (metadata, textureId) = OpenGL.Texture.CreateTextureGlFromData (OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true, OpenGL.Texture.BlockCompressable filePath, textureData)
                        let textureHandle = OpenGL.Texture.CreateTextureHandle textureId
                        let lazyTexture = new OpenGL.Texture.LazyTexture (filePath, metadata, textureId, textureHandle, OpenGL.TextureMinFilter.LinearMipmapLinear, OpenGL.TextureMagFilter.Linear, true)
                        textureMemo.LazyTextures.Enqueue lazyTexture
                        OpenGL.Texture.LazyTexture lazyTexture
                textureMemo.Textures.[filePath] <- texture
            | Left error -> Log.info error

        // run assimp scene loading ops
        for assimpScene in assimpSceneLoadOps |> Vsync.Parallel |> Vsync.RunSynchronously do
            match assimpScene with
            | Right (filePath, scene) -> assimpSceneMemo.AssimpScenes.[filePath] <- scene
            | Left error -> Log.info error

        // memoize cube maps directly
        for cubeMap in cubeMapAssets do
            match File.ReadAllLines cubeMap.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = PathF.GetDirectoryName cubeMap.FilePath
                let faceRightFilePath = dirPath + "/" + faceRightFilePath.Trim ()
                let faceLeftFilePath = dirPath + "/" + faceLeftFilePath.Trim ()
                let faceTopFilePath = dirPath + "/" + faceTopFilePath.Trim ()
                let faceBottomFilePath = dirPath + "/" + faceBottomFilePath.Trim ()
                let faceBackFilePath = dirPath + "/" + faceBackFilePath.Trim ()
                let faceFrontFilePath = dirPath + "/" + faceFrontFilePath.Trim ()
                let cubeMapMemoKey = (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)
                match OpenGL.CubeMap.TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
                | Right cubeMap -> cubeMapMemo.CubeMaps.[cubeMapMemoKey] <- cubeMap
                | Left error -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to: " + error)
            | _ -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line.")