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
    let memoizeParallel is2d (assets : obj Asset list) (textureMemo : OpenGL.Texture.TextureMemo) (cubeMapMemo : OpenGL.CubeMap.CubeMapMemo) (assimpSceneMemo : OpenGL.Assimp.AssimpSceneMemo) =

        // collect memoizable assets
        let textureAssets = List ()
        let cubeMapAssets = List ()
        let assimpSceneAssets = List ()
        for asset in assets do
            match Pathf.GetExtensionLower asset.FilePath with
            | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" -> textureAssets.Add asset
            | ".cbm" -> cubeMapAssets.Add asset
            | ".fbx" | ".dae" | ".obj" -> assimpSceneAssets.Add asset
            | _ -> ()

        // instantiate texture data loading ops
        let textureDataLoadOps =
            [for textureAsset in textureAssets do
                vsync
                    { let internalFormat =
                        if is2d
                        then Constants.OpenGL.UncompressedTextureFormat
                        else AssetTag.inferInternalFormatFromAssetName textureAsset.AssetTag
                      match OpenGL.Texture.TryCreateTextureData (internalFormat, true, textureAsset.FilePath) with
                      | Some (metadata, textureData, disposable) -> return Right (textureAsset.FilePath, metadata, textureData, disposable)
                      | None -> return Left ("Error creating texture data from '" + textureAsset.FilePath + "'") }]

        // run texture data loading ops
        let textureDataArray = textureDataLoadOps |> Vsync.Parallel |> Vsync.RunSynchronously

        // instantiate assimp scene loading ops
        let assimpSceneLoadOps =
            [for assimpSceneAsset in assimpSceneAssets do
                vsync
                    { use assimp = new Assimp.AssimpContext ()
                      try let scene = assimp.ImportFile (assimpSceneAsset.FilePath, Constants.Assimp.PostProcessSteps)
                          return Right (assimpSceneAsset.FilePath, scene)
                      with exn ->
                          return Left ("Could not load assimp scene from '" + assimpSceneAsset.FilePath + "' due to: " + scstring exn) }]

        // run texture data loading ops
        for textureData in textureDataArray do
            match textureData with
            | Right (filePath, metadata, textureData, _) ->
                let texture =
                    if is2d
                    then OpenGL.Texture.CreateTextureFromDataUnfiltered (metadata.TextureInternalFormat, metadata, textureData)
                    else OpenGL.Texture.CreateTextureFromDataFiltered (metadata.TextureInternalFormat, metadata, textureData)
                textureMemo.Textures.[filePath] <- (metadata, texture)
            | Left error -> Log.info error

        // create texture data dispose ops
        let textureDataDisposeOps =
            [for textureData in textureDataArray do
                vsync
                    { match textureData with
                      | Right (_, _, _, disposer) -> disposer.Dispose ()
                      | Left _ -> () }]

        // run texture data dispose ops
        textureDataDisposeOps |> Vsync.Parallel |> Vsync.RunSynchronously |> ignore<unit array>

        // run assimp scene loading op
        for assimpScene in assimpSceneLoadOps |> Vsync.Parallel |> Vsync.RunSynchronously do
            match assimpScene with
            | Right (filePath, scene) -> assimpSceneMemo.AssimpScenes.[filePath] <- scene
            | Left error -> Log.info error

        // memoize cube maps directly
        for cubeMap in cubeMapAssets do
            match File.ReadAllLines cubeMap.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = Pathf.GetDirectoryName cubeMap.FilePath
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