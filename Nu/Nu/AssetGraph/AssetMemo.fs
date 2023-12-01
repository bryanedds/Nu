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
            match Path.GetExtension(asset.FilePath).ToLowerInvariant() with
            | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".tga" | ".tif" | ".tiff" -> textureAssets.Add asset
            | ".cbm" -> cubeMapAssets.Add asset
            | ".fbx" | ".dae" | ".obj" -> assimpSceneAssets.Add asset
            | _ -> ()

        // instantiate texture data loading tasks
        let textureDataLoadTasks =
            [for textureAsset in textureAssets do
                let task =
                    new OpenGL.Texture.TextureDataLoadTask (fun () ->
                        let internalFormat =
                            if is2d
                            then Constants.OpenGl.UncompressedTextureFormat
                            else AssetTag.inferInternalFormatFromAssetName textureAsset.AssetTag
                        match OpenGL.Texture.TryCreateTextureData (internalFormat, true, textureAsset.FilePath) with
                        | Some (metadata, textureData, disposable) -> Right (textureAsset.FilePath, metadata, textureData, disposable)
                        | None -> Left ("Error creating texture data from '" + textureAsset.FilePath + "'"))
                task.Start ()
                Vsync.AwaitTaskT task]

        // instantiate assimp scene loading tasks
        let assimpSceneLoadTasks =
            [for assimpSceneAsset in assimpSceneAssets do
                let task =
                    new OpenGL.Assimp.AssimpSceneLoadTask (fun () ->
                        use assimp = new Assimp.AssimpContext ()
                        try let scene = assimp.ImportFile (assimpSceneAsset.FilePath, Constants.Assimp.PostProcessSteps)
                            Right (assimpSceneAsset.FilePath, scene)
                        with exn ->
                            Left ("Could not load assimp scene from '" + assimpSceneAsset.FilePath + "' due to: " + scstring exn))
                task.Start ()
                Vsync.AwaitTaskT task]

        // run texture data loading tasks, handling results as soon as they come in (to minimize RAM utilization)
        for task in textureDataLoadTasks do
            match Vsync.RunSynchronously task with
            | Right (filePath, metadata, textureData, disposer) ->
                use _ = disposer
                let texture =
                    if is2d
                    then OpenGL.Texture.CreateTextureFromDataUnfiltered (metadata.TextureInternalFormat, metadata, textureData)
                    else OpenGL.Texture.CreateTextureFromDataFiltered (metadata.TextureInternalFormat, metadata, textureData)
                textureMemo.Textures.[filePath] <- (metadata, texture)
            | Left error -> Log.info error

        // run assimp scene loading tasks, handling results as soon as they come in (to minimize RAM utilization)
        for task in assimpSceneLoadTasks do
            match Vsync.RunSynchronously task with
            | Right (filePath, scene) -> assimpSceneMemo.AssimpScenes.[filePath] <- scene
            | Left error -> Log.info error

        // memoize cube maps directly
        for cubeMap in cubeMapAssets do
            match File.ReadAllLines cubeMap.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = Path.GetDirectoryName(cubeMap.FilePath).Replace("\\", "/")
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