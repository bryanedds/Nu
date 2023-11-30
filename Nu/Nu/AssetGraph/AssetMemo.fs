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

    /// Infer the internal format of an asset by its name.
    /// TODO: move this somewhere more relevant.
    let getInternalFormatFromAssetName (assetName : string) =
        if  assetName.EndsWith "_n" ||
            assetName.EndsWith "_u" ||
            assetName.EndsWith "Normal" ||
            assetName.EndsWith "Uncompressed" then
            Constants.OpenGl.UncompressedTextureFormat
        else Constants.OpenGl.CompressedColorTextureFormat

    /// Memoize assets in a parallel manner.
    let memoizeAssets (assets : obj Asset list) (textureMemo : OpenGL.Texture.TextureMemo) (cubeMapMemo : OpenGL.CubeMap.CubeMapMemo) (assimpSceneMemo : Assimp.AssimpSceneMemo) =

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

        let textureDataLoadTasks =
            [for textureAsset in textureAssets do
                let task =
                    new OpenGL.Texture.TextureDataLoadTask (fun () ->
                        let internalFormat = getInternalFormatFromAssetName textureAsset.AssetTag.AssetName
                        match OpenGL.Texture.TryCreateTextureData (internalFormat, true, textureAsset.FilePath) with
                        | Some (metadata, textureData, disposable) -> Right (textureAsset.FilePath, metadata, textureData, disposable)
                        | None -> Left ("Error creating texture data from '" + textureAsset.FilePath + "'"))
                task.Start ()
                Vsync.AwaitTaskT task]

        for task in textureDataLoadTasks do
            match Vsync.RunSynchronously task with
            | Right (filePath, metadata, textureData, disposer) ->
                use _ = disposer
                let texture = OpenGL.Texture.CreateTextureFromDataFiltered (Constants.OpenGl.CompressedColorTextureFormat, metadata, textureData)
                textureMemo.Textures.[filePath] <- (metadata, texture)
            | Left error -> Log.info error

        for cubeMap in cubeMapAssets do
            match File.ReadAllLines cubeMap.FilePath |> Array.filter (String.IsNullOrWhiteSpace >> not) with
            | [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|] ->
                let dirPath = Path.GetDirectoryName cubeMap.FilePath
                let faceRightFilePath = dirPath + "/" + faceRightFilePath |> fun str -> str.Trim ()
                let faceLeftFilePath = dirPath + "/" + faceLeftFilePath |> fun str -> str.Trim ()
                let faceTopFilePath = dirPath + "/" + faceTopFilePath |> fun str -> str.Trim ()
                let faceBottomFilePath = dirPath + "/" + faceBottomFilePath |> fun str -> str.Trim ()
                let faceBackFilePath = dirPath + "/" + faceBackFilePath |> fun str -> str.Trim ()
                let faceFrontFilePath = dirPath + "/" + faceFrontFilePath |> fun str -> str.Trim ()
                let cubeMapMemoKey = (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)
                match OpenGL.CubeMap.TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
                | Right cubeMap -> cubeMapMemo.CubeMaps.[cubeMapMemoKey] <- cubeMap
                | Left error -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to: " + error)
            | _ -> Log.info ("Could not load cube map '" + cubeMap.FilePath + "' due to requiring exactly 6 file paths with each file path on its own line.")

        let assimpSceneLoadTasks =
            [for assimpSceneAsset in assimpSceneAssets do
                let task =
                    new Assimp.AssimpSceneLoadTask (fun () ->
                        use assimp = new Assimp.AssimpContext ()
                        try let scene = assimp.ImportFile (assimpSceneAsset.FilePath, Constants.Assimp.PostProcessSteps)
                            Right (assimpSceneAsset.FilePath, scene)
                        with exn ->
                            Left ("Could not load assimp scene from '" + assimpSceneAsset.FilePath + "' due to: " + scstring exn))
                task.Start ()
                Vsync.AwaitTaskT task]

        for task in assimpSceneLoadTasks do
            match Vsync.RunSynchronously task with
            | Right (filePath, scene) -> assimpSceneMemo.AssimpScenes.[filePath] <- scene
            | Left error -> Log.info error