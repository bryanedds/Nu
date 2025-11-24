// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module CubeMap =

    /// Attempt to create a cube map from 6 files.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the included
    /// files can't be located.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, thread, vkc) =

        // load faces into cube map
        // NOTE: DJL: opengl seems to allow individual faces to differ in compression or maybe even size, but vulkan does not, so these are now determined by the first face.
        // TODO: DJL: maybe check that size and compression match?
        let mutable vulkanTextureOpt = None
        let mutable errorOpt = None
        let faceFilePaths = [|faceLeftFilePath; faceRightFilePath; faceBottomFilePath; faceTopFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".png") else faceFilePath // in case of PsdToPng
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".dds") else faceFilePath // in case of ConvertToDds
                match Texture.TryCreateTextureData (false, faceFilePath) with
                | Some textureData ->
                    match textureData with
                    | Texture.TextureData.TextureDataDotNet (metadata, bytes) ->
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None -> Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true Texture.Uncompressed metadata vkc
                        Texture.VulkanTexture.uploadArray metadata 0 i bytes thread vulkanTexture vkc
                    | Texture.TextureData.TextureDataMipmap (metadata, compressed, bytes, _) ->
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None ->
                                let compression = if compressed then Texture.ColorCompression else Texture.Uncompressed
                                Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true compression metadata vkc
                        Texture.VulkanTexture.uploadArray metadata 0 i bytes thread vulkanTexture vkc
                    | Texture.TextureData.TextureDataNative (metadata, bytesPtr, disposer) ->
                        use _ = disposer
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None -> Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true Texture.Uncompressed metadata vkc
                        Texture.VulkanTexture.upload metadata 0 i bytesPtr thread vulkanTexture vkc
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            // TODO: DJL: review error handling.
            let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; VulkanTexture = vulkanTextureOpt.Value }
            Right cubeMap
        | Some error ->
            match vulkanTextureOpt with
            | Some vulkanTexture -> Texture.VulkanTexture.destroy vulkanTexture vkc
            | None -> ()
            Left error
