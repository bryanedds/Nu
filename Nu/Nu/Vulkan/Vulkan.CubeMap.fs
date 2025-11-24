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
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, vkc) =

        //Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true Texture.Uncompressed metadata vkc

        ()