// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    // TODO: DJL: implement format fallbacks, and don't forget to check for linear filtering to enable blit.
    
    /// Create general-purpose color attachment with optional linear filters.
    let CreateColorAttachment (resolutionX, resolutionY, filtered, vkc) =
        let filter = if filtered then VkFilter.Linear else VkFilter.Nearest
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        let vulkanTexture = Texture.VulkanTexture.create Texture.Rgba filter filter false Texture.MipmapNone Texture.TextureAttachmentColor Hl.Rgba16f metadata vkc
        Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; VulkanTexture = vulkanTexture }
