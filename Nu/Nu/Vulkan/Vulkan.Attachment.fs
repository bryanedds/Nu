// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    // TODO: DJL: implement format fallbacks (must not be ints for blit conversion), and don't forget to check for blit support (see TextureInternal.create).
    // NOTE: DJL: currently some "depth attachments" are actually real depth textures (i.e. used for depth testing) while others are just normal color ones.
    
    /// Create depth attachment.
    let private CreateDepthAttachment (optionalUsages, metadata, vkc) =
        let textureInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Nearest VkFilter.Nearest false
                Texture.MipmapNone (Texture.AttachmentDepth true) Texture.Texture2d optionalUsages
                Hl.D32f Hl.Depth metadata vkc
        Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = textureInternal }
    
    /// Create general-purpose color attachments with optional linear filters.
    let CreateColorAttachments (resolutionX, resolutionY, filtered, vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        let filter = if filtered then VkFilter.Linear else VkFilter.Nearest
        let colorInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge filter filter false
                Texture.MipmapNone (Texture.AttachmentColor true) Texture.Texture2d [|VkImageUsageFlags.TransferSrc|]
                Hl.Rgba16f Hl.Rgba metadata vkc
        let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = colorInternal }
        let depth = CreateDepthAttachment ([||], metadata, vkc)
        (color, depth)

    /// Update size of color attachments. Must be used every frame.
    let UpdateColorAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata depth vkc

    /// Destroy color attachments.
    let DestroyColorAttachments ((color : Texture.Texture, depth : Texture.Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc
    
    /// Create shadow texture array attachments.
    let CreateShadowTextureArrayAttachments (shadowResolutionX, shadowResolutionY, shadowResolutionZ, vkc) =
        let metadata = Texture.TextureMetadata.make shadowResolutionX shadowResolutionY
        let colorInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false
                Texture.MipmapNone (Texture.AttachmentColor true) (Texture.Texture2dArray shadowResolutionZ) [|VkImageUsageFlags.Sampled|]
                Hl.Rg32f Hl.Rg metadata vkc
        let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = colorInternal }
        let depth = CreateDepthAttachment ([||], metadata, vkc)
        (color, depth)
    
    /// Update size of shadow texture array attachments. Must be used every frame.
    let UpdateShadowTextureArrayAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata depth vkc

    /// Destroy shadow texture array attachments.
    let DestroyShadowTextureArrayAttachments ((color : Texture.Texture, depth : Texture.Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc
    
    /// Create shadow map attachments.
    let CreateShadowMapAttachments (shadowResolutionX, shadowResolutionY, vkc) =
        let metadata = Texture.TextureMetadata.make shadowResolutionX shadowResolutionY
        let colorInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false
                Texture.MipmapNone (Texture.AttachmentColor true) Texture.TextureCubeMap [|VkImageUsageFlags.Sampled|]
                Hl.R16f Hl.Red metadata vkc
        let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = colorInternal }
        let depth = CreateDepthAttachment ([||], metadata, vkc)
        (color, depth)

    /// Update size of shadow map attachments. Must be used every frame.
    let UpdateShadowMapAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata depth vkc

    /// Destroy shadow map attachments.
    let DestroyShadowMapAttachments ((color : Texture.Texture, depth : Texture.Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc
    
    /// Create shadow cascade array attachments.
    let CreateShadowCascadeArrayAttachments (shadowCascadeResolutionX, shadowCascadeResolutionY, shadowCascadeLevels, vkc) =
        let metadata = Texture.TextureMetadata.make shadowCascadeResolutionX shadowCascadeResolutionY
        let colorInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false
                Texture.MipmapNone (Texture.AttachmentColor true) (Texture.Texture2dArray shadowCascadeLevels) [|VkImageUsageFlags.Sampled|]
                Hl.Rg32f Hl.Rg metadata vkc
        let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = colorInternal }
        let depth = CreateDepthAttachment ([||], metadata, vkc)
        (color, depth)
    
    /// Update size of shadow cascade array attachments. Must be used every frame.
    let UpdateShadowCascadeArrayAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata depth vkc

    /// Destroy shadow cascade array attachments.
    let DestroyShadowCascadeArrayAttachments ((color : Texture.Texture, depth : Texture.Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc
    
    /// Create coloring attachments.
    let CreateColoringAttachments (resolutionX, resolutionY, vkc) =
        
        // create color attachment
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        let colorInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Nearest VkFilter.Nearest false
                Texture.MipmapNone (Texture.AttachmentColor true) Texture.Texture2d [|VkImageUsageFlags.Sampled|]
                Hl.Rgba16f Hl.Rgb metadata vkc // TODO: DJL: use Rgb16f with fallback to Rgba16f.
        let color = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = colorInternal }

        // create depth attachment (using linear filtering since it's the source for a down-sampling filter)
        let depthInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false
                Texture.MipmapNone (Texture.AttachmentColor true) Texture.Texture2d [|VkImageUsageFlags.Sampled|]
                Hl.R16f Hl.Red metadata vkc
        let depth = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = depthInternal }

        // fin
        (color, depth)
    
    /// Update size of coloring attachments. Must be used every frame.
    let UpdateColoringAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata depth vkc

    /// Destroy coloring attachments.
    let DestroyColoringAttachments ((color : Texture.Texture, depth : Texture.Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc