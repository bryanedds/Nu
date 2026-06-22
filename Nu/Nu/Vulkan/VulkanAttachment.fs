// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open Vortice.Vulkan
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    // NOTE: DJL: in the context of individual attachment textures, depth component attachments used for depth testing are named "z" to easily distinguish them
    // from color component attachments using the name "depth". Otherwise color and depth component textures are just called "color" and "depth" attachments,
    // as they are called when passed to Vulkan structures.
    
    /// Create color attachment.
    let private createColorAttachment textureType optionalUsages internalFormat pixelFormat resolutionX resolutionY (vkc : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureParallel =
            TextureParallel.create
                MipmapNone (AttachmentColor true) textureType optionalUsages
                (Hl.checkAttachmentFormat vkc.VkPhysicalDevice internalFormat) pixelFormat metadata vkc
        EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = textureParallel }
    
    /// Create depth attachment.
    let private createDepthAttachment optionalUsages resolutionX resolutionY (vkc : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureParallel =
            TextureParallel.create
                MipmapNone (AttachmentDepth true) Texture2d optionalUsages
                (Hl.checkAttachmentFormat vkc.VkPhysicalDevice D32f) Depth metadata vkc
        EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = textureParallel }
    
    /// Create general-purpose attachments.
    let createGeneralAttachments resolutionX resolutionY vkc =
        let color = createColorAttachment Texture2d [|VkImageUsageFlags.TransferSrc|] Rgba16f Rgba resolutionX resolutionY vkc
        let z = createDepthAttachment [||] resolutionX resolutionY vkc
        (color, z)

    /// Update size of general attachments. Must be used every frame.
    let updateGeneralAttachmentsSize resolutionX resolutionY (color, z) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy general attachments.
    let destroyGeneralAttachments (color : Texture, z : Texture) vkc =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow texture array attachments.
    let createShadowTextureArrayAttachments shadowResolutionX shadowResolutionY shadowResolutionZ vkc =
        let color = createColorAttachment (Texture2dArray shadowResolutionZ) [|VkImageUsageFlags.Sampled|] Rg32f Rg shadowResolutionX shadowResolutionY vkc
        let z = createDepthAttachment [||] shadowResolutionX shadowResolutionY vkc
        (color, z)
    
    /// Update size of shadow texture array attachments. Must be used every frame.
    let updateShadowTextureArrayAttachmentsSize resolutionX resolutionY (color, z) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow texture array attachments.
    let destroyShadowTextureArrayAttachments (color : Texture, z : Texture) vkc =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow map attachments.
    let createShadowMapAttachments shadowResolutionX shadowResolutionY vkc =
        let color = createColorAttachment TextureCubeMap [|VkImageUsageFlags.Sampled|] R16f Red shadowResolutionX shadowResolutionY vkc
        let z = createDepthAttachment [||] shadowResolutionX shadowResolutionY vkc
        (color, z)

    /// Update size of shadow map attachments. Must be used every frame.
    let updateShadowMapAttachmentsSize resolutionX resolutionY (color, z) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow map attachments.
    let destroyShadowMapAttachments (color : Texture, z : Texture) vkc =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow cascade array attachments.
    let createShadowCascadeArrayAttachments shadowCascadeResolutionX shadowCascadeResolutionY shadowCascadeLevels vkc =
        let color =
            createColorAttachment
                (Texture2dArray shadowCascadeLevels) [|VkImageUsageFlags.Sampled|]
                Rg32f Rg shadowCascadeResolutionX shadowCascadeResolutionY vkc
        let z = createDepthAttachment [||] shadowCascadeResolutionX shadowCascadeResolutionY vkc
        (color, z)
    
    /// Update size of shadow cascade array attachments. Must be used every frame.
    let updateShadowCascadeArrayAttachmentsSize resolutionX resolutionY (color, z) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow cascade array attachments.
    let destroyShadowCascadeArrayAttachments (color : Texture, z : Texture) vkc =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create geometry attachments.
    /// TODO: DJL: this z attachment is unused so maybe worth removing.
    let createGeometryAttachments resolutionX resolutionY vkc =
        let depth = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] R32f Red resolutionX resolutionY vkc
        let albedo = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba8 Rgba resolutionX resolutionY vkc
        let material = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba8 Rgba resolutionX resolutionY vkc
        let normalPlus = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba16f Rgba resolutionX resolutionY vkc
        let subdermalPlus = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba8 Rgba resolutionX resolutionY vkc
        let scatterPlus = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba8 Rgba resolutionX resolutionY vkc
        let clearCoatPlus = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgba16f Rgba resolutionX resolutionY vkc
        let z = createDepthAttachment [||] resolutionX resolutionY vkc
        (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z)
    
    /// Update size of geometry attachments. Must be used every frame.
    let updateGeometryAttachmentsSize resolutionX resolutionY (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata depth vkc
        Texture.updateSize metadata albedo vkc
        Texture.updateSize metadata material vkc
        Texture.updateSize metadata normalPlus vkc
        Texture.updateSize metadata subdermalPlus vkc
        Texture.updateSize metadata scatterPlus vkc
        Texture.updateSize metadata clearCoatPlus vkc
        Texture.updateSize metadata z vkc

    /// Destroy geometry attachments.
    let destroyGeometryAttachments
        (depth : Texture,
         albedo : Texture,
         material : Texture,
         normalPlus : Texture,
         subdermalPlus : Texture,
         scatterPlus : Texture,
         clearCoatPlus : Texture,
         z : Texture)
        vkc =
        depth.Destroy vkc
        albedo.Destroy vkc
        material.Destroy vkc
        normalPlus.Destroy vkc
        subdermalPlus.Destroy vkc
        scatterPlus.Destroy vkc
        clearCoatPlus.Destroy vkc
        z.Destroy vkc
    
    /// Create lighting attachment.
    let createLightingAttachment resolutionX resolutionY vkc =
        createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgb16f Rgb resolutionX resolutionY vkc

    /// Update size of lighting attachment. Must be used every frame.
    let updateLightingAttachmentSize resolutionX resolutionY lighting vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata lighting vkc

    /// Destroy lighting attachment.
    let destroyLightingAttachment (lighting : Texture) vkc =
        lighting.Destroy vkc
    
    /// Create coloring attachments.
    let createColoringAttachments resolutionX resolutionY vkc =
        let color = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] Rgb16f Rgb resolutionX resolutionY vkc
        let depth = createColorAttachment Texture2d [|VkImageUsageFlags.Sampled|] R16f Red resolutionX resolutionY vkc
        (color, depth)
    
    /// Update size of coloring attachments. Must be used every frame.
    let updateColoringAttachmentsSize resolutionX resolutionY (color, depth) vkc =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata depth vkc

    /// Destroy coloring attachments.
    let destroyColoringAttachments (color : Texture, depth : Texture) vkc =
        color.Destroy vkc
        depth.Destroy vkc