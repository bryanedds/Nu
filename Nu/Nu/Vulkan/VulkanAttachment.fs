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
    let private CreateColorAttachment (textureType, optionalUsages, internalFormat, pixelFormat, resolutionX, resolutionY, vkc : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureParallel =
            TextureParallel.create
                MipmapNone (AttachmentColor true) textureType optionalUsages
                (Hl.CheckAttachmentFormat (vkc.VkPhysicalDevice, internalFormat)) pixelFormat metadata vkc
        EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = textureParallel }
    
    /// Create depth attachment.
    let private CreateDepthAttachment (optionalUsages, resolutionX, resolutionY, vkc : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureParallel =
            TextureParallel.create
                MipmapNone (AttachmentDepth true) Texture2d optionalUsages
                (Hl.CheckAttachmentFormat (vkc.VkPhysicalDevice, D32f)) Depth metadata vkc
        EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = textureParallel }
    
    /// Create general-purpose attachments.
    let CreateGeneralAttachments (resolutionX, resolutionY, vkc) =
        let color = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.TransferSrc|], Rgba16f, Rgba, resolutionX, resolutionY, vkc)
        let z = CreateDepthAttachment ([||], resolutionX, resolutionY, vkc)
        (color, z)

    /// Update size of general attachments. Must be used every frame.
    let UpdateGeneralAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy general attachments.
    let DestroyGeneralAttachments ((color : Texture, z : Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow texture array attachments.
    let CreateShadowTextureArrayAttachments (shadowResolutionX, shadowResolutionY, shadowResolutionZ, vkc) =
        let color = CreateColorAttachment ((Texture2dArray shadowResolutionZ), [|VkImageUsageFlags.Sampled|], Rg32f, Rg, shadowResolutionX, shadowResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowResolutionX, shadowResolutionY, vkc)
        (color, z)
    
    /// Update size of shadow texture array attachments. Must be used every frame.
    let UpdateShadowTextureArrayAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow texture array attachments.
    let DestroyShadowTextureArrayAttachments ((color : Texture, z : Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow map attachments.
    let CreateShadowMapAttachments (shadowResolutionX, shadowResolutionY, vkc) =
        let color = CreateColorAttachment (TextureCubeMap, [|VkImageUsageFlags.Sampled|], R16f, Red, shadowResolutionX, shadowResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowResolutionX, shadowResolutionY, vkc)
        (color, z)

    /// Update size of shadow map attachments. Must be used every frame.
    let UpdateShadowMapAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow map attachments.
    let DestroyShadowMapAttachments ((color : Texture, z : Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow cascade array attachments.
    let CreateShadowCascadeArrayAttachments (shadowCascadeResolutionX, shadowCascadeResolutionY, shadowCascadeLevels, vkc) =
        let color =
            CreateColorAttachment
                ((Texture2dArray shadowCascadeLevels), [|VkImageUsageFlags.Sampled|],
                 Rg32f, Rg, shadowCascadeResolutionX, shadowCascadeResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowCascadeResolutionX, shadowCascadeResolutionY, vkc)
        (color, z)
    
    /// Update size of shadow cascade array attachments. Must be used every frame.
    let UpdateShadowCascadeArrayAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata z vkc

    /// Destroy shadow cascade array attachments.
    let DestroyShadowCascadeArrayAttachments ((color : Texture, z : Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create geometry attachments.
    /// TODO: DJL: this z attachment is unused so maybe worth removing.
    let CreateGeometryAttachments (resolutionX, resolutionY, vkc) =
        let depth = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], R32f, Red, resolutionX, resolutionY, vkc)
        let albedo = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba8, Rgba, resolutionX, resolutionY, vkc)
        let material = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba8, Rgba, resolutionX, resolutionY, vkc)
        let normalPlus = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba16f, Rgba, resolutionX, resolutionY, vkc)
        let subdermalPlus = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba8, Rgba, resolutionX, resolutionY, vkc)
        let scatterPlus = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba8, Rgba, resolutionX, resolutionY, vkc)
        let clearCoatPlus = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgba16f, Rgba, resolutionX, resolutionY, vkc)
        let z = CreateDepthAttachment ([||], resolutionX, resolutionY, vkc)
        (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z)
    
    /// Update size of geometry attachments. Must be used every frame.
    let UpdateGeometryAttachmentsSize (resolutionX, resolutionY, (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z), vkc) =
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
    let DestroyGeometryAttachments
        ((depth : Texture,
          albedo : Texture,
          material : Texture,
          normalPlus : Texture,
          subdermalPlus : Texture,
          scatterPlus : Texture,
          clearCoatPlus : Texture,
          z : Texture),
          vkc) =
        depth.Destroy vkc
        albedo.Destroy vkc
        material.Destroy vkc
        normalPlus.Destroy vkc
        subdermalPlus.Destroy vkc
        scatterPlus.Destroy vkc
        clearCoatPlus.Destroy vkc
        z.Destroy vkc
    
    /// Create lighting attachment.
    let CreateLightingAttachment (resolutionX, resolutionY, vkc) =
        CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgb16f, Rgb, resolutionX, resolutionY, vkc)

    /// Update size of lighting attachment. Must be used every frame.
    let UpdateLightingAttachmentSize (resolutionX, resolutionY, lighting, vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata lighting vkc

    /// Destroy lighting attachment.
    let DestroyLightingAttachment (lighting : Texture, vkc) =
        lighting.Destroy vkc
    
    /// Create coloring attachments.
    let CreateColoringAttachments (resolutionX, resolutionY, vkc) =
        let color = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], Rgb16f, Rgb, resolutionX, resolutionY, vkc)
        let depth = CreateColorAttachment (Texture2d, [|VkImageUsageFlags.Sampled|], R16f, Red, resolutionX, resolutionY, vkc)
        (color, depth)
    
    /// Update size of coloring attachments. Must be used every frame.
    let UpdateColoringAttachmentsSize (resolutionX, resolutionY, (color, depth), vkc) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color vkc
        Texture.updateSize metadata depth vkc

    /// Destroy coloring attachments.
    let DestroyColoringAttachments ((color : Texture, depth : Texture), vkc) =
        color.Destroy vkc
        depth.Destroy vkc