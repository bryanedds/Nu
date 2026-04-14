// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    // NOTE: DJL: in the context of individual attachment textures, depth component attachments used for depth testing are named "z" to easily distinguish them
    // from color component attachments using the name "depth". Otherwise color and depth component textures are just called "color" and "depth" attachments,
    // as they are called when passed to Vulkan structures.
    
    /// Create color attachment.
    let private CreateColorAttachment (textureType, optionalUsages, internalFormat, pixelFormat, resolutionX, resolutionY, vkc : Hl.VulkanContext) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        let textureInternal =
            Texture.TextureInternal.create
                Texture.MipmapNone (Texture.AttachmentColor true) textureType optionalUsages
                (Hl.CheckAttachmentFormat (vkc.VkPhysicalDevice, internalFormat)) pixelFormat metadata vkc
        Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = textureInternal }
    
    /// Create depth attachment.
    let private CreateDepthAttachment (optionalUsages, resolutionX, resolutionY, vkc : Hl.VulkanContext) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        let textureInternal =
            Texture.TextureInternal.create
                Texture.MipmapNone (Texture.AttachmentDepth true) Texture.Texture2d optionalUsages
                (Hl.CheckAttachmentFormat (vkc.VkPhysicalDevice, Hl.D32f)) Hl.Depth metadata vkc
        Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = textureInternal }
    
    /// Create general-purpose attachments.
    let CreateGeneralAttachments (resolutionX, resolutionY, vkc) =
        let color = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.TransferSrc|], Hl.Rgba16f, Hl.Rgba, resolutionX, resolutionY, vkc)
        let z = CreateDepthAttachment ([||], resolutionX, resolutionY, vkc)
        (color, z)

    /// Update size of general attachments. Must be used every frame.
    let UpdateGeneralAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata z vkc

    /// Destroy general attachments.
    let DestroyGeneralAttachments ((color : Texture.Texture, z : Texture.Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow texture array attachments.
    let CreateShadowTextureArrayAttachments (shadowResolutionX, shadowResolutionY, shadowResolutionZ, vkc) =
        let color = CreateColorAttachment ((Texture.Texture2dArray shadowResolutionZ), [|VkImageUsageFlags.Sampled|], Hl.Rg32f, Hl.Rg, shadowResolutionX, shadowResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowResolutionX, shadowResolutionY, vkc)
        (color, z)
    
    /// Update size of shadow texture array attachments. Must be used every frame.
    let UpdateShadowTextureArrayAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata z vkc

    /// Destroy shadow texture array attachments.
    let DestroyShadowTextureArrayAttachments ((color : Texture.Texture, z : Texture.Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow map attachments.
    let CreateShadowMapAttachments (shadowResolutionX, shadowResolutionY, vkc) =
        let color = CreateColorAttachment (Texture.TextureCubeMap, [|VkImageUsageFlags.Sampled|], Hl.R16f, Hl.Red, shadowResolutionX, shadowResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowResolutionX, shadowResolutionY, vkc)
        (color, z)

    /// Update size of shadow map attachments. Must be used every frame.
    let UpdateShadowMapAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata z vkc

    /// Destroy shadow map attachments.
    let DestroyShadowMapAttachments ((color : Texture.Texture, z : Texture.Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create shadow cascade array attachments.
    let CreateShadowCascadeArrayAttachments (shadowCascadeResolutionX, shadowCascadeResolutionY, shadowCascadeLevels, vkc) =
        let color =
            CreateColorAttachment
                ((Texture.Texture2dArray shadowCascadeLevels), [|VkImageUsageFlags.Sampled|],
                 Hl.Rg32f, Hl.Rg, shadowCascadeResolutionX, shadowCascadeResolutionY, vkc)
        let z = CreateDepthAttachment ([||], shadowCascadeResolutionX, shadowCascadeResolutionY, vkc)
        (color, z)
    
    /// Update size of shadow cascade array attachments. Must be used every frame.
    let UpdateShadowCascadeArrayAttachmentsSize (resolutionX, resolutionY, (color, z), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata color vkc
        Texture.Texture.updateSize metadata z vkc

    /// Destroy shadow cascade array attachments.
    let DestroyShadowCascadeArrayAttachments ((color : Texture.Texture, z : Texture.Texture), vkc) =
        color.Destroy vkc
        z.Destroy vkc
    
    /// Create geometry attachments.
    let CreateGeometryAttachments (resolutionX, resolutionY, vkc) =
        let depth = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.R32f, Hl.Red, resolutionX, resolutionY, vkc)
        let albedo = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba8, Hl.Rgba, resolutionX, resolutionY, vkc)
        let material = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba8, Hl.Rgba, resolutionX, resolutionY, vkc)
        let normalPlus = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba16f, Hl.Rgba, resolutionX, resolutionY, vkc)
        let subdermalPlus = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba8, Hl.Rgba, resolutionX, resolutionY, vkc)
        let scatterPlus = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba8, Hl.Rgba, resolutionX, resolutionY, vkc)
        let clearCoatPlus = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgba16f, Hl.Rgba, resolutionX, resolutionY, vkc)
        let z = CreateDepthAttachment ([||], resolutionX, resolutionY, vkc)
        (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z)
    
    /// Update size of geometry attachments. Must be used every frame.
    let UpdateGeometryAttachmentsSize (resolutionX, resolutionY, (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z), vkc) =
        let metadata = Texture.TextureMetadata.make resolutionX resolutionY
        Texture.Texture.updateSize metadata depth vkc
        Texture.Texture.updateSize metadata albedo vkc
        Texture.Texture.updateSize metadata material vkc
        Texture.Texture.updateSize metadata normalPlus vkc
        Texture.Texture.updateSize metadata subdermalPlus vkc
        Texture.Texture.updateSize metadata scatterPlus vkc
        Texture.Texture.updateSize metadata clearCoatPlus vkc
        Texture.Texture.updateSize metadata z vkc

    /// Destroy geometry attachments.
    let DestroyGeometryAttachments
        ((depth : Texture.Texture,
          albedo : Texture.Texture,
          material : Texture.Texture,
          normalPlus : Texture.Texture,
          subdermalPlus : Texture.Texture,
          scatterPlus : Texture.Texture,
          clearCoatPlus : Texture.Texture,
          z : Texture.Texture),
          vkc) =
        depth.Destroy vkc
        albedo.Destroy vkc
        material.Destroy vkc
        normalPlus.Destroy vkc
        subdermalPlus.Destroy vkc
        scatterPlus.Destroy vkc
        clearCoatPlus.Destroy vkc
        z.Destroy vkc
    
    /// Create coloring attachments.
    let CreateColoringAttachments (resolutionX, resolutionY, vkc) =
        let color = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.Rgb16f, Hl.Rgb, resolutionX, resolutionY, vkc)
        let depth = CreateColorAttachment (Texture.Texture2d, [|VkImageUsageFlags.Sampled|], Hl.R16f, Hl.Red, resolutionX, resolutionY, vkc)
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