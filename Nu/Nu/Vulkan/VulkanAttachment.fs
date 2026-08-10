// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open Vortice.Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module Attachment =
    
    /// Create a color attachment.
    let createColorAttachment textureType optionalUsages internalFormat pixelFormat resolutionX resolutionY (context : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureInternal =
            TextureInternal.create
                MipmapNone (AttachmentColor true) textureType optionalUsages
                (Hl.checkAttachmentFormat context.PhysicalDevice.VkPhysicalDevice internalFormat) pixelFormat metadata context
        EagerTexture textureInternal

    /// Update size of color attachment.
    let updateColorAttachmentSize resolutionX resolutionY color context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context

    /// Destroy color attachment.
    let destroyColorAttachment (color : Texture) context =
        Texture.destroy color context

    /// Create depth attachment.
    let createDepthAttachment optionalUsages resolutionX resolutionY (context : VulkanContext) =
        let metadata = TextureMetadata.make resolutionX resolutionY
        let textureInternal =
            TextureInternal.create
                MipmapNone (AttachmentDepth true) Texture2d optionalUsages
                (Hl.checkAttachmentFormat context.PhysicalDevice.VkPhysicalDevice D32f) Depth metadata context
        EagerTexture textureInternal

    /// Update size of depth attachment.
    let updateDepthAttachmentSize resolutionX resolutionY depth context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata depth context

    /// Destroy depth attachment.
    let destroyDepthAttachment (depth : Texture) context =
        Texture.destroy depth context

    /// Create bloom sample attachments.
    let createBloomSampleAttachments resolutionX resolutionY context =
        [|for i in 0 .. dec Constants.Render.BloomSampleLevels do
            let (resolutionX', resolutionY') = (resolutionX >>> i, resolutionY >>> i)
            if resolutionX' = 0 || resolutionY' = 0 then failwith ("Invalid resolution [" + string resolutionX' + " " + string resolutionY' + "] for bloom filter level.")
            createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgb16f Rgb resolutionX' resolutionY' context|]

    /// Update size of bloom sample attachments.
    let updateBloomSampleAttachmentsSize resolutionX resolutionY (bloomSamples : Texture array) context =
        for i in 0 .. dec Constants.Render.BloomSampleLevels do
            let (resolutionX', resolutionY') = (resolutionX >>> i, resolutionY >>> i)
            let metadata = TextureMetadata.make resolutionX' resolutionY'
            Texture.updateSize metadata bloomSamples[i] context

    /// Destroy bloom sample attachments.
    let destroyBloomSampleAttachments (bloomSamples : Texture array) context =
        for i in 0 .. dec Constants.Render.BloomSampleLevels do
            Texture.destroy bloomSamples[i] context

    /// Create tone-mapping attachments.
    let createToneMappingAttachments resolutionX resolutionY context =
        createColorAttachment Texture2d (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc ||| VkImageUsageFlags.TransferDst) Rgb16f Rgb resolutionX resolutionY context

    /// Update size of tone-mapping attachments.
    let updateToneMappingAttachmentsSize resolutionX resolutionY toneMapping context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata toneMapping context

    /// Destroy tone-mapping attachments.
    let destroyToneMappingAttachments (toneMapping : Texture) context =
        Texture.destroy toneMapping context

    /// Create gamma correction attachments.
    let createGammaCorrectionAttachments resolutionX resolutionY context =
        createColorAttachment Texture2d (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc) Rgba16f Rgba resolutionX resolutionY context

    /// Update size of gamma-correction attachments.
    let updateGammaCorrectionAttachmentsSize resolutionX resolutionY gammaCorrection context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata gammaCorrection context

    /// Destroy gamma-correction attachments.
    let destroyGammaCorrectionAttachment (gammaCorrection : Texture) context =
        Texture.destroy gammaCorrection context

    /// Create shadow texture array attachments.
    let createShadowTextureArrayAttachments shadowResolutionX shadowResolutionY shadowResolutionZ context =
        let color = createColorAttachment (Texture2dArray shadowResolutionZ) VkImageUsageFlags.Sampled Rg32f Rg shadowResolutionX shadowResolutionY context
        let z = createDepthAttachment VkImageUsageFlags.None shadowResolutionX shadowResolutionY context
        (color, z)
    
    /// Update size of shadow texture array attachments.
    let updateShadowTextureArrayAttachmentsSize resolutionX resolutionY (color, z) context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context
        Texture.updateSize metadata z context

    /// Destroy shadow texture array attachments.
    let destroyShadowTextureArrayAttachments (color : Texture, z : Texture) context =
        Texture.destroy color context
        Texture.destroy z context
    
    /// Create shadow map attachments.
    let createShadowMapAttachments shadowResolutionX shadowResolutionY context =
        let color = createColorAttachment TextureCubeMap VkImageUsageFlags.Sampled R16f Red shadowResolutionX shadowResolutionY context
        let z = createDepthAttachment VkImageUsageFlags.None shadowResolutionX shadowResolutionY context
        (color, z)

    /// Update size of shadow map attachments.
    let updateShadowMapAttachmentsSize resolutionX resolutionY (color, z) context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context
        Texture.updateSize metadata z context

    /// Destroy shadow map attachments.
    let destroyShadowMapAttachments (color : Texture, z : Texture) context =
        Texture.destroy color context
        Texture.destroy z context
    
    /// Create shadow cascade array attachments.
    let createShadowCascadeArrayAttachments shadowCascadeResolutionX shadowCascadeResolutionY shadowCascadeLevels context =
        let color =
            createColorAttachment
                (Texture2dArray shadowCascadeLevels) VkImageUsageFlags.Sampled
                Rg32f Rg shadowCascadeResolutionX shadowCascadeResolutionY context
        let z = createDepthAttachment VkImageUsageFlags.None shadowCascadeResolutionX shadowCascadeResolutionY context
        (color, z)
    
    /// Update size of shadow cascade array attachments.
    let updateShadowCascadeArrayAttachmentsSize resolutionX resolutionY (color, z) context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context
        Texture.updateSize metadata z context

    /// Destroy shadow cascade array attachments.
    let destroyShadowCascadeArrayAttachments (color : Texture, z : Texture) context =
        Texture.destroy color context
        Texture.destroy z context
    
    /// Create geometry attachments.
    let createGeometryAttachments resolutionX resolutionY context =
        let depth = createColorAttachment Texture2d VkImageUsageFlags.Sampled R32f Red resolutionX resolutionY context
        let albedo = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba8 Rgba resolutionX resolutionY context
        let material = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba8 Rgba resolutionX resolutionY context
        let normalPlus = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context
        let subdermalPlus = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba8 Rgba resolutionX resolutionY context
        let scatterPlus = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba8 Rgba resolutionX resolutionY context
        let clearCoatPlus = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context
        let z = createDepthAttachment VkImageUsageFlags.None resolutionX resolutionY context
        (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z)
    
    /// Update size of geometry attachments.
    let updateGeometryAttachmentsSize resolutionX resolutionY (depth, albedo, material, normalPlus, subdermalPlus, scatterPlus, clearCoatPlus, z) context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata depth context
        Texture.updateSize metadata albedo context
        Texture.updateSize metadata material context
        Texture.updateSize metadata normalPlus context
        Texture.updateSize metadata subdermalPlus context
        Texture.updateSize metadata scatterPlus context
        Texture.updateSize metadata clearCoatPlus context
        Texture.updateSize metadata z context

    /// Destroy geometry attachments.
    let destroyGeometryAttachments (depth : Texture, albedo : Texture, material : Texture, normalPlus : Texture, subdermalPlus : Texture, scatterPlus : Texture, clearCoatPlus : Texture, z : Texture) context =
        Texture.destroy depth context
        Texture.destroy albedo context
        Texture.destroy material context
        Texture.destroy normalPlus context
        Texture.destroy subdermalPlus context
        Texture.destroy scatterPlus context
        Texture.destroy clearCoatPlus context
        Texture.destroy z context

    /// Create lighting attachment.
    let createLightingAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgb16f Rgb resolutionX resolutionY context

    /// Update size of lighting attachment.
    let updateLightingAttachmentSize resolutionX resolutionY lighting context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata lighting context

    /// Destroy lighting attachment.
    let destroyLightingAttachment (lighting : Texture) context =
        Texture.destroy lighting context

    /// Create light mapping attachment.
    let createLightMappingAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context

    /// Update size of light mapping attachment.
    let updateLightMappingAttachmentSize resolutionX resolutionY lightmapping context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata lightmapping context

    /// Destroy light mapping attachment.
    let destroyLightMappingAttachment (lightmapping : Texture) context =
        Texture.destroy lightmapping context

    /// Create ambient attachment.
    let createAmbientAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context

    /// Update size of ambient attachment.
    let updateAmbientAttachmentSize resolutionX resolutionY ambient context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata ambient context

    /// Destroy ambient attachment.
    let destroyAmbientAttachment (ambient : Texture) context =
        Texture.destroy ambient context

    /// Create irradiance attachment.
    let createIrradianceAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context

    /// Update size of irradiance attachment.
    let updateIrradianceAttachmentSize resolutionX resolutionY irradiance context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata irradiance context

    /// Destroy irradiance attachment.
    let destroyIrradianceAttachment (irradiance : Texture) context =
        Texture.destroy irradiance context

    /// Create environment filter attachment.
    let createEnvironmentFilterAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgba16f Rgba resolutionX resolutionY context

    /// Update size of environment filter attachment.
    let updateEnvironmentFilterAttachmentSize resolutionX resolutionY environmentfilter context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata environmentfilter context

    /// Destroy environment filter attachment.
    let destroyEnvironmentFilterAttachment (environmentfilter : Texture) context =
        Texture.destroy environmentfilter context

    /// Create fogging attachment.
    let createFoggingAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferDst) Rgb16f Rgb resolutionX resolutionY context

    /// Update size of fogging attachment.
    let updateFoggingAttachmentSize resolutionX resolutionY fogging context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata fogging context

    /// Destroy fogging attachment.
    let destroyFoggingAttachment (fogging : Texture) context =
        Texture.destroy fogging context

    /// Create coloring attachments.
    let createColoringAttachments resolutionX resolutionY context =
        let color = createColorAttachment Texture2d VkImageUsageFlags.Sampled Rgb16f Rgb resolutionX resolutionY context
        let depth = createColorAttachment Texture2d VkImageUsageFlags.Sampled R16f Red resolutionX resolutionY context
        (color, depth)

    /// Update size of coloring attachments.
    let updateColoringAttachmentsSize resolutionX resolutionY (color, depth) context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context
        Texture.updateSize metadata depth context

    /// Destroy coloring attachments.
    let destroyColoringAttachments (color : Texture, depth : Texture) context =
        Texture.destroy color context
        Texture.destroy depth context

    /// Create composition attachments.
    let createCompositionAttachment resolutionX resolutionY context =
        createColorAttachment Texture2d (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferSrc ||| VkImageUsageFlags.TransferDst) Rgba16f Rgba resolutionX resolutionY context

    /// Update size of composition attachments.
    let updateCompositionAttachmentSize resolutionX resolutionY color context =
        let metadata = TextureMetadata.make resolutionX resolutionY
        Texture.updateSize metadata color context

    /// Destroy composition attachments.
    let destroyCompositionAttachment (color : Texture) context =
        Texture.destroy color context