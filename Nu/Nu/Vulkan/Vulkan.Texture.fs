// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Threading
open SDL2
open Pfim
open Prime
open Nu

// NOTE: on Nu's texturing nomenclature -
// Texture | Texture2d =    2d texture
// Texture3d =              3d texture
// (Cube|_)Map =            cube map
[<RequireQualifiedAccess>]
module Texture =

    /// The forward-declared empty texture value.
    /// Initialized in RendererProcesses.
    /// NOTE: if performance issues arise from checking / casting this, maybe use ValueOption or null directly.
    /// TODO: see if instead of exposing mutability of this directly, we should define Init and CleanUp fns.
    let mutable internal EmptyOpt : obj option = None

    /// The thread on which a texture is loaded.
    type TextureLoadThread =
        | RenderThread
        | TextureStreamingThread

        /// Get the vulkan resources responsible for loading textures on a thread.
        static member getResources thread (vkc : Hl.VulkanContext) =
            match thread with
            | RenderThread -> (vkc.RenderQueue, vkc.TransientCommandPool, vkc.TransientFence)
            | TextureStreamingThread -> (vkc.TextureQueue, vkc.TextureCommandPool, vkc.TextureFence)
    
    /// The type of block compression to use for a texture, if any.
    type BlockCompression =
        | Uncompressed
        | ColorCompression
        | NormalCompression

        /// The ImageFormat corresponding to this block compression.
        member this.ImageFormat =
            match this with
            | Uncompressed -> Hl.Rgba8
            | ColorCompression -> Hl.Bc3 // aka s3tc dxt5
            | NormalCompression -> Hl.Bc5 // aka rg rgtc2

    /// Determines whether a texture has mipmaps, and whether they are handled manually or automatically.
    type MipmapMode =
        | MipmapNone
        | MipmapManual of MipmapCount : int
        | MipmapAuto
    
    /// Determines how a texture is configured based on intended usage.
    type TextureType =
        | TextureGeneral
        | TextureCubeMap
        | TextureAttachmentColor

        /// Whether the type must be paralleled for frames in flight.
        member this.IsParallel =
            match this with
            | TextureGeneral
            | TextureCubeMap -> false
            | TextureAttachmentColor -> true
        
        /// The VkSamplerAddressMode used for a given type.
        member this.VkSamplerAddressMode =
            match this with
            | TextureGeneral -> VkSamplerAddressMode.Repeat
            | TextureCubeMap
            | TextureAttachmentColor -> VkSamplerAddressMode.ClampToEdge

        /// The VkImageUsageFlags for a given type.
        member this.VkImageUsageFlags =
            match this with
            | TextureGeneral
            | TextureCubeMap -> VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferDst
            | TextureAttachmentColor -> VkImageUsageFlags.ColorAttachment ||| VkImageUsageFlags.TransferSrc
    
    /// Record command to copy buffer to image.
    /// TODO: DJL: understand and resolve difference between curried and tuple functions.
    let private RecordBufferToImageCopy cb width height mipLevel layer vkBuffer vkImage =
        Hl.recordTransitionLayout cb false mipLevel layer Hl.UndefinedHost Hl.TransferDst vkImage
        let mutable region = VkBufferImageCopy ()
        region.imageSubresource <- Hl.makeSubresourceLayersColor mipLevel layer
        region.imageExtent <- VkExtent3D (width, height, 1)
        Vulkan.vkCmdCopyBufferToImage
            (cb, vkBuffer, vkImage,
             Hl.TransferDst.VkImageLayout,
             1u, asPointer &region)
        Hl.recordTransitionLayout cb false mipLevel layer Hl.TransferDst Hl.ShaderRead vkImage
    
    /// Record commands to generate mipmaps.
    let private RecordGenerateMipmaps cb width height mipLevels layer vkImage =
        
        // use single barrier for all transfer operations
        let mutable barrier = VkImageMemoryBarrier ()
        barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.image <- vkImage
        
        // transition mipmap images from undefined as they haven't been touched yet
        barrier.srcAccessMask <- Hl.UndefinedHost.Access
        barrier.dstAccessMask <- Hl.TransferDst.Access
        barrier.oldLayout <- Hl.UndefinedHost.VkImageLayout
        barrier.newLayout <- Hl.TransferDst.VkImageLayout
        barrier.subresourceRange <- Hl.makeSubresourceRangeColor (mipLevels - 1) 1
        barrier.subresourceRange.baseArrayLayer <- uint layer
        barrier.subresourceRange.baseMipLevel <- 1u
        Vulkan.vkCmdPipelineBarrier
            (cb,
             Hl.UndefinedHost.PipelineStage,
             Hl.TransferDst.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, asPointer &barrier)
        
        // transition original image separately as it's already set to shader read
        barrier.srcAccessMask <- Hl.ShaderRead.Access
        barrier.dstAccessMask <- Hl.TransferDst.Access
        barrier.oldLayout <- Hl.ShaderRead.VkImageLayout
        barrier.newLayout <- Hl.TransferDst.VkImageLayout
        barrier.subresourceRange.baseMipLevel <- 0u
        barrier.subresourceRange.levelCount <- 1u // only one level at a time from here on
        Vulkan.vkCmdPipelineBarrier
            (cb,
             Hl.ShaderRead.PipelineStage,
             Hl.TransferDst.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, asPointer &barrier)
        
        // init mipmap dimensions
        let mutable mipWidth = width
        let mutable mipHeight = height

        for i in 1 .. dec mipLevels do
            
            // transition layout of previous image to be copied from
            barrier.srcAccessMask <- Hl.TransferDst.Access
            barrier.dstAccessMask <- Hl.TransferSrc.Access
            barrier.oldLayout <- Hl.TransferDst.VkImageLayout
            barrier.newLayout <- Hl.TransferSrc.VkImageLayout
            barrier.subresourceRange.baseMipLevel <- uint (i - 1)
            Vulkan.vkCmdPipelineBarrier
                (cb,
                 Hl.TransferDst.PipelineStage,
                 Hl.TransferSrc.PipelineStage,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &barrier)
            
            // generate the next mipmap image from the previous one
            let nextWidth = if mipWidth > 1 then mipWidth / 2 else 1
            let nextHeight = if mipHeight > 1 then mipHeight / 2 else 1
            let mutable blit =
                Hl.makeBlit
                    (i - 1) i layer layer
                    (VkRect2D (0, 0, uint mipWidth, uint mipHeight))
                    (VkRect2D (0, 0, uint nextWidth, uint nextHeight))
            Vulkan.vkCmdBlitImage (cb, vkImage, Hl.TransferSrc.VkImageLayout, vkImage, Hl.TransferDst.VkImageLayout, 1u, asPointer &blit, VkFilter.Linear)
            
            // transition layout of previous image to be read by shader
            barrier.srcAccessMask <- Hl.TransferSrc.Access
            barrier.dstAccessMask <- Hl.ShaderRead.Access
            barrier.oldLayout <- Hl.TransferSrc.VkImageLayout
            barrier.newLayout <- Hl.ShaderRead.VkImageLayout
            Vulkan.vkCmdPipelineBarrier
                (cb,
                 Hl.TransferSrc.PipelineStage,
                 Hl.ShaderRead.PipelineStage,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &barrier)
            
            // update mipmap dimensions
            mipWidth <- nextWidth
            mipHeight <- nextHeight
        
        // transition final mip image left unfinished by loop
        barrier.srcAccessMask <- Hl.TransferDst.Access
        barrier.dstAccessMask <- Hl.ShaderRead.Access
        barrier.oldLayout <- Hl.TransferDst.VkImageLayout
        barrier.newLayout <- Hl.ShaderRead.VkImageLayout
        barrier.subresourceRange.baseMipLevel <- uint (mipLevels - 1)
        Vulkan.vkCmdPipelineBarrier
            (cb,
             Hl.TransferDst.PipelineStage,
             Hl.ShaderRead.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, asPointer &barrier)
    
    /// Infer that an asset with the given file path should be filtered in a 2D rendering context.
    let InferFiltered2d (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        name.EndsWith "_f" ||
        name.EndsWith "Filtered"
    
    /// Infer the type of block compression that an asset with the given file path should utilize.
    let InferCompression (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        if  name.EndsWith "_f" ||
            name.EndsWith "_hm" ||
            name.EndsWith "_b" ||
            name.EndsWith "_t" ||
            name.EndsWith "_u" ||
            name.EndsWith "Face" ||
            name.EndsWith "HeightMap" ||
            name.EndsWith "Blend" ||
            name.EndsWith "Tint" ||
            name.EndsWith "Uncompressed" then Uncompressed
        elif
            name.EndsWith "_n" ||
            name.EndsWith "_normal" ||
            name.EndsWith "Normal" then NormalCompression
        else ColorCompression

    /// Attempt to format an uncompressed pfim image texture (non-mipmap).
    /// TODO: make this an IImage extension and move elsewhere?
    let TryFormatUncompressedPfimageTexture (format, height, stride, data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < height do
                    let mutable x = 0
                    while x < stride - 2 do
                        let i = x + stride * y
                        data.[i]; data.[i+1]; data.[i+2]; 255uy
                        x <- x + 3
                    y <- inc y|]
            Some converted
        | ImageFormat.Rgba32 ->
            let converted =
                [|let mutable y = 0
                  while y < height do
                    let mutable x = 0
                    while x < stride - 3 do
                        let i = x + stride * y
                        data.[i]; data.[i+1]; data.[i+2]; data.[i+3]
                        x <- x + 4
                    y <- inc y|]
            Some converted
        | _ -> Log.info ("Unsupported image format '" + scstring format + "'."); None
        
    /// Attempt to format an uncompressed pfim image mipmap.
    /// TODO: make this an IImage extension and move elsewhere?
    let FormatUncompressedPfimageMipmap (format, mipmap : MipMapOffset, data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < mipmap.Height do
                    let mutable x = 0
                    while x < mipmap.Stride - 2 do
                        let i = x + mipmap.Stride * y + mipmap.DataOffset
                        data.[i]; data.[i+1]; data.[i+2]; 255uy
                        x <- x + 3
                    y <- inc y|]
            (v2i mipmap.Width mipmap.Height, converted)
        | ImageFormat.Rgba32 ->
            let converted =
                [|let mutable y = 0
                  while y < mipmap.Height do
                    let mutable x = 0
                    while x < mipmap.Stride - 3 do
                        let i = x + mipmap.Stride * y + mipmap.DataOffset
                        data.[i]; data.[i+1]; data.[i+2]; data.[i+3]
                        x <- x + 4
                    y <- inc y|]
            (v2i mipmap.Width mipmap.Height, converted)
        | _ -> failwithumf ()

    /// Attempt to format an uncompressed pfim image.
    /// TODO: make this an IImage extension and move elsewhere?
    let TryFormatUncompressedPfimage (minimal, image : IImage) =
        let minimal = minimal && image.MipMaps.Length >= 1 // NOTE: at least one mipmap is needed for minimal load.
        let data = image.Data // OPTIMIZATION: pulling all values out of image to avoid slow property calls.
        let format = image.Format
        let height = image.Height
        let stride = image.Stride
        let mipmaps = image.MipMaps
        let bytesOpt =
            if not minimal
            then TryFormatUncompressedPfimageTexture (format, height, stride, data)
            else Some [||]
        match bytesOpt with
        | Some bytes ->
            let minimalMipmapIndex =
                if minimal
                then min mipmaps.Length Constants.Render.TextureMinimalMipmapIndex
                else 0
            let mipmapBytesArray =
                [|for i in minimalMipmapIndex .. dec mipmaps.Length do
                    FormatUncompressedPfimageMipmap (format, mipmaps.[i], data)|]
            if minimal then
                let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray.[0]
                let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
                Some (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
            else Some (v2i image.Width image.Height, bytes, mipmapBytesArray)
        | None -> None

    /// Attempt to format compressed pfim image data.
    /// TODO: make this a Dds extension and move elsewhere?
    let FormatCompressedPfdds (minimal, dds : Dds) =
        let minimal = minimal && dds.Header.MipMapCount >= 3u // NOTE: at least three mipmaps are needed for minimal load since the last 2 are not valid when compressed.
        let mutable dims = v2i dds.Width dds.Height
        let mutable size = ((dims.X + 3) / 4) * ((dims.Y + 3) / 4) * 16
        let mutable index = 0
        let bytes =
            if not minimal
            then dds.Data.AsSpan(index, size).ToArray()
            else [||]
        let minimalMipmapIndex =
            if minimal // NOTE: inc mipmap indexes here because dds header seems to count full image as mipmap 0.
            then min dds.Header.MipMapCount (uint Constants.Render.TextureMinimalMipmapIndex)
            else 0u
        let mipmapBytesArray =
            if dds.Header.MipMapCount >= 2u then
                [|for _ in minimalMipmapIndex .. dec dds.Header.MipMapCount - 1u do
                    dims <- dims / 2
                    index <- index + size
                    size <- size / 4
                    if size >= 16 then (dims, dds.Data.AsSpan(index, size).ToArray())|] // NOTE: as mentioned above, mipmap with size < 16 can exist but isn't valid when compressed.
            else [||]
        if minimal then
            let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray.[0]
            let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
            (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
        else (v2i dds.Width dds.Height, bytes, mipmapBytesArray)

    /// A Vulkan texture's metadata.
    type TextureMetadata =
        { TextureWidth : int
          TextureHeight : int
          TextureTexelWidth : single
          TextureTexelHeight : single }

        /// Make texture metadata.
        static member make width height =
            { TextureWidth = width
              TextureHeight = height
              TextureTexelWidth = 1.0f / single width
              TextureTexelHeight = 1.0f / single height }

        /// Unpopulated texture data.
        static member empty =
            { TextureWidth = 0
              TextureHeight = 0
              TextureTexelWidth = 0.0f
              TextureTexelHeight = 0.0f }

    /// An abstraction of a texture as managed by Vulkan.
    /// TODO: extract sampler out of here.
    type [<CustomEquality; NoComparison>] VulkanTexture =
        private
            { Images_ : VkImage array
              Allocations_ : VmaAllocation array
              ImageViews_ : VkImageView array
              ImageSizes_ : TextureMetadata array
              Sampler_ : VkSampler
              InternalFormat_ : Hl.ImageFormat
              PixelFormat_ : Hl.PixelFormat
              MipLevels_ : int
              TextureType_ : TextureType }

        member private this.IsParallel = this.TextureType_.IsParallel
        member private this.CurrentIndex = if this.IsParallel then Hl.CurrentFrame else 0
        member private this.ImageSize = this.ImageSizes_.[this.CurrentIndex]
        
        /// The unique texture id.
        member this.TextureId = this.Images_.[0].Handle
        
        /// The image.
        member this.Image = this.Images_.[this.CurrentIndex]

        /// The image view.
        member this.ImageView = this.ImageViews_.[this.CurrentIndex]

        /// The sampler.
        member this.Sampler = this.Sampler_

        /// The VkFormat.
        member this.Format = this.InternalFormat_.VkFormat
        
        /// The mip level count.
        member this.MipLevels = this.MipLevels_
        
        override this.Equals thatObj =
            match thatObj with
            | :? VulkanTexture as that -> this.TextureId = that.TextureId
            | _ -> false

        override this.GetHashCode () = 
            hash this.TextureId

        /// Create the image.
        static member private createImage format extent mipLevels (textureType : TextureType) (vkc : Hl.VulkanContext) =
            
            // determine appropriate usage
            // TODO: DJL: handle potential double transferSrc flag properly.
            let usage =
                if mipLevels = 1
                then textureType.VkImageUsageFlags
                else textureType.VkImageUsageFlags ||| VkImageUsageFlags.TransferSrc // for mipmap generation; TODO: DJL: get rid of this for manual mipmaps.
                    
            // create image
            let mutable iInfo = VkImageCreateInfo ()
            if textureType.IsTextureCubeMap then
                iInfo.flags <- VkImageCreateFlags.CubeCompatible
            iInfo.imageType <- VkImageType.Image2D
            iInfo.format <- format
            iInfo.extent <- extent
            iInfo.mipLevels <- uint mipLevels
            iInfo.arrayLayers <- if textureType.IsTextureCubeMap then 6u else 1u
            iInfo.samples <- VkSampleCountFlags.Count1
            iInfo.tiling <- VkImageTiling.Optimal
            iInfo.usage <- usage
            iInfo.sharingMode <- VkSharingMode.Exclusive
            iInfo.initialLayout <- Hl.UndefinedHost.VkImageLayout
            let aInfo = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (vkc.VmaAllocator, &iInfo, &aInfo, &image, &allocation, nullPtr) |> Hl.check
            (image, allocation)
        
        /// Create the sampler.
        static member private createSampler minFilter magFilter anisoFilter (textureType : TextureType) (vkc : Hl.VulkanContext) =
            let mutable info = VkSamplerCreateInfo ()
            info.magFilter <- magFilter
            info.minFilter <- minFilter
            info.mipmapMode <- VkSamplerMipmapMode.Linear
            info.addressModeU <- textureType.VkSamplerAddressMode
            info.addressModeV <- textureType.VkSamplerAddressMode
            info.addressModeW <- textureType.VkSamplerAddressMode
            if anisoFilter then
                info.anisotropyEnable <- true
                info.maxAnisotropy <- min vkc.MaxAnisotropy Constants.Render.TextureAnisotropyMax
            info.maxLod <- Vulkan.VK_LOD_CLAMP_NONE
            let mutable sampler = Unchecked.defaultof<VkSampler>
            Vulkan.vkCreateSampler (vkc.Device, &info, nullPtr, &sampler) |> Hl.check
            sampler
        
        /// Create a VulkanTexture.
        static member create
            (pixelFormat : Hl.PixelFormat)
            minFilter
            magFilter
            anisoFilter
            mipmapMode
            (textureType : TextureType)
            (internalFormat : Hl.ImageFormat)
            metadata
            (vkc : Hl.VulkanContext) =

            // determine mip levels
            let mipLevels =
                match textureType with
                | TextureGeneral | TextureCubeMap ->
                    match mipmapMode with
                    | MipmapNone -> 1
                    | MipmapManual mips -> mips
                    | MipmapAuto ->
                        
                        // check if hardware supports mipmap generation; this is done here to prevent unused (i.e. blank) mip levels
                        // TODO: DJL: check for VkFormatFeatureFlags.BlitSrc/Dst as well.
                        let mutable formatProperties = Unchecked.defaultof<VkFormatProperties>
                        Vulkan.vkGetPhysicalDeviceFormatProperties (vkc.PhysicalDevice, internalFormat.VkFormat, &formatProperties)
                        let mipGenSupport = formatProperties.optimalTilingFeatures &&& VkFormatFeatureFlags.SampledImageFilterLinear <> VkFormatFeatureFlags.None
                        
                        // calculate mip levels
                        if mipGenSupport then max metadata.TextureWidth metadata.TextureHeight |> Math.Log2 |> floor |> inc |> int
                        else Log.infoOnce "Graphics device does not support mipmap generation for some used image format(s)."; 1

                | TextureAttachmentColor ->
                    if not mipmapMode.IsMipmapNone then Log.warn "Mipmaps not supported for attachment texture."
                    1
            
            // create images, image views and sampler
            let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            let length = if textureType.IsParallel then Constants.Vulkan.MaxFramesInFlight else 1
            let images = Array.zeroCreate<VkImage> length
            let allocations = Array.zeroCreate<VmaAllocation> length
            let imageViews = Array.zeroCreate<VkImageView> length
            let imageSizes = Array.zeroCreate<TextureMetadata> length
            for i in 0 .. dec length do
                let (image, allocation) = VulkanTexture.createImage internalFormat.VkFormat extent mipLevels textureType vkc
                images.[i] <- image
                allocations.[i] <- allocation
                imageViews.[i] <- Hl.createImageView pixelFormat.IsBgra internalFormat.VkFormat mipLevels textureType.IsTextureCubeMap image vkc.Device
                imageSizes.[i] <- metadata
            let sampler = VulkanTexture.createSampler minFilter magFilter anisoFilter textureType vkc
            
            // transition layout as appropriate
            match textureType with
            | TextureAttachmentColor ->
                let (queue, pool, fence) = TextureLoadThread.getResources RenderThread vkc
                let cb = Hl.beginTransientCommandBlock pool vkc.Device
                for i in 0 .. dec length do
                    match textureType with
                    | TextureAttachmentColor -> Hl.recordTransitionLayout cb true 1 0 Hl.Undefined Hl.ColorAttachmentWrite images.[i]
                    | _ -> ()
                Hl.endTransientCommandBlock cb queue pool fence vkc.Device
            | _ -> ()
            
            // make VulkanTexture
            let vulkanTexture =
                { Images_ = images
                  Allocations_ = allocations
                  ImageViews_ = imageViews
                  ImageSizes_ = imageSizes
                  Sampler_ = sampler
                  InternalFormat_ = internalFormat
                  PixelFormat_ = pixelFormat
                  MipLevels_ = mipLevels
                  TextureType_ = textureType }

            // fin
            vulkanTexture

        /// Check that the current texture size is the same as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize metadata (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            if metadata <> vulkanTexture.ImageSize then
                let i = vulkanTexture.CurrentIndex
                Vulkan.vkDestroyImageView (vkc.Device, vulkanTexture.ImageViews_.[i], nullPtr)
                Vma.vmaDestroyImage (vkc.VmaAllocator, vulkanTexture.Images_.[i], vulkanTexture.Allocations_.[i])
                let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
                let (image, allocation) = VulkanTexture.createImage vulkanTexture.Format extent vulkanTexture.MipLevels vulkanTexture.TextureType_ vkc
                vulkanTexture.Images_.[i] <- image
                vulkanTexture.Allocations_.[i] <- allocation
                vulkanTexture.ImageViews_.[i] <- Hl.createImageView vulkanTexture.PixelFormat_.IsBgra vulkanTexture.Format vulkanTexture.MipLevels vulkanTexture.TextureType_.IsTextureCubeMap image vkc.Device
                match vulkanTexture.TextureType_ with
                | TextureAttachmentColor ->
                    let (queue, pool, fence) = TextureLoadThread.getResources RenderThread vkc
                    let cb = Hl.beginTransientCommandBlock pool vkc.Device
                    match vulkanTexture.TextureType_ with
                    | TextureAttachmentColor -> Hl.recordTransitionLayout cb true 1 0 Hl.Undefined Hl.ColorAttachmentWrite vulkanTexture.Images_.[i]
                    | _ -> ()
                    Hl.endTransientCommandBlock cb queue pool fence vkc.Device
                | _ -> ()
                vulkanTexture.ImageSizes_.[i] <- metadata
        
        /// Upload pixel data to VulkanTexture. Can only be done once.
        static member upload metadata mipLevel layer pixels thread (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            match vulkanTexture.TextureType_ with
            | TextureGeneral | TextureCubeMap ->
                let uploadSize = Hl.ImageFormat.getImageSize metadata.TextureWidth metadata.TextureHeight vulkanTexture.InternalFormat_
                let stagingBuffer = Buffer.Buffer.stageData uploadSize pixels vkc
                let (queue, pool, fence) = TextureLoadThread.getResources thread vkc
                let cb = Hl.beginTransientCommandBlock pool vkc.Device
                RecordBufferToImageCopy cb metadata.TextureWidth metadata.TextureHeight mipLevel layer stagingBuffer.VkBuffer vulkanTexture.Image
                Hl.endTransientCommandBlock cb queue pool fence vkc.Device
                Buffer.Buffer.destroy stagingBuffer vkc
            | TextureAttachmentColor -> Log.warn "Upload not supported for attachment texture."

        /// Upload array of pixel data to VulkanTexture. Can only be done once.
        static member uploadArray metadata mipLevel layer (array : 'a array) thread vulkanTexture vkc =
            use arrayPin = new ArrayPin<_> (array)
            VulkanTexture.upload metadata mipLevel layer arrayPin.NativeInt thread vulkanTexture vkc
        
        /// Generate mipmaps in VulkanTexture. Can only be done once, after upload to (only) mipLevel 0.
        static member generateMipmaps metadata layer thread (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            if vulkanTexture.MipLevels > 1 then
                let (queue, pool, fence) = TextureLoadThread.getResources thread vkc
                let cb = Hl.beginTransientCommandBlock pool vkc.Device
                RecordGenerateMipmaps cb metadata.TextureWidth metadata.TextureHeight vulkanTexture.MipLevels layer vulkanTexture.Image
                Hl.endTransientCommandBlock cb queue pool fence vkc.Device
            else Log.warn "Mipmap generation attempted on texture with only one mip level."
        
        /// Create an empty VulkanTexture.
        /// NOTE: DJL: this is for fast empty texture creation. It is not preferred for VulkanTexture.empty, which is created from Assets.Default.Image.
        static member createEmpty (vkc : Hl.VulkanContext) =
            VulkanTexture.create Hl.Rgba VkFilter.Nearest VkFilter.Nearest false MipmapNone TextureGeneral Uncompressed.ImageFormat (TextureMetadata.make 32 32) vkc
        
        /// Destroy VulkanTexture.
        static member destroy (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            Vulkan.vkDestroySampler (vkc.Device, vulkanTexture.Sampler, nullPtr)
            for i in 0 .. dec vulkanTexture.Images_.Length do
                Vulkan.vkDestroyImageView (vkc.Device, vulkanTexture.ImageViews_.[i], nullPtr)
                Vma.vmaDestroyImage (vkc.VmaAllocator, vulkanTexture.Images_.[i], vulkanTexture.Allocations_.[i])

        /// Represents the empty texture used in Vulkan.
        static member empty =
            match EmptyOpt with
            | Some (:? VulkanTexture as empty) -> empty
            | Some _ | None -> failwith "VulkanTexture.empty not initialized properly."
    
    /// Describes data loaded from a texture.
    type TextureData =
        | TextureDataDotNet of Metadata : TextureMetadata * Bytes : byte array
        | TextureDataMipmap of Metadata : TextureMetadata * BlockCompressed : bool * Bytes : byte array * Mipmaps : (Vector2i * byte array) array
        | TextureDataNative of Metadata : TextureMetadata * TextureDataPtr : nativeint * Disposer : IDisposable
        
        /// Whether the texture can be loaded lazily.
        member this.LazyLoadable =
            match this with
            | TextureDataDotNet (_, _) -> false
            | TextureDataMipmap (_, _, _, _) -> true
            | TextureDataNative (_, _, _) -> false
        
        /// The metadata portion of this texture data.
        member this.Metadata =
            match this with
            | TextureDataDotNet (metadata, _) -> metadata
            | TextureDataMipmap (metadata, _, _, _) -> metadata
            | TextureDataNative (metadata, _, _) -> metadata
        
        /// The texture byte data.
        member this.Bytes =
            match this with
            | TextureDataDotNet (_, bytes) -> (false, bytes)
            | TextureDataMipmap (_, compressed, bytes, _) -> (compressed, bytes)
            | TextureDataNative (metadata, textureDataPtr, _) ->
                let bytes = Array.zeroCreate<byte> (metadata.TextureWidth * metadata.TextureHeight * sizeof<uint>)
                Marshal.Copy (textureDataPtr, bytes, 0, bytes.Length)
                (false, bytes)
        
        /// Manual disposal.
        member this.Dispose () =
            match this with
            | TextureDataDotNet (_, _) -> ()
            | TextureDataMipmap (_, _, _, _) -> ()
            | TextureDataNative (_, _, disposer) -> disposer.Dispose ()

    /// Create a Vulkan texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, compression : BlockCompression, textureData, thread, vkc) =

        // upload data to vulkan as appropriate
        match textureData with
        | TextureDataDotNet (metadata, bytes) ->
            let mipmapMode = if mipmaps then MipmapAuto else MipmapNone
            let vulkanTexture = VulkanTexture.create Hl.Bgra minFilter magFilter (anisoFilter && mipmaps) mipmapMode TextureGeneral compression.ImageFormat metadata vkc
            VulkanTexture.uploadArray metadata 0 0 bytes thread vulkanTexture vkc
            if mipmaps then VulkanTexture.generateMipmaps metadata 0 thread vulkanTexture vkc
            (metadata, vulkanTexture)
        | TextureDataMipmap (metadata, blockCompressed, bytes, mipmapBytesArray) ->

            // handle all compression scenarios
            let compression =
                if blockCompressed then
                    if compression.IsUncompressed then Log.info "Potential inadvertent block-compression of texture (place a breakpoint here for more detail)."
                    compression
                else Uncompressed
            
            // if pregenerated mipmap images are available then that determines texture mipmaps, otherwise determined by parameter as usual
            let mipmapMode =
                if mipmapBytesArray.Length > 0 then MipmapManual (mipmapBytesArray.Length + 1)
                elif mipmaps then MipmapAuto else MipmapNone

            // create texture and upload original image
            let vulkanTexture = VulkanTexture.create Hl.Bgra minFilter magFilter (anisoFilter && mipmapMode <> MipmapNone) mipmapMode TextureGeneral compression.ImageFormat metadata vkc
            VulkanTexture.uploadArray metadata 0 0 bytes thread vulkanTexture vkc

            // populate mipmaps as determined
            match mipmapMode with
            | MipmapNone -> ()
            | MipmapManual mipLevels ->
                let mutable mipmapIndex = 0
                while mipmapIndex < mipLevels - 1 do
                    let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
                    let metadata = TextureMetadata.make mipmapResolution.X mipmapResolution.Y
                    VulkanTexture.uploadArray metadata (inc mipmapIndex) 0 mipmapBytes thread vulkanTexture vkc
                    mipmapIndex <- inc mipmapIndex
            | MipmapAuto -> VulkanTexture.generateMipmaps metadata 0 thread vulkanTexture vkc
            
            // fin
            (metadata, vulkanTexture)

        | TextureDataNative (metadata, bytesPtr, disposer) ->
            use _ = disposer
            let mipmapMode = if mipmaps then MipmapAuto else MipmapNone
            let vulkanTexture = VulkanTexture.create Hl.Bgra minFilter magFilter (anisoFilter && mipmaps) mipmapMode TextureGeneral compression.ImageFormat metadata vkc
            VulkanTexture.upload metadata 0 0 bytesPtr thread vulkanTexture vkc
            if mipmaps then VulkanTexture.generateMipmaps metadata 0 thread vulkanTexture vkc
            (metadata, vulkanTexture)

    /// Attempt to create uploadable texture data from the given file path.
    /// Don't forget to dispose the last field when finished with the texture data.
    let TryCreateTextureData (minimal, filePath : string) =
        if File.Exists filePath then

            // attempt to load data as dds (compressed or uncompressed)
            let platform = Environment.OSVersion.Platform
            let fileExtension = PathF.GetExtensionLower filePath
            if fileExtension = ".dds" then
                try let config = PfimConfig (decompress = false)
                    use fileStream = File.OpenRead filePath
                    use dds = Dds.Create (fileStream, config)
                    if dds.Compressed then
                        let (resolution, bytes, mipmapBytesArray) = FormatCompressedPfdds (minimal, dds)
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataMipmap (metadata, true, bytes, mipmapBytesArray))
                    else
                        match TryFormatUncompressedPfimage (minimal, dds) with
                        | Some (resolution, bytes, mipmapBytesArray) ->
                            let metadata = TextureMetadata.make resolution.X resolution.Y
                            Some (TextureDataMipmap (metadata, false, bytes, mipmapBytesArray))
                        | None -> None
                with _ -> None

            // attempt to load data as tga
            elif fileExtension = ".tga" then
                try let image = Pfimage.FromFile filePath
                    match TryFormatUncompressedPfimage (false, image) with
                    | Some (resolution, bytes, _) ->
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataDotNet (metadata, bytes))
                    | None -> None
                with _ -> None

            // attempt to load data as any format supported by Drawing.Bitmap on Windows
            elif platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
                let extension = PathF.GetExtensionLower filePath
                match extension with
                | ImageExtension _ ->
                    try let bitmap = new Drawing.Bitmap (filePath)
                        let data = bitmap.LockBits (Drawing.Rectangle (0, 0, bitmap.Width, bitmap.Height), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                        let metadata = TextureMetadata.make bitmap.Width bitmap.Height
                        let scan0 = data.Scan0
                        Some (TextureDataNative (metadata, scan0, { new IDisposable with member this.Dispose () = bitmap.UnlockBits data; bitmap.Dispose () })) // NOTE: calling UnlockBits explicitly since I can't figure out if Dispose does.
                    with _ -> None
                | _ -> None

            // attempt to load data as any format supported by SDL_image on any device
            else
                let format = SDL.SDL_PIXELFORMAT_ARGB8888 // seems to be the right format on Ubuntu...
                let unconvertedPtr = SDL_image.IMG_Load filePath
                if unconvertedPtr <> nativeint 0 then
                    let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
                    let metadata = TextureMetadata.make unconverted.w unconverted.h
                    let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                    if unconvertedFormat.format <> format then
                        let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                        let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                        SDL.SDL_FreeSurface unconvertedPtr // no longer need this
                        Some (TextureDataNative (metadata, converted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface convertedPtr }))
                    else Some (TextureDataNative (metadata, unconverted.pixels, { new IDisposable with member this.Dispose () = SDL.SDL_FreeSurface unconvertedPtr }))
                else None
        else None

    /// Attempt to create a Vulkan texture from a file.
    let TryCreateTextureVulkan (minimal, minFilter, magFilter, anisoFilter, mipmaps, compression : BlockCompression, filePath, thread, vkc) =
        match TryCreateTextureData (minimal, filePath) with
        | Some textureData ->
            let (metadata, vulkanTexture) = CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, compression, textureData, thread, vkc)
            Right (metadata, vulkanTexture)
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// A texture that's immediately loaded.
    type [<Struct; NoEquality; NoComparison>] EagerTexture =
        { TextureMetadata : TextureMetadata
          VulkanTexture : VulkanTexture }
        
        /// Destroy this texture's backing Vulkan texture.
        member this.Destroy vkc =
            VulkanTexture.destroy this.VulkanTexture vkc

    /// A texture that can be loaded from another thread.
    type LazyTexture (filePath : string, minimalMetadata : TextureMetadata, minimalVulkanTexture : VulkanTexture, fullMinFilter : VkFilter, fullMagFilter : VkFilter, fullAnisoFilter) =
    
        let [<VolatileField>] mutable fullServeAttempted = false
        let [<VolatileField>] mutable fullMetadataAndVulkanTextureOpt = ValueNone
        let [<VolatileField>] mutable destroyed = false
        let destructionLock = obj ()

        (* Client API - only the client may call this! *)

        member this.FilePath =
            if destroyed then failwith "Accessing field of destroyed texture."
            filePath

        member this.TextureMetadata =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndVulkanTextureOpt with
                | ValueSome (metadata, _) -> metadata
                | ValueNone -> minimalMetadata
            else minimalMetadata

        member this.VulkanTexture =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndVulkanTextureOpt with
                | ValueSome (_, vulkanTexture) -> vulkanTexture
                | ValueNone -> minimalVulkanTexture
            else minimalVulkanTexture

        member this.Destroy vkc =
            lock destructionLock $ fun () ->
                if not destroyed then
                    VulkanTexture.destroy minimalVulkanTexture vkc
                    if fullServeAttempted then
                        match fullMetadataAndVulkanTextureOpt with
                        | ValueSome (_, fullVulkanTexture) ->
                            VulkanTexture.destroy fullVulkanTexture vkc
                            fullMetadataAndVulkanTextureOpt <- ValueNone
                        | ValueNone -> ()
                        fullServeAttempted <- false
                    destroyed <- true

        (* Server API - only the server may call this! *)

        member internal this.TryServe vkc =
            lock destructionLock $ fun () ->
                if not destroyed && not fullServeAttempted then
                    match TryCreateTextureVulkan (false, fullMinFilter, fullMagFilter, fullAnisoFilter, false, InferCompression filePath, filePath, TextureStreamingThread, vkc) with
                    | Right (metadata, vulkanTexture) -> fullMetadataAndVulkanTextureOpt <- ValueSome (metadata, vulkanTexture)
                    | Left error -> Log.info ("Could not serve lazy texture due to:" + error)
                    fullServeAttempted <- true

    /// A 2d texture.
    type [<CustomEquality; NoComparison>] Texture =
        | EmptyTexture
        | EagerTexture of EagerTexture
        | LazyTexture of LazyTexture
        
        static member hash texture =
            match texture with
            | EmptyTexture -> 0
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture.GetHashCode ()
            | LazyTexture lazyTexture -> lazyTexture.GetHashCode ()

        static member equals this that =
            match this with
            | EmptyTexture ->
                match that with
                | EmptyTexture -> true
                | _ -> false
            | EagerTexture eagerThis ->
                match that with
                | EagerTexture eagerThat -> eagerThis.VulkanTexture.TextureId = eagerThat.VulkanTexture.TextureId
                | _ -> false
            | LazyTexture lazyThis ->
                match that with
                | LazyTexture lazyThat -> lazyThis = lazyThat
                | _ -> false
        
        member this.TextureMetadata =
            match this with
            | EmptyTexture -> TextureMetadata.empty
            | EagerTexture eagerTexture -> eagerTexture.TextureMetadata
            | LazyTexture lazyTexture -> lazyTexture.TextureMetadata
        
        member this.Image =
            match this with
            | EmptyTexture -> VulkanTexture.empty.Image
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture.Image
            | LazyTexture lazyTexture -> lazyTexture.VulkanTexture.Image
        
        member this.ImageView =
            match this with
            | EmptyTexture -> VulkanTexture.empty.ImageView
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture.ImageView
            | LazyTexture lazyTexture -> lazyTexture.VulkanTexture.ImageView

        member this.Sampler =
            match this with
            | EmptyTexture -> VulkanTexture.empty.Sampler
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture.Sampler
            | LazyTexture lazyTexture -> lazyTexture.VulkanTexture.Sampler

        member this.Format =
            match this with
            | EmptyTexture -> VulkanTexture.empty.Format
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture.Format
            | LazyTexture lazyTexture -> lazyTexture.VulkanTexture.Format
        
        member this.Destroy vkc =
            match this with
            | EmptyTexture -> () // TODO: DJL: protect VulkanTexture.empty from premature destruction.
            | EagerTexture eagerTexture -> eagerTexture.Destroy vkc
            | LazyTexture lazyTexture -> lazyTexture.Destroy vkc

        /// Check that the current texture size is the same as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize metadata texture vkc =
            match texture with
            | EmptyTexture -> ()
            | EagerTexture eagerTexture -> VulkanTexture.updateSize metadata eagerTexture.VulkanTexture vkc
            | LazyTexture lazyTexture -> VulkanTexture.updateSize metadata lazyTexture.VulkanTexture vkc
        
        override this.GetHashCode () =
            Texture.hash this

        override this.Equals that =
            match that with
            | :? Texture as texture -> Texture.equals this texture
            | _ -> false

        interface System.IEquatable<Texture> with
            member this.Equals that =
                Texture.equals this that

    /// An abstraction for managing dynamically generated unfiltered Textures accumulated over multiple renders within a frame.
    type TextureAccumulator =
        private
            { StagingBuffers : Buffer.Buffer
              Textures : Texture List array
              mutable StagingBufferSize : int
              InternalFormat : Hl.ImageFormat
              PixelFormat : Hl.PixelFormat }

        /// Get Texture at index.
        member this.Item index = this.Textures.[Hl.CurrentFrame].[index]

        /// Stage pixels and record transfer commands.
        static member load index cb metadata pixels textureAccumulator vkc =
            
            // enlarge staging buffer size if needed
            let imageSize = Hl.ImageFormat.getImageSize metadata.TextureWidth metadata.TextureHeight textureAccumulator.InternalFormat
            while imageSize > textureAccumulator.StagingBufferSize do textureAccumulator.StagingBufferSize <- textureAccumulator.StagingBufferSize * 2
            Buffer.Buffer.update index textureAccumulator.StagingBufferSize textureAccumulator.StagingBuffers vkc

            // stage pixels
            Buffer.Buffer.upload index 0 imageSize pixels textureAccumulator.StagingBuffers vkc

            // create texture
            let texture = VulkanTexture.create textureAccumulator.PixelFormat VkFilter.Nearest VkFilter.Nearest false MipmapNone TextureGeneral textureAccumulator.InternalFormat metadata vkc

            // add texture to index, destroying existing texture if present and expanding list as necessary
            if index < textureAccumulator.Textures.[Hl.CurrentFrame].Count then
                textureAccumulator.Textures.[Hl.CurrentFrame].[index].Destroy vkc
                textureAccumulator.Textures.[Hl.CurrentFrame].[index] <- EagerTexture { TextureMetadata = metadata; VulkanTexture = texture }
            else 
                // fill gaps if index has been skipped for some reason
                while index > textureAccumulator.Textures.[Hl.CurrentFrame].Count do
                    textureAccumulator.Textures.[Hl.CurrentFrame].Add (EagerTexture { TextureMetadata = TextureMetadata.empty; VulkanTexture = (VulkanTexture.createEmpty vkc) })
                textureAccumulator.Textures.[Hl.CurrentFrame].Add (EagerTexture { TextureMetadata = metadata; VulkanTexture = texture })
            
            // record commands to transfer staged image to the texture
            RecordBufferToImageCopy cb metadata.TextureWidth metadata.TextureHeight 0 0 textureAccumulator.StagingBuffers.[index] texture.Image

        /// Create TextureAccumulator.
        static member create pixelFormat (internalFormat : Hl.ImageFormat) vkc =
            
            // create the resources
            // TODO: DJL: choose appropriate starting size to minimize most probable upsizing.
            let stagingBufferSize = 4096
            let stagingBuffers = Buffer.Buffer.create stagingBufferSize (Buffer.Staging true) vkc
            let textures = Array.zeroCreate<List<Texture>> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec textures.Length do textures.[i] <- List ()

            // make TextureAccumulator
            let textureAccumulator =
                { StagingBuffers = stagingBuffers
                  Textures = textures
                  StagingBufferSize = stagingBufferSize
                  InternalFormat = internalFormat
                  PixelFormat = pixelFormat }

            // fin
            textureAccumulator
        
        /// Destroy TextureAccumulator.
        static member destroy textureAccumulator vkc =
            Buffer.Buffer.destroy textureAccumulator.StagingBuffers vkc
            for i in 0 .. dec textureAccumulator.Textures.Length do
                for j in 0 .. dec textureAccumulator.Textures.[i].Count do
                    textureAccumulator.Textures.[i].[j].Destroy vkc
    
    /// Memoizes and optionally threads texture loads.
    type TextureClient (lazyTextureQueuesOpt : ConcurrentDictionary<_, _> option) =
        let textures = Dictionary<string, Texture> HashIdentity.Structural
        let lazyTextureQueue = ConcurrentQueue ()
        do  match lazyTextureQueuesOpt with
            | Some lazyTextureQueues -> lazyTextureQueues.TryAdd (lazyTextureQueue, lazyTextureQueue) |> ignore<bool>
            | None -> ()

        /// Memoized textures.
        member this.Textures = textures

        /// Lazy texture queue.
        member this.LazyTextureQueue = lazyTextureQueue

        /// Attempt to create a memoized texture from a file.
        member this.TryCreateTexture (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, compression, filePath : string, thread, vkc) =

            // memoize texture
            match textures.TryGetValue filePath with
            | (false, _) ->

                // attempt to create texture
                match TryCreateTextureVulkan (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, compression, filePath, thread, vkc) with
                | Right (metadata, vulkanTexture) ->
                    let texture =
                        if desireLazy && PathF.GetExtensionLower filePath = ".dds" then
                            let lazyTexture = new LazyTexture (filePath, metadata, vulkanTexture, minFilter, magFilter, anisoFilter)
                            lazyTextureQueue.Enqueue lazyTexture
                            LazyTexture lazyTexture
                        else EagerTexture { TextureMetadata = metadata; VulkanTexture = vulkanTexture}
                    textures.Add (filePath, texture)
                    Right texture
                | Left error -> Left error

            // already exists
            | (true, texture) -> Right texture

        /// Attempt to create a filtered memoized texture from a file.
        member this.TryCreateTextureFiltered (desireLazy, compression, filePath, thread, vkc) =
            this.TryCreateTexture (desireLazy, VkFilter.Linear, VkFilter.Linear, true, true, compression, filePath, thread, vkc)
        
        /// Attempt to create an unfiltered memoized texture from a file.
        member this.TryCreateTextureUnfiltered (desireLazy, filePath, thread, vkc) =
            this.TryCreateTexture (desireLazy, VkFilter.Nearest, VkFilter.Nearest, false, false, Uncompressed, filePath, thread, vkc)

    /// Populate the vulkan textures and handles of lazy textures in a threaded manner.
    /// TODO: abstract this to interface that can represent either inline or threaded implementation.
    type TextureServer (lazyTextureQueues : ConcurrentDictionary<LazyTexture ConcurrentQueue, LazyTexture ConcurrentQueue>, vkc) =
        let mutable threadOpt = None
        let [<VolatileField>] mutable started = false
        let [<VolatileField>] mutable terminated = false

        member private this.Run () =
            started <- true
            while not terminated do
                let batchTime = Stopwatch.StartNew () // NOTE: we stop loading after 1/2 frame passed so far.
                let desiredFrameTimeMinimumMs = GameTime.DesiredFrameTimeMinimum * 1000.0
                let frameTimeOutMs = max 4L (int64 (desiredFrameTimeMinimumMs * 0.5))
                let lazyTextureQueueEnr = lazyTextureQueues.GetEnumerator ()
                while not terminated && batchTime.ElapsedMilliseconds < frameTimeOutMs && lazyTextureQueueEnr.MoveNext () do
                    let lazyTextureQueue = lazyTextureQueueEnr.Current.Key
                    let mutable lazyTexture = Unchecked.defaultof<_>
                    while not terminated && batchTime.ElapsedMilliseconds < frameTimeOutMs && lazyTextureQueue.TryDequeue &lazyTexture do
                        lazyTexture.TryServe vkc
                Thread.Sleep (max 1 (int desiredFrameTimeMinimumMs - int batchTime.ElapsedMilliseconds + 1))

        member this.Start () =
            if not started then
                let thread =
                    Thread (ThreadStart (fun () ->
                        try this.Run ()
                        with _ -> Environment.Exit Constants.Engine.ExitCodeFailure))
                threadOpt <- Some thread
                thread.IsBackground <- true
                thread.Start ()
                while not started do Thread.Yield () |> ignore<bool>

        member this.Terminate () =
            if started && not terminated then
                let thread = Option.get threadOpt
                terminated <- true
                thread.Join ()
                threadOpt <- None