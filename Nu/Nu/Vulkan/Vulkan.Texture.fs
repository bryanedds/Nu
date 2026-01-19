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
    
    /// Determines whether a texture is intended as an attachment, what sort, and whether to parallelize for frames in flight.
    type AttachmentMode =
        | AttachmentNone
        | AttachmentColor of IsParallel : bool
        | AttachmentDepth of IsParallel : bool

        /// Whether the type must be paralleled for frames in flight.
        member this.IsParallel =
            match this with
            | AttachmentNone -> false
            | AttachmentColor isParallel -> isParallel // 'parallel' is taken!
            | AttachmentDepth isParallel -> isParallel

        /// The VkImageAspectFlags for a given attachment mode. This is because, unlike layout transitions, image view creation only needs
        /// the *intended* image aspect e.g. depth, not every aspect contained in the format e.g. depth & stencil.
        member this.VkImageAspectFlags =
            match this with
            | AttachmentNone
            | AttachmentColor _ -> VkImageAspectFlags.Color
            | AttachmentDepth _ -> VkImageAspectFlags.Depth
    
    /// Determines how a texture is configured based on intended usage.
    type TextureType =
        | Texture2d
        | TextureCubeMap

        /// The number of layers.
        member this.Layers =
            match this with
            | Texture2d -> 1
            | TextureCubeMap -> 6
        
        /// The VkImageViewType for a given type.
        member this.VkImageViewType =
            match this with
            | Texture2d -> VkImageViewType.Image2D
            | TextureCubeMap -> VkImageViewType.ImageCube

    /// Record command to copy buffer to image.
    let private RecordBufferToImageCopy (cb, width, height, mipLevel, layer, vkBuffer, vkImage) =
        Hl.recordTransitionLayout cb false mipLevel layer VkImageAspectFlags.Color Hl.UndefinedHost Hl.TransferDst vkImage
        let mutable region = VkBufferImageCopy ()
        region.imageSubresource <- Hl.makeSubresourceLayers mipLevel layer VkImageAspectFlags.Color
        region.imageExtent <- VkExtent3D (width, height, 1)
        Vulkan.vkCmdCopyBufferToImage
            (cb, vkBuffer, vkImage,
             Hl.TransferDst.VkImageLayout,
             1u, asPointer &region)
        Hl.recordTransitionLayout cb false mipLevel layer VkImageAspectFlags.Color Hl.TransferDst Hl.ShaderRead vkImage
    
    /// Record commands to generate mipmaps.
    let private RecordGenerateMipmaps (cb, width, height, mipLevels, layer, vkImage) =
        
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
        barrier.subresourceRange <- Hl.makeSubresourceRange 1 (mipLevels - 1) layer 1 VkImageAspectFlags.Color
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
    type [<CustomEquality; NoComparison>] TextureInternal =
        private
            { Images_ : VkImage array
              Allocations_ : VmaAllocation array
              ImageViews_ : VkImageView array
              ImageSizes_ : TextureMetadata array
              Sampler_ : VkSampler
              InternalFormat_ : Hl.ImageFormat
              PixelFormat_ : Hl.PixelFormat
              MipLevels_ : int
              AttachmentMode_ : AttachmentMode
              TextureType_ : TextureType }

        member private this.IsParallel = this.AttachmentMode_.IsParallel
        member private this.CurrentIndex = if this.IsParallel then Hl.CurrentFrame else 0
        member private this.ImageSize = this.ImageSizes_.[this.CurrentIndex]
        
        /// The unique texture id.
        /// TODO: DJL: review, should probably just generate Id since images can now be recreated.
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
            | :? TextureInternal as that -> this.TextureId = that.TextureId
            | _ -> false

        override this.GetHashCode () = 
            hash this.TextureId

        /// Determine which image usage flags are needed, without limiting flexibility.
        static member private determineImageUsage (mipmapMode : MipmapMode) (attachmentMode : AttachmentMode) =

            // collect any usage flags determined as needed
            let imageUsages = List ()
            if attachmentMode.IsAttachmentColor || mipmapMode.IsMipmapAuto then imageUsages.Add VkImageUsageFlags.TransferSrc
            if attachmentMode.IsAttachmentNone then imageUsages.Add VkImageUsageFlags.TransferDst
            if not attachmentMode.IsAttachmentDepth then imageUsages.Add VkImageUsageFlags.Sampled
            if attachmentMode.IsAttachmentColor then imageUsages.Add VkImageUsageFlags.ColorAttachment
            if attachmentMode.IsAttachmentDepth then imageUsages.Add VkImageUsageFlags.DepthStencilAttachment

            // bitwise-or flags together 
            let mutable imageUsagesOred = VkImageUsageFlags.None
            let imageUsagesArray = imageUsages.ToArray ()
            for i in 0 .. dec imageUsagesArray.Length do imageUsagesOred <- imageUsagesOred ||| imageUsagesArray.[i]
            imageUsagesOred
        
        /// Create the image.
        static member private createImage format extent mipLevels (textureType : TextureType) usageFlags (vkc : Hl.VulkanContext) =
            let mutable iInfo = VkImageCreateInfo ()
            if textureType.IsTextureCubeMap then
                iInfo.flags <- VkImageCreateFlags.CubeCompatible
            iInfo.imageType <- VkImageType.Image2D
            iInfo.format <- format
            iInfo.extent <- extent
            iInfo.mipLevels <- uint mipLevels
            iInfo.arrayLayers <- uint textureType.Layers
            iInfo.samples <- VkSampleCountFlags.Count1
            iInfo.tiling <- VkImageTiling.Optimal
            iInfo.usage <- usageFlags
            iInfo.sharingMode <- VkSharingMode.Exclusive
            iInfo.initialLayout <- Hl.UndefinedHost.VkImageLayout
            let aInfo = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (vkc.VmaAllocator, &iInfo, &aInfo, &image, &allocation, nullPtr) |> Hl.check
            (image, allocation)
        
        /// Create the sampler.
        static member private createSampler addressMode minFilter magFilter anisoFilter (vkc : Hl.VulkanContext) =
            let mutable info = VkSamplerCreateInfo ()
            info.magFilter <- magFilter
            info.minFilter <- minFilter
            info.mipmapMode <- VkSamplerMipmapMode.Linear
            info.addressModeU <- addressMode
            info.addressModeV <- addressMode
            info.addressModeW <- addressMode
            if anisoFilter then
                info.anisotropyEnable <- true
                info.maxAnisotropy <- min vkc.MaxAnisotropy Constants.Render.TextureAnisotropyMax
            info.maxLod <- Vulkan.VK_LOD_CLAMP_NONE
            let mutable sampler = Unchecked.defaultof<VkSampler>
            Vulkan.vkCreateSampler (vkc.Device, &info, nullPtr, &sampler) |> Hl.check
            sampler
        
        /// Create a TextureInternal.
        static member create
            (pixelFormat : Hl.PixelFormat)
            addressMode
            minFilter
            magFilter
            anisoFilter
            mipmapMode
            (attachmentMode : AttachmentMode)
            (textureType : TextureType)
            (internalFormat : Hl.ImageFormat)
            metadata
            (vkc : Hl.VulkanContext) =

            // determine mip levels
            let mipLevels =
                match attachmentMode with
                | AttachmentNone ->
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

                | AttachmentColor _
                | AttachmentDepth _ ->
                    if not mipmapMode.IsMipmapNone then Log.warn "Mipmaps not supported for attachment texture."
                    1
            
            // create images, image views and sampler
            let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            let length = if attachmentMode.IsParallel then Constants.Vulkan.MaxFramesInFlight else 1
            let usageFlags = TextureInternal.determineImageUsage mipmapMode attachmentMode
            let images = Array.zeroCreate<VkImage> length
            let allocations = Array.zeroCreate<VmaAllocation> length
            let imageViews = Array.zeroCreate<VkImageView> length
            let imageSizes = Array.zeroCreate<TextureMetadata> length
            for i in 0 .. dec length do
                let (image, allocation) = TextureInternal.createImage internalFormat.VkFormat extent mipLevels textureType usageFlags vkc
                images.[i] <- image
                allocations.[i] <- allocation
                imageViews.[i] <- Hl.createImageView pixelFormat internalFormat.VkFormat mipLevels textureType.Layers textureType.VkImageViewType attachmentMode.VkImageAspectFlags image vkc.Device
                imageSizes.[i] <- metadata
            
            // TODO: DJL: just for now, depth texture does not use sampler, but for simplicity we make one anyway.
            let sampler = TextureInternal.createSampler addressMode minFilter magFilter anisoFilter vkc
            
            // transition layout as appropriate
            match attachmentMode with
            | AttachmentColor _
            | AttachmentDepth _ ->
                let (queue, pool, fence) = TextureLoadThread.getResources RenderThread vkc
                let cb = Hl.initCommandBufferTransient pool vkc.Device
                for i in 0 .. dec length do
                    match attachmentMode with
                    | AttachmentColor _ -> Hl.recordTransitionLayout cb true 1 0 internalFormat.VkImageAspectFlags Hl.Undefined Hl.ColorAttachmentWrite images.[i]
                    | AttachmentDepth _ -> Hl.recordTransitionLayout cb true 1 0 internalFormat.VkImageAspectFlags Hl.Undefined Hl.DepthAttachment images.[i]
                    | _ -> ()
                Hl.Queue.executeTransient cb pool fence queue vkc.Device
            | _ -> ()
            
            // make TextureInternal
            let textureInternal =
                { Images_ = images
                  Allocations_ = allocations
                  ImageViews_ = imageViews
                  ImageSizes_ = imageSizes
                  Sampler_ = sampler
                  InternalFormat_ = internalFormat
                  PixelFormat_ = pixelFormat
                  MipLevels_ = mipLevels
                  AttachmentMode_ = attachmentMode
                  TextureType_ = textureType }

            // fin
            textureInternal

        /// Check that the current texture size is the same as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize metadata (textureInternal : TextureInternal) (vkc : Hl.VulkanContext) =
            if metadata <> textureInternal.ImageSize then
                let i = textureInternal.CurrentIndex
                Vulkan.vkDestroyImageView (vkc.Device, textureInternal.ImageViews_.[i], nullPtr)
                Vma.vmaDestroyImage (vkc.VmaAllocator, textureInternal.Images_.[i], textureInternal.Allocations_.[i])
                let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
                let usageFlags = TextureInternal.determineImageUsage MipmapNone textureInternal.AttachmentMode_ // NOTE: DJL: MipmapNone because not supported for attachments.
                let (image, allocation) = TextureInternal.createImage textureInternal.Format extent textureInternal.MipLevels textureInternal.TextureType_ usageFlags vkc
                textureInternal.Images_.[i] <- image
                textureInternal.Allocations_.[i] <- allocation
                textureInternal.ImageViews_.[i] <-
                    Hl.createImageView
                        textureInternal.PixelFormat_
                        textureInternal.Format
                        textureInternal.MipLevels
                        textureInternal.TextureType_.Layers
                        textureInternal.TextureType_.VkImageViewType
                        textureInternal.AttachmentMode_.VkImageAspectFlags
                        image
                        vkc.Device
                match textureInternal.AttachmentMode_ with
                | AttachmentColor _
                | AttachmentDepth _ ->
                    let (queue, pool, fence) = TextureLoadThread.getResources RenderThread vkc
                    let cb = Hl.initCommandBufferTransient pool vkc.Device
                    match textureInternal.AttachmentMode_ with
                    | AttachmentColor _ -> Hl.recordTransitionLayout cb true 1 0 textureInternal.InternalFormat_.VkImageAspectFlags Hl.Undefined Hl.ColorAttachmentWrite textureInternal.Images_.[i]
                    | AttachmentDepth _ -> Hl.recordTransitionLayout cb true 1 0 textureInternal.InternalFormat_.VkImageAspectFlags Hl.Undefined Hl.DepthAttachment textureInternal.Images_.[i]
                    | _ -> ()
                    Hl.Queue.executeTransient cb pool fence queue vkc.Device
                | _ -> ()
                textureInternal.ImageSizes_.[i] <- metadata
        
        /// Upload pixel data to TextureInternal. Can only be done once.
        static member upload metadata mipLevel layer pixels thread (textureInternal : TextureInternal) (vkc : Hl.VulkanContext) =
            match textureInternal.AttachmentMode_ with
            | AttachmentNone ->
                let uploadSize = Hl.ImageFormat.getImageSize metadata.TextureWidth metadata.TextureHeight textureInternal.InternalFormat_
                let stagingBuffer = Buffer.Buffer.stageData uploadSize pixels vkc
                let (queue, pool, fence) = TextureLoadThread.getResources thread vkc
                let cb = Hl.initCommandBufferTransient pool vkc.Device
                RecordBufferToImageCopy (cb, metadata.TextureWidth, metadata.TextureHeight, mipLevel, layer, stagingBuffer.VkBuffer, textureInternal.Image)
                Hl.Queue.executeTransient cb pool fence queue vkc.Device
                Buffer.Buffer.destroy stagingBuffer vkc
            | AttachmentColor _
            | AttachmentDepth _ -> Log.warn "Upload not supported for attachment texture."

        /// Upload array of pixel data to TextureInternal. Can only be done once.
        static member uploadArray metadata mipLevel layer (array : 'a array) thread textureInternal vkc =
            use arrayPin = new ArrayPin<_> (array)
            TextureInternal.upload metadata mipLevel layer arrayPin.NativeInt thread textureInternal vkc
        
        /// Generate mipmaps in TextureInternal. Can only be done once, after upload to (only) mipLevel 0.
        static member generateMipmaps metadata layer thread (textureInternal : TextureInternal) (vkc : Hl.VulkanContext) =
            if textureInternal.MipLevels > 1 then
                let (queue, pool, fence) = TextureLoadThread.getResources thread vkc
                let cb = Hl.initCommandBufferTransient pool vkc.Device
                RecordGenerateMipmaps (cb, metadata.TextureWidth, metadata.TextureHeight, textureInternal.MipLevels, layer, textureInternal.Image)
                Hl.Queue.executeTransient cb pool fence queue vkc.Device
            else Log.warn "Mipmap generation attempted on texture with only one mip level."
        
        /// Create an empty TextureInternal.
        /// NOTE: DJL: this is for fast empty texture creation. It is not preferred for TextureInternal.empty, which is created from Assets.Default.Image.
        static member createEmpty (vkc : Hl.VulkanContext) =
            TextureInternal.create Hl.Rgba VkSamplerAddressMode.Repeat VkFilter.Nearest VkFilter.Nearest false MipmapNone AttachmentNone Texture2d Uncompressed.ImageFormat (TextureMetadata.make 32 32) vkc
        
        /// Destroy TextureInternal.
        static member destroy (textureInternal : TextureInternal) (vkc : Hl.VulkanContext) =
            Vulkan.vkDestroySampler (vkc.Device, textureInternal.Sampler, nullPtr)
            for i in 0 .. dec textureInternal.Images_.Length do
                Vulkan.vkDestroyImageView (vkc.Device, textureInternal.ImageViews_.[i], nullPtr)
                Vma.vmaDestroyImage (vkc.VmaAllocator, textureInternal.Images_.[i], textureInternal.Allocations_.[i])

        /// Represents the empty texture used in Vulkan.
        static member empty =
            match EmptyOpt with
            | Some (:? TextureInternal as empty) -> empty
            | Some _ | None -> failwith "TextureInternal.empty not initialized properly."
    
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
            let textureInternal = TextureInternal.create Hl.Bgra VkSamplerAddressMode.Repeat minFilter magFilter (anisoFilter && mipmaps) mipmapMode AttachmentNone Texture2d compression.ImageFormat metadata vkc
            TextureInternal.uploadArray metadata 0 0 bytes thread textureInternal vkc
            if mipmaps then TextureInternal.generateMipmaps metadata 0 thread textureInternal vkc
            (metadata, textureInternal)
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
            let textureInternal = TextureInternal.create Hl.Bgra VkSamplerAddressMode.Repeat minFilter magFilter (anisoFilter && mipmapMode <> MipmapNone) mipmapMode AttachmentNone Texture2d compression.ImageFormat metadata vkc
            TextureInternal.uploadArray metadata 0 0 bytes thread textureInternal vkc

            // populate mipmaps as determined
            match mipmapMode with
            | MipmapNone -> ()
            | MipmapManual mipLevels ->
                let mutable mipmapIndex = 0
                while mipmapIndex < mipLevels - 1 do
                    let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
                    let metadata = TextureMetadata.make mipmapResolution.X mipmapResolution.Y
                    TextureInternal.uploadArray metadata (inc mipmapIndex) 0 mipmapBytes thread textureInternal vkc
                    mipmapIndex <- inc mipmapIndex
            | MipmapAuto -> TextureInternal.generateMipmaps metadata 0 thread textureInternal vkc
            
            // fin
            (metadata, textureInternal)

        | TextureDataNative (metadata, bytesPtr, disposer) ->
            use _ = disposer
            let mipmapMode = if mipmaps then MipmapAuto else MipmapNone
            let textureInternal = TextureInternal.create Hl.Bgra VkSamplerAddressMode.Repeat minFilter magFilter (anisoFilter && mipmaps) mipmapMode AttachmentNone Texture2d compression.ImageFormat metadata vkc
            TextureInternal.upload metadata 0 0 bytesPtr thread textureInternal vkc
            if mipmaps then TextureInternal.generateMipmaps metadata 0 thread textureInternal vkc
            (metadata, textureInternal)

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
            let (metadata, textureInternal) = CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, compression, textureData, thread, vkc)
            Right (metadata, textureInternal)
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// A texture that's immediately loaded.
    type [<Struct; NoEquality; NoComparison>] EagerTexture =
        { TextureMetadata : TextureMetadata
          TextureInternal : TextureInternal }
        
        /// Destroy this texture's backing Vulkan texture.
        member this.Destroy vkc =
            TextureInternal.destroy this.TextureInternal vkc

    /// A texture that can be loaded from another thread.
    type LazyTexture (filePath : string, minimalMetadata : TextureMetadata, minimalTextureInternal : TextureInternal, fullMinFilter : VkFilter, fullMagFilter : VkFilter, fullAnisoFilter) =
    
        let [<VolatileField>] mutable fullServeAttempted = false
        let [<VolatileField>] mutable fullMetadataAndTextureInternalOpt = ValueNone
        let [<VolatileField>] mutable destroyed = false
        let destructionLock = obj ()

        (* Client API - only the client may call this! *)

        member this.FilePath =
            if destroyed then failwith "Accessing field of destroyed texture."
            filePath

        member this.TextureMetadata =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndTextureInternalOpt with
                | ValueSome (metadata, _) -> metadata
                | ValueNone -> minimalMetadata
            else minimalMetadata

        member this.TextureInternal =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndTextureInternalOpt with
                | ValueSome (_, textureInternal) -> textureInternal
                | ValueNone -> minimalTextureInternal
            else minimalTextureInternal

        member this.Destroy vkc =
            lock destructionLock $ fun () ->
                if not destroyed then
                    TextureInternal.destroy minimalTextureInternal vkc
                    if fullServeAttempted then
                        match fullMetadataAndTextureInternalOpt with
                        | ValueSome (_, fullTextureInternal) ->
                            TextureInternal.destroy fullTextureInternal vkc
                            fullMetadataAndTextureInternalOpt <- ValueNone
                        | ValueNone -> ()
                        fullServeAttempted <- false
                    destroyed <- true

        (* Server API - only the server may call this! *)

        member internal this.TryServe vkc =
            lock destructionLock $ fun () ->
                if not destroyed && not fullServeAttempted then
                    match TryCreateTextureVulkan (false, fullMinFilter, fullMagFilter, fullAnisoFilter, false, InferCompression filePath, filePath, TextureStreamingThread, vkc) with
                    | Right (metadata, textureInternal) -> fullMetadataAndTextureInternalOpt <- ValueSome (metadata, textureInternal)
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
            | EagerTexture eagerTexture -> eagerTexture.TextureInternal.GetHashCode () // TODO: DJL: GetHashCode for eager?
            | LazyTexture lazyTexture -> lazyTexture.GetHashCode ()

        static member equals this that =
            match this with
            | EmptyTexture ->
                match that with
                | EmptyTexture -> true
                | _ -> false
            | EagerTexture eagerThis ->
                match that with
                | EagerTexture eagerThat -> eagerThis.TextureInternal.TextureId = eagerThat.TextureInternal.TextureId
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
            | EmptyTexture -> TextureInternal.empty.Image
            | EagerTexture eagerTexture -> eagerTexture.TextureInternal.Image
            | LazyTexture lazyTexture -> lazyTexture.TextureInternal.Image
        
        member this.ImageView =
            match this with
            | EmptyTexture -> TextureInternal.empty.ImageView
            | EagerTexture eagerTexture -> eagerTexture.TextureInternal.ImageView
            | LazyTexture lazyTexture -> lazyTexture.TextureInternal.ImageView

        member this.Sampler =
            match this with
            | EmptyTexture -> TextureInternal.empty.Sampler
            | EagerTexture eagerTexture -> eagerTexture.TextureInternal.Sampler
            | LazyTexture lazyTexture -> lazyTexture.TextureInternal.Sampler

        member this.Format =
            match this with
            | EmptyTexture -> TextureInternal.empty.Format
            | EagerTexture eagerTexture -> eagerTexture.TextureInternal.Format
            | LazyTexture lazyTexture -> lazyTexture.TextureInternal.Format
        
        member this.Destroy vkc =
            match this with
            | EmptyTexture -> () // TODO: DJL: protect TextureInternal.empty from premature destruction.
            | EagerTexture eagerTexture -> eagerTexture.Destroy vkc
            | LazyTexture lazyTexture -> lazyTexture.Destroy vkc

        /// Check that the current texture size is the same as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize metadata texture vkc =
            match texture with
            | EmptyTexture -> ()
            | EagerTexture eagerTexture -> TextureInternal.updateSize metadata eagerTexture.TextureInternal vkc
            | LazyTexture lazyTexture -> TextureInternal.updateSize metadata lazyTexture.TextureInternal vkc
        
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
            Buffer.Buffer.upload index 0 0 imageSize 1 pixels textureAccumulator.StagingBuffers vkc

            // create texture
            let texture = TextureInternal.create textureAccumulator.PixelFormat VkSamplerAddressMode.Repeat VkFilter.Nearest VkFilter.Nearest false MipmapNone AttachmentNone Texture2d textureAccumulator.InternalFormat metadata vkc

            // add texture to index, destroying existing texture if present and expanding list as necessary
            if index < textureAccumulator.Textures.[Hl.CurrentFrame].Count then
                textureAccumulator.Textures.[Hl.CurrentFrame].[index].Destroy vkc
                textureAccumulator.Textures.[Hl.CurrentFrame].[index] <- EagerTexture { TextureMetadata = metadata; TextureInternal = texture }
            else 
                // fill gaps if index has been skipped for some reason
                while index > textureAccumulator.Textures.[Hl.CurrentFrame].Count do
                    textureAccumulator.Textures.[Hl.CurrentFrame].Add (EagerTexture { TextureMetadata = TextureMetadata.empty; TextureInternal = (TextureInternal.createEmpty vkc) })
                textureAccumulator.Textures.[Hl.CurrentFrame].Add (EagerTexture { TextureMetadata = metadata; TextureInternal = texture })
            
            // record commands to transfer staged image to the texture
            RecordBufferToImageCopy (cb, metadata.TextureWidth, metadata.TextureHeight, 0, 0, textureAccumulator.StagingBuffers.[index], texture.Image)

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
                | Right (metadata, textureInternal) ->
                    let texture =
                        if desireLazy && PathF.GetExtensionLower filePath = ".dds" then
                            let lazyTexture = new LazyTexture (filePath, metadata, textureInternal, minFilter, magFilter, anisoFilter)
                            lazyTextureQueue.Enqueue lazyTexture
                            LazyTexture lazyTexture
                        else EagerTexture { TextureMetadata = metadata; TextureInternal = textureInternal}
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