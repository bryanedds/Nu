// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open OpenGL
open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
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
    /// TODO: see if instead of exposing this directly, we should define Init and CleanUp fns.
    let mutable internal EmptyOpt : obj option = None

    /// Check that an asset with the given file path should be filtered in a 2D rendering context.
    let Filtered2d (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        name.EndsWith "_f" ||
        name.EndsWith "Filtered"
    
    /// Check that an asset with the given file path can utilize block compression (IE, it's not a normal map,
    /// blend map, or specified as uncompressed).
    /// TODO: move this somewhere more general?
    let BlockCompressable (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        not (name.EndsWith "_n") &&
        not (name.EndsWith "_hm") &&
        not (name.EndsWith "_b") &&
        not (name.EndsWith "_t") &&
        not (name.EndsWith "_u") &&
        not (name.EndsWith "Normal") &&
        not (name.EndsWith "HeightMap") &&
        not (name.EndsWith "Blend") &&
        not (name.EndsWith "Tint") &&
        not (name.EndsWith "Uncompressed")

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

    /// The format of a vkImage.
    type ImageFormat =
        | Bgra
        | Rgba

        /// The vkFormat.
        member this.Format =
            match this with
            | Bgra -> Vulkan.VK_FORMAT_B8G8R8A8_UNORM
            | Rgba -> Vulkan.VK_FORMAT_R8G8B8A8_UNORM

        /// The number of bytes per pixel.
        member this.BytesPerPixel =
            match this with
            | Bgra -> 4
            | Rgba -> 4
    
    /// Determines whether a texture has mipmaps, and whether they are handled manually or automatically.
    type MipmapMode =
        | MipmapNone
        | MipmapManual of int // mipmap count
        | MipmapAuto
    
    /// An abstraction of a texture as managed by Vulkan.
    /// TODO: extract sampler out of here.
    type [<CustomEquality; NoComparison>] VulkanTexture =
        private
            { _Image : VkImage
              _Allocation : VmaAllocation
              _ImageView : VkImageView
              _Sampler : VkSampler
              _Format : ImageFormat
              _MipLevels : int }

        /// The unique texture id.
        member this.TextureId = this._Image.Handle
        
        /// The image.
        member this.Image = this._Image

        /// The image view.
        member this.ImageView = this._ImageView

        /// The sampler.
        member this.Sampler = this._Sampler

        /// The image format.
        member this.Format = this._Format

        /// The mip level count.
        member this.MipLevels = this._MipLevels
        
        override this.Equals thatObj =
            match thatObj with
            | :? VulkanTexture as that -> this.TextureId = that.TextureId
            | _ -> false

        override this.GetHashCode () = 
            hash this.TextureId

        /// Create the image.
        static member private createImage format extent mipLevels (vkc : Hl.VulkanContext) =
            
            // prepare for mipmap generation if applicable
            let usage =
                if mipLevels = 1
                then Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
                else Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
                    
            // create image
            let mutable iInfo = VkImageCreateInfo ()
            iInfo.imageType <- Vulkan.VK_IMAGE_TYPE_2D
            iInfo.format <- format
            iInfo.extent <- extent
            iInfo.mipLevels <- uint mipLevels
            iInfo.arrayLayers <- 1u
            iInfo.samples <- Vulkan.VK_SAMPLE_COUNT_1_BIT
            iInfo.tiling <- Vulkan.VK_IMAGE_TILING_OPTIMAL
            iInfo.usage <- usage
            iInfo.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            iInfo.initialLayout <- Hl.UndefinedHost.vkImageLayout
            let aInfo = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (vkc.VmaAllocator, &iInfo, &aInfo, &image, &allocation, nullPtr) |> Hl.check
            (image, allocation)
        
        /// Create the sampler.
        static member private createSampler minFilter magFilter anisoFilter (vkc : Hl.VulkanContext) =
            let mutable info = VkSamplerCreateInfo ()
            info.magFilter <- magFilter
            info.minFilter <- minFilter
            info.mipmapMode <- Vulkan.VK_SAMPLER_MIPMAP_MODE_LINEAR
            info.addressModeU <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            info.addressModeV <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            info.addressModeW <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            if anisoFilter then
                info.anisotropyEnable <- VkBool32.True
                info.maxAnisotropy <- min vkc.MaxAnisotropy Constants.Render.TextureAnisotropyMax
            info.maxLod <- Vulkan.VK_LOD_CLAMP_NONE
            let mutable sampler = Unchecked.defaultof<VkSampler>
            Vulkan.vkCreateSampler (vkc.Device, &info, nullPtr, &sampler) |> Hl.check
            sampler
        
        /// Record command to copy buffer to image.
        static member private recordBufferToImageCopyMinimal cb metadata mipLevel vkBuffer vkImage =
            let mutable region = VkBufferImageCopy ()
            region.imageSubresource <- Hl.makeSubresourceLayersColor mipLevel
            region.imageExtent <- VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            Vulkan.vkCmdCopyBufferToImage
                (cb, vkBuffer, vkImage,
                 Hl.TransferDst.vkImageLayout,
                 1u, asPointer &region)
        
        /// Record commands to generate mipmaps.
        static member private recordGenerateMipmapsInternal cb metadata mipLevels vkImage =
            
            // use single barrier for all transfer operations
            let mutable barrier = VkImageMemoryBarrier ()
            barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.image <- vkImage
            
            // transition mipmap images from undefined as they haven't been touched yet
            barrier.srcAccessMask <- Hl.UndefinedHost.Access
            barrier.dstAccessMask <- Hl.TransferDst.Access
            barrier.oldLayout <- Hl.UndefinedHost.vkImageLayout
            barrier.newLayout <- Hl.TransferDst.vkImageLayout
            barrier.subresourceRange <- Hl.makeSubresourceRangeColor (mipLevels - 1)
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
            barrier.oldLayout <- Hl.ShaderRead.vkImageLayout
            barrier.newLayout <- Hl.TransferDst.vkImageLayout
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
            let mutable mipWidth = int metadata.TextureWidth
            let mutable mipHeight = int metadata.TextureHeight

            for i in 1 .. dec mipLevels do
                
                // transition layout of previous image to be copied from
                barrier.srcAccessMask <- Hl.TransferDst.Access
                barrier.dstAccessMask <- Hl.TransferSrc.Access
                barrier.oldLayout <- Hl.TransferDst.vkImageLayout
                barrier.newLayout <- Hl.TransferSrc.vkImageLayout
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
                let mutable blit = VkImageBlit ()
                blit.srcSubresource <- Hl.makeSubresourceLayersColor (i - 1)
                blit.srcOffsets <- NativePtr.writeArrayToFixedBuffer [|VkOffset3D.Zero; VkOffset3D (mipWidth, mipHeight, 1)|] blit.srcOffsets
                blit.dstSubresource <- Hl.makeSubresourceLayersColor i
                blit.dstOffsets <- NativePtr.writeArrayToFixedBuffer [|VkOffset3D.Zero; VkOffset3D (nextWidth, nextHeight, 1)|] blit.dstOffsets
                Vulkan.vkCmdBlitImage
                    (cb,
                     vkImage, Hl.TransferSrc.vkImageLayout,
                     vkImage, Hl.TransferDst.vkImageLayout,
                     1u, asPointer &blit, Vulkan.VK_FILTER_LINEAR)
                
                // transition layout of previous image to be read by shader
                barrier.srcAccessMask <- Hl.TransferSrc.Access
                barrier.dstAccessMask <- Hl.ShaderRead.Access
                barrier.oldLayout <- Hl.TransferSrc.vkImageLayout
                barrier.newLayout <- Hl.ShaderRead.vkImageLayout
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
            barrier.oldLayout <- Hl.TransferDst.vkImageLayout
            barrier.newLayout <- Hl.ShaderRead.vkImageLayout
            barrier.subresourceRange.baseMipLevel <- uint (mipLevels - 1)
            Vulkan.vkCmdPipelineBarrier
                (cb,
                 Hl.TransferDst.PipelineStage,
                 Hl.ShaderRead.PipelineStage,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &barrier)
        
        /// Create a VulkanTexture.
        static member create (format : ImageFormat) minFilter magFilter anisoFilter mipmapMode metadata (vkc : Hl.VulkanContext) =

            // determine mip levels
            let mipLevels =
                match mipmapMode with
                | MipmapNone -> 1
                | MipmapManual mips -> mips
                | MipmapAuto ->
                    
                    // check if hardware supports mipmap generation; this is done here to prevent unused (i.e. blank) mip levels
                    let mutable formatProperties = Unchecked.defaultof<VkFormatProperties>
                    Vulkan.vkGetPhysicalDeviceFormatProperties (vkc.PhysicalDevice, format.Format, &formatProperties)
                    let mipGenSupport = formatProperties.optimalTilingFeatures &&& VkFormatFeatureFlags.SampledImageFilterLinear <> VkFormatFeatureFlags.None
                    
                    // calculate mip levels
                    if mipGenSupport then max metadata.TextureWidth metadata.TextureHeight |> Math.Log2 |> floor |> inc |> int
                    else Log.infoOnce "Graphics device does not support mipmap generation for some used image format(s)."; 1
            
            // create image, image view and sampler
            let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            let (image, allocation) = VulkanTexture.createImage format.Format extent mipLevels vkc
            let imageView = Hl.createImageView format.Format mipLevels image vkc.Device
            let sampler = VulkanTexture.createSampler minFilter magFilter anisoFilter vkc
            
            // make VulkanTexture
            let vulkanTexture =
                { _Image = image
                  _Allocation = allocation
                  _ImageView = imageView
                  _Sampler = sampler
                  _Format = format
                  _MipLevels = mipLevels }

            // fin
            vulkanTexture

        /// Record commands to copy from buffer to image.
        static member recordBufferToImageCopy cb metadata mipLevel vkBuffer vkImage =
            Hl.recordTransitionLayout cb false mipLevel Hl.UndefinedHost Hl.TransferDst vkImage
            VulkanTexture.recordBufferToImageCopyMinimal cb metadata mipLevel vkBuffer vkImage
            Hl.recordTransitionLayout cb false mipLevel Hl.TransferDst Hl.ShaderRead vkImage
        
        /// Record commands to generate mipmaps.
        static member recordGenerateMipmaps cb metadata mipLevels vkImage =
            if mipLevels > 1 then VulkanTexture.recordGenerateMipmapsInternal cb metadata mipLevels vkImage
        
        /// Upload pixel data to VulkanTexture. Can only be done once.
        static member upload metadata mipLevel pixels (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            let uploadSize = metadata.TextureWidth * metadata.TextureHeight * vulkanTexture.Format.BytesPerPixel
            let stagingBuffer = Buffer.Buffer.stageData uploadSize pixels vkc
            let cb = Hl.beginTransientCommandBlock vkc.TransientCommandPool vkc.Device
            VulkanTexture.recordBufferToImageCopy cb metadata mipLevel stagingBuffer.VkBuffer vulkanTexture.Image
            Hl.endTransientCommandBlock cb vkc.GraphicsQueue vkc.TransientCommandPool vkc.ResourceReadyFence vkc.Device
            Buffer.Buffer.destroy stagingBuffer vkc

        /// Upload array of pixel data to VulkanTexture. Can only be done once.
        static member uploadArray metadata mipLevel array vulkanTexture vkc =
            use arrayPin = new ArrayPin<_> (array)
            VulkanTexture.upload metadata mipLevel arrayPin.NativeInt vulkanTexture vkc
        
        /// Generate mipmaps in VulkanTexture. Can only be done once, after upload to (only) mipLevel 0.
        static member generateMipmaps metadata (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            if vulkanTexture.MipLevels > 1 then
                let cb = Hl.beginTransientCommandBlock vkc.TransientCommandPool vkc.Device
                VulkanTexture.recordGenerateMipmaps cb metadata vulkanTexture.MipLevels vulkanTexture.Image
                Hl.endTransientCommandBlock cb vkc.GraphicsQueue vkc.TransientCommandPool vkc.ResourceReadyFence vkc.Device
        
        /// Create an empty VulkanTexture.
        /// TODO: DJL: make color (1.0f, 0.0f, 1.0f, 1.0f), perhaps just loading up the Assets.Default.Image asset.
        static member createEmpty (vkc : Hl.VulkanContext) =
            VulkanTexture.create Rgba Vulkan.VK_FILTER_NEAREST Vulkan.VK_FILTER_NEAREST false MipmapNone (TextureMetadata.make 32 32) vkc
        
        /// Destroy VulkanTexture.
        static member destroy (vulkanTexture : VulkanTexture) (vkc : Hl.VulkanContext) =
            Vulkan.vkDestroySampler (vkc.Device, vulkanTexture.Sampler, nullPtr)
            Vulkan.vkDestroyImageView (vkc.Device, vulkanTexture.ImageView, nullPtr)
            Vma.vmaDestroyImage (vkc.VmaAllocator, vulkanTexture._Image, vulkanTexture._Allocation)

        /// Represents the empty texture used in Vulkan.
        static member empty =
            match EmptyOpt with
            | Some (:? VulkanTexture as empty) -> empty
            | Some _ | None -> failwith "VulkanTexture.empty not initialized properly."
    
    /// An abstraction for managing dynamically generated unfiltered VulkanTextures accumulated over multiple renders within a frame.
    /// TODO: DJL: determine relationship with Texture.Texture.
    type TextureAccumulator =
        private
            { StagingBuffers : Buffer.BufferAccumulator
              Textures : VulkanTexture List array
              mutable StagingBufferSize : int
              Format : ImageFormat }

        /// Get VulkanTexture at index.
        member this.Item index = this.Textures.[Hl.CurrentFrame].[index]

        /// Stage pixels and record transfer commands.
        static member load index cb metadata pixels textureAccumulator vkc =
            
            // enlarge staging buffer size if needed
            let imageSize = metadata.TextureWidth * metadata.TextureHeight * textureAccumulator.Format.BytesPerPixel
            while imageSize > textureAccumulator.StagingBufferSize do textureAccumulator.StagingBufferSize <- textureAccumulator.StagingBufferSize * 2
            Buffer.BufferAccumulator.updateSize index textureAccumulator.StagingBufferSize textureAccumulator.StagingBuffers vkc

            // stage pixels
            Buffer.BufferAccumulator.upload index 0 imageSize pixels textureAccumulator.StagingBuffers vkc

            // create texture
            let texture = VulkanTexture.create textureAccumulator.Format Vulkan.VK_FILTER_NEAREST Vulkan.VK_FILTER_NEAREST false MipmapNone metadata vkc

            // add texture to index, destroying existing texture if present and expanding list as necessary
            if index < textureAccumulator.Textures.[Hl.CurrentFrame].Count then
                VulkanTexture.destroy textureAccumulator.Textures.[Hl.CurrentFrame].[index] vkc
                textureAccumulator.Textures.[Hl.CurrentFrame].[index] <- texture
            else 
                // fill gaps if index has been skipped for some reason
                while index > textureAccumulator.Textures.[Hl.CurrentFrame].Count do textureAccumulator.Textures.[Hl.CurrentFrame].Add (VulkanTexture.createEmpty vkc)
                textureAccumulator.Textures.[Hl.CurrentFrame].Add texture
            
            // record commands to transfer staged image to the texture
            VulkanTexture.recordBufferToImageCopy cb metadata 0 textureAccumulator.StagingBuffers.[index].VkBuffer texture.Image

        /// Create TextureAccumulator.
        static member create format vkc =
            
            // create the resources
            // TODO: DJL: choose appropriate starting size to minimize most probable upsizing.
            let stagingBufferSize = 4096
            let stagingBuffers = Buffer.BufferAccumulator.create stagingBufferSize (Buffer.Staging true) vkc
            let textures = Array.zeroCreate<List<VulkanTexture>> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec textures.Length do textures.[i] <- List ()

            // make TextureAccumulator
            let textureAccumulator =
                { StagingBuffers = stagingBuffers
                  Textures = textures
                  StagingBufferSize = stagingBufferSize
                  Format = format }

            // fin
            textureAccumulator
        
        /// Destroy TextureAccumulator.
        static member destroy textureAccumulator vkc =
            Buffer.BufferAccumulator.destroy textureAccumulator.StagingBuffers vkc
            for i in 0 .. dec textureAccumulator.Textures.Length do
                for j in 0 .. dec textureAccumulator.Textures.[i].Count do
                    VulkanTexture.destroy textureAccumulator.Textures.[i].[j] vkc
    
    /// Describes data loaded from a texture.
    type TextureData =
        | TextureDataDotNet of Metadata : TextureMetadata * Bytes : byte array
        | TextureDataMipmap of Metadata : TextureMetadata * BlockCompressed : bool * Bytes : byte array * Mipmaps : (Vector2i * byte array) array
        | TextureDataNative of Metadata : TextureMetadata * TextureDataPtr : nativeint * Disposer : IDisposable
        member this.Metadata =
            match this with
            | TextureDataDotNet (metadata, _) -> metadata
            | TextureDataMipmap (metadata, _, _, _) -> metadata
            | TextureDataNative (metadata, _, _) -> metadata
        member this.Bytes =
            match this with
            | TextureDataDotNet (_, bytes) -> (false, bytes)
            | TextureDataMipmap (_, blockCompressed, bytes, _) -> (blockCompressed, bytes)
            | TextureDataNative (metadata, textureDataPtr, _) ->
                let bytes = Array.zeroCreate<byte> (metadata.TextureWidth * metadata.TextureHeight * sizeof<uint>)
                Marshal.Copy (textureDataPtr, bytes, 0, bytes.Length)
                (false, bytes)
        member this.Dispose () =
            match this with
            | TextureDataDotNet (_, _) -> ()
            | TextureDataMipmap (_, _, _, _) -> ()
            | TextureDataNative (_, _, disposer) -> disposer.Dispose ()

    /// Create a Vulkan texture from existing texture data.
    /// NOTE: this function will dispose textureData.
    let CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, blockCompress, textureData, vkc) =

        // upload data to vulkan as appropriate
        match textureData with
        | TextureDataDotNet (metadata, bytes) ->
            let mipmapMode = if mipmaps then MipmapAuto else MipmapNone
            let vulkanTexture = VulkanTexture.create Bgra minFilter magFilter (anisoFilter && mipmaps) mipmapMode metadata vkc
            VulkanTexture.uploadArray metadata 0 bytes vulkanTexture vkc
            if mipmaps then VulkanTexture.generateMipmaps metadata vulkanTexture vkc
            (metadata, vulkanTexture)
        | TextureDataMipmap (metadata, blockCompressed, bytes, mipmapBytesArray) ->

            // if pregenerated mipmap images are available then that determines texture mipmaps, otherwise determined by parameter as usual
            let mipmapMode =
                if mipmapBytesArray.Length > 0 then MipmapManual (mipmapBytesArray.Length + 1)
                elif mipmaps then MipmapAuto else MipmapNone

            // create texture and upload original image
            let vulkanTexture = VulkanTexture.create Bgra minFilter magFilter (anisoFilter && mipmapMode <> MipmapNone) mipmapMode metadata vkc
            VulkanTexture.uploadArray metadata 0 bytes vulkanTexture vkc

            // populate mipmaps as determined
            match mipmapMode with
            | MipmapNone -> ()
            | MipmapManual mipLevels ->
                let mutable mipmapIndex = 0
                while mipmapIndex < mipLevels - 1 do
                    let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
                    let metadata = TextureMetadata.make mipmapResolution.X mipmapResolution.Y
                    VulkanTexture.uploadArray metadata (inc mipmapIndex) mipmapBytes vulkanTexture vkc
                    mipmapIndex <- inc mipmapIndex
            | MipmapAuto -> VulkanTexture.generateMipmaps metadata vulkanTexture vkc
            
            // fin
            (metadata, vulkanTexture)

        | TextureDataNative (metadata, bytesPtr, disposer) ->
            use _ = disposer
            let mipmapMode = if mipmaps then MipmapAuto else MipmapNone
            let vulkanTexture = VulkanTexture.create Bgra minFilter magFilter (anisoFilter && mipmaps) mipmapMode metadata vkc
            VulkanTexture.upload metadata 0 bytesPtr vulkanTexture vkc
            if mipmaps then VulkanTexture.generateMipmaps metadata vulkanTexture vkc
            (metadata, vulkanTexture)

    /// Attempt to create uploadable texture data from the given file path.
    /// Don't forget to dispose the last field when finished with the texture data.
    let TryCreateTextureData (minimal, filePath : string) =
        if File.Exists filePath then

            // attempt to load data as tga
            let platform = Environment.OSVersion.Platform
            let fileExtension = PathF.GetExtensionLower filePath
            if fileExtension = ".tga" then
                try let image = Pfimage.FromFile filePath
                    match TryFormatUncompressedPfimage (false, image) with
                    | Some (resolution, bytes, _) ->
                        let metadata = TextureMetadata.make resolution.X resolution.Y
                        Some (TextureDataDotNet (metadata, bytes))
                    | None -> None
                with _ -> None

            // attempt to load data as dds (compressed or uncompressed)
            elif fileExtension = ".dds" then
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

            // attempt to load data as any format supported by Drawing.Bitmap on Windows
            elif platform = PlatformID.Win32NT || platform = PlatformID.Win32Windows then
                let extension = Path.GetExtension filePath
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
    let TryCreateTextureVulkan (minimal, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath, vkc) =
        match TryCreateTextureData (minimal, filePath) with
        | Some textureData ->
            let (metadata, vulkanTexture) = CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, blockCompress, textureData, vkc)
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
    type LazyTexture (filePath : string, minimalMetadata : TextureMetadata, minimalId : uint, fullMinFilter : TextureMinFilter, fullMagFilter : TextureMagFilter, fullAnisoFilter) =

        let [<VolatileField>] mutable fullServeAttempted = false
        let [<VolatileField>] mutable fullServeParameterized = false
        let [<VolatileField>] mutable fullMetadataAndIdOpt = ValueNone
        let [<VolatileField>] mutable destroyed = false
        let destructionLock = obj ()

        (* Client API - only the client may call this! *)

        member this.FilePath =
            if destroyed then failwith "Accessing field of destroyed texture."
            filePath

        member this.TextureMetadata =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndIdOpt with
                | ValueSome (metadata, _) -> metadata
                | ValueNone -> minimalMetadata
            else minimalMetadata

        member this.TextureId =
            if destroyed then failwith "Accessing field of destroyed texture."
            if fullServeAttempted then
                match fullMetadataAndIdOpt with
                | ValueSome (_, textureId) ->
                    if not fullServeParameterized then
                        Gl.BindTexture (TextureTarget.Texture2d, textureId)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int fullMinFilter)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int fullMagFilter)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
                        if fullAnisoFilter then Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
                        Gl.BindTexture (TextureTarget.Texture2d, 0u)
                        fullServeParameterized <- true
                    textureId
                | ValueNone -> minimalId
            else minimalId

        member this.Destroy () =
            lock destructionLock $ fun () ->
                if not destroyed then
                    Gl.DeleteTextures [|minimalId|]
                    if fullServeAttempted then
                        match fullMetadataAndIdOpt with
                        | ValueSome (_, fullId) ->
                            Gl.DeleteTextures [|fullId|]
                            fullMetadataAndIdOpt <- ValueNone
                        | ValueNone -> ()
                        fullServeAttempted <- false
                    destroyed <- true

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
        
        member this.VulkanTexture = // TODO: BGE: maybe we can come up with a better name for this?
            match this with
            | EmptyTexture -> VulkanTexture.empty
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture
            | LazyTexture lazyTexture -> VulkanTexture.empty
        
        member this.Destroy vkc =
            match this with
            | EmptyTexture -> ()
            | EagerTexture eagerTexture -> eagerTexture.Destroy vkc
            | LazyTexture lazyTexture -> lazyTexture.Destroy () // TODO: DJL: protect VulkanTexture.empty from premature destruction.

        override this.GetHashCode () =
            Texture.hash this

        override this.Equals that =
            match that with
            | :? Texture as texture -> Texture.equals this texture
            | _ -> false

        interface System.IEquatable<Texture> with
            member this.Equals that =
                Texture.equals this that

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
        member this.TryCreateTexture (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath : string, vkc) =

            // memoize texture
            match textures.TryGetValue filePath with
            | (false, _) ->

                // attempt to create texture
                match TryCreateTextureVulkan (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath, vkc) with
                | Right (metadata, vulkanTexture) ->
                    let texture = EagerTexture { TextureMetadata = metadata; VulkanTexture = vulkanTexture}
                    textures.Add (filePath, texture)
                    Right texture
                | Left error -> Left error

            // already exists
            | (true, texture) -> Right texture

        /// Attempt to create a filtered memoized texture from a file.
        member this.TryCreateTextureFiltered (desireLazy, blockCompress, filePath, vkc) =
            this.TryCreateTexture (desireLazy, Vulkan.VK_FILTER_LINEAR, Vulkan.VK_FILTER_LINEAR, true, true, blockCompress, filePath, vkc)
        
        /// Attempt to create an unfiltered memoized texture from a file.
        member this.TryCreateTextureUnfiltered (desireLazy, filePath, vkc) =
            this.TryCreateTexture (desireLazy, Vulkan.VK_FILTER_NEAREST, Vulkan.VK_FILTER_NEAREST, false, false, false, filePath, vkc)
