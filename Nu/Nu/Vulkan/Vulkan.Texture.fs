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

    type private ImageLayout =
        | Undefined
        | TransferSrc
        | TransferDst
        | ShaderRead

        member this.vkImageLayout =
            match this with
            | Undefined -> Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            | TransferSrc -> Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            | TransferDst -> Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            | ShaderRead -> Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

        member this.Access =
            match this with
            | Undefined -> VkAccessFlags.None
            | TransferSrc -> Vulkan.VK_ACCESS_TRANSFER_READ_BIT
            | TransferDst -> Vulkan.VK_ACCESS_TRANSFER_WRITE_BIT
            | ShaderRead -> Vulkan.VK_ACCESS_SHADER_READ_BIT

        member this.PipelineStage =
            match this with
            | Undefined -> Vulkan.VK_PIPELINE_STAGE_HOST_BIT
            | TransferSrc -> Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT
            | TransferDst -> Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT
            | ShaderRead -> Vulkan.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    
    /// An abstraction of a texture as managed by Vulkan.
    /// TODO: extract sampler out of here.
    type [<CustomEquality; NoComparison>] VulkanTexture =
        { Image : VulkanMemory.Image
          ImageView : VkImageView
          Sampler : VkSampler
          MipLevels : int }

        override this.Equals thatObj =
            match thatObj with
            | :? VulkanTexture as that ->
                this.Image.VkImage.Handle = that.Image.VkImage.Handle &&
                this.ImageView.Handle = that.ImageView.Handle
            | _ -> false

        override this.GetHashCode () = 
            hash this.Image.VkImage.Handle ^^^
            hash this.ImageView.Handle

        /// Create the image.
        static member private createImage format extent mipLevels vkc =
            
            // prepare for mipmap generation if applicable
            let usage =
                if mipLevels = 1
                then Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
                else Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
                    
            // create image
            let mutable info = VkImageCreateInfo ()
            info.imageType <- Vulkan.VK_IMAGE_TYPE_2D
            info.format <- format
            info.extent <- extent
            info.mipLevels <- uint mipLevels
            info.arrayLayers <- 1u
            info.samples <- Vulkan.VK_SAMPLE_COUNT_1_BIT
            info.tiling <- Vulkan.VK_IMAGE_TILING_OPTIMAL
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            info.initialLayout <- Undefined.vkImageLayout
            let image = VulkanMemory.Image.create info vkc
            image
        
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
        
        /// Record command to transition image layout.
        static member private recordTransitionLayout cb allLevels mipNumber (oldLayout : ImageLayout) (newLayout : ImageLayout) vkImage =
            
            // mipNumber means total number of mips or the target mip depending on context
            let mipLevels = if allLevels then mipNumber else 1
            let mipLevel = if allLevels then 0 else mipNumber
            
            // transition layout
            let mutable barrier = VkImageMemoryBarrier ()
            barrier.srcAccessMask <- oldLayout.Access
            barrier.dstAccessMask <- newLayout.Access
            barrier.oldLayout <- oldLayout.vkImageLayout
            barrier.newLayout <- newLayout.vkImageLayout
            barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.image <- vkImage
            barrier.subresourceRange <- Hl.makeSubresourceRangeColor mipLevels
            barrier.subresourceRange.baseMipLevel <- uint mipLevel
            Vulkan.vkCmdPipelineBarrier
                (cb,
                 oldLayout.PipelineStage,
                 newLayout.PipelineStage,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &barrier)
        
        /// Record commands to generate mipmaps.
        static member private recordGenerateMipmaps cb (extent : VkExtent3D) mipLevels vkImage =
            
            // TODO: DJL: figure out how to handle possible lack of linear filtering support, see https://vulkan-tutorial.com/Generating_Mipmaps#page_Linear-filtering-support.
            
            // use single barrier for all transfer operations
            let mutable barrier = VkImageMemoryBarrier ()
            barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            barrier.image <- vkImage
            barrier.subresourceRange <- Hl.makeSubresourceRangeColor 1

            // init mipmap dimensions
            let mutable mipWidth = int extent.width
            let mutable mipHeight = int extent.height

            for i in 1 .. dec mipLevels do
                
                // transition layout of previous image to be copied from
                barrier.srcAccessMask <- TransferDst.Access
                barrier.dstAccessMask <- TransferSrc.Access
                barrier.oldLayout <- TransferDst.vkImageLayout
                barrier.newLayout <- TransferSrc.vkImageLayout
                barrier.subresourceRange.baseMipLevel <- uint (i - 1)
                Vulkan.vkCmdPipelineBarrier
                    (cb,
                     TransferDst.PipelineStage,
                     TransferSrc.PipelineStage,
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
                     vkImage, TransferSrc.vkImageLayout,
                     vkImage, TransferDst.vkImageLayout,
                     1u, asPointer &blit, Vulkan.VK_FILTER_LINEAR)
                
                // transition layout of previous image to be read by shader
                barrier.srcAccessMask <- TransferSrc.Access
                barrier.dstAccessMask <- ShaderRead.Access
                barrier.oldLayout <- TransferSrc.vkImageLayout
                barrier.newLayout <- ShaderRead.vkImageLayout
                Vulkan.vkCmdPipelineBarrier
                    (cb,
                     TransferSrc.PipelineStage,
                     ShaderRead.PipelineStage,
                     VkDependencyFlags.None,
                     0u, nullPtr, 0u, nullPtr,
                     1u, asPointer &barrier)
                
                // update mipmap dimensions
                mipWidth <- nextWidth
                mipHeight <- nextHeight
            
            // transition final mip image left unfinished by loop
            barrier.srcAccessMask <- TransferDst.Access
            barrier.dstAccessMask <- ShaderRead.Access
            barrier.oldLayout <- TransferDst.vkImageLayout
            barrier.newLayout <- ShaderRead.vkImageLayout
            barrier.subresourceRange.baseMipLevel <- uint (mipLevels - 1)
            Vulkan.vkCmdPipelineBarrier
                (cb,
                 TransferDst.PipelineStage,
                 ShaderRead.PipelineStage,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &barrier)
        
        /// Record commands to copy from buffer to image.
        static member recordBufferToImageCopy cb extent mipLevels vkBuffer vkImage =
            
            // transition image layout for data transfer
            VulkanTexture.recordTransitionLayout cb true mipLevels Undefined TransferDst vkImage

            // copy data from buffer to image
            let mutable region = VkBufferImageCopy ()
            region.imageSubresource <- Hl.makeSubresourceLayersColor 0
            region.imageExtent <- extent
            Vulkan.vkCmdCopyBufferToImage
                (cb, vkBuffer, vkImage,
                 TransferDst.vkImageLayout,
                 1u, asPointer &region)

            // transition image layout for usage either here or in mipmap generation as applicable
            if mipLevels > 1 then VulkanTexture.recordGenerateMipmaps cb extent mipLevels vkImage
            else VulkanTexture.recordTransitionLayout cb true mipLevels TransferDst ShaderRead vkImage
        
        /// Copy the pixels from the staging buffer to the image.
        static member private copyBufferToImage extent mipLevels vkBuffer vkImage (vkc : Hl.VulkanContext) =
            
            // setup command buffer for copy
            let mutable cb = Hl.allocateCommandBuffer vkc.TransientCommandPool vkc.Device
            Vulkan.vkResetCommandPool (vkc.Device, vkc.TransientCommandPool, VkCommandPoolResetFlags.None) |> Hl.check
            let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
            Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> Hl.check

            // record copy
            VulkanTexture.recordBufferToImageCopy cb extent mipLevels vkBuffer vkImage

            // execute copy
            Vulkan.vkEndCommandBuffer cb |> Hl.check
            let mutable sInfo = VkSubmitInfo ()
            sInfo.commandBufferCount <- 1u
            sInfo.pCommandBuffers <- asPointer &cb
            Vulkan.vkQueueSubmit (vkc.GraphicsQueue, 1u, asPointer &sInfo, vkc.ResourceReadyFence) |> Hl.check
            Hl.awaitFence vkc.ResourceReadyFence vkc.Device
        
        /// Create a VulkanTexture.
        static member private createInternal format bytesPerPixel minFilter magFilter anisoFilter mipmaps metadata pixelsOpt (vkc : Hl.VulkanContext) =

            // calculate mip levels if mipmaps enabled
            let mipLevels = if mipmaps then max metadata.TextureWidth metadata.TextureHeight |> Math.Log2 |> floor |> inc |> int else 1
            
            // create image, image view and sampler
            let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            let image = VulkanTexture.createImage format extent mipLevels vkc
            let imageView = Hl.createImageView format mipLevels image.VkImage vkc.Device
            let sampler = VulkanTexture.createSampler minFilter magFilter anisoFilter vkc
            
            // upload pixels to image if provided
            match pixelsOpt with
            | Some pixels ->
                let uploadSize = metadata.TextureWidth * metadata.TextureHeight * bytesPerPixel
                let stagingBuffer = VulkanMemory.Buffer.stageData uploadSize pixels vkc
                VulkanTexture.copyBufferToImage extent mipLevels stagingBuffer.VkBuffer image.VkImage vkc
                VulkanMemory.Buffer.destroy stagingBuffer vkc
            | None -> ()

            // make VulkanTexture
            let vulkanTexture =
                { Image = image
                  ImageView = imageView
                  Sampler = sampler
                  MipLevels = mipLevels }

            // fin
            vulkanTexture

        /// Create a VulkanTexture with Bgra format.
        static member createBgra minFilter magFilter anisoFilter mipmaps metadata pixelsOpt vkc =
            VulkanTexture.createInternal Vulkan.VK_FORMAT_B8G8R8A8_UNORM 4 minFilter magFilter anisoFilter mipmaps metadata pixelsOpt vkc

        /// Create a VulkanTexture with Rgba format.
        static member createRgba minFilter magFilter anisoFilter mipmaps metadata pixelsOpt vkc =
            VulkanTexture.createInternal Vulkan.VK_FORMAT_R8G8B8A8_UNORM 4 minFilter magFilter anisoFilter mipmaps metadata pixelsOpt vkc

        /// Create an empty VulkanTexture.
        /// TODO: DJL: make size 32x32 and color (1.0f, 0.0f, 1.0f, 1.0f), perhaps just loading up the Assets.Default.Image asset.
        static member createEmpty (vkc : Hl.VulkanContext) =
            VulkanTexture.createRgba Vulkan.VK_FILTER_NEAREST Vulkan.VK_FILTER_NEAREST false false (TextureMetadata.make 1 1) None vkc
        
        /// Destroy VulkanTexture.
        static member destroy vulkanTexture (vkc : Hl.VulkanContext) =
            Vulkan.vkDestroySampler (vkc.Device, vulkanTexture.Sampler, nullPtr)
            Vulkan.vkDestroyImageView (vkc.Device, vulkanTexture.ImageView, nullPtr)
            VulkanMemory.Image.destroy vulkanTexture.Image vkc

        /// Represents the empty texture used in Vulkan.
        static member empty =
            match EmptyOpt with
            | Some (:? VulkanTexture as empty) -> empty
            | Some _ | None -> failwith "VulkanTexture.empty not initialized properly."
    
    /// A VulkanTexture that can be spontaneously recreated with automatic staging to load the data at render time.
    /// TODO: DJL: determine relationship with Texture.Texture.
    type DynamicTexture =
        private
            { VulkanTextures : VulkanTexture array
              StagingBuffers : VulkanMemory.Buffer array
              mutable Extent : VkExtent3D
              mutable StagingBufferSize : int
              mutable TextureIndex : int }

        /// The VulkanTexture.
        member this.VulkanTexture = this.VulkanTextures.[this.TextureIndex]

        member private this.CurrentStagingBuffer = this.StagingBuffers.[this.TextureIndex]
        member private this.ImageSize = (int this.Extent.width) * (int this.Extent.height) * 4

        static member private newCycle metadata pixels dynamicTexture (vkc : Hl.VulkanContext) =
            
            // advance index
            dynamicTexture.TextureIndex <- (inc dynamicTexture.TextureIndex) % 2

            // enlarge staging buffer size if needed
            dynamicTexture.Extent <- VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)
            while dynamicTexture.ImageSize > dynamicTexture.StagingBufferSize do dynamicTexture.StagingBufferSize <- dynamicTexture.StagingBufferSize * 2
            VulkanMemory.Buffer.updateSize dynamicTexture.StagingBufferSize dynamicTexture.CurrentStagingBuffer vkc

            // stage pixels
            VulkanMemory.Buffer.upload 0 dynamicTexture.ImageSize pixels dynamicTexture.CurrentStagingBuffer vkc

            // destroy expired VulkanTexture
            VulkanTexture.destroy dynamicTexture.VulkanTexture vkc

        /// Add a new bgra VulkanTexture.
        static member private addBgra minFilter magFilter anisoFilter mipmaps metadata pixels dynamicTexture vkc =
            DynamicTexture.newCycle metadata pixels dynamicTexture vkc
            dynamicTexture.VulkanTextures.[dynamicTexture.TextureIndex] <- VulkanTexture.createBgra minFilter magFilter anisoFilter mipmaps metadata None vkc

        /// Add a new rgba VulkanTexture.
        static member private addRgba minFilter magFilter anisoFilter mipmaps metadata pixels dynamicTexture vkc =
            DynamicTexture.newCycle metadata pixels dynamicTexture vkc
            dynamicTexture.VulkanTextures.[dynamicTexture.TextureIndex] <- VulkanTexture.createRgba minFilter magFilter anisoFilter mipmaps metadata None vkc
        
        /// Transfer pixels to texture.
        static member private loadTexture cb commandQueue fence dynamicTexture device =
            Hl.beginCommandBlock cb fence device
            VulkanTexture.recordBufferToImageCopy
                cb dynamicTexture.Extent dynamicTexture.VulkanTexture.MipLevels dynamicTexture.CurrentStagingBuffer.VkBuffer dynamicTexture.VulkanTexture.Image.VkImage
            Hl.endCommandBlock cb commandQueue [||] [||] VkFence.Null

        /// Instantly stage a bgra image, then submit texture load once fence is ready.
        /// A Pipeline barrier ensures the load is complete before use. TODO: DJL: confirm this!
        static member loadBgra cb commandQueue minFilter magFilter anisoFilter mipmaps metadata pixels fence dynamicTexture vkc =
            DynamicTexture.addBgra minFilter magFilter anisoFilter mipmaps metadata pixels dynamicTexture vkc
            DynamicTexture.loadTexture cb commandQueue fence dynamicTexture vkc.Device

        /// Instantly stage an rgba image, then submit texture load once fence is ready.
        /// A Pipeline barrier ensures the load is complete before use. TODO: DJL: confirm this!
        static member loadRgba cb commandQueue minFilter magFilter anisoFilter mipmaps metadata pixels fence dynamicTexture vkc =
            DynamicTexture.addRgba minFilter magFilter anisoFilter mipmaps metadata pixels dynamicTexture vkc
            DynamicTexture.loadTexture cb commandQueue fence dynamicTexture vkc.Device

        /// Create DynamicTexture.
        static member create vkc =

            // create the resources
            let extent = VkExtent3D (32, 32, 1)
            let sbSize = 4096 // TODO: DJL: choose appropriate starting size to minimize most probable upsizing.
            let vulkanTextures = [|VulkanTexture.createEmpty vkc; VulkanTexture.createEmpty vkc|]
            let stagingBuffers = [|VulkanMemory.Buffer.createStagingInFrame sbSize vkc; VulkanMemory.Buffer.createStagingInFrame sbSize vkc|]

            // make DynamicTexture
            let dynamicTexture =
                { VulkanTextures = vulkanTextures
                  StagingBuffers = stagingBuffers
                  Extent = extent
                  StagingBufferSize = sbSize
                  TextureIndex = 1 }

            // fin
            dynamicTexture
        
        /// Destroy DynamicTexture.
        static member destroy dynamicTexture vkc =
            VulkanTexture.destroy dynamicTexture.VulkanTextures.[0] vkc
            VulkanTexture.destroy dynamicTexture.VulkanTextures.[1] vkc
            VulkanMemory.Buffer.destroy dynamicTexture.StagingBuffers.[0] vkc
            VulkanMemory.Buffer.destroy dynamicTexture.StagingBuffers.[1] vkc
    
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

            // upload dotnet texture data
            let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
            try 
                let vulkanTexture = VulkanTexture.createBgra minFilter magFilter (anisoFilter && mipmaps) mipmaps metadata (Some (bytesPtr.AddrOfPinnedObject ())) vkc
                (metadata, vulkanTexture)
            finally bytesPtr.Free ()

        | TextureDataMipmap (metadata, blockCompressed, bytes, mipmapBytesArray) ->

            // TODO: DJL: implement.
            
            (metadata, VulkanTexture.empty)

        | TextureDataNative (metadata, bytesPtr, disposer) ->

            // upload native texture data
            use _ = disposer

            let vulkanTexture = VulkanTexture.createBgra minFilter magFilter (anisoFilter && mipmaps) mipmaps metadata (Some bytesPtr) vkc
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
    type [<Struct>] EagerTexture =
        { TextureMetadata : TextureMetadata
          VulkanTexture : VulkanTexture }
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
    /// TODO: DJL: implement equals etc. from OpenGL.Texture as part of texture maturation.
    type Texture =
        | EmptyTexture
        | EagerTexture of EagerTexture
        | LazyTexture of LazyTexture
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
