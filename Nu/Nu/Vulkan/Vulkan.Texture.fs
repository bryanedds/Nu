// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

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

    /// Check that an asset with the given file path can utilize block compression (IE, it's not a normal map,
    /// blend map, or specified as uncompressed).
    /// TODO: move this somewhere more general?
    let BlockCompressable (filePath : string) =
        let name = PathF.GetFileNameWithoutExtension filePath
        not (name.EndsWith "_n") &&
        not (name.EndsWith "_u") &&
        not (name.EndsWith "_b") &&
        not (name.EndsWith "_t") &&
        not (name.EndsWith "Normal") &&
        not (name.EndsWith "Uncompressed") &&
        not (name.EndsWith "Blend") &&
        not (name.EndsWith "Tint")

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
        { Image : Hl.AllocatedImage
          ImageView : VkImageView
          Sampler : VkSampler }

        override this.Equals thatObj =
            match thatObj with
            | :? VulkanTexture as that ->
                this.Image.Image.Handle = that.Image.Image.Handle &&
                this.ImageView.Handle = that.ImageView.Handle
            | _ -> false

        override this.GetHashCode () = 
            hash this.Image.Image.Handle ^^^
            hash this.ImageView.Handle

        /// Create the image.
        static member private createImage format extent allocator =
            let mutable info = VkImageCreateInfo ()
            info.imageType <- Vulkan.VK_IMAGE_TYPE_2D
            info.format <- format
            info.extent <- extent
            info.mipLevels <- 1u
            info.arrayLayers <- 1u
            info.samples <- Vulkan.VK_SAMPLE_COUNT_1_BIT
            info.tiling <- Vulkan.VK_IMAGE_TILING_OPTIMAL
            info.usage <- Vulkan.VK_IMAGE_USAGE_SAMPLED_BIT ||| Vulkan.VK_IMAGE_USAGE_TRANSFER_DST_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            info.initialLayout <- Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            let allocatedImage = Hl.AllocatedImage.create info allocator
            allocatedImage
        
        /// Copy the pixels from the staging buffer to the image.
        static member private copyBufferToImage extent buffer image (vkg : Hl.VulkanGlobal) =
            
            // create command buffer for transfer
            let mutable cb = Hl.allocateCommandBuffer vkg.TransferCommandPool vkg.Device

            // reset command buffer and begin recording
            Vulkan.vkResetCommandPool (vkg.Device, vkg.TransferCommandPool, VkCommandPoolResetFlags.None) |> Hl.check
            let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
            Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> Hl.check

            // transition image layout for data transfer
            let mutable copyBarrier = VkImageMemoryBarrier ()
            copyBarrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            copyBarrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            copyBarrier.subresourceRange <- Hl.makeSubresourceRangeColor 1u
            copyBarrier.dstAccessMask <- Vulkan.VK_ACCESS_TRANSFER_WRITE_BIT
            copyBarrier.oldLayout <- Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            copyBarrier.newLayout <- Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            copyBarrier.image <- image
            Vulkan.vkCmdPipelineBarrier
                (cb, Vulkan.VK_PIPELINE_STAGE_HOST_BIT,
                 Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &copyBarrier)

            // copy data from buffer to image
            let mutable region = VkBufferImageCopy ()
            region.imageSubresource <- Hl.makeSubresourceLayersColor ()
            region.imageExtent <- extent
            Vulkan.vkCmdCopyBufferToImage
                (cb, buffer, image,
                 Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                 1u, asPointer &region)

            // transition image layout for usage
            let mutable useBarrier = VkImageMemoryBarrier ()
            useBarrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            useBarrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
            useBarrier.subresourceRange <- Hl.makeSubresourceRangeColor 1u
            useBarrier.srcAccessMask <- Vulkan.VK_ACCESS_TRANSFER_WRITE_BIT
            useBarrier.dstAccessMask <- Vulkan.VK_ACCESS_SHADER_READ_BIT
            useBarrier.oldLayout <- Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            useBarrier.newLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            useBarrier.image <- image
            Vulkan.vkCmdPipelineBarrier
                (cb, Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT,
                 Vulkan.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                 VkDependencyFlags.None,
                 0u, nullPtr, 0u, nullPtr,
                 1u, asPointer &useBarrier)

            // execute commands
            Vulkan.vkEndCommandBuffer cb |> Hl.check
            let mutable sInfo = VkSubmitInfo ()
            sInfo.commandBufferCount <- 1u
            sInfo.pCommandBuffers <- asPointer &cb
            Vulkan.vkQueueSubmit (vkg.GraphicsQueue, 1u, asPointer &sInfo, vkg.ResourceReadyFence) |> Hl.check
            Hl.awaitFence vkg.ResourceReadyFence vkg.Device
        
        /// Create the sampler.
        static member private createSampler minFilter magFilter device =
            let mutable sampler = Unchecked.defaultof<VkSampler>
            let mutable info = VkSamplerCreateInfo ()
            info.magFilter <- magFilter
            info.minFilter <- minFilter
            info.mipmapMode <- Vulkan.VK_SAMPLER_MIPMAP_MODE_LINEAR
            info.addressModeU <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            info.addressModeV <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            info.addressModeW <- Vulkan.VK_SAMPLER_ADDRESS_MODE_REPEAT
            Vulkan.vkCreateSampler (device, &info, nullPtr, &sampler) |> Hl.check
            sampler
        
        /// Destroy VulkanTexture.
        static member destroy vulkanTexture (vkg : Hl.VulkanGlobal) =
            Vulkan.vkDestroySampler (vkg.Device, vulkanTexture.Sampler, nullPtr)
            Vulkan.vkDestroyImageView (vkg.Device, vulkanTexture.ImageView, nullPtr)
            Hl.AllocatedImage.destroy vulkanTexture.Image vkg.VmaAllocator
        
        /// Create a VulkanTexture.
        static member private createInternal format bytesPerPixel minFilter magFilter metadata pixels (vkg : Hl.VulkanGlobal) =

            // general data
            let uploadSize = metadata.TextureWidth * metadata.TextureHeight * bytesPerPixel
            let extent = VkExtent3D (metadata.TextureWidth, metadata.TextureHeight, 1)

            // upload pixels to staging buffer
            let stagingBuffer = Hl.AllocatedBuffer.stageData uploadSize pixels vkg.VmaAllocator

            // create image and copy pixels to it
            let image = VulkanTexture.createImage format extent vkg.VmaAllocator
            VulkanTexture.copyBufferToImage extent stagingBuffer.Buffer image.Image vkg

            // create image view and sampler
            let imageView = Hl.createImageView format 1u image.Image vkg.Device
            let sampler = VulkanTexture.createSampler minFilter magFilter vkg.Device

            // destroy staging buffer
            Hl.AllocatedBuffer.destroy stagingBuffer vkg.VmaAllocator

            // make VulkanTexture
            let vulkanTexture =
                { Image = image
                  ImageView = imageView
                  Sampler = sampler }

            // fin
            vulkanTexture

        /// Create a VulkanTexture with Bgra format.
        static member createBgra minFilter magFilter metadata pixels vkg =
            VulkanTexture.createInternal Vulkan.VK_FORMAT_B8G8R8A8_UNORM 4 minFilter magFilter metadata pixels vkg

        /// Create a VulkanTexture with Rgba format.
        static member createRgba minFilter magFilter metadata pixels vkg =
            VulkanTexture.createInternal Vulkan.VK_FORMAT_R8G8B8A8_UNORM 4 minFilter magFilter metadata pixels vkg

        /// Create an empty VulkanTexture.
        /// TODO: DJL: make size 32x32 and color (1.0f, 0.0f, 1.0f, 1.0f).
        static member createEmpty (vkg : Hl.VulkanGlobal) =
            
            // create components
            let image = VulkanTexture.createImage Vulkan.VK_FORMAT_R8G8B8A8_UNORM (VkExtent3D (1, 1, 1)) vkg.VmaAllocator
            let imageView = Hl.createImageView Vulkan.VK_FORMAT_R8G8B8A8_UNORM 1u image.Image vkg.Device
            let sampler = VulkanTexture.createSampler Vulkan.VK_FILTER_NEAREST Vulkan.VK_FILTER_NEAREST vkg.Device
            
            // make VulkanTexture
            let vulkanTexture =
                { Image = image
                  ImageView = imageView
                  Sampler = sampler }

            // fin
            vulkanTexture

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
    /// NOTE: the parameters may no longer be adequate for filtered texturing because VkFilter does not include LinearMipmapLinear.
    let CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, blockCompress, textureData, vkg) =

        // upload data to vulkan as appropriate
        match textureData with
        | TextureDataDotNet (metadata, bytes) ->

            // upload dotnet texture data
            let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
            try 
//                let textureId = Gl.GenTexture ()
//                Gl.BindTexture (TextureTarget.Texture2d, textureId)
//                let format = if blockCompress then Constants.OpenGL.BlockCompressedTextureFormat else Constants.OpenGL.UncompressedTextureFormat
//                Gl.TexImage2D (TextureTarget.Texture2d, 0, format, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
//                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
//                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
//                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
//                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
//                if anisoFilter then Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
//                if mipmaps then Gl.GenerateMipmap TextureTarget.Texture2d
//                Gl.BindTexture (TextureTarget.Texture2d, 0u)
                
                let vulkanTexture = VulkanTexture.createBgra minFilter magFilter metadata (bytesPtr.AddrOfPinnedObject ()) vkg
                (metadata, vulkanTexture)
            finally bytesPtr.Free ()

        | TextureDataMipmap (metadata, blockCompressed, bytes, mipmapBytesArray) ->

//            // upload block-compressed dotnet texture data
//            if blockCompressed then
//                if not blockCompress then Log.info "Potential inadvertent block-compression of texture (place a breakpoint here for more detail)."
//                let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
//                try let textureId = Gl.GenTexture ()
//                    Gl.BindTexture (TextureTarget.Texture2d, textureId)
//                    Gl.TexStorage2D (TextureTarget.Texture2d, inc mipmapBytesArray.Length, Branchless.reinterpret Constants.OpenGL.BlockCompressedTextureFormat, metadata.TextureWidth, metadata.TextureHeight)
//                    Gl.CompressedTexSubImage2D (TextureTarget.Texture2d, 0, 0, 0, metadata.TextureWidth, metadata.TextureHeight, Constants.OpenGL.BlockCompressedTextureFormat, bytes.Length, bytesPtr.AddrOfPinnedObject ())
//                    let mutable mipmapIndex = 0
//                    while mipmapIndex < mipmapBytesArray.Length do
//                        let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
//                        let mipmapBytesPtr = GCHandle.Alloc (mipmapBytes, GCHandleType.Pinned)
//                        try Gl.CompressedTexSubImage2D (TextureTarget.Texture2d, inc mipmapIndex, 0, 0, mipmapResolution.X, mipmapResolution.Y, Constants.OpenGL.BlockCompressedTextureFormat, mipmapBytes.Length, mipmapBytesPtr.AddrOfPinnedObject ())
//                        finally mipmapBytesPtr.Free ()
//                        mipmapIndex <- inc mipmapIndex
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
//                    if mipmaps || mipmapBytesArray.Length > 0 then
//                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
//                    if mipmaps && mipmapBytesArray.Length = 0 then
//                        Gl.GenerateMipmap TextureTarget.Texture2d
//                    Gl.BindTexture (TextureTarget.Texture2d, 0u)
//                    (metadata, textureId)
//                finally bytesPtr.Free ()
//
//            // upload uncompressed dotnet texture data
//            else
//                let bytesPtr = GCHandle.Alloc (bytes, GCHandleType.Pinned)
//                try let textureId = Gl.GenTexture ()
//                    Gl.BindTexture (TextureTarget.Texture2d, textureId)
//                    let format = if blockCompress then Constants.OpenGL.BlockCompressedTextureFormat else Constants.OpenGL.UncompressedTextureFormat
//                    Gl.TexImage2D (TextureTarget.Texture2d, 0, format, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr.AddrOfPinnedObject ())
//                    let mutable mipmapIndex = 0
//                    while mipmapIndex < mipmapBytesArray.Length do
//                        let (mipmapResolution, mipmapBytes) = mipmapBytesArray.[mipmapIndex]
//                        let mipmapBytesPtr = GCHandle.Alloc (mipmapBytes, GCHandleType.Pinned)
//                        try Gl.TexImage2D (TextureTarget.Texture2d, inc mipmapIndex, Constants.OpenGL.UncompressedTextureFormat, mipmapResolution.X, mipmapResolution.Y, 0, PixelFormat.Bgra, PixelType.UnsignedByte, mipmapBytesPtr.AddrOfPinnedObject ())
//                        finally mipmapBytesPtr.Free ()
//                        mipmapIndex <- inc mipmapIndex
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
//                    Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
//                    if mipmaps || mipmapBytesArray.Length > 0 then
//                        Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)                
//                    if mipmaps && mipmapBytesArray.Length = 0 then
//                        Gl.GenerateMipmap TextureTarget.Texture2d
//                    Gl.BindTexture (TextureTarget.Texture2d, 0u)
//                    (metadata, textureId)
//                finally bytesPtr.Free ()

            (metadata, VulkanTexture.empty)

        | TextureDataNative (metadata, bytesPtr, disposer) ->

            // upload native texture data
            use _ = disposer

//            let textureId = Gl.GenTexture ()
//            Gl.BindTexture (TextureTarget.Texture2d, textureId)
//            let format = if blockCompress then Constants.OpenGL.BlockCompressedTextureFormat else Constants.OpenGL.UncompressedTextureFormat
//            Gl.TexImage2D (TextureTarget.Texture2d, 0, format, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bytesPtr)
//            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMinFilter, int minFilter)
//            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMagFilter, int magFilter)
//            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
//            Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
//            if mipmaps then
//                Gl.TexParameter (TextureTarget.Texture2d, TextureParameterName.TextureMaxAnisotropy, Constants.Render.TextureAnisotropyMax)
//                Gl.GenerateMipmap TextureTarget.Texture2d
//            Gl.BindTexture (TextureTarget.Texture2d, 0u)

            let vulkanTexture = VulkanTexture.createBgra minFilter magFilter metadata bytesPtr vkg
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
                try let bitmap = new Drawing.Bitmap (filePath)
                    let data = bitmap.LockBits (Drawing.Rectangle (0, 0, bitmap.Width, bitmap.Height), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format32bppArgb)
                    let metadata = TextureMetadata.make bitmap.Width bitmap.Height
                    let scan0 = data.Scan0
                    Some (TextureDataNative (metadata, scan0, { new IDisposable with member this.Dispose () = bitmap.UnlockBits data; bitmap.Dispose () })) // NOTE: calling UnlockBits explicitly since I can't figure out if Dispose does.
                with _ -> None

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
    let TryCreateTextureVulkan (minimal, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath, vkg) =
        match TryCreateTextureData (minimal, filePath) with
        | Some textureData ->
            let (metadata, vulkanTexture) = CreateTextureVulkanFromData (minFilter, magFilter, anisoFilter, mipmaps, blockCompress, textureData, vkg)
            Right (metadata, vulkanTexture)
        | None -> Left ("Missing file or unloadable texture data '" + filePath + "'.")

    /// A texture that's immediately loaded.
    type [<Struct>] EagerTexture =
        { TextureMetadata : TextureMetadata
          VulkanTexture : VulkanTexture }
        member this.Destroy vkg =
            VulkanTexture.destroy this.VulkanTexture vkg

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

        (* Server API - only the server may call this! *)

//        member internal this.TryServe () =
//            lock destructionLock $ fun () ->
//                if not destroyed && not fullServeAttempted then
//                    match TryCreateTextureGl (false, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, fullAnisoFilter, false, BlockCompressable filePath, filePath) with
//                    | Right (metadata, textureId) ->
//                        Gl.Finish () // NOTE: calling this seems to prevent a bug, IIRC.
//                        fullMetadataAndIdOpt <- ValueSome (metadata, textureId)
//                    | Left error -> Log.info ("Could not serve lazy texture due to:" + error)
//                    fullServeAttempted <- true

    /// A 2d texture.
    type Texture =
        | EmptyTexture
        | EagerTexture of EagerTexture
        | LazyTexture of LazyTexture
        member this.TextureMetadata =
            match this with
            | EmptyTexture -> TextureMetadata.empty
            | EagerTexture eagerTexture -> eagerTexture.TextureMetadata
            | LazyTexture lazyTexture -> lazyTexture.TextureMetadata
        member this.VulkanTexture =
            match this with
            | EmptyTexture -> VulkanTexture.empty
            | EagerTexture eagerTexture -> eagerTexture.VulkanTexture
            | LazyTexture lazyTexture -> VulkanTexture.empty
        member this.Destroy vkg =
            match this with
            | EmptyTexture -> ()
            | EagerTexture eagerTexture -> eagerTexture.Destroy vkg
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
        member this.TryCreateTexture (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath : string, vkg) =

            // memoize texture
            match textures.TryGetValue filePath with
            | (false, _) ->

                // attempt to create texture
                match TryCreateTextureVulkan (desireLazy, minFilter, magFilter, anisoFilter, mipmaps, blockCompress, filePath, vkg) with
                | Right (metadata, vulkanTexture) ->
                    let texture = EagerTexture { TextureMetadata = metadata; VulkanTexture = vulkanTexture}

//                        if desireLazy && PathF.GetExtensionLower filePath = ".dds" then
//                            let lazyTexture = new LazyTexture (filePath, metadata, textureId, minFilter, magFilter, anisoFilter)
//                            lazyTextureQueue.Enqueue lazyTexture
//                            LazyTexture lazyTexture
//                        else EagerTexture { TextureMetadata = metadata; TextureId = textureId }

                    textures.Add (filePath, texture)
                    Right texture
                | Left error -> Left error

            // already exists
            | (true, texture) -> Right texture

//        /// Attempt to create a filtered memoized texture from a file.
//        member this.TryCreateTextureFiltered (desireLazy, blockCompress, filePath) =
//            this.TryCreateTexture (desireLazy, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear, true, true, blockCompress, filePath)

        /// Attempt to create an unfiltered memoized texture from a file.
        member this.TryCreateTextureUnfiltered (desireLazy, filePath, vkg) =
            this.TryCreateTexture (desireLazy, Vulkan.VK_FILTER_NEAREST, Vulkan.VK_FILTER_NEAREST, false, false, false, filePath, vkg)

//    /// Populated the texture ids and handles of lazy textures in a threaded manner.
//    /// TODO: abstract this to interface that can represent either inline or threaded implementation.
//    type TextureServer (lazyTextureQueues : ConcurrentDictionary<LazyTexture ConcurrentQueue, LazyTexture ConcurrentQueue>, sharedContext, window) =
//        let mutable threadOpt = None
//        let [<VolatileField>] mutable started = false
//        let [<VolatileField>] mutable terminated = false
//
//        member private this.Run () =
//            started <- true
//            while not terminated do
//                let batchTime = Stopwatch.StartNew () // NOTE: we stop loading after 1/2 frame passed so far.
//                let desiredFrameTimeMinimumMs = GameTime.DesiredFrameTimeMinimum * 1000.0
//                let lazyTextureQueueEnr = lazyTextureQueues.GetEnumerator ()
//                while not terminated && batchTime.ElapsedMilliseconds < int64 (desiredFrameTimeMinimumMs * 0.5) && lazyTextureQueueEnr.MoveNext () do
//                    let lazyTextureQueue = lazyTextureQueueEnr.Current.Key
//                    let mutable lazyTexture = Unchecked.defaultof<_>
//                    while not terminated && batchTime.ElapsedMilliseconds < int64 (desiredFrameTimeMinimumMs * 0.5) && lazyTextureQueue.TryDequeue &lazyTexture do
//                        lazyTexture.TryServe ()
//                Thread.Sleep (max 1 (int desiredFrameTimeMinimumMs - int batchTime.ElapsedMilliseconds + 1))
//
//        member this.Start () =
//            if not started then
//                let thread =
//                    Thread (ThreadStart (fun () ->
//                        try this.Run ()
//                        with _ -> Environment.Exit Constants.Engine.ExitCodeFailure))
//                threadOpt <- Some thread
//                thread.IsBackground <- true
//                thread.Start ()
//                while not started do Thread.Yield () |> ignore<bool>
//
//        member this.Terminate () =
//            if started && not terminated then
//                let thread = Option.get threadOpt
//                terminated <- true
//                thread.Join ()
//                threadOpt <- None
