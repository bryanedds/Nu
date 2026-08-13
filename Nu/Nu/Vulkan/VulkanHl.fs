// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Reflection
open System.Runtime.InteropServices
open System.Security.Cryptography
open System.Text
open System.Threading
open System.IO
open FSharp.NativeInterop
open AstcEncoder
open BCnEncoder.Shared.ImageFiles
open ImageMagick
open Pfim
open SDL
open Vortice.ShaderCompiler
open Vortice.Vulkan
open Prime
open Nu

/// The format of an image.
type ImageFormat =
    | Rgba8
    | Rgba16f
    | Rgba32f
    | Rgb16f
    | Rgb32f
    | Rg32f
    | R16f
    | R32f
    | Bc3
    | Bc5
    | Astc
    | D16
    | X8d24Pack32
    | D32f
    | D16s8ui
    | D24s8ui
    | D32fs8ui

    /// The VkFormat.
    member this.VkFormat =
        match this with
        | Rgba8 -> VkFormat.R8G8B8A8Unorm
        | Rgba16f -> VkFormat.R16G16B16A16Sfloat
        | Rgba32f -> VkFormat.R32G32B32A32Sfloat
        | Rgb16f -> VkFormat.R16G16B16Sfloat
        | Rgb32f -> VkFormat.R32G32B32Sfloat
        | Rg32f -> VkFormat.R32G32Sfloat
        | R16f -> VkFormat.R16Sfloat
        | R32f -> VkFormat.R32Sfloat
        | Bc3 -> VkFormat.Bc3UnormBlock
        | Bc5 -> VkFormat.Bc5UnormBlock
        | Astc -> VkFormat.Astc4x4UnormBlock
        | D16 -> VkFormat.D16Unorm
        | X8d24Pack32 -> VkFormat.X8D24UnormPack32
        | D32f -> VkFormat.D32Sfloat
        | D16s8ui -> VkFormat.D16UnormS8Uint
        | D24s8ui -> VkFormat.D24UnormS8Uint
        | D32fs8ui -> VkFormat.D32SfloatS8Uint

    /// The VkImageAspectFlags.
    member this.VkImageAspectFlags =
        match this with
        | Rgba8
        | Rgba16f
        | Rgba32f
        | Rgb16f
        | Rgb32f
        | Rg32f
        | R16f
        | R32f
        | Bc3
        | Bc5
        | Astc -> VkImageAspectFlags.Color
        | D16
        | X8d24Pack32
        | D32f -> VkImageAspectFlags.Depth
        | D16s8ui
        | D24s8ui
        | D32fs8ui -> VkImageAspectFlags.Depth ||| VkImageAspectFlags.Stencil
        
    /// Get the size in bytes of an image with given width, height and format.
    static member getImageSize width height imageFormat =
        match imageFormat with
        | Rgba8 -> width * height * 4
        | Rgba16f -> width * height * 8
        | Rgba32f -> width * height * 16
        | Rgb16f -> width * height * 6
        | Rgb32f -> width * height * 12
        | Rg32f -> width * height * 8
        | R16f -> width * height * 2
        | R32f -> width * height * 4
        | Bc3
        | Bc5 
        | Astc ->
            let x = if width % 4 = 0 then width else (width / 4 + 1) * 4
            let y = if height % 4 = 0 then height else (height / 4 + 1) * 4
            x * y
        | D16 -> width * height * 2
        | X8d24Pack32 -> width * height * 4
        | D32f -> width * height * 4
        | D16s8ui -> width * height * 3
        | D24s8ui -> width * height * 4
        | D32fs8ui -> width * height * 5
    
/// The pixel format of an image.
type PixelFormat =
    | Rgba
    | Bgra
    | Rgb
    | Rg
    | Red
    | Depth

    /// The VkComponentSwizzles of a PixelFormat.
    member this.VkComponentSwizzles =
        match this with
        | Rgba -> (VkComponentSwizzle.R, VkComponentSwizzle.G, VkComponentSwizzle.B, VkComponentSwizzle.A)
        | Bgra -> (VkComponentSwizzle.B, VkComponentSwizzle.G, VkComponentSwizzle.R, VkComponentSwizzle.A)
        | Rgb -> (VkComponentSwizzle.R, VkComponentSwizzle.G, VkComponentSwizzle.B, VkComponentSwizzle.A)
        | Rg -> (VkComponentSwizzle.R, VkComponentSwizzle.G, VkComponentSwizzle.B, VkComponentSwizzle.A)
        | Red -> (VkComponentSwizzle.R, VkComponentSwizzle.G, VkComponentSwizzle.B, VkComponentSwizzle.A)
        | Depth -> (VkComponentSwizzle.R, VkComponentSwizzle.G, VkComponentSwizzle.B, VkComponentSwizzle.A) // doesn't matter

/// An image layout in its access and pipeline stage context.
type ImageLayout =
    | Undefined
    | TransferSrc
    | TransferDst
    | ColorAttachmentRead
    | ColorAttachmentWrite
    | DepthAttachmentRead
    | DepthAttachmentWrite
    | Present

    /// The VkImageLayout.
    member this.VkImageLayout =
        match this with
        | Undefined -> VkImageLayout.Undefined
        | TransferSrc -> VkImageLayout.TransferSrcOptimal
        | TransferDst -> VkImageLayout.TransferDstOptimal
        | ColorAttachmentRead -> VkImageLayout.ShaderReadOnlyOptimal
        | ColorAttachmentWrite -> VkImageLayout.ColorAttachmentOptimal
        | DepthAttachmentRead -> VkImageLayout.DepthReadOnlyStencilAttachmentOptimal
        | DepthAttachmentWrite -> VkImageLayout.DepthStencilAttachmentOptimal
        | Present -> VkImageLayout.PresentSrcKHR

    /// The access flag.
    member this.Access =
        match this with
        | Undefined -> VkAccessFlags.None
        | TransferSrc -> VkAccessFlags.TransferRead
        | TransferDst -> VkAccessFlags.TransferWrite
        | ColorAttachmentRead -> VkAccessFlags.ShaderRead
        | ColorAttachmentWrite -> VkAccessFlags.ColorAttachmentWrite
        | DepthAttachmentRead -> VkAccessFlags.DepthStencilAttachmentRead
        | DepthAttachmentWrite -> VkAccessFlags.DepthStencilAttachmentRead ||| VkAccessFlags.DepthStencilAttachmentWrite
        | Present -> VkAccessFlags.None

    /// The pipeline stage.
    member this.PipelineStage =
            
        // NOTE: for Undefined as image layout transition source, texture upload and mipmap generation previously used
        // VK_PIPELINE_STAGE_HOST_BIT. DJL can't remember why, but it's not in the tutorial and apparently may lead to
        // failure on Android devices. DJL suspects it was inherited from ImGui backend.
        match this with
        | Undefined -> VkPipelineStageFlags.TopOfPipe
        | TransferSrc -> VkPipelineStageFlags.Transfer
        | TransferDst -> VkPipelineStageFlags.Transfer
        | ColorAttachmentRead -> VkPipelineStageFlags.FragmentShader
        | ColorAttachmentWrite -> VkPipelineStageFlags.ColorAttachmentOutput
        | DepthAttachmentRead -> VkPipelineStageFlags.EarlyFragmentTests ||| VkPipelineStageFlags.LateFragmentTests
        | DepthAttachmentWrite -> VkPipelineStageFlags.EarlyFragmentTests ||| VkPipelineStageFlags.LateFragmentTests
        | Present -> VkPipelineStageFlags.BottomOfPipe

/// The format of a vertex attribute.
type VertexAttribFormat =
    | Int       | Int2      | Int3      | Int4
    | Uint      | Uint2     | Uint3     | Uint4
    | Quarter   | Quarter2  | Quarter3  | Quarter4
    | Half      | Half2     | Half3     | Half4
    | Single    | Single2   | Single3   | Single4
    | Double    | Double2   | Double3   | Double4

    /// The VkFormat.
    member this.VkFormat =
        match this with
        | Int -> VkFormat.R32Sint
        | Int2 -> VkFormat.R32G32Sint
        | Int3 -> VkFormat.R32G32B32Sint
        | Int4 -> VkFormat.R32G32B32A32Sint
        | Uint -> VkFormat.R32Uint
        | Uint2 -> VkFormat.R32G32Uint
        | Uint3 -> VkFormat.R32G32B32Uint
        | Uint4 -> VkFormat.R32G32B32A32Uint
        | Quarter -> VkFormat.R8Unorm
        | Quarter2 -> VkFormat.R8G8Unorm
        | Quarter3 -> VkFormat.R8G8B8Unorm
        | Quarter4 -> VkFormat.R8G8B8A8Unorm
        | Half -> VkFormat.R16Sfloat
        | Half2 -> VkFormat.R16G16Sfloat
        | Half3 -> VkFormat.R16G16B16Sfloat
        | Half4 -> VkFormat.R16G16B16A16Sfloat
        | Single -> VkFormat.R32Sfloat
        | Single2 -> VkFormat.R32G32Sfloat
        | Single3 -> VkFormat.R32G32B32Sfloat
        | Single4 -> VkFormat.R32G32B32A32Sfloat
        | Double -> VkFormat.R64Sfloat
        | Double2 -> VkFormat.R64G64Sfloat
        | Double3 -> VkFormat.R64G64B64Sfloat
        | Double4 -> VkFormat.R64G64B64A64Sfloat
    
/// A shader stage or combination.
type ShaderStage =
    | VertexStage
    | FragmentStage
    | VertexAndFragmentStage

    /// The VkShaderStageFlags.
    member this.VkShaderStageFlags =
        match this with
        | VertexStage -> VkShaderStageFlags.Vertex
        | FragmentStage -> VkShaderStageFlags.Fragment
        | VertexAndFragmentStage -> VkShaderStageFlags.Vertex ||| VkShaderStageFlags.Fragment
    
/// The type of a resource descriptor.
type DescriptorType =
    | Sampler
    | CombinedImageSampler
    | SampledImage
    | UniformBuffer
    | StorageBuffer

    /// The VkDescriptorType.
    member this.VkDescriptorType =
        match this with
        | Sampler -> VkDescriptorType.Sampler
        | CombinedImageSampler -> VkDescriptorType.CombinedImageSampler
        | SampledImage -> VkDescriptorType.SampledImage
        | UniformBuffer -> VkDescriptorType.UniformBuffer
        | StorageBuffer -> VkDescriptorType.StorageBuffer

/// The compression to use for a texture, if any.
type TextureCompression =
    | Uncompressed
    | ColorCompression
    | NormalCompression

    /// The Vulkan internal format corresponding to this block compression. This can vary based on
    /// Constants.Render.TextureBlockCompression.
    member this.ImageFormat =
        match this with
        | Uncompressed ->
            Rgba8
        | ColorCompression ->
            match Constants.Render.TextureBlockCompression with
            | BcCompression -> Bc3
            | AstcCompression -> Astc
        | NormalCompression ->
            match Constants.Render.TextureBlockCompression with
            | BcCompression -> Bc5
            | AstcCompression -> Astc

    /// The Vulkan pixel format corresponding to this block compression.
    member this.PixelFormat =
        match this with
        | Uncompressed -> Bgra
        | ColorCompression | NormalCompression -> Rgba

/// The state of the program's OS-provided rendering surface.
type SurfaceState =
    | SurfaceReady
    | SurfaceLost
    | SurfaceDestroyed

/// Represents a strict cycle ensuring that any presentation resources (surface and swapchains) that exist or are being created during the onset
/// of app backgrounding on a mobile device are torn down/cancelled.
/// TODO: consider encapsulating most of this stuff into a Surface abstraction as it should not be visible to Swapchain
/// and VulkanContext.
type internal BackgroundingResponseState =
    | PresentationSetupInitiated // setup of presentation resources has begun and may be complete
    | PresentationTeardownPending // presentation resources can no longer be trusted as app has commenced backgrounding
    | PresentationTeardownComplete // presentation resources have been destroyed and restoration will commence when app is back in foreground

[<AutoOpen>]
module Vulkan =

    let mutable internal VkInstanceApi = Unchecked.defaultof<VkInstanceApi>
    let mutable internal VkDeviceApi = Unchecked.defaultof<VkDeviceApi>

    /// Set a VkInstanceApi value. Under normal operation, this can never be null.
    let internal SetInstanceApi vkInstanceApi = VkInstanceApi <- vkInstanceApi

    /// Set a VkDeviceApi value. Under normal operation, this can never be null.
    let internal SetDeviceApi vkDeviceApi = VkDeviceApi <- vkDeviceApi

    /// The Vulkan instance API. Ignore the type parameter as it's only use to expose InstanceApi in a convenient way.
    let inline internal InstanceApi<'a> = VkInstanceApi

    /// The Vulkan device API. Ignore the type parameter as it's only use to expose InstanceApi in a convenient way.
    let inline internal DeviceApi<'a> = VkDeviceApi

[<RequireQualifiedAccess>]
module Hl =

    // TODO: P0: these free-floating bindings have become a bit of a mess and need to be reordered or moved into
    // VulkanContext.
    let mutable internal ValidationLayersActivated = false

    let mutable internal DrawCountersLock = obj ()
    let mutable internal DrawInstanceCount = 0
    let mutable internal DrawCallCount = 0
    let mutable internal DrawScopeCount = 0

    // provides id for a texture on the gpu that is globally unique i.e. cannot be reused after texture is destroyed,
    // which is essential for tracking descriptor writes
    let mutable private TextureIdGenerationLock = obj ()
    let mutable private TextureIdCounter = 0u

    /// Index of the current Swapchain image.
    let mutable internal ImageIndex = 0u

    /// The forward-declared empty texture value.
    /// Initialized in RendererProcesses.
    /// NOTE: if performance issues arise from checking / casting this, maybe use ValueOption or null directly.
    /// TODO: see if instead of exposing mutability of this directly, we should define Init and CleanUp fns.
    let mutable internal EmptyTextureOpt : obj option = None

    let mutable internal SurfaceState = SurfaceDestroyed
    let mutable internal Surface = Unchecked.defaultof<VkSurfaceKHR>

    // presentation teardown in response to backgrounding follows BackgroundingResponseState cycle,
    // whereas presentation setup need only care whether app is *currently* in foreground
    let mutable private BackgroundingResponseStateLock = obj ()
    let mutable private BackgroundingResponseState = PresentationTeardownComplete
    let mutable private Backgrounded = false

    // cached window properties that have to come in from the main thread.
    let mutable WindowProperties_ = WindowProperties.empty
    let inline WindowProperties<'a> = WindowProperties_

    // callback to inform render loop about app backgrounding
    // official documentation for android case: https://github.com/libsdl-org/SDL/blob/main/docs/README-android.md#activity-lifecycle
#nowarn 202
    [<UnmanagedCallersOnly (CallConvs = [|typeof<System.Runtime.CompilerServices.CallConvCdecl>|])>]
#warnon 202
    let private handleBackgrounding (userData : voidptr) (event : SDL_Event nativeptr) : SDLBool =
        ignore userData
        let event = NativePtr.toByRef event
        match event.Type with
        | SDL_EventType.SDL_EVENT_WILL_ENTER_BACKGROUND ->
            Backgrounded <- true
            lock BackgroundingResponseStateLock (fun () ->
                if BackgroundingResponseState = PresentationSetupInitiated then BackgroundingResponseState <- PresentationTeardownPending)
            true
        | SDL_EventType.SDL_EVENT_DID_ENTER_FOREGROUND ->
            Backgrounded <- false
            true
        | _ -> true
    let internal backgroundingCallback () =
        let handle = Assembly.GetExecutingAssembly().GetType("Nu.Vulkan.Hl").GetMethod(nameof handleBackgrounding, BindingFlags.NonPublic ||| BindingFlags.Static).MethodHandle
        handle.GetFunctionPointer ()

    let setWindowProperties windowProperties =
        WindowProperties_ <- windowProperties

    let internal setPresentationSetupInitiated () =
        lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState <- PresentationSetupInitiated)

    let internal setPresentationTeardownComplete () =
        lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState <- PresentationTeardownComplete)

    /// Has app been SET for backgrounding (i.e. not necessarily IN background yet/still), invalidating existing surface.
    let internal getBackgroundingRequested () =
        lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState = PresentationTeardownPending)

    let internal getBackgrounded () =
        Backgrounded

    let internal genTextureId () =
        lock TextureIdGenerationLock (fun () -> TextureIdCounter <- inc TextureIdCounter; TextureIdCounter)

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then
            let message = "Vulkan assertion failed due to: " + string result
            Log.error message

    /// Determine whether format is supported for use as an attachment.
    let supportsAttachment vkPhysicalDevice format =
        let requiredFeatures =
            match format with
            | Rgba8
            | Rgba16f
            | Rgba32f
            | Rgb16f
            | Rgb32f
            | Rg32f
            | R16f
            | R32f
            | Bc3
            | Bc5
            | Astc -> VkFormatFeatureFlags.BlitSrc ||| VkFormatFeatureFlags.BlitDst ||| VkFormatFeatureFlags.ColorAttachment ||| VkFormatFeatureFlags.SampledImage
            | D16
            | X8d24Pack32
            | D32f
            | D16s8ui
            | D24s8ui
            | D32fs8ui -> VkFormatFeatureFlags.DepthStencilAttachment
        let mutable properties = Unchecked.defaultof<VkFormatProperties>
        InstanceApi.vkGetPhysicalDeviceFormatProperties (vkPhysicalDevice, format.VkFormat, &properties)
        properties.optimalTilingFeatures &&& requiredFeatures = requiredFeatures

    /// Check if an image format is supported for attachments, falling back to a standard format where possible.
    let rec checkAttachmentFormat vkPhysicalDevice (format : ImageFormat) =
        if not (supportsAttachment vkPhysicalDevice format) then
            
            // NOTE: formats required by spec - https://docs.vulkan.org/spec/latest/chapters/formats.html#features-required-format-support
            // NOTE: format fallbacks must not be ints for blit conversion.
            let (formatFallback : ImageFormat) =
                match format with
                | Bc3 | Bc5 | Astc ->
                    Log.fail ("Compressed image formats are not supported for attachment textures.")
                | Rgb16f ->
                    checkAttachmentFormat vkPhysicalDevice Rgba16f
                | Rgb32f ->
                    checkAttachmentFormat vkPhysicalDevice Rgba32f
                | Rgba32f ->
                    checkAttachmentFormat vkPhysicalDevice Rgba16f
                | Rgba8 | Rgba16f | Rg32f | R16f | R32f ->
                    Log.fail ("Vulkan attachment image format '" + scstring format.VkFormat + "' support is absent but required. Further, it's a requirement in the Vulkan specification!")
                | D32f ->
                    checkAttachmentFormat vkPhysicalDevice D32fs8ui
                | D32fs8ui ->
                    checkAttachmentFormat vkPhysicalDevice D24s8ui
                | D24s8ui ->
                    checkAttachmentFormat vkPhysicalDevice X8d24Pack32
                | X8d24Pack32 ->
                    checkAttachmentFormat vkPhysicalDevice D16
                | D16 ->
                    checkAttachmentFormat vkPhysicalDevice D16s8ui
                | D16s8ui ->
                    Log.fail "Could not find a suitable format for depth attachment textures."
            Log.info ("Falling back to " + scstring formatFallback.VkFormat + " attachment format due to unavailability of " + scstring format.VkFormat + " attachment format.")
            formatFallback

        else format

    /// Convert VkExtensionProperties.extensionName to a string.
    /// TODO: see if we can inline functions like these once F# supports C#'s representation of this fixed buffer type.
    let getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName

    /// Convert VkLayerProperties.layerName to a string.
    let getLayerName (layerProps : VkLayerProperties) =
        NativePtr.fixedBufferToString layerProps.layerName

    /// Make a VkComponentMapping.
    let makeComponentMapping (pixelFormat : PixelFormat) =
        let (r, g, b, a) = pixelFormat.VkComponentSwizzles
        let mutable componentMapping = VkComponentMapping ()
        componentMapping.r <- r
        componentMapping.g <- g
        componentMapping.b <- b
        componentMapping.a <- a
        componentMapping

    /// Make a VkImageSubresourceRange representing a color image.
    let makeSubresourceRange mipLevel mipCount layer layerCount imageAspect =
        let mutable subresourceRange = VkImageSubresourceRange ()
        subresourceRange.aspectMask <- imageAspect
        subresourceRange.baseMipLevel <- uint mipLevel
        subresourceRange.levelCount <- uint mipCount
        subresourceRange.baseArrayLayer <- uint layer
        subresourceRange.layerCount <- uint layerCount
        subresourceRange

    /// Make a VkImageSubresourceLayers representing a color image.
    let makeSubresourceLayers (mipLevel : int) (layer : int) imageAspect =
        let mutable subresourceLayers = VkImageSubresourceLayers ()
        subresourceLayers.aspectMask <- imageAspect
        subresourceLayers.mipLevel <- uint mipLevel
        subresourceLayers.baseArrayLayer <- uint layer
        subresourceLayers.layerCount <- 1u
        subresourceLayers

    /// Make a VkViewport.
    let makeViewport invertY (rect : VkRect2D) =
        let mutable viewport = VkViewport ()
        viewport.x <- single rect.offset.x
        viewport.y <- if invertY then single rect.offset.y + single rect.extent.height else single rect.offset.y
        viewport.width <- single rect.extent.width
        viewport.height <- if invertY then -(single rect.extent.height) else single rect.extent.height
        viewport.minDepth <- 0.0f
        viewport.maxDepth <- 1.0f
        viewport

    /// Make a VkPipelineColorBlendAttachmentState.
    let makeBlendAttachment blendDataOpt =
        let mutable blendAttachment = VkPipelineColorBlendAttachmentState ()
        match blendDataOpt with
        | Some (srcColor, dstColor, srcAlpha, dstAlpha) ->
            blendAttachment.blendEnable <- true
            blendAttachment.srcColorBlendFactor <- srcColor
            blendAttachment.dstColorBlendFactor <- dstColor
            blendAttachment.colorBlendOp <- VkBlendOp.Add
            blendAttachment.srcAlphaBlendFactor <- srcAlpha
            blendAttachment.dstAlphaBlendFactor <- dstAlpha
            blendAttachment.alphaBlendOp <- VkBlendOp.Add
        | None -> ()
        blendAttachment.colorWriteMask <-
            VkColorComponentFlags.R |||
            VkColorComponentFlags.G |||
            VkColorComponentFlags.B |||
            VkColorComponentFlags.A
        blendAttachment

    /// Make a VkVertexInputBindingDescription.
    let makeVertexBinding (binding : int) (stride : int) inputRate =
        let mutable bindingDescription = VkVertexInputBindingDescription ()
        bindingDescription.binding <- uint binding
        bindingDescription.stride <- uint stride
        bindingDescription.inputRate <- inputRate
        bindingDescription

    /// Make a VkVertexInputAttributeDescription.
    let makeVertexAttribute (location : int) (binding : int) (format : VertexAttribFormat) (offset : int) =
        let mutable attribute = VkVertexInputAttributeDescription ()
        attribute.location <- uint location
        attribute.binding <- uint binding
        attribute.format <- format.VkFormat
        attribute.offset <- uint offset
        attribute

    /// Make a VkDescriptorSetLayoutBinding.
    let makeDescriptorBinding (binding : int) (descriptorType : DescriptorType) (descriptorCount : int) (shaderStage : ShaderStage) =
        let mutable layoutBinding = VkDescriptorSetLayoutBinding ()
        layoutBinding.binding <- uint binding
        layoutBinding.descriptorType <- descriptorType.VkDescriptorType
        layoutBinding.descriptorCount <- uint descriptorCount
        layoutBinding.stageFlags <- shaderStage.VkShaderStageFlags
        layoutBinding

    /// Make a VkPushConstantRange.
    let makePushConstantRange (offset : int) (size : int) (shaderStage : ShaderStage) =
        let mutable range = VkPushConstantRange ()
        range.stageFlags <- shaderStage.VkShaderStageFlags
        range.offset <- uint offset
        range.size <- uint size
        range

    /// Make a VkImageBlit.
    let makeBlit srcMipLevel dstMipLevel srcLayer dstLayer (srcRect : VkRect2D) (dstRect : VkRect2D) =
        let srcOffsetMin = VkOffset3D (srcRect.offset.x, srcRect.offset.y, 0)
        let dstOffsetMin = VkOffset3D (dstRect.offset.x, dstRect.offset.y, 0)
        let srcOffsetMax = VkOffset3D (srcRect.offset.x + int srcRect.extent.width, srcRect.offset.y + int srcRect.extent.height, 1)
        let dstOffsetMax = VkOffset3D (dstRect.offset.x + int dstRect.extent.width, dstRect.offset.y + int dstRect.extent.height, 1)
        let mutable blit = VkImageBlit ()
        blit.srcSubresource <- makeSubresourceLayers srcMipLevel srcLayer VkImageAspectFlags.Color
        blit.srcOffsets <- NativePtr.writeArrayToFixedBuffer [|srcOffsetMin; srcOffsetMax|] blit.srcOffsets
        blit.dstSubresource <- makeSubresourceLayers dstMipLevel dstLayer VkImageAspectFlags.Color
        blit.dstOffsets <- NativePtr.writeArrayToFixedBuffer [|dstOffsetMin; dstOffsetMax|] blit.dstOffsets
        blit
        
    /// Make a VkRenderingInfo and utilize within the given scope for memory safety.
    let withRenderingInfo (colorAttachments : VkImageView array) depthAttachmentOpt renderArea clearValueOpt action =
        
        // color attachment infos
        let colorInfos = Array.zeroCreate colorAttachments.Length
        for i in 0 .. dec colorInfos.Length do
            let mutable colorInfo = VkRenderingAttachmentInfo ()
            colorInfo.imageView <- colorAttachments[i]
            colorInfo.imageLayout <- ColorAttachmentWrite.VkImageLayout
            colorInfo.storeOp <- VkAttachmentStoreOp.Store
            match clearValueOpt with
            | Some clearValue ->
                colorInfo.loadOp <- VkAttachmentLoadOp.Clear
                colorInfo.clearValue <- clearValue
            | None ->
                colorInfo.loadOp <- VkAttachmentLoadOp.Load
            colorInfos[i] <- colorInfo
        use colorInfosPin = new ArrayPin<_> (colorInfos)

        // depth attachment info
        let mutable depthInfo = VkRenderingAttachmentInfo ()
        match depthAttachmentOpt with
        | Some depthAttachment ->
            depthInfo.imageView <- depthAttachment
            depthInfo.imageLayout <- DepthAttachmentWrite.VkImageLayout
            depthInfo.storeOp <- VkAttachmentStoreOp.Store
            match clearValueOpt with
            | Some _ ->
                depthInfo.loadOp <- VkAttachmentLoadOp.Clear
                depthInfo.clearValue <- VkClearValue (1.0f, 0u)
            | None ->
                depthInfo.loadOp <- VkAttachmentLoadOp.Load
        | None -> ()

        // rendering info
        let mutable renderingInfo = VkRenderingInfo ()
        renderingInfo.renderArea <- renderArea
        renderingInfo.layerCount <- 1u
        renderingInfo.colorAttachmentCount <- uint colorInfos.Length
        renderingInfo.pColorAttachments <- colorInfosPin.Pointer
        if depthAttachmentOpt.IsSome then renderingInfo.pDepthAttachment <- &&depthInfo

        // invoke action
        action renderingInfo

    /// Check that VkRect2D has non-zero area.
    let validateRect (rect : VkRect2D) =
        rect.extent.width > 0u && rect.extent.height > 0u

    /// Clip a VkRect2D within the bounds of another.
    let clipRect (bounds : VkRect2D) (rect : VkRect2D) =
        let boundsMaxX = bounds.offset.x + int bounds.extent.width
        let boundsMaxY = bounds.offset.y + int bounds.extent.height
        let rectMaxX = rect.offset.x + int rect.extent.width
        let rectMaxY = rect.offset.y + int rect.extent.height
        let offsetX = max bounds.offset.x rect.offset.x
        let offsetY = max bounds.offset.y rect.offset.y
        let maxX = min boundsMaxX rectMaxX
        let maxY = min boundsMaxY rectMaxY
        let extentWidth = max 0 (maxX - offsetX)
        let extentHeight = max 0 (maxY - offsetY)
        let mutable result = VkRect2D ()
        result.offset.x <- offsetX
        result.offset.y <- offsetY
        result.extent.width <- uint extentWidth
        result.extent.height <- uint extentHeight
        result
        
    // Check whether window resource is availabile for utilization.
    let private isWindowResourceAvailable () =
        if OperatingSystem.IsAndroid () then
            let windowProperties = WindowProperties.PropertiesHandle
            let windowPointer = SDL3.SDL_GetPointerProperty (windowProperties, SDL3.SDL_PROP_WINDOW_ANDROID_WINDOW_POINTER, 0n)
            windowPointer <> 0n
        else true // will presumably never be blocked on other platforms

    let tryCreateVulkanSurface window instance =

        // attempt to recreate surface if destroyed
        match SurfaceState with
        | SurfaceDestroyed ->

            // ensure window resource is available for utilization
            if isWindowResourceAvailable () then

                // inform the backgrounding callback that we begin the process of creating the surface and swapchain
                // that may need to be aborted/destroyed at any point before *or* after completion due to a
                // backgrounding event, hence setup *initiated*
                setPresentationSetupInitiated ()
                let mutable surfacePtr = Unchecked.defaultof<VkSurfaceKHR_T nativeptr>
                let instance = NativePtr.ofNativeInt (VkInstance.op_Implicit instance)
                if not (SDL3.SDL_Vulkan_CreateSurface (window, instance, NativePtr.nullPtr, &&surfacePtr)) then
                    Log.error (SDL3.SDL_GetError ())
                    setPresentationTeardownComplete () // inform callback to scratch that
                else
                    Surface <- NativePtr.toNativeInt surfacePtr |> uint64 |> VkSurfaceKHR.op_Implicit
                    SurfaceState <- SurfaceReady

        // handle error cases
        | SurfaceReady -> Log.error "Attempted creation of Vulkan surface when existing surface has not been destroyed!"
        | SurfaceLost -> Log.error "Attempted creation of Vulkan surface when existing surface has been lost but not destroyed!"

        // fin
        SurfaceState

    let createVulkanSurface window instance =
    
        // wait for app to enter foreground if not already
        while getBackgrounded () do
            Thread.Yield () |> ignore<bool>

        // attempt to recreate vulkan surface
        // cannot tolerate failure as this function is intended to guarantee surface creation, otherwise must set up
        // a retry mechanism
        if (tryCreateVulkanSurface window instance).IsSurfaceDestroyed then
            Log.fail "Vulkan surface creation failed."

    let destroyVulkanSurface () =
        match SurfaceState with
        | SurfaceReady
        | SurfaceLost ->

            // destroy surface and then inform the backgrounding callback that the required teardown of presentation is
            // complete so no action is required if another backgrounding event is triggered prior to recreation; this
            // must correspond exactly with SurfaceDestroyed, which is used by Swapchain
            Log.info "Destroying Vulkan surface..."
            InstanceApi.vkDestroySurfaceKHR (Surface, nullPtr)
            SurfaceState <- SurfaceDestroyed
            setPresentationTeardownComplete ()

        | SurfaceDestroyed ->
            Log.error "Attempted destruction of Vulkan surface that has already been destroyed!"

    /// Try to compile GLSL file to SPIR-V code.
    let tryCompileShader shaderPath shaderKind =
        let shaderStr = File.ReadAllText shaderPath
        let optimizationLevel = if Constants.Render.RenderDebug then OptimizationLevel.Zero else OptimizationLevel.Performance
        let generatedDebug = Constants.Engine.EngineDebug
        let cacheKey = shaderStr + scstring shaderKind + "|" + scstring optimizationLevel + "|" + scstring generatedDebug
        let cacheHash = Convert.ToHexString (SHA256.HashData (Encoding.UTF8.GetBytes cacheKey))
        try Directory.CreateDirectory "ShaderCache" |> ignore<DirectoryInfo>
        with exn -> Log.warn ("Failed to create ./ShaderCache directory due to: " + scstring exn)
        let cachePath = PathF.Combine ("ShaderCache", cacheHash + ".spv")
        if not (File.Exists cachePath) then
            use compiler = new Compiler ()
            let options = CompilerOptions (ShaderStage = shaderKind, OptimizationLevel = optimizationLevel, GeneratedDebug = generatedDebug)
            let result = compiler.Compile (shaderStr, shaderPath, options)
            if result.Status = CompilationStatus.Success then
                try File.WriteAllBytes (cachePath, result.Bytecode)
                with exn -> Log.warn ("Failed to save SPIR-V bytecode for shader '" + shaderPath + "' due to: " + scstring exn)
                Right result.Bytecode
            else Left ("Vulkan shader compilation failed due to:\n" + result.ErrorMessage)
        else Right (File.ReadAllBytes cachePath)

    /// Try to create a shader module from a GLSL file.
    /// TODO: create matching destroy fn and use that?
    let tryCreateShaderModuleFromGlsl shaderPath shaderKind =
        match tryCompileShader shaderPath shaderKind with
        | Right shader ->

            // NOTE: using a high level overload here to avoid questions about reinterpret casting and memory
            // alignment; see -
            // https://vulkan-tutorial.com/Drawing_a_triangle/Graphics_pipeline_basics/Shader_modules#page_Creating-shader-modules
            let mutable shaderModule = Unchecked.defaultof<VkShaderModule>
            DeviceApi.vkCreateShaderModule (shader.AsSpan (), nullPtr, &shaderModule) |> check
            Right shaderModule

        | Left msg -> Left msg

    /// Get the available vulkan present modes.
    let getPresentModes device =
        let mutable presentModeCount = 0u
        InstanceApi.vkGetPhysicalDeviceSurfacePresentModesKHR (device, Surface, &&presentModeCount, NativePtr.nullPtr) |> check
        let presentModes = Array.zeroCreate<VkPresentModeKHR> (int presentModeCount)
        use presentModesPin = new ArrayPin<_> (presentModes)
        InstanceApi.vkGetPhysicalDeviceSurfacePresentModesKHR (device, Surface, &&presentModeCount, presentModesPin.Pointer) |> check
        presentModes

    /// Record command to transition image layout.
    let recordTransitionLayout allLevels mipNumber layer layerCount imageAspect (oldLayout : ImageLayout) (newLayout : ImageLayout) vkImage commandBuffer =
    
        // mipNumber means total number of mips or the target mip depending on context
        let mipLevels = if allLevels then mipNumber else 1
        let mipLevel = if allLevels then 0 else mipNumber
    
        // transition layout
        let mutable barrier = VkImageMemoryBarrier ()
        barrier.srcAccessMask <- oldLayout.Access
        barrier.dstAccessMask <- newLayout.Access
        barrier.oldLayout <- oldLayout.VkImageLayout
        barrier.newLayout <- newLayout.VkImageLayout
        barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.image <- vkImage
        barrier.subresourceRange <- makeSubresourceRange mipLevel mipLevels layer layerCount imageAspect
        DeviceApi.vkCmdPipelineBarrier
            (commandBuffer,
             oldLayout.PipelineStage,
             newLayout.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, &&barrier)

    /// Attempt to get surface capabilities.
    let tryGetSurfaceCapabilities vkPhysicalDevice =
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        let result = InstanceApi.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, Surface, &capabilities)
        if result <> VkResult.ErrorSurfaceLostKHR then
            check result
            Some capabilities
        else
            SurfaceState <- SurfaceLost
            None

    /// Attempt to get a valid swap extent.
    let tryGetSwapExtent (capabilities : VkSurfaceCapabilitiesKHR) =

        // ensure that extent is valid
        if capabilities.currentExtent.width <> 0u then

            // ensure that extent is variable
            if capabilities.currentExtent.width = UInt32.MaxValue then

                // get pixel resolution from sdl
                let mutable width = WindowProperties.WidthPixels
                let mutable height = WindowProperties.HeightPixels

                // ensure pixel resolution is valid for use as swap extent
                if width <> 0 && height <> 0 then

                    // clamp resolution to size limits
                    width <- max width (int capabilities.minImageExtent.width)
                    width <- min width (int capabilities.maxImageExtent.width)
                    height <- max height (int capabilities.minImageExtent.height)
                    height <- min height (int capabilities.maxImageExtent.height)
                    Some (VkExtent2D (width, height))

                // invalid
                else None

            // otherwise it's fixed
            else Some capabilities.currentExtent

        // otherwise it's invalid
        else None

    /// Create an image view.
    let createImageView pixelFormat vkFormat mipLevel mipCount (layer : int) (layerCount : int) viewType imageAspect image =
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- viewType
        info.format <- vkFormat
        info.components <- makeComponentMapping pixelFormat
        info.subresourceRange <- makeSubresourceRange mipLevel mipCount layer layerCount imageAspect
        let mutable imageView = Unchecked.defaultof<VkImageView>
        DeviceApi.vkCreateImageView (&info, nullPtr, &imageView) |> check
        imageView

    /// Allocate an array of command buffers.
    let allocateCommandBuffers count commandBufferLevel commandPool =
        let mutable info = VkCommandBufferAllocateInfo ()
        info.commandPool <- commandPool
        info.level <- commandBufferLevel
        info.commandBufferCount <- uint count
        let commandBuffers = Array.zeroCreate<VkCommandBuffer> count
        use commandBuffersPin = new ArrayPin<_> (commandBuffers)
        DeviceApi.vkAllocateCommandBuffers (&&info, commandBuffersPin.Pointer) |> check
        commandBuffers

    /// Allocate a command buffer.
    let allocateCommandBuffer commandBufferLevel commandPool =
        let commandBuffers = allocateCommandBuffers 1 commandBufferLevel commandPool
        commandBuffers[0]

    /// Create a semaphore.
    /// TODO: create matching destroy fn and use that?
    let createSemaphore () =
        let info = VkSemaphoreCreateInfo ()
        let mutable semaphore = Unchecked.defaultof<VkSemaphore>
        DeviceApi.vkCreateSemaphore (&info, nullPtr, &semaphore) |> check
        semaphore

    /// Create a fence.
    /// TODO: create matching destroy fn and use that?
    let createFence createSignaled =
        let info =
            if createSignaled then VkFenceCreateInfo (flags = VkFenceCreateFlags.Signaled)
            else VkFenceCreateInfo ()
        let mutable fence = Unchecked.defaultof<VkFence>
        DeviceApi.vkCreateFence (&info, nullPtr, &fence) |> check
        fence

    /// Create a transient command buffer.
    /// TODO: create matching destroy fn and use that?
    let createTransientCommandBuffer commandPool =
        let commandBuffer = allocateCommandBuffer VkCommandBufferLevel.Primary commandPool
        let mutable cbInfo = VkCommandBufferBeginInfo (flags = VkCommandBufferUsageFlags.OneTimeSubmit)
        DeviceApi.vkBeginCommandBuffer (commandBuffer, &&cbInfo) |> check
        commandBuffer

    ///
    let findMemoryType typeFilter properties physicalDevice =

        // get memory types
        let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
        InstanceApi.vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
        let memoryTypes = NativePtr.fixedBufferToArray<VkMemoryType> (int memProperties.memoryTypeCount) memProperties.memoryTypes

        // try find suitable memory type
        let mutable memoryTypeOpt = None
        for i in 0 .. dec memoryTypes.Length do
            match memoryTypeOpt with
            | None when typeFilter &&& (1u <<< i) <> 0u && memoryTypes[i].propertyFlags &&& properties = properties ->
                memoryTypeOpt <- Some (uint i)
            | Some _ | None -> ()

        // fin
        match memoryTypeOpt with
        | Some memoryType -> memoryType
        | None -> Log.fail "Failed to find suitable memory type!"

    /// Record command to copy buffer to image.
    let recordCopyBufferToImage commandBuffer width height mipLevel layer vkBuffer vkImage =
        recordTransitionLayout false mipLevel layer 1 VkImageAspectFlags.Color Undefined TransferDst vkImage commandBuffer
        let mutable region = VkBufferImageCopy ()
        region.imageSubresource <- makeSubresourceLayers mipLevel layer VkImageAspectFlags.Color
        region.imageExtent <- VkExtent3D (width, height, 1)
        DeviceApi.vkCmdCopyBufferToImage
            (commandBuffer, vkBuffer, vkImage,
             TransferDst.VkImageLayout,
             1u, &&region)
        recordTransitionLayout false mipLevel layer 1 VkImageAspectFlags.Color TransferDst ColorAttachmentRead vkImage commandBuffer

    /// Record commands to generate mipmaps.
    let recordGenerateMipmaps commandBuffer width height mipLevels layer vkImage =

        // use single barrier for all transfer operations
        let mutable barrier = VkImageMemoryBarrier ()
        barrier.srcQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.dstQueueFamilyIndex <- Vulkan.VK_QUEUE_FAMILY_IGNORED
        barrier.image <- vkImage

        // transition mipmap images from undefined as they haven't been touched yet
        barrier.srcAccessMask <- Undefined.Access
        barrier.dstAccessMask <- TransferDst.Access
        barrier.oldLayout <- Undefined.VkImageLayout
        barrier.newLayout <- TransferDst.VkImageLayout
        barrier.subresourceRange <- makeSubresourceRange 1 (mipLevels - 1) layer 1 VkImageAspectFlags.Color
        DeviceApi.vkCmdPipelineBarrier
            (commandBuffer,
                Undefined.PipelineStage,
                TransferDst.PipelineStage,
                VkDependencyFlags.None,
                0u, nullPtr, 0u, nullPtr,
                1u, &&barrier)

        // transition original image separately as it's already set to shader read
        barrier.srcAccessMask <- ColorAttachmentRead.Access
        barrier.dstAccessMask <- TransferDst.Access
        barrier.oldLayout <- ColorAttachmentRead.VkImageLayout
        barrier.newLayout <- TransferDst.VkImageLayout
        barrier.subresourceRange.baseMipLevel <- 0u
        barrier.subresourceRange.levelCount <- 1u // only one level at a time from here on
        DeviceApi.vkCmdPipelineBarrier
            (commandBuffer,
                ColorAttachmentRead.PipelineStage,
                TransferDst.PipelineStage,
                VkDependencyFlags.None,
                0u, nullPtr, 0u, nullPtr,
                1u, &&barrier)

        // compute mipmap dimensions
        let mutable mipWidth = width
        let mutable mipHeight = height
        for i in 1 .. dec mipLevels do

            // transition layout of previous image to be copied from
            barrier.srcAccessMask <- TransferDst.Access
            barrier.dstAccessMask <- TransferSrc.Access
            barrier.oldLayout <- TransferDst.VkImageLayout
            barrier.newLayout <- TransferSrc.VkImageLayout
            barrier.subresourceRange.baseMipLevel <- uint (i - 1)
            DeviceApi.vkCmdPipelineBarrier
                (commandBuffer,
                    TransferDst.PipelineStage,
                    TransferSrc.PipelineStage,
                    VkDependencyFlags.None,
                    0u, nullPtr, 0u, nullPtr,
                    1u, &&barrier)

            // generate the next mipmap image from the previous one
            let nextWidth = if mipWidth > 1 then mipWidth / 2 else 1
            let nextHeight = if mipHeight > 1 then mipHeight / 2 else 1
            let mutable blit =
                makeBlit
                    (i - 1) i layer layer
                    (VkRect2D (0, 0, uint mipWidth, uint mipHeight))
                    (VkRect2D (0, 0, uint nextWidth, uint nextHeight))
            DeviceApi.vkCmdBlitImage (commandBuffer, vkImage, TransferSrc.VkImageLayout, vkImage, TransferDst.VkImageLayout, 1u, &&blit, VkFilter.Linear)

            // transition layout of previous image to be read by shader
            barrier.srcAccessMask <- TransferSrc.Access
            barrier.dstAccessMask <- ColorAttachmentRead.Access
            barrier.oldLayout <- TransferSrc.VkImageLayout
            barrier.newLayout <- ColorAttachmentRead.VkImageLayout
            DeviceApi.vkCmdPipelineBarrier
                (commandBuffer,
                    TransferSrc.PipelineStage,
                    ColorAttachmentRead.PipelineStage,
                    VkDependencyFlags.None,
                    0u, nullPtr, 0u, nullPtr,
                    1u, &&barrier)

            // update mipmap dimensions
            mipWidth <- nextWidth
            mipHeight <- nextHeight

        // transition final mip image left unfinished by loop
        barrier.srcAccessMask <- TransferDst.Access
        barrier.dstAccessMask <- ColorAttachmentRead.Access
        barrier.oldLayout <- TransferDst.VkImageLayout
        barrier.newLayout <- ColorAttachmentRead.VkImageLayout
        barrier.subresourceRange.baseMipLevel <- uint (mipLevels - 1)
        DeviceApi.vkCmdPipelineBarrier
            (commandBuffer,
                TransferDst.PipelineStage,
                ColorAttachmentRead.PipelineStage,
                VkDependencyFlags.None,
                0u, nullPtr, 0u, nullPtr,
                1u, &&barrier)

    /// Infer that an asset with the given file path should be filtered in a 2D rendering context.
    let inferTextureFiltered2d filePath =
        let name = PathF.GetFileNameWithoutExtension filePath
        name.EndsWith "_f" ||
        name.EndsWith "Filtered"
        
    /// Infer whether the texture at the given file path may be compressed.
    let inferTextureCompressible filePath =
        match PathF.GetExtensionLower filePath with
        | ".dds" | ".ktx" -> true
        | _ -> false
        
    /// Infer the type of block compression that an asset with the given file path should utilize.
    let inferTextureCompression filePath =
        if inferTextureCompressible filePath then
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
        else Uncompressed

    /// Detect that a dds file uses a compressed representation.
    let detectTextureCompressionDds (dds : DdsFile) =
        let format = dds.header.ddsPixelFormat.DxgiFormat
        let formatStr = string format
        formatStr.StartsWith "DxgiFormatBc" ||
        formatStr.StartsWith "DxgiFormatAtc"

    /// Detect that a ktx file uses a compressed representation.
    let detectTextureCompressionKtx (ktx : KtxFile) =
        let format = ktx.header.GlInternalFormat
        let formatStr = string format
        formatStr.StartsWith "GlCompressed"

    /// Write the binary header of a ktx file.
    /// Implementation based on https://registry.khronos.org/KTX/specs/1.0/ktxspec.v1.html
    let writeKtxHeader (resolution : Vector2i) mipmapLevels compressed (writer : BinaryWriter) =
        writer.Write                            // ktx identifier
            [|0xABuy; 0x4Buy; 0x54uy; 0x58uy    //
              0x20uy; 0x31uy; 0x31uy; 0xBBuy    //
              0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy|]  //
        writer.Write 0x04030201u                // endianness
        if compressed                           // glType
        then writer.Write 0x0000u               // (zero when compressed)
        else writer.Write 0x1401u               // OpenGL.Gl.UNSIGNED_BYTE
        writer.Write 1u                         // glTypeSize
        if compressed                           // glFormat
        then writer.Write 0x0000u               // (zero when compressed)
        else writer.Write 0x80E1u               // OpenGL.PixelFormat.Bgra
        if compressed                           // glInternalFormat
        then writer.Write 0x93B0u               // OpenGL.InternalFormat.CompressedRgbaAstc4x4
        else writer.Write 0x8058u               // OpenGL.InternalFormat.Rgba8
        writer.Write 0x80E1                     // glBaseInternalFormat = OpenGL.PixelFormat.Bgra
        writer.Write (uint32 resolution.X)      // width
        writer.Write (uint32 resolution.Y)      // height
        writer.Write 1u                         // depth
        writer.Write 0u                         // array elements
        writer.Write 1u                         // faces
        writer.Write (uint32 mipmapLevels)      // mip levels
        writer.Write 0u                         // key-value data size

    /// Attempt to generate uncompressed astc bytes from a MagickImage.
    let tryGenerateUncompressedImage (image : MagickImage) =
        let pixelBytes = image.GetPixels().ToByteArray(PixelMapping.BGRA) // uncompressed images are BGRA
        let resolution = v2i (int image.Width) (int image.Height)
        Some (resolution, pixelBytes)

    /// Attempt to generate uncompressed astc mipmap bytes from a MagickImage.
    let tryGenerateUncompressedMipmaps (image : MagickImage) =
        let mutable (width, height) = (image.Width, image.Height)
        let mipmapOpts =
            [while width >= 1u && height >= 1u do
                width <- width / 2u
                height <- height / 2u
                let mip = image.Clone () :?> MagickImage
                mip.Resize (width, height)
                tryGenerateUncompressedImage mip]
        match List.definitizePlus mipmapOpts with
        | (true, mipmaps) -> Some mipmaps
        | (false, _) -> None

    /// Attempt to compress a MagickImage to astc bytes.
    let tryCompressImage (image : MagickImage) =
    
        // attempt to configure astc encoder
        let pixelBytes = image.GetPixels().ToByteArray(PixelMapping.RGBA)
        let blockSize = 4u
        let mutable config = AstcencConfig ()
        let status = Astcenc.AstcencConfigInit (AstcencProfile.AstcencPrfLdr, blockSize, blockSize, 1u, Astcenc.AstcencPreMedium, Unchecked.defaultof<AstcencFlags>, &config)
        if status = AstcencError.AstcencSuccess then
    
            // attempt to initialize astc encoder
            let mutable context = AstcencContext ()
            let status = Astcenc.AstcencContextAlloc(ref config, 1u, &context)
            if status = AstcencError.AstcencSuccess then
    
                // attempt to compress astc image
                let mutable astcImage = AstcencImage (dimX = image.Width, dimY = image.Height, dimZ = 1u, dataType = AstcencType.AstcencTypeU8, data = [|pixelBytes|])
                let swizzle = AstcencSwizzle (r = AstcencSwz.AstcencSwzR, g = AstcencSwz.AstcencSwzG, b = AstcencSwz.AstcencSwzB, a = AstcencSwz.AstcencSwzA)
                let blockCountX = (uint image.Width + blockSize - 1u) / blockSize
                let blockCountY = (uint image.Height + blockSize - 1u) / blockSize
                let compressedLength = blockCountX * blockCountY * 16u
                let compressedData = Array.zeroCreate<byte> (int compressedLength)
                let status = Astcenc.AstcencCompressImage (context, &astcImage, swizzle, compressedData.AsSpan (), 0u)
                if status = AstcencError.AstcencSuccess
                then Some (v2i (int image.Width) (int image.Height), compressedData)
                else None
    
            // failure
            else None
    
        // failure
        else None

    /// Attempt to compress astc mipmap bytes from a MagickImage.
    let tryCompressMipmaps (image : MagickImage) =
        let mutable (width, height) = (image.Width, image.Height)
        let mipmapOpts =
            [while width >= 8u && height >= 8u do
                width <- width / 2u
                height <- height / 2u
                let mip = image.Clone () :?> MagickImage
                mip.Resize (width, height)
                tryCompressImage mip]
        match List.definitizePlus mipmapOpts with
        | (true, mipmaps) -> Some mipmaps
        | (false, _) -> None

    /// Attempt to format an uncompressed pfim image texture (non-mipmap).
    let tryFormatUncompressedPfimageTexture format height stride (data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < height do
                    let mutable x = 0
                    while x < stride - 2 do
                        let i = x + stride * y
                        data[i]; data[i+1]; data[i+2]; 255uy
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
                        data[i]; data[i+1]; data[i+2]; data[i+3]
                        x <- x + 4
                    y <- inc y|]
            Some converted
        | _ -> Log.info ("Unsupported image format '" + scstring format + "'."); None

    /// Format an uncompressed pfim image mipmap.
    let formatUncompressedPfimageMipmap format (mipmap : MipMapOffset) (data : byte array) =
        match format with
        | ImageFormat.Rgb24 ->
            let converted =
                [|let mutable y = 0
                  while y < mipmap.Height do
                    let mutable x = 0
                    while x < mipmap.Stride - 2 do
                        let i = x + mipmap.Stride * y + mipmap.DataOffset
                        data[i]; data[i+1]; data[i+2]; 255uy
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
                        data[i]; data[i+1]; data[i+2]; data[i+3]
                        x <- x + 4
                    y <- inc y|]
            (v2i mipmap.Width mipmap.Height, converted)
        | _ -> failwithumf ()

    /// Attempt to format an uncompressed pfim image.
    let tryFormatUncompressedPfimage minimal (image : IImage) =
        let minimal = minimal && image.MipMaps.Length >= 1 // NOTE: at least one mipmap is needed for minimal load.
        let data = image.Data // OPTIMIZATION: pulling all values out of image to avoid slow property calls.
        let format = image.Format
        let height = image.Height
        let stride = image.Stride
        let mipmaps = image.MipMaps
        let bytesOpt =
            if not minimal
            then tryFormatUncompressedPfimageTexture format height stride data
            else Some [||]
        match bytesOpt with
        | Some bytes ->
            let minimalMipmapIndex =
                if minimal
                then min (dec mipmaps.Length) (dec Constants.Render.TextureMinimalMipmapIndex)
                else 0
            let mipmapBytesArray =
                [|for i in minimalMipmapIndex .. dec mipmaps.Length do
                    formatUncompressedPfimageMipmap format mipmaps[i] data|]
            if minimal then
                let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray[0]
                let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
                Some (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
            else Some (v2i image.Width image.Height, bytes, mipmapBytesArray)
        | None -> None

    /// Format compressed pfim image data.
    let formatCompressedPfdds minimal (dds : Dds) =
        let minimal = minimal && dds.Header.MipMapCount >= 3u // NOTE: at least three mipmaps are needed for minimal load since the last 2 are not valid when compressed.
        let mutable dims = v2i dds.Width dds.Height
        let mutable size = ((dims.X + 3) / 4) * ((dims.Y + 3) / 4) * 16
        let mutable index = 0
        let bytes =
            if not minimal
            then dds.Data.AsSpan(index, size).ToArray()
            else [||]
        let minimalMipmapIndex =
            if minimal
            then min dds.Header.MipMapCount (uint Constants.Render.TextureMinimalMipmapIndex)
            else 1u
        let mipmapBytesArray =
            if dds.Header.MipMapCount >= 2u then
                [|for i in 1u .. dds.Header.MipMapCount do
                    dims <- dims / 2
                    index <- index + size
                    size <- size / 4
                    if  i >= minimalMipmapIndex &&
                        size >= 16 then // NOTE: as mentioned above, mipmap with size < 16 can exist but isn't valid when compressed.
                        (dims, dds.Data.AsSpan(index, size).ToArray())|]
            else [||]
        if minimal then
            let (minimalMipmapResolution, minimalMipmapBytes) = mipmapBytesArray[0]
            let remainingMipmapBytes = if minimalMipmapBytes.Length > 1 then Array.tail mipmapBytesArray else [||]
            (minimalMipmapResolution, minimalMipmapBytes, remainingMipmapBytes)
        else (v2i dds.Width dds.Height, bytes, mipmapBytesArray)

    /// Report the fact that a draw call has just been made with the given number of instances.
    let reportDrawScope () =
        lock DrawCountersLock (fun () ->
            DrawScopeCount <- inc DrawScopeCount )

    /// Report the fact that a draw call has just been made with the given number of instances.
    let reportDrawCall drawInstances drawScope =
        lock DrawCountersLock (fun () ->
            DrawInstanceCount <- DrawInstanceCount + drawInstances
            DrawCallCount <- inc DrawCallCount
            if drawScope then DrawScopeCount <- inc DrawScopeCount )

    /// Reset the running counts of draw events.
    let resetDrawCounters () =
        lock DrawCountersLock (fun () ->
            DrawInstanceCount <- 0
            DrawCallCount <- 0
            DrawScopeCount <- 0)

    /// Get the running number of draw scopes.
    let getDrawScopeCount () =
        lock DrawCountersLock (fun () -> DrawScopeCount)

    /// Get the running number of draw calls.
    let getDrawCallCount () =
        lock DrawCountersLock (fun () -> DrawCallCount)

    /// Get the running number of draw calls.
    let getDrawInstanceCount () =
        lock DrawCountersLock (fun () -> DrawInstanceCount)