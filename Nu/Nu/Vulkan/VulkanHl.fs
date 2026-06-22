// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Runtime.InteropServices
open System.Threading
open System.IO
open FSharp.NativeInterop
open SDL
open Vortice.ShaderCompiler
open Vortice.Vulkan
open Prime
open Nu

/// The format of an image.
type ImageFormat =
    | Rgba8
    | Rgba16f
    | Rgb16f
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
        | Rgb16f -> VkFormat.R16G16B16Sfloat
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
        | Rgb16f
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
        | Rgb16f -> width * height * 6
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

    /// Determine if format is supported for use as an attachment.
    static member supportsAttachment vkPhysicalDevice format =
        let requiredFeatures =
            match format with
            | Rgba8
            | Rgba16f
            | Rgb16f
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
        Vulkan.vkGetPhysicalDeviceFormatProperties (vkPhysicalDevice, format.VkFormat, &properties)
        properties.optimalTilingFeatures &&& requiredFeatures = requiredFeatures
    
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
    | ShaderRead
    | ColorAttachmentWrite
    | DepthAttachment
    | Present

    /// The VkImageLayout.
    member this.VkImageLayout =
        match this with
        | Undefined -> VkImageLayout.Undefined
        | TransferSrc -> VkImageLayout.TransferSrcOptimal
        | TransferDst -> VkImageLayout.TransferDstOptimal
        | ShaderRead -> VkImageLayout.ShaderReadOnlyOptimal
        | ColorAttachmentWrite -> VkImageLayout.ColorAttachmentOptimal
        | DepthAttachment -> VkImageLayout.DepthStencilAttachmentOptimal
        | Present -> VkImageLayout.PresentSrcKHR

    /// The access flag.
    member this.Access =
        match this with
        | Undefined -> VkAccessFlags.None
        | TransferSrc -> VkAccessFlags.TransferRead
        | TransferDst -> VkAccessFlags.TransferWrite
        | ShaderRead -> VkAccessFlags.ShaderRead
        | ColorAttachmentWrite -> VkAccessFlags.ColorAttachmentWrite
        | DepthAttachment -> VkAccessFlags.DepthStencilAttachmentRead ||| VkAccessFlags.DepthStencilAttachmentWrite
        | Present -> VkAccessFlags.None

    /// The pipeline stage.
    member this.PipelineStage =
        match this with
            
        // NOTE: DJL: for Undefined as image layout transition source, texture upload and mipmap generation previously used VK_PIPELINE_STAGE_HOST_BIT.
        // I can't remember why but it's not in the tutorial and apparently may lead to failure on Android devices. I suspect it was inherited
        // from ImGui backend.
        | Undefined -> VkPipelineStageFlags.TopOfPipe
        | TransferSrc -> VkPipelineStageFlags.Transfer
        | TransferDst -> VkPipelineStageFlags.Transfer
        | ShaderRead -> VkPipelineStageFlags.FragmentShader
        | ColorAttachmentWrite -> VkPipelineStageFlags.ColorAttachmentOutput
        | DepthAttachment -> VkPipelineStageFlags.EarlyFragmentTests
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
    | VertexFragmentStage

    /// The VkShaderStageFlags.
    member this.VkShaderStageFlags =
        match this with
        | VertexStage -> VkShaderStageFlags.Vertex
        | FragmentStage -> VkShaderStageFlags.Fragment
        | VertexFragmentStage -> VkShaderStageFlags.Vertex ||| VkShaderStageFlags.Fragment
    
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

///
type SurfaceState =
    | SurfaceReady
    | SurfaceLost
    | SurfaceDestroyed

/// Represents a strict cycle ensuring that any presentation resources (surface and swapchains) that exist or are being created during the onset
/// of app backgrounding on a mobile device are torn down/cancelled.
/// TODO: DJL: encapsulate most of this stuff into a Surface abstraction as it should not be visible to Swapchain and VulkanContext.
type internal BackgroundingResponseState =
    | PresentationSetupInitiated // setup of presentation resources has begun and may be complete
    | PresentationTeardownPending // presentation resources can no longer be trusted as app has commenced backgrounding
    | PresentationTeardownComplete // presentation resources have been destroyed and restoration will commence when app is back in foreground

// TODO: DJL: for mobile devices: https://learn.microsoft.com/en-us/dotnet/standard/native-interop/calling-conventions#when-you-can-omit-the-calling-convention.
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type internal DebugDelegate =
    delegate of VkDebugUtilsMessageSeverityFlagsEXT * VkDebugUtilsMessageTypeFlagsEXT * nativeint * nativeint -> uint32

// https://github.com/amerkoleci/Vortice.Vulkan/blob/32035603790b64f4c96a979193a7e1391d34a428/src/Vortice.Vulkan/Generated/Structures.cs#L14978
// VkDebugUtilsMessengerCreateInfoEXT with pfnUserCallback as "real" nativeint instead of "fake" nativeint which is actually a function pointer type
// TODO: report this F# compiler bug that allows assigning to "fake" nativeint to compile without error but causes a crash at runtime
type [<Struct>] internal VkDebugUtilsMessengerCreateInfoEXT_hack =
    val mutable sType : VkStructureType
    val mutable pNext : nativeint
    val mutable flags : VkDebugUtilsMessengerCreateFlagsEXT
    val mutable messageSeverity : VkDebugUtilsMessageSeverityFlagsEXT
    val mutable messageType : VkDebugUtilsMessageTypeFlagsEXT
    val mutable pfnUserCallback : nativeint // "real" nativeint
    val mutable pUserData : nativeint

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type internal BackgroundingDelegate = delegate of userData : voidptr * event : SDL_Event nativeptr -> SDLBool    

[<RequireQualifiedAccess>]
module Hl =

    // TODO: DJL: all these free-floating variables, types and functions have become a
    // bit of a mess and need to be reordered, not to mention the inconsistent casing.

    let internal ValidationLayersEnabled =
#if DEBUG
        true
#else
        false
#endif

    let mutable internal ValidationLayersActivated = false
    
    let mutable internal DrawReportLock = obj ()
    let mutable internal DrawCallCount = 0
    let mutable internal DrawInstanceCount = 0

    // provides id for a texture on the gpu that is globally unique i.e. cannot be reused after texture is destroyed,
    // which is essential for tracking descriptor writes
    let mutable private TextureIdGenerationLock = obj ()
    let mutable private TextureIdCounter = 0UL
    
    /// Index of the current Swapchain image.
    let mutable internal ImageIndex = 0u

    /// The current frame within MaxFramesInFlight.
    /// TODO: DJL: figure out how to prevent potential outside mutation.
    let mutable internal CurrentFrame = 0

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

    // callback to inform render loop about app backgrounding
    // official documentation for android case: https://github.com/libsdl-org/SDL/blob/main/docs/README-android.md#activity-lifecycle
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

    let private BackgroundingDelegate =
        BackgroundingDelegate handleBackgrounding

    // set up delegate for app backgrounding callback
    // TODO: DJL: for mobile devices: https://learn.microsoft.com/en-us/dotnet/standard/native-interop/calling-conventions#when-you-can-omit-the-calling-convention.
    let internal BackgroundingCallback =
        Marshal.GetFunctionPointerForDelegate<BackgroundingDelegate> BackgroundingDelegate

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

    /// Check if an image format is supported for attachments, falling back to a standard format where possible.
    let rec checkAttachmentFormat (vkPhysicalDevice, format : ImageFormat) =
        if not (ImageFormat.supportsAttachment vkPhysicalDevice format) then
            
            // NOTE: DJL: format fallbacks must not be ints for blit conversion.
            let (formatFallback : ImageFormat) =
                match format with
                | Bc3
                | Bc5
                | Astc ->
                    Log.fail ("Compressed image formats are not supported for attachment textures.")
                | Rgb16f ->
                    checkAttachmentFormat (vkPhysicalDevice, Rgba16f)
                | Rgba8 (* standard *)
                | Rgba16f (* standard *)
                | Rg32f (* standard *)
                | R16f (* standard *)
                | R32f (* standard *) ->
                    // NOTE: DJL: for spec requirements, see https://docs.vulkan.org/spec/latest/chapters/formats.html#features-required-format-support.
                    Log.fail ("Vulkan attachment image format '" + scstring format.VkFormat + "' support is absent but required. Further, it's a requirement in the Vulkan specification!")
                | D32f ->
                    checkAttachmentFormat (vkPhysicalDevice, D32fs8ui)
                | D32fs8ui ->
                    checkAttachmentFormat (vkPhysicalDevice, D24s8ui)
                | D24s8ui ->
                    checkAttachmentFormat (vkPhysicalDevice, X8d24Pack32)
                | X8d24Pack32 ->
                    checkAttachmentFormat (vkPhysicalDevice, D16)
                | D16 ->
                    checkAttachmentFormat (vkPhysicalDevice, D16s8ui)
                | D16s8ui ->
                    Log.fail "Could not find a suitable format for depth attachment textures."
            Log.warn ("Falling back to " + scstring formatFallback.VkFormat + " attachment format due to unavailability of " + scstring format.VkFormat + " attachment format.")
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

    /// Make a VkRenderingInfo.
    let inline makeRenderingInfo (colorAttachments : VkImageView array) depthAttachmentOpt renderArea clearValueOpt =
    
        // NOTE: DJL: must be inline to keep pointers valid.
    
        // color attachment infos
        let cInfos = Array.zeroCreate colorAttachments.Length
        for i in 0 .. dec cInfos.Length do
            let mutable cInfo = VkRenderingAttachmentInfo ()
            cInfo.imageView <- colorAttachments.[i]
            cInfo.imageLayout <- ColorAttachmentWrite.VkImageLayout
            cInfo.storeOp <- VkAttachmentStoreOp.Store
            match clearValueOpt with
            | Some clearValue ->
                cInfo.loadOp <- VkAttachmentLoadOp.Clear
                cInfo.clearValue <- clearValue
            | None ->
                cInfo.loadOp <- VkAttachmentLoadOp.Load
            cInfos.[i] <- cInfo
        use cInfosPin = new ArrayPin<_> (cInfos)

        // depth attachment info
        let mutable dInfo = VkRenderingAttachmentInfo ()
        match depthAttachmentOpt with
        | Some depthAttachment ->
            dInfo.imageView <- depthAttachment
            dInfo.imageLayout <- DepthAttachment.VkImageLayout
            dInfo.storeOp <- VkAttachmentStoreOp.Store
            match clearValueOpt with
            | Some _ ->
                dInfo.loadOp <- VkAttachmentLoadOp.Clear
                dInfo.clearValue <- VkClearValue (1.0f, 0u)
            | None ->
                dInfo.loadOp <- VkAttachmentLoadOp.Load
        | None -> ()
    
        // rendering info
        let mutable rInfo = VkRenderingInfo ()
        rInfo.renderArea <- renderArea
        rInfo.layerCount <- 1u
        rInfo.colorAttachmentCount <- uint cInfos.Length
        rInfo.pColorAttachments <- cInfosPin.Pointer
        if depthAttachmentOpt.IsSome then rInfo.pDepthAttachment <- asPointer &dInfo
        rInfo

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

    let tryCreateVulkanSurface window instance =

        // attempt to recreate surface if destroyed
        match SurfaceState with
        | SurfaceDestroyed ->
        
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

        | SurfaceReady -> Log.error "Attempted creation of Vulkan surface when existing surface has not been destroyed!"
        | SurfaceLost -> Log.error "Attempted creation of Vulkan surface when existing surface has been lost but not destroyed!"
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

    let destroyVulkanSurface instance =
        match SurfaceState with
        | SurfaceReady
        | SurfaceLost ->
            Vulkan.vkDestroySurfaceKHR (instance, Surface, nullPtr)
            SurfaceState <- SurfaceDestroyed

            // inform the backgrounding callback that the required teardown of presentation is complete
            // so no action is required if another backgrounding event is triggered prior to recreation;
            // this must correspond exactly with SurfaceDestroyed, which is used by Swapchain
            setPresentationTeardownComplete ()
        | SurfaceDestroyed ->
            Log.error "Attempted destruction of Vulkan surface that has already been destroyed!"

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then
            let message = "Vulkan assertion failed due to: " + string result
#if DEBUG
            Log.fail message
#else
            Log.error message
#endif            

    /// Report the fact that a draw call has just been made with the given number of instances.
    let reportDrawCall drawInstances =
        lock DrawReportLock (fun () ->
            DrawCallCount <- inc DrawCallCount
            DrawInstanceCount <- DrawInstanceCount + drawInstances)

    /// Reset the running number of draw calls.
    let resetDrawCalls () =
        lock DrawReportLock (fun () ->
            DrawCallCount <- 0
            DrawInstanceCount <- 0)

    /// Get the running number of draw calls.
    let getDrawCallCount () =
        lock DrawReportLock (fun () -> DrawCallCount)

    /// Get the running number of draw calls.
    let getDrawInstanceCount () =
        lock DrawReportLock (fun () -> DrawInstanceCount)

    /// Try to compile GLSL file to SPIR-V code.
    let tryCompileShader shaderPath shaderKind =
        use shaderStream = new StreamReader (File.OpenRead shaderPath)
        let shaderStr = shaderStream.ReadToEnd ()
        use compiler = new Compiler ()
        let options = CompilerOptions ()
        options.ShaderStage <- shaderKind
        let result = compiler.Compile (shaderStr, shaderPath, options)
        if result.Status = CompilationStatus.Success
        then Right result.Bytecode
        else Left ("Vulkan shader compilation failed due to:\n" + result.ErrorMessage)

    /// Try to create a shader module from a GLSL file.
    let tryCreateShaderModuleFromGlsl shaderPath shaderKind device =
        match tryCompileShader shaderPath shaderKind with
        | Right shader ->

            // NOTE: DJL: using a high level overload here to avoid questions about reinterpret casting and memory alignment,
            // see https://vulkan-tutorial.com/Drawing_a_triangle/Graphics_pipeline_basics/Shader_modules#page_Creating-shader-modules.
            let mutable shaderModule = Unchecked.defaultof<VkShaderModule>
            Vulkan.vkCreateShaderModule (device, shader, nullPtr, &shaderModule) |> check
            Right shaderModule
        | Left msg -> Left msg

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
        Vulkan.vkCmdPipelineBarrier
            (commandBuffer,
             oldLayout.PipelineStage,
             newLayout.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, asPointer &barrier)

    /// Try get surface capabilities.
    let tryGetSurfaceCapabilities vkPhysicalDevice =
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        let result = Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, Surface, &capabilities)
        if result <> VkResult.ErrorSurfaceLostKHR then
            check result
            Some capabilities
        else
            SurfaceState <- SurfaceLost
            None

    /// Get swap extent.
    let getSwapExtent (capabilities : VkSurfaceCapabilitiesKHR) window =

        // check if window size is fixed or variable
        if capabilities.currentExtent.width <> UInt32.MaxValue
        then capabilities.currentExtent
        else

            // get pixel resolution from sdl
            // NOTE: DJL: unlike the GLFW counterpart, this does NOT return 0 when minimized.
            // TODO: DJL: find out if that's still true for SDL3.
            let mutable width = Unchecked.defaultof<int>
            let mutable height = Unchecked.defaultof<int>
            if not (SDL3.SDL_GetWindowSizeInPixels (window, &&width, &&height)) then
                Log.fail (SDL3.SDL_GetError ())

            // clamp resolution to size limits
            width <- max width (int capabilities.minImageExtent.width)
            width <- min width (int capabilities.maxImageExtent.width)
            height <- max height (int capabilities.minImageExtent.height)
            height <- min height (int capabilities.maxImageExtent.height)

            // fin
            VkExtent2D (width, height)
    
    /// Create an image view.
    let createImageView pixelFormat vkFormat mipLevel mipCount (layer : int) (layerCount : int) viewType imageAspect image device =
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- viewType
        info.format <- vkFormat
        info.components <- makeComponentMapping pixelFormat
        info.subresourceRange <- makeSubresourceRange mipLevel mipCount layer layerCount imageAspect
        let mutable imageView = Unchecked.defaultof<VkImageView>
        Vulkan.vkCreateImageView (device, &info, nullPtr, &imageView) |> check
        imageView

    /// Allocate an array of command buffers.
    let allocateCommandBuffers count commandPool device =
        let mutable info = VkCommandBufferAllocateInfo ()
        info.commandPool <- commandPool
        info.level <- VkCommandBufferLevel.Primary
        info.commandBufferCount <- uint count
        let commandBuffers = Array.zeroCreate<VkCommandBuffer> count
        use commandBuffersPin = new ArrayPin<_> (commandBuffers)
        Vulkan.vkAllocateCommandBuffers (device, asPointer &info, commandBuffersPin.Pointer) |> check
        commandBuffers

    /// Allocate a command buffer.
    let allocateCommandBuffer commandPool device =
        let commandBuffers = allocateCommandBuffers 1 commandPool device
        commandBuffers.[0]

    /// Create a semaphore.
    let createSemaphore device =
        let info = VkSemaphoreCreateInfo ()
        let mutable semaphore = Unchecked.defaultof<VkSemaphore>
        Vulkan.vkCreateSemaphore (device, &info, nullPtr, &semaphore) |> check
        semaphore

    /// Create a fence.
    let createFence createSignaled device =
        let info =
            if createSignaled then VkFenceCreateInfo (flags = VkFenceCreateFlags.Signaled)
            else VkFenceCreateInfo ()
        let mutable fence = Unchecked.defaultof<VkFence>
        Vulkan.vkCreateFence (device, &info, nullPtr, &fence) |> check
        fence

    /// Wait for a fence to signal and reset it for reuse.
    let awaitFence fence device =
        let mutable fence = fence
        Vulkan.vkWaitForFences (device, 1u, asPointer &fence, true, UInt64.MaxValue) |> check
        Vulkan.vkResetFences (device, 1u, asPointer &fence) |> check

    /// Create a persistent command buffer.
    let createPersistentCommandBuffer commandBuffer =
        Vulkan.vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo ()
        Vulkan.vkBeginCommandBuffer (commandBuffer, asPointer &cbInfo) |> check

    /// Create a transient command buffer.
    /// TODO: DJL: review choice of transient command buffers over normal ones.
    let createTransientCommandBuffer commandPool device =
        let commandBuffer = allocateCommandBuffer commandPool device
        let mutable cbInfo = VkCommandBufferBeginInfo (flags = VkCommandBufferUsageFlags.OneTimeSubmit)
        Vulkan.vkBeginCommandBuffer (commandBuffer, asPointer &cbInfo) |> check
        commandBuffer

    let findMemoryType typeFilter properties physicalDevice =

        // get memory types
        let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
        Vulkan.vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
        let memoryTypes = NativePtr.fixedBufferToArray<VkMemoryType> (int memProperties.memoryTypeCount) memProperties.memoryTypes

        // try find suitable memory type
        let mutable memoryTypeOpt = None
        for i in 0 .. dec memoryTypes.Length do
            match memoryTypeOpt with
            | None when typeFilter &&& (1u <<< i) <> 0u && memoryTypes.[i].propertyFlags &&& properties = properties ->
                memoryTypeOpt <- Some (uint i)
            | Some _ | None -> ()

        // fin
        match memoryTypeOpt with
        | Some memoryType -> memoryType
        | None -> Log.fail "Failed to find suitable memory type!"

    let areAligned a b =
        if a = b then true
        elif a > b then a % b = 0
        else b % a = 0

    // TODO: DJL: perhaps calculating this stuff manually is a bad idea?
    let getStride alignment size =
        if size = 0 then size // just to prevent division by 0; size should be > 0
        elif alignment = 0 then size
        elif alignment = size then size
        elif size > alignment && size % alignment = 0 then size
        elif alignment % size = 0 then size
        else (size / alignment + 1) * alignment // stride = lowest multiple of alignment that contains size

    let alignOffset offset alignment =
        if alignment = 0 then offset // no alignment
        elif offset = 0 then offset // no offset to align
        elif areAligned offset alignment then offset // offset already aligned
        else (offset / alignment + 1) * alignment // offset shifted forward to align

    let getMinimumBufferSize offset alignment size count =
        let stride = getStride alignment size
        let offset = alignOffset offset alignment
        offset + stride * count