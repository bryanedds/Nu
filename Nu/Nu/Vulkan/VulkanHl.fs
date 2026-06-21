// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Runtime.InteropServices
open System.IO
open FSharp.NativeInterop
open SDL
open Vortice.ShaderCompiler
open Vortice.Vulkan
open Prime
open Nu

type internal SurfaceState =
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

[<RequireQualifiedAccess>]
module internal HlInternal =

    // TODO: DJL: all these free-floating variables, types and functions have become a
    // bit of a mess and need to be reordered, not to mention the inconsistent casing.
    
    let mutable internal DrawReportLock = obj ()
    let mutable internal DrawCallCount = 0
    let mutable internal DrawInstanceCount = 0

    let internal ValidationLayersEnabled =
#if DEBUG
        true
#else
        false
#endif

    let mutable internal ValidationLayersActivated = false

    // provides id for a texture on the gpu that is globally unique i.e. cannot be reused after texture is destroyed,
    // which is essential for tracking descriptor writes
    let mutable private TextureIdGenerationLock = obj ()
    let mutable private TextureIdCounter = 0UL
    let internal GenTextureId () = lock TextureIdGenerationLock (fun () -> TextureIdCounter <- inc TextureIdCounter; TextureIdCounter)
    
    /// Index of the current Swapchain image.
    let mutable internal ImageIndex = 0u

    /// The current frame within MaxFramesInFlight.
    /// TODO: DJL: figure out how to prevent potential outside mutation.
    let mutable internal CurrentFrame = 0

    /// The forward-declared empty texture value.
    /// Initialized in RendererProcesses.
    /// NOTE: if performance issues arise from checking / casting this, maybe use ValueOption or null directly.
    /// TODO: see if instead of exposing mutability of this directly, we should define Init and CleanUp fns.
    let mutable internal EmptyOpt : obj option = None

    let mutable internal SurfaceState = SurfaceDestroyed
    let mutable internal Surface = Unchecked.defaultof<VkSurfaceKHR>
    
    // presentation teardown in response to app backgrounding follows BackgroundingResponseState cycle,
    // whereas presentation setup need only care whether app is *currently* in foreground
    let mutable private BackgroundingResponseState = PresentationTeardownComplete
    let mutable internal IsAppInForeground = true
    
    // thread-safe access to BackgroundingResponseState as it's essentially a semaphore that needs to be set by callback *and* render thread
    let mutable private BackgroundingResponseStateLock = obj ()
    let internal setPresentationSetupInitiated () = lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState <- PresentationSetupInitiated)
    let internal setPresentationTeardownComplete () = lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState <- PresentationTeardownComplete)

    /// Has app been SET for backgrounding (i.e. not necessarily IN background yet/still), invalidating existing surface.
    let internal hasAppBegunBackgrounding () = lock BackgroundingResponseStateLock (fun () -> BackgroundingResponseState = PresentationTeardownPending)

    // callback to inform render loop about app backgrounding
    // official documentation for android case: https://github.com/libsdl-org/SDL/blob/main/docs/README-android.md#activity-lifecycle
    let private handleBackgrounding (userData : voidptr) (event : SDL_Event nativeptr) : SDLBool =
        ignore userData
        let event = NativePtr.toByRef event
        match event.Type with
        | SDL_EventType.SDL_EVENT_WILL_ENTER_BACKGROUND ->
            IsAppInForeground <- false
            lock BackgroundingResponseStateLock (fun () ->
                if BackgroundingResponseState = PresentationSetupInitiated then BackgroundingResponseState <- PresentationTeardownPending)
            true
        | SDL_EventType.SDL_EVENT_DID_ENTER_FOREGROUND ->
            IsAppInForeground <- true
            true
        | _ -> true

    // set up delegate for app backgrounding callback
    // TODO: DJL: for mobile devices: https://learn.microsoft.com/en-us/dotnet/standard/native-interop/calling-conventions#when-you-can-omit-the-calling-convention.
    [<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
    type private BackgroundingDelegate = delegate of userData : voidptr * event : SDL_Event nativeptr -> SDLBool
    let private backgroundingDelegate = BackgroundingDelegate handleBackgrounding
    let internal backgroundingCallback = Marshal.GetFunctionPointerForDelegate<BackgroundingDelegate> backgroundingDelegate

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
    | Int
    | Int2
    | Int3
    | Int4
    | Uint
    | Uint2
    | Uint3
    | Uint4
    | Quarter
    | Quarter2
    | Quarter3
    | Quarter4
    | Half
    | Half2
    | Half3
    | Half4
    | Single
    | Single2
    | Single3
    | Single4
    | Double
    | Double2
    | Double3
    | Double4

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

[<RequireQualifiedAccess>]
module Hl =

    /// Check if an image format is supported for attachments, falling back to a standard format where possible.
    let rec CheckAttachmentFormat (vkPhysicalDevice, format : ImageFormat) =
        if not (ImageFormat.supportsAttachment vkPhysicalDevice format) then
            
            // NOTE: DJL: format fallbacks must not be ints for blit conversion.
            let (formatFallback : ImageFormat) =
                match format with
                | Bc3
                | Bc5
                | Astc ->
                    Log.fail ("Compressed image formats are not supported for attachment textures.")
                | Rgb16f ->
                    CheckAttachmentFormat (vkPhysicalDevice, Rgba16f)
                | Rgba8 (* standard *)
                | Rgba16f (* standard *)
                | Rg32f (* standard *)
                | R16f (* standard *)
                | R32f (* standard *) ->
                    
                    // NOTE: DJL: for spec requirements, see https://docs.vulkan.org/spec/latest/chapters/formats.html#features-required-format-support.
                    Log.fail ("Vulkan attachment image format '" + scstring format.VkFormat + "' support is absent but required. Further, it's a requirement in the Vulkan specification!")
                | D32f ->
                    CheckAttachmentFormat (vkPhysicalDevice, D32fs8ui)
                | D32fs8ui ->
                    CheckAttachmentFormat (vkPhysicalDevice, D24s8ui)
                | D24s8ui ->
                    CheckAttachmentFormat (vkPhysicalDevice, X8d24Pack32)
                | X8d24Pack32 ->
                    CheckAttachmentFormat (vkPhysicalDevice, D16)
                | D16 ->
                    CheckAttachmentFormat (vkPhysicalDevice, D16s8ui)
                | D16s8ui ->
                    Log.fail "Could not find a suitable format for depth attachment textures."
            Log.warn ("Falling back to " + scstring formatFallback.VkFormat + " attachment format due to unavailability of " + scstring format.VkFormat + " attachment format.")
            formatFallback
        else format

    /// Convert VkExtensionProperties.extensionName to a string.
    /// TODO: see if we can inline functions like these once F# supports C#'s representation of this fixed buffer type.
    let internal getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName

    /// Convert VkLayerProperties.layerName to a string.
    let internal getLayerName (layerProps : VkLayerProperties) =
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

    let internal tryCreateVulkanSurface window instance =
        match HlInternal.SurfaceState with
        | SurfaceDestroyed ->
        
            // inform the backgrounding callback that we begin the process of creating the surface and swapchain
            // that may need to be aborted/destroyed at any point before *or* after completion due to a
            // backgrounding event, hence setup *initiated*
            HlInternal.setPresentationSetupInitiated ()
            let mutable surfacePtr = Unchecked.defaultof<VkSurfaceKHR_T nativeptr>
            let instance = NativePtr.ofNativeInt (VkInstance.op_Implicit instance)
            if not (SDL3.SDL_Vulkan_CreateSurface (window, instance, NativePtr.nullPtr, &&surfacePtr)) then
                Log.error (SDL3.SDL_GetError ())
                HlInternal.setPresentationTeardownComplete () // inform callback to scratch that
            else
                HlInternal.Surface <- NativePtr.toNativeInt surfacePtr |> uint64 |> VkSurfaceKHR.op_Implicit
                HlInternal.SurfaceState <- SurfaceReady
        | SurfaceReady -> Log.error "Attempted creation of Vulkan surface when existing surface has not been destroyed!"
        | SurfaceLost -> Log.error "Attempted creation of Vulkan surface when existing surface has been lost but not destroyed!"

    let internal createVulkanSurface window instance =
    
        // wait for app to enter foreground if not already
        while not HlInternal.IsAppInForeground do ()
        tryCreateVulkanSurface window instance

        // cannot tolerate failure as this function is intended to guarantee surface creation, otherwise must set up a retry mechanism
        if HlInternal.SurfaceState.IsSurfaceDestroyed then Log.fail "Vulkan surface creation failed."

    let internal destroyVulkanSurface instance =
        match HlInternal.SurfaceState with
        | SurfaceReady
        | SurfaceLost ->
            Vulkan.vkDestroySurfaceKHR (instance, HlInternal.Surface, nullPtr)
            HlInternal.SurfaceState <- SurfaceDestroyed

            // inform the backgrounding callback that the required teardown of presentation is complete
            // so no action is required if another backgrounding event is triggered prior to recreation;
            // this must correspond exactly with SurfaceDestroyed, which is used by Swapchain
            HlInternal.setPresentationTeardownComplete ()
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
        lock HlInternal.DrawReportLock (fun () ->
            HlInternal.DrawCallCount <- inc HlInternal.DrawCallCount
            HlInternal.DrawInstanceCount <- HlInternal.DrawInstanceCount + drawInstances)

    /// Reset the running number of draw calls.
    let resetDrawCalls () =
        lock HlInternal.DrawReportLock (fun () ->
            HlInternal.DrawCallCount <- 0
            HlInternal.DrawInstanceCount <- 0)

    /// Get the running number of draw calls.
    let getDrawCallCount () =
        lock HlInternal.DrawReportLock (fun () -> HlInternal.DrawCallCount)

    /// Get the running number of draw calls.
    let getDrawInstanceCount () =
        lock HlInternal.DrawReportLock (fun () -> HlInternal.DrawInstanceCount)

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
    let internal tryGetSurfaceCapabilities vkPhysicalDevice =
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        let result = Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, HlInternal.Surface, &capabilities)
        if result <> VkResult.ErrorSurfaceLostKHR then
            check result
            Some capabilities
        else
            HlInternal.SurfaceState <- SurfaceLost
            None

    /// Get swap extent.
    let internal getSwapExtent (capabilities : VkSurfaceCapabilitiesKHR) window =

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
    let createImageView pixelFormat vkFormat mipLevel mipCount layer layerCount viewType imageAspect image device =
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

/// A command queue that internally synchronizes use across multiple threads.
/// TODO: P0: rename this to CommandQueue?
type [<ReferenceEquality>] Queue =
    private
        { VkQueue : VkQueue
          Lock : obj }

    /// Create a Queue.
    static member create queueFamilyIndex queueIndex device =
    
        // get VkQueue
        let mutable vkQueue = Unchecked.defaultof<VkQueue>
        Vulkan.vkGetDeviceQueue (device, queueFamilyIndex, queueIndex, &vkQueue)

        // make Queue
        let queue =
            { VkQueue = vkQueue
              Lock = obj () }

        // fin
        queue

    /// Wait for Queue to finish execution.
    static member waitIdle queue =
        lock queue.Lock (fun () -> Vulkan.vkQueueWaitIdle queue.VkQueue |> Hl.check)

    /// Submit persistent command buffer for execution.
    static member submit commandBuffer waitSemaphoresStages (signalSemaphores : VkSemaphore array) signalFence (queue : Queue) =
        lock queue.Lock (fun () ->

            // end command buffer
            Vulkan.vkEndCommandBuffer commandBuffer |> Hl.check
        
            // unpack and pin arrays
            let (waitSemaphores, waitStages) = Array.unzip waitSemaphoresStages
            use waitSemaphoresPin = new ArrayPin<_> (waitSemaphores)
            use waitStagesPin = new ArrayPin<_> (waitStages)
            use signalSemaphoresPin = new ArrayPin<_> (signalSemaphores)

            // submit commands
            let mutable commandBuffer = commandBuffer
            let mutable info = VkSubmitInfo ()
            info.waitSemaphoreCount <- uint waitSemaphores.Length
            info.pWaitSemaphores <- waitSemaphoresPin.Pointer
            info.pWaitDstStageMask <- waitStagesPin.Pointer
            info.commandBufferCount <- 1u
            info.pCommandBuffers <- asPointer &commandBuffer
            info.signalSemaphoreCount <- uint signalSemaphores.Length
            info.pSignalSemaphores <- signalSemaphoresPin.Pointer
            Vulkan.vkQueueSubmit (queue.VkQueue, 1u, asPointer &info, signalFence) |> Hl.check)

    /// Execute and free transient command buffer. Command pool and fence must NOT be shared between threads!
    static member executeTransient commandBuffer commandPool finishFence (queue : Queue) device =
        let mutable commandBuffer = commandBuffer
        lock queue.Lock (fun () ->
        
            // end command buffer
            Vulkan.vkEndCommandBuffer commandBuffer |> Hl.check

            // submit commands
            let mutable info = VkSubmitInfo ()
            info.commandBufferCount <- 1u
            info.pCommandBuffers <- asPointer &commandBuffer
            Vulkan.vkQueueSubmit (queue.VkQueue, 1u, asPointer &info, finishFence) |> Hl.check

            // wait for execution to finish
            // NOTE: DJL: must ALSO be thread safe!
            Hl.awaitFence finishFence device

            // free command buffer
            Vulkan.vkFreeCommandBuffers (device, commandPool, 1u, asPointer &commandBuffer))

    /// Present swapchain image.
    static member present waitSemaphore vkSwapchain (queue : Queue) =

        // try to present image
        let result =
            lock queue.Lock (fun () ->
                let mutable waitSemaphore = waitSemaphore
                let mutable vkSwapchain = vkSwapchain
                let mutable info = VkPresentInfoKHR ()
                info.waitSemaphoreCount <- 1u
                info.pWaitSemaphores <- asPointer &waitSemaphore
                info.swapchainCount <- 1u
                info.pSwapchains <- asPointer &vkSwapchain
                info.pImageIndices <- asPointer &HlInternal.ImageIndex
                Vulkan.vkQueuePresentKHR (queue.VkQueue, asPointer &info))
    
        // return result
        result

/// A physical device and associated data.
type internal PhysicalDevice =
    { VkPhysicalDevice : VkPhysicalDevice
      Properties : VkPhysicalDeviceProperties
      Features : VkPhysicalDeviceFeatures
      Extensions : VkExtensionProperties array
      SurfaceCapabilities : VkSurfaceCapabilitiesKHR // NOTE: DJL: keep this here in case we want to use it for device selection.
      SurfaceFormats : VkSurfaceFormatKHR array
      GraphicsQueueFamily : uint
      PresentQueueFamily : uint
      GraphicsQueueCount : uint }

    /// Supports anisotropy.
    member this.SupportsAnisotropy =
        this.Features.samplerAnisotropy = VkBool32.True
    
    static member private checkSurface window instance =
        if HlInternal.hasAppBegunBackgrounding () then
            Hl.destroyVulkanSurface instance
            Hl.createVulkanSurface window instance
    
    /// Get properties.
    static member private getProperties vkPhysicalDevice =
        let mutable properties = Unchecked.defaultof<VkPhysicalDeviceProperties>
        Vulkan.vkGetPhysicalDeviceProperties (vkPhysicalDevice, &properties)
        properties

    /// Get features.
    static member private getFeatures vkPhysicalDevice =
        let mutable features = Unchecked.defaultof<VkPhysicalDeviceFeatures>
        Vulkan.vkGetPhysicalDeviceFeatures (vkPhysicalDevice, &features)
        features
    
    /// Get available extensions.
    static member private getExtensions vkPhysicalDevice =
        let mutable extensionCount = 0u
        Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, asPointer &extensionCount, nullPtr) |> Hl.check
        let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
        use extensionsPin = new ArrayPin<_> (extensions)
        Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, asPointer &extensionCount, extensionsPin.Pointer) |> Hl.check
        extensions

    /// Get available surface formats.
    static member private getSurfaceFormats vkPhysicalDevice window instance =
        PhysicalDevice.checkSurface window instance
        let mutable formatCount = 0u
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, HlInternal.Surface, asPointer &formatCount, nullPtr) |> Hl.check
        let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
        use formatsPin = new ArrayPin<_> (formats)
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, HlInternal.Surface, asPointer &formatCount, formatsPin.Pointer) |> Hl.check
        formats

    /// Get surface capabilities.
    static member private getSurfaceCapabilities vkPhysicalDevice window instance =
        PhysicalDevice.checkSurface window instance
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, HlInternal.Surface, &capabilities) |> Hl.check
        capabilities
    
    /// Attempt to get the queue families.
    static member private tryGetQueueFamilies vkPhysicalDevice window instance =

        // check surface is still valid
        PhysicalDevice.checkSurface window instance
        
        // get queue families' properties
        let mutable queueFamilyCount = 0u
        Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, asPointer &queueFamilyCount, nullPtr)
        let queueFamilyProps = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
        use queueFamilyPropsPin = new ArrayPin<_> (queueFamilyProps)
        Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, asPointer &queueFamilyCount, queueFamilyPropsPin.Pointer)

        // NOTE: DJL: it is *essential* to use the *first* compatible queue families in the array, *not* the last, as per the tutorial and vortice vulkan sample.
        // I discovered this by accident because the queue families on my AMD behaved exactly the same as the queue families on this one:
        // https://computergraphics.stackexchange.com/questions/9707/queue-from-a-family-queue-that-supports-presentation-doesnt-work-vulkan
        // general lesson: trust level for vendors is too low for deviation from common practices to be advisable.
        let mutable graphicsQueueFamilyOpt = None
        let mutable presentQueueFamilyOpt = None
        for i in 0 .. dec queueFamilyProps.Length do

            // try get graphics queue family
            // NOTE: DJL: for reason described above, do not attempt to derive transfer queue from seperate family.
            match graphicsQueueFamilyOpt with
            | None ->
                let props = queueFamilyProps.[i]
                if props.queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then
                    graphicsQueueFamilyOpt <- Some (uint i, props.queueCount)
            | Some _ -> ()

            // try get present queue family
            match presentQueueFamilyOpt with
            | None ->
                let mutable presentSupport = VkBool32.False
                Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR (vkPhysicalDevice, uint i, HlInternal.Surface, &presentSupport) |> Hl.check
                if (presentSupport = VkBool32.True) then
                    presentQueueFamilyOpt <- Some (uint i)
            | Some _ -> ()

        // fin
        (graphicsQueueFamilyOpt, presentQueueFamilyOpt)

    /// Attempt to construct PhysicalDevice.
    static member tryCreate vkPhysicalDevice window instance =
        let properties = PhysicalDevice.getProperties vkPhysicalDevice
        let features = PhysicalDevice.getFeatures vkPhysicalDevice
        let extensions = PhysicalDevice.getExtensions vkPhysicalDevice
        let surfaceFormats = PhysicalDevice.getSurfaceFormats vkPhysicalDevice window instance
        let surfaceCapabilities = PhysicalDevice.getSurfaceCapabilities vkPhysicalDevice window instance
        match PhysicalDevice.tryGetQueueFamilies vkPhysicalDevice window instance with
        | (Some (graphicsQueueFamily, graphicsQueueCount), Some presentQueueFamily) ->
            let physicalDevice =
                { VkPhysicalDevice = vkPhysicalDevice
                  Properties = properties
                  Features = features
                  Extensions = extensions
                  SurfaceCapabilities = surfaceCapabilities
                  SurfaceFormats = surfaceFormats
                  GraphicsQueueFamily = graphicsQueueFamily
                  PresentQueueFamily = presentQueueFamily
                  GraphicsQueueCount = graphicsQueueCount }
            Some physicalDevice
        | (_, _) -> None

/// A single swapchain and its assets.
type internal SwapchainInternal =
    { VkSwapchain : VkSwapchainKHR
      Images : VkImage array
      ImageViews : VkImageView array
      RenderFinishedSemaphores : VkSemaphore array
      SwapExtent : VkExtent2D }

    /// Try create the VkSwapchain.
    static member private tryCreateVkSwapchain (surfaceFormat : VkSurfaceFormatKHR) oldVkSwapchainOpt physicalDevice window device =
        match Hl.tryGetSurfaceCapabilities physicalDevice.VkPhysicalDevice with
        | Some capabilities ->
        
            // decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
            // at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
            // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain.
            let minImageCount =
                if capabilities.maxImageCount = 0u
                then capabilities.minImageCount + 1u
                else min (capabilities.minImageCount + 1u) capabilities.maxImageCount

            // get swap extent
            let swapExtent = Hl.getSwapExtent capabilities window
            
            // in case graphics and present queue families differ
            // TODO: as part of optimization, the sharing mode in this case should probably be VkSharingMode.Exclusive (see below).
            let indicesArray = [|physicalDevice.GraphicsQueueFamily; physicalDevice.PresentQueueFamily|]
            use indicesArrayPin = new ArrayPin<_> (indicesArray)

            // create swapchain
            let mutable info = VkSwapchainCreateInfoKHR ()
            info.surface <- HlInternal.Surface
            info.minImageCount <- minImageCount
            info.imageFormat <- surfaceFormat.format
            info.imageColorSpace <- surfaceFormat.colorSpace
            info.imageExtent <- swapExtent
            info.imageArrayLayers <- 1u
            info.imageUsage <- VkImageUsageFlags.ColorAttachment ||| VkImageUsageFlags.TransferDst
            if (physicalDevice.GraphicsQueueFamily = physicalDevice.PresentQueueFamily) then
                info.imageSharingMode <- VkSharingMode.Exclusive
            else
                info.imageSharingMode <- VkSharingMode.Concurrent
                info.queueFamilyIndexCount <- 2u
                info.pQueueFamilyIndices <- indicesArrayPin.Pointer
            info.preTransform <- capabilities.currentTransform
            info.compositeAlpha <- VkCompositeAlphaFlagsKHR.Opaque
            info.presentMode <- VkPresentModeKHR.Fifo // NOTE: guaranteed by the spec and seems most appropriate for Nu.
            info.clipped <- true
            info.oldSwapchain <- oldVkSwapchainOpt
            let mutable vkSwapchain = Unchecked.defaultof<VkSwapchainKHR>
            let result = Vulkan.vkCreateSwapchainKHR (device, &info, nullPtr, &vkSwapchain)
            
            if result <> VkResult.ErrorSurfaceLostKHR then
                Hl.check result
                Some (vkSwapchain, swapExtent)
            else
                HlInternal.SurfaceState <- SurfaceLost
                None

        | None -> None

    /// Get swapchain images.
    static member private getSwapchainImages vkSwapchain device =
        let mutable imageCount = 0u
        Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, asPointer &imageCount, nullPtr) |> Hl.check
        let images = Array.zeroCreate<VkImage> (int imageCount)
        use imagesPin = new ArrayPin<_> (images)
        Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, asPointer &imageCount, imagesPin.Pointer) |> Hl.check
        images

    /// Create the image views.
    static member private createImageViews format (images : VkImage array) device =
        let imageViews = Array.zeroCreate<VkImageView> images.Length
        for i in 0 .. dec imageViews.Length do imageViews.[i] <- Hl.createImageView Rgba format 0 1 0 1 VkImageViewType.Image2D VkImageAspectFlags.Color images.[i] device
        imageViews
    
    /// Create render finished semaphores.
    static member private createRenderFinishedSemaphores imageCount device =
        let semaphores = Array.zeroCreate<VkSemaphore> imageCount
        for i in 0 .. dec semaphores.Length do semaphores.[i] <- Hl.createSemaphore device
        semaphores
    
    /// Try create a SwapchainInternal.
    static member tryCreate surfaceFormat oldVkSwapchainOpt physicalDevice window device =
        
        // try create vkSwapchain and its assets
        match SwapchainInternal.tryCreateVkSwapchain surfaceFormat oldVkSwapchainOpt physicalDevice window device with
        | Some (vkSwapchain, swapExtent) ->
            let images = SwapchainInternal.getSwapchainImages vkSwapchain device
            let imageViews = SwapchainInternal.createImageViews surfaceFormat.format images device

            // render finished semaphores based on swapchain images rather than frames in flight to address
            // safety issue described in https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html.
            // these should naturally be associated with the vkSwapchain itself, especially to prevent validation
            // errors triggered by reuse of semaphores that "may still be in use" by obsolete vkSwapchains.
            let renderFinishedSemaphores = SwapchainInternal.createRenderFinishedSemaphores images.Length device

            // make SwapchainInternal
            let swapchainInternal =
                { VkSwapchain = vkSwapchain
                  Images = images
                  ImageViews = imageViews
                  RenderFinishedSemaphores = renderFinishedSemaphores
                  SwapExtent = swapExtent }

            // fin
            Some swapchainInternal
        | None -> None
    
    /// Destroy a SwapchainInternal.
    static member destroy renderQueue presentQueue swapchainInternal device =
        
        // TODO: DJL: this is not sufficient to ensure resources not still in use, that requires an extension!!
        // https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html#_vk_ext_swapchain_maintenance1_extension
        Queue.waitIdle renderQueue
        Queue.waitIdle presentQueue
        for i in 0 .. dec swapchainInternal.ImageViews.Length do Vulkan.vkDestroyImageView (device, swapchainInternal.ImageViews.[i], nullPtr)
        Vulkan.vkDestroySwapchainKHR (device, swapchainInternal.VkSwapchain, nullPtr)
        for i in 0 .. dec swapchainInternal.RenderFinishedSemaphores.Length do Vulkan.vkDestroySemaphore (device, swapchainInternal.RenderFinishedSemaphores.[i], nullPtr)

/// A swapchain and its assets that may be refreshed for a different screen size.
type internal Swapchain =
    { SwapchainInternalOpts_ : SwapchainInternal option array
      Window_ : SDL_Window nativeptr
      SurfaceFormat_ : VkSurfaceFormatKHR
      mutable SwapchainIndex_ : int }

    /// The current SwapchainInternalOpt.
    member this.SwapchainInternalOpt = this.SwapchainInternalOpts_.[this.SwapchainIndex_]
    
    /// The Vulkan swapchain itself.
    member this.VkSwapchain = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).VkSwapchain

    /// The number of swapchain images.
    member this.ImageCount = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).Images.Length
    
    /// The current swapchain image.
    member this.Image = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).Images.[int HlInternal.ImageIndex]

    /// The image view for the current swapchain image.
    member this.ImageView = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).ImageViews.[int HlInternal.ImageIndex]
    
    /// The render finished semaphore for the current swapchain image.
    member this.RenderFinishedSemaphore = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).RenderFinishedSemaphores.[int HlInternal.ImageIndex]

    /// The swap extent of the current vkSwapchain.
    member this.SwapExtent = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).SwapExtent

    static member private clear renderQueue presentQueue swapchain device =
        for i in 0 .. dec swapchain.SwapchainInternalOpts_.Length do
            match swapchain.SwapchainInternalOpts_.[i] with
            | Some swapchainInternal ->
                SwapchainInternal.destroy renderQueue presentQueue swapchainInternal device
                swapchain.SwapchainInternalOpts_.[i] <- None
            | None -> ()
    
    static member private destroySurface renderQueue presentQueue swapchain device instance =
        Swapchain.clear renderQueue presentQueue swapchain device // must do this first
        Hl.destroyVulkanSurface instance
    
    static member private tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance =
        
        // check if app is back in foreground
        if HlInternal.IsAppInForeground then
            
            // create surface
            Hl.tryCreateVulkanSurface swapchain.Window_ instance
            
            // ensure surface creation was successful
            if HlInternal.SurfaceState = SurfaceReady then
                
                // check if pause triggered during surface creation
                if not (HlInternal.hasAppBegunBackgrounding ()) then
                
                    // check window not minimized
                    if not (Swapchain.isWindowMinimized swapchain.Window_) then

                        // try create SwapchainInternal
                        let swapchainInternalOpt = SwapchainInternal.tryCreate swapchain.SurfaceFormat_ VkSwapchainKHR.Null physicalDevice swapchain.Window_ device
                        swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] <- swapchainInternalOpt
                        
                        // destroy surface if lost again or if pause triggered during swapchain creation
                        if HlInternal.SurfaceState = SurfaceLost || HlInternal.hasAppBegunBackgrounding ()
                        then Swapchain.destroySurface renderQueue presentQueue swapchain device instance

                // abort
                else Swapchain.destroySurface renderQueue presentQueue swapchain device instance
    
    /// Check if window is minimized.
    static member isWindowMinimized window =
        SDL3.SDL_GetWindowFlags window &&& SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> LanguagePrimitives.EnumOfValue 0UL
    
    /// Check if window has been resized or surface lost.
    static member isWindowResizedOrSurfaceLost vkPhysicalDevice (swapchain : Swapchain) =
        match Hl.tryGetSurfaceCapabilities vkPhysicalDevice with
        | Some capabilities -> swapchain.SwapExtent <> Hl.getSwapExtent capabilities swapchain.Window_
        | None -> true

    /// Update the swapchain.
    static member update physicalDevice renderQueue presentQueue swapchain device instance =
        
        // NOTE: DJL: by design, this method should know exactly what to do based on the current and changing state of
        // the surface and app backgrounding, anticipated or not, regardless of the calling context, which just needs 
        // to detect *if* method must be called. It should have a valid and appropriate result whatever the environment
        // throws at it.
        
        // handle surface state
        match HlInternal.SurfaceState with
        
        // attempt to recreate the swapchain, destroying the surface if suddenly lost or if app has/will enter background
        | SurfaceReady ->
        
            // check if app has or will enter background, if not then just try recreate swapchain
            if not (HlInternal.hasAppBegunBackgrounding ()) then
            
                // use current VkSwapchain to create new one
                let oldVkSwapchainOpt =
                    match swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] with
                    | Some swapchainInternal -> if swapchain.SwapchainInternalOpts_.Length > 1 then swapchainInternal.VkSwapchain else VkSwapchainKHR.Null
                    | None -> VkSwapchainKHR.Null

                // advance swapchain index
                if Option.isSome swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] then
                    swapchain.SwapchainIndex_ <- (inc swapchain.SwapchainIndex_) % swapchain.SwapchainInternalOpts_.Length

                // destroy SwapchainInternal at new index if present
                match swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] with
                | Some swapchainInternal ->
                    SwapchainInternal.destroy renderQueue presentQueue swapchainInternal device
                    swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] <- None
                | None -> ()
                
                // check once more for app pause (triggered during swapchain destruction) before attempting swapchain creation
                if not (HlInternal.hasAppBegunBackgrounding ()) then
                
                    // check window not minimized
                    if not (Swapchain.isWindowMinimized swapchain.Window_) then
                    
                        // try create new swapchain internal
                        let swapchainInternalOpt = SwapchainInternal.tryCreate swapchain.SurfaceFormat_ oldVkSwapchainOpt physicalDevice swapchain.Window_ device
                        swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] <- swapchainInternalOpt

                        // if surface is lost here (or pause triggered during pipeline creation!), destroy and attempt to recover on the spot
                        if HlInternal.SurfaceState = SurfaceLost || HlInternal.hasAppBegunBackgrounding () then
                            Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                            Swapchain.tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance

                // destroy surface and recreate if already possible
                else
                    Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                    Swapchain.tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance

            // destroy surface and recreate if already possible
            else
                Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                Swapchain.tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance

        // handle surface loss and attempt to recreate surface and swapchain immediately
        | SurfaceLost ->
            Swapchain.destroySurface renderQueue presentQueue swapchain device instance
            Swapchain.tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance

        // attempt to recreate surface and swapchain when app is in foreground
        | SurfaceDestroyed ->
            Swapchain.tryCreateSurfaceAndSwapchainInternal physicalDevice renderQueue presentQueue swapchain device instance
    
    /// Create a Swapchain.
    static member create surfaceFormat physicalDevice window device =
        
        // swapchain index starts at zero
        let swapchainIndex = 0
        
        // create SwapchainInternal array
        // NOTE: DJL: must allow for frames in flight plus 1 to prevent destroying semaphores while still in use
        // because swapchain can be refreshed at the end of one frame AND at the beginning of the next,
        // but can still only be refreshed once per frame.
        let swapchainInternalOpts = Array.create (Constants.Vulkan.MaxFramesInFlight + 1) None
        
        // check if window is minimized at startup
        let windowMinimized = Swapchain.isWindowMinimized window
        
        // try create first SwapchainInternal if window is not minimized or app paused
        if not (windowMinimized || HlInternal.hasAppBegunBackgrounding ()) then
            let swapchainInternalOpt = SwapchainInternal.tryCreate surfaceFormat VkSwapchainKHR.Null physicalDevice window device
            swapchainInternalOpts.[swapchainIndex] <- swapchainInternalOpt

        // make Swapchain
        let swapchain =
            { SwapchainInternalOpts_ = swapchainInternalOpts
              Window_ = window
              SurfaceFormat_ = surfaceFormat
              SwapchainIndex_ = swapchainIndex }

        // fin
        (swapchain, windowMinimized)
    
    /// Destroy a Swapchain.
    static member destroy swapchain device =
        Swapchain.clear swapchain device

// TODO: DJL: for mobile devices: https://learn.microsoft.com/en-us/dotnet/standard/native-interop/calling-conventions#when-you-can-omit-the-calling-convention.
[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type internal DebugDelegate =
    delegate of VkDebugUtilsMessageSeverityFlagsEXT * VkDebugUtilsMessageTypeFlagsEXT * nativeint * nativeint -> uint32

// https://github.com/amerkoleci/Vortice.Vulkan/blob/32035603790b64f4c96a979193a7e1391d34a428/src/Vortice.Vulkan/Generated/Structures.cs#L14978
// VkDebugUtilsMessengerCreateInfoEXT with pfnUserCallback as "real" nativeint instead of "fake" nativeint which is actually a function pointer type
// TODO: report this F# compiler bug that allows assigning to "fake" nativeint to compile without error but causes a crash at runtime
type [<Struct>] private VkDebugUtilsMessengerCreateInfoEXT_hack =
    val mutable sType : VkStructureType
    val mutable pNext : nativeint
    val mutable flags : VkDebugUtilsMessengerCreateFlagsEXT
    val mutable messageSeverity : VkDebugUtilsMessageSeverityFlagsEXT
    val mutable messageType : VkDebugUtilsMessageTypeFlagsEXT
    val mutable pfnUserCallback : nativeint // "real" nativeint
    val mutable pUserData : nativeint