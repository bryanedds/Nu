// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Collections.Generic
open System.IO
open System.Numerics
open FSharp.NativeInterop
open SDL2
open Vortice.ShaderCompiler
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    let mutable private DrawReportLock = obj ()
    let mutable private DrawCallCount = 0
    let mutable private DrawInstanceCount = 0

    let private ValidationLayersEnabled =
#if DEBUG
        true
#else
        false
#endif

    let mutable private ValidationLayersActivated = false
    
    /// Index of the current Swapchain image.
    let mutable private ImageIndex = 0u

    /// The current frame within MaxFramesInFlight.
    /// TODO: DJL: figure out how to prevent potential outside mutation.
    let mutable internal CurrentFrame = 0

    /// The format of an image.
    type ImageFormat =
        | Rgba8
        | Rgba16f
        | Rgb16f
        | Rg32f
        | R16f
        | Bc3
        | Bc5
        | D32f

        /// The VkFormat.
        member this.VkFormat =
            match this with
            | Rgba8 -> VkFormat.R8G8B8A8Unorm
            | Rgba16f -> VkFormat.R16G16B16A16Sfloat
            | Rgb16f -> VkFormat.R16G16B16Sfloat
            | Rg32f -> VkFormat.R32G32Sfloat
            | R16f -> VkFormat.R16Sfloat
            | Bc3 -> VkFormat.Bc3UnormBlock
            | Bc5 -> VkFormat.Bc5UnormBlock
            | D32f -> VkFormat.D32Sfloat

        /// The VkImageAspectFlags.
        member this.VkImageAspectFlags =
            match this with
            | Rgba8 -> VkImageAspectFlags.Color
            | Rgba16f -> VkImageAspectFlags.Color
            | Rgb16f -> VkImageAspectFlags.Color
            | Rg32f -> VkImageAspectFlags.Color
            | R16f -> VkImageAspectFlags.Color
            | Bc3 -> VkImageAspectFlags.Color
            | Bc5 -> VkImageAspectFlags.Color
            | D32f -> VkImageAspectFlags.Depth
        
        /// Get the size in bytes of an image with given width, height and format.
        static member getImageSize width height imageFormat =
            match imageFormat with
            | Rgba8 -> width * height * 4
            | Rgba16f -> width * height * 8
            | Rgb16f -> width * height * 6
            | Rg32f -> width * height * 8
            | R16f -> width * height * 2
            | Bc3
            | Bc5 ->
                let x = if width % 4 = 0 then width else (width / 4 + 1) * 4
                let y = if height % 4 = 0 then height else (height / 4 + 1) * 4
                x * y
            | D32f -> width * height * 4
    
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
        | UndefinedHost
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
            | UndefinedHost -> VkImageLayout.Undefined
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
            | UndefinedHost -> VkAccessFlags.None
            | TransferSrc -> VkAccessFlags.TransferRead
            | TransferDst -> VkAccessFlags.TransferWrite
            | ShaderRead -> VkAccessFlags.ShaderRead
            | ColorAttachmentWrite -> VkAccessFlags.ColorAttachmentWrite
            | DepthAttachment -> VkAccessFlags.DepthStencilAttachmentRead ||| VkAccessFlags.DepthStencilAttachmentWrite
            | Present -> VkAccessFlags.None

        /// The pipeline stage.
        member this.PipelineStage =
            match this with
            | Undefined -> VkPipelineStageFlags.TopOfPipe
            | UndefinedHost -> VkPipelineStageFlags.Host
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
        | UniformBuffer
        | CombinedImageSampler

        /// The VkDescriptorType.
        member this.VkDescriptorType =
            match this with
            | UniformBuffer -> VkDescriptorType.UniformBuffer
            | CombinedImageSampler -> VkDescriptorType.CombinedImageSampler
    
    /// Convert VkExtensionProperties.extensionName to a string.
    /// TODO: see if we can inline functions like these once F# supports C#'s representation of this fixed buffer type.
    let private getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName

    /// Convert VkLayerProperties.layerName to a string.
    let private getLayerName (layerProps : VkLayerProperties) =
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
    let inline makeRenderingInfo colorAttachment depthAttachmentOpt renderArea clearValueOpt =
        
        // NOTE: DJL: must be inline to keep pointers valid.
        
        // color attachment info
        let mutable cInfo = VkRenderingAttachmentInfo ()
        cInfo.imageView <- colorAttachment
        cInfo.imageLayout <- ColorAttachmentWrite.VkImageLayout
        cInfo.storeOp <- VkAttachmentStoreOp.Store
        match clearValueOpt with
        | Some clearValue ->
            cInfo.loadOp <- VkAttachmentLoadOp.Clear
            cInfo.clearValue <- clearValue
        | None ->
            cInfo.loadOp <- VkAttachmentLoadOp.Load

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
        rInfo.colorAttachmentCount <- 1u
        rInfo.pColorAttachments <- asPointer &cInfo
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

    let private sdlGetInstanceExtensionsFail = "SDL_Vulkan_GetInstanceExtensions failed."
    let private sdlCreateSurfaceFail = "SDL_Vulkan_CreateSurface failed."
    
    let private checkSdl errMsg result =
        if int result = 0 then Log.error ("SDL error, " + errMsg)

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

    /// Compile GLSL file to SPIR-V code.
    let compileShader shaderPath shaderKind =
        use shaderStream = new StreamReader (File.OpenRead shaderPath)
        let shaderStr = shaderStream.ReadToEnd ()
        use compiler = new Compiler ()
        let options = CompilerOptions ()
        options.ShaderStage <- shaderKind
        let result = compiler.Compile (shaderStr, shaderPath, options)
        if result.Status <> CompilationStatus.Success then
            Log.fail ("Vulkan shader compilation failed due to:\n" + result.ErrorMessage)
        let shaderCode = result.Bytecode
        shaderCode

    /// Create a shader module from a GLSL file.
    let createShaderModuleFromGlsl shaderPath shaderKind device =
        let shader = compileShader shaderPath shaderKind
        let mutable shaderModule = Unchecked.defaultof<VkShaderModule>

        // NOTE: DJL: using a high level overload here to avoid questions about reinterpret casting and memory alignment,
        // see https://vulkan-tutorial.com/Drawing_a_triangle/Graphics_pipeline_basics/Shader_modules#page_Creating-shader-modules.
        Vulkan.vkCreateShaderModule (device, shader, nullPtr, &shaderModule) |> check
        shaderModule

    /// Record command to transition image layout.
    let recordTransitionLayout cb allLevels mipNumber layer imageAspect (oldLayout : ImageLayout) (newLayout : ImageLayout) vkImage =
        
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
        barrier.subresourceRange <- makeSubresourceRange mipLevel mipLevels layer 1 imageAspect
        Vulkan.vkCmdPipelineBarrier
            (cb,
             oldLayout.PipelineStage,
             newLayout.PipelineStage,
             VkDependencyFlags.None,
             0u, nullPtr, 0u, nullPtr,
             1u, asPointer &barrier)
    
    /// Get surface capabilities.
    let private getSurfaceCapabilities vkPhysicalDevice surface =
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, surface, &capabilities) |> check
        capabilities
    
    /// Create an image view.
    let createImageView pixelFormat format mips layers viewType imageAspect image device =
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- viewType
        info.format <- format
        info.components <- makeComponentMapping pixelFormat
        info.subresourceRange <- makeSubresourceRange 0 mips 0 layers imageAspect
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

    /// Init recording to a persistent command buffer.
    let initCommandBuffer cb =
        Vulkan.vkResetCommandBuffer (cb, VkCommandBufferResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo ()
        Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check
    
    /// Init recording to a transient command buffer.
    /// TODO: DJL: review choice of transient command buffers over normal ones.
    let initCommandBufferTransient commandPool device =
        
        // create command buffer
        let cb = allocateCommandBuffer commandPool device

        // reset command buffer and begin recording
        Vulkan.vkResetCommandPool (device, commandPool, VkCommandPoolResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo (flags = VkCommandBufferUsageFlags.OneTimeSubmit)
        Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check

        // return command buffer
        cb
    
    /// A physical device and associated data.
    type private PhysicalDevice =
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
        member this.SupportsAnisotropy = this.Features.samplerAnisotropy = VkBool32.True
        
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
            Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, asPointer &extensionCount, nullPtr) |> check
            let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
            use extensionsPin = new ArrayPin<_> (extensions)
            Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, asPointer &extensionCount, extensionsPin.Pointer) |> check
            extensions

        /// Get available surface formats.
        static member private getSurfaceFormats vkPhysicalDevice surface =
            let mutable formatCount = 0u
            Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, surface, asPointer &formatCount, nullPtr) |> check
            let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
            use formatsPin = new ArrayPin<_> (formats)
            Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, surface, asPointer &formatCount, formatsPin.Pointer) |> check
            formats

        /// Attempt to get the queue families.
        static member private tryGetQueueFamilies vkPhysicalDevice surface =

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
                    Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR (vkPhysicalDevice, uint i, surface, &presentSupport) |> check
                    if (presentSupport = VkBool32.True) then
                        presentQueueFamilyOpt <- Some (uint i)
                | Some _ -> ()

            // fin
            (graphicsQueueFamilyOpt, presentQueueFamilyOpt)

        /// Attempt to construct PhysicalDevice.
        static member tryCreate vkPhysicalDevice surface =
            let properties = PhysicalDevice.getProperties vkPhysicalDevice
            let features = PhysicalDevice.getFeatures vkPhysicalDevice
            let extensions = PhysicalDevice.getExtensions vkPhysicalDevice
            let surfaceCapabilities = getSurfaceCapabilities vkPhysicalDevice surface
            let surfaceFormats = PhysicalDevice.getSurfaceFormats vkPhysicalDevice surface
            match PhysicalDevice.tryGetQueueFamilies vkPhysicalDevice surface with
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
    type private SwapchainInternal =
        { VkSwapchain : VkSwapchainKHR
          Images : VkImage array
          ImageViews : VkImageView array }

        /// Create the Vulkan swapchain itself.
        static member private createVkSwapchain (surfaceFormat : VkSurfaceFormatKHR) swapExtent oldVkSwapchainOpt physicalDevice surface device =

            // decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
            // at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
            // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain.
            let capabilities = getSurfaceCapabilities physicalDevice.VkPhysicalDevice surface
            let minImageCount =
                if capabilities.maxImageCount = 0u
                then capabilities.minImageCount + 1u
                else min (capabilities.minImageCount + 1u) capabilities.maxImageCount

            // in case graphics and present queue families differ
            // TODO: as part of optimization, the sharing mode in this case should probably be VkSharingMode.Exclusive (see below).
            let indicesArray = [|physicalDevice.GraphicsQueueFamily; physicalDevice.PresentQueueFamily|]
            use indicesArrayPin = new ArrayPin<_> (indicesArray)

            // create swapchain
            let mutable info = VkSwapchainCreateInfoKHR ()
            info.surface <- surface
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
            Vulkan.vkCreateSwapchainKHR (device, &info, nullPtr, &vkSwapchain) |> check
            vkSwapchain

        /// Get swapchain images.
        static member private getSwapchainImages vkSwapchain device =
            let mutable imageCount = 0u
            Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, asPointer &imageCount, nullPtr) |> check
            let images = Array.zeroCreate<VkImage> (int imageCount)
            use imagesPin = new ArrayPin<_> (images)
            Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, asPointer &imageCount, imagesPin.Pointer) |> check
            images

        /// Create the image views.
        static member private createImageViews format (images : VkImage array) device =
            let imageViews = Array.zeroCreate<VkImageView> images.Length
            for i in 0 .. dec imageViews.Length do imageViews.[i] <- createImageView Rgba format 1 1 VkImageViewType.Image2D VkImageAspectFlags.Color images.[i] device
            imageViews
        
        /// Create a SwapchainInternal.
        static member create surfaceFormat swapExtent oldVkSwapchainOpt physicalDevice surface device =
            
            // create Vulkan swapchain and its assets
            let vkSwapchain = SwapchainInternal.createVkSwapchain surfaceFormat swapExtent oldVkSwapchainOpt physicalDevice surface device
            let images = SwapchainInternal.getSwapchainImages vkSwapchain device
            let imageViews = SwapchainInternal.createImageViews surfaceFormat.format images device

            // make SwapchainInternal
            let swapchainInternal =
                { VkSwapchain = vkSwapchain
                  Images = images
                  ImageViews = imageViews }

            // fin
            swapchainInternal
        
        /// Destroy a SwapchainInternal.
        static member destroy swapchainInternal device =
            for i in 0 .. dec swapchainInternal.ImageViews.Length do Vulkan.vkDestroyImageView (device, swapchainInternal.ImageViews.[i], nullPtr)
            Vulkan.vkDestroySwapchainKHR (device, swapchainInternal.VkSwapchain, nullPtr)

    /// A swapchain and its assets that may be refreshed for a different screen size.
    type private Swapchain =
        { SwapchainInternalOpts_ : SwapchainInternal option array
          Window_ : nativeint
          SurfaceFormat_ : VkSurfaceFormatKHR
          mutable SwapExtent_ : VkExtent2D
          mutable SwapchainIndex_ : int }

        /// The Vulkan swapchain itself.
        member this.VkSwapchain = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).VkSwapchain

        /// The number of swapchain images.
        member this.ImageCount = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).Images.Length
        
        /// The current swapchain image.
        member this.Image = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).Images.[int ImageIndex]

        /// The image view for the current swapchain image.
        member this.ImageView = (Option.get this.SwapchainInternalOpts_.[this.SwapchainIndex_]).ImageViews.[int ImageIndex]
        
        /// Get swap extent.
        static member private getSwapExtent vkPhysicalDevice surface window =

            // get surface capabilities
            let capabilities = getSurfaceCapabilities vkPhysicalDevice surface
            
            // check if window size is fixed or variable
            if capabilities.currentExtent.width <> UInt32.MaxValue
            then capabilities.currentExtent
            else

                // get pixel resolution from sdl
                // NOTE: DJL: unlike the GLFW counterpart, this does NOT return 0 when minimized.
                let mutable width = Unchecked.defaultof<int>
                let mutable height = Unchecked.defaultof<int>
                SDL.SDL_Vulkan_GetDrawableSize (window, &width, &height)

                // clamp resolution to size limits
                width <- max width (int capabilities.minImageExtent.width)
                width <- min width (int capabilities.maxImageExtent.width)
                height <- max height (int capabilities.minImageExtent.height)
                height <- min height (int capabilities.maxImageExtent.height)

                // fin
                VkExtent2D (width, height)
        
        /// Update the swap extent.
        static member updateSwapExtent vkPhysicalDevice surface swapchain =
            swapchain.SwapExtent_ <- Swapchain.getSwapExtent vkPhysicalDevice surface swapchain.Window_
        
        /// Check if window is minimized.
        static member isWindowMinimized swapchain =
            let flags = SDL.SDL_GetWindowFlags swapchain.Window_
            flags &&& Branchless.reinterpret SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED <> 0u
        
        /// Refresh the swapchain for a new swap extent.
        static member refresh physicalDevice surface swapchain device =
            
            // don't pass the old vulkan swapchain if only 1 frame in flight as it will get destroyed immediately
            let oldVkSwapchainOpt = if swapchain.SwapchainInternalOpts_.Length > 1 then swapchain.VkSwapchain else VkSwapchainKHR.Null

            // advance swapchain index
            swapchain.SwapchainIndex_ <- (inc swapchain.SwapchainIndex_) % swapchain.SwapchainInternalOpts_.Length

            // destroy SwapchainInternal at new index if present
            match swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] with
            | Some swapchainInternal -> SwapchainInternal.destroy swapchainInternal device
            | None -> ()
            
            // update swap extent
            Swapchain.updateSwapExtent physicalDevice.VkPhysicalDevice surface swapchain
            
            // create new swapchain internal
            let swapchainInternal = SwapchainInternal.create swapchain.SurfaceFormat_ swapchain.SwapExtent_ oldVkSwapchainOpt physicalDevice surface device
            swapchain.SwapchainInternalOpts_.[swapchain.SwapchainIndex_] <- Some swapchainInternal
        
        /// Create a Swapchain.
        static member create surfaceFormat physicalDevice surface window device =
            
            // init swapchain index
            let swapchainIndex = 0
            
            // create SwapchainInternal array
            let swapchainInternalOpts = Array.create Constants.Vulkan.MaxFramesInFlight None
            
            // get swap extent
            let swapExtent = Swapchain.getSwapExtent physicalDevice.VkPhysicalDevice surface window
            
            // create first SwapchainInternal
            let swapchainInternal = SwapchainInternal.create surfaceFormat swapExtent VkSwapchainKHR.Null physicalDevice surface device
            swapchainInternalOpts.[swapchainIndex] <- Some swapchainInternal

            // make Swapchain
            let swapchain =
                { SwapchainInternalOpts_ = swapchainInternalOpts
                  Window_ = window
                  SurfaceFormat_ = surfaceFormat
                  SwapExtent_ = swapExtent
                  SwapchainIndex_ = swapchainIndex }

            // fin
            swapchain
        
        /// Destroy a Swapchain.
        static member destroy swapchain device =
            for i in 0 .. dec swapchain.SwapchainInternalOpts_.Length do
                match swapchain.SwapchainInternalOpts_.[i] with
                | Some swapchainInternal -> SwapchainInternal.destroy swapchainInternal device
                | None -> ()
    
    /// A command queue that internally synchronizes use across multiple threads.
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

        /// Submit persistent command buffer for execution.
        static member submit cb waitSemaphoresStages (signalSemaphores : VkSemaphore array) signalFence (queue : Queue) =
            lock queue.Lock (fun () ->

                // end command buffer
                Vulkan.vkEndCommandBuffer cb |> check
                
                // unpack and pin arrays
                let (waitSemaphores, waitStages) = Array.unzip waitSemaphoresStages
                use waitSemaphoresPin = new ArrayPin<_> (waitSemaphores)
                use waitStagesPin = new ArrayPin<_> (waitStages)
                use signalSemaphoresPin = new ArrayPin<_> (signalSemaphores)

                // submit commands
                let mutable cb = cb
                let mutable info = VkSubmitInfo ()
                info.waitSemaphoreCount <- uint waitSemaphores.Length
                info.pWaitSemaphores <- waitSemaphoresPin.Pointer
                info.pWaitDstStageMask <- waitStagesPin.Pointer
                info.commandBufferCount <- 1u
                info.pCommandBuffers <- asPointer &cb
                info.signalSemaphoreCount <- uint signalSemaphores.Length
                info.pSignalSemaphores <- signalSemaphoresPin.Pointer
                Vulkan.vkQueueSubmit (queue.VkQueue, 1u, asPointer &info, signalFence) |> check)

        /// Execute and free transient command buffer. Command pool and fence must NOT be shared between threads!
        static member executeTransient cb commandPool finishFence (queue : Queue) device =
            let mutable cb = cb
            lock queue.Lock (fun () ->
                
                // end command buffer
                Vulkan.vkEndCommandBuffer cb |> check

                // submit commands
                let mutable sInfo = VkSubmitInfo ()
                sInfo.commandBufferCount <- 1u
                sInfo.pCommandBuffers <- asPointer &cb
                Vulkan.vkQueueSubmit (queue.VkQueue, 1u, asPointer &sInfo, finishFence) |> check)

            // wait for execution to finish
            awaitFence finishFence device

            // free command buffer
            Vulkan.vkFreeCommandBuffers (device, commandPool, 1u, asPointer &cb)

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
                    info.pImageIndices <- asPointer &ImageIndex
                    Vulkan.vkQueuePresentKHR (queue.VkQueue, asPointer &info))
            
            // return result
            result
    
    [<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
    type private DebugDelegate =
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
    
    /// Exposes the vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanContext =
        private
            { mutable WindowSizeOpt_ : Vector2i option
              mutable WindowMinimized_ : bool
              mutable RenderDesired_ : bool
              Instance_ : VkInstance
              DebugMessengerOpt_ : VkDebugUtilsMessengerEXT option
              Surface_ : VkSurfaceKHR
              PhysicalDevice_ : PhysicalDevice
              Device_ : VkDevice
              VmaAllocator_ : VmaAllocator
              Swapchain_ : Swapchain
              RenderCommandPool_ : VkCommandPool
              TransientCommandPool_ : VkCommandPool
              TextureCommandPool_ : VkCommandPool
              RenderCommandBuffers_ : VkCommandBuffer array
              RenderQueue_ : Queue
              PresentQueue_ : Queue
              TextureQueue_ : Queue
              ImageAvailableSemaphores_ : VkSemaphore array
              RenderFinishedSemaphores_ : VkSemaphore array
              InFlightFences_ : VkFence array
              TransientFence_ : VkFence
              TextureFence_ : VkFence }

        /// Render desired.
        member this.RenderDesired = this.RenderDesired_
        
        /// The physical device.
        member this.PhysicalDevice = this.PhysicalDevice_.VkPhysicalDevice

        /// Anisotropy supported.
        member this.AnisotropySupported = this.PhysicalDevice_.SupportsAnisotropy

        /// Maximum anisotropy.
        member this.MaxAnisotropy = this.PhysicalDevice_.Properties.limits.maxSamplerAnisotropy
        
        /// The logical device.
        member this.Device = this.Device_

        /// The VMA allocator.
        member this.VmaAllocator = this.VmaAllocator_

        /// The command pool for transient command buffers.
        member this.TransientCommandPool = this.TransientCommandPool_

        /// The command pool for texture command buffers.
        member this.TextureCommandPool = this.TextureCommandPool_
        
        /// The render command buffer for the current frame.
        member this.RenderCommandBuffer = this.RenderCommandBuffers_.[CurrentFrame]

        /// The render command queue.
        member this.RenderQueue = this.RenderQueue_

        /// The texture command queue.
        member this.TextureQueue = this.TextureQueue_

        /// The image available semaphore for the current frame.
        member this.ImageAvailableSemaphore = this.ImageAvailableSemaphores_.[CurrentFrame]

        /// The render finished semaphore for the current frame.
        member this.RenderFinishedSemaphore = this.RenderFinishedSemaphores_.[int ImageIndex]

        /// The in flight fence for the current frame.
        member this.InFlightFence = this.InFlightFences_.[CurrentFrame]

        /// The texture fence.
        member this.TextureFence = this.TextureFence_
        
        /// The transient fence.
        member this.TransientFence = this.TransientFence_

        /// The current swapchain image.
        member this.SwapchainImage = this.Swapchain_.Image
        
        /// The current swapchain image view.
        member this.SwapchainImageView = this.Swapchain_.ImageView
        
        /// The swap format.
        member this.SwapFormat = this.Swapchain_.SurfaceFormat_.format

        static let mutable debugDelegate : DebugDelegate = null
        
        static member private debugCallback
            (messageSeverity : VkDebugUtilsMessageSeverityFlagsEXT)
            (messageType : VkDebugUtilsMessageTypeFlagsEXT)
            (pCallbackData : nativeint)
            (pUserData : nativeint) : uint32 =

            // get callback data
            let callbackData = NativePtr.ofNativeInt<VkDebugUtilsMessengerCallbackDataEXT> pCallbackData |> NativePtr.read
            let message = NativePtr.unmanagedToString callbackData.pMessage

            // construct log header
            let typeLabel =
                match messageType with
                | VkDebugUtilsMessageTypeFlagsEXT.General -> "General"
                | VkDebugUtilsMessageTypeFlagsEXT.Validation -> "Validation"
                | VkDebugUtilsMessageTypeFlagsEXT.Performance -> "Performance"
                | _ -> ""
            let severityLabel =
                match messageSeverity with
                | VkDebugUtilsMessageSeverityFlagsEXT.Verbose -> "Verbose"
                | VkDebugUtilsMessageSeverityFlagsEXT.Info -> "Info"
                | VkDebugUtilsMessageSeverityFlagsEXT.Warning -> "Warning"
                | VkDebugUtilsMessageSeverityFlagsEXT.Error -> "Error"
                | _ -> ""
            let header = "Vulkan" + typeLabel + severityLabel
            
            // decide when to log
            if not (messageType = VkDebugUtilsMessageTypeFlagsEXT.General && messageSeverity <= VkDebugUtilsMessageSeverityFlagsEXT.Info) then Log.custom header message
            
            // decide when to fail
            if messageSeverity = VkDebugUtilsMessageSeverityFlagsEXT.Error then failwith "Vulkan error, see Log."
            
            // finish passively
            ignore pUserData
            0u
        
        static member private makeDebugMessengerInfo () =
            debugDelegate <- DebugDelegate VulkanContext.debugCallback
            let mutable info = VkDebugUtilsMessengerCreateInfoEXT_hack ()
            info.sType <- VkStructureType.DebugUtilsMessengerCreateInfoEXT
            info.messageSeverity <-
                VkDebugUtilsMessageSeverityFlagsEXT.Verbose |||
                VkDebugUtilsMessageSeverityFlagsEXT.Info |||
                VkDebugUtilsMessageSeverityFlagsEXT.Warning |||
                VkDebugUtilsMessageSeverityFlagsEXT.Error
            info.messageType <-
                VkDebugUtilsMessageTypeFlagsEXT.General |||
                VkDebugUtilsMessageTypeFlagsEXT.Validation |||
                VkDebugUtilsMessageTypeFlagsEXT.Performance
            info.pfnUserCallback <- Marshal.GetFunctionPointerForDelegate<DebugDelegate> debugDelegate // assign to "real" nativeint in the "fake" struct
            info.pUserData <- 0n
            Branchless.reinterpret info : VkDebugUtilsMessengerCreateInfoEXT // reinterpret as the "real" struct
        
        /// Create the Vulkan instance.
        static member private createVulkanInstance debugInfo window =

            // get available instance layers
            let mutable layerCount = 0u
            Vulkan.vkEnumerateInstanceLayerProperties (asPointer &layerCount, nullPtr) |> check
            let layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
            use layersPin = new ArrayPin<_> (layers)
            Vulkan.vkEnumerateInstanceLayerProperties (asPointer &layerCount, layersPin.Pointer) |> check

            // check if validation layer exists
            let validationLayer = "VK_LAYER_KHRONOS_validation"
            let validationLayerExists = Array.exists (fun x -> getLayerName x = validationLayer) layers
            if ValidationLayersEnabled && not validationLayerExists then Log.info (validationLayer + " is not available. Vulkan programmers must install the Vulkan SDK to enable validation.")
            ValidationLayersActivated <- ValidationLayersEnabled && validationLayerExists
            use layerWrap = new StringArrayWrap ([|validationLayer|]) // must remain in scope until vkCreateInstance

            // get sdl extensions
            let mutable sdlExtensionCount = 0u
            SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, null) |> checkSdl sdlGetInstanceExtensionsFail
            let sdlExtensionsOut = Array.zeroCreate<nativeint> (int sdlExtensionCount)
            SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, sdlExtensionsOut) |> checkSdl sdlGetInstanceExtensionsFail
            let sdlExtensions = Array.zeroCreate<nativeptr<byte>> (int sdlExtensionCount)
            for i in 0 .. dec (int sdlExtensionCount) do sdlExtensions.[i] <- NativePtr.nativeintToBytePtr sdlExtensionsOut.[i]

            // choose extensions
            use debugUtilsWrap = new StringWrap (Vulkan.VK_EXT_DEBUG_UTILS_EXTENSION_NAME)
            let debugUtilsArray = if ValidationLayersActivated then [|debugUtilsWrap.Pointer|] else [||]
            let extensions = Array.append sdlExtensions debugUtilsArray
            use extensionsPin = new ArrayPin<_> (extensions)
            
            // TODO: P1: DJL: complete VkApplicationInfo before merging to master
            // and check for available vulkan version (for the instance, NOT the physical device) as described in 
            // https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap4.html#VkApplicationInfo.
            // does the wrapper even cover NULL vkGetInstanceProcAddr for vkEnumerateInstanceVersion?
            let mutable aInfo = VkApplicationInfo ()

            // this is the *maximum* Vulkan version
            aInfo.apiVersion <- VkVersion.Version_1_3

            // create instance
            let mutable info = VkInstanceCreateInfo ()
            info.pApplicationInfo <- asPointer &aInfo
            info.enabledExtensionCount <- uint extensions.Length
            info.ppEnabledExtensionNames <- extensionsPin.Pointer
            if ValidationLayersActivated then
                let mutable debugInfo = debugInfo
                info.pNext <- asVoidPtr &debugInfo
                info.enabledLayerCount <- 1u
                info.ppEnabledLayerNames <- layerWrap.Pointer
            let mutable instance = Unchecked.defaultof<VkInstance>
            Vulkan.vkCreateInstance (&info, nullPtr, &instance) |> check
            instance

        static member private tryCreateDebugMessenger info instance =
            if ValidationLayersActivated then
                let mutable debugMessenger = Unchecked.defaultof<VkDebugUtilsMessengerEXT>
                Vulkan.vkCreateDebugUtilsMessengerEXT (instance, &info, nullPtr, &debugMessenger) |> check
                Some debugMessenger
            else None
        
        /// Create vulkan surface.
        static member private createVulkanSurface window instance =
            let mutable surface = Unchecked.defaultof<VkSurfaceKHR>
            SDL.SDL_Vulkan_CreateSurface (window, instance, &(NativePtr.reinterpretRef<VkSurfaceKHR, uint64> &surface)) |> checkSdl sdlCreateSurfaceFail
            surface

        /// Select compatible physical device if available.
        static member private trySelectPhysicalDevice surface instance =

            // get available physical devices
            let mutable deviceCount = 0u
            Vulkan.vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, nullPtr) |> check
            let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
            use devicesPin = new ArrayPin<_> (devices)
            Vulkan.vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, devicesPin.Pointer) |> check

            // gather devices together with relevant data for selection
            let candidates =
                [for i in 0 .. dec devices.Length do
                    match PhysicalDevice.tryCreate devices.[i] surface with
                    | Some physicalDevice -> physicalDevice
                    | None -> ()]

            // compatibility criteria: device must support essential rendering components, texture compression and at least Vulkan 1.3
            let isCompatible physicalDevice =
                let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
                let swapchainSupported = Array.exists (fun ext -> getExtensionName ext = swapchainExtensionName) physicalDevice.Extensions
                swapchainSupported &&
                physicalDevice.SurfaceFormats.Length > 0 &&
                physicalDevice.Properties.apiVersion >= VkVersion.Version_1_3 &&
                physicalDevice.Features.textureCompressionBC

            // preferability criteria: device ought to be discrete
            let isPreferable physicalDevice =
                physicalDevice.Properties.deviceType = VkPhysicalDeviceType.DiscreteGpu

            // filter and order candidates according to criteria
            let candidatesFiltered = List.filter isCompatible candidates
            let (fstChoice, sndChoice) = List.partition isPreferable candidatesFiltered
            let candidatesFilteredAndOrdered = List.append fstChoice sndChoice
                
            // if compatible devices exist then return the first along with its data
            let physicalDeviceOpt =
                if candidatesFilteredAndOrdered.Length > 0 then
                    
                    // select physical device
                    let physicalDevice = List.head candidatesFilteredAndOrdered
                    
                    // log any important data about physical device
                    // TODO: DJL: log device name!
                    if not physicalDevice.SupportsAnisotropy then Log.info "Graphics device does not support anisotropy."
                    
                    // return physical device
                    Some physicalDevice
                
                // no physical device
                else Log.info "Could not find a suitable graphics device for Vulkan."; None

            // fin
            physicalDeviceOpt
        
        /// Create the logical device.
        static member private createLogicalDevice (physicalDevice : PhysicalDevice) =

            // Vulkan 1.3 features
            let mutable vulkan13 = VkPhysicalDeviceVulkan13Features (dynamicRendering = true)
            
            // descriptor indexing features
            let mutable descriptorIndexing = VkPhysicalDeviceDescriptorIndexingFeatures ()
            descriptorIndexing.pNext <- asVoidPtr &vulkan13
            descriptorIndexing.descriptorBindingUniformBufferUpdateAfterBind <- true
            descriptorIndexing.descriptorBindingSampledImageUpdateAfterBind <- true
            descriptorIndexing.descriptorBindingUpdateUnusedWhilePending <- true
            descriptorIndexing.descriptorBindingPartiallyBound <- true
            descriptorIndexing.runtimeDescriptorArray <- true
            
            // queue create infos
            let mutable queuePriority = 1.0f
            let queueCreateInfosList = List ()
            let mutable qInfo = VkDeviceQueueCreateInfo ()
            qInfo.queueFamilyIndex <- physicalDevice.GraphicsQueueFamily
            qInfo.queueCount <- min 2u physicalDevice.GraphicsQueueCount
            qInfo.pQueuePriorities <- asPointer &queuePriority
            queueCreateInfosList.Add qInfo
            if physicalDevice.GraphicsQueueFamily <> physicalDevice.PresentQueueFamily then
                let mutable qInfo = VkDeviceQueueCreateInfo ()
                qInfo.queueFamilyIndex <- physicalDevice.PresentQueueFamily
                qInfo.queueCount <- 1u
                qInfo.pQueuePriorities <- asPointer &queuePriority
                queueCreateInfosList.Add qInfo
            let queueCreateInfos = queueCreateInfosList.ToArray ()
            use queueCreateInfosPin = new ArrayPin<_> (queueCreateInfos)

            // get swapchain extension
            let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
            use extensionArrayWrap = new StringArrayWrap ([|swapchainExtensionName|])

            // NOTE: DJL: for particularly dated implementations of Vulkan, validation depends on device layers which
            // are deprecated. These must be enabled if validation support for said implementations is desired.

            // specify device features to be enabled
            let mutable features = VkPhysicalDeviceFeatures ()
            if physicalDevice.SupportsAnisotropy then features.samplerAnisotropy <- true
            
            // create device
            let mutable info = VkDeviceCreateInfo ()
            info.pNext <- asVoidPtr &descriptorIndexing
            info.queueCreateInfoCount <- uint queueCreateInfos.Length
            info.pQueueCreateInfos <- queueCreateInfosPin.Pointer
            info.enabledExtensionCount <- 1u
            info.ppEnabledExtensionNames <- extensionArrayWrap.Pointer
            info.pEnabledFeatures <- asPointer &features
            let mutable device = Unchecked.defaultof<VkDevice>
            Vulkan.vkCreateDevice (physicalDevice.VkPhysicalDevice, &info, nullPtr, &device) |> check
            device

        /// Create the VMA allocator.
        static member private createVmaAllocator (physicalDevice : PhysicalDevice) device instance =
            let mutable info = VmaAllocatorCreateInfo ()
            info.physicalDevice <- physicalDevice.VkPhysicalDevice
            info.device <- device
            info.instance <- instance
            let mutable allocator = Unchecked.defaultof<VmaAllocator>
            Vma.vmaCreateAllocator (&info, &allocator) |> check
            allocator

        /// Get surface format.
        static member private getSurfaceFormat formats =

            // specify preferred format and color space
            let isPreferred (format : VkSurfaceFormatKHR) =
                format.format = VkFormat.B8G8R8A8Unorm &&
                format.colorSpace = VkColorSpaceKHR.SrgbNonLinear

            // default to first format if preferred is unavailable
            let format =
                match Array.tryFind isPreferred formats with
                | Some format -> format
                | None -> formats[0]

            // fin
            format

        /// Create a command pool.
        static member private createCommandPool transient queueFamilyIndex device =
            
            // apply transient flag if desired
            let flags =
                if transient
                then VkCommandPoolCreateFlags.ResetCommandBuffer ||| VkCommandPoolCreateFlags.Transient
                else VkCommandPoolCreateFlags.ResetCommandBuffer

            // create command pool
            let mutable info = VkCommandPoolCreateInfo ()
            info.flags <- flags
            info.queueFamilyIndex <- queueFamilyIndex
            let mutable commandPool = Unchecked.defaultof<VkCommandPool>
            Vulkan.vkCreateCommandPool (device, &info, nullPtr, &commandPool) |> check
            commandPool

        /// Allocate an array of command buffers for each frame in flight.
        static member private allocateFifCommandBuffers commandPool device =
            allocateCommandBuffers Constants.Vulkan.MaxFramesInFlight commandPool device
        
        /// Create image available semaphores.
        static member private createImageAvailableSemaphores device =
            let semaphores = Array.zeroCreate<VkSemaphore> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec semaphores.Length do semaphores.[i] <- createSemaphore device
            semaphores

        /// Create render finished semaphores.
        static member private createRenderFinishedSemaphores imageCount device =
            let semaphores = Array.zeroCreate<VkSemaphore> imageCount
            for i in 0 .. dec semaphores.Length do semaphores.[i] <- createSemaphore device
            semaphores

        /// Create in-flight fences.
        static member private createInFlightFences device =
            let fences = Array.zeroCreate<VkFence> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec fences.Length do fences.[i] <- createFence true device
            fences
        
        /// Handle changes in window size, and check for minimization.
        static member private handleWindowSize vkc =
            
            // always disable rendering here, rendering only given permission by beginFrame
            vkc.RenderDesired_ <- false

            // query minimization status
            // NOTE: DJL: this both detects the beginning of minimization and checks for the end.
            vkc.WindowMinimized_ <- Swapchain.isWindowMinimized vkc.Swapchain_

            // refresh the swapchain if window is not minimized
            // NOTE: DJL: this happens a) when the window size simply changes and b) when minimization ends as detected above.
            // see https://vulkan-tutorial.com/Drawing_a_triangle/Swap_chain_recreation#page_Handling-minimization.
            if not vkc.WindowMinimized_ then Swapchain.refresh vkc.PhysicalDevice_ vkc.Surface_ vkc.Swapchain_ vkc.Device
        
        /// Begin the frame.
        static member beginFrame windowSize_ (windowViewport : Viewport) (vkc : VulkanContext) =

            // check for window resize
            // NOTE: DJL: WindowSizeOpt should never be used directly, only use the swap extent.
            // TODO: DJL: we need to replace this functional way of updating windowResized with a proper callback.
            // if it's out of date, some devices may fail to refresh swapchain when they should. Plus it creates an
            // awkward 2-tiered system which leads to double swapchain refreshes.
            let mutable windowResized = false
            match vkc.WindowSizeOpt_ with
            | Some windowSize ->
                windowResized <- windowSize <> windowSize_
                vkc.WindowSizeOpt_ <- Some windowSize_ // update window size
            | None -> vkc.WindowSizeOpt_ <- Some windowSize_ // init window size
            
            // ensure current frame is ready
            let mutable fence = vkc.InFlightFence
            Vulkan.vkWaitForFences (vkc.Device, 1u, asPointer &fence, true, UInt64.MaxValue) |> check

            // either deal with window bullshit or draw!
            if vkc.WindowMinimized_ then VulkanContext.handleWindowSize vkc // refresh swapchain if window restored, otherwise do nothing
            else
                if windowResized then VulkanContext.handleWindowSize vkc // refresh swapchain if size changes
                else
                    // check that swap extent >= viewport.Bounds >= viewport.Inner
                    let extent = vkc.Swapchain_.SwapExtent_
                    let swapchainBounds = box2i v2iZero (v2i (int extent.width) (int extent.height))
                    if
                        swapchainBounds.Contains windowViewport.Bounds = ContainmentType.Contains &&
                        windowViewport.Bounds.Contains windowViewport.Inner = ContainmentType.Contains
                    then
                        // try to acquire image from swapchain to draw onto
                        // NOTE: DJL: due to semaphore, if this is successful, the render *must* proceed!
                        let result = Vulkan.vkAcquireNextImageKHR (vkc.Device, vkc.Swapchain_.VkSwapchain, UInt64.MaxValue, vkc.ImageAvailableSemaphore, VkFence.Null, &ImageIndex)
                        if result = VkResult.ErrorOutOfDateKHR then VulkanContext.handleWindowSize vkc // refresh swapchain if out of date
                        else
                            check result // NOTE: DJL: this will report a suboptimal swapchain image.
                            vkc.RenderDesired_ <- true // permit rendering

            if vkc.RenderDesired_ then
            
                // reset fence for current frame if rendering is to go ahead (should be cancelled if swapchain refreshed)
                Vulkan.vkResetFences (vkc.Device, 1u, asPointer &fence) |> check

                // begin command recording
                initCommandBuffer vkc.RenderCommandBuffer
                
                // transition swapchain image layout to color attachment
                recordTransitionLayout vkc.RenderCommandBuffer true 1 0 VkImageAspectFlags.Color Undefined ColorAttachmentWrite vkc.Swapchain_.Image
                
                // clear screen
                let renderArea = VkRect2D (VkOffset2D.Zero, vkc.Swapchain_.SwapExtent_)
                let clearColor = VkClearValue (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
                let mutable rendering = makeRenderingInfo vkc.SwapchainImageView None renderArea (Some clearColor)
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // clear viewport
                let renderArea = VkRect2D (windowViewport.Bounds.Min.X, windowViewport.Bounds.Min.Y, uint windowViewport.Bounds.Size.X, uint windowViewport.Bounds.Size.Y)
                let clearColor = VkClearValue (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                let mutable rendering = makeRenderingInfo vkc.SwapchainImageView None renderArea (Some clearColor)
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

        /// End the frame.
        static member endFrame () =
            () // nothing to do

        /// Present the image back to the swapchain to appear on screen.
        static member present (vkc : VulkanContext) =
            if vkc.RenderDesired_ then
            
                // transition swapchain image layout to presentation
                recordTransitionLayout vkc.RenderCommandBuffer true 1 0 VkImageAspectFlags.Color ColorAttachmentWrite Present vkc.Swapchain_.Image
                
                // the *simple* solution: https://vulkan-tutorial.com/Drawing_a_triangle/Drawing/Rendering_and_presentation#page_Subpass-dependencies
                let waitStage = VkPipelineStageFlags.TopOfPipe
                
                // flush commands
                Queue.submit vkc.RenderCommandBuffer [|vkc.ImageAvailableSemaphore, waitStage|] [|vkc.RenderFinishedSemaphore|] vkc.InFlightFence vkc.RenderQueue
                
                // try to present image
                let result = Queue.present vkc.RenderFinishedSemaphore vkc.Swapchain_.VkSwapchain vkc.PresentQueue_

                // refresh swapchain if framebuffer out of date or suboptimal
                if result = VkResult.ErrorOutOfDateKHR || result = VkResult.SuboptimalKHR then
                    VulkanContext.handleWindowSize vkc
                else check result

                // advance frame in flight
                CurrentFrame <- inc CurrentFrame % Constants.Vulkan.MaxFramesInFlight

        /// Wait for all device operations to complete before cleaning up resources.
        static member waitIdle (vkc : VulkanContext) =
            Vulkan.vkDeviceWaitIdle vkc.Device |> check

        /// Destroy the Vulkan handles.
        static member cleanup vkc =
            Swapchain.destroy vkc.Swapchain_ vkc.Device
            for i in 0 .. dec vkc.ImageAvailableSemaphores_.Length do Vulkan.vkDestroySemaphore (vkc.Device, vkc.ImageAvailableSemaphores_.[i], nullPtr)
            for i in 0 .. dec vkc.RenderFinishedSemaphores_.Length do Vulkan.vkDestroySemaphore (vkc.Device, vkc.RenderFinishedSemaphores_.[i], nullPtr)
            for i in 0 .. dec vkc.InFlightFences_.Length do Vulkan.vkDestroyFence (vkc.Device, vkc.InFlightFences_.[i], nullPtr)
            Vulkan.vkDestroyFence (vkc.Device, vkc.TextureFence, nullPtr)
            Vulkan.vkDestroyFence (vkc.Device, vkc.TransientFence, nullPtr)
            Vulkan.vkDestroyCommandPool (vkc.Device, vkc.RenderCommandPool_, nullPtr)
            Vulkan.vkDestroyCommandPool (vkc.Device, vkc.TextureCommandPool_, nullPtr)
            Vulkan.vkDestroyCommandPool (vkc.Device, vkc.TransientCommandPool, nullPtr)
            Vma.vmaDestroyAllocator vkc.VmaAllocator
            Vulkan.vkDestroyDevice (vkc.Device, nullPtr)
            Vulkan.vkDestroySurfaceKHR (vkc.Instance_, vkc.Surface_, nullPtr)
            match vkc.DebugMessengerOpt_ with Some debugMessenger -> Vulkan.vkDestroyDebugUtilsMessengerEXT (vkc.Instance_, debugMessenger, nullPtr) | None -> ()
            Vulkan.vkDestroyInstance (vkc.Instance_, nullPtr)

        /// Attempt to create a VulkanContext.
        static member tryCreate window =

            // load vulkan; not vulkan function
            Vulkan.vkInitialize () |> check

            // make debug info
            let debugInfo = VulkanContext.makeDebugMessengerInfo ()
            
            // create instance
            let instance = VulkanContext.createVulkanInstance debugInfo window

            // load instance commands; not vulkan function
            Vulkan.vkLoadInstanceOnly instance

            // create debug messenger if validation activated
            let debugMessengerOpt = VulkanContext.tryCreateDebugMessenger debugInfo instance
            
            // create surface
            let surface = VulkanContext.createVulkanSurface window instance

            // attempt to select physical device
            match VulkanContext.trySelectPhysicalDevice surface instance with
            | Some physicalDevice ->

                // create device
                let device = VulkanContext.createLogicalDevice physicalDevice

                // load device commands; not vulkan function
                Vulkan.vkLoadDevice device

                // create vma allocator
                let allocator = VulkanContext.createVmaAllocator physicalDevice device instance

                // create render queue
                let renderQueue = Queue.create physicalDevice.GraphicsQueueFamily 0u device
                
                // create seperate present queue if graphics queue family does not support presentation
                let presentQueue =
                    if physicalDevice.GraphicsQueueFamily <> physicalDevice.PresentQueueFamily
                    then Queue.create physicalDevice.PresentQueueFamily 0u device
                    else renderQueue
                
                // create seperate queue for texture server thread if available
                let textureQueue =
                    if physicalDevice.GraphicsQueueCount > 1u
                    then Queue.create physicalDevice.GraphicsQueueFamily 1u device
                    else renderQueue
                
                // setup execution for rendering on render thread
                let renderCommandPool = VulkanContext.createCommandPool false physicalDevice.GraphicsQueueFamily device
                let renderCommandBuffers = VulkanContext.allocateFifCommandBuffers renderCommandPool device
                let inFlightFences = VulkanContext.createInFlightFences device
                
                // setup execution for presentation on render thread
                let imageAvailableSemaphores = VulkanContext.createImageAvailableSemaphores device
                
                // setup transient (one time) execution on render thread
                let transientCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily device
                let transientFence = createFence false device
                
                // setup transient (one time) execution on texture server thread
                let textureCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily device
                let textureFence = createFence false device

                // setup swapchain
                let surfaceFormat = VulkanContext.getSurfaceFormat physicalDevice.SurfaceFormats
                let swapchain = Swapchain.create surfaceFormat physicalDevice surface window device
                
                // render finished semaphores based on swapchain images rather than frames in flight to address
                // safety issue described in https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html.
                // TODO: DJL: can ImageCount change across swapchain rebuilds? If so these semaphores should be associated with the actual current vkSwapchain.
                let renderFinishedSemaphores = VulkanContext.createRenderFinishedSemaphores swapchain.ImageCount device

                // make VulkanContext
                let vulkanContext =
                    { WindowSizeOpt_ = None
                      WindowMinimized_ = false
                      RenderDesired_ = false
                      Instance_ = instance
                      DebugMessengerOpt_ = debugMessengerOpt
                      Surface_ = surface
                      PhysicalDevice_ = physicalDevice
                      Device_ = device
                      VmaAllocator_ = allocator
                      Swapchain_ = swapchain
                      RenderCommandPool_ = renderCommandPool
                      TransientCommandPool_ = transientCommandPool
                      TextureCommandPool_ = textureCommandPool
                      RenderCommandBuffers_ = renderCommandBuffers
                      RenderQueue_ = renderQueue
                      PresentQueue_ = presentQueue
                      TextureQueue_ = textureQueue
                      ImageAvailableSemaphores_ = imageAvailableSemaphores
                      RenderFinishedSemaphores_ = renderFinishedSemaphores
                      InFlightFences_ = inFlightFences
                      TransientFence_ = transientFence
                      TextureFence_ = textureFence }

                // fin
                Some vulkanContext

            // failure
            | None -> None
