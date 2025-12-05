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

    /// An image layout in its access and pipeline stage context.
    type ImageLayout =
        | Undefined
        | UndefinedHost
        | TransferSrc
        | TransferDst
        | ShaderRead
        | ColorAttachmentWrite
        | Present

        /// The VkImageLayout.
        member this.VkImageLayout =
            match this with
            | Undefined -> Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            | UndefinedHost -> Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
            | TransferSrc -> Vulkan.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            | TransferDst -> Vulkan.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            | ShaderRead -> Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            | ColorAttachmentWrite -> Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            | Present -> Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

        /// The access flag.
        member this.Access =
            match this with
            | Undefined -> VkAccessFlags.None
            | UndefinedHost -> VkAccessFlags.None
            | TransferSrc -> Vulkan.VK_ACCESS_TRANSFER_READ_BIT
            | TransferDst -> Vulkan.VK_ACCESS_TRANSFER_WRITE_BIT
            | ShaderRead -> Vulkan.VK_ACCESS_SHADER_READ_BIT
            | ColorAttachmentWrite -> Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
            | Present -> VkAccessFlags.None

        /// The pipeline stage.
        member this.PipelineStage =
            match this with
            | Undefined -> Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
            | UndefinedHost -> Vulkan.VK_PIPELINE_STAGE_HOST_BIT
            | TransferSrc -> Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT
            | TransferDst -> Vulkan.VK_PIPELINE_STAGE_TRANSFER_BIT
            | ShaderRead -> Vulkan.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
            | ColorAttachmentWrite -> Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            | Present -> Vulkan.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    
    /// The format of a vertex attribute.
    type VertexAttribFormat =
        | Byte
        | Byte2
        | Byte3
        | Byte4
        | Int
        | Int2
        | Int3
        | Int4
        | Uint
        | Uint2
        | Uint3
        | Uint4
        | Single
        | Single2
        | Single3
        | Single4
        | Double

        /// The VkFormat.
        member this.VkFormat =
            match this with
            | Byte -> Vulkan.VK_FORMAT_R8_UNORM
            | Byte2 -> Vulkan.VK_FORMAT_R8G8_UNORM
            | Byte3 -> Vulkan.VK_FORMAT_R8G8B8_UNORM
            | Byte4 -> Vulkan.VK_FORMAT_R8G8B8A8_UNORM
            | Int -> Vulkan.VK_FORMAT_R32_SINT
            | Int2 -> Vulkan.VK_FORMAT_R32G32_SINT
            | Int3 -> Vulkan.VK_FORMAT_R32G32B32_SINT
            | Int4 -> Vulkan.VK_FORMAT_R32G32B32A32_SINT
            | Uint -> Vulkan.VK_FORMAT_R32_UINT
            | Uint2 -> Vulkan.VK_FORMAT_R32G32_UINT
            | Uint3 -> Vulkan.VK_FORMAT_R32G32B32_UINT
            | Uint4 -> Vulkan.VK_FORMAT_R32G32B32A32_UINT
            | Single -> Vulkan.VK_FORMAT_R32_SFLOAT
            | Single2 -> Vulkan.VK_FORMAT_R32G32_SFLOAT
            | Single3 -> Vulkan.VK_FORMAT_R32G32B32_SFLOAT
            | Single4 -> Vulkan.VK_FORMAT_R32G32B32A32_SFLOAT
            | Double -> Vulkan.VK_FORMAT_R64_SFLOAT
    
    /// A shader stage or combination.
    type ShaderStage =
        | Vertex
        | Fragment
        | VertexFragment

        /// The VkShaderStageFlags.
        member this.VkShaderStageFlags =
            match this with
            | Vertex -> Vulkan.VK_SHADER_STAGE_VERTEX_BIT
            | Fragment -> Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
            | VertexFragment -> Vulkan.VK_SHADER_STAGE_VERTEX_BIT ||| Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
    
    /// Convert VkExtensionProperties.extensionName to a string.
    /// TODO: see if we can inline functions like these once F# supports C#'s representation of this fixed buffer type.
    let private getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName

    /// Convert VkLayerProperties.layerName to a string.
    let private getLayerName (layerProps : VkLayerProperties) =
        NativePtr.fixedBufferToString layerProps.layerName

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
            blendAttachment.colorBlendOp <- Vulkan.VK_BLEND_OP_ADD
            blendAttachment.srcAlphaBlendFactor <- srcAlpha
            blendAttachment.dstAlphaBlendFactor <- dstAlpha
            blendAttachment.alphaBlendOp <- Vulkan.VK_BLEND_OP_ADD
        | None -> ()
        blendAttachment.colorWriteMask <- Vulkan.VK_COLOR_COMPONENT_R_BIT ||| Vulkan.VK_COLOR_COMPONENT_G_BIT ||| Vulkan.VK_COLOR_COMPONENT_B_BIT ||| Vulkan.VK_COLOR_COMPONENT_A_BIT
        blendAttachment

    /// Make a VkImageSubresourceRange representing a color image.
    let makeSubresourceRangeColor mips layers =
        let mutable subresourceRange = VkImageSubresourceRange ()
        subresourceRange.aspectMask <- Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        subresourceRange.levelCount <- uint mips
        subresourceRange.layerCount <- uint layers
        subresourceRange

    /// Make a VkImageSubresourceLayers representing a color image.
    let makeSubresourceLayersColor (mipLevel : int) (layer : int) =
        let mutable subresourceLayers = VkImageSubresourceLayers ()
        subresourceLayers.aspectMask <- Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        subresourceLayers.mipLevel <- uint mipLevel
        subresourceLayers.baseArrayLayer <- uint layer
        subresourceLayers.layerCount <- 1u
        subresourceLayers

    /// Make a VkVertexInputBindingDescription with vertex input rate.
    let makeVertexBindingVertex (bindingIndex : int) (stride : int) =
        let mutable binding = VkVertexInputBindingDescription ()
        binding.binding <- uint bindingIndex
        binding.stride <- uint stride
        binding.inputRate <- Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
        binding

    /// Make a VkVertexInputAttributeDescription.
    let makeVertexAttribute (location : int) (binding : int) (format : VertexAttribFormat) (offset : int) =
        let mutable attribute = VkVertexInputAttributeDescription ()
        attribute.location <- uint location
        attribute.binding <- uint binding
        attribute.format <- format.VkFormat
        attribute.offset <- uint offset
        attribute

    /// Make a VkDescriptorSetLayoutBinding.
    let makeDescriptorBinding (bindingIndex : int) descriptorType (descriptorCount : int) (shaderStage : ShaderStage) =
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.binding <- uint bindingIndex
        binding.descriptorType <- descriptorType
        binding.descriptorCount <- uint descriptorCount
        binding.stageFlags <- shaderStage.VkShaderStageFlags
        binding

    /// Make a push constant range.
    let makePushConstantRange stages (offset : int) (size : int) =
        let mutable range = VkPushConstantRange ()
        range.stageFlags <- stages
        range.offset <- uint offset
        range.size <- uint size
        range

    /// Make a VkRenderingInfo.
    /// NOTE: DJL: must be inline to keep pointer valid.
    let inline makeRenderingInfo imageView renderArea clearValueOpt =
        
        // attachment info
        let mutable aInfo = VkRenderingAttachmentInfo ()
        aInfo.imageView <- imageView
        aInfo.imageLayout <- ColorAttachmentWrite.VkImageLayout
        aInfo.storeOp <- Vulkan.VK_ATTACHMENT_STORE_OP_STORE
        match clearValueOpt with
        | Some clearValue ->
            aInfo.loadOp <- Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
            aInfo.clearValue <- clearValue
        | None ->
            aInfo.loadOp <- Vulkan.VK_ATTACHMENT_LOAD_OP_LOAD

        // rendering info
        let mutable rInfo = VkRenderingInfo ()
        rInfo.renderArea <- renderArea
        rInfo.layerCount <- 1u
        rInfo.colorAttachmentCount <- 1u
        rInfo.pColorAttachments <- asPointer &aInfo
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
        elif int result < 0 then Log.error ("Vulkan assertion failed due to: " + string result)

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
    let recordTransitionLayout cb allLevels mipNumber layer (oldLayout : ImageLayout) (newLayout : ImageLayout) vkImage =
        
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
        barrier.subresourceRange <- makeSubresourceRangeColor mipLevels 1
        barrier.subresourceRange.baseArrayLayer <- uint layer
        barrier.subresourceRange.baseMipLevel <- uint mipLevel
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
    let createImageView reverseSwizzle format mips isCube image device =
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- if isCube then Vulkan.VK_IMAGE_VIEW_TYPE_CUBE else Vulkan.VK_IMAGE_VIEW_TYPE_2D
        info.format <- format
        
        // rgba -> bgra
        if reverseSwizzle then
            let mutable components = VkComponentMapping ()
            components.r <- Vulkan.VK_COMPONENT_SWIZZLE_B
            components.b <- Vulkan.VK_COMPONENT_SWIZZLE_R
            info.components <- components

        let layers = if isCube then 6 else 1
        info.subresourceRange <- makeSubresourceRangeColor mips layers
        let mutable imageView = Unchecked.defaultof<VkImageView>
        Vulkan.vkCreateImageView (device, &info, nullPtr, &imageView) |> check
        imageView

    /// Allocate an array of command buffers.
    let allocateCommandBuffers count commandPool device =
        let mutable info = VkCommandBufferAllocateInfo ()
        info.commandPool <- commandPool
        info.level <- Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
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
            if createSignaled then VkFenceCreateInfo (flags = Vulkan.VK_FENCE_CREATE_SIGNALED_BIT)
            else VkFenceCreateInfo ()
        let mutable fence = Unchecked.defaultof<VkFence>
        Vulkan.vkCreateFence (device, &info, nullPtr, &fence) |> check
        fence
    
    /// Wait for a fence to signal and reset it for reuse.
    let awaitFence fence device =
        let mutable fence = fence
        Vulkan.vkWaitForFences (device, 1u, asPointer &fence, true, UInt64.MaxValue) |> check
        Vulkan.vkResetFences (device, 1u, asPointer &fence) |> check

    /// Begin recording to a transient command buffer.
    /// TODO: DJL: review choice of transient command buffers over normal ones.
    let beginTransientCommandBlock commandPool device =
        
        // create command buffer
        let cb = allocateCommandBuffer commandPool device

        // reset command buffer and begin recording
        Vulkan.vkResetCommandPool (device, commandPool, VkCommandPoolResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
        Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check

        // return command buffer
        cb
    
    /// End recording to a transient command buffer, execute and free.
    let endTransientCommandBlock cb commandQueue commandPool finishFence device =
        
        // execute command
        let mutable cb = cb
        Vulkan.vkEndCommandBuffer cb |> check
        let mutable sInfo = VkSubmitInfo ()
        sInfo.commandBufferCount <- 1u
        sInfo.pCommandBuffers <- asPointer &cb
        Vulkan.vkQueueSubmit (commandQueue, 1u, asPointer &sInfo, finishFence) |> check
        awaitFence finishFence device

        // free command buffer
        Vulkan.vkFreeCommandBuffers (device, commandPool, 1u, asPointer &cb)
    
    /// Begin persistent command buffer recording.
    let beginPersistentCommandBlock cb =
        Vulkan.vkResetCommandBuffer (cb, VkCommandBufferResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo ()
        Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check
    
    /// End persistent command buffer recording and submit for execution.
    let endPersistentCommandBlock cb commandQueue waitSemaphoresStages (signalSemaphores : VkSemaphore array) signalFence =

        // end command buffer recording
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
        Vulkan.vkQueueSubmit (commandQueue, 1u, asPointer &info, signalFence) |> check
    
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
            // TODO: as part of optimization, the sharing mode in this case should probably be VK_SHARING_MODE_EXCLUSIVE (see below).
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
            info.imageUsage <- Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            if (physicalDevice.GraphicsQueueFamily = physicalDevice.PresentQueueFamily) then
                info.imageSharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            else
                info.imageSharingMode <- Vulkan.VK_SHARING_MODE_CONCURRENT
                info.queueFamilyIndexCount <- 2u
                info.pQueueFamilyIndices <- indicesArrayPin.Pointer
            info.preTransform <- capabilities.currentTransform
            info.compositeAlpha <- Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
            info.presentMode <- Vulkan.VK_PRESENT_MODE_FIFO_KHR // NOTE: guaranteed by the spec and seems most appropriate for Nu.
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
            for i in 0 .. dec imageViews.Length do imageViews.[i] <- createImageView false format 1 false images.[i] device
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
    
    /// Exposes the vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanContext =
        private
            { mutable WindowSizeOpt_ : Vector2i option
              mutable WindowMinimized_ : bool
              mutable RenderDesired_ : bool
              Instance_ : VkInstance
              Surface_ : VkSurfaceKHR
              PhysicalDevice_ : PhysicalDevice
              Device_ : VkDevice
              VmaAllocator_ : VmaAllocator
              Swapchain_ : Swapchain
              RenderCommandPool_ : VkCommandPool
              TransientCommandPool_ : VkCommandPool
              TextureCommandPool_ : VkCommandPool
              RenderCommandBuffers_ : VkCommandBuffer array
              RenderQueue_ : VkQueue
              PresentQueue_ : VkQueue
              TextureQueue_ : VkQueue
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
        member this.RenderFinishedSemaphore = this.RenderFinishedSemaphores_.[CurrentFrame]

        /// The in flight fence for the current frame.
        member this.InFlightFence = this.InFlightFences_.[CurrentFrame]

        /// The texture fence.
        member this.TextureFence = this.TextureFence_
        
        /// The transient fence.
        member this.TransientFence = this.TransientFence_

        /// The current swapchain image view.
        member this.SwapchainImageView = this.Swapchain_.ImageView
        
        /// The swap format.
        member this.SwapFormat = this.Swapchain_.SurfaceFormat_.format

        [<UnmanagedCallersOnly>]
        static member private debugCallback
            (messageSeverity : VkDebugUtilsMessageSeverityFlagsEXT,
             messageTypes : VkDebugUtilsMessageTypeFlagsEXT,
             callbackData : nativeint,
             userData : nativeint) : uint =
            
            Vulkan.VK_FALSE
        
        static member private makeDebugMessengerInfo () =
            let mutable info = VkDebugUtilsMessengerCreateInfoEXT ()
            info.messageSeverity <-
                Vulkan.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |||
                Vulkan.VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT |||
                Vulkan.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |||
                Vulkan.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
            info.messageType <-
                Vulkan.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |||
                Vulkan.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |||
                Vulkan.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
            info.pfnUserCallback // TODO: DJL: put pointer to debugCallback here!
            info.pUserData <- nullVoidPtr
            info
        
        /// Create the Vulkan instance.
        static member private createVulkanInstance window =

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

            // TODO: P0: DJL: setup message callback with debug utils.

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
                physicalDevice.Properties.deviceType = Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU

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
                format.format = Vulkan.VK_FORMAT_B8G8R8A8_UNORM &&
                format.colorSpace = Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

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
                then Vulkan.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ||| Vulkan.VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
                else Vulkan.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT

            // create command pool
            let mutable info = VkCommandPoolCreateInfo ()
            info.flags <- flags
            info.queueFamilyIndex <- queueFamilyIndex
            let mutable commandPool = Unchecked.defaultof<VkCommandPool>
            Vulkan.vkCreateCommandPool (device, &info, nullPtr, &commandPool) |> check
            commandPool

        /// Get command queue.
        static member private getQueue queueFamilyIndex queueIndex device =
            let mutable queue = Unchecked.defaultof<VkQueue>
            Vulkan.vkGetDeviceQueue (device, queueFamilyIndex, queueIndex, &queue)
            queue

        /// Allocate an array of command buffers for each frame in flight.
        static member private allocateFifCommandBuffers commandPool device =
            allocateCommandBuffers Constants.Vulkan.MaxFramesInFlight commandPool device
        
        /// Create an array of semaphores for each frame in flight.
        static member private createSemaphores device =
            let semaphores = Array.zeroCreate<VkSemaphore> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec semaphores.Length do semaphores.[i] <- createSemaphore device
            semaphores

        /// Create an array of fences for each frame in flight.
        static member private createFences device =
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
        static member beginFrame windowSize_ (bounds : Box2i) (vkc : VulkanContext) =

            // check for window resize
            // NOTE: DJL: this should never be used directly, only use the swap extent.
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
                    // try to acquire image from swapchain to draw onto
                    let result = Vulkan.vkAcquireNextImageKHR (vkc.Device, vkc.Swapchain_.VkSwapchain, UInt64.MaxValue, vkc.ImageAvailableSemaphore, VkFence.Null, &ImageIndex)
                    if result = Vulkan.VK_ERROR_OUT_OF_DATE_KHR then VulkanContext.handleWindowSize vkc // refresh swapchain if out of date
                    else
                        vkc.RenderDesired_ <- true // permit rendering
                        check result

            if vkc.RenderDesired_ then
            
                // reset fence for current frame if rendering is to go ahead (should be cancelled if swapchain refreshed)
                Vulkan.vkResetFences (vkc.Device, 1u, asPointer &fence) |> check

                // begin command recording
                beginPersistentCommandBlock vkc.RenderCommandBuffer
                
                // transition swapchain image layout to color attachment
                recordTransitionLayout vkc.RenderCommandBuffer true 1 0 Undefined ColorAttachmentWrite vkc.Swapchain_.Image
                
                // clear screen
                let renderArea = VkRect2D (VkOffset2D.Zero, vkc.Swapchain_.SwapExtent_)
                let clearColor = VkClearValue (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
                let mutable rendering = makeRenderingInfo vkc.SwapchainImageView renderArea (Some clearColor)
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // clear viewport
                let renderArea = VkRect2D (bounds.Min.X, bounds.Min.Y, uint bounds.Size.X, uint bounds.Size.Y)
                let clearColor = VkClearValue (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
                let mutable rendering = makeRenderingInfo vkc.SwapchainImageView renderArea (Some clearColor)
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

        /// End the frame.
        static member endFrame () =
            () // nothing to do

        /// Present the image back to the swapchain to appear on screen.
        static member present (vkc : VulkanContext) =
            if vkc.RenderDesired_ then
            
                // transition swapchain image layout to presentation
                recordTransitionLayout vkc.RenderCommandBuffer true 1 0 ColorAttachmentWrite Present vkc.Swapchain_.Image
                
                // the *simple* solution: https://vulkan-tutorial.com/Drawing_a_triangle/Drawing/Rendering_and_presentation#page_Subpass-dependencies
                let waitStage = Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
                
                // flush commands
                let mutable renderFinished = vkc.RenderFinishedSemaphore
                endPersistentCommandBlock vkc.RenderCommandBuffer vkc.RenderQueue [|vkc.ImageAvailableSemaphore, waitStage|] [|renderFinished|] vkc.InFlightFence
                
                // try to present image
                let mutable swapchain = vkc.Swapchain_.VkSwapchain
                let mutable info = VkPresentInfoKHR ()
                info.waitSemaphoreCount <- 1u
                info.pWaitSemaphores <- asPointer &renderFinished
                info.swapchainCount <- 1u
                info.pSwapchains <- asPointer &swapchain
                info.pImageIndices <- asPointer &ImageIndex
                let result = Vulkan.vkQueuePresentKHR (vkc.PresentQueue_, asPointer &info)

                // refresh swapchain if framebuffer out of date or suboptimal
                if result = Vulkan.VK_ERROR_OUT_OF_DATE_KHR || result = Vulkan.VK_SUBOPTIMAL_KHR then
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
            Vulkan.vkDestroyInstance (vkc.Instance_, nullPtr)

        /// Attempt to create a VulkanContext.
        static member tryCreate window =

            // load vulkan; not vulkan function
            Vulkan.vkInitialize () |> check

            // make debug info
            //let debugInfo = VulkanContext.makeDebugMessengerInfo ()
            
            // create instance
            let instance = VulkanContext.createVulkanInstance window

            // load instance commands; not vulkan function
            Vulkan.vkLoadInstanceOnly instance

            // create debug messenger if validation activated
            //let debugMessengerOpt = VulkanContext.tryCreateDebugMessenger debugInfo instance
            
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

                // setup execution for rendering on render thread
                let renderCommandPool = VulkanContext.createCommandPool false physicalDevice.GraphicsQueueFamily device
                let renderCommandBuffers = VulkanContext.allocateFifCommandBuffers renderCommandPool device
                let renderQueue = VulkanContext.getQueue physicalDevice.GraphicsQueueFamily 0u device
                let inFlightFences = VulkanContext.createFences device
                
                // setup execution for presentation on render thread
                let presentQueue = VulkanContext.getQueue physicalDevice.PresentQueueFamily 0u device
                let imageAvailableSemaphores = VulkanContext.createSemaphores device
                let renderFinishedSemaphores = VulkanContext.createSemaphores device
                
                // setup transient (one time) execution on render thread
                let transientCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily device
                let transientFence = createFence false device
                
                // setup transient (one time) execution on texture server thread
                let textureCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily device
                let textureQueue = VulkanContext.getQueue physicalDevice.GraphicsQueueFamily (min 1u (physicalDevice.GraphicsQueueCount - 1u)) device
                let textureFence = createFence false device

                // setup swapchain
                let surfaceFormat = VulkanContext.getSurfaceFormat physicalDevice.SurfaceFormats
                let swapchain = Swapchain.create surfaceFormat physicalDevice surface window device

                // make VulkanContext
                let vulkanContext =
                    { WindowSizeOpt_ = None
                      WindowMinimized_ = false
                      RenderDesired_ = false
                      Instance_ = instance
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
