// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open System
open System.Collections.Generic
open System.IO
open SDL2
open Vortice.ShaderCompiler
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    let mutable private DrawReportLock = obj ()
    let mutable private DrawCallCount = 0
    let mutable private DrawInstanceCount = 0

    /// Validation layers enabled.
    let private ValidationLayersEnabled =
#if DEBUG
        true
#else
        false
#endif

    /// Index of the current Swapchain image.
    let mutable private ImageIndex = 0u

    /// The current frame within MaxFramesInFlight.
    let mutable CurrentFrame = 0

    /// Convert VkExtensionProperties.extensionName to a string.
    /// TODO: see if we can inline functions like these once F# supports C#'s representation of this fixed buffer type.
    let private getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName

    /// Convert VkLayerProperties.layerName to a string.
    let private getLayerName (layerProps : VkLayerProperties) =
        NativePtr.fixedBufferToString layerProps.layerName

    /// Make a VkViewport.
    let makeViewport (rect : VkRect2D) =
        let mutable viewport = VkViewport ()
        viewport.x <- single rect.offset.x
        viewport.y <- single rect.offset.y
        viewport.width <- single rect.extent.width
        viewport.height <- single rect.extent.height
        viewport.minDepth <- 0.0f
        viewport.maxDepth <- 1.0f
        viewport

    /// Make a VkPipelineColorBlendAttachmentState.
    let makeBlendAttachment srcColor dstColor srcAlpha dstAlpha =
        let mutable blendAttachment = VkPipelineColorBlendAttachmentState ()
        blendAttachment.blendEnable <- true
        blendAttachment.srcColorBlendFactor <- srcColor
        blendAttachment.dstColorBlendFactor <- dstColor
        blendAttachment.colorBlendOp <- Vulkan.VK_BLEND_OP_ADD
        blendAttachment.srcAlphaBlendFactor <- srcAlpha
        blendAttachment.dstAlphaBlendFactor <- dstAlpha
        blendAttachment.alphaBlendOp <- Vulkan.VK_BLEND_OP_ADD
        blendAttachment.colorWriteMask <- Vulkan.VK_COLOR_COMPONENT_R_BIT ||| Vulkan.VK_COLOR_COMPONENT_G_BIT ||| Vulkan.VK_COLOR_COMPONENT_B_BIT ||| Vulkan.VK_COLOR_COMPONENT_A_BIT
        blendAttachment

    /// Make a VkImageSubresourceRange representing a color image.
    let makeSubresourceRangeColor mips =
        let mutable subresourceRange = VkImageSubresourceRange ()
        subresourceRange.aspectMask <- Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
        subresourceRange.levelCount <- mips
        subresourceRange.layerCount <- 1u
        subresourceRange

    /// Make a VkImageSubresourceLayers representing a color image.
    let makeSubresourceLayersColor () =
        let mutable subresourceLayers = VkImageSubresourceLayers ()
        subresourceLayers.aspectMask <- Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
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
    let makeVertexAttribute (location : int) (binding : int) format (offset : int) =
        let mutable attribute = VkVertexInputAttributeDescription ()
        attribute.location <- uint location
        attribute.binding <- uint binding
        attribute.format <- format
        attribute.offset <- uint offset
        attribute

    /// Make a VkDescriptorSetLayoutBinding for the vertex stage.
    let makeDescriptorBindingVertex (bindingIndex : int) descriptorType (descriptorCount : int) =
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.binding <- uint bindingIndex
        binding.descriptorType <- descriptorType
        binding.descriptorCount <- uint descriptorCount
        binding.stageFlags <- Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        binding

    /// Make a VkDescriptorSetLayoutBinding for the fragment stage.
    let makeDescriptorBindingFragment (bindingIndex : int) descriptorType (descriptorCount : int) =
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.binding <- uint bindingIndex
        binding.descriptorType <- descriptorType
        binding.descriptorCount <- uint descriptorCount
        binding.stageFlags <- Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        binding

    /// Make a VkDescriptorSetLayoutBinding for the vertex and fragment stages.
    let makeDescriptorBindingVertexFragment (bindingIndex : int) descriptorType (descriptorCount : int) =
        let mutable binding = VkDescriptorSetLayoutBinding ()
        binding.binding <- uint bindingIndex
        binding.descriptorType <- descriptorType
        binding.descriptorCount <- uint descriptorCount
        binding.stageFlags <- Vulkan.VK_SHADER_STAGE_VERTEX_BIT ||| Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        binding

    /// Make a push constant range.
    let makePushConstantRange stages (offset : int) (size : int) =
        let mutable range = VkPushConstantRange ()
        range.stageFlags <- stages
        range.offset <- uint offset
        range.size <- uint size
        range

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan assertion failed due to: " + string result)

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

    /// Create an image view.
    let createImageView format mips image device =
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- Vulkan.VK_IMAGE_VIEW_TYPE_2D
        info.format <- format
        info.subresourceRange <- makeSubresourceRangeColor mips
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
        Vulkan.vkWaitForFences (device, 1u, asPointer &fence, VkBool32.True, UInt64.MaxValue) |> check
        Vulkan.vkResetFences (device, 1u, asPointer &fence) |> check

    /// Begin command buffer recording and render pass.
    let beginRenderBlock cb renderPass framebuffer renderArea clearValues waitFence device =

#if DEBUG
        // show whether fence is signalled during debugging
        if waitFence <> VkFence.Null then
            let status = Vulkan.vkGetFenceStatus(device, waitFence)
            ()
#endif
        
        // await fence if not null
        // TODO: investigate if passing the null fence into awaitFence performs a no-op as expected and if so, don't
        // bother to check for it here :)
        if waitFence <> VkFence.Null then awaitFence waitFence device

        // reset command buffer and begin recording
        Vulkan.vkResetCommandBuffer (cb, VkCommandBufferResetFlags.None) |> check
        let mutable cbInfo = VkCommandBufferBeginInfo ()
        Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check

        // begin render pass
        use clearValuesPin = new ArrayPin<_> (clearValues)
        let mutable rpInfo = VkRenderPassBeginInfo ()
        rpInfo.renderPass <- renderPass
        rpInfo.framebuffer <- framebuffer
        rpInfo.renderArea <- renderArea
        rpInfo.clearValueCount <- uint clearValues.Length
        rpInfo.pClearValues <- clearValuesPin.Pointer
        Vulkan.vkCmdBeginRenderPass (cb, asPointer &rpInfo, Vulkan.VK_SUBPASS_CONTENTS_INLINE)

    /// End command buffer recording and render pass and submit for execution.
    let endRenderBlock cb commandQueue waitSemaphoresStages signalSemaphores signalFence =

        // end render pass
        Vulkan.vkCmdEndRenderPass cb

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
    /// TODO: rename to PhysicalDeviceContext or something that doesn't imply this is mere dumb data.
    type PhysicalDeviceData =
        { PhysicalDevice : VkPhysicalDevice
          Properties : VkPhysicalDeviceProperties
          Extensions : VkExtensionProperties array
          SurfaceCapabilities : VkSurfaceCapabilitiesKHR
          Formats : VkSurfaceFormatKHR array
          GraphicsQueueFamily : uint
          PresentQueueFamily : uint }

        /// Get properties.
        static member private getProperties physicalDevice =
            let mutable properties = Unchecked.defaultof<VkPhysicalDeviceProperties>
            Vulkan.vkGetPhysicalDeviceProperties (physicalDevice, &properties)
            properties

        /// Get available extensions.
        static member private getExtensions physicalDevice =
            let mutable extensionCount = 0u
            Vulkan.vkEnumerateDeviceExtensionProperties (physicalDevice, nullPtr, asPointer &extensionCount, nullPtr) |> check
            let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
            use extensionsPin = new ArrayPin<_> (extensions)
            Vulkan.vkEnumerateDeviceExtensionProperties (physicalDevice, nullPtr, asPointer &extensionCount, extensionsPin.Pointer) |> check
            extensions

        /// Get surface capabilities.
        static member private getSurfaceCapabilities physicalDevice surface =
            let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
            Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (physicalDevice, surface, &capabilities) |> check
            capabilities

        /// Get available surface formats.
        static member private getSurfaceFormats physicalDevice surface =
            let mutable formatCount = 0u
            Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice, surface, asPointer &formatCount, nullPtr) |> check
            let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
            use formatsPin = new ArrayPin<_> (formats)
            Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice, surface, asPointer &formatCount, formatsPin.Pointer) |> check
            formats

        /// Attempt to get the queue families.
        static member private tryGetQueueFamilies physicalDevice surface =

            // get queue families' properties
            let mutable queueFamilyCount = 0u
            Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, asPointer &queueFamilyCount, nullPtr)
            let queueFamilyProps = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
            use queueFamilyPropsPin = new ArrayPin<_> (queueFamilyProps)
            Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, asPointer &queueFamilyCount, queueFamilyPropsPin.Pointer)

            // NOTE: DJL: It is *essential* to use the *first* compatible queue families in the array, *not* the last, as per the tutorial and vortice vulkan sample.
            // I discovered this by accident because the queue families on my AMD behaved exactly the same as the queue families on this one:
            // https://computergraphics.stackexchange.com/questions/9707/queue-from-a-family-queue-that-supports-presentation-doesnt-work-vulkan
            // general lesson: trust level for vendors is too low for deviation from common practices to be advisable.
            let mutable graphicsQueueFamilyOpt = None
            let mutable presentQueueFamilyOpt = None
            for i in 0 .. dec queueFamilyProps.Length do

                // try get graphics queue family
                match graphicsQueueFamilyOpt with
                | None ->
                    let props = queueFamilyProps.[i]
                    if props.queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then
                        graphicsQueueFamilyOpt <- Some (uint i)
                | Some _ -> ()

                // try get present queue family
                match presentQueueFamilyOpt with
                | None ->
                    let mutable presentSupport = VkBool32.False
                    Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR (physicalDevice, uint i, surface, &presentSupport) |> check
                    if (presentSupport = VkBool32.True) then
                        presentQueueFamilyOpt <- Some (uint i)
                | Some _ -> ()

            // fin
            (graphicsQueueFamilyOpt, presentQueueFamilyOpt)

        /// Attempt to construct PhysicalDeviceData.
        static member tryCreate physicalDevice surface =
            let properties = PhysicalDeviceData.getProperties physicalDevice
            let extensions = PhysicalDeviceData.getExtensions physicalDevice
            let surfaceCapabilities = PhysicalDeviceData.getSurfaceCapabilities physicalDevice surface
            let surfaceFormats = PhysicalDeviceData.getSurfaceFormats physicalDevice surface
            match PhysicalDeviceData.tryGetQueueFamilies physicalDevice surface with
            | (Some graphicsQueueFamily, Some presentQueueFamily) ->
                let physicalDeviceData =
                    { PhysicalDevice = physicalDevice
                      Properties = properties
                      Extensions = extensions
                      SurfaceCapabilities = surfaceCapabilities
                      Formats = surfaceFormats
                      GraphicsQueueFamily = graphicsQueueFamily
                      PresentQueueFamily = presentQueueFamily }
                Some physicalDeviceData
            | (_, _) -> None
    
    /// Exposes the vulkan handles that must be globally accessible within the renderer.
    /// TODO: maybe rename this to VulkanContext.
    /// TODO: DJL: make props private.
    type [<ReferenceEquality>] VulkanGlobal =
        { Instance : VkInstance
          Surface : VkSurfaceKHR
          Device : VkDevice
          VmaAllocator : VmaAllocator
          Swapchain : VkSwapchainKHR
          SwapchainImageViews : VkImageView array
          TransferCommandPool : VkCommandPool
          RenderCommandPool : VkCommandPool
          RenderCommandBuffers : VkCommandBuffer array
          GraphicsQueue : VkQueue
          PresentQueue : VkQueue
          ImageAvailableSemaphores : VkSemaphore array
          RenderFinishedSemaphores : VkSemaphore array
          InFlightFences : VkFence array
          ResourceReadyFence : VkFence
          RenderPass : VkRenderPass
          ClearRenderPass : VkRenderPass
          PresentRenderPass : VkRenderPass
          SwapchainFramebuffers : VkFramebuffer array
          SwapExtent : VkExtent2D }

        /// The render command buffer for the current frame.
        member this.RenderCommandBuffer = this.RenderCommandBuffers.[CurrentFrame]

        /// The image available semaphore for the current frame.
        member this.ImageAvailableSemaphore = this.ImageAvailableSemaphores.[CurrentFrame]

        /// The render finished semaphore for the current frame.
        member this.RenderFinishedSemaphore = this.RenderFinishedSemaphores.[CurrentFrame]

        /// The in flight fence for the current frame.
        member this.InFlightFence = this.InFlightFences.[CurrentFrame]
        
        /// The current swapchain framebuffer.
        member this.SwapchainFramebuffer = this.SwapchainFramebuffers.[int ImageIndex]

        /// Create the Vulkan instance.
        static member private createVulkanInstance window =

            // get sdl extensions
            let mutable sdlExtensionCount = 0u
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, null)
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensionsOut = Array.zeroCreate<nativeint> (int sdlExtensionCount)
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, sdlExtensionsOut)
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensions = Array.zeroCreate<nativeptr<byte>> (int sdlExtensionCount)
            for i in 0 .. dec (int sdlExtensionCount) do sdlExtensions.[i] <- NativePtr.nativeintToBytePtr sdlExtensionsOut.[i]
            use sdlExtensionsPin = new ArrayPin<_> (sdlExtensions)

            // TODO: P0: DJL: setup message callback with debug utils.

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

            // TODO: P1: DJL: apply VkApplicationInfo once all compulsory fields have been decided (e.g. engineVersion)
            // and check for available vulkan version as described in 
            // https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap4.html#VkApplicationInfo.

            // must be assigned outside conditional to remain in scope until vkCreateInstance
            use layerWrap = new StringArrayWrap ([|validationLayer|])

            // create instance
            let mutable info = VkInstanceCreateInfo ()
            info.enabledExtensionCount <- sdlExtensionCount
            info.ppEnabledExtensionNames <- sdlExtensionsPin.Pointer
            if ValidationLayersEnabled && validationLayerExists then
                info.enabledLayerCount <- 1u
                info.ppEnabledLayerNames <- layerWrap.Pointer
            let mutable instance = Unchecked.defaultof<VkInstance>
            Vulkan.vkCreateInstance (&info, nullPtr, &instance) |> check
            instance

        /// Create vulkan surface.
        static member private createVulkanSurface window instance =
            let mutable surface = Unchecked.defaultof<VkSurfaceKHR>
            let result = SDL.SDL_Vulkan_CreateSurface (window, instance, &(NativePtr.reinterpretRef<VkSurfaceKHR, uint64> &surface))
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_CreateSurface failed."
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
                    match PhysicalDeviceData.tryCreate devices.[i] surface with
                    | Some pdd -> pdd
                    | None -> ()]

            // compatibility criteria: device must support essential rendering components
            let isCompatible physicalDeviceData =
                let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
                let swapchainSupported = Array.exists (fun ext -> getExtensionName ext = swapchainExtensionName) physicalDeviceData.Extensions
                swapchainSupported && physicalDeviceData.Formats.Length > 0

            // preferability criteria: device ought to be discrete
            let isPreferable physicalDeviceData =
                physicalDeviceData.Properties.deviceType = Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU

            // filter and order candidates according to criteria
            let candidatesFiltered = List.filter isCompatible candidates
            let (fstChoice, sndChoice) = List.partition isPreferable candidatesFiltered
            let candidatesFilteredAndOrdered = List.append fstChoice sndChoice
                
            // if compatible devices exist then return the first along with its data
            let physicalDeviceOpt =
                if candidatesFilteredAndOrdered.Length > 0
                then Some (List.head candidatesFilteredAndOrdered)
                else Log.info "Could not find a suitable graphics device for Vulkan."; None

            // fin
            physicalDeviceOpt
        
        /// Create the logical device.
        static member private createLogicalDevice (physicalDeviceData : PhysicalDeviceData) =

            // get unique queue family array
            let uniqueQueueFamiliesSet = new HashSet<uint> ()
            uniqueQueueFamiliesSet.Add physicalDeviceData.GraphicsQueueFamily |> ignore
            uniqueQueueFamiliesSet.Add physicalDeviceData.PresentQueueFamily |> ignore
            let uniqueQueueFamilies = Array.zeroCreate<uint> uniqueQueueFamiliesSet.Count
            uniqueQueueFamiliesSet.CopyTo uniqueQueueFamilies

            // queue create infos
            let mutable queuePriority = 1.0f
            let queueCreateInfos = Array.zeroCreate<VkDeviceQueueCreateInfo> uniqueQueueFamilies.Length
            use queueCreateInfosPin = new ArrayPin<_> (queueCreateInfos)
            for i in [0 .. dec uniqueQueueFamilies.Length] do
                let mutable info = VkDeviceQueueCreateInfo ()
                info.queueFamilyIndex <- uniqueQueueFamilies.[i]
                info.queueCount <- 1u
                info.pQueuePriorities <- asPointer &queuePriority
                queueCreateInfos[i] <- info

            // get swapchain extension
            let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
            use extensionArrayWrap = new StringArrayWrap ([|swapchainExtensionName|])

            // NOTE: DJL: for particularly dated implementations of Vulkan, validation depends on device layers which
            // are deprecated. These must be enabled if validation support for said implementations is desired.

            // create device
            let mutable info = VkDeviceCreateInfo ()
            info.queueCreateInfoCount <- uint queueCreateInfos.Length
            info.pQueueCreateInfos <- queueCreateInfosPin.Pointer
            info.enabledExtensionCount <- 1u
            info.ppEnabledExtensionNames <- extensionArrayWrap.Pointer
            let mutable device = Unchecked.defaultof<VkDevice>
            Vulkan.vkCreateDevice (physicalDeviceData.PhysicalDevice, &info, nullPtr, &device) |> check
            device

        /// Create the VMA allocator.
        static member private createVmaAllocator physicalDeviceData device instance =
            let mutable info = VmaAllocatorCreateInfo ()
            info.physicalDevice <- physicalDeviceData.PhysicalDevice
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

        /// Get swap extent.
        static member private getSwapExtent (surfaceCapabilities : VkSurfaceCapabilitiesKHR) window =

            // swap extent
            if surfaceCapabilities.currentExtent.width <> UInt32.MaxValue
            then surfaceCapabilities.currentExtent
            else

                // get pixel resolution from sdl
                let mutable width = Unchecked.defaultof<int>
                let mutable height = Unchecked.defaultof<int>
                SDL.SDL_Vulkan_GetDrawableSize (window, &width, &height)

                // clamp resolution to size limits
                width <- max width (int surfaceCapabilities.minImageExtent.width)
                width <- min width (int surfaceCapabilities.maxImageExtent.width)
                height <- max height (int surfaceCapabilities.minImageExtent.height)
                height <- min height (int surfaceCapabilities.maxImageExtent.height)

                // fin
                VkExtent2D (width, height)

        /// Create the swapchain.
        static member private createSwapchain (surfaceFormat : VkSurfaceFormatKHR) swapExtent physicalDeviceData surface device =

            // decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
            // at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
            // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain.
            let capabilities = physicalDeviceData.SurfaceCapabilities
            let minImageCount =
                if capabilities.maxImageCount = 0u
                then capabilities.minImageCount + 1u
                else min (capabilities.minImageCount + 1u) capabilities.maxImageCount

            // in case graphics and present queue families differ
            // TODO: as part of optimization, the sharing mode in this case should probably be VK_SHARING_MODE_EXCLUSIVE (see below).
            let indicesArray = [|physicalDeviceData.GraphicsQueueFamily; physicalDeviceData.PresentQueueFamily|]
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
            if (physicalDeviceData.GraphicsQueueFamily = physicalDeviceData.PresentQueueFamily) then
                info.imageSharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            else
                info.imageSharingMode <- Vulkan.VK_SHARING_MODE_CONCURRENT
                info.queueFamilyIndexCount <- 2u
                info.pQueueFamilyIndices <- indicesArrayPin.Pointer
            info.preTransform <- capabilities.currentTransform
            info.compositeAlpha <- Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
            info.presentMode <- Vulkan.VK_PRESENT_MODE_FIFO_KHR // NOTE: guaranteed by the spec and seems most appropriate for Nu.
            info.clipped <- true
            info.oldSwapchain <- VkSwapchainKHR.Null
            let mutable swapchain = Unchecked.defaultof<VkSwapchainKHR>
            Vulkan.vkCreateSwapchainKHR (device, &info, nullPtr, &swapchain) |> check
            swapchain

        /// Get swapchain images.
        static member private getSwapchainImages swapchain device =
            let mutable imageCount = 0u
            Vulkan.vkGetSwapchainImagesKHR (device, swapchain, asPointer &imageCount, nullPtr) |> check
            let images = Array.zeroCreate<VkImage> (int imageCount)
            use imagesPin = new ArrayPin<_> (images)
            Vulkan.vkGetSwapchainImagesKHR (device, swapchain, asPointer &imageCount, imagesPin.Pointer) |> check
            images

        /// Create swapchain image views.
        static member private createSwapchainImageViews format (images : VkImage array) device =
            let imageViews = Array.zeroCreate<VkImageView> images.Length
            for i in 0 .. dec imageViews.Length do imageViews.[i] <- createImageView format 1u images.[i] device
            imageViews

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
        static member private getQueue queueFamilyIndex device =
            let mutable queue = Unchecked.defaultof<VkQueue>
            Vulkan.vkGetDeviceQueue (device, queueFamilyIndex, 0u, &queue)
            queue

        /// Allocate an array of render command buffers for each frame in flight.
        static member private allocateRenderCommandBuffers commandPool device =
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
        
        /// Create a renderpass.
        static member private createRenderPass clear presentLayout format device =

            // attachment
            let mutable attachment = VkAttachmentDescription ()
            attachment.format <- format
            attachment.samples <- Vulkan.VK_SAMPLE_COUNT_1_BIT
            attachment.loadOp <- if clear then Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR else Vulkan.VK_ATTACHMENT_LOAD_OP_LOAD
            attachment.storeOp <- Vulkan.VK_ATTACHMENT_STORE_OP_STORE
            attachment.stencilLoadOp <- Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
            attachment.stencilStoreOp <- Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
            attachment.initialLayout <- if clear then Vulkan.VK_IMAGE_LAYOUT_UNDEFINED else Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            attachment.finalLayout <- if presentLayout then Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR else Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

            // attachment reference
            let mutable attachmentReference = VkAttachmentReference ()
            attachmentReference.attachment <- 0u
            attachmentReference.layout <- Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

            // subpass
            let mutable subpass = VkSubpassDescription ()
            subpass.pipelineBindPoint <- Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
            subpass.colorAttachmentCount <- 1u
            subpass.pColorAttachments <- asPointer &attachmentReference

            // create renderpass
            let mutable info = VkRenderPassCreateInfo ()
            info.attachmentCount <- 1u
            info.pAttachments <- asPointer &attachment
            info.subpassCount <- 1u
            info.pSubpasses <- asPointer &subpass
            let mutable renderPass = Unchecked.defaultof<VkRenderPass>
            Vulkan.vkCreateRenderPass (device, &info, nullPtr, &renderPass) |> check
            renderPass

        /// Create the swapchain framebuffers.
        static member private createSwapchainFramebuffers (extent : VkExtent2D) renderPass (imageViews : VkImageView array) device =

            // handle array
            let framebuffers = Array.zeroCreate<VkFramebuffer> imageViews.Length

            // create framebuffers
            for i in 0 .. dec framebuffers.Length do
                let mutable imageView = imageViews.[i]
                let mutable info = VkFramebufferCreateInfo ()
                info.renderPass <- renderPass
                info.attachmentCount <- 1u
                info.pAttachments <- asPointer &imageView
                info.width <- extent.width
                info.height <- extent.height
                info.layers <- 1u
                Vulkan.vkCreateFramebuffer (device, &info, nullPtr, &framebuffers.[i]) |> check
            
            // fin
            framebuffers

        /// Begin the frame.
        static member beginFrame (vkg : VulkanGlobal) =

            // wait for previous cycle to finish
            awaitFence vkg.InFlightFence vkg.Device

            // acquire image from swapchain to draw onto
            Vulkan.vkAcquireNextImageKHR (vkg.Device, vkg.Swapchain, UInt64.MaxValue, vkg.ImageAvailableSemaphore, VkFence.Null, &ImageIndex) |> check

            // the *simple* solution: https://vulkan-tutorial.com/Drawing_a_triangle/Drawing/Rendering_and_presentation#page_Subpass-dependencies
            let waitStage = Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT

            // clear screen
            // TODO: DJL: clear viewport as well, as applicable.
            let renderArea = VkRect2D (VkOffset2D.Zero, vkg.SwapExtent)
            let clearColor = VkClearValue (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
            beginRenderBlock vkg.RenderCommandBuffer vkg.ClearRenderPass vkg.SwapchainFramebuffer renderArea [|clearColor|] VkFence.Null vkg.Device
            endRenderBlock vkg.RenderCommandBuffer vkg.GraphicsQueue [|vkg.ImageAvailableSemaphore, waitStage|] [||] vkg.InFlightFence

        /// End the frame.
        static member endFrame () =
            () // nothing to do

        /// Present the image back to the swapchain to appear on screen.
        static member present (vkg : VulkanGlobal) =

            // transition image layout for presentation
            let mutable renderFinished = vkg.RenderFinishedSemaphore
            let renderArea = VkRect2D (VkOffset2D.Zero, vkg.SwapExtent)
            beginRenderBlock vkg.RenderCommandBuffer vkg.PresentRenderPass vkg.SwapchainFramebuffer renderArea [||] vkg.InFlightFence vkg.Device
            endRenderBlock vkg.RenderCommandBuffer vkg.GraphicsQueue [||] [|renderFinished|] vkg.InFlightFence
            
            // present image
            let mutable swapchain = vkg.Swapchain
            let mutable info = VkPresentInfoKHR ()
            info.waitSemaphoreCount <- 1u
            info.pWaitSemaphores <- asPointer &renderFinished
            info.swapchainCount <- 1u
            info.pSwapchains <- asPointer &swapchain
            info.pImageIndices <- asPointer &ImageIndex
            Vulkan.vkQueuePresentKHR (vkg.PresentQueue, asPointer &info) |> check

        /// Wait for all device operations to complete before cleaning up resources.
        static member waitIdle vkg =
            Vulkan.vkDeviceWaitIdle vkg.Device |> check

        /// Destroy the Vulkan handles.
        static member cleanup vkg =
            for i in 0 .. dec vkg.SwapchainFramebuffers.Length do Vulkan.vkDestroyFramebuffer (vkg.Device, vkg.SwapchainFramebuffers.[i], nullPtr)
            Vulkan.vkDestroyRenderPass (vkg.Device, vkg.RenderPass, nullPtr)
            Vulkan.vkDestroyRenderPass (vkg.Device, vkg.ClearRenderPass, nullPtr)
            Vulkan.vkDestroyRenderPass (vkg.Device, vkg.PresentRenderPass, nullPtr)
            for i in 0 .. dec vkg.ImageAvailableSemaphores.Length do Vulkan.vkDestroySemaphore (vkg.Device, vkg.ImageAvailableSemaphores.[i], nullPtr)
            for i in 0 .. dec vkg.RenderFinishedSemaphores.Length do Vulkan.vkDestroySemaphore (vkg.Device, vkg.RenderFinishedSemaphores.[i], nullPtr)
            for i in 0 .. dec vkg.InFlightFences.Length do Vulkan.vkDestroyFence (vkg.Device, vkg.InFlightFences.[i], nullPtr)
            Vulkan.vkDestroyFence (vkg.Device, vkg.ResourceReadyFence, nullPtr)
            Vulkan.vkDestroyCommandPool (vkg.Device, vkg.RenderCommandPool, nullPtr)
            Vulkan.vkDestroyCommandPool (vkg.Device, vkg.TransferCommandPool, nullPtr)
            for i in 0 .. dec vkg.SwapchainImageViews.Length do Vulkan.vkDestroyImageView (vkg.Device, vkg.SwapchainImageViews.[i], nullPtr)
            Vulkan.vkDestroySwapchainKHR (vkg.Device, vkg.Swapchain, nullPtr)
            Vma.vmaDestroyAllocator vkg.VmaAllocator
            Vulkan.vkDestroyDevice (vkg.Device, nullPtr)
            Vulkan.vkDestroySurfaceKHR (vkg.Instance, vkg.Surface, nullPtr)
            Vulkan.vkDestroyInstance (vkg.Instance, nullPtr)

        /// Attempt to create a VulkanGlobal.
        static member tryCreate window =

            // load vulkan; not vulkan function
            Vulkan.vkInitialize () |> check

            // create instance
            let instance = VulkanGlobal.createVulkanInstance window

            // load instance commands; not vulkan function
            Vulkan.vkLoadInstanceOnly instance

            // create surface
            let surface = VulkanGlobal.createVulkanSurface window instance

            // attempt to select physical device
            match VulkanGlobal.trySelectPhysicalDevice surface instance with
            | Some physicalDeviceData ->

                // create device
                let device = VulkanGlobal.createLogicalDevice physicalDeviceData

                // load device commands; not vulkan function
                Vulkan.vkLoadDevice device

                // create vma allocator
                let allocator = VulkanGlobal.createVmaAllocator physicalDeviceData device instance

                // get surface format and swap extent
                let surfaceFormat = VulkanGlobal.getSurfaceFormat physicalDeviceData.Formats
                let swapExtent = VulkanGlobal.getSwapExtent physicalDeviceData.SurfaceCapabilities window

                // setup swapchain and its assets
                let swapchain = VulkanGlobal.createSwapchain surfaceFormat swapExtent physicalDeviceData surface device
                let swapchainImages = VulkanGlobal.getSwapchainImages swapchain device
                let swapchainImageViews = VulkanGlobal.createSwapchainImageViews surfaceFormat.format swapchainImages device

                // setup command system
                let transferCommandPool = VulkanGlobal.createCommandPool true physicalDeviceData.GraphicsQueueFamily device
                let renderCommandPool = VulkanGlobal.createCommandPool false physicalDeviceData.GraphicsQueueFamily device
                let renderCommandBuffers = VulkanGlobal.allocateRenderCommandBuffers renderCommandPool device
                let graphicsQueue = VulkanGlobal.getQueue physicalDeviceData.GraphicsQueueFamily device
                let presentQueue = VulkanGlobal.getQueue physicalDeviceData.PresentQueueFamily device

                // create sync objects
                let imageAvailableSemaphores = VulkanGlobal.createSemaphores device
                let renderFinishedSemaphores = VulkanGlobal.createSemaphores device
                let inFlightFences = VulkanGlobal.createFences device
                let resourceReadyFence = createFence false device

                // render actual content; clear render area; transition layout for presentation
                let renderPass = VulkanGlobal.createRenderPass false false surfaceFormat.format device
                let clearRenderPass = VulkanGlobal.createRenderPass true false surfaceFormat.format device
                let presentRenderPass = VulkanGlobal.createRenderPass false true surfaceFormat.format device

                // create swapchain framebuffers
                let swapchainFramebuffers = VulkanGlobal.createSwapchainFramebuffers swapExtent renderPass swapchainImageViews device

                // make VulkanGlobal
                let vulkanGlobal =
                    { Instance = instance
                      Surface = surface
                      Device = device
                      VmaAllocator = allocator
                      Swapchain = swapchain
                      SwapchainImageViews = swapchainImageViews
                      TransferCommandPool = transferCommandPool
                      RenderCommandPool = renderCommandPool
                      RenderCommandBuffers = renderCommandBuffers
                      GraphicsQueue = graphicsQueue
                      PresentQueue = presentQueue
                      ImageAvailableSemaphores = imageAvailableSemaphores
                      RenderFinishedSemaphores = renderFinishedSemaphores
                      InFlightFences = inFlightFences
                      ResourceReadyFence = resourceReadyFence
                      RenderPass = renderPass
                      ClearRenderPass = clearRenderPass
                      PresentRenderPass = presentRenderPass
                      SwapchainFramebuffers = swapchainFramebuffers
                      SwapExtent = swapExtent }

                // fin
                Some vulkanGlobal

            // failure
            | None -> None

    /// Abstraction for vma allocated buffer.
    type AllocatedBuffer =
        { Buffer : VkBuffer
          Allocation : VmaAllocation
          AllocationInfo : VmaAllocationInfo
          UploadEnabled : bool }

        static member private createInternal uploadEnabled bufferInfo allocator =

            // allocation create info
            let mutable info = VmaAllocationCreateInfo ()
            info.usage <- VmaMemoryUsage.Auto
            if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

            // create vma buffer
            let mutable buffer = Unchecked.defaultof<VkBuffer>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            let mutable allocationInfo = Unchecked.defaultof<VmaAllocationInfo>
            Vma.vmaCreateBuffer (allocator, &bufferInfo, &info, &buffer, &allocation, asPointer &allocationInfo) |> check

            // make AllocatedBuffer
            let allocatedBuffer =
                { Buffer = buffer
                  Allocation = allocation
                  AllocationInfo = allocationInfo
                  UploadEnabled = uploadEnabled }

            // fin
            allocatedBuffer

        /// Copy data from the source buffer to the destination buffer.
        static member private copyData size source destination vkg =

            // create command buffer for transfer
            let mutable cb = allocateCommandBuffer vkg.TransferCommandPool vkg.Device

            // reset command buffer and begin recording
            Vulkan.vkResetCommandPool (vkg.Device, vkg.TransferCommandPool, VkCommandPoolResetFlags.None) |> check
            let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
            Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> check

            // copy data
            let mutable region = VkBufferCopy (size = uint64 size)
            Vulkan.vkCmdCopyBuffer (cb, source.Buffer, destination.Buffer, 1u, asPointer &region)

            // execute command
            Vulkan.vkEndCommandBuffer cb |> check
            let mutable sInfo = VkSubmitInfo ()
            sInfo.commandBufferCount <- 1u
            sInfo.pCommandBuffers <- asPointer &cb
            Vulkan.vkQueueSubmit (vkg.GraphicsQueue, 1u, asPointer &sInfo, vkg.ResourceReadyFence) |> check
            awaitFence vkg.ResourceReadyFence vkg.Device

        (*
        TODO: DJL: *maybe* try and get vmaMapMemory by way of the vortice wrapper fixed to enable these methods.

        /// Map pointer to buffer if upload is enabled.
        static member tryMap buffer =
            let mutable memoryPtrPtr = Unchecked.defaultof<nativeptr<voidptr>>
            if buffer.UploadEnabled then
                Vma.vmaMapMemory (buffer.VmaAllocator, buffer.VmaAllocation, memoryPtrPtr) |> check
                Vma.vmaInvalidateAllocation (buffer.VmaAllocator, buffer.VmaAllocation, 0UL, Vulkan.VK_WHOLE_SIZE) |> check
            else Log.info "Mapping to Vulkan buffer failed because upload was not enabled for that buffer."
            memoryPtrPtr

        /// Unmap buffer.
        static member unmap buffer =

            // no point checking UploadEnabled because success or failure simply depends on calling context
            Vma.vmaFlushAllocation (buffer.VmaAllocator, buffer.VmaAllocation, 0UL, Vulkan.VK_WHOLE_SIZE) |> check
            Vma.vmaUnmapMemory (buffer.VmaAllocator, buffer.VmaAllocation)
        *)

        /// Upload data to buffer if upload is enabled.
        static member upload offset size ptr buffer =
            if buffer.UploadEnabled
            then NativePtr.memCopy offset size (NativePtr.nativeintToVoidPtr ptr) buffer.AllocationInfo.pMappedData
            else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

        /// Upload an array to buffer if upload is enabled.
        static member uploadArray offset (array : 'a array) buffer =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.upload offset size arrayPin.NativeInt buffer

        /// Create an allocated staging buffer.
        static member createStaging size allocator =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal true info allocator
            allocatedBuffer

        /// Create an allocated vertex buffer.
        static member createVertex uploadEnabled size allocator =

            // data can and must be transferred from a staging buffer if upload is not enabled
            let usage =
                if uploadEnabled
                then Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
                else Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT

            // create buffer
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info allocator
            allocatedBuffer

        /// Create an allocated index buffer.
        static member createIndex uploadEnabled size allocator =

            // data can and must be transferred from a staging buffer if upload is not enabled
            let usage =
                if uploadEnabled
                then Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
                else Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT

            // create buffer
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info allocator
            allocatedBuffer

        /// Create an allocated uniform buffer.
        static member createUniform size allocator =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal true info allocator
            allocatedBuffer

        /// Create an allocated staging buffer and stage the data.
        static member stageData size ptr allocator =
            let buffer = AllocatedBuffer.createStaging size allocator
            AllocatedBuffer.upload 0 size ptr buffer
            buffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer.
        static member createVertexStaged size ptr vkg =
            let stagingBuffer = AllocatedBuffer.stageData size ptr vkg.VmaAllocator
            let vertexBuffer = AllocatedBuffer.createVertex false size vkg.VmaAllocator
            AllocatedBuffer.copyData size stagingBuffer vertexBuffer vkg
            AllocatedBuffer.destroy stagingBuffer vkg.VmaAllocator
            vertexBuffer

        /// Create an allocated index buffer with data uploaded via staging buffer.
        static member createIndexStaged size ptr vkg =
            let stagingBuffer = AllocatedBuffer.stageData size ptr vkg.VmaAllocator
            let indexBuffer = AllocatedBuffer.createIndex false size vkg.VmaAllocator
            AllocatedBuffer.copyData size stagingBuffer indexBuffer vkg
            AllocatedBuffer.destroy stagingBuffer vkg.VmaAllocator
            indexBuffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer from an array.
        static member createVertexStagedFromArray (array : 'a array) vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.createVertexStaged size arrayPin.NativeInt vkg

        /// Create an allocated index buffer with data uploaded via staging buffer from an array.
        static member createIndexStagedFromArray (array : 'a array) vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.createIndexStaged size arrayPin.NativeInt vkg
        
        /// Destroy buffer and allocation.
        static member destroy buffer allocator =
            Vma.vmaDestroyBuffer (allocator, buffer.Buffer, buffer.Allocation)

    /// Abstraction for vma allocated image.
    type AllocatedImage =
        { Image : VkImage
          Allocation : VmaAllocation }

        /// Destroy image and allocation.
        static member destroy allocatedImage allocator =
            Vma.vmaDestroyImage (allocator, allocatedImage.Image, allocatedImage.Allocation)

        /// Create an AllocatedImage.
        static member create imageInfo allocator =
            let info = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (allocator, &imageInfo, &info, &image, &allocation, nullPtr) |> check
            let allocatedImage = { Image = image; Allocation = allocation }
            allocatedImage