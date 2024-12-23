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

    // enable validation layers in debug mode
    let private ValidationLayersEnabled =
#if DEBUG
        true
#else
        false
#endif
    
    /// Convert VkExtensionProperties.extensionName to a string.
    let private getExtensionName (extensionProps : VkExtensionProperties) =
        NativePtr.fixedBufferToString extensionProps.extensionName
    
    /// Convert VkLayerProperties.layerName to a string.
    let private getLayerName (layerProps : VkLayerProperties) =
        NativePtr.fixedBufferToString layerProps.layerName
    
    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan assertion failed due to: " + string result)

    /// Compile GLSL file to SPIR-V code.
    let compileShader shaderPath shaderKind =
        use shaderStream = new StreamReader (File.OpenRead shaderPath)
        let shaderStr = shaderStream.ReadToEnd ()
        use compiler = new Compiler ()
        use result = compiler.Compile (shaderStr, shaderPath, shaderKind)
        if result.Status <> CompilationStatus.Success then failwith ("Vulkan shader compilation failed due to: " + result.ErrorMessage)
        let shaderCode = result.GetBytecode().ToArray()
        shaderCode
    
    /// Create a shader module from a GLSL file.
    let createShaderModuleFromGlsl shaderPath shaderKind device =
        
        // handle and shader
        let mutable shaderModule = Unchecked.defaultof<VkShaderModule>
        let shader = compileShader shaderPath shaderKind

        // NOTE: using a high level overload here to avoid questions about reinterpret casting and memory alignment,
        // see https://vulkan-tutorial.com/Drawing_a_triangle/Graphics_pipeline_basics/Shader_modules#page_Creating-shader-modules.
        Vulkan.vkCreateShaderModule (device, shader, nullPtr, &shaderModule) |> check
        shaderModule
    
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
    
    /// Create an image view.
    let createImageView format mips image device =
        let mutable imageView = Unchecked.defaultof<VkImageView>
        let mutable info = VkImageViewCreateInfo ()
        info.image <- image
        info.viewType <- Vulkan.VK_IMAGE_VIEW_TYPE_2D
        info.format <- format
        info.subresourceRange <- makeSubresourceRangeColor mips
        Vulkan.vkCreateImageView (device, &info, nullPtr, &imageView) |> check
        imageView
    
    /// Allocate a command buffer.
    let allocateCommandBuffer commandPool device =
        let mutable commandBuffer = Unchecked.defaultof<VkCommandBuffer>
        let mutable info = VkCommandBufferAllocateInfo ()
        info.commandPool <- commandPool
        info.level <- Vulkan.VK_COMMAND_BUFFER_LEVEL_PRIMARY
        info.commandBufferCount <- 1u
        Vulkan.vkAllocateCommandBuffers (device, asPointer &info, asPointer &commandBuffer) |> check
        commandBuffer
    
    /// Abstraction for vma allocated buffer.
    type AllocatedBuffer =
        { Buffer : VkBuffer
          Allocation : VmaAllocation
          UploadEnabled : bool }

        static member private createInternal uploadEnabled bufferInfo allocator =
            
            // handles
            let mutable buffer = Unchecked.defaultof<VkBuffer>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>

            // allocation create info
            let mutable allocInfo = VmaAllocationCreateInfo ()
            allocInfo.usage <- VmaMemoryUsage.Auto
            if uploadEnabled then allocInfo.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite

            // create vma buffer
            Vma.vmaCreateBuffer (allocator, &bufferInfo, &allocInfo, &buffer, &allocation, nullPtr) |> check

            // make allocatedBuffer
            let allocatedBuffer =
                { Buffer = buffer
                  Allocation = allocation
                  UploadEnabled = uploadEnabled }

            // fin
            allocatedBuffer

        (*
        TODO: *maybe* try and get vmaMapMemory fixed to enable these methods.

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
        static member upload offset size ptr buffer allocator =
            if buffer.UploadEnabled
            then Vma.vmaCopyMemoryToAllocation (allocator, NativePtr.nativeintToVoidPtr ptr, buffer.Allocation, uint64 offset, uint64 size) |> check
            else failwith "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

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
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info allocator
            allocatedBuffer

        /// Create an allocated index buffer.
        static member createIndex uploadEnabled size allocator =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info allocator
            allocatedBuffer
        
        /// Create an allocated staging buffer and stage the data.
        static member stageData size ptr allocator =
            let buffer = AllocatedBuffer.createStaging size allocator
            AllocatedBuffer.upload 0 size ptr buffer allocator
            buffer
        
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
            
            // handles
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>

            // allocation create info
            let allocInfo = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)

            // create vma image
            Vma.vmaCreateImage (allocator, &imageInfo, &allocInfo, &image, &allocation, nullPtr) |> check

            // fin
            let allocatedImage = { Image = image; Allocation = allocation }
            allocatedImage
    
    /// A physical device and associated data.
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

            // NOTE: It is *essential* to use the *first* compatible queue families in the array, *not* the last, as per the tutorial and vortice vulkan sample.
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
    type [<ReferenceEquality>] VulkanGlobal =
        { Instance : VkInstance
          Surface : VkSurfaceKHR
          Device : VkDevice
          VmaAllocator : VmaAllocator
          Swapchain : VkSwapchainKHR
          SwapchainImageViews : VkImageView array
          TransferCommandPool : VkCommandPool
          RenderCommandPool : VkCommandPool
          RenderCommandBuffer : VkCommandBuffer
          GraphicsQueue : VkQueue
          PresentQueue : VkQueue
          ImageAvailableSemaphore : VkSemaphore
          RenderFinishedSemaphore : VkSemaphore
          InFlightFence : VkFence
          RenderPass : VkRenderPass
          SwapchainFramebuffers : VkFramebuffer array
          SwapExtent : VkExtent2D }

        /// Create the Vulkan instance.
        static member private createVulkanInstance window =

            // handle
            let mutable instance = Unchecked.defaultof<VkInstance>

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
            
            // TODO: setup message callback with debug utils *if* motivation arises.
            
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
            
            // TODO: apply VkApplicationInfo once all compulsory fields have been decided (e.g. engineVersion)
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

            // compatibility criteria: device must support essential rendering components and Vulkan 1.3
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

            // handle
            let mutable device = Unchecked.defaultof<VkDevice>

            // get unique queue family array
            let uniqueQueueFamiliesSet = new HashSet<uint> ()
            uniqueQueueFamiliesSet.Add physicalDeviceData.GraphicsQueueFamily |> ignore
            uniqueQueueFamiliesSet.Add physicalDeviceData.PresentQueueFamily |> ignore
            let uniqueQueueFamilies = Array.zeroCreate<uint> uniqueQueueFamiliesSet.Count
            uniqueQueueFamiliesSet.CopyTo (uniqueQueueFamilies)

            // queue create infos
            let mutable queuePriority = 1.0f
            let queueCreateInfos = Array.zeroCreate<VkDeviceQueueCreateInfo> uniqueQueueFamilies.Length
            use queueCreateInfosPin = new ArrayPin<_> (queueCreateInfos)
            for i in [0 .. dec (uniqueQueueFamilies.Length)] do
                let mutable info = VkDeviceQueueCreateInfo ()
                info.queueFamilyIndex <- uniqueQueueFamilies.[i]
                info.queueCount <- 1u
                info.pQueuePriorities <- asPointer &queuePriority
                queueCreateInfos[i] <- info

            // get swapchain extension
            let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
            use extensionArrayWrap = new StringArrayWrap ([|swapchainExtensionName|])

            // NOTE: for particularly dated implementations of Vulkan, validation depends on device layers which are
            // deprecated. These must be enabled if validation support for said implementations is desired.
            
            // create device
            let mutable info = VkDeviceCreateInfo ()
            info.queueCreateInfoCount <- uint queueCreateInfos.Length
            info.pQueueCreateInfos <- queueCreateInfosPin.Pointer
            info.enabledExtensionCount <- 1u
            info.ppEnabledExtensionNames <- extensionArrayWrap.Pointer
            Vulkan.vkCreateDevice (physicalDeviceData.PhysicalDevice, &info, nullPtr, &device) |> check
            device

        /// Create the VMA allocator.
        static member private createVmaAllocator physicalDeviceData device instance =
            let mutable allocator = Unchecked.defaultof<VmaAllocator>
            let mutable info = VmaAllocatorCreateInfo ()
            info.physicalDevice <- physicalDeviceData.PhysicalDevice
            info.device <- device
            info.instance <- instance
            Vma.vmaCreateAllocator (&info, &allocator) |> check
            allocator
        
        /// Get surface format.
        static member private getSurfaceFormat formats =
            
            // specify preferred format and color space
            let isPreferred (format : VkSurfaceFormatKHR) =
                format.format = Vulkan.VK_FORMAT_B8G8R8A8_SRGB &&
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
            
            // handles
            let mutable swapchain = Unchecked.defaultof<VkSwapchainKHR>
            let capabilities = physicalDeviceData.SurfaceCapabilities
            
            // present mode; VK_PRESENT_MODE_FIFO_KHR is guaranteed by the spec and seems most appropriate for nu
            let presentMode = Vulkan.VK_PRESENT_MODE_FIFO_KHR

            // decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
            // at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
            // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain.
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
            info.presentMode <- presentMode
            info.clipped <- true
            info.oldSwapchain <- VkSwapchainKHR.Null
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
            
            // handle
            let mutable commandPool = Unchecked.defaultof<VkCommandPool>
            
            // apply transient flag if desired
            let flags =
                if transient
                then Vulkan.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT ||| Vulkan.VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
                else Vulkan.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            
            // create command pool
            let mutable info = VkCommandPoolCreateInfo ()
            info.flags <- flags
            info.queueFamilyIndex <- queueFamilyIndex
            Vulkan.vkCreateCommandPool (device, &info, nullPtr, &commandPool) |> check
            commandPool

        /// Get command queue.
        static member private getQueue queueFamilyIndex device =
            let mutable queue = Unchecked.defaultof<VkQueue>
            Vulkan.vkGetDeviceQueue (device, queueFamilyIndex, 0u, &queue)
            queue
        
        /// Create a semaphore.
        static member private createSemaphore device =
            let mutable semaphore = Unchecked.defaultof<VkSemaphore>
            let info = VkSemaphoreCreateInfo ()
            Vulkan.vkCreateSemaphore (device, &info, nullPtr, &semaphore) |> check
            semaphore
        
        /// Create a fence.
        static member private createFence device =
            let mutable fence = Unchecked.defaultof<VkFence>
            let info = VkFenceCreateInfo (flags = Vulkan.VK_FENCE_CREATE_SIGNALED_BIT)
            Vulkan.vkCreateFence (device, &info, nullPtr, &fence) |> check
            fence
        
        /// Create a renderpass.
        static member private createRenderPass clearScreen presentLayout format device =
            
            // handle
            let mutable renderPass = Unchecked.defaultof<VkRenderPass>
            
            // attachment
            let mutable attachment = VkAttachmentDescription ()
            attachment.format <- format
            attachment.samples <- Vulkan.VK_SAMPLE_COUNT_1_BIT
            attachment.loadOp <- if clearScreen then Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR else Vulkan.VK_ATTACHMENT_LOAD_OP_LOAD
            attachment.storeOp <- Vulkan.VK_ATTACHMENT_STORE_OP_STORE
            attachment.stencilLoadOp <- Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
            attachment.stencilStoreOp <- Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
            attachment.initialLayout <- if clearScreen then Vulkan.VK_IMAGE_LAYOUT_UNDEFINED else Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
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
        
        /// Begin the frame (clearing the screen in the process).
        static member beginFrame vulkanGlobal =

            // swapchain image index and handles
            let mutable imageIndex = 0u
            let device = vulkanGlobal.Device
            let swapchain = vulkanGlobal.Swapchain
            let commandBuffer = vulkanGlobal.RenderCommandBuffer
            let imageAvailable = vulkanGlobal.ImageAvailableSemaphore
            let mutable inFlight = vulkanGlobal.InFlightFence

            // wait for previous cycle to finish
            Vulkan.vkWaitForFences (device, 1u, asPointer &inFlight, VkBool32.True, UInt64.MaxValue) |> check
            Vulkan.vkResetFences (device, 1u, asPointer &inFlight) |> check

            // acquire image from swapchain to draw onto
            Vulkan.vkAcquireNextImageKHR (device, swapchain, UInt64.MaxValue, imageAvailable, VkFence.Null, &imageIndex) |> check

            // reset command buffer and begin recording
            Vulkan.vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None) |> check
            let mutable cbInfo = VkCommandBufferBeginInfo ()
            Vulkan.vkBeginCommandBuffer (commandBuffer, asPointer &cbInfo) |> check

            // set color for screen clear
            // TODO: P0: change to proper color once the testing utility of white is no longer needed.
            let mutable clearColor = VkClearValue (1.0f, 1.0f, 1.0f, 1.0f)

            // begin render pass
            let mutable rpInfo = VkRenderPassBeginInfo ()
            rpInfo.renderPass <- vulkanGlobal.RenderPass
            rpInfo.framebuffer <- vulkanGlobal.SwapchainFramebuffers[int imageIndex]
            rpInfo.renderArea <- VkRect2D (VkOffset2D.Zero, vulkanGlobal.SwapExtent)
            rpInfo.clearValueCount <- 1u
            rpInfo.pClearValues <- asPointer &clearColor
            Vulkan.vkCmdBeginRenderPass (commandBuffer, asPointer &rpInfo, Vulkan.VK_SUBPASS_CONTENTS_INLINE)

            // fin
            imageIndex

        /// End the frame.
        static member endFrame vulkanGlobal =
            
            // handles
            let mutable commandBuffer = vulkanGlobal.RenderCommandBuffer
            let mutable imageAvailable = vulkanGlobal.ImageAvailableSemaphore
            let mutable renderFinished = vulkanGlobal.RenderFinishedSemaphore

            // end render pass
            Vulkan.vkCmdEndRenderPass commandBuffer
            
            // end command buffer recording
            Vulkan.vkEndCommandBuffer commandBuffer |> check
            
            // submit commands
            let mutable waitStage = Vulkan.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT // the *simple* solution: https://vulkan-tutorial.com/Drawing_a_triangle/Drawing/Rendering_and_presentation#page_Subpass-dependencies
            let mutable info = VkSubmitInfo ()
            info.waitSemaphoreCount <- 1u
            info.pWaitSemaphores <- asPointer &imageAvailable
            info.pWaitDstStageMask <- asPointer &waitStage
            info.commandBufferCount <- 1u
            info.pCommandBuffers <- asPointer &commandBuffer
            info.signalSemaphoreCount <- 1u
            info.pSignalSemaphores <- asPointer &renderFinished
            Vulkan.vkQueueSubmit (vulkanGlobal.GraphicsQueue, 1u, asPointer &info, vulkanGlobal.InFlightFence) |> check

        /// Present the image back to the swapchain to appear on screen.
        static member present imageIndex vulkanGlobal =

            // swapchain image index and handles
            let mutable imageIndex = imageIndex
            let mutable swapchain = vulkanGlobal.Swapchain
            let mutable renderFinished = vulkanGlobal.RenderFinishedSemaphore
            
            // present image
            let mutable info = VkPresentInfoKHR ()
            info.waitSemaphoreCount <- 1u
            info.pWaitSemaphores <- asPointer &renderFinished
            info.swapchainCount <- 1u
            info.pSwapchains <- asPointer &swapchain
            info.pImageIndices <- asPointer &imageIndex
            Vulkan.vkQueuePresentKHR (vulkanGlobal.PresentQueue, asPointer &info) |> check
        
        /// Wait for all device operations to complete before cleaning up resources.
        static member waitIdle vulkanGlobal =
            Vulkan.vkDeviceWaitIdle vulkanGlobal.Device |> check
        
        /// Destroy Vulkan handles.
        static member cleanup vulkanGlobal =
            
            // commonly used handles
            let instance = vulkanGlobal.Instance
            let device = vulkanGlobal.Device
            let framebuffers = vulkanGlobal.SwapchainFramebuffers
            let imageViews = vulkanGlobal.SwapchainImageViews
            
            //
            for i in 0 .. dec framebuffers.Length do Vulkan.vkDestroyFramebuffer (device, framebuffers.[i], nullPtr)
            Vulkan.vkDestroyRenderPass (device, vulkanGlobal.RenderPass, nullPtr)
            Vulkan.vkDestroyFence (device, vulkanGlobal.InFlightFence, nullPtr)
            Vulkan.vkDestroySemaphore (device, vulkanGlobal.RenderFinishedSemaphore, nullPtr)
            Vulkan.vkDestroySemaphore (device, vulkanGlobal.ImageAvailableSemaphore, nullPtr)
            Vulkan.vkDestroyCommandPool (device, vulkanGlobal.RenderCommandPool, nullPtr)
            Vulkan.vkDestroyCommandPool (device, vulkanGlobal.TransferCommandPool, nullPtr)
            for i in 0 .. dec imageViews.Length do Vulkan.vkDestroyImageView (device, imageViews.[i], nullPtr)
            Vulkan.vkDestroySwapchainKHR (device, vulkanGlobal.Swapchain, nullPtr)
            Vma.vmaDestroyAllocator vulkanGlobal.VmaAllocator
            Vulkan.vkDestroyDevice (device, nullPtr)
            Vulkan.vkDestroySurfaceKHR (instance, vulkanGlobal.Surface, nullPtr)
            Vulkan.vkDestroyInstance (instance, nullPtr)

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
                let renderCommandBuffer = allocateCommandBuffer renderCommandPool device
                let graphicsQueue = VulkanGlobal.getQueue physicalDeviceData.GraphicsQueueFamily device
                let presentQueue = VulkanGlobal.getQueue physicalDeviceData.PresentQueueFamily device

                // create sync objects
                let imageAvailableSemaphore = VulkanGlobal.createSemaphore device
                let renderFinishedSemaphore = VulkanGlobal.createSemaphore device
                let inFlightFence = VulkanGlobal.createFence device

                // create render pass
                let renderPass = VulkanGlobal.createRenderPass true true surfaceFormat.format device

                // create swapchain framebuffers
                let swapchainFramebuffers = VulkanGlobal.createSwapchainFramebuffers swapExtent renderPass swapchainImageViews device
                
                // make vulkanGlobal
                let vulkanGlobal =
                    { Instance = instance
                      Surface = surface
                      Device = device
                      VmaAllocator = allocator
                      Swapchain = swapchain
                      SwapchainImageViews = swapchainImageViews
                      TransferCommandPool = transferCommandPool
                      RenderCommandPool = renderCommandPool
                      RenderCommandBuffer = renderCommandBuffer
                      GraphicsQueue = graphicsQueue
                      PresentQueue = presentQueue
                      ImageAvailableSemaphore = imageAvailableSemaphore
                      RenderFinishedSemaphore = renderFinishedSemaphore
                      InFlightFence = inFlightFence
                      RenderPass = renderPass
                      SwapchainFramebuffers = swapchainFramebuffers
                      SwapExtent = swapExtent }

                // fin
                Some vulkanGlobal

            // failure
            | None -> None