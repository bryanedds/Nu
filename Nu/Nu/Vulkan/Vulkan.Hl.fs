// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

// TODO: confirm this module/namespace arrangement is correct.

/// Force qualification of Vulkan namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module Vulkan = let _ = ()

namespace Vulkan
open System
open System.Collections.Generic
open SDL2
open Vortice.Vulkan
open type Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    // enable validation layers in debug mode
#if DEBUG
    let validationLayersEnabled = true
#else
    let validationLayersEnabled = false
#endif
    
    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan error: " + string result)
    
    /// Convert VkExtensionProperties.extensionName to a string.
    let getExtensionName (extensionProps : VkExtensionProperties) =
        getBufferString extensionProps.extensionName
    
    /// Convert VkLayerProperties.layerName to a string.
    let getLayerName (layerProps : VkLayerProperties) =
        getBufferString layerProps.layerName
    
    /// A physical device and associated data.
    type PhysicalDeviceData =
        { PhysicalDevice : VkPhysicalDevice
          Properties : VkPhysicalDeviceProperties
          Extensions : VkExtensionProperties array
          SurfaceCapabilities : VkSurfaceCapabilitiesKHR
          Formats : VkSurfaceFormatKHR array
          GraphicsQueueFamilyOpt : uint option
          PresentQueueFamilyOpt : uint option }

        /// Graphics queue family, whose existence must be established.
        member this.GraphicsQueueFamily = Option.get this.GraphicsQueueFamilyOpt

        /// Present queue family, whose existence must be established.
        member this.PresentQueueFamily = Option.get this.PresentQueueFamilyOpt
        
        /// Get properties.
        static member getProperties physicalDevice =
            let mutable properties = Unchecked.defaultof<VkPhysicalDeviceProperties>
            vkGetPhysicalDeviceProperties (physicalDevice, &properties)
            properties
        
        /// Get available extensions.
        static member getExtensions physicalDevice =
            let mutable extensionCount = 0u
            vkEnumerateDeviceExtensionProperties (physicalDevice, nullPtr, asPointer &extensionCount, nullPtr) |> check
            let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
            use extensionsPin = ArrayPin extensions
            vkEnumerateDeviceExtensionProperties (physicalDevice, nullPtr, asPointer &extensionCount, extensionsPin.Pointer) |> check
            extensions

        /// Get surface capabilities.
        static member getSurfaceCapabilities physicalDevice surface =
            let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
            vkGetPhysicalDeviceSurfaceCapabilitiesKHR (physicalDevice, surface, &capabilities) |> check
            capabilities
        
        /// Get available surface formats.
        static member getFormats physicalDevice surface =
            let mutable formatCount = 0u
            vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice, surface, asPointer &formatCount, nullPtr) |> check
            let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
            use formatsPin = ArrayPin formats
            vkGetPhysicalDeviceSurfaceFormatsKHR (physicalDevice, surface, asPointer &formatCount, formatsPin.Pointer) |> check
            formats
        
        /// Get queue family opts.
        static member getQueueFamilyOpts physicalDevice surface =
            
            // get queue families' properties
            let mutable queueFamilyCount = 0u
            vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, asPointer &queueFamilyCount, nullPtr)
            let queueFamilyProps = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
            use queueFamilyPropsPin = ArrayPin queueFamilyProps
            vkGetPhysicalDeviceQueueFamilyProperties (physicalDevice, asPointer &queueFamilyCount, queueFamilyPropsPin.Pointer)

            (* It is *essential* to use the *first* compatible queue families in the array, *not* the last, as per the tutorial and vortice vulkan sample.
               I discovered this by accident because the queue families on my AMD behaved exactly the same as the queue families on this one:

               https://computergraphics.stackexchange.com/questions/9707/queue-from-a-family-queue-that-supports-presentation-doesnt-work-vulkan

               general lesson: trust level for vendors is too low for deviation from common practices to be advisable. *)
            
            let mutable graphicsQueueFamilyOpt = None
            let mutable presentQueueFamilyOpt = None
            for i in [0 .. dec queueFamilyProps.Length] do
                
                // try get graphics queue family
                match graphicsQueueFamilyOpt with
                | None ->
                    let props = queueFamilyProps[i]
                    if props.queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then
                        graphicsQueueFamilyOpt <- Some (uint i)
                | Some _ -> ()

                // try get present queue family
                match presentQueueFamilyOpt with
                | None ->
                    let mutable presentSupport = VkBool32.False
                    vkGetPhysicalDeviceSurfaceSupportKHR (physicalDevice, uint i, surface, &presentSupport) |> check
                    if (presentSupport = VkBool32.True) then
                        presentQueueFamilyOpt <- Some (uint i)
                | Some _ -> ()

            (graphicsQueueFamilyOpt, presentQueueFamilyOpt)
        
        /// Make PhysicalDeviceData.
        static member make physicalDevice surface =
            
            // get data
            let properties = PhysicalDeviceData.getProperties physicalDevice
            let extensions = PhysicalDeviceData.getExtensions physicalDevice
            let surfaceCapabilities = PhysicalDeviceData.getSurfaceCapabilities physicalDevice surface
            let formats = PhysicalDeviceData.getFormats physicalDevice surface
            let (graphicsQueueFamilyOpt, presentQueueFamilyOpt) = PhysicalDeviceData.getQueueFamilyOpts physicalDevice surface

            // make physicalDeviceData
            let physicalDeviceData =
                { PhysicalDevice = physicalDevice
                  Properties = properties
                  Extensions = extensions
                  SurfaceCapabilities = surfaceCapabilities
                  Formats = formats
                  GraphicsQueueFamilyOpt = graphicsQueueFamilyOpt
                  PresentQueueFamilyOpt = presentQueueFamilyOpt }

            // fin
            physicalDeviceData
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance
              Surface : VkSurfaceKHR
              Device : VkDevice
              Swapchain : VkSwapchainKHR
              SwapchainImageViews : VkImageView array
              CommandPool : VkCommandPool
              CommandBuffer : VkCommandBuffer
              GraphicsQueue : VkQueue
              PresentQueue : VkQueue
              ImageAvailableSemaphore : VkSemaphore
              RenderFinishedSemaphore : VkSemaphore
              InFlightFence : VkFence
              ScreenClearRenderPass : VkRenderPass
              SwapchainFramebuffers : VkFramebuffer array
              SwapExtent : VkExtent2D }

        /// Create the Vulkan instance.
        static member createInstance window =

            // instance handle
            let mutable instance = Unchecked.defaultof<VkInstance>

            // get sdl extensions
            let mutable sdlExtensionCount = 0u
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, null)
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensionsOut = Array.zeroCreate<nativeint> (int sdlExtensionCount)
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, sdlExtensionsOut)
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensions = Array.zeroCreate<nativeptr<byte>> (int sdlExtensionCount)
            for i in [0 .. dec (int sdlExtensionCount)] do sdlExtensions[i] <- nintToBytePointer sdlExtensionsOut[i]
            use sdlExtensionsPin = ArrayPin sdlExtensions
            
            // TODO: setup message callback with debug utils *if* motivation arises.
            
            // get available instance layers
            let mutable layerCount = 0u
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, nullPtr) |> check
            let layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
            use layersPin = ArrayPin layers
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, layersPin.Pointer) |> check

            // check if validation layer exists
            let validationLayer = "VK_LAYER_KHRONOS_validation"
            let validationLayerExists = Array.exists (fun x -> getLayerName x = validationLayer) layers
            if validationLayersEnabled && not validationLayerExists then Log.info (validationLayer + " is not available. Vulkan programmers must install the Vulkan SDK to enable validation.")
            
            // TODO: apply VkApplicationInfo once all compulsory fields have been decided (e.g. engineVersion)
            // and check for available vulkan version as described in 
            // https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap4.html#VkApplicationInfo.

            // populate createinstance info
            let mutable createInfo = VkInstanceCreateInfo ()
            createInfo.enabledExtensionCount <- sdlExtensionCount
            createInfo.ppEnabledExtensionNames <- sdlExtensionsPin.Pointer

            // must be assigned outside conditional to remain in scope until vkCreateInstance
            use layerWrap = StringArrayWrap [|validationLayer|]
            
            // load validation layer if enabled and available
            if validationLayersEnabled && validationLayerExists then
                createInfo.enabledLayerCount <- 1u
                createInfo.ppEnabledLayerNames <- layerWrap.Pointer
            else
                createInfo.enabledLayerCount <- 0u

            // create instance
            vkCreateInstance (&createInfo, nullPtr, &instance) |> check

            // fin
            instance
        
        /// Create surface.
        static member createSurface window instance =

            // surface handle
            let mutable surface = Unchecked.defaultof<VkSurfaceKHR>

            // get surface from sdl
            let result = SDL.SDL_Vulkan_CreateSurface (window, instance, &(asRefType<VkSurfaceKHR, uint64> &surface))
            if int result = 0 then Log.error "SDL error, SDL_Vulkan_CreateSurface failed."

            // fin
            surface
        
        /// Select compatible physical device if available.
        static member trySelectPhysicalDevice surface instance =
            
            // get available physical devices
            let mutable deviceCount = 0u
            vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, nullPtr) |> check
            let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
            use devicesPin = ArrayPin devices
            vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, devicesPin.Pointer) |> check

            // gather devices together with relevant data for selection
            let candidates = [ for i in [0 .. dec devices.Length] -> PhysicalDeviceData.make devices[i] surface ]

            // compatibility criteria: device must support essential rendering components and Vulkan 1.3
            let isCompatible physicalDeviceData =
                
                // determine swapchain support
                let swapchainExtensionName = spanToString VK_KHR_SWAPCHAIN_EXTENSION_NAME
                let swapchainSupported = Array.exists (fun x -> getExtensionName x = swapchainExtensionName) physicalDeviceData.Extensions
                
                // checklist
                swapchainSupported &&
                physicalDeviceData.Formats.Length > 0 &&
                Option.isSome physicalDeviceData.GraphicsQueueFamilyOpt &&
                Option.isSome physicalDeviceData.PresentQueueFamilyOpt &&
                physicalDeviceData.Properties.apiVersion.Minor >= 3u

            // preferability criteria: device ought to be discrete
            let isPreferable physicalDeviceData = physicalDeviceData.Properties.deviceType = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
            
            // filter and order candidates according to criteria
            let candidatesFiltered = List.filter isCompatible candidates
            let (fstChoice, sndChoice) = List.partition isPreferable candidatesFiltered
            let candidatesFilteredAndOrdered = List.append fstChoice sndChoice
                
            // if compatible devices exist then return the first along with its data
            let physicalDeviceOpt =
                if candidatesFilteredAndOrdered.Length > 0 then Some (List.head candidatesFilteredAndOrdered)
                else
                    Log.info "Could not find a suitable graphics device for Vulkan."
                    None

            // fin
            physicalDeviceOpt
        
        /// Create the logical device.
        static member createDevice (physicalDeviceData : PhysicalDeviceData) =

            // device handle
            let mutable device = Unchecked.defaultof<VkDevice>

            // get unique queue family array
            let uniqueQueueFamiliesSet = new HashSet<uint> ()
            uniqueQueueFamiliesSet.Add physicalDeviceData.GraphicsQueueFamily |> ignore
            uniqueQueueFamiliesSet.Add physicalDeviceData.PresentQueueFamily |> ignore
            let uniqueQueueFamilies = Array.zeroCreate<uint> uniqueQueueFamiliesSet.Count
            uniqueQueueFamiliesSet.CopyTo (uniqueQueueFamilies)

            // populate queue create infos
            let mutable queuePriority = 1.0f
            let queueCreateInfos = Array.zeroCreate<VkDeviceQueueCreateInfo> uniqueQueueFamilies.Length
            use queueCreateInfosPin = ArrayPin queueCreateInfos
            for i in [0 .. dec (uniqueQueueFamilies.Length)] do
                let mutable createInfo = VkDeviceQueueCreateInfo ()
                createInfo.queueFamilyIndex <- uniqueQueueFamilies[i]
                createInfo.queueCount <- 1u
                createInfo.pQueuePriorities <- asPointer &queuePriority
                queueCreateInfos[i] <- createInfo

            // get swapchain extension
            let swapchainExtensionName = spanToString VK_KHR_SWAPCHAIN_EXTENSION_NAME
            use extensionArrayWrap = StringArrayWrap [|swapchainExtensionName|]

            // NOTE: for particularly dated implementations of Vulkan, validation depends on device layers which are
            // deprecated. These must be enabled if validation support for said implementations is desired.
            
            // populate createdevice info
            let mutable createInfo = VkDeviceCreateInfo ()
            createInfo.queueCreateInfoCount <- uint queueCreateInfos.Length
            createInfo.pQueueCreateInfos <- queueCreateInfosPin.Pointer
            createInfo.enabledExtensionCount <- 1u
            createInfo.ppEnabledExtensionNames <- extensionArrayWrap.Pointer

            // create device
            vkCreateDevice (physicalDeviceData.PhysicalDevice, &createInfo, nullPtr, &device) |> check

            // fin
            device

        /// Get surface format.
        static member getSurfaceFormat formats =
            
            // specify preferred format and color space
            let isPreferred (format : VkSurfaceFormatKHR) =
                format.format = VK_FORMAT_B8G8R8A8_SRGB &&
                
                // NOTE: in older implementations this color space is called VK_COLORSPACE_SRGB_NONLINEAR_KHR.
                // See https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Surface-format.
                format.colorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR

            // default to first format if preferred is unavailable
            let format =
                match Array.tryFind isPreferred formats with
                | Some format -> format
                | None -> formats[0]

            // fin
            format

        /// Get swap extent.
        static member getSwapExtent (surfaceCapabilities : VkSurfaceCapabilitiesKHR) window =
            
            // swap extent
            let extent =
                if surfaceCapabilities.currentExtent.width <> UInt32.MaxValue then surfaceCapabilities.currentExtent
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

                    // make extent
                    VkExtent2D (width, height)

            // fin
            extent
        
        /// Create the swapchain.
        static member createSwapchain (surfaceFormat : VkSurfaceFormatKHR) swapExtent physicalDeviceData surface device =
            
            // swapchain handle
            let mutable swapchain = Unchecked.defaultof<VkSwapchainKHR>
            
            // get capabilities for cleaner code
            let capabilities = physicalDeviceData.SurfaceCapabilities
            
            // present mode; VK_PRESENT_MODE_FIFO_KHR is guaranteed by the spec and seems most appropriate for nu
            let presentMode = VK_PRESENT_MODE_FIFO_KHR

            (* Decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
               at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
               https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain. *)

            let minImageCount =
                if capabilities.maxImageCount = 0u then capabilities.minImageCount + 1u
                else min (capabilities.minImageCount + 1u) capabilities.maxImageCount

            // in case graphics and present queue families differ
            // TODO: as part of optimization, the sharing mode in this case should probably be VK_SHARING_MODE_EXCLUSIVE (see below).
            let indicesArray = [|physicalDeviceData.GraphicsQueueFamily; physicalDeviceData.PresentQueueFamily|]
            use indicesArrayPin = ArrayPin indicesArray

            // populate create swapchain info
            let mutable createInfo = VkSwapchainCreateInfoKHR ()
            createInfo.surface <- surface
            createInfo.minImageCount <- minImageCount
            createInfo.imageFormat <- surfaceFormat.format
            createInfo.imageColorSpace <- surfaceFormat.colorSpace
            createInfo.imageExtent <- swapExtent
            createInfo.imageArrayLayers <- 1u
            createInfo.imageUsage <- VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            
            if (physicalDeviceData.GraphicsQueueFamily = physicalDeviceData.PresentQueueFamily) then
                createInfo.imageSharingMode <- VK_SHARING_MODE_EXCLUSIVE
                createInfo.queueFamilyIndexCount <- 0u
                createInfo.pQueueFamilyIndices <- nullPtr
            else
                createInfo.imageSharingMode <- VK_SHARING_MODE_CONCURRENT
                createInfo.queueFamilyIndexCount <- 2u
                createInfo.pQueueFamilyIndices <- indicesArrayPin.Pointer

            createInfo.preTransform <- capabilities.currentTransform
            createInfo.compositeAlpha <- VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
            createInfo.presentMode <- presentMode
            createInfo.clipped <- true
            createInfo.oldSwapchain <- VkSwapchainKHR.Null

            // create swapchain
            vkCreateSwapchainKHR (device, &createInfo, nullPtr, &swapchain) |> check

            // fin
            swapchain

        /// Get swapchain images.
        static member getSwapchainImages swapchain device =
            let mutable imageCount = 0u
            vkGetSwapchainImagesKHR (device, swapchain, asPointer &imageCount, nullPtr) |> check
            let images = Array.zeroCreate<VkImage> (int imageCount)
            use imagesPin = ArrayPin images
            vkGetSwapchainImagesKHR (device, swapchain, asPointer &imageCount, imagesPin.Pointer) |> check
            images
        
        /// Create swapchain image views.
        static member createSwapchainImageViews format (images : VkImage array) device =
            
            // image view handle array
            let imageViews = Array.zeroCreate<VkImageView> images.Length

            // populate create infos
            for i in [0 .. dec imageViews.Length] do
                let mutable createInfo = VkImageViewCreateInfo ()
                createInfo.image <- images[i]
                createInfo.viewType <- VK_IMAGE_VIEW_TYPE_2D
                createInfo.format <- format
                createInfo.components.r <- VK_COMPONENT_SWIZZLE_IDENTITY
                createInfo.components.g <- VK_COMPONENT_SWIZZLE_IDENTITY
                createInfo.components.b <- VK_COMPONENT_SWIZZLE_IDENTITY
                createInfo.components.a <- VK_COMPONENT_SWIZZLE_IDENTITY
                createInfo.subresourceRange.aspectMask <- VK_IMAGE_ASPECT_COLOR_BIT
                createInfo.subresourceRange.baseMipLevel <- 0u
                createInfo.subresourceRange.levelCount <- 1u
                createInfo.subresourceRange.baseArrayLayer <- 0u
                createInfo.subresourceRange.layerCount <- 1u

                // create image view
                vkCreateImageView (device, &createInfo, nullPtr, &imageViews[i]) |> check

            // fin
            imageViews

        /// Create the command pool.
        static member createCommandPool queueFamilyIndex device =
            
            // command pool handle
            let mutable commandPool = Unchecked.defaultof<VkCommandPool>

            // populate create info
            let mutable createInfo = VkCommandPoolCreateInfo ()
            createInfo.flags <- VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
            createInfo.queueFamilyIndex <- queueFamilyIndex

            // create command pool
            vkCreateCommandPool (device, &createInfo, nullPtr, &commandPool) |> check

            // fin
            commandPool

        /// Allocate the command buffer.
        static member allocateCommandBuffer commandPool device =
            
            // command buffer handle
            let mutable commandBuffer = Unchecked.defaultof<VkCommandBuffer>

            // populate allocate info
            let mutable allocateInfo = VkCommandBufferAllocateInfo ()
            allocateInfo.commandPool <- commandPool
            allocateInfo.level <- VK_COMMAND_BUFFER_LEVEL_PRIMARY
            allocateInfo.commandBufferCount <- 1u

            // allocate command buffer
            vkAllocateCommandBuffers (device, asPointer &allocateInfo, asPointer &commandBuffer) |> check

            // fin
            commandBuffer

        /// Get command queue.
        static member getQueue queueFamilyIndex device =
            let mutable queue = Unchecked.defaultof<VkQueue>
            vkGetDeviceQueue (device, queueFamilyIndex, 0u, &queue)
            queue
        
        /// Create a semaphore.
        static member createSemaphore device =
            let mutable semaphore = Unchecked.defaultof<VkSemaphore>
            let createInfo = VkSemaphoreCreateInfo ()
            vkCreateSemaphore (device, &createInfo, nullPtr, &semaphore) |> check
            semaphore
        
        /// Create a fence.
        static member createFence device =
            let mutable fence = Unchecked.defaultof<VkFence>
            let mutable createInfo = VkFenceCreateInfo ()
            createInfo.flags <- VK_FENCE_CREATE_SIGNALED_BIT
            vkCreateFence (device, &createInfo, nullPtr, &fence) |> check
            fence
        
        /// Create the renderpass used to clear the screen.
        static member createScreenClearRenderPass format device =
            
            // renderpass handle
            let mutable renderPass = Unchecked.defaultof<VkRenderPass>
            
            // populate attachment
            let mutable attachment = VkAttachmentDescription ()
            attachment.format <- format
            attachment.samples <- VK_SAMPLE_COUNT_1_BIT
            attachment.loadOp <- VK_ATTACHMENT_LOAD_OP_CLEAR
            attachment.storeOp <- VK_ATTACHMENT_STORE_OP_STORE
            attachment.stencilLoadOp <- VK_ATTACHMENT_LOAD_OP_DONT_CARE
            attachment.stencilStoreOp <- VK_ATTACHMENT_STORE_OP_DONT_CARE
            attachment.initialLayout <- VK_IMAGE_LAYOUT_UNDEFINED
            attachment.finalLayout <- VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

            // populate attachment reference
            let mutable attachmentReference = VkAttachmentReference ()
            attachmentReference.attachment <- 0u
            attachmentReference.layout <- VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

            // populate subpass
            let mutable subpass = VkSubpassDescription ()
            subpass.pipelineBindPoint <- VK_PIPELINE_BIND_POINT_GRAPHICS
            subpass.inputAttachmentCount <- 0u
            subpass.colorAttachmentCount <- 1u
            subpass.pColorAttachments <- asPointer &attachmentReference
            subpass.preserveAttachmentCount <- 0u

            // populate dependency
            let mutable dependency = VkSubpassDependency ()
            dependency.srcSubpass <- VK_SUBPASS_EXTERNAL
            dependency.dstSubpass <- 0u
            dependency.srcStageMask <- VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            dependency.dstStageMask <- VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            dependency.srcAccessMask <- VK_ACCESS_NONE
            dependency.dstAccessMask <- VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT

            // populate create info
            let mutable createInfo = VkRenderPassCreateInfo ()
            createInfo.attachmentCount <- 1u
            createInfo.pAttachments <- asPointer &attachment
            createInfo.subpassCount <- 1u
            createInfo.pSubpasses <- asPointer &subpass
            createInfo.dependencyCount <- 1u
            createInfo.pDependencies <- asPointer &dependency

            // create renderpass
            vkCreateRenderPass (device, &createInfo, nullPtr, &renderPass) |> check

            // fin
            renderPass
        
        /// Create the swapchain framebuffers.
        static member createSwapchainFramebuffers (extent : VkExtent2D) renderPass (imageViews : VkImageView array) device =
            
            // framebuffer handle array
            let framebuffers = Array.zeroCreate<VkFramebuffer> imageViews.Length

            // populate create infos
            for i in [0 .. dec framebuffers.Length] do
                let mutable imageView = imageViews[i]
                let mutable createInfo = VkFramebufferCreateInfo ()
                createInfo.renderPass <- renderPass
                createInfo.attachmentCount <- 1u
                createInfo.pAttachments <- asPointer &imageView
                createInfo.width <- extent.width
                createInfo.height <- extent.height
                createInfo.layers <- 1u

                // create framebuffer
                vkCreateFramebuffer (device, &createInfo, nullPtr, &framebuffers[i]) |> check
            
            // fin
            framebuffers
        
        /// Begin the frame and clear the screen.
        static member beginFrame vulkanGlobal =
            
            // swapchain image index and other handles
            let mutable imageIndex = 0u
            let device = vulkanGlobal.Device
            let swapchain = vulkanGlobal.Swapchain
            let commandBuffer = vulkanGlobal.CommandBuffer
            let imageAvailable = vulkanGlobal.ImageAvailableSemaphore
            let mutable inFlight = vulkanGlobal.InFlightFence

            // wait for previous cycle to finish
            vkWaitForFences (device, 1u, asPointer &inFlight, VkBool32.True, UInt64.MaxValue) |> check
            vkResetFences (device, 1u, asPointer &inFlight) |> check

            // acquire image from swapchain to draw onto
            vkAcquireNextImageKHR (device, swapchain, UInt64.MaxValue, imageAvailable, VkFence.Null, &imageIndex) |> check

            // reset command buffer and begin recording
            vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None) |> check
            let mutable beginInfo = VkCommandBufferBeginInfo ()
            vkBeginCommandBuffer (commandBuffer, asPointer &beginInfo) |> check

            // set color for screen clear
            // TODO: change to proper color once the testing utility of white is no longer needed.
            let mutable clearColor = VkClearValue (1.0f, 1.0f, 1.0f, 1.0f)

            // populate render pass info
            let mutable renderPassInfo = VkRenderPassBeginInfo ()
            renderPassInfo.renderPass <- vulkanGlobal.ScreenClearRenderPass
            renderPassInfo.framebuffer <- vulkanGlobal.SwapchainFramebuffers[int imageIndex]
            renderPassInfo.renderArea.offset <- VkOffset2D.Zero
            renderPassInfo.renderArea.extent <- vulkanGlobal.SwapExtent
            renderPassInfo.clearValueCount <- 1u
            renderPassInfo.pClearValues <- asPointer &clearColor

            // clear the screen
            vkCmdBeginRenderPass (commandBuffer, asPointer &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE)
            vkCmdEndRenderPass commandBuffer
            
            // fin
            imageIndex

        /// End the frame.
        static member endFrame imageIndex vulkanGlobal =
            
            // swapchain image index and other handles
            let mutable imageIndex = imageIndex
            let mutable swapchain = vulkanGlobal.Swapchain
            let mutable commandBuffer = vulkanGlobal.CommandBuffer
            let mutable imageAvailable = vulkanGlobal.ImageAvailableSemaphore
            let mutable renderFinished = vulkanGlobal.RenderFinishedSemaphore

            // end command buffer recording
            vkEndCommandBuffer commandBuffer |> check
            
            // populate submit info
            let mutable flags = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            let mutable submitInfo = VkSubmitInfo ()
            submitInfo.waitSemaphoreCount <- 1u
            submitInfo.pWaitSemaphores <- asPointer &imageAvailable
            submitInfo.pWaitDstStageMask <- asPointer &flags
            submitInfo.commandBufferCount <- 1u
            submitInfo.pCommandBuffers <- asPointer &commandBuffer
            submitInfo.signalSemaphoreCount <- 1u
            submitInfo.pSignalSemaphores <- asPointer &renderFinished

            // submit commands
            vkQueueSubmit (vulkanGlobal.GraphicsQueue, 1u, asPointer &submitInfo, vulkanGlobal.InFlightFence) |> check

            // populate present info
            let mutable presentInfo = VkPresentInfoKHR ()
            presentInfo.waitSemaphoreCount <- 1u
            presentInfo.pWaitSemaphores <- asPointer &renderFinished
            presentInfo.swapchainCount <- 1u
            presentInfo.pSwapchains <- asPointer &swapchain
            presentInfo.pImageIndices <- asPointer &imageIndex

            // present image back to swapchain to appear on screen
            vkQueuePresentKHR (vulkanGlobal.PresentQueue, asPointer &presentInfo) |> check
        
        /// Wait for all device operations to complete before cleaning up resources.
        static member waitIdle vulkanGlobal = vkDeviceWaitIdle vulkanGlobal.Device |> check
        
        /// Destroy Vulkan handles.
        static member cleanup vulkanGlobal =
            
            // commonly used handles
            let instance = vulkanGlobal.Instance
            let device = vulkanGlobal.Device
            let framebuffers = vulkanGlobal.SwapchainFramebuffers
            let imageViews = vulkanGlobal.SwapchainImageViews
            
            for i in [0 .. dec framebuffers.Length] do vkDestroyFramebuffer (device, framebuffers[i], nullPtr)
            vkDestroyRenderPass (device, vulkanGlobal.ScreenClearRenderPass, nullPtr)
            vkDestroyFence (device, vulkanGlobal.InFlightFence, nullPtr)
            vkDestroySemaphore (device, vulkanGlobal.RenderFinishedSemaphore, nullPtr)
            vkDestroySemaphore (device, vulkanGlobal.ImageAvailableSemaphore, nullPtr)
            vkDestroyCommandPool (device, vulkanGlobal.CommandPool, nullPtr)
            for i in [0 .. dec imageViews.Length] do vkDestroyImageView (device, imageViews[i], nullPtr)
            vkDestroySwapchainKHR (device, vulkanGlobal.Swapchain, nullPtr)
            vkDestroyDevice (device, nullPtr)
            vkDestroySurfaceKHR (instance, vulkanGlobal.Surface, nullPtr)
            vkDestroyInstance (instance, nullPtr)
        
        /// Begin frame if VulkanGlobal exists.
        static member tryBeginFrame vulkanGlobalOpt =
            match vulkanGlobalOpt with
            | Some vulkanGlobal -> VulkanGlobal.beginFrame vulkanGlobal
            | None -> 0u

        /// End frame if VulkanGlobal exists.
        static member tryEndFrame imageIndex vulkanGlobalOpt =
            match vulkanGlobalOpt with
            | Some vulkanGlobal -> VulkanGlobal.endFrame imageIndex vulkanGlobal
            | None -> ()

        /// Wait idle if VulkanGlobal exists.
        static member tryWaitIdle vulkanGlobalOpt =
            match vulkanGlobalOpt with
            | Some vulkanGlobal -> VulkanGlobal.waitIdle vulkanGlobal
            | None -> ()

        /// Cleanup if VulkanGlobal exists.
        static member tryCleanup vulkanGlobalOpt =
            match vulkanGlobalOpt with
            | Some vulkanGlobal -> VulkanGlobal.cleanup vulkanGlobal
            | None -> ()
        
        /// Try to make a VulkanGlobal.
        static member tryMake window =

            // load vulkan; not vulkan function
            vkInitialize () |> check

            // create instance
            let instance = VulkanGlobal.createInstance window

            // load instance commands; not vulkan function
            vkLoadInstanceOnly instance

            // create surface
            let surface = VulkanGlobal.createSurface window instance
            
            // try select physical device
            match VulkanGlobal.trySelectPhysicalDevice surface instance with
            | Some physicalDeviceData ->

                // create device
                let device = VulkanGlobal.createDevice physicalDeviceData

                // load device commands; not vulkan function
                vkLoadDevice device

                // get surface format and swap extent
                let surfaceFormat = VulkanGlobal.getSurfaceFormat physicalDeviceData.Formats
                let swapExtent = VulkanGlobal.getSwapExtent physicalDeviceData.SurfaceCapabilities window

                // setup swapchain and its assets
                let swapchain = VulkanGlobal.createSwapchain surfaceFormat swapExtent physicalDeviceData surface device
                let swapchainImages = VulkanGlobal.getSwapchainImages swapchain device
                let swapchainImageViews = VulkanGlobal.createSwapchainImageViews surfaceFormat.format swapchainImages device

                // setup command system
                let commandPool = VulkanGlobal.createCommandPool physicalDeviceData.GraphicsQueueFamily device
                let commandBuffer = VulkanGlobal.allocateCommandBuffer commandPool device
                let graphicsQueue = VulkanGlobal.getQueue physicalDeviceData.GraphicsQueueFamily device
                let presentQueue = VulkanGlobal.getQueue physicalDeviceData.PresentQueueFamily device

                // create sync objects
                let imageAvailableSemaphore = VulkanGlobal.createSemaphore device
                let renderFinishedSemaphore = VulkanGlobal.createSemaphore device
                let inFlightFence = VulkanGlobal.createFence device

                // create screen clear renderpass
                let screenClearRenderPass = VulkanGlobal.createScreenClearRenderPass surfaceFormat.format device

                // create swapchain framebuffers
                let swapchainFramebuffers = VulkanGlobal.createSwapchainFramebuffers swapExtent screenClearRenderPass swapchainImageViews device
                
                // make vulkanGlobal
                let vulkanGlobal =
                    { Instance = instance
                      Surface = surface
                      Device = device
                      Swapchain = swapchain
                      SwapchainImageViews = swapchainImageViews
                      CommandPool = commandPool
                      CommandBuffer = commandBuffer
                      GraphicsQueue = graphicsQueue
                      PresentQueue = presentQueue
                      ImageAvailableSemaphore = imageAvailableSemaphore
                      RenderFinishedSemaphore = renderFinishedSemaphore
                      InFlightFence = inFlightFence
                      ScreenClearRenderPass = screenClearRenderPass
                      SwapchainFramebuffers = swapchainFramebuffers
                      SwapExtent = swapExtent }

                // fin
                Some vulkanGlobal

            // abort vulkan
            | None -> None