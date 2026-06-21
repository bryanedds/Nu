// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Numerics
open FSharp.NativeInterop
open SDL
open Vortice.Vulkan
open Prime
open Nu
    
/// Exposes the vulkan handles that must be globally accessible within the renderer.
type [<ReferenceEquality>] VulkanContext =
    private
        { mutable WaitingForWindowRestore_ : bool
          mutable RenderAllowed_ : bool
          Instance_ : VkInstance
          DebugMessengerOpt_ : VkDebugUtilsMessengerEXT option
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
          InFlightFences_ : VkFence array
          TransientFence_ : VkFence
          TextureFence_ : VkFence }

    /// Whether rendering is permitted in the engine's current state.
    member this.RenderAllowed = this.RenderAllowed_
    
    /// The physical device.
    member this.VkPhysicalDevice = this.PhysicalDevice_.VkPhysicalDevice

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
    member this.RenderCommandBuffer = this.RenderCommandBuffers_.[HlInternal.CurrentFrame]

    /// The render command queue.
    member this.RenderQueue = this.RenderQueue_

    /// The texture command queue.
    member this.TextureQueue = this.TextureQueue_

    /// The image available semaphore for the current frame.
    member this.ImageAvailableSemaphore = this.ImageAvailableSemaphores_.[HlInternal.CurrentFrame]

    /// The render finished semaphore for the current frame.
    member this.RenderFinishedSemaphore = this.Swapchain_.RenderFinishedSemaphore

    /// The in flight fence for the current frame.
    member this.InFlightFence = this.InFlightFences_.[HlInternal.CurrentFrame]

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
        let callbackData = NativePtr.toByRef (NativePtr.ofNativeInt<VkDebugUtilsMessengerCallbackDataEXT> pCallbackData)
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
        if not
            (messageType = VkDebugUtilsMessageTypeFlagsEXT.General && messageSeverity <= VkDebugUtilsMessageSeverityFlagsEXT.Info ||
             messageType = VkDebugUtilsMessageTypeFlagsEXT.Performance && messageSeverity <= VkDebugUtilsMessageSeverityFlagsEXT.Warning) then
            Log.custom header message
        
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
    static member private createVulkanInstance debugInfo =

        // get available instance layers
        let mutable layerCount = 0u
        Vulkan.vkEnumerateInstanceLayerProperties (asPointer &layerCount, nullPtr) |> Hl.check
        let layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
        use layersPin = new ArrayPin<_> (layers)
        Vulkan.vkEnumerateInstanceLayerProperties (asPointer &layerCount, layersPin.Pointer) |> Hl.check

        // check if validation layer exists
        // TODO: DJL: try to automatically prevent validation from interfering with Nsight, starting with VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT.
        let validationLayer = "VK_LAYER_KHRONOS_validation"
        let validationLayerExists = Array.exists (fun x -> Hl.getLayerName x = validationLayer) layers
        if HlInternal.ValidationLayersEnabled && not validationLayerExists then Log.info (validationLayer + " is not available. Vulkan programmers must install the Vulkan SDK to enable validation.")
        HlInternal.ValidationLayersActivated <- HlInternal.ValidationLayersEnabled && validationLayerExists
        use layerWrap = new StringArrayWrap ([|validationLayer|]) // must remain in scope until vkCreateInstance

        // get sdl extensions
        let mutable sdlExtensionCount = 0u
        let sdlExtensions = SDL3.SDL_Vulkan_GetInstanceExtensions &&sdlExtensionCount
        let sdlExtensionCountInt = int sdlExtensionCount
        if NativePtr.isNullPtr sdlExtensions then Log.fail (SDL3.SDL_GetError ())

        // choose extensions
        use debugUtilsWrap = new StringWrap (Vulkan.VK_EXT_DEBUG_UTILS_EXTENSION_NAME)
        let extensions =
            Array.init (sdlExtensionCountInt + if HlInternal.ValidationLayersActivated then 1 else 0)
                (fun i -> if i < sdlExtensionCountInt then NativePtr.get sdlExtensions i else debugUtilsWrap.Pointer)
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
        if HlInternal.ValidationLayersActivated then
            let mutable debugInfo = debugInfo
            info.pNext <- asVoidPtr &debugInfo
            info.enabledLayerCount <- 1u
            info.ppEnabledLayerNames <- layerWrap.Pointer
        let mutable instance = Unchecked.defaultof<VkInstance>
        Vulkan.vkCreateInstance (&info, nullPtr, &instance) |> Hl.check
        instance

    // TODO: DJL: try separate this from validation status, same for create instance debug.
    static member private tryCreateDebugMessenger info instance =
        if HlInternal.ValidationLayersActivated then
            let mutable debugMessenger = Unchecked.defaultof<VkDebugUtilsMessengerEXT>
            Vulkan.vkCreateDebugUtilsMessengerEXT (instance, &info, nullPtr, &debugMessenger) |> Hl.check
            Some debugMessenger
        else None
    
    /// Select compatible physical device if available.
    static member private trySelectPhysicalDevice window instance =

        // get available physical devices
        let mutable deviceCount = 0u
        Vulkan.vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, nullPtr) |> Hl.check
        let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
        use devicesPin = new ArrayPin<_> (devices)
        Vulkan.vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, devicesPin.Pointer) |> Hl.check

        // gather devices together with relevant data for selection
        let candidates =
            [for i in 0 .. dec devices.Length do
                match PhysicalDevice.tryCreate devices.[i] window instance with
                | Some physicalDevice -> physicalDevice
                | None -> ()]

        // compatibility criteria: device must support essential rendering components, texture compression and at least Vulkan 1.3
        let isCompatible physicalDevice =
            let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
            let swapchainSupported = Array.exists (fun ext -> Hl.getExtensionName ext = swapchainExtensionName) physicalDevice.Extensions
            swapchainSupported &&
            physicalDevice.SurfaceFormats.Length > 0 &&
            physicalDevice.Properties.apiVersion >= VkVersion.Version_1_3 &&
            match Constants.Render.TextureBlockCompression with
            | BcCompression -> physicalDevice.Features.textureCompressionBC
            | AstcCompression -> physicalDevice.Features.textureCompressionASTC_LDR

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
        let mutable vulkan13 = VkPhysicalDeviceVulkan13Features ()
        vulkan13.dynamicRendering <- true
        
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
        let extensionArray = [|swapchainExtensionName|]
        use extensionArrayWrap = new StringArrayWrap (extensionArray)

        // NOTE: DJL: for particularly dated implementations of Vulkan, validation depends on device layers which
        // are deprecated. These must be enabled if validation support for said implementations is desired.

        // specify device features to be enabled
        let mutable features = VkPhysicalDeviceFeatures ()
        if physicalDevice.SupportsAnisotropy then features.samplerAnisotropy <- true
        
        // create device
        let mutable info = VkDeviceCreateInfo ()
        info.pNext <- asVoidPtr &vulkan13
        info.queueCreateInfoCount <- uint queueCreateInfos.Length
        info.pQueueCreateInfos <- queueCreateInfosPin.Pointer
        info.enabledExtensionCount <- uint extensionArray.Length
        info.ppEnabledExtensionNames <- extensionArrayWrap.Pointer
        info.pEnabledFeatures <- asPointer &features
        let mutable device = Unchecked.defaultof<VkDevice>
        Vulkan.vkCreateDevice (physicalDevice.VkPhysicalDevice, &info, nullPtr, &device) |> Hl.check
        device

    /// Create the VMA allocator.
    static member private createVmaAllocator (physicalDevice : PhysicalDevice) device instance =
        let mutable info = VmaAllocatorCreateInfo ()
        info.physicalDevice <- physicalDevice.VkPhysicalDevice
        info.device <- device
        info.instance <- instance
        let mutable allocator = Unchecked.defaultof<VmaAllocator>
        Vma.vmaCreateAllocator (&info, &allocator) |> Hl.check
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
        Vulkan.vkCreateCommandPool (device, &info, nullPtr, &commandPool) |> Hl.check
        commandPool

    /// Allocate an array of command buffers for each frame in flight.
    static member private allocateFifCommandBuffers commandPool device =
        Hl.allocateCommandBuffers Constants.Vulkan.MaxFramesInFlight commandPool device
    
    /// Create image available semaphores.
    static member private createImageAvailableSemaphores device =
        let semaphores = Array.zeroCreate<VkSemaphore> Constants.Vulkan.MaxFramesInFlight
        for i in 0 .. dec semaphores.Length do semaphores.[i] <- Hl.createSemaphore device
        semaphores

    /// Create in-flight fences.
    static member private createInFlightFences device =
        let fences = Array.zeroCreate<VkFence> Constants.Vulkan.MaxFramesInFlight
        for i in 0 .. dec fences.Length do fences.[i] <- Hl.createFence true device
        fences
    
    /// Handle changes in window size, and check for minimization.
    static member private handleWindowSize vkc =
        
        // query minimization status
        // NOTE: DJL: this both detects the beginning of minimization and checks for the end.
        vkc.WaitingForWindowRestore_ <- Swapchain.isWindowMinimized vkc.Swapchain_.Window_

        // update the swapchain if window is not minimized, which happens a) when the window size simply changes
        // and b) when minimization ends as detected above; must also check for backgrounding in case minimization
        // occurs first so backgrounding can still be handled straight away
        if not vkc.WaitingForWindowRestore_ || HlInternal.hasAppBegunBackgrounding ()
        then Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
    
    /// Wait for app to return to foreground.
    static member private handleBackgrounding vkc =
        vkc.WaitingForWindowRestore_ <- Swapchain.isWindowMinimized vkc.Swapchain_.Window_
        if HlInternal.IsAppInForeground && not vkc.WaitingForWindowRestore_
        then Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
    
    /// Begin the frame.
    static member beginFrame (windowViewport : Viewport) (vkc : VulkanContext) =

        // ensure that rendering is only allowed after passing all checks below
        vkc.RenderAllowed_ <- false

        // ensure current frame is ready
        let mutable fence = vkc.InFlightFence
        Vulkan.vkWaitForFences (vkc.Device, 1u, asPointer &fence, true, UInt64.MaxValue) |> Hl.check

        // the cascade of checks needed to handle window behavior, avoiding 'elif' statements for maximum understandability
        
        // check if current swapchain is non-existent, typically because app has been backgrounded
        if Option.isNone vkc.Swapchain_.SwapchainInternalOpt then VulkanContext.handleBackgrounding vkc
        else
            // check for handling of minimized window from previous frame(s); if *still* minimized then do nothing; if restored then refresh swapchain
            if vkc.WaitingForWindowRestore_ then VulkanContext.handleWindowSize vkc
            else
                // check if app backgrounding has been triggered, if so then teardown the surface and swapchain
                if HlInternal.hasAppBegunBackgrounding () then Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
                else
                    // check if screen *has become* minimized, if so then set WaitingForWindowRestore_ and don't render
                    if Swapchain.isWindowMinimized vkc.Swapchain_.Window_ then VulkanContext.handleWindowSize vkc
                    else
                        // check if screen size changed (or surface lost), if so then refresh swapchain
                        if Swapchain.isWindowResizedOrSurfaceLost vkc.VkPhysicalDevice vkc.Swapchain_ then VulkanContext.handleWindowSize vkc
                        else
                            // check that swap extent >= viewport.Bounds >= viewport.Inner; done *after* screen change check to avoid outdated swap extent
                            let extent = vkc.Swapchain_.SwapExtent
                            let swapchainBounds = box2i v2iZero (v2i (int extent.width) (int extent.height))
                            if
                                swapchainBounds.ContainsInclusive windowViewport.Bounds = ContainmentType.Contains &&
                                windowViewport.Bounds.ContainsInclusive windowViewport.Inner = ContainmentType.Contains
                            then
                                // try to acquire image from swapchain to draw onto
                                // NOTE: DJL: due to semaphore, if this is successful, the render *must* proceed!
                                let result = Vulkan.vkAcquireNextImageKHR (vkc.Device, vkc.Swapchain_.VkSwapchain, UInt64.MaxValue, vkc.ImageAvailableSemaphore, VkFence.Null, &HlInternal.ImageIndex)
                                if result = VkResult.ErrorOutOfDateKHR then VulkanContext.handleWindowSize vkc // refresh swapchain if out of date
                                else
                                    // destroy surface if lost
                                    if result = VkResult.ErrorSurfaceLostKHR then
                                        HlInternal.SurfaceState <- SurfaceLost
                                        Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
                                    else
                                        Hl.check result // NOTE: DJL: this will report a suboptimal swapchain image.
                                        vkc.RenderAllowed_ <- true // permit rendering

        if vkc.RenderAllowed_ then
        
            // begin command recording
            Hl.createPersistentCommandBuffer vkc.RenderCommandBuffer
            
            // transition swapchain image layout to color attachment
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color Undefined ColorAttachmentWrite vkc.Swapchain_.Image vkc.RenderCommandBuffer
            
            // clear screen
            let renderArea = VkRect2D (VkOffset2D.Zero, vkc.Swapchain_.SwapExtent)
            let clearColor = VkClearValue (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
            let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea (Some clearColor)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

            // clear viewport
            let renderArea = VkRect2D (windowViewport.Bounds.Min.X, windowViewport.Bounds.Min.Y, uint windowViewport.Bounds.Size.X, uint windowViewport.Bounds.Size.Y)
            let clearColor = VkClearValue (Constants.Render.ViewportClearColor.R, Constants.Render.ViewportClearColor.G, Constants.Render.ViewportClearColor.B, Constants.Render.ViewportClearColor.A)
            let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea (Some clearColor)
            Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

    /// End the frame.
    static member endFrame () =
        () // nothing to do

    /// Present the image back to the swapchain to appear on screen.
    static member present (vkc : VulkanContext) =
        if vkc.RenderAllowed_ then
        
            // transition swapchain image layout to presentation
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color ColorAttachmentWrite Present vkc.Swapchain_.Image vkc.RenderCommandBuffer
            
            // the *simple* solution: https://vulkan-tutorial.com/Drawing_a_triangle/Drawing/Rendering_and_presentation#page_Subpass-dependencies
            let waitStage = VkPipelineStageFlags.TopOfPipe
            
            // reset fence and flush commands
            let mutable fence = vkc.InFlightFence
            Vulkan.vkResetFences (vkc.Device, 1u, asPointer &fence) |> Hl.check
            Queue.submit vkc.RenderCommandBuffer [|vkc.ImageAvailableSemaphore, waitStage|] [|vkc.RenderFinishedSemaphore|] fence vkc.RenderQueue
            
            // one more check for app backgrounding before we present
            if not (HlInternal.hasAppBegunBackgrounding ()) then
            
                // try to present image
                let result = Queue.present vkc.RenderFinishedSemaphore vkc.Swapchain_.VkSwapchain vkc.PresentQueue_

                // refresh swapchain if framebuffer out of date or suboptimal
                if result = VkResult.ErrorOutOfDateKHR || result = VkResult.SuboptimalKHR then VulkanContext.handleWindowSize vkc
                
                // destroy surface if lost
                elif result = VkResult.ErrorSurfaceLostKHR then
                    HlInternal.SurfaceState <- SurfaceLost
                    Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
                else Hl.check result

            // this is valid because RenderFinishedSemaphore will be destroyed
            else Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_

        // advance frame in flight
        // NOTE: DJL: this MUST happen EVERY frame regardless of RenderDesired_ otherwise fence security is broken.
        HlInternal.CurrentFrame <- inc HlInternal.CurrentFrame % Constants.Vulkan.MaxFramesInFlight

    /// Wait for all device operations to complete before cleaning up resources.
    static member waitIdle (vkc : VulkanContext) =
        
        // never call vkDeviceWaitIdle as its implementation compromises queue thread safety
        Queue.waitIdle vkc.RenderQueue_
        Queue.waitIdle vkc.PresentQueue_
        Queue.waitIdle vkc.TextureQueue_

    /// Attempt to create a VulkanContext.
    /// NOTE: this procedure is intended to be invoked from the main thread to satisfy the requirements of Mac and
    /// iOS surface creation, and possibly other platforms.
    static member tryCreate window =

        // load vulkan; not vulkan function
        Vulkan.vkInitialize () |> Hl.check

        // make debug info
        let debugInfo = VulkanContext.makeDebugMessengerInfo ()
        
        // create instance
        let instance = VulkanContext.createVulkanInstance debugInfo

        // load instance commands; not vulkan function
        Vulkan.vkLoadInstanceOnly instance

        // create debug messenger if validation activated
        let debugMessengerOpt = VulkanContext.tryCreateDebugMessenger debugInfo instance
        
        // create surface
        Hl.createVulkanSurface window instance

        // attempt to select physical device
        match VulkanContext.trySelectPhysicalDevice window instance with
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
            let transientFence = Hl.createFence false device
            
            // setup transient (one time) execution on texture server thread
            let textureCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily device
            let textureFence = Hl.createFence false device

            // setup swapchain
            let surfaceFormat = VulkanContext.getSurfaceFormat physicalDevice.SurfaceFormats
            let (swapchain, windowMinimized) = Swapchain.create surfaceFormat physicalDevice window device

            // make VulkanContext
            let vulkanContext =
                { WaitingForWindowRestore_ = windowMinimized
                  RenderAllowed_ = false
                  Instance_ = instance
                  DebugMessengerOpt_ = debugMessengerOpt
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
                  InFlightFences_ = inFlightFences
                  TransientFence_ = transientFence
                  TextureFence_ = textureFence }

            // fin
            Some vulkanContext

        // failure
        | None -> None

    /// Clean-up a VulkanContext.
    /// NOTE: intended to be invoked from the main thread.
    static member cleanup vkc =
        Swapchain.destroy vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device
        for i in 0 .. dec vkc.ImageAvailableSemaphores_.Length do Vulkan.vkDestroySemaphore (vkc.Device, vkc.ImageAvailableSemaphores_.[i], nullPtr)
        for i in 0 .. dec vkc.InFlightFences_.Length do Vulkan.vkDestroyFence (vkc.Device, vkc.InFlightFences_.[i], nullPtr)
        Vulkan.vkDestroyFence (vkc.Device, vkc.TextureFence, nullPtr)
        Vulkan.vkDestroyFence (vkc.Device, vkc.TransientFence, nullPtr)
        Vulkan.vkDestroyCommandPool (vkc.Device, vkc.RenderCommandPool_, nullPtr)
        Vulkan.vkDestroyCommandPool (vkc.Device, vkc.TextureCommandPool_, nullPtr)
        Vulkan.vkDestroyCommandPool (vkc.Device, vkc.TransientCommandPool, nullPtr)
        Vma.vmaDestroyAllocator vkc.VmaAllocator
        Vulkan.vkDestroyDevice (vkc.Device, nullPtr)
        Hl.destroyVulkanSurface vkc.Instance_
        match vkc.DebugMessengerOpt_ with Some debugMessenger -> Vulkan.vkDestroyDebugUtilsMessengerEXT (vkc.Instance_, debugMessenger, nullPtr) | None -> ()
        Vulkan.vkDestroyInstance (vkc.Instance_, nullPtr)