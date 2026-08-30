// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Reflection
open System.Runtime.CompilerServices
open System.Numerics
open FSharp.NativeInterop
open SDL
open Vortice.Vulkan
open Prime
open Nu

/// A command queue that internally synchronizes use across multiple threads.
type [<ReferenceEquality>] ConcurrentCommandQueue =
    private
        { VkQueue_ : VkQueue
          Lock_ : obj }

    /// Perform an arbitrary operation on the internal vulkan queue.
    static member withLock<'a> queue (op : VkQueue -> 'a) : 'a =
        lock queue.Lock_ (fun () -> op queue.VkQueue_)

    /// Wait for Queue to finish execution.
    static member waitIdle queue =
        ConcurrentCommandQueue.withLock queue $ fun vkQueue ->
            DeviceApi.vkQueueWaitIdle vkQueue |> Hl.check

    /// Transiently run and then free the given command buffer. Command pool and finish fence must NOT be shared
    /// between threads!
    static member runTransient commandBuffer commandPool finishFence (commandQueue : ConcurrentCommandQueue) =

        // lock to get access to vulkan queue
        let mutable commandBuffer = commandBuffer
        let mutable finishFence = finishFence
        ConcurrentCommandQueue.withLock commandQueue $ fun vkQueue ->

            // end command buffer
            DeviceApi.vkEndCommandBuffer commandBuffer |> Hl.check

            // submit commands
            let mutable info = VkSubmitInfo ()
            info.commandBufferCount <- 1u
            info.pCommandBuffers <- &&commandBuffer
            DeviceApi.vkQueueSubmit (vkQueue, 1u, &&info, finishFence) |> Hl.check

            // wait for run to finish
            // NOTE: on Android on my A17, we have to put vkWaitForFences in a loop because it will return before the given
            // timeout with a VkResult.Timeout result (which I'm not sure is standard-conformant).
            let mutable waiting = true
            while waiting do
                let result = DeviceApi.vkWaitForFences (1u, &&finishFence, true, UInt64.MaxValue)
                if result <> VkResult.Timeout then
                    waiting <- false
                    Hl.check result
            DeviceApi.vkResetFences (1u, &&finishFence) |> Hl.check

            // free command buffer
            DeviceApi.vkFreeCommandBuffers (commandPool, 1u, &&commandBuffer)

    /// Create a ConcurrentCommandQueue.
    static member create queueFamilyIndex queueIndex =
        let mutable vkQueue = Unchecked.defaultof<VkQueue>
        DeviceApi.vkGetDeviceQueue (queueFamilyIndex, queueIndex, &vkQueue)
        { VkQueue_ = vkQueue; Lock_ = obj () }

/// A representation of a physical device and associated information.
type PhysicalDevice =
    { VkPhysicalDevice : VkPhysicalDevice
      Properties : VkPhysicalDeviceProperties
      Features : VkPhysicalDeviceFeatures
      Extensions : VkExtensionProperties array
      SurfaceCapabilities : VkSurfaceCapabilitiesKHR // NOTE: keeping this here in case we want to use it for device selection.
      SurfaceFormats : VkSurfaceFormatKHR array
      GraphicsQueueFamily : uint
      PresentQueueFamily : uint
      GraphicsQueueCount : uint }

    /// Supports anisotropy.
    member this.SupportsAnisotropy =
        this.Features.samplerAnisotropy = VkBool32.True

    /// Get properties.
    static member private getProperties vkPhysicalDevice =
        let mutable properties = Unchecked.defaultof<VkPhysicalDeviceProperties>
        InstanceApi.vkGetPhysicalDeviceProperties (vkPhysicalDevice, &properties)
        properties

    /// Get features.
    static member private getFeatures vkPhysicalDevice =
        let mutable features = Unchecked.defaultof<VkPhysicalDeviceFeatures>
        InstanceApi.vkGetPhysicalDeviceFeatures (vkPhysicalDevice, &features)
        features

    /// Get available extensions.
    static member private getExtensions vkPhysicalDevice =
        let mutable extensionCount = 0u
        InstanceApi.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, &&extensionCount, nullPtr) |> Hl.check
        let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
        use extensionsPin = new ArrayPin<_> (extensions)
        InstanceApi.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, &&extensionCount, extensionsPin.Pointer) |> Hl.check
        extensions

    /// Get available surface formats.
    static member private getSurfaceFormats vkPhysicalDevice =
        let mutable formatCount = 0u
        InstanceApi.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, Hl.Surface, &&formatCount, nullPtr) |> Hl.check
        let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
        use formatsPin = new ArrayPin<_> (formats)
        InstanceApi.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, Hl.Surface, &&formatCount, formatsPin.Pointer) |> Hl.check
        formats

    /// Get surface capabilities.
    static member private getSurfaceCapabilities vkPhysicalDevice =
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        InstanceApi.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, Hl.Surface, &capabilities) |> Hl.check
        capabilities
    
    /// Attempt to get the queue families.
    static member private tryGetQueueFamilies vkPhysicalDevice =
        
        // get queue families' properties
        let mutable queueFamilyCount = 0u
        InstanceApi.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, &&queueFamilyCount, nullPtr)
        let queueFamilyProps = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
        use queueFamilyPropsPin = new ArrayPin<_> (queueFamilyProps)
        InstanceApi.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, &&queueFamilyCount, queueFamilyPropsPin.Pointer)

        // NOTE: it is *essential* to use the *first* compatible queue families in the array, *not* the last, as per
        // the tutorial and vortice vulkan sample. This was discovered this by accident because the queue families on
        // my AMD behaved exactly the same as the queue families on this one:
        // https://computergraphics.stackexchange.com/questions/9707/queue-from-a-family-queue-that-supports-presentation-doesnt-work-vulkan
        // General Lesson: trust level for vendors is too low for deviation from common practices to be advisable.
        let mutable graphicsQueueFamilyOpt = None
        let mutable presentQueueFamilyOpt = None
        for i in 0 .. dec queueFamilyProps.Length do

            // try get graphics queue family
            // NOTE: for reason noted above, do not attempt to derive transfer queue from seperate family.
            match graphicsQueueFamilyOpt with
            | None ->
                let props = queueFamilyProps[i]
                if props.queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then
                    graphicsQueueFamilyOpt <- Some (uint i, props.queueCount)
            | Some _ -> ()

            // try get present queue family
            match presentQueueFamilyOpt with
            | None ->
                let mutable presentSupport = VkBool32.False
                InstanceApi.vkGetPhysicalDeviceSurfaceSupportKHR (vkPhysicalDevice, uint i, Hl.Surface, &presentSupport) |> Hl.check
                if presentSupport = VkBool32.True then
                    presentQueueFamilyOpt <- Some (uint i)
            | Some _ -> ()

        // fin
        (graphicsQueueFamilyOpt, presentQueueFamilyOpt)

    /// Attempt to construct a PhysicalDevice representation.
    static member tryMake vkPhysicalDevice =
        let properties = PhysicalDevice.getProperties vkPhysicalDevice
        let features = PhysicalDevice.getFeatures vkPhysicalDevice
        let extensions = PhysicalDevice.getExtensions vkPhysicalDevice
        let surfaceFormats = PhysicalDevice.getSurfaceFormats vkPhysicalDevice
        let surfaceCapabilities = PhysicalDevice.getSurfaceCapabilities vkPhysicalDevice
        match PhysicalDevice.tryGetQueueFamilies vkPhysicalDevice with
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

/// A wrapper for a vulkan swapchain and its assets.
type SwapchainWrapper =
    { VkSwapchain : VkSwapchainKHR
      Images : VkImage array
      ImageViews : VkImageView array
      RenderSemaphores : VkSemaphore array
      SwapExtent : VkExtent2D }

    /// The current render semaphore.
    member this.RenderSemaphore =
        this.RenderSemaphores[int Hl.ImageIndex]

    /// Get swapchain images.
    static member private getSwapchainImages vkSwapchain =
        let mutable imageCount = 0u
        DeviceApi.vkGetSwapchainImagesKHR (vkSwapchain, &&imageCount, nullPtr) |> Hl.check
        let images = Array.zeroCreate<VkImage> (int imageCount)
        use imagesPin = new ArrayPin<_> (images)
        DeviceApi.vkGetSwapchainImagesKHR (vkSwapchain, &&imageCount, imagesPin.Pointer) |> Hl.check
        images

    /// Create the image views.
    static member private createImageViews format (images : VkImage array) =
        let imageViews = Array.zeroCreate<VkImageView> images.Length
        for i in 0 .. dec imageViews.Length do imageViews[i] <- Hl.createImageView Rgba format 0 1 0 1 VkImageViewType.Image2D VkImageAspectFlags.Color images[i]
        imageViews
        
    /// Create render semaphores.
    static member private createRenderSemaphores imageCount =
        let semaphores = Array.zeroCreate<VkSemaphore> imageCount
        for i in 0 .. dec semaphores.Length do semaphores[i] <- Hl.createSemaphore ()
        semaphores

    /// Try create a SwapchainWrapper.
    static member tryCreate surfaceFormat oldVkSwapchainOpt physicalDevice =
        
        // try create vkSwapchain and its assets
        match Hl.tryCreateVkSwapchain surfaceFormat oldVkSwapchainOpt physicalDevice.GraphicsQueueFamily physicalDevice.PresentQueueFamily physicalDevice.VkPhysicalDevice with
        | Some (vkSwapchain, swapExtent) ->

            // create images / views
            let images = SwapchainWrapper.getSwapchainImages vkSwapchain
            let imageViews = SwapchainWrapper.createImageViews surfaceFormat.format images

            // render semaphores based on swapchain images rather than frames in flight to address safety issue
            // described in https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html. these should naturally
            // be associated with the vkSwapchain itself, especially to prevent validation errors triggered by reuse of
            // semaphores that 'may still be in use' by obsolete vkSwapchains.
            let renderSemaphores = SwapchainWrapper.createRenderSemaphores images.Length

            // make SwapchainWrapper
            let swapchainWrapper =
                { VkSwapchain = vkSwapchain
                  Images = images
                  ImageViews = imageViews
                  RenderSemaphores = renderSemaphores
                  SwapExtent = swapExtent }

            // fin
            Some swapchainWrapper
        | None -> None

    /// Destroy a SwapchainWrapper.
    static member destroy renderQueue presentQueue swapchainWrapper =
        
        // NOTE: this is not sufficient to ensure resources are not still in use; that requires a Vulkan extension!!!
        // https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html#_vk_ext_swapchain_maintenance1_extension
        ConcurrentCommandQueue.waitIdle renderQueue
        ConcurrentCommandQueue.waitIdle presentQueue

        // destroy vulkan resources
        for i in 0 .. dec swapchainWrapper.ImageViews.Length do DeviceApi.vkDestroyImageView (swapchainWrapper.ImageViews[i], nullPtr)
        DeviceApi.vkDestroySwapchainKHR (swapchainWrapper.VkSwapchain, nullPtr)
        for i in 0 .. dec swapchainWrapper.RenderSemaphores.Length do DeviceApi.vkDestroySemaphore (swapchainWrapper.RenderSemaphores[i], nullPtr)

/// A swapchain and its assets that may be refreshed for a different screen size.
type Swapchain =
    private
        { SwapchainWrapperOpts_ : SwapchainWrapper option array
          Window_ : SDL_Window nativeptr
          SurfaceFormat_ : VkSurfaceFormatKHR
          mutable SwapchainIndex_ : int }

    /// The underlying vulkan swapchain when available.
    member this.SwapchainWrapperOpt =
        match this.SwapchainWrapperOpts_[this.SwapchainIndex_] with
        | Some wrapper -> Some wrapper
        | None -> None

    static member private destroySwapchainWrappers renderQueue presentQueue swapchain =
        for i in 0 .. dec swapchain.SwapchainWrapperOpts_.Length do
            match swapchain.SwapchainWrapperOpts_[i] with
            | Some swapchainWrapper ->
                SwapchainWrapper.destroy renderQueue presentQueue swapchainWrapper
                swapchain.SwapchainWrapperOpts_[i] <- None
            | None -> ()

    static member private destroySurface renderQueue presentQueue swapchain =
        Swapchain.destroySwapchainWrappers renderQueue presentQueue swapchain
        Hl.destroyVulkanSurface ()

    static member private tryCreateSurfaceAndSwapchainWrapper physicalDevice renderQueue presentQueue swapchain instance =

        // ensure app is in foreground
        if not Hl.Backgrounded then

            // attempt to create surface
            let surfaceState = Hl.tryCreateVulkanSurface swapchain.Window_ instance
            if surfaceState.IsSurfaceReady then

                // check if backgrounding requested during surface creation
                if not (Hl.getBackgroundingRequested ()) then

                    // check that window is not minimized
                    if not (Hl.getWindowMinimized ()) then

                        // try create swapchain wrapper
                        let swapchainWrapperOpt = SwapchainWrapper.tryCreate swapchain.SurfaceFormat_ VkSwapchainKHR.Null physicalDevice
                        swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_] <- swapchainWrapperOpt

                        // destroy surface if lost again or when backgrounding triggered during swapchain creation
                        if  Hl.SurfaceState.IsSurfaceLost ||
                            Hl.getBackgroundingRequested () then
                            Swapchain.destroySurface renderQueue presentQueue swapchain

                // abort
                else Swapchain.destroySurface renderQueue presentQueue swapchain

    /// Attempt to recreate the current vulkan swapchain.
    static member tryRecreate physicalDevice renderQueue presentQueue swapchain instance =

        // handle surface state
        match Hl.SurfaceState with
        
        // attempt to recreate the swapchain, destroying the surface if suddenly lost or if app has/will enter background
        | SurfaceReady ->

            // use current VkSwapchain to create new one
            let oldVkSwapchainOpt =
                match swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_] with
                | Some swapchainWrapper -> if swapchain.SwapchainWrapperOpts_.Length > 1 then swapchainWrapper.VkSwapchain else VkSwapchainKHR.Null
                | None -> VkSwapchainKHR.Null

            // advance swapchain index
            if swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_].IsSome then
                swapchain.SwapchainIndex_ <- inc swapchain.SwapchainIndex_ % swapchain.SwapchainWrapperOpts_.Length

            // destroy SwapchainWrapper at new index if present
            match swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_] with
            | Some swapchainWrapper ->
                SwapchainWrapper.destroy renderQueue presentQueue swapchainWrapper
                swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_] <- None
            | None -> ()
                
            // check once more for app pause (triggered during swapchain destruction) before attempting swapchain creation
            if not (Hl.getBackgroundingRequested ()) then
                
                // check window not minimized
                if not (Hl.getWindowMinimized ()) then
                    
                    // try create new swapchain internal
                    let swapchainWrapperOpt = SwapchainWrapper.tryCreate swapchain.SurfaceFormat_ oldVkSwapchainOpt physicalDevice
                    swapchain.SwapchainWrapperOpts_[swapchain.SwapchainIndex_] <- swapchainWrapperOpt

                    // if surface is lost here (or pause triggered during pipeline creation!), destroy and attempt to recover on the spot
                    if Hl.SurfaceState.IsSurfaceLost || Hl.getBackgroundingRequested () then
                        Swapchain.destroySurface renderQueue presentQueue swapchain
                        Swapchain.tryCreateSurfaceAndSwapchainWrapper physicalDevice renderQueue presentQueue swapchain instance

            // destroy surface and recreate if already possible
            else
                Swapchain.destroySurface renderQueue presentQueue swapchain
                Swapchain.tryCreateSurfaceAndSwapchainWrapper physicalDevice renderQueue presentQueue swapchain instance

        // handle surface loss and attempt to recreate surface and swapchain immediately
        | SurfaceLost ->
            Swapchain.destroySurface renderQueue presentQueue swapchain
            Swapchain.tryCreateSurfaceAndSwapchainWrapper physicalDevice renderQueue presentQueue swapchain instance

        // attempt to recreate surface and swapchain when app is in foreground
        | SurfaceDestroyed ->
            Swapchain.tryCreateSurfaceAndSwapchainWrapper physicalDevice renderQueue presentQueue swapchain instance

    /// Create a Swapchain.
    static member create surfaceFormat physicalDevice window =

        // swapchain index starts at zero
        let swapchainIndex = 0

        // create SwapchainWrapper array
        // NOTE: this must allow for frames in flight plus 1 to prevent destroying semaphores while still in use
        // because swapchain can be refreshed at the end of one frame AND at the beginning of the next, but can still
        // only be refreshed once per frame.
        let swapchainWrapperOpts = Array.create (Constants.Vulkan.FramesInFlight + 1) None

        // try create first SwapchainWrapper if window is not minimized or app paused
        if not (Hl.getBackgroundingRequested ()) && not (Hl.getWindowMinimized ()) then
            let swapchainWrapperOpt = SwapchainWrapper.tryCreate surfaceFormat VkSwapchainKHR.Null physicalDevice
            swapchainWrapperOpts[swapchainIndex] <- swapchainWrapperOpt

        // make Swapchain
        let swapchain =
            { SwapchainWrapperOpts_ = swapchainWrapperOpts
              Window_ = window
              SurfaceFormat_ = surfaceFormat
              SwapchainIndex_ = swapchainIndex }

        // fin
        swapchain
    
    /// Destroy a Swapchain.
    static member destroy swapchain device =
        Swapchain.destroySwapchainWrappers swapchain device

/// Exposes the vulkan handles that must be globally accessible within the renderer.
/// TODO: P1: group fields / properties by role rather than type.
type [<ReferenceEquality>] VulkanContext =
    private
        { Instance_ : VkInstance
          DebugMessengerOpt_ : VkDebugUtilsMessengerEXT option
          PhysicalDevice_ : PhysicalDevice
          Device_ : VkDevice
          VmaAllocator_ : VmaAllocator
          Swapchain_ : Swapchain
          RenderCommandPool_ : VkCommandPool
          TransientCommandPool_ : VkCommandPool
          TextureCommandPool_ : VkCommandPool
          RenderCommandBuffers_ : VkCommandBuffer List
          mutable RenderCommandBuffersCursor_ : int
          RenderQueue_ : ConcurrentCommandQueue
          PresentQueue_ : ConcurrentCommandQueue
          TextureQueue_ : ConcurrentCommandQueue
          SwapchainImageSemaphore_ : VkSemaphore
          mutable RenderFence_ : VkFence
          TransientFence_ : VkFence
          TextureFence_ : VkFence }

    /// The current swapchain image when available.
    member private this.SwapchainWrapperOpt = this.Swapchain_.SwapchainWrapperOpt

    /// The physical device.
    member this.PhysicalDevice = this.PhysicalDevice_

    /// Anisotropy supported.
    member this.AnisotropySupported = this.PhysicalDevice_.SupportsAnisotropy

    /// Maximum anisotropy.
    member this.MaxAnisotropy = this.PhysicalDevice_.Properties.limits.maxSamplerAnisotropy

    /// The vulkan instance API. Provided for use from user lambda callbacks.
    member this.InstanceApi = InstanceApi

    /// The vulkan device API. Provided for use from user lambda callbacks.
    member this.DeviceApi = DeviceApi

    /// The VMA allocator.
    member this.VmaAllocator = this.VmaAllocator_

    /// The command pool for transient command buffers.
    member this.TransientCommandPool = this.TransientCommandPool_

    /// The command pool for texture command buffers.
    member this.TextureCommandPool = this.TextureCommandPool_
    
    /// The current render command buffer.
    member this.RenderCommandBuffer = this.RenderCommandBuffers_[this.RenderCommandBuffersCursor_]

    /// The render command queue.
    member this.RenderQueue = this.RenderQueue_

    /// The texture command queue.
    member this.TextureQueue = this.TextureQueue_
    
    /// The transient fence.
    member this.TransientFence = this.TransientFence_

    /// The texture fence.
    member this.TextureFence = this.TextureFence_

#nowarn 202
    [<UnmanagedCallersOnly (CallConvs = [|typeof<System.Runtime.CompilerServices.CallConvCdecl>|])>]
#warnon 202
    static member private debugCallback
        (messageSeverity : VkDebugUtilsMessageSeverityFlagsEXT)
        (messageType : VkDebugUtilsMessageTypeFlagsEXT)
        (pCallbackData : nativeint)
        (pUserData : nativeint) : uint32 =

        // get callback data
        let callbackData = NativePtr.toByRef (NativePtr.ofNativeInt<VkDebugUtilsMessengerCallbackDataEXT> pCallbackData)
        let message = NativePtr.unmanagedToString callbackData.pMessage

        // determine when to log
        let shouldLog =
            if messageType = VkDebugUtilsMessageTypeFlagsEXT.Performance
            then messageSeverity > VkDebugUtilsMessageSeverityFlagsEXT.Warning
            else messageSeverity > VkDebugUtilsMessageSeverityFlagsEXT.Info

        // construct log header
        if shouldLog then
            match messageSeverity with
            | VkDebugUtilsMessageSeverityFlagsEXT.Verbose -> Log.info message
            | VkDebugUtilsMessageSeverityFlagsEXT.Info -> Log.info message
            | VkDebugUtilsMessageSeverityFlagsEXT.Warning -> Log.warn message
            | VkDebugUtilsMessageSeverityFlagsEXT.Error -> Log.error message
            | _ -> Log.info message

        // finish passively
        ignore pUserData
        0u

    static member private makeDebugMessengerInfo () =
        let mutable info = VkDebugUtilsMessengerCreateInfoEXT ()
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
        let debugCallbackMethod = typeof<VulkanContext>.GetMethod(nameof VulkanContext.debugCallback, BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic).MethodHandle
        let callbackPointer = debugCallbackMethod.GetFunctionPointer () // requires UnmanagedCallersOnly on the function! See https://learn.microsoft.com/en-us/dotnet/api/system.runtimemethodhandle.getfunctionpointer#remarks
        let offset = Marshal.OffsetOf<VkDebugUtilsMessengerCreateInfoEXT> (nameof info.pfnUserCallback)
        let fieldRef = NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&info + offset)
        Unsafe.WriteUnaligned (NativePtr.toByRef<byte> fieldRef, callbackPointer) // TODO: P1: report this F# compiler bug that allows direct assignment to compile without error but causes a crash at runtime.
        info.pUserData <- NativePtr.toVoidPtr NativePtr.nullPtr<byte>
        info

    /// Create the Vulkan instance.
    static member private createVulkanInstance debugInfo =

        // get available instance layers
        let mutable layerCount = 0u
        Vulkan.vkEnumerateInstanceLayerProperties (&&layerCount, nullPtr) |> Hl.check
        let layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
        use layersPin = new ArrayPin<_> (layers)
        Vulkan.vkEnumerateInstanceLayerProperties (&&layerCount, layersPin.Pointer) |> Hl.check

        // check whether validation layer exists
        // TODO: try to automatically prevent validation from interfering with Nsight, starting with VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT.
        let validationLayerName = "VK_LAYER_KHRONOS_validation"
        let validationLayerExists = Array.exists (fun layer -> Hl.getLayerName layer = validationLayerName) layers
        if Constants.Render.RenderDebug && not validationLayerExists then
            Log.info (validationLayerName + " is not available. The Vulkan SDK must be installed to enable validation.")

        // attempt to use validation layer when desired
        let validationLayersActivated = Constants.Render.RenderDebug && validationLayerExists
        use layerWrap = new StringArrayWrap ([|validationLayerName|]) // must remain in scope until vkCreateInstance

        // get vulkan extensions
        let mutable vkExtensionCount = 0u
        let vkExtensions = SDL3.SDL_Vulkan_GetInstanceExtensions &&vkExtensionCount
        let vkExtensionCountInt = int vkExtensionCount
        if NativePtr.isNullPtr vkExtensions then Log.fail (SDL3.SDL_GetError ())

        // get available instance extensions
        let mutable availableExtensionCount = 0u
        Vulkan.vkEnumerateInstanceExtensionProperties (nullPtr, &&availableExtensionCount, nullPtr) |> Hl.check
        let availableExtensionProps = Array.zeroCreate<VkExtensionProperties> (int availableExtensionCount)
        use availableExtensionPropsPin = new ArrayPin<_> (availableExtensionProps)
        Vulkan.vkEnumerateInstanceExtensionProperties (nullPtr, &&availableExtensionCount, availableExtensionPropsPin.Pointer) |> Hl.check
        let availableExtensions = Array.map Hl.getExtensionName availableExtensionProps

        // choose extensions
        use debugUtilsWrap = new StringWrap (Vulkan.VK_EXT_DEBUG_UTILS_EXTENSION_NAME)
        let extensions =
            Array.init
                (vkExtensionCountInt + if validationLayersActivated then 1 else 0)
                (fun i -> if i < vkExtensionCountInt then NativePtr.get vkExtensions i else debugUtilsWrap.Pointer)

        // check for portability enumeration extension - using MoltenVK in place of Vulkan loader won't support it (on iOS Simulator),
        // while using MoltenVK from Vulkan loader (on iOS device / macOS) requires it
        let portabilityEnumeration = NativePtr.spanToString Vulkan.VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
        use portabilityWrap = new StringWrap (portabilityEnumeration)
        let portabilityEnumerationAvailable = Array.contains portabilityEnumeration availableExtensions
        let extensions =
            if Constants.Vulkan.MoltenVk && portabilityEnumerationAvailable
            then Array.append extensions [|portabilityWrap.Pointer|]
            else extensions
        use extensionsPin = new ArrayPin<_> (extensions)

        // configure app info
        let mutable appInfo = VkApplicationInfo ()
        appInfo.apiVersion <- VkVersion.Version_1_3 // NOTE: this is the _max_ Vulkan version, not the _required_ version.

        // create instance
        let mutable instanceInfo = VkInstanceCreateInfo ()
        instanceInfo.pApplicationInfo <- &&appInfo
        instanceInfo.enabledExtensionCount <- uint extensions.Length
        instanceInfo.ppEnabledExtensionNames <- extensionsPin.Pointer
        if Constants.Vulkan.MoltenVk && portabilityEnumerationAvailable then
            instanceInfo.flags <- VkInstanceCreateFlags.EnumeratePortabilityKHR
        if validationLayersActivated then
            let mutable debugInfo = debugInfo
            instanceInfo.pNext <- asVoidPtr &debugInfo
            instanceInfo.enabledLayerCount <- 1u
            instanceInfo.ppEnabledLayerNames <- layerWrap.Pointer
        let mutable instance = Unchecked.defaultof<VkInstance>
        Vulkan.vkCreateInstance (&instanceInfo, nullPtr, &instance) |> Hl.check
        SetInstanceApi (Vulkan.GetApi instance)
        (validationLayersActivated, instance)

    // TODO: try separate this from validation status, same for create instance debug.
    static member private tryCreateDebugMessenger validationLayersActivated info =
        if validationLayersActivated then
            let mutable debugMessenger = Unchecked.defaultof<VkDebugUtilsMessengerEXT>
            InstanceApi.vkCreateDebugUtilsMessengerEXT (&info, nullPtr, &debugMessenger) |> Hl.check
            Some debugMessenger
        else None
    
    /// Select compatible physical device when available.
    static member private trySelectPhysicalDevice () =

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

        // log device selection process
        Log.info "Selecting Vulkan Device..."

        // get available physical devices
        let mutable deviceCount = 0u
        InstanceApi.vkEnumeratePhysicalDevices &deviceCount |> Hl.check
        let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
        use devicesPin = new ArrayPin<_> (devices)
        InstanceApi.vkEnumeratePhysicalDevices (&&deviceCount, devicesPin.Pointer) |> Hl.check

        // gather devices together with relevant data for selection
        let candidates =
            [for i in 0 .. dec devices.Length do
                match PhysicalDevice.tryMake devices[i] with
                | Some physicalDevice -> physicalDevice
                | None -> ()]

        // filter and order candidates according to criteria
        let candidatesFiltered = List.filter isCompatible candidates
        let (fstChoice, sndChoice) = List.partition isPreferable candidatesFiltered
        let candidatesFilteredAndOrdered = List.append fstChoice sndChoice

        // attempt to select the most preferable compatible device
        let physicalDeviceOpt =

            // return the first along with its data
            if candidatesFilteredAndOrdered.Length > 0 then

                // select physical device
                let physicalDevice = List.head candidatesFilteredAndOrdered

                // log device information
                let properties = physicalDevice.Properties
                let deviceName = NativePtr.unmanagedToString &&properties.deviceName.FixedElementField
                Log.info (sprintf "Selected Vulkan Device %s, Driver v%u.%u.%u(%u)" deviceName properties.apiVersion.Major properties.apiVersion.Minor properties.apiVersion.Patch properties.apiVersion.Variant)
                if not physicalDevice.SupportsAnisotropy then Log.warn "Graphics device does not support anisotropy."

                // return physical device
                Some physicalDevice

            // otherwise error
            else Log.error "Could not find a suitable Vulkan Device."; None

        // fin
        physicalDeviceOpt
    
    /// Create the logical device.
    static member private createLogicalDevice instance (physicalDevice : PhysicalDevice) =

        // configure MoltenVK features
        let portabilitySubsetExtensionName = NativePtr.spanToString Vulkan.VK_KHR_PORTABILITY_SUBSET_EXTENSION_NAME
        let portabilitySubsetAvailable =
            Array.exists (fun ext -> Hl.getExtensionName ext = portabilitySubsetExtensionName) physicalDevice.Extensions
        let mutable portabilityFeatures = VkPhysicalDevicePortabilitySubsetFeaturesKHR ()
        portabilityFeatures.imageViewFormatSwizzle <- true

        // configure Vulkan 1.3 features
        let mutable vulkan13 = VkPhysicalDeviceVulkan13Features ()
        vulkan13.dynamicRendering <- true
        if Constants.Vulkan.MoltenVk && portabilitySubsetAvailable then vulkan13.pNext <- asVoidPtr &portabilityFeatures

        // queue create infos
        let mutable queuePriority = 1.0f
        let queueCreateInfosList = List ()
        let mutable qInfo = VkDeviceQueueCreateInfo ()
        qInfo.queueFamilyIndex <- physicalDevice.GraphicsQueueFamily
        qInfo.queueCount <- min 2u physicalDevice.GraphicsQueueCount
        qInfo.pQueuePriorities <- &&queuePriority
        queueCreateInfosList.Add qInfo
        if physicalDevice.GraphicsQueueFamily <> physicalDevice.PresentQueueFamily then
            let mutable qInfo = VkDeviceQueueCreateInfo ()
            qInfo.queueFamilyIndex <- physicalDevice.PresentQueueFamily
            qInfo.queueCount <- 1u
            qInfo.pQueuePriorities <- &&queuePriority
            queueCreateInfosList.Add qInfo
        let queueCreateInfos = queueCreateInfosList.ToArray ()
        use queueCreateInfosPin = new ArrayPin<_> (queueCreateInfos)

        // get swapchain extension
        let swapchainExtensionName = NativePtr.spanToString Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME
        let extensionArray =
            if Constants.Vulkan.MoltenVk && portabilitySubsetAvailable
            then [|swapchainExtensionName; portabilitySubsetExtensionName|]
            else [|swapchainExtensionName|]
        use extensionArrayWrap = new StringArrayWrap (extensionArray)

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
        info.pEnabledFeatures <- &&features
        let mutable device = Unchecked.defaultof<VkDevice>
        InstanceApi.vkCreateDevice (physicalDevice.VkPhysicalDevice, &info, nullPtr, &device) |> Hl.check
        SetDeviceApi (Vulkan.GetApi (instance, device))
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
    static member private createCommandPool transient queueFamilyIndex =

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
        DeviceApi.vkCreateCommandPool (&info, nullPtr, &commandPool) |> Hl.check
        commandPool

    static member private beginRenderCommandBuffer context =
        
        // allocate command buffer if needed
        if context.RenderCommandBuffersCursor_ >= context.RenderCommandBuffers_.Count then
            let buffers = Hl.allocateCommandBuffers context.RenderCommandBuffers_.Count VkCommandBufferLevel.Primary context.RenderCommandPool_
            context.RenderCommandBuffers_.AddRange buffers
        let commandBuffer = context.RenderCommandBuffers_[context.RenderCommandBuffersCursor_]

        // prepare command buffer for immediate use
        DeviceApi.vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None) |> Hl.check
        let mutable beginInfo = VkCommandBufferBeginInfo ()
        DeviceApi.vkBeginCommandBuffer (commandBuffer, &&beginInfo) |> Hl.check

    static member private endRenderCommandBuffer waitForSwapchainImage signalRender context =

        // lock to get access to vulkan queue
        ConcurrentCommandQueue.withLock context.RenderQueue_ $ fun vkQueue ->

            // end command buffer
            let mutable commandBuffer = context.RenderCommandBuffers_[context.RenderCommandBuffersCursor_]
            DeviceApi.vkEndCommandBuffer commandBuffer |> Hl.check

            // populate common submit info values, adding additional optionally utilized vulkan values on the stack
            let mutable submitInfo = VkSubmitInfo ()
            submitInfo.commandBufferCount <- 1u
            submitInfo.pCommandBuffers <- &&commandBuffer
            let mutable swapchainImageSemaphoreOpt = VkSemaphore.Null
            let mutable stageFlagOpt = VkPipelineStageFlags.None
            let mutable renderSemaphoreOpt = VkSemaphore.Null
            let mutable renderFenceOpt = VkFence.Null

            // optionally wait for swapchain image
            if waitForSwapchainImage then
                swapchainImageSemaphoreOpt <- context.SwapchainImageSemaphore_
                stageFlagOpt <- VkPipelineStageFlags.AllCommands
                submitInfo.waitSemaphoreCount <- 1u
                submitInfo.pWaitSemaphores <- &&swapchainImageSemaphoreOpt
                submitInfo.pWaitDstStageMask <- &&stageFlagOpt
                
            // optionally signal rendering finished
            if signalRender then
                match context.SwapchainWrapperOpt with
                | Some swapchainWrapper ->
                    renderSemaphoreOpt <- swapchainWrapper.RenderSemaphore
                    submitInfo.signalSemaphoreCount <- 1u
                    submitInfo.pSignalSemaphores <- &&renderSemaphoreOpt
                | None -> Log.info "Swapchain wrapper absent."
                renderFenceOpt <- context.RenderFence_

            // submit commands
            DeviceApi.vkQueueSubmit (vkQueue, 1u, &&submitInfo, renderFenceOpt) |> Hl.check

            // advance cursor
            context.RenderCommandBuffersCursor_ <- inc context.RenderCommandBuffersCursor_

    /// Indicate that the current render command buffer is ready for submission and a new one shall be started.
    static member advanceRenderCommandBuffer context =
        VulkanContext.endRenderCommandBuffer false false context
        VulkanContext.beginRenderCommandBuffer context

    static member private withSwapchainWrapper (context : VulkanContext) op =
        let shouldRecreate =
            if Hl.getBackgroundingRequested () then false // don't recreate when going into background
            elif Hl.getWindowMinimized () then false // don't recreate when minimized
            else match context.SwapchainWrapperOpt with
                 | Some swapchainWrapper ->
                    match Hl.tryGetSurfaceCapabilities context.PhysicalDevice_.VkPhysicalDevice with
                    | Some capabilities ->
                        match Hl.tryGetSwapExtent capabilities with
                        | Some swapExtent when swapExtent = swapchainWrapper.SwapExtent -> op swapchainWrapper // perform operation
                        | Some _ | None -> true // recreate when swap extent is invalid
                    | None -> true // recreate when surface is absent
                 | None -> Log.info "Swapchain wrapper absent."; true // recreate when swapchain is absent
        if shouldRecreate then
            Log.info "Recreating swapchain..."
            Swapchain.tryRecreate context.PhysicalDevice_ context.RenderQueue_ context.PresentQueue_ context.Swapchain_ context.Instance_

    /// Begin the frame.
    static member beginFrame resolveImage context =

        // await render fence
        // NOTE: on Android on my A17, we have to put vkWaitForFences in a loop because it will return before the given
        // timeout with a VkResult.Timeout result (which I'm not sure is standard-conformant).
        let mutable waiting = true
        while waiting do
            let result = DeviceApi.vkWaitForFences (1u, &&context.RenderFence_, true, UInt64.MaxValue)
            if result <> VkResult.Timeout then
                waiting <- false
                Hl.check result
        DeviceApi.vkResetFences (1u, &&context.RenderFence_) |> Hl.check

        // reset render command buffers cursor
        context.RenderCommandBuffersCursor_ <- 0

        // reset draw counters
        Hl.resetDrawCounters ()

        // attempt to recreate the swapchain when needed
        VulkanContext.withSwapchainWrapper context absurdity

        // begin render command recording
        VulkanContext.beginRenderCommandBuffer context

        // clear resolve texture
        Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color ColorAttachmentRead TransferDst resolveImage context.RenderCommandBuffer
        let clearColor = Constants.Render.WindowClearColor
        let mutable clearColorValue = VkClearColorValue (clearColor.R, clearColor.G, clearColor.B, clearColor.A)
        let mutable subresourceRange = Hl.makeSubresourceRange 0 1 0 1 VkImageAspectFlags.Color
        DeviceApi.vkCmdClearColorImage (context.RenderCommandBuffer, resolveImage, TransferDst.VkImageLayout, &&clearColorValue, 1u, &&subresourceRange)

        // begin main rendering path by making resolve texture ready for rendering
        Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color TransferDst ColorAttachmentWrite resolveImage context.RenderCommandBuffer

    /// End the frame.
    static member endFrame windowViewport resolveImage (context : VulkanContext) =

        // request swapchain image
        VulkanContext.withSwapchainWrapper context $ fun swapchainWrapper ->
            let mutable imageIndex = Hl.ImageIndex
            let result = DeviceApi.vkAcquireNextImageKHR (swapchainWrapper.VkSwapchain, UInt64.MaxValue, context.SwapchainImageSemaphore_, VkFence.Null, &imageIndex)
            Hl.setImageIndex imageIndex
            match result with
            | VkResult.ErrorOutOfDateKHR -> true // recreate when swapchain is out of date
            | VkResult.ErrorSurfaceLostKHR -> Hl.notifySurfaceLost (); true // recreate when surface is lost
            | _ -> false // no swapchain recreation

        // await swapchain image then end main rendering path
        VulkanContext.endRenderCommandBuffer true false context

        // now that swapchain image is available, begin resolve rendering path
        VulkanContext.beginRenderCommandBuffer context

        // attempt to blit resolve texture to swapchain image
        VulkanContext.withSwapchainWrapper context $ fun swapchainWrapper ->
            let swapchainImage = swapchainWrapper.Images[int Hl.ImageIndex]
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color ColorAttachmentWrite TransferSrc resolveImage context.RenderCommandBuffer
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color Undefined TransferDst swapchainImage context.RenderCommandBuffer
            let bounds = VkRect2D (0, 0, uint windowViewport.Outer.Size.X, uint windowViewport.Outer.Size.Y)
            let mutable region = Hl.makeBlit 0 0 0 0 bounds bounds
            DeviceApi.vkCmdBlitImage (context.RenderCommandBuffer, resolveImage, TransferSrc.VkImageLayout, swapchainImage, TransferDst.VkImageLayout, 1u, &&region, VkFilter.Nearest)
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color TransferDst Present swapchainImage context.RenderCommandBuffer
            Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color TransferSrc ColorAttachmentWrite resolveImage context.RenderCommandBuffer
            false // no swapchain recreation

        // transition resolve image back to read
        Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead resolveImage context.RenderCommandBuffer

        // end resolve rendering path, signaling render semaphore and fence
        VulkanContext.endRenderCommandBuffer false true context

    /// Present the image back to the swapchain to appear on screen.
    static member present (context : VulkanContext) =

        // lock to get access to vulkan queue
        ConcurrentCommandQueue.withLock context.PresentQueue_ $ fun vkQueue ->

            // attempt to await render semaphore and then present image
            VulkanContext.withSwapchainWrapper context $ fun swapchainWrapper ->
                let mutable renderSemaphore = swapchainWrapper.RenderSemaphore
                let mutable vkSwapchain = swapchainWrapper.VkSwapchain
                let mutable imageIndex = Hl.ImageIndex
                let mutable info = VkPresentInfoKHR ()
                info.waitSemaphoreCount <- 1u
                info.pWaitSemaphores <- &&renderSemaphore
                info.swapchainCount <- 1u
                info.pSwapchains <- &&vkSwapchain
                info.pImageIndices <- &&imageIndex
                match DeviceApi.vkQueuePresentKHR (vkQueue, &&info) with
                | VkResult.ErrorOutOfDateKHR -> true // recreate when swapchain is out of data
                | VkResult.ErrorSurfaceLostKHR -> Hl.notifySurfaceLost (); true // recreate when surface is lost
                | _ -> false // no swapchain recreation

    /// Wait for all device operations to complete before cleaning up resources.
    static member waitIdle context =

        // NOTE: we never call vkDeviceWaitIdle as its implementation compromises queue thread safety.
        ConcurrentCommandQueue.waitIdle context.RenderQueue_
        ConcurrentCommandQueue.waitIdle context.PresentQueue_
        ConcurrentCommandQueue.waitIdle context.TextureQueue_

    /// Attempt to create a VulkanContext.
    /// NOTE: this procedure is intended to be invoked from the main thread to satisfy the requirements of Mac and
    /// iOS surface creation, and possibly other platforms.
    static member tryCreate window =

        // load vulkan; not vulkan function
        Vulkan.vkInitialize () |> Hl.check

        // make debug info
        let debugInfo = VulkanContext.makeDebugMessengerInfo ()

        // create instance
        let (validationLayersActivated, instance) = VulkanContext.createVulkanInstance debugInfo

        // create debug messenger if validation activated
        let debugMessengerOpt = VulkanContext.tryCreateDebugMessenger validationLayersActivated debugInfo

        // create surface
        Hl.createVulkanSurface window instance

        // attempt to select physical device
        match VulkanContext.trySelectPhysicalDevice () with
        | Some physicalDevice ->

            // create device
            let device = VulkanContext.createLogicalDevice instance physicalDevice

            // create vma allocator
            let allocator = VulkanContext.createVmaAllocator physicalDevice device instance

            // create render queue
            let renderQueue = ConcurrentCommandQueue.create physicalDevice.GraphicsQueueFamily 0u

            // create seperate present queue if graphics queue family does not support presentation
            let presentQueue =
                if physicalDevice.GraphicsQueueFamily <> physicalDevice.PresentQueueFamily
                then ConcurrentCommandQueue.create physicalDevice.PresentQueueFamily 0u
                else renderQueue

            // create seperate queue for texture server thread if available
            let textureQueue =
                if physicalDevice.GraphicsQueueCount > 1u
                then ConcurrentCommandQueue.create physicalDevice.GraphicsQueueFamily 1u
                else renderQueue

            // setup execution for rendering on render thread
            let renderFence = Hl.createFence true
            let renderCommandPool = VulkanContext.createCommandPool false physicalDevice.GraphicsQueueFamily
            let renderCommandBuffers = Hl.allocateCommandBuffers Constants.Vulkan.RenderCommandBufferCountDefault VkCommandBufferLevel.Primary renderCommandPool

            // setup execution for presentation on render thread
            let swapchainImageSemaphore = Hl.createSemaphore ()

            // setup transient (one time) execution on render thread
            let transientCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily
            let transientFence = Hl.createFence false

            // setup transient (one time) execution on texture server thread
            let textureCommandPool = VulkanContext.createCommandPool true physicalDevice.GraphicsQueueFamily
            let textureFence = Hl.createFence false

            // setup swapchain
            let surfaceFormat = VulkanContext.getSurfaceFormat physicalDevice.SurfaceFormats
            let swapchain = Swapchain.create surfaceFormat physicalDevice window

            // make VulkanContext
            let vulkanContext =
                { Instance_ = instance
                  DebugMessengerOpt_ = debugMessengerOpt
                  PhysicalDevice_ = physicalDevice
                  Device_ = device
                  VmaAllocator_ = allocator
                  Swapchain_ = swapchain
                  RenderCommandPool_ = renderCommandPool
                  TransientCommandPool_ = transientCommandPool
                  TextureCommandPool_ = textureCommandPool
                  RenderCommandBuffers_ = List renderCommandBuffers
                  RenderCommandBuffersCursor_ = 0
                  RenderQueue_ = renderQueue
                  PresentQueue_ = presentQueue
                  TextureQueue_ = textureQueue
                  SwapchainImageSemaphore_ = swapchainImageSemaphore
                  RenderFence_ = renderFence
                  TransientFence_ = transientFence
                  TextureFence_ = textureFence }

            // success
            Some vulkanContext

        // failure
        | None -> None

    /// Clean-up a vulkan context.
    /// NOTE: intended to be invoked from the main thread.
    static member cleanUp context =
        Swapchain.destroy context.RenderQueue_ context.PresentQueue_ context.Swapchain_
        DeviceApi.vkDestroySemaphore (context.SwapchainImageSemaphore_, nullPtr)
        DeviceApi.vkDestroyFence (context.RenderFence_, nullPtr)
        DeviceApi.vkDestroyFence (context.TransientFence, nullPtr)
        DeviceApi.vkDestroyFence (context.TextureFence, nullPtr)
        DeviceApi.vkDestroyCommandPool (context.RenderCommandPool_, nullPtr)
        DeviceApi.vkDestroyCommandPool (context.TransientCommandPool, nullPtr)
        DeviceApi.vkDestroyCommandPool (context.TextureCommandPool_, nullPtr)
        Vma.vmaDestroyAllocator context.VmaAllocator
        DeviceApi.vkDestroyDevice (nullPtr)
        Hl.destroyVulkanSurface ()
        match context.DebugMessengerOpt_ with Some debugMessenger -> InstanceApi.vkDestroyDebugUtilsMessengerEXT (debugMessenger, nullPtr) | None -> ()
        InstanceApi.vkDestroyInstance nullPtr