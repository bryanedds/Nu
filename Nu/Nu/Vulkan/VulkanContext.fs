// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Runtime.InteropServices
open System.Collections.Generic
open System.Numerics
open System.Reflection
open System.Runtime.CompilerServices
open FSharp.NativeInterop
open SDL
open Vortice.Vulkan
open Prime
open Nu

type CommandBufferSubmissionType =
    | FirstSubmission
    | MiddleSubmission
    | LastSubmission

/// A command queue that internally synchronizes use across multiple threads.
type [<ReferenceEquality>] ConcurrentCommandQueue =
    private
        { VkQueue_ : VkQueue
          Lock_ : obj }

    /// Perform an arbitrary operation on the internal vulkan queue.
    static member withLock queue (op : VkQueue -> unit) =
        //let sw = System.Diagnostics.Stopwatch.StartNew ()
        lock queue.Lock_ (fun () ->
            //sw.Stop ()
            //Log.info ("ConcurrentCommandQueue.withLock: " + string sw.ElapsedTicks)
            op queue.VkQueue_) : unit

    /// Wait for Queue to finish execution.
    static member waitIdle queue =
        ConcurrentCommandQueue.withLock queue (fun vkQueue -> Vulkan.vkQueueWaitIdle vkQueue |> Hl.check)

    /// Execute and free transient command buffer. Command pool and fence must NOT be shared between threads!
    static member executeTransient commandBuffer commandPool finishFence (commandQueue : ConcurrentCommandQueue) device =

        // lock to get access to vulkan queue
        let mutable commandBuffer = commandBuffer
        ConcurrentCommandQueue.withLock commandQueue (fun vkQueue ->
        
            // end command buffer
            Vulkan.vkEndCommandBuffer commandBuffer |> Hl.check

            // submit commands
            let mutable info = VkSubmitInfo ()
            info.commandBufferCount <- 1u
            info.pCommandBuffers <- &&commandBuffer
            Vulkan.vkQueueSubmit (vkQueue, 1u, &&info, finishFence) |> Hl.check

            // wait for execution to finish
            Hl.awaitFence finishFence device

            // free command buffer
            Vulkan.vkFreeCommandBuffers (device, commandPool, 1u, &&commandBuffer))

    /// Create a ConcurrentCommandQueue.
    static member create queueFamilyIndex queueIndex device =
        let mutable vkQueue = Unchecked.defaultof<VkQueue>
        Vulkan.vkGetDeviceQueue (device, queueFamilyIndex, queueIndex, &vkQueue)
        let queue = { VkQueue_ = vkQueue; Lock_ = obj () }
        queue

/// A physical device and associated data.
type PhysicalDevice =
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
        if Hl.getBackgroundingRequested () then
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
        Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, &&extensionCount, nullPtr) |> Hl.check
        let extensions = Array.zeroCreate<VkExtensionProperties> (int extensionCount)
        use extensionsPin = new ArrayPin<_> (extensions)
        Vulkan.vkEnumerateDeviceExtensionProperties (vkPhysicalDevice, nullPtr, &&extensionCount, extensionsPin.Pointer) |> Hl.check
        extensions

    /// Get available surface formats.
    static member private getSurfaceFormats vkPhysicalDevice window instance =
        PhysicalDevice.checkSurface window instance
        let mutable formatCount = 0u
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, Hl.Surface, &&formatCount, nullPtr) |> Hl.check
        let formats = Array.zeroCreate<VkSurfaceFormatKHR> (int formatCount)
        use formatsPin = new ArrayPin<_> (formats)
        Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR (vkPhysicalDevice, Hl.Surface, &&formatCount, formatsPin.Pointer) |> Hl.check
        formats

    /// Get surface capabilities.
    static member private getSurfaceCapabilities vkPhysicalDevice window instance =
        PhysicalDevice.checkSurface window instance
        let mutable capabilities = Unchecked.defaultof<VkSurfaceCapabilitiesKHR>
        Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR (vkPhysicalDevice, Hl.Surface, &capabilities) |> Hl.check
        capabilities
    
    /// Attempt to get the queue families.
    static member private tryGetQueueFamilies vkPhysicalDevice window instance =

        // check surface is still valid
        PhysicalDevice.checkSurface window instance
        
        // get queue families' properties
        let mutable queueFamilyCount = 0u
        Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, &&queueFamilyCount, nullPtr)
        let queueFamilyProps = Array.zeroCreate<VkQueueFamilyProperties> (int queueFamilyCount)
        use queueFamilyPropsPin = new ArrayPin<_> (queueFamilyProps)
        Vulkan.vkGetPhysicalDeviceQueueFamilyProperties (vkPhysicalDevice, &&queueFamilyCount, queueFamilyPropsPin.Pointer)

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
                let props = queueFamilyProps[i]
                if props.queueFlags &&& VkQueueFlags.Graphics <> VkQueueFlags.None then
                    graphicsQueueFamilyOpt <- Some (uint i, props.queueCount)
            | Some _ -> ()

            // try get present queue family
            match presentQueueFamilyOpt with
            | None ->
                let mutable presentSupport = VkBool32.False
                Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR (vkPhysicalDevice, uint i, Hl.Surface, &presentSupport) |> Hl.check
                if presentSupport = VkBool32.True then
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
type SwapchainSingleton =
    { VkSwapchain : VkSwapchainKHR
      Images : VkImage array
      ImageViews : VkImageView array
      SwapExtent : VkExtent2D }

    /// Try create the VkSwapchain.
    static member private tryCreateVkSwapchain (surfaceFormat : VkSurfaceFormatKHR) oldVkSwapchainOpt physicalDevice window device =
        match Hl.tryGetSurfaceCapabilities physicalDevice.VkPhysicalDevice with
        | Some capabilities ->

            // get swap extent
            let swapExtent =
                Hl.getSwapExtent capabilities window

            // decide the minimum number of images in the swapchain. Sellers, Vulkan Programming Guide p. 144, recommends
            // at least 3 for performance, but to keep latency low let's start with the more conservative recommendation of
            // https://vulkan-tutorial.com/Drawing_a_triangle/Presentation/Swap_chain#page_Creating-the-swap-chain.
            let minImageCount =
                if capabilities.maxImageCount = 0u
                then capabilities.minImageCount + 1u
                else min (capabilities.minImageCount + 1u) capabilities.maxImageCount

            // check that we can use a more efficient mailbox-based present mode
            let canUseMailbox =
                let presentModes = Hl.getPresentModes physicalDevice.VkPhysicalDevice
                Array.contains VkPresentModeKHR.Mailbox presentModes

            // create swapchain
            let indicesArray = [|physicalDevice.GraphicsQueueFamily; physicalDevice.PresentQueueFamily|]
            use indicesArrayPin = new ArrayPin<_> (indicesArray)
            let mutable info = VkSwapchainCreateInfoKHR ()
            info.surface <- Hl.Surface
            info.minImageCount <- minImageCount
            info.imageFormat <- surfaceFormat.format
            info.imageColorSpace <- surfaceFormat.colorSpace
            info.imageExtent <- swapExtent
            info.imageArrayLayers <- 1u
            info.imageUsage <- VkImageUsageFlags.ColorAttachment ||| VkImageUsageFlags.TransferDst
            if physicalDevice.GraphicsQueueFamily = physicalDevice.PresentQueueFamily then
                info.imageSharingMode <- VkSharingMode.Exclusive
            else
                info.imageSharingMode <- VkSharingMode.Concurrent
                info.queueFamilyIndexCount <- 2u
                info.pQueueFamilyIndices <- indicesArrayPin.Pointer
            info.preTransform <- VkSurfaceTransformFlagsKHR.Identity
            info.compositeAlpha <-
                if capabilities.supportedCompositeAlpha &&& VkCompositeAlphaFlagsKHR.Opaque <> VkCompositeAlphaFlagsKHR.None then VkCompositeAlphaFlagsKHR.Opaque
                elif capabilities.supportedCompositeAlpha &&& VkCompositeAlphaFlagsKHR.PreMultiplied <> VkCompositeAlphaFlagsKHR.None then VkCompositeAlphaFlagsKHR.PreMultiplied
                elif capabilities.supportedCompositeAlpha &&& VkCompositeAlphaFlagsKHR.PostMultiplied <> VkCompositeAlphaFlagsKHR.None then VkCompositeAlphaFlagsKHR.PostMultiplied
                else VkCompositeAlphaFlagsKHR.Inherit
            info.presentMode <-
                if Constants.Render.Vsync then
                    if canUseMailbox
                    then VkPresentModeKHR.Mailbox
                    else VkPresentModeKHR.Fifo
                else VkPresentModeKHR.Immediate
            info.clipped <- true
            info.oldSwapchain <- oldVkSwapchainOpt
            let mutable vkSwapchain = Unchecked.defaultof<VkSwapchainKHR>
            let result = Vulkan.vkCreateSwapchainKHR (device, &info, nullPtr, &vkSwapchain)
            
            // fail if surface is lost
            if result <> VkResult.ErrorSurfaceLostKHR then
                Hl.check result
                Some (vkSwapchain, swapExtent)
            else
                Hl.SurfaceState <- SurfaceLost
                None

        | None -> None

    /// Get swapchain images.
    static member private getSwapchainImages vkSwapchain device =
        let mutable imageCount = 0u
        Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, &&imageCount, nullPtr) |> Hl.check
        let images = Array.zeroCreate<VkImage> (int imageCount)
        use imagesPin = new ArrayPin<_> (images)
        Vulkan.vkGetSwapchainImagesKHR (device, vkSwapchain, &&imageCount, imagesPin.Pointer) |> Hl.check
        images

    /// Create the image views.
    static member private createImageViews format (images : VkImage array) device =
        let imageViews = Array.zeroCreate<VkImageView> images.Length
        for i in 0 .. dec imageViews.Length do imageViews[i] <- Hl.createImageView Rgba format 0 1 0 1 VkImageViewType.Image2D VkImageAspectFlags.Color images[i] device
        imageViews
    
    /// Try create a SwapchainSingleton.
    static member tryCreate surfaceFormat oldVkSwapchainOpt physicalDevice window device =
        
        // try create vkSwapchain and its assets
        match SwapchainSingleton.tryCreateVkSwapchain surfaceFormat oldVkSwapchainOpt physicalDevice window device with
        | Some (vkSwapchain, swapExtent) ->

            // make SwapchainSingleton
            let images = SwapchainSingleton.getSwapchainImages vkSwapchain device
            let imageViews = SwapchainSingleton.createImageViews surfaceFormat.format images device
            let swapchainSingleton =
                { VkSwapchain = vkSwapchain
                  Images = images
                  ImageViews = imageViews
                  SwapExtent = swapExtent }

            // fin
            Some swapchainSingleton
        | None -> None
    
    /// Destroy a SwapchainSingleton.
    static member destroy renderQueue presentQueue swapchainSingleton device =
        
        // TODO: DJL: this is not sufficient to ensure resources not still in use, that requires an extension!!
        // https://docs.vulkan.org/guide/latest/swapchain_semaphore_reuse.html#_vk_ext_swapchain_maintenance1_extension
        ConcurrentCommandQueue.waitIdle renderQueue
        ConcurrentCommandQueue.waitIdle presentQueue
        for i in 0 .. dec swapchainSingleton.ImageViews.Length do Vulkan.vkDestroyImageView (device, swapchainSingleton.ImageViews[i], nullPtr)
        Vulkan.vkDestroySwapchainKHR (device, swapchainSingleton.VkSwapchain, nullPtr)

/// A swapchain and its assets that may be refreshed for a different screen size.
type Swapchain =
    private
        { SwapchainSingletonOpts_ : SwapchainSingleton option array
          Window_ : SDL_Window nativeptr
          SurfaceFormat_ : VkSurfaceFormatKHR
          mutable SwapchainIndex_ : int }

    /// The current SwapchainSingletonOpt.
    member this.SwapchainSingletonOpt = this.SwapchainSingletonOpts_[this.SwapchainIndex_]
    
    /// The Vulkan swapchain itself.
    member this.VkSwapchain = (Option.get this.SwapchainSingletonOpts_[this.SwapchainIndex_]).VkSwapchain

    /// The number of swapchain images.
    member this.ImageCount = (Option.get this.SwapchainSingletonOpts_[this.SwapchainIndex_]).Images.Length
    
    /// The current swapchain image.
    member this.Image = (Option.get this.SwapchainSingletonOpts_[this.SwapchainIndex_]).Images[int Hl.ImageIndex]

    /// The image view for the current swapchain image.
    member this.ImageView = (Option.get this.SwapchainSingletonOpts_[this.SwapchainIndex_]).ImageViews[int Hl.ImageIndex]

    /// The swap extent of the current vkSwapchain.
    member this.SwapExtent = (Option.get this.SwapchainSingletonOpts_[this.SwapchainIndex_]).SwapExtent

    static member private clear renderQueue presentQueue swapchain device =
        for i in 0 .. dec swapchain.SwapchainSingletonOpts_.Length do
            match swapchain.SwapchainSingletonOpts_[i] with
            | Some swapchainSingleton ->
                SwapchainSingleton.destroy renderQueue presentQueue swapchainSingleton device
                swapchain.SwapchainSingletonOpts_[i] <- None
            | None -> ()
    
    static member private destroySurface renderQueue presentQueue swapchain device instance =
        Swapchain.clear renderQueue presentQueue swapchain device // must do this first
        Hl.destroyVulkanSurface instance
    
    static member private tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance =
        
        // check if app is not in background
        if not (Hl.getBackgrounded ()) then
            
            // ensure surface creation was successful
            if Hl.tryCreateVulkanSurface swapchain.Window_ instance = SurfaceReady then

                // check if pause triggered during surface creation
                if not (Hl.getBackgroundingRequested ()) then
                
                    // check window not minimized
                    if not (Swapchain.isWindowMinimized swapchain.Window_) then

                        // try create SwapchainSingleton
                        let swapchainSingletonOpt = SwapchainSingleton.tryCreate swapchain.SurfaceFormat_ VkSwapchainKHR.Null physicalDevice swapchain.Window_ device
                        swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] <- swapchainSingletonOpt
                        
                        // destroy surface if lost again or if pause triggered during swapchain creation
                        if Hl.SurfaceState = SurfaceLost || Hl.getBackgroundingRequested ()
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
        match Hl.SurfaceState with
        
        // attempt to recreate the swapchain, destroying the surface if suddenly lost or if app has/will enter background
        | SurfaceReady ->
        
            // check if app has or will enter background, if not then just try recreate swapchain
            if not (Hl.getBackgroundingRequested ()) then
            
                // use current VkSwapchain to create new one
                let oldVkSwapchainOpt =
                    match swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] with
                    | Some swapchainSingleton -> if swapchain.SwapchainSingletonOpts_.Length > 1 then swapchainSingleton.VkSwapchain else VkSwapchainKHR.Null
                    | None -> VkSwapchainKHR.Null

                // advance swapchain index
                if Option.isSome swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] then
                    swapchain.SwapchainIndex_ <- (inc swapchain.SwapchainIndex_) % swapchain.SwapchainSingletonOpts_.Length

                // destroy SwapchainSingleton at new index if present
                match swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] with
                | Some swapchainSingleton ->
                    SwapchainSingleton.destroy renderQueue presentQueue swapchainSingleton device
                    swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] <- None
                | None -> ()
                
                // check once more for app pause (triggered during swapchain destruction) before attempting swapchain creation
                if not (Hl.getBackgroundingRequested ()) then
                
                    // check window not minimized
                    if not (Swapchain.isWindowMinimized swapchain.Window_) then
                    
                        // try create new swapchain internal
                        let swapchainSingletonOpt = SwapchainSingleton.tryCreate swapchain.SurfaceFormat_ oldVkSwapchainOpt physicalDevice swapchain.Window_ device
                        swapchain.SwapchainSingletonOpts_[swapchain.SwapchainIndex_] <- swapchainSingletonOpt

                        // if surface is lost here (or pause triggered during pipeline creation!), destroy and attempt to recover on the spot
                        if Hl.SurfaceState = SurfaceLost || Hl.getBackgroundingRequested () then
                            Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                            Swapchain.tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance

                // destroy surface and recreate if already possible
                else
                    Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                    Swapchain.tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance

            // destroy surface and recreate if already possible
            else
                Swapchain.destroySurface renderQueue presentQueue swapchain device instance
                Swapchain.tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance

        // handle surface loss and attempt to recreate surface and swapchain immediately
        | SurfaceLost ->
            Swapchain.destroySurface renderQueue presentQueue swapchain device instance
            Swapchain.tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance

        // attempt to recreate surface and swapchain when app is in foreground
        | SurfaceDestroyed ->
            Swapchain.tryCreateSurfaceAndSwapchainSingleton physicalDevice renderQueue presentQueue swapchain device instance

    /// Create a Swapchain.
    static member create surfaceFormat physicalDevice window device =
        
        // swapchain index starts at zero
        let swapchainIndex = 0

        // create SwapchainSingleton array
        // NOTE: DJL: must allow for frames in flight plus 1 to prevent destroying semaphores while still in use
        // because swapchain can be refreshed at the end of one frame AND at the beginning of the next,
        // but can still only be refreshed once per frame.
        let swapchainSingletonOpts = Array.create 2 None

        // check if window is minimized at startup
        let windowMinimized = Swapchain.isWindowMinimized window

        // try create first SwapchainSingleton if window is not minimized or app paused
        if not (windowMinimized || Hl.getBackgroundingRequested ()) then
            let swapchainSingletonOpt = SwapchainSingleton.tryCreate surfaceFormat VkSwapchainKHR.Null physicalDevice window device
            swapchainSingletonOpts[swapchainIndex] <- swapchainSingletonOpt

        // make Swapchain
        let swapchain =
            { SwapchainSingletonOpts_ = swapchainSingletonOpts
              Window_ = window
              SurfaceFormat_ = surfaceFormat
              SwapchainIndex_ = swapchainIndex }

        // fin
        (swapchain, windowMinimized)
    
    /// Destroy a Swapchain.
    static member destroy swapchain device =
        Swapchain.clear swapchain device

/// Exposes the vulkan handles that must be globally accessible within the renderer.
/// TODO: P0: cease publicly exposing unnecessary fields.
/// TODO: P0: group these by role rather than arbitrary field type.
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
          PresentCommandPool_ : VkCommandPool
          TransientCommandPool_ : VkCommandPool
          TextureCommandPool_ : VkCommandPool
          RenderCommandBuffers_ : VkCommandBuffer List
          mutable RenderCommandBuffersCursor_ : int
          PresentCommandBuffer_ : VkCommandBuffer
          RenderQueue_ : ConcurrentCommandQueue
          PresentQueue_ : ConcurrentCommandQueue
          TextureQueue_ : ConcurrentCommandQueue
          ImageAvailableSemaphore_ : VkSemaphore
          RenderFence_ : VkFence
          PresentFence_ : VkFence
          TransientFence_ : VkFence
          TextureFence_ : VkFence }

    /// Whether rendering is permitted in the engine's current state.
    member this.RenderAllowed = this.RenderAllowed_

    /// The SDL window used by the swapchain.
    member this.Window = this.Swapchain_.Window_
    
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
    
    /// The current render command buffer.
    member this.RenderCommandBuffer = this.RenderCommandBuffers_[this.RenderCommandBuffersCursor_]

    /// The render command queue.
    member this.RenderQueue = this.RenderQueue_

    /// The texture command queue.
    member this.TextureQueue = this.TextureQueue_

    /// The render fence.
    member this.RenderFence = this.RenderFence_

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
        if messageType = VkDebugUtilsMessageTypeFlagsEXT.Performance then
            if messageSeverity > VkDebugUtilsMessageSeverityFlagsEXT.Warning then
                Log.custom header message
        else
            if messageSeverity > VkDebugUtilsMessageSeverityFlagsEXT.Info then
                Log.custom header message

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
        Unsafe.WriteUnaligned (NativePtr.toByRef<byte> fieldRef, callbackPointer) // TODO: report this F# compiler bug that allows direct assignment to compile without error but causes a crash at runtime
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

        // check if validation layer exists
        // TODO: DJL: try to automatically prevent validation from interfering with Nsight, starting with VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT.
        let validationLayerName = "VK_LAYER_KHRONOS_validation"
        let validationLayerExists = Array.exists (fun x -> Hl.getLayerName x = validationLayerName) layers
        if Constants.Render.Debug && not validationLayerExists then
            Log.info (validationLayerName + " is not available. Vulkan programmers must install the Vulkan SDK to enable validation.")

        // attempt to use validation layer when desired
        Hl.ValidationLayersActivated <- Constants.Render.Debug && validationLayerExists
        use layerWrap = new StringArrayWrap ([|validationLayerName|]) // must remain in scope until vkCreateInstance

        // get sdl extensions
        let mutable sdlExtensionCount = 0u
        let sdlExtensions = SDL3.SDL_Vulkan_GetInstanceExtensions &&sdlExtensionCount
        let sdlExtensionCountInt = int sdlExtensionCount
        if NativePtr.isNullPtr sdlExtensions then Log.fail (SDL3.SDL_GetError ())

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
            Array.init (sdlExtensionCountInt + if Hl.ValidationLayersActivated then 1 else 0)
                (fun i -> if i < sdlExtensionCountInt then NativePtr.get sdlExtensions i else debugUtilsWrap.Pointer)

        // check for portability enumeration extension - using MoltenVK in place of Vulkan loader won't support it (on iOS Simulator),
        // while using MoltenVK from Vulkan loader (on iOS device / macOS) requires it
        let portabilityEnumeration = NativePtr.spanToString Vulkan.VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
        use portabilityWrap = new StringWrap (portabilityEnumeration)
        let portabilityEnumerationAvailable = Array.contains portabilityEnumeration availableExtensions
        let extensions =
            if Constants.Vulkan.MoltenVk && portabilityEnumerationAvailable then
                Array.append extensions [|portabilityWrap.Pointer|]
            else extensions
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
        info.pApplicationInfo <- &&aInfo
        info.enabledExtensionCount <- uint extensions.Length
        info.ppEnabledExtensionNames <- extensionsPin.Pointer
        if Constants.Vulkan.MoltenVk && portabilityEnumerationAvailable then
            info.flags <- VkInstanceCreateFlags.EnumeratePortabilityKHR
        if Hl.ValidationLayersActivated then
            let mutable debugInfo = debugInfo
            info.pNext <- asVoidPtr &debugInfo
            info.enabledLayerCount <- 1u
            info.ppEnabledLayerNames <- layerWrap.Pointer
        let mutable instance = Unchecked.defaultof<VkInstance>
        Vulkan.vkCreateInstance (&info, nullPtr, &instance) |> Hl.check
        instance

    // TODO: DJL: try separate this from validation status, same for create instance debug.
    static member private tryCreateDebugMessenger info instance =
        if Hl.ValidationLayersActivated then
            let mutable debugMessenger = Unchecked.defaultof<VkDebugUtilsMessengerEXT>
            Vulkan.vkCreateDebugUtilsMessengerEXT (instance, &info, nullPtr, &debugMessenger) |> Hl.check
            Some debugMessenger
        else None
    
    /// Select compatible physical device if available.
    static member private trySelectPhysicalDevice window instance =

        // get available physical devices
        let mutable deviceCount = 0u
        Vulkan.vkEnumeratePhysicalDevices (instance, &&deviceCount, nullPtr) |> Hl.check
        let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
        use devicesPin = new ArrayPin<_> (devices)
        Vulkan.vkEnumeratePhysicalDevices (instance, &&deviceCount, devicesPin.Pointer) |> Hl.check

        // gather devices together with relevant data for selection
        let candidates =
            [for i in 0 .. dec devices.Length do
                match PhysicalDevice.tryCreate devices[i] window instance with
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

        // MoltenVK features
        let portabilitySubsetExtensionName = NativePtr.spanToString Vulkan.VK_KHR_PORTABILITY_SUBSET_EXTENSION_NAME
        let portabilitySubsetAvailable =
            Array.exists (fun ext -> Hl.getExtensionName ext = portabilitySubsetExtensionName) physicalDevice.Extensions
        let mutable portabilityFeatures = VkPhysicalDevicePortabilitySubsetFeaturesKHR ()
        portabilityFeatures.imageViewFormatSwizzle <- true

        // Vulkan 1.3 features
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
        info.pEnabledFeatures <- &&features
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

    /// Handle changes in window size, and check for minimization.
    static member private handleWindowSize vkc =
        
        // query minimization status
        // NOTE: DJL: this both detects the beginning of minimization and checks for the end.
        vkc.WaitingForWindowRestore_ <- Swapchain.isWindowMinimized vkc.Swapchain_.Window_

        // update the swapchain if window is not minimized, which happens a) when the window size simply changes
        // and b) when minimization ends as detected above; must also check for backgrounding in case minimization
        // occurs first so backgrounding can still be handled straight away
        if not vkc.WaitingForWindowRestore_ || Hl.getBackgroundingRequested ()
        then Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_

    /// Wait for app to return to foreground.
    static member private handleBackgrounding vkc =
        vkc.WaitingForWindowRestore_ <- Swapchain.isWindowMinimized vkc.Swapchain_.Window_
        if  not (Hl.getBackgrounded ()) &&
            not vkc.WaitingForWindowRestore_ then
            Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_

    static member private beginRenderCommandBuffer (vkc : VulkanContext) =
        if vkc.RenderCommandBuffersCursor_ >= vkc.RenderCommandBuffers_.Count then
            let buffers = Hl.allocateCommandBuffers vkc.RenderCommandBuffers_.Count VkCommandBufferLevel.Primary vkc.RenderCommandPool_ vkc.Device
            vkc.RenderCommandBuffers_.AddRange buffers
        let commandBuffer = vkc.RenderCommandBuffers_[vkc.RenderCommandBuffersCursor_]
        Vulkan.vkResetCommandBuffer (commandBuffer, VkCommandBufferResetFlags.None) |> Hl.check
        let mutable beginInfo = VkCommandBufferBeginInfo ()
        Vulkan.vkBeginCommandBuffer (commandBuffer, &&beginInfo) |> Hl.check

    static member private endRenderCommandBuffer submissionType (vkc : VulkanContext) =

        // lock to get access to vulkan queue
        ConcurrentCommandQueue.withLock vkc.RenderQueue_ (fun vkQueue ->

            // end command buffer
            let mutable commandBuffer = vkc.RenderCommandBuffers_[vkc.RenderCommandBuffersCursor_]
            Vulkan.vkEndCommandBuffer commandBuffer |> Hl.check

            // submit commands as appropriate
            let mutable submitInfo = VkSubmitInfo ()
            submitInfo.commandBufferCount <- 1u
            submitInfo.pCommandBuffers <- &&commandBuffer
            match submissionType with
            | FirstSubmission ->
                let mutable imageAvailableSemaphore = vkc.ImageAvailableSemaphore_
                let mutable stageFlag = VkPipelineStageFlags.ColorAttachmentOutput
                submitInfo.waitSemaphoreCount <- 1u
                submitInfo.pWaitSemaphores <- &&imageAvailableSemaphore
                submitInfo.pWaitDstStageMask <- &&stageFlag
                Vulkan.vkQueueSubmit (vkQueue, 1u, &&submitInfo, VkFence.Null) |> Hl.check // fine without waiting because we've already waited on the render fence
            | MiddleSubmission ->
                Vulkan.vkQueueSubmit (vkQueue, 1u, &&submitInfo, VkFence.Null) |> Hl.check
            | LastSubmission ->
                Vulkan.vkQueueSubmit (vkQueue, 1u, &&submitInfo, vkc.RenderFence_) |> Hl.check

            // advance cursor
            vkc.RenderCommandBuffersCursor_ <- inc vkc.RenderCommandBuffersCursor_)

    static member advanceRenderCommandBuffer (vkc : VulkanContext) =
        let submissionType = if vkc.RenderCommandBuffersCursor_ = 0 then FirstSubmission else MiddleSubmission
        VulkanContext.endRenderCommandBuffer submissionType vkc
        VulkanContext.beginRenderCommandBuffer vkc

    /// Begin the frame.
    static member beginFrame (windowViewport : Viewport) (vkc : VulkanContext) =

        // wait for current frame to be ready
        //let sw = System.Diagnostics.Stopwatch.StartNew ()
        Hl.awaitFence vkc.RenderFence_ vkc.Device_
        //sw.Stop ()
        //Log.info ("Hl.awaitFence: " + string sw.ElapsedTicks)

        // update render allowed flag and check if current swapchain is non-existent, typically because app is backgrounded
        vkc.RenderAllowed_ <- false
        if Option.isNone vkc.Swapchain_.SwapchainSingletonOpt then VulkanContext.handleBackgrounding vkc
        else
            // check for handling of minimized window from previous frame(s); if *still* minimized then do nothing; if restored then refresh swapchain
            if vkc.WaitingForWindowRestore_ then VulkanContext.handleWindowSize vkc
            else
                // check if app backgrounding has been triggered, if so then teardown the surface and swapchain
                if Hl.getBackgroundingRequested () then Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
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
                            if  swapchainBounds.ContainsInclusive windowViewport.Bounds = ContainmentType.Contains &&
                                windowViewport.Bounds.ContainsInclusive windowViewport.Inner = ContainmentType.Contains then
                                // try to acquire image from swapchain to draw onto
                                //let sw = System.Diagnostics.Stopwatch.StartNew ()
                                let result = Vulkan.vkAcquireNextImageKHR (vkc.Device, vkc.Swapchain_.VkSwapchain, UInt64.MaxValue, vkc.ImageAvailableSemaphore_, VkFence.Null, &Hl.ImageIndex)
                                //sw.Stop ()
                                //Log.info ("Vulkan.vkAcquireNextImageKHR: " + string sw.ElapsedTicks)
                                if result = VkResult.ErrorOutOfDateKHR then VulkanContext.handleWindowSize vkc // refresh swapchain if out of date
                                else
                                    // destroy surface if lost
                                    if result = VkResult.ErrorSurfaceLostKHR then
                                        Hl.SurfaceState <- SurfaceLost
                                        Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
                                    else
                                        Hl.check result // NOTE: DJL: this will report a suboptimal swapchain image.
                                        vkc.RenderAllowed_ <- true // permit rendering

        // reset render command buffers cursor
        vkc.RenderCommandBuffersCursor_ <- 0

        // begin render command recording
        VulkanContext.beginRenderCommandBuffer vkc

        // make swapchain image is ready to be rendered to
        Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color Undefined ColorAttachmentWrite vkc.SwapchainImage vkc.RenderCommandBuffer
        let windowResolution = windowViewport.Bounds.Size
        let renderArea = VkRect2D (0, 0, uint windowResolution.X, uint windowResolution.Y)
        let clearColor = VkClearValue (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        let mutable renderingInfo = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea (Some clearColor)
        Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, &&renderingInfo)
        Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer
        Hl.reportDrawScope ()

    /// End the frame.
    static member endFrame vkc =

        // transition swapchain image layout to presentation
        Hl.recordTransitionLayout true 1 0 1 VkImageAspectFlags.Color ColorAttachmentWrite Present vkc.Swapchain_.Image vkc.RenderCommandBuffer

        // end rendering
        VulkanContext.endRenderCommandBuffer LastSubmission vkc

        // reset draw counters
        Hl.resetDrawCounters ()

    /// Present the image back to the swapchain to appear on screen.
    static member present (vkc : VulkanContext) =

        // lock to get access to vulkan queue
        ConcurrentCommandQueue.withLock vkc.PresentQueue_ (fun vkQueue ->

            // one more check for app backgrounding before we present
            if not (Hl.getBackgroundingRequested ()) then

                // try to present image
                let mutable vkSwapchain = vkc.Swapchain_.VkSwapchain
                let mutable info = VkPresentInfoKHR ()
                info.swapchainCount <- 1u
                info.pSwapchains <- &&vkSwapchain
                info.pImageIndices <- &&Hl.ImageIndex
                //let sw = System.Diagnostics.Stopwatch.StartNew ()
                let result = Vulkan.vkQueuePresentKHR (vkQueue, &&info)
                //sw.Stop ()
                //Log.info ("Vulkan.vkQueuePresentKHR: " + string sw.ElapsedTicks)

                // refresh swapchain if framebuffer out of date or suboptimal
                if result = VkResult.ErrorOutOfDateKHR || result = VkResult.SuboptimalKHR then
                    VulkanContext.handleWindowSize vkc
                
                // destroy surface if lost
                elif result = VkResult.ErrorSurfaceLostKHR then
                    Hl.SurfaceState <- SurfaceLost
                    Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_
                
                // normal check
                else Hl.check result

            // still need to update the swapchain even if we haven't rendered
            else Swapchain.update vkc.PhysicalDevice_ vkc.RenderQueue_ vkc.PresentQueue_ vkc.Swapchain_ vkc.Device vkc.Instance_)

    /// Wait for all device operations to complete before cleaning up resources.
    static member waitIdle (vkc : VulkanContext) =

        // NOTE: we never call vkDeviceWaitIdle as its implementation compromises queue thread safety.
        ConcurrentCommandQueue.waitIdle vkc.RenderQueue_
        ConcurrentCommandQueue.waitIdle vkc.PresentQueue_
        ConcurrentCommandQueue.waitIdle vkc.TextureQueue_

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
            let renderQueue = ConcurrentCommandQueue.create physicalDevice.GraphicsQueueFamily 0u device

            // create seperate present queue if graphics queue family does not support presentation
            let presentQueue =
                if physicalDevice.GraphicsQueueFamily <> physicalDevice.PresentQueueFamily
                then ConcurrentCommandQueue.create physicalDevice.PresentQueueFamily 0u device
                else renderQueue

            // create seperate queue for texture server thread if available
            let textureQueue =
                if physicalDevice.GraphicsQueueCount > 1u
                then ConcurrentCommandQueue.create physicalDevice.GraphicsQueueFamily 1u device
                else renderQueue

            // setup execution for rendering on render thread
            let renderFence = Hl.createFence true device
            let renderCommandPool = VulkanContext.createCommandPool false physicalDevice.GraphicsQueueFamily device
            let renderCommandBuffers = Hl.allocateCommandBuffers Constants.Vulkan.RenderCommandBufferCountDefault VkCommandBufferLevel.Primary renderCommandPool device

            // setup execution for presentation on render thread
            let presentCommandPool = VulkanContext.createCommandPool false physicalDevice.PresentQueueFamily device
            let presentCommandBuffer = (Hl.allocateCommandBuffers 1 VkCommandBufferLevel.Primary renderCommandPool device)[0]
            let presentFence = Hl.createFence true device
            let imageAvailableSemaphore = Hl.createSemaphore device

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
                  PresentCommandPool_ = presentCommandPool
                  TransientCommandPool_ = transientCommandPool
                  TextureCommandPool_ = textureCommandPool
                  RenderCommandBuffers_ = List renderCommandBuffers
                  RenderCommandBuffersCursor_ = 0
                  PresentCommandBuffer_ = presentCommandBuffer
                  RenderQueue_ = renderQueue
                  PresentQueue_ = presentQueue
                  TextureQueue_ = textureQueue
                  ImageAvailableSemaphore_ = imageAvailableSemaphore
                  RenderFence_ = renderFence
                  PresentFence_ = presentFence
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
        Vulkan.vkDestroySemaphore (vkc.Device, vkc.ImageAvailableSemaphore_, nullPtr)
        Vulkan.vkDestroyFence (vkc.Device, vkc.RenderFence_, nullPtr)
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