// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

// TODO: confirm this module/namespace arrangement is correct.

/// Force qualification of Vulkan namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module Vulkan = let _ = ()

namespace Vulkan
open System
open System.Runtime.CompilerServices
open System.Collections.Generic
open System.Text
open FSharp.NativeInterop
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
    
    /// Convert VkLayerProperties.layerName to a string.
    let getLayerName (layerProps : VkLayerProperties) =
        let mutable layerName = layerProps.layerName
        let ptr = asBytePointer &layerName
        let vkUtf8Str = new VkUtf8String (ptr)
        vkUtf8Str.ToString ()
    
    // TODO: implement VkStringInterop helpers to remove Vortice.Vulkan dependence and move gems like this into Native.fs.

    /// A container for a pinned array of unmanaged strings.
    type StringArrayWrap private (array : nativeptr<byte> array) =
    
        let array = array
        let pin = ArrayPin array
    
        new (strs : string array) =
            let ptrs = Array.zeroCreate<nativeptr<byte>> strs.Length
            for i in [0 .. dec strs.Length] do ptrs[i] <- VkStringInterop.ConvertToUnmanaged strs[i]
            new StringArrayWrap (ptrs)

        // TODO: see if implicit conversion can be used to remove the need to call this member directly.
        member this.Pointer = pin.Pointer

        // make disposal publicly available without casting
        member this.Dispose () =
            for i in [0 .. dec array.Length] do VkStringInterop.Free array[i]
            pin.Dispose ()
    
        interface IDisposable with
            member this.Dispose () =
                this.Dispose ()
    
    /// A physical device and associated data.
    type PhysicalDeviceData =
        { PhysicalDevice : VkPhysicalDevice
          Properties : VkPhysicalDeviceProperties
          Extensions : VkExtensionProperties array
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
            let (graphicsQueueFamilyOpt, presentQueueFamilyOpt) = PhysicalDeviceData.getQueueFamilyOpts physicalDevice surface

            // make physicalDeviceData
            let physicalDeviceData =
                { PhysicalDevice = physicalDevice
                  Properties = properties
                  Extensions = extensions
                  GraphicsQueueFamilyOpt = graphicsQueueFamilyOpt
                  PresentQueueFamilyOpt = presentQueueFamilyOpt }

            // fin
            physicalDeviceData
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance
              Surface : VkSurfaceKHR
              Device : VkDevice }

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
            for i in [0 .. dec (int sdlExtensionCount)] do sdlExtensions[i] <- NativePtr.ofNativeInt<byte> sdlExtensionsOut[i]
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
            let result = SDL.SDL_Vulkan_CreateSurface (window, instance, &(Unsafe.As<VkSurfaceKHR, uint64> &surface))
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

            // compatibility criteria: device must support the essential queue operations and Vulkan 1.3
            let isCompatible physicalDeviceData =
                Option.isSome physicalDeviceData.GraphicsQueueFamilyOpt &&
                Option.isSome physicalDeviceData.PresentQueueFamilyOpt &&
                physicalDeviceData.Properties.apiVersion.Minor >= 3u

            // preferability criteria: device ought to be discrete
            let isPreferable physicalDeviceData = physicalDeviceData.Properties.deviceType = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
            
            // filter and order candidates according to criteria
            let candidatesFiltered = List.filter isCompatible candidates
            let (fstChoice, sndChoice) = List.partition isPreferable candidatesFiltered
            let candidatesFilteredAndOrdered = List.append fstChoice sndChoice
                
            // if compatible devices exist then return the first along with its queue families
            let physicalDeviceOpt =
                if candidatesFilteredAndOrdered.Length > 0 then
                    let physicalDeviceData = List.head candidatesFilteredAndOrdered
                    Some (physicalDeviceData.PhysicalDevice, physicalDeviceData.GraphicsQueueFamily, physicalDeviceData.PresentQueueFamily)
                else
                    Log.info "Could not find a suitable graphics device for Vulkan."
                    None

            // fin
            physicalDeviceOpt
        
        /// Create the logical device.
        static member createDevice graphicsQueueFamily presentQueueFamily physicalDevice =

            // device handle
            let mutable device = Unchecked.defaultof<VkDevice>

            // get unique queue family array
            let uniqueQueueFamiliesSet = new HashSet<uint> ()
            uniqueQueueFamiliesSet.Add graphicsQueueFamily |> ignore
            uniqueQueueFamiliesSet.Add presentQueueFamily |> ignore
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
            let swapchainExtensionName = Encoding.UTF8.GetString VK_KHR_SWAPCHAIN_EXTENSION_NAME
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
            vkCreateDevice (physicalDevice, asPointer &createInfo, nullPtr, &device) |> check

            // fin
            device

        /// Get command queues.
        static member getQueues graphicsQueueFamily presentQueueFamily device =

            // queue handles
            let mutable graphicsQueue = Unchecked.defaultof<VkQueue>
            let mutable presentQueue = Unchecked.defaultof<VkQueue>

            // get queues
            vkGetDeviceQueue (device, graphicsQueueFamily, 0u, &graphicsQueue)
            vkGetDeviceQueue (device, presentQueueFamily, 0u, &presentQueue)

            // fin
            (graphicsQueue, presentQueue)
        
        /// Destroy Vulkan handles.
        static member cleanup vulkanGlobal =
            vkDestroyDevice (vulkanGlobal.Device, nullPtr)
            vkDestroySurfaceKHR (vulkanGlobal.Instance, vulkanGlobal.Surface, nullPtr)
            vkDestroyInstance (vulkanGlobal.Instance, nullPtr)
        
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
            | Some (physicalDevice, graphicsQueueFamily, presentQueueFamily) ->

                // create device
                let device = VulkanGlobal.createDevice graphicsQueueFamily presentQueueFamily physicalDevice

                // load device commands; not vulkan function
                vkLoadDevice device

                // get queues
                let (graphicsQueue, presentQueue) = VulkanGlobal.getQueues graphicsQueueFamily presentQueueFamily device
                
                // make vulkanGlobal
                let vulkanGlobal =
                    { Instance = instance
                      Surface = surface
                      Device = device }

                // fin
                Some vulkanGlobal

            // abort vulkan
            | None -> None