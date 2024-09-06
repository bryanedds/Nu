// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu

// TODO: confirm this module/namespace arrangement is correct.

/// Force qualification of Vulkan namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module Vulkan = let _ = ()

namespace Vulkan
open System
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
        member this.Dispose () = pin.Dispose ()
    
        interface IDisposable with
            member this.Dispose () =
                this.Dispose ()
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance
              PhysicalDevice : VkPhysicalDevice }

        /// Create the Vulkan instance.
        static member createInstance window =

            // instance handle
            let mutable instance = Unchecked.defaultof<VkInstance>

            // get sdl extensions
            let mutable sdlExtensionCount = 0u
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, null)
            if int result <> 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensionsOut = Array.zeroCreate<nativeint> (int sdlExtensionCount)
            let result = SDL.SDL_Vulkan_GetInstanceExtensions (window, &sdlExtensionCount, sdlExtensionsOut)
            if int result <> 0 then Log.error "SDL error, SDL_Vulkan_GetInstanceExtensions failed."
            let sdlExtensions = Array.zeroCreate<nativeptr<byte>> (int sdlExtensionCount)
            for i in [0 .. dec (int sdlExtensionCount)] do sdlExtensions[i] <- NativePtr.ofNativeInt<byte> sdlExtensionsOut[i]
            use sdlExtensionsWrap = ArrayPin sdlExtensions
            
            // TODO: setup message callback with debug utils *if* motivation arises.
            
            // get available instance layers
            let mutable layerCount = 0u
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, NativePtr.nullPtr) |> check
            let layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
            use layersWrap = ArrayPin layers
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, layersWrap.Pointer) |> check

            // check if validation layer exists
            let validationLayer = "VK_LAYER_KHRONOS_validation"
            let validationLayerExists = Array.exists (fun x -> getLayerName x = validationLayer) layers
            if validationLayersEnabled && not validationLayerExists then Log.info (validationLayer + " is not available. Vulkan programmers must install the Vulkan SDK to enable validation.")
            
            // TODO: apply VkApplicationInfo once all compulsory fields have been decided (e.g. engineVersion)
            // and check for available vulkan version as described in 
            // https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap4.html#VkApplicationInfo

            // populate createinstance info
            let mutable createInfo = VkInstanceCreateInfo ()
            createInfo.enabledExtensionCount <- sdlExtensionCount
            createInfo.ppEnabledExtensionNames <- sdlExtensionsWrap.Pointer

            // load validation layer if enabled and available
            if validationLayersEnabled && validationLayerExists then
                use layerWrap = StringArrayWrap [|validationLayer|]
                createInfo.enabledLayerCount <- 1u
                createInfo.ppEnabledLayerNames <- layerWrap.Pointer
            else
                createInfo.enabledLayerCount <- 0u

            // create instance
            vkCreateInstance (&createInfo, NativePtr.nullPtr, &instance) |> check

            // fin
            instance
        
        /// Select physical device.
        static member selectPhysicalDevice instance =
            
            // physical device handle
            let mutable physicalDevice = Unchecked.defaultof<VkPhysicalDevice>

            // get available physical devices
            let mutable deviceCount = 0u
            vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, NativePtr.nullPtr) |> check
            let devices = Array.zeroCreate<VkPhysicalDevice> (int deviceCount)
            use devicesWrap = ArrayPin devices
            vkEnumeratePhysicalDevices (instance, asPointer &deviceCount, devicesWrap.Pointer) |> check

            // get the devices' props
            let devicesProps = Array.zeroCreate<VkPhysicalDeviceProperties> devices.Length
            for i in [0 .. dec devices.Length] do
                let mutable props = Unchecked.defaultof<VkPhysicalDeviceProperties>
                vkGetPhysicalDeviceProperties (devices[i], &props)
                devicesProps[i] <- props
                

            // fin
            physicalDevice
        
        /// Destroy Vulkan handles.
        static member cleanup vulkanGlobal =
            vkDestroyInstance (vulkanGlobal.Instance, NativePtr.nullPtr)
        
        /// Make a VulkanGlobal.
        static member make window =

            // loads vulkan; not vulkan function
            vkInitialize () |> check

            // create instance
            let instance = VulkanGlobal.createInstance window

            // loads instance commands; not vulkan function
            vkLoadInstanceOnly instance

            // select physical device
            let physicalDevice = VulkanGlobal.selectPhysicalDevice instance

            // make vulkanGlobal
            let vulkanGlobal =
                { Instance = instance
                  PhysicalDevice = physicalDevice }

            // fin
            vulkanGlobal