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
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance }

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
            
            // get available instance layers
            let mutable layerCount = 0u
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, NativePtr.nullPtr) |> check
            let mutable layers = Array.zeroCreate<VkLayerProperties> (int layerCount)
            use layersWrap = ArrayPin layers
            vkEnumerateInstanceLayerProperties (asPointer &layerCount, layersWrap.Pointer) |> check

            // TODO: setup more advanced debug functionality (debug utils etc.) as motivation arises.
            
            (*
            TODO: apply VkApplicationInfo once all compulsory fields have been decided (e.g. engineVersion)
            and check for available vulkan version as described in 
            https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap4.html#VkApplicationInfo
            *)

            // populate createinstance info
            let mutable createInfo = VkInstanceCreateInfo ()
            createInfo.enabledExtensionCount <- sdlExtensionCount
            createInfo.ppEnabledExtensionNames <- sdlExtensionsWrap.Pointer

            // load validation layer if available
            let validationLayer = "VK_LAYER_KHRONOS_validation"


            // fin
            instance
        
        /// Make a VulkanGlobal.
        static member make window =

            // loads vulkan; not vulkan function
            vkInitialize () |> check

            // create instance
            let instance = VulkanGlobal.createInstance window

            // make vulkanGlobal
            let vulkanGlobal =
                { Instance = instance }

            // fin
            vulkanGlobal