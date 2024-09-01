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
open Vortice.Vulkan
open type Vulkan
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan error: " + string result)
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance }

        /// Create the Vulkan instance.
        static member createInstance () =

            // instance handle
            let mutable instance = Unchecked.defaultof<VkInstance>

            // get available instance layers
            let mutable layerCount = 0u

            // fin
            instance
        
        /// Make a VulkanGlobal.
        static member make window =

            // loads vulkan; not vulkan function
            vkInitialize () |> check

            // create instance
            let instance = VulkanGlobal.createInstance ()

            // make vulkanGlobal
            let vulkanGlobal =
                { Instance = instance }

            // fin
            vulkanGlobal