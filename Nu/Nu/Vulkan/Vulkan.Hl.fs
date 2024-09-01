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
open type Vortice.Vulkan.Vulkan
open Nu

[<RequireQualifiedAccess>]
module Hl =

    // TODO: have Bryan verify safety, try to apply implicit conversion and move to appropriate location.

    /// An experimental container for a pinned array exposing its pointer.
    type ArrayPin<'a when 'a: unmanaged> private (handle : Buffers.MemoryHandle, pointer : nativeptr<'a>) =

        let handle = handle
        let pointer = pointer

        member this.Pointer = pointer

        new (array : 'a array) =
            let handle = array.AsMemory().Pin()
            let pointer = NativePtr.ofVoidPtr<'a> handle.Pointer
            new ArrayPin<'a> (handle, pointer)

        interface IDisposable with
            member this.Dispose() = handle.Dispose()

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan error: " + string result)
    
    /// The Vulkan handles that must be globally accessible within the renderer.
    type [<ReferenceEquality>] VulkanGlobal =
        private
            { Instance : VkInstance }

        /// Make a VulkanGlobal.
        static member make window =

            // create handle variables
            let mutable instance = Unchecked.defaultof<VkInstance>

            // loads vulkan; not vulkan function
            vkInitialize () |> check

            // make vulkanGlobal
            let vulkanGlobal =
                { Instance = instance }

            // fin
            vulkanGlobal