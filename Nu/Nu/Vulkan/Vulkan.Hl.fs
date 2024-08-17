namespace Vortice.Vulkan
open System
open System.Runtime.InteropServices
open FSharp.NativeInterop
open type Vma
open Prime
open Nu

[<AutoOpen>]
module Vulkan =

    /// Experimental abstraction of memory pinning and native pointer.
    /// TODO: document this as no longer experimental?
    type ArrayPin<'a when 'a : unmanaged> private (handle : Buffers.MemoryHandle, pointer : nativeptr<'a>) =

        let handle = handle
        let pointer = pointer

        new (array : 'a array) =
            let handle = array.AsMemory().Pin()
            let pointer = NativePtr.ofVoidPtr<'a> handle.Pointer
            new ArrayPin<'a> (handle, pointer)

        member this.Pointer = pointer

        interface IDisposable with
            member this.Dispose () =
                handle.Dispose ()

    let hlCreateBuffer<'ub when 'ub : unmanaged> (allocator : VmaAllocator inref, usage : VkBufferUsageFlags, buffer : VkBuffer byref, allocation : VmaAllocation byref) =
        let size = sizeof<'ub>
        let mutable bufferCreateInfo = VkBufferCreateInfo ()
        bufferCreateInfo.size <- uint64 size
        bufferCreateInfo.usage <- usage
        bufferCreateInfo.sharingMode <- VkSharingMode.Exclusive
        let mutable allocationCreateInfo = VmaAllocationCreateInfo ()
        allocationCreateInfo.usage <- VmaMemoryUsage.CpuToGpu
        let mutable allocationInfo = VmaAllocationInfo ()
        vmaCreateBuffer (allocator, Interop.AsPointer &bufferCreateInfo, Interop.AsPointer &allocationCreateInfo, Interop.AsPointer &buffer, Interop.AsPointer &allocation, Interop.AsPointer &allocationInfo)

    let hlWriteBuffer<'ub when 'ub : unmanaged> (allocator : VmaAllocator inref, allocation : VmaAllocation inref, values : 'ub inref) =
        let memoryPtrPtr = Unchecked.defaultof<nativeptr<voidptr>>
        let result = vmaMapMemory (allocator, allocation, memoryPtrPtr)
        Marshal.StructureToPtr<'ub> (values, NativePtr.toNativeInt memoryPtrPtr, false)
        vmaUnmapMemory (allocator, allocation)
        result