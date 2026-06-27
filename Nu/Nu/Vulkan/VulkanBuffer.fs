// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.Numerics
open FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

// TODO: DJL: doc comments!

type Allocation =
    | Vma of VmaAllocation
    | Manual of VkDeviceMemory

// TODO: P0: rename this to BufferStreamType or BufferMultiType?
type BufferType =
    | Staging
    | Vertex of UploadEnabled : bool
    | Index of UploadEnabled : bool
    | Uniform
    | Storage

    static member private makeInfoInternal size usage =
        let mutable info = VkBufferCreateInfo ()
        info.size <- uint64 size
        info.usage <- usage
        info.sharingMode <- VkSharingMode.Exclusive
        info

    static member makeInfo size bufferType =
        match bufferType with
        | Staging -> BufferType.makeInfoInternal size VkBufferUsageFlags.TransferSrc
        | Vertex uploadEnabled ->
            let usage =
                if uploadEnabled
                then VkBufferUsageFlags.VertexBuffer
                else VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferDst
            BufferType.makeInfoInternal size usage
        | Index uploadEnabled ->
            let usage =
                if uploadEnabled
                then VkBufferUsageFlags.IndexBuffer
                else VkBufferUsageFlags.IndexBuffer ||| VkBufferUsageFlags.TransferDst
            BufferType.makeInfoInternal size usage
        | Uniform -> BufferType.makeInfoInternal size VkBufferUsageFlags.UniformBuffer
        | Storage -> BufferType.makeInfoInternal size VkBufferUsageFlags.StorageBuffer

/// Internal representation of an allocated buffer.
type BufferInternal =
    private
        { mutable VkBuffer_ : VkBuffer // set to VkBuffer.Null when buffer destroyed
          Allocation_ : Allocation
          Mapping_ : voidptr
          Size_ : int
          UploadEnabled_ : bool }

    /// The underlying VkBuffer.
    member this.VkBuffer = this.VkBuffer_

    /// The size of the buffer.
    member this.Size = this.Size_
    
    static member private createManual uploadEnabled bufferInfo (vkc : VulkanContext) =

        // create vkBuffer
        let mutable vkBuffer = Unchecked.defaultof<VkBuffer>
        Vulkan.vkCreateBuffer (vkc.Device, &bufferInfo, nullPtr, asPointer &vkBuffer) |> Hl.check

        // get vkBuffer memory requirements
        let mutable memRequirements = Unchecked.defaultof<VkMemoryRequirements>
        Vulkan.vkGetBufferMemoryRequirements (vkc.Device, vkBuffer, &memRequirements)

        // choose appropriate memory properties
        let properties =
            if uploadEnabled then VkMemoryPropertyFlags.HostVisible ||| VkMemoryPropertyFlags.HostCoherent
            else VkMemoryPropertyFlags.DeviceLocal

        // allocate memory
        let mutable info = VkMemoryAllocateInfo ()
        info.allocationSize <- memRequirements.size
        info.memoryTypeIndex <- Hl.findMemoryType memRequirements.memoryTypeBits properties vkc.VkPhysicalDevice
        let mutable memory = Unchecked.defaultof<VkDeviceMemory>
        Vulkan.vkAllocateMemory (vkc.Device, asPointer &info, nullPtr, &memory) |> Hl.check

        // bind vkBuffer to memory
        Vulkan.vkBindBufferMemory (vkc.Device, vkBuffer, memory, 0UL) |> Hl.check

        // map memory if upload enabled
        let mappingPtr = NativePtr.stackalloc<voidptr> 1 // must be allocated manually because managed allocation doesn't work
        if uploadEnabled then Vulkan.vkMapMemory (vkc.Device, memory, 0UL, Vulkan.VK_WHOLE_SIZE, VkMemoryMapFlags.None, mappingPtr) |> Hl.check
        let mapping = NativePtr.read mappingPtr

        // make BufferInternal
        let bufferInternal = 
            { VkBuffer_ = vkBuffer
              Allocation_ = Manual memory
              Mapping_ = mapping
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferInternal

    static member private createVma uploadEnabled bufferInfo (vkc : VulkanContext) =

        // allocation create info
        let mutable info = VmaAllocationCreateInfo ()
        info.usage <- VmaMemoryUsage.Auto
        if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

        // create vma buffer
        let mutable vkBuffer = Unchecked.defaultof<VkBuffer>
        let mutable allocation = Unchecked.defaultof<VmaAllocation>
        let mutable allocationInfo = Unchecked.defaultof<VmaAllocationInfo>
        Vma.vmaCreateBuffer (vkc.VmaAllocator, &bufferInfo, &info, &vkBuffer, &allocation, asPointer &allocationInfo) |> Hl.check

        // make BufferInternal
        let bufferInternal =
            { VkBuffer_ = vkBuffer
              Allocation_ = Vma allocation
              Mapping_ = allocationInfo.pMappedData
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferInternal

    /// Create BufferInternal.
    static member createPlus uploadEnabled bufferInfo (vkc : VulkanContext) =

        // NOTE: DJL: change this to Manual to bypass VMA and test buffers with manually allocated memory.
        let allocType = Vma Unchecked.defaultof<_>

        // create with selected allocation type
        match allocType with
        | Vma _ -> BufferInternal.createVma uploadEnabled bufferInfo vkc
        | Manual _ -> BufferInternal.createManual uploadEnabled bufferInfo vkc

    /// Create BufferInternal.
    static member create bufferSize bufferType vkc =

        // compute uploadability
        let uploadEnabled =
            match bufferType with
            | Staging -> true
            | Vertex uploadEnabled -> uploadEnabled
            | Index uploadEnabled -> uploadEnabled
            | Uniform -> true
            | Storage -> true

        // make info
        let info = BufferType.makeInfo bufferSize bufferType

        // make buffer
        BufferInternal.createPlus uploadEnabled info vkc

    /// Write data to buffer if upload is enabled.
    static member write offset alignment size count data bufferInternal (_ : VulkanContext) =
        if bufferInternal.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferInternal.Size_ then
                    
                    // upload as single blob if possible, otherwise upload one value at a time to create padding
                    if size = stride then
                        NativePtr.memCopy offset (size * count) (NativePtr.nativeintToVoidPtr data) bufferInternal.Mapping_
                    else
                        for i in 0 .. dec count do
                            let ptr = NativePtr.add (NativePtr.nativeintToBytePtr data) (i * size)
                            NativePtr.memCopy (offset + i * stride) size (NativePtr.toVoidPtr ptr) bufferInternal.Mapping_

                else Log.warn "Write to Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Write to Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Write to Vulkan buffer failed because upload was not enabled for that buffer."

    /// Flush data to buffer if upload is enabled.
    static member flush offset alignment size count bufferInternal (vkc : VulkanContext) =
        if bufferInternal.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferInternal.Size_ then

                    // manually flush as memory may not be host-coherent on non-windows platforms, see
                    // https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/memory_mapping.html#memory_mapping_cache_control
                    match bufferInternal.Allocation_ with
                    | Vma vmaAllocation -> Vma.vmaFlushAllocation (vkc.VmaAllocator, vmaAllocation, uint64 offset, uint64 (stride * count)) |> Hl.check
                    | Manual _ -> () // currently no point bothering

                else Log.warn "Flush of Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Flush of Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Flush of Vulkan buffer failed because upload was not enabled for that buffer."

    /// Destroy buffer and allocation.
    static member destroy (bufferInternal : BufferInternal) (vkc : VulkanContext) =
        if bufferInternal.VkBuffer_.IsNotNull then
            match bufferInternal.Allocation_ with
            | Vma vmaAllocation ->
                Vma.vmaDestroyBuffer (vkc.VmaAllocator, bufferInternal.VkBuffer, vmaAllocation)
            | Manual manualAllocation ->
                if bufferInternal.Mapping_ <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkc.Device, manualAllocation)
                Vulkan.vkDestroyBuffer (vkc.Device, bufferInternal.VkBuffer, nullPtr)
                Vulkan.vkFreeMemory (vkc.Device, manualAllocation, nullPtr)
            bufferInternal.VkBuffer_ <- VkBuffer.Null

/// Represents a dynamically growing multibuffer with parallel underlying vulkan buffers. Maintains an internal
/// cursor that selects the currently active buffer, which is reset via beginFrame and advanced to the next vulkan
/// buffer with advance. Automatically resizes when usage exceeds its capacity and creates additional buffers when
/// the cursor moves beyond current capacity. This type is intended for transient or frequently updated GPU data
/// such as storage data, uniform data, and streaming data.
/// TODO: P0: rename this to BufferStream or BufferMulti since we otherwise have to qualify it to disambiguate from
/// System.Buffer?
type Buffer =
    private
        { mutable BufferCursor_ : int
          BufferInternals_ : BufferInternal List
          BufferType_ : BufferType }

    member private this.BufferInternal =
        this.BufferInternals_[this.BufferCursor_]

    /// Get the vulkan buffer currently at the cursor.
    member this.VkBuffer =
        this.BufferInternal.VkBuffer

    static member private ensureHeight (buffer : Buffer) vkc =
        while buffer.BufferCursor_ >= buffer.BufferInternals_.Count do
            let bufferInternals = Array.init buffer.BufferInternals_.Count (fun _ -> BufferInternal.create buffer.BufferInternals_[0].Size buffer.BufferType_ vkc)
            buffer.BufferInternals_.AddRange bufferInternals

    /// Expand buffer width as necessary, disregarding all existing content.
    static member ensureWidth size (buffer : Buffer) vkc =
        Buffer.ensureHeight buffer vkc
        if buffer.BufferInternals_[buffer.BufferCursor_].Size < size then
            BufferInternal.destroy buffer.BufferInternals_[buffer.BufferCursor_] vkc
            buffer.BufferInternals_[buffer.BufferCursor_] <- BufferInternal.create size buffer.BufferType_ vkc

    /// Copy data from the source buffer to the destination buffer.
    static member private copyData size source destination (vkc : VulkanContext) =
        let commandBuffer = Hl.createTransientCommandBuffer vkc.TransientCommandPool vkc.Device
        let mutable region = VkBufferCopy (size = uint64 size)
        Vulkan.vkCmdCopyBuffer (commandBuffer, source, destination, 1u, asPointer &region)
        CommandQueue.executeTransient commandBuffer vkc.TransientCommandPool vkc.TransientFence vkc.RenderQueue vkc.Device

    /// Begin use of this buffer for the current frame.
    static member beginFrame buffer =
        buffer.BufferCursor_ <- 0

    /// Advance the cursor.
    static member advance buffer =
        buffer.BufferCursor_ <- inc buffer.BufferCursor_

    /// Create a new Buffer.
    static member create bufferSize (bufferType : BufferType) vkc =
        { BufferCursor_ = 0
          BufferInternals_ = List [BufferInternal.create bufferSize bufferType vkc]
          BufferType_ = bufferType }

    /// Write subdata to Buffer. Caller is reponsible for ensuring buffer width and height.
    static member writeSubdata offset alignment size count data (buffer : Buffer) vkc =
        Buffer.ensureHeight buffer vkc
        BufferInternal.write offset alignment size count data buffer.BufferInternal vkc

    /// Flush subdata from Buffer. Caller is reponsible for ensuring buffer width and height.
    static member flushSubdata offset alignment size count (buffer : Buffer) vkc =
        Buffer.ensureHeight buffer vkc
        BufferInternal.flush offset alignment size count buffer.BufferInternal vkc

    /// Upload data to Buffer.
    static member uploadData size count data (buffer : Buffer) vkc =
        let bufferSize = size * count
        Buffer.ensureHeight buffer vkc
        Buffer.ensureWidth bufferSize buffer vkc
        BufferInternal.write 0 0 size count data buffer.BufferInternal vkc
        BufferInternal.flush 0 0 size count buffer.BufferInternal vkc

    /// Upload a value to Buffer.
    static member uploadValue (value : 'a) buffer vkc =
        let mutable value = value
        Buffer.uploadData sizeof<'a> 1 (asNativeInt &value) buffer vkc

    /// Upload an array to Buffer.
    static member uploadArray (array : 'a array) buffer vkc =
        use arrayPin = new ArrayPin<_> (array)
        Buffer.uploadData sizeof<'a> array.Length arrayPin.NativeInt buffer vkc

    /// Create a staging buffer and stage the data.
    static member stageData size data vkc =
        let buffer = Buffer.create size Staging vkc
        Buffer.uploadData size 1 data buffer vkc
        buffer

    /// Create a vertex buffer with data uploaded via staging buffer.
    static member createVertexStaged size data vkc =
        let stagingBuffer = Buffer.stageData size data vkc
        let vertexBuffer = Buffer.create size (Vertex false) vkc
        Buffer.copyData size stagingBuffer.BufferInternal.VkBuffer vertexBuffer.BufferInternal.VkBuffer vkc
        Buffer.destroy stagingBuffer vkc
        vertexBuffer

    /// Create an index buffer with data uploaded via staging buffer.
    static member createIndexStaged size data vkc =
        let stagingBuffer = Buffer.stageData size data vkc
        let indexBuffer = Buffer.create size (Index false) vkc
        Buffer.copyData size stagingBuffer.BufferInternal.VkBuffer indexBuffer.BufferInternal.VkBuffer vkc
        Buffer.destroy stagingBuffer vkc
        indexBuffer

    /// Create a vertex buffer with data uploaded via staging buffer from an array.
    static member createVertexStagedFromArray (array : 'a array) vkc =
        let size = array.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (array)
        Buffer.createVertexStaged size arrayPin.NativeInt vkc

    /// Create an index buffer with data uploaded via staging buffer from an array.
    static member createIndexStagedFromArray (array : 'a array) vkc =
        let size = array.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (array)
        Buffer.createIndexStaged size arrayPin.NativeInt vkc

    /// Create a vertex buffer with data uploaded via staging buffer from memory.
    static member createVertexStagedFromMemory (memory : 'a Memory) vkc =
        let size = memory.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (memory)
        Buffer.createVertexStaged size arrayPin.NativeInt vkc

    /// Create an index buffer with data uploaded via staging buffer from memory.
    static member createIndexStagedFromMemory (memory : 'a Memory) vkc =
        let size = memory.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (memory)
        Buffer.createIndexStaged size arrayPin.NativeInt vkc
    
    /// Destroy Buffer.
    static member destroy (buffer : Buffer) vkc =
        for i in 0 .. dec buffer.BufferInternals_.Count do
            BufferInternal.destroy buffer.BufferInternals_[i] vkc