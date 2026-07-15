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

// The type of vulkan buffer being utilized.
type VulkanBufferType =
    | Staging
    | Vertex of UploadEnabled : bool
    | Index of UploadEnabled : bool
    | Instance
    | Uniform

/// Internal representation of a vulkan buffer.
type VulkanBufferInternal =
    private
        { mutable VkBuffer_ : VkBuffer // set to VkBuffer.Null when buffer destroyed
          VmaAllocation_ : VmaAllocation
          Mapping_ : voidptr
          Size_ : int
          UploadEnabled_ : bool }

    /// The underlying VkBuffer.
    member this.VkBuffer = this.VkBuffer_

    /// The size of the buffer.
    member this.Size = this.Size_

    static member private makeBufferCreateInfo usage (size : int) =
        let mutable info = VkBufferCreateInfo ()
        info.usage <- usage
        info.size <- uint64 size
        info.sharingMode <- VkSharingMode.Exclusive
        info

    /// Create BufferInternal.
    static member createPlus (uploadEnabled, bufferUsage, bufferInfo : VkBufferCreateInfo byref, context : VulkanContext) =

        // allocation create info
        let mutable info = VmaAllocationCreateInfo ()
        info.usage <- bufferUsage
        if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

        // create vma buffer
        let mutable vkBuffer = Unchecked.defaultof<VkBuffer>
        let mutable vmaAllocation = Unchecked.defaultof<VmaAllocation>
        let mutable vmaAllocationInfo = Unchecked.defaultof<VmaAllocationInfo>
        Vma.vmaCreateBuffer (context.VmaAllocator, &&bufferInfo, &&info, &vkBuffer, &vmaAllocation, &vmaAllocationInfo) |> VulkanHl.check

        // make BufferInternal
        let bufferInternal =
            { VkBuffer_ = vkBuffer
              VmaAllocation_ = vmaAllocation
              Mapping_ = vmaAllocationInfo.pMappedData
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferInternal

    /// Create BufferInternal.
    static member create bufferType bufferSize context =

        // compute uploadability
        let struct (uploadEnabled, bufferUsage) =
            match bufferType with
            | Staging -> struct (true, VmaMemoryUsage.AutoPreferDevice)
            | Vertex true -> struct (true, VmaMemoryUsage.AutoPreferDevice)
            | Vertex false -> struct (false, VmaMemoryUsage.AutoPreferDevice)
            | Index true -> struct (true, VmaMemoryUsage.AutoPreferDevice)
            | Index false -> struct (false, VmaMemoryUsage.AutoPreferDevice)
            | Instance -> struct (true, VmaMemoryUsage.AutoPreferDevice)
            | Uniform -> struct (true, VmaMemoryUsage.AutoPreferDevice)

        // make create info
        let mutable createInfo =
            match bufferType with
            | Staging ->
                let usage = VkBufferUsageFlags.TransferSrc
                VulkanBufferInternal.makeBufferCreateInfo usage bufferSize
            | Vertex uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                    else VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferDst
                VulkanBufferInternal.makeBufferCreateInfo usage bufferSize
            | Index uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then VkBufferUsageFlags.IndexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                    else VkBufferUsageFlags.IndexBuffer ||| VkBufferUsageFlags.TransferDst
                VulkanBufferInternal.makeBufferCreateInfo usage bufferSize
            | Instance ->
                let usage = VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                VulkanBufferInternal.makeBufferCreateInfo usage bufferSize
            | Uniform ->
                let usage = VkBufferUsageFlags.UniformBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                VulkanBufferInternal.makeBufferCreateInfo usage bufferSize

        // make buffer
        VulkanBufferInternal.createPlus (uploadEnabled, bufferUsage, &createInfo, context)

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
    static member flush offset alignment size count bufferInternal (context : VulkanContext) =
        if bufferInternal.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferInternal.Size_ then

                    // manually flush as memory may not be host-coherent on non-windows platforms, see
                    // https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/memory_mapping.html#memory_mapping_cache_control
                    Vma.vmaFlushAllocation (context.VmaAllocator, bufferInternal.VmaAllocation_, uint64 offset, uint64 (stride * count)) |> VulkanHl.check

                else Log.warn "Flush of Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Flush of Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Flush of Vulkan buffer failed because upload was not enabled for that buffer."

    /// Destroy buffer and allocation.
    static member destroy (bufferInternal : VulkanBufferInternal) (context : VulkanContext) =
        if bufferInternal.VkBuffer_.IsNotNull then
            Vma.vmaDestroyBuffer (context.VmaAllocator, bufferInternal.VkBuffer, bufferInternal.VmaAllocation_)
            bufferInternal.VkBuffer_ <- VkBuffer.Null

/// Represents a dynamically growing multibuffer with parallel underlying vulkan buffers. Maintains an internal
/// cursor that selects the currently active buffer, which is reset via beginFrame and advanced to the next vulkan
/// buffer with advance. Automatically resizes when usage exceeds its capacity and creates additional buffers when
/// the cursor moves beyond current capacity. This type is intended for transient or frequently updated GPU data
/// such as storage data, uniform data, and streaming data.
type VulkanBuffer =
    private
        { mutable BufferInternalCursor_ : int
          BufferInternals_ : VulkanBufferInternal List
          BufferType_ : VulkanBufferType }

    member private this.BufferInternal =
        this.BufferInternals_[this.BufferInternalCursor_]

    /// Get the vulkan buffer currently at the cursor.
    member this.VkBuffer =
        this.BufferInternal.VkBuffer

    static member private ensureHeight (buffer : VulkanBuffer) context =
        while buffer.BufferInternalCursor_ >= buffer.BufferInternals_.Count do
            let bufferInternals = Array.init buffer.BufferInternals_.Count (fun _ -> VulkanBufferInternal.create buffer.BufferType_ buffer.BufferInternals_[0].Size context)
            buffer.BufferInternals_.AddRange bufferInternals

    /// Expand buffer width as necessary, disregarding all existing content.
    static member ensureWidth size (buffer : VulkanBuffer) context =
        VulkanBuffer.ensureHeight buffer context
        let bufferInternalOld = buffer.BufferInternals_[buffer.BufferInternalCursor_]
        if bufferInternalOld.Size < size then
            let bufferInternalNew = VulkanBufferInternal.create buffer.BufferType_ size context
            VulkanBuffer.copyData bufferInternalOld.Size bufferInternalOld.VkBuffer_ bufferInternalNew.VkBuffer_ context
            buffer.BufferInternals_[buffer.BufferInternalCursor_] <- bufferInternalNew
            VulkanBufferInternal.destroy bufferInternalOld context

    /// Copy data from the source buffer to the destination buffer.
    static member private copyData size source destination (context : VulkanContext) =
        let commandBuffer = VulkanHl.createTransientCommandBuffer context.TransientCommandPool
        let mutable region = VkBufferCopy (size = uint64 size)
        VulkanDeviceApi.vkCmdCopyBuffer (commandBuffer, source, destination, 1u, &&region)
        ConcurrentCommandQueue.executeTransient commandBuffer context.TransientCommandPool context.TransientFence context.RenderQueue

    /// Begin use of this buffer for the current frame.
    static member beginFrame buffer =
        buffer.BufferInternalCursor_ <- 0

    /// Advance the cursor.
    static member advance buffer =
        buffer.BufferInternalCursor_ <- inc buffer.BufferInternalCursor_

    /// Create a new Buffer.
    static member create (bufferType : VulkanBufferType) bufferSize context =
        { BufferInternalCursor_ = 0
          BufferInternals_ = List [VulkanBufferInternal.create bufferType bufferSize context]
          BufferType_ = bufferType }

    /// Write subdata to Buffer. Caller is reponsible for ensuring buffer width and height.
    static member writeSubdata offset alignment size count data (buffer : VulkanBuffer) context =
        VulkanBuffer.ensureHeight buffer context
        VulkanBufferInternal.write offset alignment size count data buffer.BufferInternal context

    /// Flush subdata from Buffer. Caller is reponsible for ensuring buffer width and height.
    static member flushSubdata offset alignment size count (buffer : VulkanBuffer) context =
        VulkanBuffer.ensureHeight buffer context
        VulkanBufferInternal.flush offset alignment size count buffer.BufferInternal context

    /// Upload data to Buffer.
    static member uploadData size count data (buffer : VulkanBuffer) context =
        let bufferSize = size * count
        VulkanBuffer.ensureHeight buffer context
        VulkanBuffer.ensureWidth bufferSize buffer context
        VulkanBufferInternal.write 0 0 size count data buffer.BufferInternal context
        VulkanBufferInternal.flush 0 0 size count buffer.BufferInternal context

    /// Upload a value to Buffer.
    static member uploadValue (value : 'a) buffer context =
        let mutable value = value
        VulkanBuffer.uploadData sizeof<'a> 1 (asNativeInt &value) buffer context

    /// Upload an array to Buffer.
    static member uploadArray (array : 'a array) buffer context =
        use arrayPin = new ArrayPin<_> (array)
        VulkanBuffer.uploadData sizeof<'a> array.Length arrayPin.NativeInt buffer context

    /// Create a staging buffer and stage the data.
    static member stageData size data context =
        let buffer = VulkanBuffer.create Staging size context
        VulkanBuffer.uploadData size 1 data buffer context
        buffer

    /// Create a vertex buffer with data uploaded via staging buffer.
    static member createVertexStaged size data context =
        let stagingBuffer = VulkanBuffer.stageData size data context
        let vertexBuffer = VulkanBuffer.create (Vertex false) size context
        VulkanBuffer.copyData size stagingBuffer.BufferInternal.VkBuffer vertexBuffer.BufferInternal.VkBuffer context
        VulkanBuffer.destroy stagingBuffer context
        vertexBuffer

    /// Create an index buffer with data uploaded via staging buffer.
    static member createIndexStaged size data context =
        let stagingBuffer = VulkanBuffer.stageData size data context
        let indexBuffer = VulkanBuffer.create (Index false) size context
        VulkanBuffer.copyData size stagingBuffer.BufferInternal.VkBuffer indexBuffer.BufferInternal.VkBuffer context
        VulkanBuffer.destroy stagingBuffer context
        indexBuffer

    /// Create a vertex buffer with data uploaded via staging buffer from an array.
    static member createVertexStagedFromArray (array : 'a array) context =
        let size = array.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (array)
        VulkanBuffer.createVertexStaged size arrayPin.NativeInt context

    /// Create an index buffer with data uploaded via staging buffer from an array.
    static member createIndexStagedFromArray (array : 'a array) context =
        let size = array.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (array)
        VulkanBuffer.createIndexStaged size arrayPin.NativeInt context

    /// Create a vertex buffer with data uploaded via staging buffer from memory.
    static member createVertexStagedFromMemory (memory : 'a Memory) context =
        let size = memory.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (memory)
        VulkanBuffer.createVertexStaged size arrayPin.NativeInt context

    /// Create an index buffer with data uploaded via staging buffer from memory.
    static member createIndexStagedFromMemory (memory : 'a Memory) context =
        let size = memory.Length * sizeof<'a>
        use arrayPin = new ArrayPin<_> (memory)
        VulkanBuffer.createIndexStaged size arrayPin.NativeInt context
    
    /// Destroy Buffer.
    static member destroy (buffer : VulkanBuffer) context =
        for i in 0 .. dec buffer.BufferInternals_.Count do
            VulkanBufferInternal.destroy buffer.BufferInternals_[i] context