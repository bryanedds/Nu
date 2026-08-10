// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.Numerics
open FSharp.NativeInterop
open Vortice.Vulkan
open Prime
open Nu

// The type of vulkan buffer being utilized.
type BufferType =
    | Staging
    | Vertex of UploadEnabled : bool
    | Index of UploadEnabled : bool
    | Instance
    | Uniform
    | Storage

/// Wraps a vulkan buffer and its metadata.
type BufferWrapper =
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

    static member private makeCreateInfo usage (size : int) =
        let mutable info = VkBufferCreateInfo ()
        info.usage <- usage
        info.size <- uint64 size
        info.sharingMode <- VkSharingMode.Exclusive
        info

    /// Create buffer wrapper.
    static member createPlus (uploadEnabled, bufferUsage, bufferInfo : VkBufferCreateInfo byref, context : VulkanContext) =

        // allocation create info
        let mutable info = VmaAllocationCreateInfo ()
        info.usage <- bufferUsage
        if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

        // create vma buffer
        let mutable vkBuffer = Unchecked.defaultof<VkBuffer>
        let mutable vmaAllocation = Unchecked.defaultof<VmaAllocation>
        let mutable vmaAllocationInfo = Unchecked.defaultof<VmaAllocationInfo>
        Vma.vmaCreateBuffer (context.VmaAllocator, &&bufferInfo, &&info, &vkBuffer, &vmaAllocation, &vmaAllocationInfo) |> Hl.check

        // make buffer wrapper
        let bufferWrapper =
            { VkBuffer_ = vkBuffer
              VmaAllocation_ = vmaAllocation
              Mapping_ = vmaAllocationInfo.pMappedData
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferWrapper

    /// Create buffer wrapper.
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
            | Storage -> struct (true, VmaMemoryUsage.AutoPreferDevice)

        // make create info
        let mutable createInfo =
            match bufferType with
            | Staging ->
                let usage = VkBufferUsageFlags.TransferSrc
                BufferWrapper.makeCreateInfo usage bufferSize
            | Vertex uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                    else VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferDst
                BufferWrapper.makeCreateInfo usage bufferSize
            | Index uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then VkBufferUsageFlags.IndexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                    else VkBufferUsageFlags.IndexBuffer ||| VkBufferUsageFlags.TransferDst
                BufferWrapper.makeCreateInfo usage bufferSize
            | Instance ->
                let usage = VkBufferUsageFlags.VertexBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                BufferWrapper.makeCreateInfo usage bufferSize
            | Uniform ->
                let usage = VkBufferUsageFlags.UniformBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                BufferWrapper.makeCreateInfo usage bufferSize
            | Storage ->
                let usage = VkBufferUsageFlags.StorageBuffer ||| VkBufferUsageFlags.TransferSrc ||| VkBufferUsageFlags.TransferDst
                BufferWrapper.makeCreateInfo usage bufferSize

        // make buffer
        BufferWrapper.createPlus (uploadEnabled, bufferUsage, &createInfo, context)

    /// Write data to buffer if upload is enabled.
    static member write offset alignment size count data bufferWrapper (_ : VulkanContext) =
        if bufferWrapper.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferWrapper.Size_ then
                    
                    // upload as single blob if possible, otherwise upload one value at a time to create padding
                    if size = stride then
                        NativePtr.memCopy offset (size * count) (NativePtr.nativeintToVoidPtr data) bufferWrapper.Mapping_
                    else
                        for i in 0 .. dec count do
                            let ptr = NativePtr.add (NativePtr.nativeintToBytePtr data) (i * size)
                            NativePtr.memCopy (offset + i * stride) size (NativePtr.toVoidPtr ptr) bufferWrapper.Mapping_

                else Log.warn "Write to Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Write to Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Write to Vulkan buffer failed because upload was not enabled for that buffer."

    /// Flush data to buffer if upload is enabled.
    static member flush offset alignment size count bufferWrapper (context : VulkanContext) =
        if bufferWrapper.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferWrapper.Size_ then

                    // manually flush as memory may not be host-coherent on non-windows platforms, see
                    // https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/memory_mapping.html#memory_mapping_cache_control
                    Vma.vmaFlushAllocation (context.VmaAllocator, bufferWrapper.VmaAllocation_, uint64 offset, uint64 (stride * count)) |> Hl.check

                else Log.warn "Flush of Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Flush of Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Flush of Vulkan buffer failed because upload was not enabled for that buffer."

    /// Destroy buffer and allocation.
    static member destroy (bufferWrapper : BufferWrapper) (context : VulkanContext) =
        if bufferWrapper.VkBuffer_.IsNotNull then
            Vma.vmaDestroyBuffer (context.VmaAllocator, bufferWrapper.VkBuffer, bufferWrapper.VmaAllocation_)
            bufferWrapper.VkBuffer_ <- VkBuffer.Null

/// Represents a dynamically growing multibuffer with parallel underlying vulkan buffers. Maintains an internal
/// cursor that selects the currently active buffer, which is reset via beginFrame and advanced to the next vulkan
/// buffer with advance. Automatically resizes when usage exceeds its capacity and creates additional buffers when
/// the cursor moves beyond current capacity. This type is intended for transient or frequently updated GPU data
/// such as storage data, uniform data, and streaming data.
/// NOTE: this type's name is prefixed with "Vulkan" in order to reliably disambiguate it from System.Buffer.
type VulkanBuffer =
    private
        { mutable BufferWrappersCursor_ : int
          BufferWrappers_ : BufferWrapper List
          BufferType_ : BufferType
          BufferSize_ : int }

    member private this.BufferWrapper =
        this.BufferWrappers_[this.BufferWrappersCursor_]

    /// Get the vulkan buffer currently at the cursor.
    member this.VkBuffer =
        this.BufferWrapper.VkBuffer

    static member private ensureHeight (buffer : VulkanBuffer) context =
        while buffer.BufferWrappersCursor_ >= buffer.BufferWrappers_.Count do
            let bufferWrappers = Array.init buffer.BufferWrappers_.Count (fun _ -> BufferWrapper.create buffer.BufferType_ buffer.BufferSize_ context)
            buffer.BufferWrappers_.AddRange bufferWrappers

    /// Expand current buffer width as necessary.
    /// OPTIMIZATION: this may swap unutilized buffers around to best utilize existing buffers.
    static member ensureWidth size (buffer : VulkanBuffer) context =

        // ensure height before attempt to ensure width
        VulkanBuffer.ensureHeight buffer context

        // ensure current buffer is wide enough
        let cursor = buffer.BufferWrappersCursor_
        if buffer.BufferWrappers_[cursor].Size < size then

            // when too narrow, find the best fit buffer as well as largest buffer and...
            let mutable bestFitIndex = -1
            let mutable bestFitSize = Int32.MaxValue
            let mutable largestIndex = cursor
            let mutable largestSize = buffer.BufferWrappers_[cursor].Size
            for i in inc cursor .. dec buffer.BufferWrappers_.Count do
                let candidate = buffer.BufferWrappers_[i]
                if candidate.Size >= size && candidate.Size < bestFitSize then
                    bestFitIndex <- i
                    bestFitSize <- candidate.Size
                if candidate.Size > largestSize then
                    largestIndex <- i
                    largestSize <- candidate.Size

            // when a fit is found...
            if bestFitIndex > -1 then

                // swap buffer into current buffer
                let tmp = buffer.BufferWrappers_[cursor]
                buffer.BufferWrappers_[cursor] <- buffer.BufferWrappers_[bestFitIndex]
                buffer.BufferWrappers_[bestFitIndex] <- tmp

            // otherwise when no fit is found...
            else

                // increase the width of the largest buffer found
                let bufferWrapperOld = buffer.BufferWrappers_[largestIndex]
                let bufferWrapperNew = BufferWrapper.create buffer.BufferType_ size context
                VulkanBuffer.copyData bufferWrapperOld.Size bufferWrapperOld.VkBuffer_ bufferWrapperNew.VkBuffer_ context
                buffer.BufferWrappers_[largestIndex] <- bufferWrapperNew
                BufferWrapper.destroy bufferWrapperOld context

                // ...and swap it if it's not already the current buffer
                if largestIndex <> cursor then
                    let tmp = buffer.BufferWrappers_[cursor]
                    buffer.BufferWrappers_[cursor] <- buffer.BufferWrappers_[largestIndex]
                    buffer.BufferWrappers_[largestIndex] <- tmp

    /// Copy data from the source buffer to the destination buffer.
    static member private copyData size source destination (context : VulkanContext) =
        let commandBuffer = Hl.createTransientCommandBuffer context.TransientCommandPool
        let mutable region = VkBufferCopy (size = uint64 size)
        DeviceApi.vkCmdCopyBuffer (commandBuffer, source, destination, 1u, &&region)
        ConcurrentCommandQueue.runTransient commandBuffer context.TransientCommandPool context.TransientFence context.RenderQueue

    /// Begin use of this buffer for the current frame.
    static member beginFrame buffer =
        buffer.BufferWrappersCursor_ <- 0

    /// Advance the cursor.
    static member advance buffer =
        buffer.BufferWrappersCursor_ <- inc buffer.BufferWrappersCursor_

    /// Create a new buffer.
    static member create (bufferType : BufferType) bufferSize context =
        { BufferWrappersCursor_ = 0
          BufferWrappers_ = List [BufferWrapper.create bufferType bufferSize context]
          BufferType_ = bufferType
          BufferSize_ = bufferSize }

    /// Write subdata to buffer. Caller is reponsible for ensuring buffer width and height.
    static member writeSubdata offset alignment size count data (buffer : VulkanBuffer) context =
        VulkanBuffer.ensureHeight buffer context
        BufferWrapper.write offset alignment size count data buffer.BufferWrapper context

    /// Flush subdata from buffer. Caller is reponsible for ensuring buffer width and height.
    static member flushSubdata offset alignment size count (buffer : VulkanBuffer) context =
        VulkanBuffer.ensureHeight buffer context
        BufferWrapper.flush offset alignment size count buffer.BufferWrapper context

    /// Upload data to buffer.
    static member uploadData size count data (buffer : VulkanBuffer) context =
        let bufferSize = size * count
        VulkanBuffer.ensureHeight buffer context
        VulkanBuffer.ensureWidth bufferSize buffer context
        BufferWrapper.write 0 0 size count data buffer.BufferWrapper context
        BufferWrapper.flush 0 0 size count buffer.BufferWrapper context

    /// Upload a value to buffer.
    static member uploadValue (value : 'a) buffer context =
        let mutable value = value
        VulkanBuffer.uploadData sizeof<'a> 1 (asNativeInt &value) buffer context

    /// Upload an array to buffer.
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
        VulkanBuffer.copyData size stagingBuffer.BufferWrapper.VkBuffer vertexBuffer.BufferWrapper.VkBuffer context
        VulkanBuffer.destroy stagingBuffer context
        vertexBuffer

    /// Create an index buffer with data uploaded via staging buffer.
    static member createIndexStaged size data context =
        let stagingBuffer = VulkanBuffer.stageData size data context
        let indexBuffer = VulkanBuffer.create (Index false) size context
        VulkanBuffer.copyData size stagingBuffer.BufferWrapper.VkBuffer indexBuffer.BufferWrapper.VkBuffer context
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
    
    /// Destroy buffer.
    static member destroy (buffer : VulkanBuffer) context =
        for i in 0 .. dec buffer.BufferWrappers_.Count do
            BufferWrapper.destroy buffer.BufferWrappers_[i] context