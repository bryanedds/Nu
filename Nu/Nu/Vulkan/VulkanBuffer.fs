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

    member this.IsParallel =
        match this with
        | Staging -> false
        | Vertex uploadEnabled -> uploadEnabled
        | Index uploadEnabled -> uploadEnabled
        | Uniform -> true
        | Storage -> true

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

/// Abstraction for allocated buffer.
type BufferSingleton =
    private
        { VkBuffer_ : VkBuffer
          Allocation_ : Allocation
          Mapping_ : voidptr
          Size_ : int
          UploadEnabled_ : bool }

    /// The VkBuffer.
    member this.VkBuffer = this.VkBuffer_
    
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
        
        // make BufferSingleton
        let bufferSingleton = 
            { VkBuffer_ = vkBuffer
              Allocation_ = Manual memory
              Mapping_ = mapping
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferSingleton

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

        // make BufferSingleton
        let bufferSingleton =
            { VkBuffer_ = vkBuffer
              Allocation_ = Vma allocation
              Mapping_ = allocationInfo.pMappedData
              Size_ = int bufferInfo.size
              UploadEnabled_ = uploadEnabled }

        // fin
        bufferSingleton

    /// Create BufferSingleton.
    static member create uploadEnabled bufferInfo (vkc : VulkanContext) =
        
        // NOTE: DJL: change this to Manual to bypass VMA and test buffers with manually allocated memory.
        let allocType = Vma Unchecked.defaultof<_>
        
        // create with selected allocation type
        match allocType with
        | Vma _ -> BufferSingleton.createVma uploadEnabled bufferInfo vkc
        | Manual _ -> BufferSingleton.createManual uploadEnabled bufferInfo vkc
    
    /// Upload data to buffer if upload is enabled, including manual flush.
    static member upload offset alignment size count data bufferSingleton (vkc : VulkanContext) =
        if bufferSingleton.UploadEnabled_ then
            if size > 0 then
                let stride = Math.Stride (alignment, size)
                let offset = Math.AlignOffset (offset, alignment)
                if offset + stride * count <= bufferSingleton.Size_ then
                    
                    // upload as single blob if possible, otherwise upload one value at a time to create padding
                    if size = stride then
                        NativePtr.memCopy offset (size * count) (NativePtr.nativeintToVoidPtr data) bufferSingleton.Mapping_
                    else
                        for i in 0 .. dec count do
                            let ptr = NativePtr.add (NativePtr.nativeintToBytePtr data) (i * size)
                            NativePtr.memCopy (offset + i * stride) size (NativePtr.toVoidPtr ptr) bufferSingleton.Mapping_
        
                    // manually flush as memory may not be host-coherent on non-windows platforms, see
                    // https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/memory_mapping.html#memory_mapping_cache_control
                    match bufferSingleton.Allocation_ with
                    | Vma vmaAllocation -> Vma.vmaFlushAllocation (vkc.VmaAllocator, vmaAllocation, uint64 offset, uint64 (stride * count)) |> Hl.check
                    | Manual _ -> () // currently no point bothering

                else Log.warn "Data upload to Vulkan buffer failed because it exceeded the size of that buffer."
            else Log.warn "Data upload to Vulkan buffer failed because 'size' argument was less than or equal to zero."
        else Log.warn "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

    /// Destroy buffer and allocation.
    static member destroy (bufferSingleton : BufferSingleton) (vkc : VulkanContext) =
        match bufferSingleton.Allocation_ with
        | Vma vmaAllocation ->
            Vma.vmaDestroyBuffer (vkc.VmaAllocator, bufferSingleton.VkBuffer, vmaAllocation)
        | Manual manualAllocation ->
            if bufferSingleton.Mapping_ <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkc.Device, manualAllocation)
            Vulkan.vkDestroyBuffer (vkc.Device, bufferSingleton.VkBuffer, nullPtr)
            Vulkan.vkFreeMemory (vkc.Device, manualAllocation, nullPtr)

/// A buffer interface that internally automates parallelization for frames in flight.
type BufferParallel =
    private 
        { BufferSingletons_ : BufferSingleton array
          BufferSizes_ : int array
          BufferType_ : BufferType }

    member private this.IsParallel = this.BufferType_.IsParallel
    member private this.CurrentIndex = if this.IsParallel then Hl.CurrentFrame else 0
    member private this.BufferSingleton = this.BufferSingletons_[this.CurrentIndex]
    member private this.BufferSize = this.BufferSizes_[this.CurrentIndex]

    /// The currently selected vulkan buffer.
    member this.VkBuffer = this.BufferSingleton.VkBuffer

    /// Create a BufferSingleton.
    static member private createBufferSingleton size bufferType vkc =
        
        // make info
        let info = BufferType.makeInfo size bufferType

        // create BufferSingleton
        match bufferType with
        | Staging -> BufferSingleton.create true info vkc
        | Vertex uploadEnabled -> BufferSingleton.create uploadEnabled info vkc
        | Index uploadEnabled -> BufferSingleton.create uploadEnabled info vkc
        | Uniform -> BufferSingleton.create true info vkc
        | Storage -> BufferSingleton.create true info vkc
    
    /// Create a BufferParallel.
    static member create size (bufferType : BufferType) vkc =
        
        // create buffers and sizes
        let length = if bufferType.IsParallel then Constants.Vulkan.MaxFramesInFlight else 1
        let bufferSizes = Array.create length size
        let bufferSingletons = Array.zeroCreate<BufferSingleton> length
        for i in 0 .. dec bufferSingletons.Length do bufferSingletons[i] <- BufferParallel.createBufferSingleton size bufferType vkc

        // make BufferParallel
        let bufferParallel =
            { BufferSingletons_ = bufferSingletons 
              BufferSizes_ = bufferSizes
              BufferType_ = bufferType }

        // fin
        bufferParallel
    
    /// Check that the current buffer is at least as big as the given size, resizing if necessary. If used, must be called every frame.
    static member updateSize size (bufferParallel : BufferParallel) vkc =
        if size > bufferParallel.BufferSize then
            BufferSingleton.destroy bufferParallel.BufferSingleton vkc
            bufferParallel.BufferSingletons_[bufferParallel.CurrentIndex] <- BufferParallel.createBufferSingleton size bufferParallel.BufferType_ vkc
            bufferParallel.BufferSizes_[bufferParallel.CurrentIndex] <- size

    /// Upload data to BufferParallel.
    static member upload offset alignment size count data (bufferParallel : BufferParallel) vkc =
        BufferSingleton.upload offset alignment size count data bufferParallel.BufferSingleton vkc

    /// Destroy BufferParallel. Never call this in frame as previous frame(s) may still be using it.
    static member destroy bufferParallel vkc =
        for i in 0 .. dec bufferParallel.BufferSingletons_.Length do
            BufferSingleton.destroy bufferParallel.BufferSingletons_[i] vkc

/// Represents a dynamically growing multibuffer with parallel underlying vulkan buffers. Maintains an internal
/// cursor that selects the currently active buffer, which is reset via beginFrame and advanced to the next vulkan
/// buffer with advance. Automatically resizes when usage exceeds its capacity and creates additional buffers when
/// the cursor moves beyond current capacity. This type is intended for transient or frequently updated GPU data
/// such as storage data, uniform data, and streaming data.
/// TODO: P0: rename this to BufferStream or BufferMulti since we otherwise have to qualify it to disambiguate from
/// System.Buffer?
type Buffer =
    private
        { BufferParallels_ : BufferParallel List
          mutable BufferCursor_ : int
          BufferType_ : BufferType
          mutable BufferSize_ : int }

    member private this.BufferParallel = this.BufferParallels_[this.BufferCursor_]

    /// Get the vulkan buffer currently at the cursor.
    member this.VkBuffer = this.BufferParallel.VkBuffer

    /// Begin use of this buffer for the current frame.
    static member beginFrame buffer =
        buffer.BufferCursor_ <- 0

    /// Advance the cursor.
    static member advance buffer =
        buffer.BufferCursor_ <- inc buffer.BufferCursor_

    /// Copy data from the source buffer to the destination buffer.
    static member copyData size source destination (vkc : VulkanContext) =
        let commandBuffer = Hl.createTransientCommandBuffer vkc.TransientCommandPool vkc.Device
        let mutable region = VkBufferCopy (size = uint64 size)
        Vulkan.vkCmdCopyBuffer (commandBuffer, source, destination, 1u, asPointer &region)
        CommandQueue.executeTransient commandBuffer vkc.TransientCommandPool vkc.TransientFence vkc.RenderQueue vkc.Device
    
    /// Create a new Buffer.
    static member create bufferSize (bufferType : BufferType) vkc =
        { BufferParallels_ = List [BufferParallel.create bufferSize bufferType vkc]
          BufferCursor_ = 0
          BufferType_ = bufferType
          BufferSize_ = bufferSize }

    /// Expand buffer size and count as necessary.
    static member update size (buffer : Buffer) vkc =
        if size > buffer.BufferSize_ then buffer.BufferSize_ <- size
        while buffer.BufferCursor_ > dec buffer.BufferParallels_.Count do // TODO: P1: consider doubling capacity instead of just increasing to cursor.
            let bufferParallel = BufferParallel.create buffer.BufferSize_ buffer.BufferType_ vkc
            buffer.BufferParallels_.Add bufferParallel
        BufferParallel.updateSize buffer.BufferSize_ buffer.BufferParallel vkc

    /// Upload subdata to Buffer.
    static member uploadSubdata offset alignment size count data (buffer : Buffer) vkc =
        let bufferSize = Math.MinimumBufferSize (offset, alignment, size, count)
        Buffer.update bufferSize buffer vkc
        BufferParallel.upload offset alignment size count data buffer.BufferParallel vkc

    /// Upload a value to Buffer.
    static member uploadValue (value : 'a) buffer vkc =
        let mutable value = value
        Buffer.uploadSubdata 0 0 sizeof<'a> 1 (asNativeInt &value) buffer vkc
    
    /// Upload an array to Buffer.
    static member uploadArray (array : 'a array) buffer vkc =
        use arrayPin = new ArrayPin<_> (array)
        Buffer.uploadSubdata 0 0 sizeof<'a> array.Length arrayPin.NativeInt buffer vkc
    
    /// Create a staging buffer and stage the data.
    static member stageData size data vkc =
        let buffer = Buffer.create size Staging vkc
        Buffer.uploadSubdata 0 0 size 1 data buffer vkc
        buffer
    
    /// Create a vertex buffer with data uploaded via staging buffer.
    static member createVertexStaged size data vkc =
        let stagingBuffer = Buffer.stageData size data vkc
        let vertexBuffer = Buffer.create size (Vertex false) vkc
        Buffer.copyData size stagingBuffer.BufferParallel.VkBuffer vertexBuffer.BufferParallel.VkBuffer vkc
        Buffer.destroy stagingBuffer vkc
        vertexBuffer

    /// Create an index buffer with data uploaded via staging buffer.
    static member createIndexStaged size data vkc =
        let stagingBuffer = Buffer.stageData size data vkc
        let indexBuffer = Buffer.create size (Index false) vkc
        Buffer.copyData size stagingBuffer.BufferParallel.VkBuffer indexBuffer.BufferParallel.VkBuffer vkc
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
        for i in 0 .. dec buffer.BufferParallels_.Count do
            BufferParallel.destroy buffer.BufferParallels_[i] vkc