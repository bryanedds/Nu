// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Collections.Generic
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module Buffer =

    // TODO: DJL: doc comments!
    type BufferType =
        | Staging of InFrame : bool
        | Vertex of UpdateEnabled : bool
        | Index of UploadEnabled : bool
        | Uniform

        member this.IsParallel =
            match this with
            | Staging inFrame -> inFrame
            | Vertex uploadEnabled -> uploadEnabled
            | Index uploadEnabled -> uploadEnabled
            | Uniform -> true
        
        static member private makeInfoInternal size usage =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- VkSharingMode.Exclusive
            info
        
        static member makeInfo size bufferType =
            match bufferType with
            | Staging _ -> BufferType.makeInfoInternal size VkBufferUsageFlags.TransferSrc
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
    
    let private findMemoryType typeFilter properties physicalDevice =
        
        // get memory types
        let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
        Vulkan.vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
        let memoryTypes = NativePtr.fixedBufferToArray<VkMemoryType> (int memProperties.memoryTypeCount) memProperties.memoryTypes

        // try find suitable memory type
        let mutable memoryTypeOpt = None
        for i in 0 .. dec memoryTypes.Length do
            match memoryTypeOpt with
            | None when typeFilter &&& (1u <<< i) <> 0u && memoryTypes.[i].propertyFlags &&& properties = properties ->
                memoryTypeOpt <- Some (uint i)
            | Some _ | None -> ()

        // fin
        match memoryTypeOpt with
        | Some memoryType -> memoryType
        | None -> Log.fail "Failed to find suitable memory type!"
    
    /// Copy data from the source buffer to the destination buffer.
    let private copyData size source destination (vkc : Hl.VulkanContext) =
        let cb = Hl.initCommandBufferTransient vkc.TransientCommandPool vkc.Device
        let mutable region = VkBufferCopy (size = uint64 size)
        Vulkan.vkCmdCopyBuffer (cb, source, destination, 1u, asPointer &region)
        Hl.Queue.executeTransient cb vkc.TransientCommandPool vkc.TransientFence vkc.RenderQueue vkc.Device
    
    let private areAligned a b =
        if a = b then true
        elif a > b then a % b = 0
        else b % a = 0
    
    // TODO: DJL: perhaps calculating this stuff manually is a bad idea?
    let private getStride alignment size =
        if size = 0 then size // just to prevent division by 0; size should be > 0
        elif alignment = 0 then size
        elif alignment = size then size
        elif size > alignment && size % alignment = 0 then size
        elif alignment % size = 0 then alignment
        else (size / alignment + 1) * alignment // stride = lowest multiple of alignment that contains size

    let private alignOffset offset alignment =
        if alignment = 0 then offset // no alignment
        elif offset = 0 then offset // no offset to align
        elif areAligned offset alignment then offset // offset already aligned
        else (offset / alignment + 1) * alignment // offset shifted forward to align

    let private getMinimumBufferSize offset alignment size count =
        let stride = getStride alignment size
        let offset = alignOffset offset alignment
        offset + stride * count
    
    type private Allocation =
        | Vma of VmaAllocation
        | Manual of VkDeviceMemory
    
    /// Abstraction for allocated buffer.
    type private BufferInternal =
        private
            { VkBuffer_ : VkBuffer
              Allocation_ : Allocation
              Mapping_ : voidptr
              Size_ : int
              UploadEnabled_ : bool }

        /// The VkBuffer.
        member this.VkBuffer = this.VkBuffer_
        
        static member private createManual uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =

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
            info.memoryTypeIndex <- findMemoryType memRequirements.memoryTypeBits properties vkc.PhysicalDevice
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

        static member private createVma uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =

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
        static member create uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =
            
            // NOTE: DJL: change this to Manual to bypass VMA and test buffers with manually allocated memory.
            let allocType = Vma Unchecked.defaultof<_>
            
            // create with selected allocation type
            match allocType with
            | Vma _ -> BufferInternal.createVma uploadEnabled bufferInfo vkc
            | Manual _ -> BufferInternal.createManual uploadEnabled bufferInfo vkc
        
        /// Upload data to buffer if upload is enabled, including manual flush.
        static member upload offset alignment size count data bufferInternal (vkc : Hl.VulkanContext) =
            if bufferInternal.UploadEnabled_ then
                if size > 0 then
                    let stride = getStride alignment size
                    let offset = alignOffset offset alignment
                    if offset + stride * count <= bufferInternal.Size_ then
                        
                        // upload as single blob if possible, otherwise upload one value at a time to create padding
                        if size = stride then
                            NativePtr.memCopy offset (size * count) (NativePtr.nativeintToVoidPtr data) bufferInternal.Mapping_
                        else
                            for i in 0 .. dec count do
                                let ptr = NativePtr.add (NativePtr.nativeintToBytePtr data) (i * size)
                                NativePtr.memCopy (offset + i * stride) size (NativePtr.toVoidPtr ptr) bufferInternal.Mapping_
            
                        // manually flush as memory may not be host-coherent on non-windows platforms, see
                        // https://gpuopen-librariesandsdks.github.io/VulkanMemoryAllocator/html/memory_mapping.html#memory_mapping_cache_control
                        match bufferInternal.Allocation_ with
                        | Vma vmaAllocation -> Vma.vmaFlushAllocation (vkc.VmaAllocator, vmaAllocation, uint64 offset, uint64 (stride * count)) |> Hl.check
                        | Manual _ -> () // currently no point bothering

                    else Log.warn "Data upload to Vulkan buffer failed because it exceeded the size of that buffer."
                else Log.warn "Data upload to Vulkan buffer failed because 'size' argument was less than or equal to zero."
            else Log.warn "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

        /// Destroy buffer and allocation.
        static member destroy (bufferInternal : BufferInternal) (vkc : Hl.VulkanContext) =
            match bufferInternal.Allocation_ with
            | Vma vmaAllocation -> Vma.vmaDestroyBuffer (vkc.VmaAllocator, bufferInternal.VkBuffer, vmaAllocation)
            | Manual manualAllocation ->
                if bufferInternal.Mapping_ <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkc.Device, manualAllocation)
                Vulkan.vkDestroyBuffer (vkc.Device, bufferInternal.VkBuffer, nullPtr)
                Vulkan.vkFreeMemory (vkc.Device, manualAllocation, nullPtr)

    /// A buffer interface that internally automates parallelization for frames in flight.
    type private BufferParallel =
        private 
            { BufferInternals : BufferInternal array
              BufferSizes : int array
              BufferType : BufferType }

        member private this.IsParallel = this.BufferType.IsParallel
        member private this.CurrentIndex = if this.IsParallel then Hl.CurrentFrame else 0
        member private this.BufferInternal = this.BufferInternals.[this.CurrentIndex]
        member private this.BufferSize = this.BufferSizes.[this.CurrentIndex]
        
        member this.VkBuffer = this.BufferInternal.VkBuffer
        member this.VkBuffers = Array.map (fun (bufferInternal : BufferInternal) -> bufferInternal.VkBuffer) this.BufferInternals

        /// Create a BufferInternal.
        static member private createBufferInternal size bufferType vkc =
            
            // make info
            let info = BufferType.makeInfo size bufferType

            // create BufferInternal
            match bufferType with
            | Staging _ -> BufferInternal.create true info vkc
            | Vertex uploadEnabled -> BufferInternal.create uploadEnabled info vkc
            | Index uploadEnabled -> BufferInternal.create uploadEnabled info vkc
            | Uniform -> BufferInternal.create true info vkc
        
        /// Create a BufferParallel.
        static member create size (bufferType : BufferType) vkc =
            
            // create buffers and sizes
            let length = if bufferType.IsParallel then Constants.Vulkan.MaxFramesInFlight else 1
            let bufferSizes = Array.create length size
            let bufferInternals = Array.zeroCreate<BufferInternal> length
            for i in 0 .. dec bufferInternals.Length do bufferInternals.[i] <- BufferParallel.createBufferInternal size bufferType vkc

            // make BufferParallel
            let bufferParallel =
                { BufferInternals = bufferInternals 
                  BufferSizes = bufferSizes
                  BufferType = bufferType }

            // fin
            bufferParallel
        
        /// Check that the current buffer is at least as big as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize size (bufferParallel : BufferParallel) vkc =
            if size > bufferParallel.BufferSize then
                BufferInternal.destroy bufferParallel.BufferInternal vkc
                bufferParallel.BufferInternals.[bufferParallel.CurrentIndex] <- BufferParallel.createBufferInternal size bufferParallel.BufferType vkc
                bufferParallel.BufferSizes.[bufferParallel.CurrentIndex] <- size

        /// Upload data to BufferParallel.
        static member upload offset alignment size count data (bufferParallel : BufferParallel) vkc =
            BufferInternal.upload offset alignment size count data bufferParallel.BufferInternal vkc

        /// Destroy BufferParallel. Never call this in frame as previous frame(s) may still be using it.
        static member destroy bufferParallel vkc =
            for i in 0 .. dec bufferParallel.BufferInternals.Length do BufferInternal.destroy bufferParallel.BufferInternals.[i] vkc

    /// The Vulkan buffer interface for Nu, which internally automates parallelization for frames in flight
    /// and manages a multitude of indexable buffer instances to allow repeated use prior to command submission.
    type Buffer =
        private
            { BufferParallels : BufferParallel List
              mutable BufferSize : int
              BufferType : BufferType }

        /// The VkBuffer at index for current frame in flight.
        member this.Item index = this.BufferParallels.[index].VkBuffer

        /// The first VkBuffer for current frame in flight.
        member this.VkBuffer = this.BufferParallels.[0].VkBuffer

        /// The VkBuffers at index for all frames in flight.
        member this.VkBuffers index = this.BufferParallels.[index].VkBuffers
        
        /// Buffer count.
        /// TODO: DJL: probably, this should be limited to buffers being used, i.e. buffers allocated in advance should not be visible to the api.
        member this.Count = this.BufferParallels.Count
        
        static member private updateCount index (buffer : Buffer) vkc =
            while index > dec buffer.Count do
                let bufferParallels = Array.zeroCreate<BufferParallel> 1
                for i in 0 .. dec bufferParallels.Length do bufferParallels.[i] <- BufferParallel.create buffer.BufferSize buffer.BufferType vkc
                buffer.BufferParallels.AddRange bufferParallels
        
        /// Create Buffer.
        static member create bufferSize (bufferType : BufferType) vkc =
            
            // create initial buffers
            let bufferParallels = Array.zeroCreate<BufferParallel> 1 // TODO: DJL: develop advance buffer creation strategy as guided by performance.
            for i in 0 .. dec bufferParallels.Length do bufferParallels.[i] <- BufferParallel.create bufferSize bufferType vkc

            // make Buffer
            let buffer =
                { BufferParallels = List (bufferParallels)
                  BufferSize = bufferSize
                  BufferType = bufferType }
            
            // fin
            buffer

        /// Expand buffer size and count as necessary.
        static member update index size (buffer : Buffer) vkc =
            if size > buffer.BufferSize then buffer.BufferSize <- size
            Buffer.updateCount index buffer vkc
            BufferParallel.updateSize buffer.BufferSize buffer.BufferParallels.[index] vkc
        
        /// Upload data to Buffer at index.
        /// TODO: DJL: maybe remove automatic update as previous uploads to same buffer are wiped!
        static member upload index offset alignment size count data (buffer : Buffer) vkc =
            let bufferSize = getMinimumBufferSize offset alignment size count
            Buffer.update index bufferSize buffer vkc
            BufferParallel.upload offset alignment size count data buffer.BufferParallels.[index] vkc

        /// Upload a value to Buffer at index.
        static member uploadValue index offset alignment (value : 'a) buffer vkc =
            let mutable value = value
            Buffer.upload index offset alignment sizeof<'a> 1 (asNativeInt &value) buffer vkc
        
        /// Upload an array to Buffer at index.
        static member uploadArray index offset alignment (array : 'a array) buffer vkc =
            use arrayPin = new ArrayPin<_> (array)
            Buffer.upload index offset alignment sizeof<'a> array.Length arrayPin.NativeInt buffer vkc
        
        /// Create a staging buffer and stage the data.
        static member stageData size data vkc =
            let buffer = Buffer.create size (Staging false) vkc
            Buffer.upload 0 0 0 size 1 data buffer vkc
            buffer
        
        /// Create a vertex buffer with data uploaded via staging buffer.
        static member createVertexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let vertexBuffer = Buffer.create size (Vertex false) vkc
            copyData size stagingBuffer.VkBuffer vertexBuffer.VkBuffer vkc
            Buffer.destroy stagingBuffer vkc
            vertexBuffer

        /// Create an index buffer with data uploaded via staging buffer.
        static member createIndexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let indexBuffer = Buffer.create size (Index false) vkc
            copyData size stagingBuffer.VkBuffer indexBuffer.VkBuffer vkc
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
            for i in 0 .. dec buffer.Count do BufferParallel.destroy buffer.BufferParallels.[i] vkc
