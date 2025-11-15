// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System.Collections.Generic
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module Buffer =

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
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            info
        
        static member makeInfo size bufferType =
            match bufferType with
            | Staging _ -> BufferType.makeInfoInternal size Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            | Vertex uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
                    else Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT
                BufferType.makeInfoInternal size usage
            | Index uploadEnabled ->
                let usage =
                    if uploadEnabled
                    then Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
                    else Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT
                BufferType.makeInfoInternal size usage
            | Uniform -> BufferType.makeInfoInternal size Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
    
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
    
    let private upload uploadEnabled offset size data mapping =
        if uploadEnabled then
            NativePtr.memCopy offset size (NativePtr.nativeintToVoidPtr data) mapping
        else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

    let private uploadStrided16 uploadEnabled offset typeSize count data mapping =
        if uploadEnabled then
            if typeSize > 16 then Log.fail "'typeSize' must not exceed stride."
            for i in 0 .. dec count do
                let ptr = NativePtr.add (NativePtr.nativeintToBytePtr data) (i * typeSize)
                NativePtr.memCopy ((offset + i) * 16) typeSize (NativePtr.toVoidPtr ptr) mapping
        else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."
    
    /// Copy data from the source buffer to the destination buffer.
    let copyData size source destination (vkc : Hl.VulkanContext) =
        let cb = Hl.beginTransientCommandBlock vkc.TransientCommandPool vkc.Device
        let mutable region = VkBufferCopy (size = uint64 size)
        Vulkan.vkCmdCopyBuffer (cb, source, destination, 1u, asPointer &region)
        Hl.endTransientCommandBlock cb vkc.RenderQueue vkc.TransientCommandPool vkc.TransientFence vkc.Device
    
    type private Allocation =
        | Vma of VmaAllocation
        | Manual of VkDeviceMemory
    
    /// Abstraction for allocated buffer.
    type private BufferInternal =
        private
            { VkBuffer_ : VkBuffer
              Allocation_ : Allocation
              Mapping_ : voidptr
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
                if uploadEnabled then Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ||| Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                else Vulkan.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

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
        static member upload offset size data bufferInternal (vkc : Hl.VulkanContext) =
            upload bufferInternal.UploadEnabled_ offset size data bufferInternal.Mapping_
            match bufferInternal.Allocation_ with
            | Vma vmaAllocation -> Vma.vmaFlushAllocation (vkc.VmaAllocator, vmaAllocation, uint64 offset, uint64 size) |> Hl.check // may be necessary as memory may not be host-coherent
            | Manual _ -> () // no point bothering

        /// Upload data to buffer with a stride of 16 if upload is enabled, including manual flush.
        static member uploadStrided16 offset typeSize count data bufferInternal (vkc : Hl.VulkanContext) =
            uploadStrided16 bufferInternal.UploadEnabled_ offset typeSize count data bufferInternal.Mapping_
            match bufferInternal.Allocation_ with
            | Vma vmaAllocation -> Vma.vmaFlushAllocation (vkc.VmaAllocator, vmaAllocation, uint64 (offset * 16), uint64 (count * 16)) |> Hl.check // may be necessary as memory may not be host-coherent
            | Manual _ -> () // no point bothering
        
        /// Destroy buffer and allocation.
        static member destroy (bufferInternal : BufferInternal) (vkc : Hl.VulkanContext) =
            match bufferInternal.Allocation_ with
            | Vma vmaAllocation -> Vma.vmaDestroyBuffer (vkc.VmaAllocator, bufferInternal.VkBuffer, vmaAllocation)
            | Manual manualAllocation ->
                if bufferInternal.Mapping_ <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkc.Device, manualAllocation)
                Vulkan.vkDestroyBuffer (vkc.Device, bufferInternal.VkBuffer, nullPtr)
                Vulkan.vkFreeMemory (vkc.Device, manualAllocation, nullPtr)

    /// The Vulkan buffer interface for Nu, which internally automates parallelization for frames in flight as necessary.
    type Buffer =
        private 
            { BufferInternals : BufferInternal array
              BufferSizes : int array
              BufferType : BufferType }

        member private this.IsParallel = this.BufferType.IsParallel
        member private this.CurrentIndex = if this.IsParallel then Hl.CurrentFrame else 0
        member private this.BufferInternal = this.BufferInternals.[this.CurrentIndex]
        member private this.BufferSize = this.BufferSizes.[this.CurrentIndex]
        
        /// The currently needed VkBuffer.
        member this.VkBuffer = this.BufferInternal.VkBuffer

        /// All VkBuffers as parallelized.
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
        
        /// Create a Buffer.
        static member create size (bufferType : BufferType) vkc =
            
            // create buffers and sizes
            let length = if bufferType.IsParallel then Constants.Vulkan.MaxFramesInFlight else 1
            let bufferSizes = Array.create length size
            let bufferInternals = Array.zeroCreate<BufferInternal> length
            for i in 0 .. dec bufferInternals.Length do bufferInternals.[i] <- Buffer.createBufferInternal size bufferType vkc

            // make Buffer
            let buffer =
                { BufferInternals = bufferInternals 
                  BufferSizes = bufferSizes
                  BufferType = bufferType }

            // fin
            buffer
        
        /// Check that the current buffer is at least as big as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize size (buffer : Buffer) vkc =
            if size > buffer.BufferSize then
                BufferInternal.destroy buffer.BufferInternal vkc
                buffer.BufferInternals.[buffer.CurrentIndex] <- Buffer.createBufferInternal size buffer.BufferType vkc
                buffer.BufferSizes.[buffer.CurrentIndex] <- size

        /// Upload data to Buffer.
        static member upload offset size data (buffer : Buffer) vkc =
            BufferInternal.upload offset size data buffer.BufferInternal vkc

        /// Upload data to Buffer with a stride of 16.
        static member uploadStrided16 offset typeSize count data (buffer : Buffer) vkc =
            BufferInternal.uploadStrided16 offset typeSize count data buffer.BufferInternal vkc
        
        /// Upload an array to Buffer.
        static member uploadArray offset (array : 'a array) (buffer : Buffer) vkc =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            Buffer.upload offset size arrayPin.NativeInt buffer vkc

        /// Create a Buffer for a stride of 16.
        static member createStrided16 length bufferType vkc =
            Buffer.create (length * 16) bufferType vkc

        /// Create a staging buffer and stage the data.
        static member stageData size data vkc =
            let buffer = Buffer.create size (Staging false) vkc
            Buffer.upload 0 size data buffer vkc
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
        
        /// Destroy Buffer. Never call this in frame as previous frame(s) may still be using it.
        static member destroy buffer vkc =
            for i in 0 .. dec buffer.BufferInternals.Length do BufferInternal.destroy buffer.BufferInternals.[i] vkc

    /// An abstraction for managing Buffers accumulated over multiple draw calls within a frame.
    type BufferAccumulator =
        private
            { Buffers : Buffer List
              mutable BufferSize : int
              BufferType : BufferType }

        /// Get Buffer at index.
        member this.Item index = this.Buffers.[index]

        /// Buffer count.
        member this.Count = this.Buffers.Count
        
        static member private manageBufferCount index (bufferAccumulator : BufferAccumulator) vkc =
            while index > dec bufferAccumulator.Count do
                let buffers = Array.zeroCreate<Buffer> 16
                for i in 0 .. dec buffers.Length do buffers.[i] <- Buffer.create bufferAccumulator.BufferSize bufferAccumulator.BufferType vkc
                bufferAccumulator.Buffers.AddRange buffers
        
        /// Create BufferAccumulator.
        static member create bufferSize (bufferType : BufferType) vkc =
            
            // create initial buffers
            let buffers = Array.zeroCreate<Buffer> 16
            for i in 0 .. dec buffers.Length do buffers.[i] <- Buffer.create bufferSize bufferType vkc

            // make BufferAccumulator
            let bufferAccumulator =
                { Buffers = List (buffers)
                  BufferSize = bufferSize
                  BufferType = bufferType }
            
            // fin
            bufferAccumulator

        /// Check that the current buffer at index is at least as big as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize index size (bufferAccumulator : BufferAccumulator) vkc =
            if size > bufferAccumulator.BufferSize then bufferAccumulator.BufferSize <- size
            BufferAccumulator.manageBufferCount index bufferAccumulator vkc
            Buffer.updateSize bufferAccumulator.BufferSize bufferAccumulator.[index] vkc
        
        /// Upload data to Buffer at index.
        static member upload index offset size data (bufferAccumulator : BufferAccumulator) vkc =
            BufferAccumulator.manageBufferCount index bufferAccumulator vkc
            Buffer.upload offset size data bufferAccumulator.[index] vkc

        /// Upload data to Buffer at index with a stride of 16.
        static member uploadStrided16 index offset typeSize count data (bufferAccumulator : BufferAccumulator) vkc =
            BufferAccumulator.manageBufferCount index bufferAccumulator vkc
            Buffer.uploadStrided16 offset typeSize count data bufferAccumulator.[index] vkc
            
        /// Upload an array to Buffer at index.
        static member uploadArray index offset (array : 'a array) (bufferAccumulator : BufferAccumulator) vkc =
            BufferAccumulator.manageBufferCount index bufferAccumulator vkc
            Buffer.uploadArray offset array bufferAccumulator.[index] vkc
        
        /// Create a BufferAccumulator for a stride of 16.
        static member createStrided16 length bufferType vkc =
            BufferAccumulator.create (length * 16) bufferType vkc
        
        /// Destroy BufferAccumulator.
        static member destroy (bufferAccumulator : BufferAccumulator) vkc =
            for i in 0 .. dec bufferAccumulator.Count do Buffer.destroy bufferAccumulator.[i] vkc
