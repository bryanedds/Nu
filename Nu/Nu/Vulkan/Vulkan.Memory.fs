// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module VulkanMemory =

    type private BufferType =
        | Staging
        | Vertex of bool
        | Index of bool
        | Uniform

        static member private makeInfoInternal size usage =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            info
        
        static member makeInfo size bufferType =
            match bufferType with
            | Staging -> BufferType.makeInfoInternal size Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT
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
    
    /// A manually allocated buffer for diagnostic purposes.
    type ManualBuffer =
        { VkBuffer : VkBuffer 
          Memory : VkDeviceMemory
          Mapping : voidptr }

        static member private findMemoryType typeFilter properties physicalDevice =
            
            // get memory types
            let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
            Vulkan.vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
            
            // TODO: DJL: fix this.
            let memoryTypes = NativePtr.fixedBufferToArray<VkMemoryType> (int memProperties.memoryTypeCount) memProperties.memoryTypes

            // try find suitable memory type
            let mutable memoryTypeOpt = None
            for i in 0 .. dec memoryTypes.Length do
                match memoryTypeOpt with
                | None -> if typeFilter &&& (1u <<< i) <> 0u && memoryTypes.[i].propertyFlags &&& properties = properties then memoryTypeOpt <- Some (uint i)
                | Some _ -> ()

            // fin
            match memoryTypeOpt with
            | Some memoryType -> memoryType
            | None -> Log.fail "Failed to find suitable memory type!"
        
        static member private createInternal uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =

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
            info.memoryTypeIndex <- ManualBuffer.findMemoryType memRequirements.memoryTypeBits properties vkc.PhysicalDevice
            let mutable memory = Unchecked.defaultof<VkDeviceMemory>
            Vulkan.vkAllocateMemory (vkc.Device, asPointer &info, nullPtr, &memory) |> Hl.check

            // bind vkBuffer to memory
            Vulkan.vkBindBufferMemory (vkc.Device, vkBuffer, memory, 0UL) |> Hl.check

            // map memory if upload enabled
            let mappingPtr = NativePtr.stackalloc<voidptr> 1 // must be allocated manually because managed allocation doesn't work
            if uploadEnabled then Vulkan.vkMapMemory (vkc.Device, memory, 0UL, Vulkan.VK_WHOLE_SIZE, VkMemoryMapFlags.None, mappingPtr) |> Hl.check
            let mapping = NativePtr.read mappingPtr // TODO: DJL: find out if this needs to be manually freed.
            
            // make ManualBuffer
            let manualBuffer = 
                { VkBuffer = vkBuffer
                  Memory = memory
                  Mapping = mapping }

            // fin
            manualBuffer

        /// Upload data to buffer if upload is enabled.
        static member upload offset size data buffer _ =
            upload (buffer.Mapping <> Unchecked.defaultof<voidptr>) offset size data buffer.Mapping

        /// Upload data to buffer with a stride of 16 if upload is enabled.
        static member uploadStrided16 offset typeSize count data buffer _ =
            uploadStrided16 (buffer.Mapping <> Unchecked.defaultof<voidptr>) offset typeSize count data buffer.Mapping
        
        /// Upload an array to buffer if upload is enabled.
        static member uploadArray offset (array : 'a array) buffer vkc =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            ManualBuffer.upload offset size arrayPin.NativeInt buffer vkc
        
        /// Create a manually allocated staging buffer.
        static member createStaging size vkc =
            let info = BufferType.makeInfo size Staging
            let manualBuffer = ManualBuffer.createInternal true info vkc
            manualBuffer

        /// Create a manually allocated vertex buffer.
        static member createVertex uploadEnabled size vkc =
            let info = BufferType.makeInfo size (Vertex uploadEnabled)
            let manualBuffer = ManualBuffer.createInternal uploadEnabled info vkc
            manualBuffer

        /// Create a manually allocated index buffer.
        static member createIndex uploadEnabled size vkc =
            let info = BufferType.makeInfo size (Index uploadEnabled)
            let manualBuffer = ManualBuffer.createInternal uploadEnabled info vkc
            manualBuffer
        
        /// Create a manually allocated uniform buffer.
        static member createUniform size vkc =
            let info = BufferType.makeInfo size Uniform
            let manualBuffer = ManualBuffer.createInternal true info vkc
            manualBuffer
        
        /// Destroy a ManualBuffer.
        static member destroy buffer (vkc : Hl.VulkanContext) =
            if buffer.Mapping <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkc.Device, buffer.Memory)
            Vulkan.vkDestroyBuffer (vkc.Device, buffer.VkBuffer, nullPtr)
            Vulkan.vkFreeMemory (vkc.Device, buffer.Memory, nullPtr)
    
    /// Abstraction for vma allocated buffer.
    type Buffer =
        private
            { _VkBuffer : VkBuffer
              _Allocation : VmaAllocation
              _Mapping : voidptr
              _UploadEnabled : bool }

        /// The VkBuffer.
        member this.VkBuffer = this._VkBuffer
        
        static member private createInternal uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =

            // allocation create info
            let mutable info = VmaAllocationCreateInfo ()
            info.usage <- VmaMemoryUsage.Auto
            if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

            // create vma buffer
            let mutable vkBuffer = Unchecked.defaultof<VkBuffer>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            let mutable allocationInfo = Unchecked.defaultof<VmaAllocationInfo>
            Vma.vmaCreateBuffer (vkc.VmaAllocator, &bufferInfo, &info, &vkBuffer, &allocation, asPointer &allocationInfo) |> Hl.check

            // make Buffer
            let buffer =
                { _VkBuffer = vkBuffer
                  _Allocation = allocation
                  _Mapping = allocationInfo.pMappedData
                  _UploadEnabled = uploadEnabled }

            // fin
            buffer

        /// Copy data from the source buffer to the destination buffer.
        static member private copyData size (source : Buffer) (destination : Buffer) (vkc : Hl.VulkanContext) =

            // create command buffer for transfer
            let mutable cb = Hl.allocateCommandBuffer vkc.TransientCommandPool vkc.Device

            // reset command buffer and begin recording
            Vulkan.vkResetCommandPool (vkc.Device, vkc.TransientCommandPool, VkCommandPoolResetFlags.None) |> Hl.check
            let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
            Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> Hl.check

            // copy data
            let mutable region = VkBufferCopy (size = uint64 size)
            Vulkan.vkCmdCopyBuffer (cb, source.VkBuffer, destination.VkBuffer, 1u, asPointer &region)

            // execute command
            Vulkan.vkEndCommandBuffer cb |> Hl.check
            let mutable sInfo = VkSubmitInfo ()
            sInfo.commandBufferCount <- 1u
            sInfo.pCommandBuffers <- asPointer &cb
            Vulkan.vkQueueSubmit (vkc.GraphicsQueue, 1u, asPointer &sInfo, vkc.ResourceReadyFence) |> Hl.check
            Hl.awaitFence vkc.ResourceReadyFence vkc.Device

        /// Upload data to buffer if upload is enabled.
        static member upload offset size data buffer (vkc : Hl.VulkanContext) =
            upload buffer._UploadEnabled offset size data buffer._Mapping
            Vma.vmaFlushAllocation (vkc.VmaAllocator, buffer._Allocation, uint64 offset, uint64 size) |> Hl.check // may be necessary as memory may not be host-coherent

        /// Upload data to buffer with a stride of 16 if upload is enabled.
        static member uploadStrided16 offset typeSize count data buffer (vkc : Hl.VulkanContext) =
            uploadStrided16 buffer._UploadEnabled offset typeSize count data buffer._Mapping
            Vma.vmaFlushAllocation (vkc.VmaAllocator, buffer._Allocation, uint64 (offset * 16), uint64 (count * 16)) |> Hl.check // may be necessary as memory may not be host-coherent
        
        /// Upload an array to buffer if upload is enabled.
        static member uploadArray offset (array : 'a array) buffer vkc =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            Buffer.upload offset size arrayPin.NativeInt buffer vkc

        /// Create an allocated staging buffer.
        static member createStaging size vkc =
            let info = BufferType.makeInfo size Staging
            let buffer = Buffer.createInternal true info vkc
            buffer

        /// Create an allocated vertex buffer.
        static member createVertex uploadEnabled size vkc =
            let info = BufferType.makeInfo size (Vertex uploadEnabled)
            let buffer = Buffer.createInternal uploadEnabled info vkc
            buffer

        /// Create an allocated index buffer.
        static member createIndex uploadEnabled size vkc =
            let info = BufferType.makeInfo size (Index uploadEnabled)
            let buffer = Buffer.createInternal uploadEnabled info vkc
            buffer

        /// Create an allocated uniform buffer.
        static member createUniform size vkc =
            let info = BufferType.makeInfo size Uniform
            let buffer = Buffer.createInternal true info vkc
            buffer
        
        /// Create an allocated staging buffer and stage the data.
        static member stageData size data vkc =
            let buffer = Buffer.createStaging size vkc
            Buffer.upload 0 size data buffer vkc
            buffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer.
        static member createVertexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let vertexBuffer = Buffer.createVertex false size vkc
            Buffer.copyData size stagingBuffer vertexBuffer vkc
            Buffer.destroy stagingBuffer vkc
            vertexBuffer

        /// Create an allocated index buffer with data uploaded via staging buffer.
        static member createIndexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let indexBuffer = Buffer.createIndex false size vkc
            Buffer.copyData size stagingBuffer indexBuffer vkc
            Buffer.destroy stagingBuffer vkc
            indexBuffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer from an array.
        static member createVertexStagedFromArray (array : 'a array) vkc =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            Buffer.createVertexStaged size arrayPin.NativeInt vkc

        /// Create an allocated index buffer with data uploaded via staging buffer from an array.
        static member createIndexStagedFromArray (array : 'a array) vkc =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            Buffer.createIndexStaged size arrayPin.NativeInt vkc
        
        /// Destroy buffer and allocation.
        static member destroy buffer vkc =
            Vma.vmaDestroyBuffer (vkc.VmaAllocator, buffer.VkBuffer, buffer._Allocation)

    /// Abstraction for vma allocated image.
    type Image =
        private
            { _VkImage : VkImage
              _Allocation : VmaAllocation }

        /// The VkImage.
        member this.VkImage = this._VkImage
        
        /// Destroy vkImage and allocation.
        static member destroy (image : Image) (vkc : Hl.VulkanContext) =
            Vma.vmaDestroyImage (vkc.VmaAllocator, image.VkImage, image._Allocation)

        /// Create an Image.
        static member create imageInfo (vkc : Hl.VulkanContext) =
            let info = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable vkImage = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (vkc.VmaAllocator, &imageInfo, &info, &vkImage, &allocation, nullPtr) |> Hl.check
            let image = { _VkImage = vkImage; _Allocation = allocation }
            image

    /// A set of upload enabled buffers for each frame in flight.
    type FifBuffer =
        private 
            { Buffers : Buffer array
              BufferSizes : int array
              BufferType : BufferType }

        member private this.Current = this.Buffers.[Hl.CurrentFrame]
        
        /// The VkBuffer for the current frame.
        member this.VkBuffer = this.Current.VkBuffer

        /// The VkBuffer for each frame in flight.
        member this.PerFrameBuffers = Array.map (fun (buffer : Buffer) -> buffer.VkBuffer) this.Buffers

        /// Create a Buffer based on usage.
        static member private createBuffer size bufferType vkc =
            match bufferType with
            | Staging -> Buffer.createStaging size vkc
            | Vertex uploadEnabled -> Buffer.createVertex uploadEnabled size vkc
            | Index uploadEnabled -> Buffer.createIndex uploadEnabled size vkc
            | Uniform -> Buffer.createUniform size vkc
        
        /// Create a FifBuffer.
        static member private createInternal size bufferType vkc =
            
            // create buffers and sizes
            let bufferSizes = Array.create Constants.Vulkan.MaxFramesInFlight size
            let buffers = Array.zeroCreate<Buffer> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec buffers.Length do buffers.[i] <- FifBuffer.createBuffer size bufferType vkc

            // make FifBuffer
            let fifBuffer =
                { Buffers = buffers 
                  BufferSizes = bufferSizes
                  BufferType = bufferType }

            // fin
            fifBuffer
        
        /// Check that the current buffer is at least as big as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize size fifBuffer vkc =
            if size > fifBuffer.BufferSizes.[Hl.CurrentFrame] then
                Buffer.destroy fifBuffer.Current vkc
                fifBuffer.Buffers.[Hl.CurrentFrame] <- FifBuffer.createBuffer size fifBuffer.BufferType vkc
                fifBuffer.BufferSizes.[Hl.CurrentFrame] <- size

        /// Upload data to FifBuffer.
        static member upload offset size data (fifBuffer : FifBuffer) vkc =
            Buffer.upload offset size data fifBuffer.Current vkc

        /// Upload data to FifBuffer with a stride of 16.
        static member uploadStrided16 offset typeSize count data (fifBuffer : FifBuffer) vkc =
            Buffer.uploadStrided16 offset typeSize count data fifBuffer.Current vkc
        
        
        /// Upload an array to FifBuffer.
        static member uploadArray offset array (fifBuffer : FifBuffer) vkc =
            Buffer.uploadArray offset array fifBuffer.Current vkc

        /// Create a staging FifBuffer.
        static member createStaging size vkc =
            FifBuffer.createInternal size Staging vkc
        
        /// Create a vertex FifBuffer.
        static member createVertex size vkc =
            FifBuffer.createInternal size (Vertex true) vkc

        /// Create an index FifBuffer.
        static member createIndex size vkc =
            FifBuffer.createInternal size (Index true) vkc

        /// Create a uniform FifBuffer.
        static member createUniform size vkc =
            FifBuffer.createInternal size Uniform vkc

        /// Create a uniform FifBuffer for a stride of 16.
        static member createUniformStrided16 length vkc =
            FifBuffer.createUniform (length * 16) vkc

        /// Destroy FifBuffer.
        static member destroy fifBuffer vkc =
            for i in 0 .. dec fifBuffer.Buffers.Length do Buffer.destroy fifBuffer.Buffers.[i] vkc