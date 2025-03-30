// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module VulkanMemory =

    /// A manually allocated buffer for diagnostic purposes.
    type ManualAllocatedBuffer =
        { Buffer : VkBuffer 
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
        
        static member private createInternal uploadEnabled bufferInfo (vkg : Hl.VulkanGlobal) =

            // create buffer
            let mutable buffer = Unchecked.defaultof<VkBuffer>
            Vulkan.vkCreateBuffer (vkg.Device, &bufferInfo, nullPtr, asPointer &buffer) |> Hl.check

            // get buffer memory requirements
            let mutable memRequirements = Unchecked.defaultof<VkMemoryRequirements>
            Vulkan.vkGetBufferMemoryRequirements (vkg.Device, buffer, &memRequirements)

            // choose appropriate memory properties
            let properties =
                if uploadEnabled then Vulkan.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT ||| Vulkan.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                else Vulkan.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT

            // allocate memory
            let mutable info = VkMemoryAllocateInfo ()
            info.allocationSize <- memRequirements.size
            info.memoryTypeIndex <- ManualAllocatedBuffer.findMemoryType memRequirements.memoryTypeBits properties vkg.PhysicalDevice
            let mutable memory = Unchecked.defaultof<VkDeviceMemory>
            Vulkan.vkAllocateMemory (vkg.Device, asPointer &info, nullPtr, &memory) |> Hl.check

            // bind buffer to memory
            Vulkan.vkBindBufferMemory (vkg.Device, buffer, memory, 0UL) |> Hl.check

            // map memory if upload enabled
            let mappingPtr = NativePtr.stackalloc<voidptr> 1 // must be allocated manually because managed allocation doesn't work
            if uploadEnabled then Vulkan.vkMapMemory (vkg.Device, memory, 0UL, Vulkan.VK_WHOLE_SIZE, VkMemoryMapFlags.None, mappingPtr) |> Hl.check
            let mapping = NativePtr.read mappingPtr // TODO: DJL: find out if this needs to be manually freed.
            
            // make ManualAllocatedBuffer
            let manualAllocatedBuffer = 
                { Buffer = buffer
                  Memory = memory
                  Mapping = mapping }

            // fin
            manualAllocatedBuffer

        /// Upload data to buffer if upload is enabled.
        static member upload offset size nint buffer _ =
            if buffer.Mapping <> Unchecked.defaultof<voidptr>
            then NativePtr.memCopy offset size (NativePtr.nativeintToVoidPtr nint) buffer.Mapping
            else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

        /// Upload an array to buffer if upload is enabled.
        static member uploadArray offset (array : 'a array) buffer vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            ManualAllocatedBuffer.upload offset size arrayPin.NativeInt buffer vkg
        
        /// Create a manually allocated uniform buffer.
        static member createUniform size vkg =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = ManualAllocatedBuffer.createInternal true info vkg
            allocatedBuffer
        
        /// Destroy a ManualAllocatedBuffer.
        static member destroy buffer (vkg : Hl.VulkanGlobal) =
            if buffer.Mapping <> Unchecked.defaultof<voidptr> then Vulkan.vkUnmapMemory (vkg.Device, buffer.Memory)
            Vulkan.vkDestroyBuffer (vkg.Device, buffer.Buffer, nullPtr)
            Vulkan.vkFreeMemory (vkg.Device, buffer.Memory, nullPtr)
    
    /// A set of upload enabled manually allocated buffers for each frame in flight.
    type ManualFifBuffer =
        private { ManualAllocatedBuffers : ManualAllocatedBuffer array
                  BufferSizes : int array
                  BufferUsage : VkBufferUsageFlags }

        /// The VkBuffer for the current frame.
        member this.Buffer = this.ManualAllocatedBuffers.[Hl.CurrentFrame].Buffer

        /// The VkBuffer for each frame in flight.
        member this.PerFrameBuffers = Array.map (fun manualAllocatedBuffer -> manualAllocatedBuffer.Buffer) this.ManualAllocatedBuffers

        /// Create a ManualAllocatedBuffer based on usage.
        static member private createBuffer size usage vkg =
            match usage with
            | Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT -> ManualAllocatedBuffer.createUniform size vkg
            | _ -> Log.fail "Invalid VkBufferUsageFlags value for ManualFifBuffer."
        
        /// Create a ManualFifBuffer.
        static member private createInternal size (usage : VkBufferUsageFlags) vkg =
            
            // create buffers and sizes
            let bufferSizes = Array.create Constants.Vulkan.MaxFramesInFlight size
            let manualAllocatedBuffers = Array.zeroCreate<ManualAllocatedBuffer> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec manualAllocatedBuffers.Length do manualAllocatedBuffers.[i] <- ManualFifBuffer.createBuffer size usage vkg

            // make FifBuffer
            let manualFifBuffer =
                { ManualAllocatedBuffers = manualAllocatedBuffers
                  BufferSizes = bufferSizes
                  BufferUsage = usage }

            // fin
            manualFifBuffer
        
        /// Upload data to ManualFifBuffer.
        static member upload offset size nint manualFifBuffer vkg =
            ManualAllocatedBuffer.upload offset size nint manualFifBuffer.ManualAllocatedBuffers.[Hl.CurrentFrame] vkg

        /// Upload an array to ManualFifBuffer.
        static member uploadArray offset array manualFifBuffer vkg =
            ManualAllocatedBuffer.uploadArray offset array manualFifBuffer.ManualAllocatedBuffers.[Hl.CurrentFrame] vkg

        /// Create a uniform ManualFifBuffer.
        static member createUniform size vkg =
            ManualFifBuffer.createInternal size Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT vkg

        /// Destroy ManualFifBuffer.
        static member destroy manualFifBuffer vkg =
            for i in 0 .. dec manualFifBuffer.ManualAllocatedBuffers.Length do ManualAllocatedBuffer.destroy manualFifBuffer.ManualAllocatedBuffers.[i] vkg
    
    /// Abstraction for vma allocated buffer.
    type AllocatedBuffer =
        { Buffer : VkBuffer
          Allocation : VmaAllocation
          AllocationInfo : VmaAllocationInfo
          UploadEnabled : bool }

        static member private createInternal uploadEnabled bufferInfo (vkg : Hl.VulkanGlobal) =

            // allocation create info
            let mutable info = VmaAllocationCreateInfo ()
            info.usage <- VmaMemoryUsage.Auto
            if uploadEnabled then info.flags <- VmaAllocationCreateFlags.HostAccessSequentialWrite ||| VmaAllocationCreateFlags.Mapped

            // create vma buffer
            let mutable buffer = Unchecked.defaultof<VkBuffer>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            let mutable allocationInfo = Unchecked.defaultof<VmaAllocationInfo>
            Vma.vmaCreateBuffer (vkg.VmaAllocator, &bufferInfo, &info, &buffer, &allocation, asPointer &allocationInfo) |> Hl.check

            // make AllocatedBuffer
            let allocatedBuffer =
                { Buffer = buffer
                  Allocation = allocation
                  AllocationInfo = allocationInfo
                  UploadEnabled = uploadEnabled }

            // fin
            allocatedBuffer

        /// Copy data from the source buffer to the destination buffer.
        static member private copyData size source destination (vkg : Hl.VulkanGlobal) =

            // create command buffer for transfer
            let mutable cb = Hl.allocateCommandBuffer vkg.TransientCommandPool vkg.Device

            // reset command buffer and begin recording
            Vulkan.vkResetCommandPool (vkg.Device, vkg.TransientCommandPool, VkCommandPoolResetFlags.None) |> Hl.check
            let mutable cbInfo = VkCommandBufferBeginInfo (flags = Vulkan.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
            Vulkan.vkBeginCommandBuffer (cb, asPointer &cbInfo) |> Hl.check

            // copy data
            let mutable region = VkBufferCopy (size = uint64 size)
            Vulkan.vkCmdCopyBuffer (cb, source.Buffer, destination.Buffer, 1u, asPointer &region)

            // execute command
            Vulkan.vkEndCommandBuffer cb |> Hl.check
            let mutable sInfo = VkSubmitInfo ()
            sInfo.commandBufferCount <- 1u
            sInfo.pCommandBuffers <- asPointer &cb
            Vulkan.vkQueueSubmit (vkg.GraphicsQueue, 1u, asPointer &sInfo, vkg.ResourceReadyFence) |> Hl.check
            Hl.awaitFence vkg.ResourceReadyFence vkg.Device

        /// Upload data to buffer if upload is enabled.
        static member upload offset size nint buffer (vkg : Hl.VulkanGlobal) =
            if buffer.UploadEnabled then
                NativePtr.memCopy offset size (NativePtr.nativeintToVoidPtr nint) buffer.AllocationInfo.pMappedData
                Vma.vmaFlushAllocation (vkg.VmaAllocator, buffer.Allocation, uint64 offset, uint64 size) |> Hl.check // may be necessary as memory may not be host-coherent
            else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."

        /// Upload data to buffer with a stride of 16 if upload is enabled.
        static member uploadStrided16 offset typeSize count nint buffer (vkg : Hl.VulkanGlobal) =
            if buffer.UploadEnabled then
                if typeSize > 16 then Log.fail "'typeSize' must not exceed stride."
                for i in 0 .. dec count do
                    let ptr = NativePtr.add (NativePtr.nativeintToBytePtr nint) (i * typeSize)
                    NativePtr.memCopy ((offset + i) * 16) typeSize (NativePtr.toVoidPtr ptr) buffer.AllocationInfo.pMappedData
                Vma.vmaFlushAllocation (vkg.VmaAllocator, buffer.Allocation, uint64 (offset * 16), uint64 (count * 16)) |> Hl.check // may be necessary as memory may not be host-coherent
            else Log.fail "Data upload to Vulkan buffer failed because upload was not enabled for that buffer."
        
        /// Upload an array to buffer if upload is enabled.
        static member uploadArray offset (array : 'a array) buffer vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.upload offset size arrayPin.NativeInt buffer vkg

        /// Create an allocated staging buffer.
        static member createStaging size vkg =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal true info vkg
            allocatedBuffer

        /// Create an allocated vertex buffer.
        static member createVertex uploadEnabled size vkg =

            // data can and must be transferred from a staging buffer if upload is not enabled
            let usage =
                if uploadEnabled
                then Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
                else Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT

            // create buffer
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info vkg
            allocatedBuffer

        /// Create an allocated index buffer.
        static member createIndex uploadEnabled size vkg =

            // data can and must be transferred from a staging buffer if upload is not enabled
            let usage =
                if uploadEnabled
                then Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT
                else Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT ||| Vulkan.VK_BUFFER_USAGE_TRANSFER_DST_BIT

            // create buffer
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- usage
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal uploadEnabled info vkg
            allocatedBuffer

        /// Create an allocated uniform buffer.
        static member createUniform size vkg =
            let mutable info = VkBufferCreateInfo ()
            info.size <- uint64 size
            info.usage <- Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
            info.sharingMode <- Vulkan.VK_SHARING_MODE_EXCLUSIVE
            let allocatedBuffer = AllocatedBuffer.createInternal true info vkg
            allocatedBuffer

        /// Create an allocated uniform buffer for a stride of 16.
        static member createUniformStrided16 length vkg =
            AllocatedBuffer.createUniform (length * 16) vkg
        
        /// Create an allocated staging buffer and stage the data.
        static member stageData size nint vkg =
            let buffer = AllocatedBuffer.createStaging size vkg
            AllocatedBuffer.upload 0 size nint buffer vkg
            buffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer.
        static member createVertexStaged size nint vkg =
            let stagingBuffer = AllocatedBuffer.stageData size nint vkg
            let vertexBuffer = AllocatedBuffer.createVertex false size vkg
            AllocatedBuffer.copyData size stagingBuffer vertexBuffer vkg
            AllocatedBuffer.destroy stagingBuffer vkg
            vertexBuffer

        /// Create an allocated index buffer with data uploaded via staging buffer.
        static member createIndexStaged size nint vkg =
            let stagingBuffer = AllocatedBuffer.stageData size nint vkg
            let indexBuffer = AllocatedBuffer.createIndex false size vkg
            AllocatedBuffer.copyData size stagingBuffer indexBuffer vkg
            AllocatedBuffer.destroy stagingBuffer vkg
            indexBuffer

        /// Create an allocated vertex buffer with data uploaded via staging buffer from an array.
        static member createVertexStagedFromArray (array : 'a array) vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.createVertexStaged size arrayPin.NativeInt vkg

        /// Create an allocated index buffer with data uploaded via staging buffer from an array.
        static member createIndexStagedFromArray (array : 'a array) vkg =
            let size = array.Length * sizeof<'a>
            use arrayPin = new ArrayPin<_> (array)
            AllocatedBuffer.createIndexStaged size arrayPin.NativeInt vkg
        
        /// Destroy buffer and allocation.
        static member destroy buffer vkg =
            Vma.vmaDestroyBuffer (vkg.VmaAllocator, buffer.Buffer, buffer.Allocation)

    /// Abstraction for vma allocated image.
    type AllocatedImage =
        { Image : VkImage
          Allocation : VmaAllocation }

        /// Destroy image and allocation.
        static member destroy allocatedImage (vkg : Hl.VulkanGlobal) =
            Vma.vmaDestroyImage (vkg.VmaAllocator, allocatedImage.Image, allocatedImage.Allocation)

        /// Create an AllocatedImage.
        static member create imageInfo (vkg : Hl.VulkanGlobal) =
            let info = VmaAllocationCreateInfo (usage = VmaMemoryUsage.Auto)
            let mutable image = Unchecked.defaultof<VkImage>
            let mutable allocation = Unchecked.defaultof<VmaAllocation>
            Vma.vmaCreateImage (vkg.VmaAllocator, &imageInfo, &info, &image, &allocation, nullPtr) |> Hl.check
            let allocatedImage = { Image = image; Allocation = allocation }
            allocatedImage

    /// A set of upload enabled allocated buffers for each frame in flight.
    type FifBuffer =
        private { AllocatedBuffers : AllocatedBuffer array
                  BufferSizes : int array
                  BufferUsage : VkBufferUsageFlags }

        /// The VkBuffer for the current frame.
        member this.Buffer = this.AllocatedBuffers.[Hl.CurrentFrame].Buffer

        /// The VkBuffer for each frame in flight.
        member this.PerFrameBuffers = Array.map (fun allocatedBuffer -> allocatedBuffer.Buffer) this.AllocatedBuffers

        /// Create an AllocatedBuffer based on usage.
        static member private createBuffer size usage vkg =
            match usage with
            | Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT -> AllocatedBuffer.createStaging size vkg
            | Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT -> AllocatedBuffer.createVertex true size vkg
            | Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT -> AllocatedBuffer.createIndex true size vkg
            | Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT -> AllocatedBuffer.createUniform size vkg
            | _ -> Log.fail "Invalid VkBufferUsageFlags value for FifBuffer."
        
        /// Create a FifBuffer.
        static member private createInternal size (usage : VkBufferUsageFlags) vkg =
            
            // create buffers and sizes
            let bufferSizes = Array.create Constants.Vulkan.MaxFramesInFlight size
            let allocatedBuffers = Array.zeroCreate<AllocatedBuffer> Constants.Vulkan.MaxFramesInFlight
            for i in 0 .. dec allocatedBuffers.Length do allocatedBuffers.[i] <- FifBuffer.createBuffer size usage vkg

            // make FifBuffer
            let fifBuffer =
                { AllocatedBuffers = allocatedBuffers 
                  BufferSizes = bufferSizes
                  BufferUsage = usage }

            // fin
            fifBuffer
        
        /// Check that the current buffer is at least as big as the given size, resizing if necessary. If used, must be called every frame.
        static member updateSize size fifBuffer vkg =
            if size > fifBuffer.BufferSizes.[Hl.CurrentFrame] then
                AllocatedBuffer.destroy fifBuffer.AllocatedBuffers.[Hl.CurrentFrame] vkg
                fifBuffer.AllocatedBuffers.[Hl.CurrentFrame] <- FifBuffer.createBuffer size fifBuffer.BufferUsage vkg
                fifBuffer.BufferSizes.[Hl.CurrentFrame] <- size

        /// Upload data to FifBuffer.
        static member upload offset size nint fifBuffer vkg =
            AllocatedBuffer.upload offset size nint fifBuffer.AllocatedBuffers.[Hl.CurrentFrame] vkg

        /// Upload data to FifBuffer with a stride of 16.
        static member uploadStrided16 offset typeSize count nint fifBuffer vkg =
            AllocatedBuffer.uploadStrided16 offset typeSize count nint fifBuffer.AllocatedBuffers.[Hl.CurrentFrame] vkg
        
        
        /// Upload an array to FifBuffer.
        static member uploadArray offset array fifBuffer vkg =
            AllocatedBuffer.uploadArray offset array fifBuffer.AllocatedBuffers.[Hl.CurrentFrame] vkg

        /// Create a staging FifBuffer.
        static member createStaging size vkg =
            FifBuffer.createInternal size Vulkan.VK_BUFFER_USAGE_TRANSFER_SRC_BIT vkg
        
        /// Create a vertex FifBuffer.
        static member createVertex size vkg =
            FifBuffer.createInternal size Vulkan.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT vkg

        /// Create an index FifBuffer.
        static member createIndex size vkg =
            FifBuffer.createInternal size Vulkan.VK_BUFFER_USAGE_INDEX_BUFFER_BIT vkg

        /// Create a uniform FifBuffer.
        static member createUniform size vkg =
            FifBuffer.createInternal size Vulkan.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT vkg

        /// Create a uniform FifBuffer for a stride of 16.
        static member createUniformStrided16 length vkg =
            FifBuffer.createUniform (length * 16) vkg

        /// Destroy FifBuffer.
        static member destroy fifBuffer vkg =
            for i in 0 .. dec fifBuffer.AllocatedBuffers.Length do AllocatedBuffer.destroy fifBuffer.AllocatedBuffers.[i] vkg