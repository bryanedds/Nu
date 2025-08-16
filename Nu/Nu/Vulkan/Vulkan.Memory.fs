// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module VulkanMemory =

    type private BufferType =
        | Staging of bool // in frame
        | Vertex of bool // upload enabled
        | Index of bool // upload enabled
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
    let private copyData size source destination (vkc : Hl.VulkanContext) =
        let cb = Hl.beginTransientCommandBlock vkc.TransientCommandPool vkc.Device
        let mutable region = VkBufferCopy (size = uint64 size)
        Vulkan.vkCmdCopyBuffer (cb, source, destination, 1u, asPointer &region)
        Hl.endTransientCommandBlock cb vkc.GraphicsQueue vkc.TransientCommandPool vkc.ResourceReadyFence vkc.Device
    
    /// A manually allocated buffer for diagnostic purposes.
    /// TODO: DJL: adapt to simplified buffer api.
    type ManualBuffer =
        { VkBuffer : VkBuffer 
          Memory : VkDeviceMemory
          Mapping : voidptr }

        static member private findMemoryType typeFilter properties physicalDevice =
            
            // get memory types
            let mutable memProperties = Unchecked.defaultof<VkPhysicalDeviceMemoryProperties>
            Vulkan.vkGetPhysicalDeviceMemoryProperties (physicalDevice, &memProperties)
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
            let mapping = NativePtr.read mappingPtr
            
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
            let info = BufferType.makeInfo size (Staging false)
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
    type private BufferInternal =
        private
            { _VkBuffer : VkBuffer
              _Allocation : VmaAllocation
              _Mapping : voidptr
              _UploadEnabled : bool }

        /// The VkBuffer.
        member this.VkBuffer = this._VkBuffer
        
        /// Create BufferInternal.
        static member create uploadEnabled bufferInfo (vkc : Hl.VulkanContext) =

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
                { _VkBuffer = vkBuffer
                  _Allocation = allocation
                  _Mapping = allocationInfo.pMappedData
                  _UploadEnabled = uploadEnabled }

            // fin
            bufferInternal

        /// Upload data to buffer if upload is enabled, including manual flush.
        static member upload offset size data bufferInternal (vkc : Hl.VulkanContext) =
            upload bufferInternal._UploadEnabled offset size data bufferInternal._Mapping
            Vma.vmaFlushAllocation (vkc.VmaAllocator, bufferInternal._Allocation, uint64 offset, uint64 size) |> Hl.check // may be necessary as memory may not be host-coherent

        /// Upload data to buffer with a stride of 16 if upload is enabled, including manual flush.
        static member uploadStrided16 offset typeSize count data bufferInternal (vkc : Hl.VulkanContext) =
            uploadStrided16 bufferInternal._UploadEnabled offset typeSize count data bufferInternal._Mapping
            Vma.vmaFlushAllocation (vkc.VmaAllocator, bufferInternal._Allocation, uint64 (offset * 16), uint64 (count * 16)) |> Hl.check // may be necessary as memory may not be host-coherent
        
        /// Destroy buffer and allocation.
        static member destroy (bufferInternal : BufferInternal) (vkc : Hl.VulkanContext) =
            Vma.vmaDestroyBuffer (vkc.VmaAllocator, bufferInternal.VkBuffer, bufferInternal._Allocation)

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
        static member private createInternal size (bufferType : BufferType) vkc =
            
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

        /// Create a staging Buffer.
        static member createStaging size vkc =
            Buffer.createInternal size (Staging false) vkc
        
        /// Create a staging Buffer for use in frame.
        static member createStagingInFrame size vkc =
            Buffer.createInternal size (Staging true) vkc
        
        /// Create an uploadable vertex Buffer.
        static member createVertex size vkc =
            Buffer.createInternal size (Vertex true) vkc

        /// Create an uploadable index Buffer.
        static member createIndex size vkc =
            Buffer.createInternal size (Index true) vkc

        /// Create a uniform Buffer.
        static member createUniform size vkc =
            Buffer.createInternal size Uniform vkc

        /// Create a uniform Buffer for a stride of 16.
        static member createUniformStrided16 length vkc =
            Buffer.createUniform (length * 16) vkc

        /// Create a staging buffer and stage the data.
        static member stageData size data vkc =
            let buffer = Buffer.createStaging size vkc
            Buffer.upload 0 size data buffer vkc
            buffer

        /// Create a vertex buffer with data uploaded via staging buffer.
        static member createVertexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let vertexBuffer = Buffer.createInternal size (Vertex false) vkc
            copyData size stagingBuffer.VkBuffer vertexBuffer.VkBuffer vkc
            Buffer.destroy stagingBuffer vkc
            vertexBuffer

        /// Create an index buffer with data uploaded via staging buffer.
        static member createIndexStaged size data vkc =
            let stagingBuffer = Buffer.stageData size data vkc
            let indexBuffer = Buffer.createInternal size (Index false) vkc
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
        
        /// Destroy Buffer.
        static member destroy buffer vkc =
            for i in 0 .. dec buffer.BufferInternals.Length do BufferInternal.destroy buffer.BufferInternals.[i] vkc