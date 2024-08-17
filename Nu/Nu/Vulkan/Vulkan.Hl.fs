namespace Vortice.Vulkan
open System
open System.Runtime.InteropServices
open FSharp.NativeInterop
open type Vma
open type Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// Assert based on Vulkan operation result.
    let Assert (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan error: " + string result)

    /// Abstraction for VkBuffer usage.
    type Buffer =
        { mutable Buffer : VkBuffer
          mutable Allocation : VmaAllocation }

        static member createCpuToGpu<'ub when 'ub : unmanaged> (usage : VkBufferUsageFlags, allocator : VmaAllocator) =

            let mutable bufferCreateInfo = VkBufferCreateInfo ()
            bufferCreateInfo.size <- uint64 sizeof<'ub>
            bufferCreateInfo.usage <- usage
            bufferCreateInfo.sharingMode <- VkSharingMode.Exclusive

            let mutable allocationCreateInfo = VmaAllocationCreateInfo ()
            allocationCreateInfo.usage <- VmaMemoryUsage.CpuToGpu

            let mutable buffer = VkBuffer ()
            let mutable allocation = VmaAllocation ()
            vmaCreateBuffer (allocator, Interop.AsPointer &bufferCreateInfo, Interop.AsPointer &allocationCreateInfo, Interop.AsPointer &buffer, Interop.AsPointer &allocation, NativePtr.nullPtr) |> Assert
            { Buffer = buffer; Allocation = allocation }

        static member write<'ub when 'ub : unmanaged> (values : 'ub inref, allocation : VmaAllocation, allocator : VmaAllocator) =
            let memoryPtrPtr = Unchecked.defaultof<nativeptr<voidptr>>
            vmaMapMemory (allocator, allocation, memoryPtrPtr) |> Assert
            Marshal.StructureToPtr<'ub> (values, NativePtr.toNativeInt memoryPtrPtr, false)
            vmaUnmapMemory (allocator, allocation)

    [<RequireQualifiedAccess>]
    module DescriptorSetLayoutBinding =

        let make (binding, descriptorType, descriptorCount, stages) =
            let mutable descriptorSetLayoutBinding = VkDescriptorSetLayoutBinding ()
            descriptorSetLayoutBinding.binding <- binding
            descriptorSetLayoutBinding.descriptorType <- descriptorType
            descriptorSetLayoutBinding.descriptorCount <- descriptorCount
            descriptorSetLayoutBinding.stageFlags <- stages
            descriptorSetLayoutBinding

    [<RequireQualifiedAccess>]
    module DescriptorSetLayout =

        let make (bindings, device) =
            use bindingsWrap = new ArrayPin<_> (bindings)
            let mutable descriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo ()
            let mutable descriptorSetLayout = VkDescriptorSetLayout ()
            descriptorSetLayoutCreateInfo.bindingCount <- uint bindings.Length
            descriptorSetLayoutCreateInfo.pBindings <- bindingsWrap.Pointer
            vkCreateDescriptorSetLayout (device, Interop.AsPointer &descriptorSetLayoutCreateInfo, NativePtr.nullPtr, Interop.AsPointer &descriptorSetLayout) |> Assert
            descriptorSetLayout

    /// Abstraction for VkDescriptorSet usage.
    type DescriptorSet =
        { mutable DescriptorSetLayout : VkDescriptorSetLayout
          mutable DescriptorSet : VkDescriptorSet }

        static member create (descriptorSetLayout : VkDescriptorSetLayout byref, descriptorPool, device) =

            let mutable descriptorSetAllocateInfo = VkDescriptorSetAllocateInfo ()
            descriptorSetAllocateInfo.descriptorPool <- descriptorPool
            descriptorSetAllocateInfo.descriptorSetCount <- 1u
            descriptorSetAllocateInfo.pSetLayouts <- Interop.AsPointer &descriptorSetLayout

            let mutable descriptorSet = VkDescriptorSet ()
            vkAllocateDescriptorSets (device, Interop.AsPointer &descriptorSetAllocateInfo, Interop.AsPointer &descriptorSet) |> Assert
            { DescriptorSet = descriptorSet; DescriptorSetLayout = descriptorSetLayout }

    [<RequireQualifiedAccess>]
    module PipelineDepthStencilStateCreateInfo =

        let makeLessThanUnstenciled () =
            let mutable pipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo ()
            pipelineDepthStencilStateCreateInfo.depthCompareOp <- VkCompareOp.Less
            pipelineDepthStencilStateCreateInfo

    [<RequireQualifiedAccess>]
    module PipelineColorBlendStateCreateInfo =

        let makeOverwrite () =
            let mutable pipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState ()
            pipelineColorBlendAttachmentState.colorWriteMask <- VkColorComponentFlags.R ||| VkColorComponentFlags.G ||| VkColorComponentFlags.B ||| VkColorComponentFlags.A
            pipelineColorBlendAttachmentState.blendEnable <- VkBool32.False
            pipelineColorBlendAttachmentState.srcColorBlendFactor <- VkBlendFactor.One
            pipelineColorBlendAttachmentState.dstColorBlendFactor <- VkBlendFactor.Zero
            pipelineColorBlendAttachmentState.colorBlendOp <- VkBlendOp.Add
            pipelineColorBlendAttachmentState.srcAlphaBlendFactor <- VkBlendFactor.One
            pipelineColorBlendAttachmentState.dstAlphaBlendFactor <- VkBlendFactor.Zero
            pipelineColorBlendAttachmentState.alphaBlendOp <- VkBlendOp.Add
            VkPipelineColorBlendStateCreateInfo pipelineColorBlendAttachmentState

        let makeAlpha () =
            let mutable pipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState ()
            pipelineColorBlendAttachmentState.colorWriteMask <- VkColorComponentFlags.R ||| VkColorComponentFlags.G ||| VkColorComponentFlags.B ||| VkColorComponentFlags.A
            pipelineColorBlendAttachmentState.blendEnable <- VkBool32.True
            pipelineColorBlendAttachmentState.srcColorBlendFactor <- VkBlendFactor.One
            pipelineColorBlendAttachmentState.dstColorBlendFactor <- VkBlendFactor.Zero
            pipelineColorBlendAttachmentState.colorBlendOp <- VkBlendOp.Add
            pipelineColorBlendAttachmentState.srcAlphaBlendFactor <- VkBlendFactor.One
            pipelineColorBlendAttachmentState.dstAlphaBlendFactor <- VkBlendFactor.Zero
            pipelineColorBlendAttachmentState.alphaBlendOp <- VkBlendOp.Add
            VkPipelineColorBlendStateCreateInfo pipelineColorBlendAttachmentState

    /// Abstraction for VkPipeline usage.
    type Pipeline =
        { mutable UniformBuffersVertex : VkBuffer array
          mutable UniformAllocationsVertex : VmaAllocation array
          mutable UniformBuffersFragment : VkBuffer array
          mutable UniformAllocationsFragment : VmaAllocation array
          mutable PipelineLayout : VkPipelineLayout
          mutable Pipeline : VkPipeline }

        /// Create a pipeline with vertex and fragment stages.
        static member createVertexFragment<'ubv, 'ubf when 'ubv : unmanaged and 'ubf : unmanaged>
            (viewport : VkViewport byref,
             scissor : VkRect2D byref,
             primitiveTopology : VkPrimitiveTopology,
             pipelineDepthStencilStateCreateInfo : VkPipelineDepthStencilStateCreateInfo byref,
             pipelineColorBlendStateCreateInfo : VkPipelineColorBlendStateCreateInfo byref,
             descriptorSet : DescriptorSet,
             allocator : VmaAllocator,
             device : VkDevice) =

            let mutable shaderModuleCreateInfoVertex = Unchecked.defaultof<VkShaderModuleCreateInfo> // TODO: P0: load code.
            let mutable shaderModuleVertex = VkShaderModule ()
            vkCreateShaderModule (device, Interop.AsPointer &shaderModuleCreateInfoVertex, NativePtr.nullPtr, Interop.AsPointer &shaderModuleVertex) |> Assert

            let mutable shaderModuleCreateInfoFragment = Unchecked.defaultof<VkShaderModuleCreateInfo> // TODO: P0: load code.
            let mutable shaderModuleFragment = VkShaderModule ()
            vkCreateShaderModule (device, Interop.AsPointer &shaderModuleCreateInfoFragment, NativePtr.nullPtr, Interop.AsPointer &shaderModuleFragment) |> Assert

            let mutable pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo ()
            pipelineLayoutCreateInfo.setLayoutCount <- 1u
            pipelineLayoutCreateInfo.pSetLayouts <- Interop.AsPointer &descriptorSet.DescriptorSetLayout

            let mutable uniformBuffersVertex = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
            let mutable uniformAllocationsVertex = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
            for i in 0 .. dec Constants.Render.FrameCount do
                let buffer = Buffer.createCpuToGpu<'ubv> (VkBufferUsageFlags.UniformBuffer, allocator)
                uniformBuffersVertex.[i] <- buffer.Buffer
                uniformAllocationsVertex.[i] <- buffer.Allocation

            let mutable uniformBuffersFragment = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
            let mutable uniformAllocationsFragment = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
            for i in 0 .. dec Constants.Render.FrameCount do
                let buffer = Buffer.createCpuToGpu<'ubv> (VkBufferUsageFlags.UniformBuffer, allocator)
                uniformBuffersFragment.[i] <- buffer.Buffer
                uniformAllocationsFragment.[i] <- buffer.Allocation

            let mutable pipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo ()
            pipelineDynamicStateCreateInfo.dynamicStateCount <- 0u
            pipelineDynamicStateCreateInfo.pDynamicStates <- NativePtr.nullPtr

            let mutable pipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo ()
            pipelineVertexInputStateCreateInfo.vertexBindingDescriptionCount <- 0u
            pipelineVertexInputStateCreateInfo.pVertexBindingDescriptions <- NativePtr.nullPtr
            pipelineVertexInputStateCreateInfo.vertexAttributeDescriptionCount <- 0u
            pipelineVertexInputStateCreateInfo.pVertexAttributeDescriptions <- NativePtr.nullPtr

            let mutable pipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo ()
            pipelineInputAssemblyStateCreateInfo.topology <- primitiveTopology
            pipelineInputAssemblyStateCreateInfo.primitiveRestartEnable <- VkBool32.False

            let mutable pipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo ()
            pipelineViewportStateCreateInfo.viewportCount <- 1u
            pipelineViewportStateCreateInfo.pViewports <- Interop.AsPointer &viewport
            pipelineViewportStateCreateInfo.scissorCount <- 1u
            pipelineViewportStateCreateInfo.pScissors <- Interop.AsPointer &scissor

            let mutable pipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo ()
            pipelineRasterizationStateCreateInfo.depthClampEnable <- VkBool32.False
            pipelineRasterizationStateCreateInfo.rasterizerDiscardEnable <- VkBool32.False
            pipelineRasterizationStateCreateInfo.polygonMode <- VkPolygonMode.Fill
            pipelineRasterizationStateCreateInfo.lineWidth <- 1.0f
            pipelineRasterizationStateCreateInfo.cullMode <- VkCullModeFlags.Back
            pipelineRasterizationStateCreateInfo.frontFace <- VkFrontFace.Clockwise
            pipelineRasterizationStateCreateInfo.depthBiasEnable <- VkBool32.False
            pipelineRasterizationStateCreateInfo.depthBiasConstantFactor <- 0.0f
            pipelineRasterizationStateCreateInfo.depthBiasClamp <- 0.0f
            pipelineRasterizationStateCreateInfo.depthBiasSlopeFactor <- 0.0f

            let mutable pipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo ()
            pipelineMultisampleStateCreateInfo.sampleShadingEnable <- VkBool32.False
            pipelineMultisampleStateCreateInfo.rasterizationSamples <- VkSampleCountFlags.Count1
            pipelineMultisampleStateCreateInfo.minSampleShading <- 1.0f
            pipelineMultisampleStateCreateInfo.pSampleMask <- NativePtr.nullPtr
            pipelineMultisampleStateCreateInfo.alphaToCoverageEnable <- VkBool32.False
            pipelineMultisampleStateCreateInfo.alphaToOneEnable <- VkBool32.False

            let mutable pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo ()
            pipelineLayoutCreateInfo.setLayoutCount <- 0u
            pipelineLayoutCreateInfo.pSetLayouts <- NativePtr.nullPtr
            pipelineLayoutCreateInfo.pushConstantRangeCount <- 0u
            pipelineLayoutCreateInfo.pPushConstantRanges <- NativePtr.nullPtr

            let mutable pipelineLayout = VkPipelineLayout ()
            vkCreatePipelineLayout (device, Interop.AsPointer &pipelineLayoutCreateInfo, NativePtr.nullPtr, &pipelineLayout) |> Assert

            let mutable pipelineShaderStageCreateInfoVertex = VkPipelineShaderStageCreateInfo ()
            use pipelineShaderStageCreateInfoVertexName = new VkString (nameof pipelineShaderStageCreateInfoVertex)
            pipelineShaderStageCreateInfoVertex.stage <- VkShaderStageFlags.Vertex
            pipelineShaderStageCreateInfoVertex.``module`` <- shaderModuleVertex
            pipelineShaderStageCreateInfoVertex.pName <- pipelineShaderStageCreateInfoVertexName

            let mutable pipelineShaderStageCreateInfoFragment = VkPipelineShaderStageCreateInfo ()
            use pipelineShaderStageCreateInfoFragmentName = new VkString (nameof pipelineShaderStageCreateInfoFragment)
            pipelineShaderStageCreateInfoFragment.stage <- VkShaderStageFlags.Fragment
            pipelineShaderStageCreateInfoFragment.``module`` <- shaderModuleFragment
            pipelineShaderStageCreateInfoFragment.pName <- pipelineShaderStageCreateInfoFragmentName

            let stagesArray = [|pipelineShaderStageCreateInfoVertex; pipelineShaderStageCreateInfoFragment|]
            use stagesArrayWrap = new ArrayPin<_> (stagesArray)
            
            let mutable graphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo ()
            graphicsPipelineCreateInfo.stageCount <- uint stagesArray.Length
            graphicsPipelineCreateInfo.pStages <- stagesArrayWrap.Pointer
            graphicsPipelineCreateInfo.pVertexInputState <- Interop.AsPointer &pipelineVertexInputStateCreateInfo
            graphicsPipelineCreateInfo.pInputAssemblyState <- Interop.AsPointer &pipelineInputAssemblyStateCreateInfo
            graphicsPipelineCreateInfo.pViewportState <- Interop.AsPointer &pipelineViewportStateCreateInfo
            graphicsPipelineCreateInfo.pRasterizationState <- Interop.AsPointer &pipelineRasterizationStateCreateInfo
            graphicsPipelineCreateInfo.pMultisampleState <- Interop.AsPointer &pipelineMultisampleStateCreateInfo
            graphicsPipelineCreateInfo.pDepthStencilState <- Interop.AsPointer &pipelineDepthStencilStateCreateInfo
            graphicsPipelineCreateInfo.pColorBlendState <- Interop.AsPointer &pipelineColorBlendStateCreateInfo
            graphicsPipelineCreateInfo.pDynamicState <- Interop.AsPointer &pipelineDynamicStateCreateInfo
            graphicsPipelineCreateInfo.layout <- pipelineLayout
            graphicsPipelineCreateInfo.renderPass <- Unchecked.defaultof<_> // TODO: P0: figure out what's needed here.
            graphicsPipelineCreateInfo.subpass <- 0u

            let mutable pipeline = VkPipeline ()
            vkCreateGraphicsPipeline (device, graphicsPipelineCreateInfo, &pipeline) |> Assert
            { UniformBuffersVertex = uniformBuffersVertex
              UniformAllocationsVertex = uniformAllocationsVertex
              UniformBuffersFragment = uniformBuffersFragment
              UniformAllocationsFragment = uniformAllocationsFragment
              PipelineLayout = pipelineLayout
              Pipeline = pipeline }