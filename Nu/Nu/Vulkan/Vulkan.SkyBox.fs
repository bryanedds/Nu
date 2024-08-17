namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open type Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    [<StructLayout (LayoutKind.Sequential)>] 
    type SkyBoxUniformBufferVertex =
        struct
            val mutable View : Matrix4x4
            val mutable Projection : Matrix4x4
            end

    [<StructLayout (LayoutKind.Sequential)>] 
    type SkyBoxUniformBufferFragment =
        struct
            val mutable Color : Vector3
            val mutable Brightness : single
            val mutable SamplerCube : uint
            end

    /// Describes a skybox pipeline that's loaded into GPU.
    type SkyBoxPipeline =
        { UniformBuffersVertex : VkBuffer array
          UniformAllocationsVertex : VmaAllocation array
          UniformBuffersFragment : VkBuffer array
          UniformAllocationsFragment : VmaAllocation array
          DescriptorSetLayout : VkDescriptorSetLayout
          DescriptorSet : VkDescriptorSet
          PipelineLayout : VkPipelineLayout
          Pipeline : VkPipeline }

    /// Create a skybox shader pipeline.
    let CreateSkyBoxPipeline (shaderFilePath : string) (viewport : VkViewport byref) (scissor : VkRect2D byref) (allocator : VmaAllocator byref) (device : VkDevice byref) =

        let mutable vertShaderCode = Unchecked.defaultof<VkShaderModuleCreateInfo> // TODO: P0: load code.
        let mutable fragShaderCode = Unchecked.defaultof<VkShaderModuleCreateInfo> // TODO: P0: load code.

        let mutable shaderModuleVertex = VkShaderModule ()
        let result = vkCreateShaderModule (device, Interop.AsPointer &vertShaderCode, NativePtr.nullPtr, Interop.AsPointer &shaderModuleVertex)
        let mutable shaderModuleFragment = VkShaderModule ()
        let result = vkCreateShaderModule (device, Interop.AsPointer &fragShaderCode, NativePtr.nullPtr, Interop.AsPointer &shaderModuleFragment)

        let mutable bindingVertex = VkDescriptorSetLayoutBinding ()
        bindingVertex.binding <- 0u
        bindingVertex.descriptorType <- VkDescriptorType.UniformBuffer
        bindingVertex.descriptorCount <- 1u
        bindingVertex.stageFlags <- VkShaderStageFlags.Vertex

        let mutable bindingFragment = VkDescriptorSetLayoutBinding ()
        bindingFragment.binding <- 1u
        bindingFragment.descriptorType <- VkDescriptorType.CombinedImageSampler
        bindingFragment.descriptorCount <- 1u
        bindingFragment.stageFlags <- VkShaderStageFlags.Fragment

        let bindings = [|bindingVertex; bindingFragment|]
        let bindingsWrap = new ArrayPin<_> (bindings)

        let mutable descriptorSetLayout = VkDescriptorSetLayout ()
        let mutable descriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo ()
        descriptorSetLayoutCreateInfo.bindingCount <- uint bindings.Length
        descriptorSetLayoutCreateInfo.pBindings <- bindingsWrap.Pointer
        let result = vkCreateDescriptorSetLayout (device, Interop.AsPointer &descriptorSetLayoutCreateInfo, NativePtr.nullPtr, Interop.AsPointer &descriptorSetLayout)

        let mutable pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo ()
        pipelineLayoutCreateInfo.setLayoutCount <- 1u
        pipelineLayoutCreateInfo.pSetLayouts <- Interop.AsPointer &descriptorSetLayout

        let mutable uniformBuffersVertex = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
        let mutable uniformAllocationsVertex = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
        for i in 0 .. dec Constants.Render.FrameCount do
            let result = hlCreateBuffer<SkyBoxUniformBufferVertex> (&allocator, VkBufferUsageFlags.UniformBuffer, &uniformBuffersVertex.[i], &uniformAllocationsVertex.[i])
            ()

        let mutable uniformBuffersFragment = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
        let mutable uniformAllocationsFragment = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
        for i in 0 .. dec Constants.Render.FrameCount do
            let result = hlCreateBuffer<SkyBoxUniformBufferFragment> (&allocator, VkBufferUsageFlags.UniformBuffer, &uniformBuffersFragment.[i], &uniformAllocationsFragment.[i])
            ()

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

        let mutable pipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo ()
        pipelineDynamicStateCreateInfo.dynamicStateCount <- 0u
        pipelineDynamicStateCreateInfo.pDynamicStates <- NativePtr.nullPtr

        let mutable pipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo ()
        pipelineVertexInputStateCreateInfo.vertexBindingDescriptionCount <- 0u
        pipelineVertexInputStateCreateInfo.pVertexBindingDescriptions <- NativePtr.nullPtr
        pipelineVertexInputStateCreateInfo.vertexAttributeDescriptionCount <- 0u
        pipelineVertexInputStateCreateInfo.pVertexAttributeDescriptions <- NativePtr.nullPtr

        let mutable pipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo ()
        pipelineInputAssemblyStateCreateInfo.topology <- VkPrimitiveTopology.TriangleList
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

        let mutable pipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState ()
        pipelineColorBlendAttachmentState.colorWriteMask <- VkColorComponentFlags.R ||| VkColorComponentFlags.G ||| VkColorComponentFlags.B ||| VkColorComponentFlags.A
        pipelineColorBlendAttachmentState.blendEnable <- VkBool32.False
        pipelineColorBlendAttachmentState.srcColorBlendFactor <- VkBlendFactor.One
        pipelineColorBlendAttachmentState.dstColorBlendFactor <- VkBlendFactor.Zero
        pipelineColorBlendAttachmentState.colorBlendOp <- VkBlendOp.Add
        pipelineColorBlendAttachmentState.srcAlphaBlendFactor <- VkBlendFactor.One
        pipelineColorBlendAttachmentState.dstAlphaBlendFactor <- VkBlendFactor.Zero
        pipelineColorBlendAttachmentState.alphaBlendOp <- VkBlendOp.Add

        let mutable pipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo pipelineColorBlendAttachmentState

        let mutable pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo ()
        pipelineLayoutCreateInfo.setLayoutCount <- 0u
        pipelineLayoutCreateInfo.pSetLayouts <- NativePtr.nullPtr
        pipelineLayoutCreateInfo.pushConstantRangeCount <- 0u
        pipelineLayoutCreateInfo.pPushConstantRanges <- NativePtr.nullPtr

        let mutable pipelineLayout = VkPipelineLayout ()
        let result = vkCreatePipelineLayout (device, Interop.AsPointer &pipelineLayoutCreateInfo, NativePtr.nullPtr, &pipelineLayout)

        let mutable graphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo ()
        graphicsPipelineCreateInfo.stageCount <- uint stagesArray.Length
        graphicsPipelineCreateInfo.pStages <- stagesArrayWrap.Pointer
        graphicsPipelineCreateInfo.pVertexInputState <- Interop.AsPointer &pipelineVertexInputStateCreateInfo
        graphicsPipelineCreateInfo.pInputAssemblyState <- Interop.AsPointer &pipelineInputAssemblyStateCreateInfo
        graphicsPipelineCreateInfo.pViewportState <- Interop.AsPointer &pipelineViewportStateCreateInfo
        graphicsPipelineCreateInfo.pRasterizationState <- Interop.AsPointer &pipelineRasterizationStateCreateInfo
        graphicsPipelineCreateInfo.pMultisampleState <- Interop.AsPointer &pipelineMultisampleStateCreateInfo
        graphicsPipelineCreateInfo.pDepthStencilState <- NativePtr.nullPtr
        graphicsPipelineCreateInfo.pColorBlendState <- Interop.AsPointer &pipelineColorBlendStateCreateInfo
        graphicsPipelineCreateInfo.pDynamicState <- Interop.AsPointer &pipelineDynamicStateCreateInfo
        graphicsPipelineCreateInfo.layout <- pipelineLayout
        graphicsPipelineCreateInfo.renderPass <- Unchecked.defaultof<_> // TODO: P0: figure out what's needed here.
        graphicsPipelineCreateInfo.subpass <- 0u

        let mutable pipeline = VkPipeline ()
        let result = vkCreateGraphicsPipeline (device, graphicsPipelineCreateInfo, &pipeline)

        { UniformBuffersVertex = uniformBuffersVertex
          UniformAllocationsVertex = uniformAllocationsVertex
          UniformBuffersFragment = uniformBuffersFragment
          UniformAllocationsFragment = uniformAllocationsFragment
          DescriptorSetLayout = descriptorSetLayout
          DescriptorSet = Unchecked.defaultof<_> // TODO: P0: figure out what, if anything, is needed here.
          PipelineLayout = pipelineLayout
          Pipeline = pipeline }

    /// Draw a skybox.
    let DrawSkyBox
        (frame : int,
         commandBuffer : VkCommandBuffer,
         skyBoxPipeline : SkyBoxPipeline,
         geometry : CubeMap.CubeMapGeometry,
         view : Matrix4x4,
         projection : Matrix4x4,
         color : Vector3,
         brightness : single,
         cubeMap : VkImageView,
         allocator : VmaAllocator byref) =

        // Bind the pipeline
        vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, skyBoxPipeline.Pipeline)

        // Bind the descriptor set
        vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, skyBoxPipeline.PipelineLayout, 0u, ReadOnlySpan [|skyBoxPipeline.DescriptorSet|], ReadOnlySpan [||])

        let mutable uniformValuesVertex = SkyBoxUniformBufferVertex ()
        uniformValuesVertex.View <- view
        uniformValuesVertex.Projection <- projection
        let result = hlWriteBuffer (&allocator, &skyBoxPipeline.UniformAllocationsVertex.[frame], &uniformValuesVertex)

        let mutable uniformValuesFragment = SkyBoxUniformBufferFragment ()
        uniformValuesFragment.Color <- color
        uniformValuesFragment.Brightness <- brightness
        uniformValuesFragment.SamplerCube <- Unchecked.defaultof<_> // TODO: P0: figure out what's needed here.
        let result = hlWriteBuffer (&allocator, &skyBoxPipeline.UniformAllocationsFragment.[frame], &uniformValuesFragment)

        // Bind the vertex and index buffers
        vkCmdBindVertexBuffer (commandBuffer, 0u, geometry.VertexBuffer, 0UL)
        vkCmdBindIndexBuffer (commandBuffer, geometry.IndexBuffer, 0UL, VkIndexType.Uint32)

        // Draw the geometry
        vkCmdDrawIndexed (commandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)