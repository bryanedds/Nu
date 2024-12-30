// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open Vortice.ShaderCompiler
open Prime
open Nu

[<RequireQualifiedAccess>]
module Pipeline =

    /// An abstraction of a rendering pipeline.
    type Pipeline =
        { Pipeline : VkPipeline
          DescriptorPool : VkDescriptorPool
          DescriptorSet : VkDescriptorSet
          PipelineLayout : VkPipelineLayout
          DescriptorSetLayout : VkDescriptorSetLayout
          mutable VertexBuffer : Hl.AllocatedBuffer
          mutable IndexBuffer : Hl.AllocatedBuffer }

        /// Create the descriptor set layout.
        static member private createDescriptorSetLayout resourceBindings device =
            let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
            use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
            let mutable info = VkDescriptorSetLayoutCreateInfo ()
            info.bindingCount <- uint resourceBindings.Length
            info.pBindings <- resourceBindingsPin.Pointer
            Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
            descriptorSetLayout

        /// Create the pipeline layout.
        static member private createPipelineLayout descriptorSetLayout pushConstantRanges device =
            let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
            let mutable descriptorSetLayout = descriptorSetLayout
            use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
            let mutable info = VkPipelineLayoutCreateInfo ()
            info.setLayoutCount <- 1u
            info.pSetLayouts <- asPointer &descriptorSetLayout
            info.pushConstantRangeCount <- uint pushConstantRanges.Length
            info.pPushConstantRanges <- pushConstantRangesPin.Pointer
            Vulkan.vkCreatePipelineLayout (device, &info, nullPtr, &pipelineLayout) |> Hl.check
            pipelineLayout
        
        /// Create the descriptor pool.
        static member private createDescriptorPool (resourceBindings : VkDescriptorSetLayoutBinding array) device =
            
            // handle
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            
            // derive pool sizes from layout bindings
            let poolSizes = Array.zeroCreate<VkDescriptorPoolSize> resourceBindings.Length
            use poolSizesPin = new ArrayPin<_> (poolSizes)
            for i in [0 .. dec resourceBindings.Length] do
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- resourceBindings.[i].descriptorType
                poolSize.descriptorCount <- resourceBindings.[i].descriptorCount
                poolSizes.[i] <- poolSize
            
            // create descriptor pool
            let mutable info = VkDescriptorPoolCreateInfo ()
            info.maxSets <- 1u
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
            descriptorPool

        /// Create the descriptor set.
        static member private createDescriptorSet descriptorSetLayout descriptorPool device =
            let mutable descriptorSet = Unchecked.defaultof<VkDescriptorSet>
            let mutable descriptorSetLayout = descriptorSetLayout
            let mutable info = VkDescriptorSetAllocateInfo ()
            info.descriptorPool <- descriptorPool
            info.descriptorSetCount <- 1u
            info.pSetLayouts <- asPointer &descriptorSetLayout
            Vulkan.vkAllocateDescriptorSets (device, asPointer &info, asPointer &descriptorSet) |> Hl.check
            descriptorSet

        /// Create the Vulkan pipeline itself.
        static member private createPipeline shaderPath vertexBindings vertexAttributes pipelineLayout renderPass device =
            
            // handle
            let mutable pipeline = Unchecked.defaultof<VkPipeline>
        
            // create shader modules
            let vertModule = Hl.createShaderModuleFromGlsl (shaderPath + ".vert") ShaderKind.VertexShader device
            let fragModule = Hl.createShaderModuleFromGlsl (shaderPath + ".frag") ShaderKind.FragmentShader device

            // shader stage infos
            use entryPoint = new StringWrap ("main")
            let ssInfos = Array.zeroCreate<VkPipelineShaderStageCreateInfo> 2
            ssInfos[0] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[0].stage <- Vulkan.VK_SHADER_STAGE_VERTEX_BIT
            ssInfos[0].``module`` <- vertModule
            ssInfos[0].pName <- entryPoint.Pointer
            ssInfos[1] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[1].stage <- Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
            ssInfos[1].``module`` <- fragModule
            ssInfos[1].pName <- entryPoint.Pointer
            use ssInfosPin = new ArrayPin<_> (ssInfos)

            // vertex input info
            use vertexBindingsPin = new ArrayPin<_> (vertexBindings)
            use vertexAttributesPin = new ArrayPin<_> (vertexAttributes)
            let mutable viInfo = VkPipelineVertexInputStateCreateInfo ()
            viInfo.vertexBindingDescriptionCount <- uint vertexBindings.Length
            viInfo.pVertexBindingDescriptions <- vertexBindingsPin.Pointer
            viInfo.vertexAttributeDescriptionCount <- uint vertexAttributes.Length
            viInfo.pVertexAttributeDescriptions <- vertexAttributesPin.Pointer

            // input assembly info
            let mutable iaInfo = VkPipelineInputAssemblyStateCreateInfo (topology = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

            // viewport info
            let mutable vInfo = VkPipelineViewportStateCreateInfo ()
            vInfo.viewportCount <- 1u
            vInfo.scissorCount <- 1u

            // rasterization info
            let mutable rInfo = VkPipelineRasterizationStateCreateInfo ()
            rInfo.polygonMode <- Vulkan.VK_POLYGON_MODE_FILL
            rInfo.cullMode <- Vulkan.VK_CULL_MODE_NONE
            rInfo.frontFace <- Vulkan.VK_FRONT_FACE_COUNTER_CLOCKWISE
            rInfo.lineWidth <- 1.0f

            // multisample info
            let mutable mInfo = VkPipelineMultisampleStateCreateInfo (rasterizationSamples = Vulkan.VK_SAMPLE_COUNT_1_BIT)

            // color attachment
            let mutable color = VkPipelineColorBlendAttachmentState ()
            color.blendEnable <- true
            color.srcColorBlendFactor <- Vulkan.VK_BLEND_FACTOR_SRC_ALPHA
            color.dstColorBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
            color.colorBlendOp <- Vulkan.VK_BLEND_OP_ADD
            color.srcAlphaBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE
            color.dstAlphaBlendFactor <- Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
            color.alphaBlendOp <- Vulkan.VK_BLEND_OP_ADD
            color.colorWriteMask <- Vulkan.VK_COLOR_COMPONENT_R_BIT ||| Vulkan.VK_COLOR_COMPONENT_G_BIT ||| Vulkan.VK_COLOR_COMPONENT_B_BIT ||| Vulkan.VK_COLOR_COMPONENT_A_BIT

            // depth and blend info
            let mutable dInfo = VkPipelineDepthStencilStateCreateInfo ()
            let mutable bInfo = VkPipelineColorBlendStateCreateInfo ()
            bInfo.attachmentCount <- 1u
            bInfo.pAttachments <- asPointer &color

            // dynamic state info
            let dynamicStates = [|Vulkan.VK_DYNAMIC_STATE_VIEWPORT; Vulkan.VK_DYNAMIC_STATE_SCISSOR|]
            use dynamicStatesPin = new ArrayPin<_> (dynamicStates)
            let mutable dsInfo = VkPipelineDynamicStateCreateInfo ()
            dsInfo.dynamicStateCount <- uint dynamicStates.Length
            dsInfo.pDynamicStates <- dynamicStatesPin.Pointer

            // create pipeline
            let mutable info = VkGraphicsPipelineCreateInfo ()
            info.stageCount <- uint ssInfos.Length
            info.pStages <- ssInfosPin.Pointer
            info.pVertexInputState <- asPointer &viInfo
            info.pInputAssemblyState <- asPointer &iaInfo
            info.pViewportState <- asPointer &vInfo
            info.pRasterizationState <- asPointer &rInfo
            info.pMultisampleState <- asPointer &mInfo
            info.pDepthStencilState <- asPointer &dInfo
            info.pColorBlendState <- asPointer &bInfo
            info.pDynamicState <- asPointer &dsInfo
            info.layout <- pipelineLayout
            info.renderPass <- renderPass
            info.subpass <- 0u
            Vulkan.vkCreateGraphicsPipelines (device, VkPipelineCache.Null, 1u, &info, nullPtr, asPointer &pipeline) |> Hl.check

            // destroy shader modules
            Vulkan.vkDestroyShaderModule (device, vertModule, nullPtr)
            Vulkan.vkDestroyShaderModule (device, fragModule, nullPtr)

            // fin
            pipeline
        
        /// Destroy Pipeline.
        static member destroy pipeline (vulkanGlobal : Hl.VulkanGlobal) =
            Hl.AllocatedBuffer.destroy pipeline.VertexBuffer vulkanGlobal.VmaAllocator
            Hl.AllocatedBuffer.destroy pipeline.IndexBuffer vulkanGlobal.VmaAllocator
            Vulkan.vkDestroyPipeline (vulkanGlobal.Device, pipeline.Pipeline, nullPtr)
            Vulkan.vkDestroyDescriptorPool (vulkanGlobal.Device, pipeline.DescriptorPool, nullPtr)
            Vulkan.vkDestroyPipelineLayout (vulkanGlobal.Device, pipeline.PipelineLayout, nullPtr)
            Vulkan.vkDestroyDescriptorSetLayout (vulkanGlobal.Device, pipeline.DescriptorSetLayout, nullPtr)
        
        /// Create a Pipeline.
        static member create shaderPath vertexBindings vertexAttributes resourceBindings pushConstantRanges (vulkanGlobal : Hl.VulkanGlobal) =
            
            // create everything
            let descriptorSetLayout = Pipeline.createDescriptorSetLayout resourceBindings vulkanGlobal.Device
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayout pushConstantRanges vulkanGlobal.Device
            let descriptorPool = Pipeline.createDescriptorPool resourceBindings vulkanGlobal.Device
            let descriptorSet = Pipeline.createDescriptorSet descriptorSetLayout descriptorPool vulkanGlobal.Device
            let vulkanPipeline = Pipeline.createPipeline shaderPath vertexBindings vertexAttributes pipelineLayout vulkanGlobal.RenderPass vulkanGlobal.Device
            let vertexBuffer = Hl.AllocatedBuffer.createVertex true 0 vulkanGlobal.VmaAllocator
            let indexBuffer = Hl.AllocatedBuffer.createIndex true 0 vulkanGlobal.VmaAllocator

            // make Pipeline
            let pipeline =
                { Pipeline = vulkanPipeline
                  DescriptorPool = descriptorPool
                  DescriptorSet = descriptorSet
                  PipelineLayout = pipelineLayout
                  DescriptorSetLayout = descriptorSetLayout
                  VertexBuffer = vertexBuffer
                  IndexBuffer = indexBuffer }

            // fin
            pipeline
