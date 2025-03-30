// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open Vortice.ShaderCompiler
open Prime
open Nu

[<RequireQualifiedAccess>]
module Pipeline =

    /// A blend setting for a Vulkan pipeline.
    /// TODO: DJL: review naming.
    type Blend =
        | Transparent
        | Additive
        | Overwrite
        | ImGui

        /// Make blend attachment.
        static member makeAttachment blend =
            match blend with
            | Transparent ->
                Hl.makeBlendAttachment
                    Vulkan.VK_BLEND_FACTOR_SRC_ALPHA Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
                    Vulkan.VK_BLEND_FACTOR_ONE Vulkan.VK_BLEND_FACTOR_ZERO
            | Additive ->
                Hl.makeBlendAttachment
                    Vulkan.VK_BLEND_FACTOR_SRC_ALPHA Vulkan.VK_BLEND_FACTOR_ONE
                    Vulkan.VK_BLEND_FACTOR_ONE Vulkan.VK_BLEND_FACTOR_ZERO
            | Overwrite ->
                Hl.makeBlendAttachment
                    Vulkan.VK_BLEND_FACTOR_ONE Vulkan.VK_BLEND_FACTOR_ZERO
                    Vulkan.VK_BLEND_FACTOR_ONE Vulkan.VK_BLEND_FACTOR_ZERO
            | ImGui ->
                Hl.makeBlendAttachment
                    Vulkan.VK_BLEND_FACTOR_SRC_ALPHA Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
                    Vulkan.VK_BLEND_FACTOR_ONE Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA

    /// An abstraction of a rendering pipeline.
    type Pipeline =
        private
            { _VkPipelines : Map<Blend, VkPipeline>
              _DescriptorPool : VkDescriptorPool
              _DescriptorSets : VkDescriptorSet array
              _PipelineLayout : VkPipelineLayout
              _DescriptorSetLayout : VkDescriptorSetLayout }

        /// The pipeline layout.
        member this.PipelineLayout = this._PipelineLayout
        
        /// The descriptor set for the current frame.
        member this.DescriptorSet = this._DescriptorSets.[Hl.CurrentFrame]
        
        /// Create the descriptor set layout.
        static member private createDescriptorSetLayout resourceBindings device =
            use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
            let mutable info = VkDescriptorSetLayoutCreateInfo ()
            info.bindingCount <- uint resourceBindings.Length
            info.pBindings <- resourceBindingsPin.Pointer
            let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
            Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
            descriptorSetLayout

        /// Create the pipeline layout.
        static member private createPipelineLayout descriptorSetLayout pushConstantRanges device =
            let mutable descriptorSetLayout = descriptorSetLayout
            use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
            let mutable info = VkPipelineLayoutCreateInfo ()
            info.setLayoutCount <- 1u
            info.pSetLayouts <- asPointer &descriptorSetLayout
            info.pushConstantRangeCount <- uint pushConstantRanges.Length
            info.pPushConstantRanges <- pushConstantRangesPin.Pointer
            let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
            Vulkan.vkCreatePipelineLayout (device, &info, nullPtr, &pipelineLayout) |> Hl.check
            pipelineLayout
        
        /// Create the descriptor pool.
        static member private createDescriptorPool (resourceBindings : VkDescriptorSetLayoutBinding array) device =
            
            // derive pool sizes from layout bindings
            let poolSizes = Array.zeroCreate<VkDescriptorPoolSize> resourceBindings.Length
            use poolSizesPin = new ArrayPin<_> (poolSizes)
            for i in [0 .. dec resourceBindings.Length] do
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- resourceBindings.[i].descriptorType
                poolSize.descriptorCount <- resourceBindings.[i].descriptorCount * (uint Constants.Vulkan.MaxFramesInFlight)
                poolSizes.[i] <- poolSize
            
            // create descriptor pool
            let mutable info = VkDescriptorPoolCreateInfo ()
            info.maxSets <- uint Constants.Vulkan.MaxFramesInFlight
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
            descriptorPool

        /// Create the descriptor set.
        static member private createDescriptorSets descriptorSetLayout descriptorPool device =
            let descriptorSetLayouts = Array.zeroCreate<VkDescriptorSetLayout> Constants.Vulkan.MaxFramesInFlight
            use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
            for i in 0 .. dec descriptorSetLayouts.Length do descriptorSetLayouts.[i] <- descriptorSetLayout
            let mutable info = VkDescriptorSetAllocateInfo ()
            info.descriptorPool <- descriptorPool
            info.descriptorSetCount <- uint descriptorSetLayouts.Length
            info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
            let descriptorSets = Array.zeroCreate<VkDescriptorSet> Constants.Vulkan.MaxFramesInFlight
            use descriptorSetsPin = new ArrayPin<_> (descriptorSets)
            Vulkan.vkAllocateDescriptorSets (device, asPointer &info, descriptorSetsPin.Pointer) |> Hl.check
            descriptorSets

        /// Create the Vulkan pipelines themselves.
        static member private createVkPipelines shaderPath cullFace (blends : Blend array) vertexBindings vertexAttributes pipelineLayout renderPass device =
            
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

            // viewport info
            let mutable vInfo = VkPipelineViewportStateCreateInfo ()
            vInfo.viewportCount <- 1u
            vInfo.scissorCount <- 1u

            // rasterization info
            let mutable rInfo = VkPipelineRasterizationStateCreateInfo ()
            rInfo.polygonMode <- Vulkan.VK_POLYGON_MODE_FILL
            rInfo.cullMode <- if cullFace then Vulkan.VK_CULL_MODE_BACK_BIT else Vulkan.VK_CULL_MODE_NONE
            rInfo.frontFace <- Vulkan.VK_FRONT_FACE_COUNTER_CLOCKWISE
            rInfo.lineWidth <- 1.0f

            // input assembly; multisample; depth-stencil
            let mutable iaInfo = VkPipelineInputAssemblyStateCreateInfo (topology = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
            let mutable mInfo = VkPipelineMultisampleStateCreateInfo (rasterizationSamples = Vulkan.VK_SAMPLE_COUNT_1_BIT)
            let mutable dInfo = VkPipelineDepthStencilStateCreateInfo ()

            // dynamic state info
            let dynamicStates = [|Vulkan.VK_DYNAMIC_STATE_VIEWPORT; Vulkan.VK_DYNAMIC_STATE_SCISSOR|]
            use dynamicStatesPin = new ArrayPin<_> (dynamicStates)
            let mutable dsInfo = VkPipelineDynamicStateCreateInfo ()
            dsInfo.dynamicStateCount <- uint dynamicStates.Length
            dsInfo.pDynamicStates <- dynamicStatesPin.Pointer

            // create vulkan pipelines
            let vkPipelines = Array.zeroCreate<VkPipeline> blends.Length
            for i in 0 .. dec vkPipelines.Length do
            
                // blend info
                let mutable blend = Blend.makeAttachment blends.[i]
                let mutable bInfo = VkPipelineColorBlendStateCreateInfo ()
                bInfo.attachmentCount <- 1u
                bInfo.pAttachments <- asPointer &blend

                // create vulkan pipeline
                // TODO: DJL: create vulkan pipelines with single call outside loop.
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
                let mutable vkPipeline = Unchecked.defaultof<VkPipeline>
                Vulkan.vkCreateGraphicsPipelines (device, VkPipelineCache.Null, 1u, &info, nullPtr, asPointer &vkPipeline) |> Hl.check
                vkPipelines.[i] <- vkPipeline
            
            // destroy shader modules
            Vulkan.vkDestroyShaderModule (device, vertModule, nullPtr)
            Vulkan.vkDestroyShaderModule (device, fragModule, nullPtr)
            
            // pack vulkan pipelines with blend parameters
            let vkPipelinesPacked = Array.zip blends vkPipelines |> Map.ofArray
            vkPipelinesPacked
        
        /// Get the Vulkan Pipeline built for the given blend.
        static member getVkPipeline blend pipeline =
            Map.find blend pipeline._VkPipelines
        
        /// Write a uniform to the descriptor set for each frame in flight.
        static member writeDescriptorUniform (binding : int) (arrayIndex : int) (buffers : VkBuffer array) (pipeline : Pipeline) device =

            for i in 0 .. dec pipeline._DescriptorSets.Length do
            
                // buffer info
                let mutable info = VkDescriptorBufferInfo ()
                info.buffer <- buffers.[i]
                info.range <- Vulkan.VK_WHOLE_SIZE

                // write descriptor set
                let mutable write = VkWriteDescriptorSet ()
                write.dstSet <- pipeline._DescriptorSets.[i]
                write.dstBinding <- uint binding
                write.dstArrayElement <- uint arrayIndex
                write.descriptorCount <- 1u
                write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                write.pBufferInfo <- asPointer &info
                Vulkan.vkUpdateDescriptorSets (device, 1u, asPointer &write, 0u, nullPtr)
        
        /// Write a texture to the descriptor set for each frame in flight.
        static member writeDescriptorTexture (binding : int) (arrayIndex : int) (texture : Texture.VulkanTexture) (pipeline : Pipeline) device =
            
            for i in 0 .. dec pipeline._DescriptorSets.Length do
            
                // image info
                let mutable info = VkDescriptorImageInfo ()
                info.sampler <- texture.Sampler
                info.imageView <- texture.ImageView
                info.imageLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

                // write descriptor set
                let mutable write = VkWriteDescriptorSet ()
                write.dstSet <- pipeline._DescriptorSets.[i]
                write.dstBinding <- uint binding
                write.dstArrayElement <- uint arrayIndex
                write.descriptorCount <- 1u
                write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                write.pImageInfo <- asPointer &info
                Vulkan.vkUpdateDescriptorSets (device, 1u, asPointer &write, 0u, nullPtr)

        /// Write a texture to the descriptor set for a single frame in flight.
        /// TODO: DJL: try to figure out more explicit naming for non-single frame variants.
        static member writeDescriptorTextureSingleFrame (binding : int) (arrayIndex : int) (texture : Texture.VulkanTexture) (pipeline : Pipeline) device =
            
            // image info
            let mutable info = VkDescriptorImageInfo ()
            info.sampler <- texture.Sampler
            info.imageView <- texture.ImageView
            info.imageLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

            // write descriptor set
            let mutable write = VkWriteDescriptorSet ()
            write.dstSet <- pipeline.DescriptorSet
            write.dstBinding <- uint binding
            write.dstArrayElement <- uint arrayIndex
            write.descriptorCount <- 1u
            write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            write.pImageInfo <- asPointer &info
            Vulkan.vkUpdateDescriptorSets (device, 1u, asPointer &write, 0u, nullPtr)
        
        /// Destroy a Pipeline.
        static member destroy pipeline device =
            Map.iter (fun _ vkPipeline -> Vulkan.vkDestroyPipeline (device, vkPipeline, nullPtr)) pipeline._VkPipelines
            Vulkan.vkDestroyDescriptorPool (device, pipeline._DescriptorPool, nullPtr)
            Vulkan.vkDestroyPipelineLayout (device, pipeline.PipelineLayout, nullPtr)
            Vulkan.vkDestroyDescriptorSetLayout (device, pipeline._DescriptorSetLayout, nullPtr)

        /// Create a Pipeline.
        static member create shaderPath cullFace (blends : Blend array) vertexBindings vertexAttributes resourceBindings pushConstantRanges renderPass device =
            
            // ensure at least one pipeline is created
            if blends.Length < 1 then Log.fail "No pipeline blend was specified."
            
            // create everything
            let descriptorSetLayout = Pipeline.createDescriptorSetLayout resourceBindings device
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayout pushConstantRanges device
            let descriptorPool = Pipeline.createDescriptorPool resourceBindings device
            let descriptorSets = Pipeline.createDescriptorSets descriptorSetLayout descriptorPool device
            let vkPipelines = Pipeline.createVkPipelines shaderPath cullFace blends vertexBindings vertexAttributes pipelineLayout renderPass device

            // make Pipeline
            let pipeline =
                { _VkPipelines = vkPipelines
                  _DescriptorPool = descriptorPool
                  _DescriptorSets = descriptorSets
                  _PipelineLayout = pipelineLayout
                  _DescriptorSetLayout = descriptorSetLayout }

            // fin
            pipeline