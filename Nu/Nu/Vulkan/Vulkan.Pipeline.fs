// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open Vortice.ShaderCompiler
open Prime
open Nu

[<RequireQualifiedAccess>]
module Pipeline =

    /// A blend setting for a Vulkan pipeline.
    /// TODO: DJL: review naming.
    type Blend =
        | NoBlend
        | Transparent
        | Additive
        | Overwrite
        | ImGui

        /// Make blend attachment.
        static member makeAttachment blend =
            match blend with
            | NoBlend ->
                Hl.makeBlendAttachment None
            | Transparent ->
                Hl.makeBlendAttachment
                    (Some
                         (Vulkan.VK_BLEND_FACTOR_SRC_ALPHA, Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                          Vulkan.VK_BLEND_FACTOR_ONE, Vulkan.VK_BLEND_FACTOR_ZERO))
            | Additive ->
                Hl.makeBlendAttachment
                    (Some
                         (Vulkan.VK_BLEND_FACTOR_SRC_ALPHA, Vulkan.VK_BLEND_FACTOR_ONE,
                          Vulkan.VK_BLEND_FACTOR_ONE, Vulkan.VK_BLEND_FACTOR_ZERO))
            | Overwrite ->
                Hl.makeBlendAttachment
                    (Some
                         (Vulkan.VK_BLEND_FACTOR_ONE, Vulkan.VK_BLEND_FACTOR_ZERO,
                          Vulkan.VK_BLEND_FACTOR_ONE, Vulkan.VK_BLEND_FACTOR_ZERO))
            | ImGui ->
                Hl.makeBlendAttachment
                    (Some
                         (Vulkan.VK_BLEND_FACTOR_SRC_ALPHA, Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                          Vulkan.VK_BLEND_FACTOR_ONE, Vulkan.VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA))

    /// Describes a vertex attribute in the context of a vertex binding.
    type VertexAttribute =
        { Location : int
          Format : Hl.VertexAttribFormat
          Offset : int }

    /// Describes a vertex attribute in the context of a vertex binding.
    let attribute location format offset =
        { Location = location
          Format = format
          Offset = offset }
    
    /// Describes a binding for a vertex and its attributes.
    type VertexBinding =
        { Binding : int
          Stride : int
          Attributes : VertexAttribute array }

    /// Describes a binding for a vertex and its attributes.
    let vertex binding stride attributes =
        { Binding = binding
          Stride = stride
          Attributes = attributes }
    
    /// Describes a binding for a resource descriptor (aka uniform).
    type DescriptorBinding =
        { Binding : int
          DescriptorType : Hl.DescriptorType
          ShaderStage : Hl.ShaderStage }

    /// Describes a binding for a resource descriptor (aka uniform).
    let descriptor binding descriptorType shaderStage =
        { Binding = binding
          DescriptorType = descriptorType
          ShaderStage = shaderStage }
    
    /// An abstraction of a rendering pipeline.
    type Pipeline =
        private
            { VkPipelines_ : Map<Blend, VkPipeline>
              DescriptorPool_ : VkDescriptorPool
              DescriptorSets_ : VkDescriptorSet array
              PipelineLayout_ : VkPipelineLayout
              DescriptorSetLayout_ : VkDescriptorSetLayout
              DescriptorCount_ : int
              UniformDescriptorsUpdated_ : int array }

        /// The pipeline layout.
        member this.PipelineLayout = this.PipelineLayout_
        
        /// The descriptor set for the current frame.
        member this.DescriptorSet = this.DescriptorSets_.[Hl.CurrentFrame]
        
        /// Create the descriptor pool.
        static member private createDescriptorPool descriptorIndexing (resourceBindings : VkDescriptorSetLayoutBinding array) device =
            
            // derive pool sizes from layout bindings
            let poolSizes = Array.zeroCreate<VkDescriptorPoolSize> resourceBindings.Length
            use poolSizesPin = new ArrayPin<_> (poolSizes)
            for i in [0 .. dec resourceBindings.Length] do
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- resourceBindings.[i].descriptorType
                poolSize.descriptorCount <- resourceBindings.[i].descriptorCount * uint Constants.Vulkan.MaxFramesInFlight
                poolSizes.[i] <- poolSize
            
            // create descriptor pool
            let mutable info = VkDescriptorPoolCreateInfo ()
            if descriptorIndexing then info.flags <- Vulkan.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT
            info.maxSets <- uint Constants.Vulkan.MaxFramesInFlight
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
            descriptorPool

        /// Create the descriptor set layout.
        static member private createDescriptorSetLayout descriptorIndexing (resourceBindings : VkDescriptorSetLayoutBinding array) device =
            
            // bit of boilerplate for descriptor indexing
            let bindingFlags =
                Vulkan.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT |||
                Vulkan.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT |||
                Vulkan.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT
            let bindingFlagsArray = Array.create resourceBindings.Length bindingFlags
            use bindingFlagsArrayPin = new ArrayPin<_> (bindingFlagsArray) // must be in scope for create function
            let mutable bfInfo = VkDescriptorSetLayoutBindingFlagsCreateInfo ()
            bfInfo.bindingCount <- uint bindingFlagsArray.Length
            bfInfo.pBindingFlags <- bindingFlagsArrayPin.Pointer
            
            // create descriptor set layout
            use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
            let mutable info = VkDescriptorSetLayoutCreateInfo ()
            if descriptorIndexing then
                info.flags <- Vulkan.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT
                info.pNext <- asVoidPtr &bfInfo
            info.bindingCount <- uint resourceBindings.Length
            info.pBindings <- resourceBindingsPin.Pointer
            let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
            Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
            descriptorSetLayout

        /// Create the pipeline layout.
        static member private createPipelineLayout descriptorSetLayout (pushConstantRanges : VkPushConstantRange array) device =
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
        
        /// Create the descriptor set for each frame in flight.
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
        static member private createVkPipelines
            shaderPath
            cullFace
            (blends : Blend array)
            (vertexBindings : VkVertexInputBindingDescription array)
            (vertexAttributes : VkVertexInputAttributeDescription array)
            pipelineLayout
            format
            device =
            
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

            // rendering info
            let mutable format = format
            let mutable rnInfo = VkPipelineRenderingCreateInfo ()
            rnInfo.colorAttachmentCount <- 1u
            rnInfo.pColorAttachmentFormats <- asPointer &format
            
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
                info.pNext <- asVoidPtr &rnInfo
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
                info.renderPass <- VkRenderPass.Null
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
            Map.find blend pipeline.VkPipelines_
        
        /// Write a texture to the descriptor set during the frame.
        /// TODO: DJL: convert this to an *update* method that tracks written textureIds to prevent massive redundent writes.
        static member writeDescriptorTexture (binding : int) (descriptorIndex : int) (texture : Texture.VulkanTexture) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            
            // image info
            let mutable info = VkDescriptorImageInfo ()
            info.sampler <- texture.Sampler
            info.imageView <- texture.ImageView
            info.imageLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

            // write descriptor set
            let mutable write = VkWriteDescriptorSet ()
            write.dstSet <- pipeline.DescriptorSet
            write.dstBinding <- uint binding
            write.dstArrayElement <- uint descriptorIndex
            write.descriptorCount <- 1u
            write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
            write.pImageInfo <- asPointer &info
            Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)
        
        /// Write a texture to the descriptor sets at initialization.
        /// NOTE: DJL: this method is intended for eventual removal, do not use outside of ImGui font atlas.
        static member writeDescriptorTextureInit (binding : int) (descriptorIndex : int) (texture : Texture.VulkanTexture) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            
            for i in 0 .. dec pipeline.DescriptorSets_.Length do
            
                // image info
                let mutable info = VkDescriptorImageInfo ()
                info.sampler <- texture.Sampler
                info.imageView <- texture.ImageView
                info.imageLayout <- Vulkan.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

                // write descriptor set
                let mutable write = VkWriteDescriptorSet ()
                write.dstSet <- pipeline.DescriptorSets_.[i]
                write.dstBinding <- uint binding
                write.dstArrayElement <- uint descriptorIndex
                write.descriptorCount <- 1u
                write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                write.pImageInfo <- asPointer &info
                Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

        /// Update descriptor sets as uniform buffers are added.
        /// TODO: DJL: this method can not yet handle resized or otherwise replaced buffers in already used index slots.
        static member updateDescriptorsUniform (binding : int) (uniform : Buffer.Buffer) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            while uniform.Count > pipeline.UniformDescriptorsUpdated_.[binding] do
                let descriptorIndex = pipeline.UniformDescriptorsUpdated_.[binding]
                if descriptorIndex < pipeline.DescriptorCount_ then 
                    for descriptorSet in 0 .. dec pipeline.DescriptorSets_.Length do
                        let mutable info = VkDescriptorBufferInfo ()
                        info.buffer <- (uniform.VkBuffers descriptorIndex).[descriptorSet]
                        info.range <- Vulkan.VK_WHOLE_SIZE
                        let mutable write = VkWriteDescriptorSet ()
                        write.dstSet <- pipeline.DescriptorSets_.[descriptorSet]
                        write.dstBinding <- uint binding
                        write.dstArrayElement <- uint descriptorIndex
                        write.descriptorCount <- 1u
                        write.descriptorType <- Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
                        write.pBufferInfo <- asPointer &info
                        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)
                    pipeline.UniformDescriptorsUpdated_.[binding] <- inc pipeline.UniformDescriptorsUpdated_.[binding]
                else Log.warnOnce "Attempted uniform buffer write to descriptor set has exceeded descriptor count."
        
        /// Destroy a Pipeline.
        static member destroy pipeline (vkc : Hl.VulkanContext) =
            Map.iter (fun _ vkPipeline -> Vulkan.vkDestroyPipeline (vkc.Device, vkPipeline, nullPtr)) pipeline.VkPipelines_
            Vulkan.vkDestroyDescriptorPool (vkc.Device, pipeline.DescriptorPool_, nullPtr)
            Vulkan.vkDestroyPipelineLayout (vkc.Device, pipeline.PipelineLayout, nullPtr)
            Vulkan.vkDestroyDescriptorSetLayout (vkc.Device, pipeline.DescriptorSetLayout_, nullPtr)

        /// Create a Pipeline.
        static member create
            shaderPath
            descriptorIndexing
            cullFace
            (blends : Blend array)
            (vertexBindings : VertexBinding array)
            (descriptorBindings : DescriptorBinding array)
            pushConstantRanges
            (vkc : Hl.VulkanContext) =
            
            // ensure at least one pipeline is created
            if blends.Length < 1 then Log.fail "No pipeline blend was specified."
            
            // blow up descriptor counts if indexing
            // TODO: DJL: decide global strategy for allocating appropriate descriptor counts to avoid hitting
            // VkPhysicalDeviceDescriptorIndexingProperties.maxUpdateAfterBindDescriptorsInAllPools.
            let descriptorCount = if descriptorIndexing then 65536 else 1 // just inlining reasonable count for now
            
            // convert binding data to vulkan objects
            let vertexBindingDescriptions = Array.map (fun (binding : VertexBinding) -> Hl.makeVertexBindingVertex binding.Binding binding.Stride ) vertexBindings
            let vertexAttributes =
                [|for i in 0 .. dec vertexBindings.Length do
                      for j in 0 .. dec vertexBindings.[i].Attributes.Length do
                          let attribute = vertexBindings.[i].Attributes.[j]
                          yield Hl.makeVertexAttribute attribute.Location vertexBindings.[i].Binding attribute.Format attribute.Offset |]
            let layoutBindings = Array.map (fun binding -> Hl.makeDescriptorBinding binding.Binding binding.DescriptorType descriptorCount binding.ShaderStage) descriptorBindings

            // create everything
            let descriptorPool = Pipeline.createDescriptorPool descriptorIndexing layoutBindings vkc.Device
            let descriptorSetLayout = Pipeline.createDescriptorSetLayout descriptorIndexing layoutBindings vkc.Device
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayout pushConstantRanges vkc.Device
            let descriptorSets = Pipeline.createDescriptorSets descriptorSetLayout descriptorPool vkc.Device
            let vkPipelines = Pipeline.createVkPipelines shaderPath cullFace blends vertexBindingDescriptions vertexAttributes pipelineLayout vkc.SwapFormat vkc.Device
            let uniformDescriptorsUpdated = Array.zeroCreate<int> layoutBindings.Length // includes non uniform bindings for uncomplicated indexing

            // make Pipeline
            let pipeline =
                { VkPipelines_ = vkPipelines
                  DescriptorPool_ = descriptorPool
                  DescriptorSets_ = descriptorSets
                  PipelineLayout_ = pipelineLayout
                  DescriptorSetLayout_ = descriptorSetLayout
                  DescriptorCount_ = descriptorCount
                  UniformDescriptorsUpdated_ = uniformDescriptorsUpdated }

            // fin
            pipeline