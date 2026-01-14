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
                         (VkBlendFactor.SrcAlpha, VkBlendFactor.OneMinusSrcAlpha,
                          VkBlendFactor.One, VkBlendFactor.Zero))
            | Additive ->
                Hl.makeBlendAttachment
                    (Some
                         (VkBlendFactor.SrcAlpha, VkBlendFactor.One,
                          VkBlendFactor.One, VkBlendFactor.Zero))
            | Overwrite ->
                Hl.makeBlendAttachment
                    (Some
                         (VkBlendFactor.One, VkBlendFactor.Zero,
                          VkBlendFactor.One, VkBlendFactor.Zero))
            | ImGui ->
                Hl.makeBlendAttachment
                    (Some
                         (VkBlendFactor.SrcAlpha, VkBlendFactor.OneMinusSrcAlpha,
                          VkBlendFactor.One, VkBlendFactor.OneMinusSrcAlpha))

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
          InputRate : VkVertexInputRate
          Attributes : VertexAttribute array }

    /// Describes a binding for a vertex and its attributes.
    let vertex binding stride inputRate attributes =
        { Binding = binding
          Stride = stride
          InputRate = inputRate
          Attributes = attributes }
    
    /// Describes a binding for a resource descriptor (aka uniform).
    /// TODO: DJL: eventually integrate this descriptor count properly into overall descriptor count management.
    type DescriptorBinding =
        { Binding : int
          DescriptorType : Hl.DescriptorType
          ShaderStage : Hl.ShaderStage
          DescriptorCount : int }

    /// Describes a binding for a resource descriptor (aka uniform).
    let descriptor binding descriptorType shaderStage descriptorCount =
        { Binding = binding
          DescriptorType = descriptorType
          ShaderStage = shaderStage
          DescriptorCount = descriptorCount }
    
    /// Describes a descriptor set.
    type DescriptorSetDefinition =
        { DescriptorIndexed : bool
          Descriptors : DescriptorBinding array }

    /// Describes a descriptor set.
    let descriptorSet descriptorIndexed descriptors =
        { DescriptorIndexed = descriptorIndexed
          Descriptors = descriptors }
    
    /// Describes a push constant.
    type PushConstant =
        { Offset : int
          Size : int
          ShaderStage : Hl.ShaderStage }
    
    /// Describes a push constant.
    let pushConstant offset size shaderStage =
        { Offset = offset
          Size = size
          ShaderStage = shaderStage }
    
    /// Describes a depth test.
    type DepthTest =
        { VkFormat : VkFormat }

    /// Describes a depth test.
    let depthTest vkFormat =
        { VkFormat = vkFormat }
    
    /// An abstraction of a VkDescriptorSet parallelized for frames in flight.
    type private DescriptorSet =
        private
            { VkDescriptorSets_ : VkDescriptorSet array
              UniformDescriptorLimit_ : int
              UniformDescriptorsUpdated_ : int array }

        /// The VkDescriptorSet for the current frame.
        member this.VkDescriptorSet = this.VkDescriptorSets_.[Hl.CurrentFrame]

        /// Allocate the VkDescriptorSet for each frame in flight.
        static member private allocateVkDescriptorSets descriptorSetLayout descriptorPool device =
            let descriptorSetLayouts = Array.zeroCreate<VkDescriptorSetLayout> Constants.Vulkan.MaxFramesInFlight
            use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
            for i in 0 .. dec descriptorSetLayouts.Length do descriptorSetLayouts.[i] <- descriptorSetLayout
            let mutable info = VkDescriptorSetAllocateInfo ()
            info.descriptorPool <- descriptorPool
            info.descriptorSetCount <- uint descriptorSetLayouts.Length
            info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
            let vkDescriptorSets = Array.zeroCreate<VkDescriptorSet> Constants.Vulkan.MaxFramesInFlight
            use vkDescriptorSetsPin = new ArrayPin<_> (vkDescriptorSets)
            Vulkan.vkAllocateDescriptorSets (device, asPointer &info, vkDescriptorSetsPin.Pointer) |> Hl.check
            vkDescriptorSets
        
        /// Write a texture to the descriptor set. Must be used in-frame.
        /// TODO: DJL: convert this to an *update* method that tracks written textureIds to prevent massive redundent writes.
        static member writeDescriptorTexture (descriptorIndex : int) (binding : int) (texture : Texture.Texture) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            
            // image info
            let mutable info = VkDescriptorImageInfo ()
            info.sampler <- texture.Sampler
            info.imageView <- texture.ImageView
            info.imageLayout <- Hl.ShaderRead.VkImageLayout

            // write descriptor set
            let mutable write = VkWriteDescriptorSet ()
            write.dstSet <- descriptorSet.VkDescriptorSet
            write.dstBinding <- uint binding
            write.dstArrayElement <- uint descriptorIndex
            write.descriptorCount <- 1u
            write.descriptorType <- VkDescriptorType.CombinedImageSampler
            write.pImageInfo <- asPointer &info
            Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)
        
        /// Update descriptor sets as uniform buffers are added. Must be used in-frame.
        /// TODO: DJL: this method can not yet handle resized or otherwise replaced buffers in already used index slots.
        static member updateDescriptorsUniform (binding : int) (uniform : Buffer.Buffer) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            while uniform.Count > descriptorSet.UniformDescriptorsUpdated_.[binding] do
                let descriptorIndex = descriptorSet.UniformDescriptorsUpdated_.[binding]
                if descriptorIndex < descriptorSet.UniformDescriptorLimit_ then 
                    for descriptorSetIndex in 0 .. dec descriptorSet.VkDescriptorSets_.Length do
                        let mutable info = VkDescriptorBufferInfo ()
                        info.buffer <- (uniform.VkBuffers descriptorIndex).[descriptorSetIndex]
                        info.range <- Vulkan.VK_WHOLE_SIZE
                        let mutable write = VkWriteDescriptorSet ()
                        write.dstSet <- descriptorSet.VkDescriptorSets_.[descriptorSetIndex]
                        write.dstBinding <- uint binding
                        write.dstArrayElement <- uint descriptorIndex
                        write.descriptorCount <- 1u
                        write.descriptorType <- VkDescriptorType.UniformBuffer
                        write.pBufferInfo <- asPointer &info
                        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)
                    descriptorSet.UniformDescriptorsUpdated_.[binding] <- inc descriptorSet.UniformDescriptorsUpdated_.[binding]
                else Log.warnOnce "Attempted uniform buffer write to descriptor set has exceeded descriptor count."

        /// Create a DescriptorSet.
        static member create uniformDescriptorLimit highestBinding descriptorSetLayout descriptorPool device =

            // allocate vkDescriptorSets
            let vkDescriptorSets = DescriptorSet.allocateVkDescriptorSets descriptorSetLayout descriptorPool device

            // includes non-uniform and any non-existent bindings for uncomplicated indexing
            let uniformDescriptorsUpdated = Array.zeroCreate<int> (highestBinding + 1)

            // make DescriptorSet
            let descriptorSet =
                { VkDescriptorSets_ = vkDescriptorSets
                  UniformDescriptorLimit_ = uniformDescriptorLimit
                  UniformDescriptorsUpdated_ = uniformDescriptorsUpdated }
            
            // fin
            descriptorSet
    
    /// An abstraction of a rendering pipeline.
    type Pipeline =
        private
            { VkPipelines_ : Map<Blend * bool, VkPipeline>
              DescriptorPool_ : VkDescriptorPool
              DescriptorSets_ : DescriptorSet array
              PipelineLayout_ : VkPipelineLayout
              DescriptorSetLayouts_ : VkDescriptorSetLayout array }

        /// The pipeline layout.
        member this.PipelineLayout = this.PipelineLayout_
        
        /// The descriptor set of the given number for the current frame.
        member this.VkDescriptorSet setNumber = this.DescriptorSets_.[setNumber].VkDescriptorSet
        
        /// Create the descriptor pool.
        static member private createDescriptorPool descriptorIndexing (resourceBindingsSets : VkDescriptorSetLayoutBinding array array) device =
            
            // collect bindings from all sets
            let resourceBindings = Array.concat resourceBindingsSets
            
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
            if descriptorIndexing then info.flags <- VkDescriptorPoolCreateFlags.UpdateAfterBind
            info.maxSets <- uint (Constants.Vulkan.MaxFramesInFlight * resourceBindingsSets.Length)
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            Vulkan.vkCreateDescriptorPool (device, &info, nullPtr, &descriptorPool) |> Hl.check
            descriptorPool

        /// Create the descriptor set layout.
        static member private createDescriptorSetLayout descriptorIndexing (resourceBindings : VkDescriptorSetLayoutBinding array) device =
            
            // bit of boilerplate for descriptor indexing
            let bindingFlags =
                VkDescriptorBindingFlags.UpdateAfterBind |||
                VkDescriptorBindingFlags.UpdateUnusedWhilePending |||
                VkDescriptorBindingFlags.PartiallyBound
            let bindingFlagsArray = Array.create resourceBindings.Length bindingFlags
            use bindingFlagsArrayPin = new ArrayPin<_> (bindingFlagsArray) // must be in scope for create function
            let mutable bfInfo = VkDescriptorSetLayoutBindingFlagsCreateInfo ()
            bfInfo.bindingCount <- uint bindingFlagsArray.Length
            bfInfo.pBindingFlags <- bindingFlagsArrayPin.Pointer
            
            // create descriptor set layout
            use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
            let mutable info = VkDescriptorSetLayoutCreateInfo ()
            if descriptorIndexing then
                info.flags <- VkDescriptorSetLayoutCreateFlags.UpdateAfterBindPool
                info.pNext <- asVoidPtr &bfInfo
            info.bindingCount <- uint resourceBindings.Length
            info.pBindings <- resourceBindingsPin.Pointer
            let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
            Vulkan.vkCreateDescriptorSetLayout (device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
            descriptorSetLayout

        /// Create the pipeline layout.
        static member private createPipelineLayout (descriptorSetLayouts : VkDescriptorSetLayout array) (pushConstantRanges : VkPushConstantRange array) device =
            use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
            use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
            let mutable info = VkPipelineLayoutCreateInfo ()
            info.setLayoutCount <- uint descriptorSetLayouts.Length
            info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
            info.pushConstantRangeCount <- uint pushConstantRanges.Length
            info.pPushConstantRanges <- pushConstantRangesPin.Pointer
            let mutable pipelineLayout = Unchecked.defaultof<VkPipelineLayout>
            Vulkan.vkCreatePipelineLayout (device, &info, nullPtr, &pipelineLayout) |> Hl.check
            pipelineLayout
        
        /// Create the Vulkan pipelines themselves.
        static member private createVkPipelines
            shaderPath
            (pipelineSettings : (Blend * bool) array)
            (vertexBindings : VkVertexInputBindingDescription array)
            (vertexAttributes : VkVertexInputAttributeDescription array)
            pipelineLayout
            colorAttachmentFormat
            (depthTestOpt : DepthTest option)
            device =
            
            // create shader modules
            let vertModule = Hl.createShaderModuleFromGlsl (shaderPath + ".vert") ShaderKind.VertexShader device
            let fragModule = Hl.createShaderModuleFromGlsl (shaderPath + ".frag") ShaderKind.FragmentShader device

            // shader stage infos
            use entryPoint = new StringWrap ("main")
            let ssInfos = Array.zeroCreate<VkPipelineShaderStageCreateInfo> 2
            ssInfos[0] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[0].stage <- VkShaderStageFlags.Vertex
            ssInfos[0].``module`` <- vertModule
            ssInfos[0].pName <- entryPoint.Pointer
            ssInfos[1] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[1].stage <- VkShaderStageFlags.Fragment
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

            // rasterization info (cull mode set below)
            let mutable rInfo = VkPipelineRasterizationStateCreateInfo ()
            rInfo.polygonMode <- VkPolygonMode.Fill
            rInfo.frontFace <- VkFrontFace.CounterClockwise
            rInfo.lineWidth <- 1.0f

            // input assembly; multisample
            let mutable iaInfo = VkPipelineInputAssemblyStateCreateInfo (topology = VkPrimitiveTopology.TriangleList)
            let mutable mInfo = VkPipelineMultisampleStateCreateInfo (rasterizationSamples = VkSampleCountFlags.Count1)
            
            // depth-stencil info
            let mutable dInfo = VkPipelineDepthStencilStateCreateInfo ()
            match depthTestOpt with
            | Some _ ->
                dInfo.depthWriteEnable <- true
            | None -> ()

            // dynamic state info
            let dynamicStates =
                match depthTestOpt with
                | Some _ -> [|VkDynamicState.Viewport; VkDynamicState.Scissor; VkDynamicState.DepthTestEnable; VkDynamicState.DepthCompareOp|]
                | None -> [|VkDynamicState.Viewport; VkDynamicState.Scissor|]
            use dynamicStatesPin = new ArrayPin<_> (dynamicStates)
            let mutable dsInfo = VkPipelineDynamicStateCreateInfo ()
            dsInfo.dynamicStateCount <- uint dynamicStates.Length
            dsInfo.pDynamicStates <- dynamicStatesPin.Pointer

            // rendering info
            let mutable format = colorAttachmentFormat
            let mutable rnInfo = VkPipelineRenderingCreateInfo ()
            rnInfo.colorAttachmentCount <- 1u
            rnInfo.pColorAttachmentFormats <- asPointer &format
            match depthTestOpt with
            | Some depthTest -> rnInfo.depthAttachmentFormat <- depthTest.VkFormat
            | None -> ()
            
            // create vulkan pipelines
            let vkPipelines = Array.zeroCreate<VkPipeline> pipelineSettings.Length
            for i in 0 .. dec vkPipelines.Length do
            
                // extract settings
                let (blend, cullFace) = pipelineSettings.[i]
                
                // blend info
                let mutable blendState = Blend.makeAttachment blend
                let mutable bInfo = VkPipelineColorBlendStateCreateInfo ()
                bInfo.attachmentCount <- 1u
                bInfo.pAttachments <- asPointer &blendState

                // cull mode
                rInfo.cullMode <- if cullFace then VkCullModeFlags.Back else VkCullModeFlags.None

                // create vulkan pipeline
                // TODO: DJL: create vulkan pipelines with single call outside loop, then with derivatives, then with cache.
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
            
            // pack vulkan pipelines with settings
            let vkPipelinesPacked = Array.zip pipelineSettings vkPipelines |> Map.ofArray
            vkPipelinesPacked
        
        /// Get the Vulkan Pipeline built for the given settings.
        static member getVkPipeline blend cullFace pipeline =
            Map.find (blend, cullFace) pipeline.VkPipelines_
        
        /// Write a texture to the descriptor set. Must be used in-frame.
        /// TODO: DJL: convert this to an *update* method that tracks written textureIds to prevent massive redundent writes.
        static member writeDescriptorTexture (descriptorIndex : int) setNumber (binding : int) (texture : Texture.Texture) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorTexture descriptorIndex binding texture pipeline.DescriptorSets_.[setNumber] vkc
        
        /// Update descriptor sets as uniform buffers are added. Must be used in-frame.
        /// TODO: DJL: this method can not yet handle resized or otherwise replaced buffers in already used index slots.
        static member updateDescriptorsUniform setNumber (binding : int) (uniform : Buffer.Buffer) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.updateDescriptorsUniform binding uniform pipeline.DescriptorSets_.[setNumber] vkc
        
        /// Destroy a Pipeline.
        static member destroy pipeline (vkc : Hl.VulkanContext) =
            Map.iter (fun _ vkPipeline -> Vulkan.vkDestroyPipeline (vkc.Device, vkPipeline, nullPtr)) pipeline.VkPipelines_
            Vulkan.vkDestroyDescriptorPool (vkc.Device, pipeline.DescriptorPool_, nullPtr)
            Vulkan.vkDestroyPipelineLayout (vkc.Device, pipeline.PipelineLayout, nullPtr)
            for i in 0 .. dec pipeline.DescriptorSetLayouts_.Length do Vulkan.vkDestroyDescriptorSetLayout (vkc.Device, pipeline.DescriptorSetLayouts_.[i], nullPtr)

        /// Create a Pipeline.
        static member create
            shaderPath
            (blends : Blend array)
            (vertexBindings : VertexBinding array)
            (descriptorSetDefinitions : DescriptorSetDefinition array)
            (pushConstants : PushConstant array)
            colorAttachmentFormat
            (depthTestOpt : DepthTest option)
            (vkc : Hl.VulkanContext) =
            
            // convert vertex and push constant data to vulkan objects
            let vertexBindingDescriptions = Array.map (fun (binding : VertexBinding) -> Hl.makeVertexBinding binding.Binding binding.Stride binding.InputRate ) vertexBindings
            let vertexAttributes =
                [|for i in 0 .. dec vertexBindings.Length do
                      for j in 0 .. dec vertexBindings.[i].Attributes.Length do
                          let attribute = vertexBindings.[i].Attributes.[j]
                          yield Hl.makeVertexAttribute attribute.Location vertexBindings.[i].Binding attribute.Format attribute.Offset |]
            let pushConstantRanges = Array.map (fun pushConstant -> Hl.makePushConstantRange pushConstant.Offset pushConstant.Size pushConstant.ShaderStage) pushConstants

            // the drawing instance limit if descriptor indexing
            // TODO: DJL: decide global strategy for allocating appropriate descriptor counts to avoid hitting
            // VkPhysicalDeviceDescriptorIndexingProperties.maxUpdateAfterBindDescriptorsInAllPools.
            let descriptorIndexingLimit = 32768 // just inlining reasonable count for now
            
            // process each descriptor set definition
            let layoutBindingsSets = Array.zeroCreate descriptorSetDefinitions.Length
            let highestBindings = Array.zeroCreate descriptorSetDefinitions.Length
            let descriptorSetLayouts = Array.zeroCreate descriptorSetDefinitions.Length
            let descriptorSets = Array.zeroCreate descriptorSetDefinitions.Length
            for i in 0 .. dec descriptorSetDefinitions.Length do
                layoutBindingsSets.[i] <-
                    Array.map
                        (fun binding ->
                             
                             // NOTE: DJL: currently, binding.DescriptorCount higher than 1 is unusable for uniforms due to uniform descriptor limit in DescriptorSet.
                             let descriptorCount = if descriptorSetDefinitions.[i].DescriptorIndexed then descriptorIndexingLimit else binding.DescriptorCount
                             Hl.makeDescriptorBinding binding.Binding binding.DescriptorType descriptorCount binding.ShaderStage)
                        descriptorSetDefinitions.[i].Descriptors
                highestBindings.[i] <- Array.maxBy (fun (layoutBinding : VkDescriptorSetLayoutBinding) -> layoutBinding.binding) layoutBindingsSets.[i]
                descriptorSetLayouts.[i] <- Pipeline.createDescriptorSetLayout descriptorSetDefinitions.[i].DescriptorIndexed layoutBindingsSets.[i] vkc.Device
            
            // create descriptor pool
            let descriptorIndexing = Array.exists (fun (descSetDef : DescriptorSetDefinition) -> descSetDef.DescriptorIndexed) descriptorSetDefinitions
            let descriptorPool = Pipeline.createDescriptorPool descriptorIndexing layoutBindingsSets vkc.Device
            
            // create descriptor sets
            for i in 0 .. dec descriptorSetDefinitions.Length do
                let uniformDescriptorLimit = if descriptorSetDefinitions.[i].DescriptorIndexed then descriptorIndexingLimit else 1
                descriptorSets.[i] <- DescriptorSet.create uniformDescriptorLimit (int highestBindings.[i].binding) descriptorSetLayouts.[i] descriptorPool vkc.Device
            
            // create pipeline layout and vkPipelines
            if blends.Length < 1 then Log.fail "No pipeline blend was specified."
            let pipelineSettings = Array.allPairs blends [|false; true|] // blend and cull modes
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayouts pushConstantRanges vkc.Device
            let vkPipelines = Pipeline.createVkPipelines shaderPath pipelineSettings vertexBindingDescriptions vertexAttributes pipelineLayout colorAttachmentFormat depthTestOpt vkc.Device

            // make Pipeline
            let pipeline =
                { VkPipelines_ = vkPipelines
                  DescriptorPool_ = descriptorPool
                  DescriptorSets_ = descriptorSets
                  PipelineLayout_ = pipelineLayout
                  DescriptorSetLayouts_ = descriptorSetLayouts }

            // fin
            pipeline