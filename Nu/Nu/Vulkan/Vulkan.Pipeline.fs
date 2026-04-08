
namespace Vortice.Vulkan
open System
open System.Collections.Generic
open System.Diagnostics
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
    [<DebuggerHidden; DebuggerStepThrough>]
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
    [<DebuggerHidden; DebuggerStepThrough>]
    let vertex binding stride inputRate attributes =
        { Binding = binding
          Stride = stride
          InputRate = inputRate
          Attributes = attributes }
    
    /// Describes a binding for a resource descriptor (aka uniform).
    type DescriptorBinding =
        { Binding : int
          DescriptorType : Hl.DescriptorType
          ShaderStage : Hl.ShaderStage
          DescriptorCount : int }

    /// Describes a binding for a resource descriptor (aka uniform).
    [<DebuggerHidden; DebuggerStepThrough>]
    let descriptor binding descriptorType shaderStage descriptorCount =
        { Binding = binding
          DescriptorType = descriptorType
          ShaderStage = shaderStage
          DescriptorCount = descriptorCount }
    
    /// Describes a descriptor set.
    type DescriptorSetDefinition =
        { BulkMode : Hl.BulkDescriptorMode
          SetCount : int
          Descriptors : DescriptorBinding array }

    /// Describes a descriptor set.
    [<DebuggerHidden; DebuggerStepThrough>]
    let descriptorSet bulkMode setCount descriptors =
        { BulkMode = bulkMode
          SetCount = setCount
          Descriptors = descriptors }
    
    /// Describes a push constant.
    type PushConstant =
        { Offset : int
          Size : int
          ShaderStage : Hl.ShaderStage }
    
    /// Describes a push constant.
    [<DebuggerHidden; DebuggerStepThrough>]
    let pushConstant offset size shaderStage =
        { Offset = offset
          Size = size
          ShaderStage = shaderStage }
    
    /// Convert DepthTest to VkCompareOp.
    /// TODO: BGE: since this is the odd one out in terms of being the only function directly in this module that is
    /// not an operator, perhaps it should either be privatized or moved elsewhere?
    let depthTestToVkCompareOp depthTest =
        match depthTest with
        | LessThanTest -> VkCompareOp.Less
        | LessThanOrEqualTest -> VkCompareOp.LessOrEqual
        | EqualTest -> VkCompareOp.Equal
        | GreaterThanOrEqualTest -> VkCompareOp.GreaterOrEqual
        | GreaterThanTest -> VkCompareOp.Greater
        | NeverPassTest -> VkCompareOp.Never
        | AlwaysPassTest -> VkCompareOp.Always
    
    type private DescriptorSetState =
        private
            { BuffersWritten : uint64 List array
              ImageViewsWritten : uint64 List array
              SamplersWritten : uint64 List array }

        member this.GetBuffer binding index =
            while this.BuffersWritten.[binding].Count <= index do this.BuffersWritten.[binding].Add 0UL
            this.BuffersWritten.[binding].[index]
        
        member this.GetImageView binding index =
            while this.ImageViewsWritten.[binding].Count <= index do this.ImageViewsWritten.[binding].Add 0UL
            this.ImageViewsWritten.[binding].[index]
        
        member this.GetSampler binding index =
            while this.SamplersWritten.[binding].Count <= index do this.SamplersWritten.[binding].Add 0UL
            this.SamplersWritten.[binding].[index]

        member this.SetBuffer binding index handle =
            while this.BuffersWritten.[binding].Count <= index do this.BuffersWritten.[binding].Add 0UL
            this.BuffersWritten.[binding].[index] <- handle
        
        member this.SetImageView binding index handle =
            while this.ImageViewsWritten.[binding].Count <= index do this.ImageViewsWritten.[binding].Add 0UL
            this.ImageViewsWritten.[binding].[index] <- handle
        
        member this.SetSampler binding index handle =
            while this.SamplersWritten.[binding].Count <= index do this.SamplersWritten.[binding].Add 0UL
            this.SamplersWritten.[binding].[index] <- handle
        
        static member create bindingsLength =
            let buffersWritten = Array.zeroCreate bindingsLength
            let imageViewsWritten = Array.zeroCreate bindingsLength
            let samplersWritten = Array.zeroCreate bindingsLength
            for i in 0 .. dec bindingsLength do
                buffersWritten.[i] <- List ()
                imageViewsWritten.[i] <- List ()
                samplersWritten.[i] <- List ()
            { BuffersWritten = buffersWritten
              ImageViewsWritten = imageViewsWritten
              SamplersWritten = samplersWritten }
    
    type private DescriptorSet =
        private
            { VkDescriptorSets_ : VkDescriptorSet array array
              DescriptorSetStates_ : DescriptorSetState array array
              DescriptorLimits_ : int array }

        member private this.DescriptorSetState index = this.DescriptorSetStates_.[Hl.CurrentFrame].[index]
        member this.VkDescriptorSet index = this.VkDescriptorSets_.[Hl.CurrentFrame].[index]

        static member private allocateVkDescriptorSets count descriptorSetLayout descriptorPool device =
            let descriptorSetLayouts = Array.zeroCreate<VkDescriptorSetLayout> (count * Constants.Vulkan.MaxFramesInFlight)
            use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
            for i in 0 .. dec descriptorSetLayouts.Length do descriptorSetLayouts.[i] <- descriptorSetLayout
            let mutable info = VkDescriptorSetAllocateInfo ()
            info.descriptorPool <- descriptorPool
            info.descriptorSetCount <- uint descriptorSetLayouts.Length
            info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
            let vkDescriptorSets = Array.zeroCreate<VkDescriptorSet> (count * Constants.Vulkan.MaxFramesInFlight)
            use vkDescriptorSetsPin = new ArrayPin<_> (vkDescriptorSets)
            Vulkan.vkAllocateDescriptorSets (device, asPointer &info, vkDescriptorSetsPin.Pointer) |> Hl.check
            vkDescriptorSets
        
        static member writeDescriptorBuffer descriptorType (binding : int) setIndex (descriptorIndex : int) (buffer : VkBuffer) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            
            // only proceed if within limit
            if descriptorIndex < descriptorSet.DescriptorLimits_.[binding] then
            
                // only proceed if buffer not already written
                if (descriptorSet.DescriptorSetState setIndex).GetBuffer binding descriptorIndex <> buffer.Handle then
            
                    // buffer info
                    let mutable info = VkDescriptorBufferInfo ()
                    info.buffer <- buffer
                    info.range <- Vulkan.VK_WHOLE_SIZE
                
                    // write descriptor set
                    let mutable write = VkWriteDescriptorSet ()
                    write.dstSet <- descriptorSet.VkDescriptorSet setIndex
                    write.dstBinding <- uint binding
                    write.dstArrayElement <- uint descriptorIndex
                    write.descriptorCount <- 1u
                    write.descriptorType <- descriptorType
                    write.pBufferInfo <- asPointer &info
                    Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

                    // record written buffer
                    (descriptorSet.DescriptorSetState setIndex).SetBuffer binding descriptorIndex buffer.Handle
            
            // warn if limit exceeded
            else Log.warnOnce "Attempted buffer write to descriptor set has exceeded descriptor count. You may have failed to pass the correct descriptor count at pipeline creation."

        static member writeDescriptorSampledImage (binding : int) setIndex (descriptorIndex : int) (imageView : VkImageView) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            
            // only proceed if within limit
            if descriptorIndex < descriptorSet.DescriptorLimits_.[binding] then
            
                // only proceed if image view not already written
                if (descriptorSet.DescriptorSetState setIndex).GetImageView binding descriptorIndex <> imageView.Handle then

                    // image info
                    let mutable info = VkDescriptorImageInfo ()
                    info.imageView <- imageView
                    info.imageLayout <- Hl.ShaderRead.VkImageLayout

                    // write descriptor set
                    let mutable write = VkWriteDescriptorSet ()
                    write.dstSet <- descriptorSet.VkDescriptorSet setIndex
                    write.dstBinding <- uint binding
                    write.dstArrayElement <- uint descriptorIndex
                    write.descriptorCount <- 1u
                    write.descriptorType <- VkDescriptorType.SampledImage
                    write.pImageInfo <- asPointer &info
                    Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

                    // record written image view
                    (descriptorSet.DescriptorSetState setIndex).SetImageView binding descriptorIndex imageView.Handle

            // warn if limit exceeded
            else Log.warnOnce "Attempted sampled image write to descriptor set has exceeded descriptor count. You may have failed to pass the correct descriptor count at pipeline creation."
        
        static member writeDescriptorSampler (binding : int) setIndex (descriptorIndex : int) (sampler : Texture.Sampler) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            
            // only proceed if within limit
            if descriptorIndex < descriptorSet.DescriptorLimits_.[binding] then
            
                // only proceed if sampler not already written
                if (descriptorSet.DescriptorSetState setIndex).GetSampler binding descriptorIndex <> sampler.VkSampler.Handle then
                
                    // image info
                    let mutable info = VkDescriptorImageInfo ()
                    info.sampler <- sampler.VkSampler

                    // write descriptor set
                    let mutable write = VkWriteDescriptorSet ()
                    write.dstSet <- descriptorSet.VkDescriptorSet setIndex
                    write.dstBinding <- uint binding
                    write.dstArrayElement <- uint descriptorIndex
                    write.descriptorCount <- 1u
                    write.descriptorType <- VkDescriptorType.Sampler
                    write.pImageInfo <- asPointer &info
                    Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

                    // record written sampler
                    (descriptorSet.DescriptorSetState setIndex).SetSampler binding descriptorIndex sampler.VkSampler.Handle

            // warn if limit exceeded
            else Log.warnOnce "Attempted sampler write to descriptor set has exceeded descriptor count. You may have failed to pass the correct descriptor count at pipeline creation."
        
        static member writeDescriptorCombinedImageSampler (binding : int) setIndex (descriptorIndex : int) (imageView : VkImageView) (sampler : Texture.Sampler) (descriptorSet : DescriptorSet) (vkc : Hl.VulkanContext) =
            
            // only proceed if within limit
            if descriptorIndex < descriptorSet.DescriptorLimits_.[binding] then
            
                // only proceed if image view or sampler not already written
                if (descriptorSet.DescriptorSetState setIndex).GetImageView binding descriptorIndex <> imageView.Handle ||
                   (descriptorSet.DescriptorSetState setIndex).GetSampler binding descriptorIndex <> sampler.VkSampler.Handle
                then
            
                    // image info
                    let mutable info = VkDescriptorImageInfo ()
                    info.sampler <- sampler.VkSampler
                    info.imageView <- imageView
                    info.imageLayout <- Hl.ShaderRead.VkImageLayout

                    // write descriptor set
                    let mutable write = VkWriteDescriptorSet ()
                    write.dstSet <- descriptorSet.VkDescriptorSet setIndex
                    write.dstBinding <- uint binding
                    write.dstArrayElement <- uint descriptorIndex
                    write.descriptorCount <- 1u
                    write.descriptorType <- VkDescriptorType.CombinedImageSampler
                    write.pImageInfo <- asPointer &info
                    Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

                    // record written image view and sampler
                    (descriptorSet.DescriptorSetState setIndex).SetImageView binding descriptorIndex imageView.Handle
                    (descriptorSet.DescriptorSetState setIndex).SetSampler binding descriptorIndex sampler.VkSampler.Handle

            // warn if limit exceeded
            else Log.warnOnce "Attempted combined image sampler write to descriptor set has exceeded descriptor count. You may have failed to pass the correct descriptor count at pipeline creation."
        
        static member create count (descriptorBindings : VkDescriptorSetLayoutBinding array) descriptorSetLayout descriptorPool device =

            // allocate vkDescriptorSets
            let vkDescriptorSets = DescriptorSet.allocateVkDescriptorSets count descriptorSetLayout descriptorPool device
            let vkDescriptorSets = Array.chunkBySize count vkDescriptorSets

            // create descriptor set states
            let highestBinding = Array.maxBy (fun (layoutBinding : VkDescriptorSetLayoutBinding) -> layoutBinding.binding) descriptorBindings
            let descriptorSetStates = Array.zeroCreate<DescriptorSetState> (count * Constants.Vulkan.MaxFramesInFlight)
            for i in 0 .. dec descriptorSetStates.Length do
                descriptorSetStates.[i] <- DescriptorSetState.create (int highestBinding.binding + 1)
            let descriptorSetStates = Array.chunkBySize count descriptorSetStates

            // store descriptor limits
            let descriptorLimits = Array.zeroCreate<int> (int highestBinding.binding + 1)
            for i in 0 .. dec descriptorBindings.Length do
                let binding = descriptorBindings.[i]
                descriptorLimits.[int binding.binding] <- int binding.descriptorCount
            
            // make DescriptorSet
            let descriptorSet =
                { VkDescriptorSets_ = vkDescriptorSets
                  DescriptorSetStates_ = descriptorSetStates
                  DescriptorLimits_ = descriptorLimits }
            
            // fin
            descriptorSet
    
    /// An abstraction of a rendering pipeline.
    type Pipeline =
        private
            { mutable VkPipelines_ : Map<Blend * bool, VkPipeline>
              DescriptorPool_ : VkDescriptorPool
              DescriptorSets_ : DescriptorSet array
              PipelineLayout_ : VkPipelineLayout
              DescriptorSetLayouts_ : VkDescriptorSetLayout array
              ShaderPath_ : string
              PipelineSettings_ : (Blend * bool) array
              VertexBindings_ : VkVertexInputBindingDescription array
              VertexAttributes_ : VkVertexInputAttributeDescription array
              ColorAttachmentFormats_ : VkFormat array
              DepthTestFormatOpt_ : VkFormat option
              BulkDrawLimit_ : int }

        /// The pipeline layout.
        member this.PipelineLayout = this.PipelineLayout_

        /// The maximum number of arbitrarily repeated draws, only applicable with descriptor indexing.
        member this.BulkDrawLimit = this.BulkDrawLimit_
        
        /// The descriptor set of the given number for the current frame.
        member this.VkDescriptorSet setNumber setIndex = this.DescriptorSets_.[setNumber].VkDescriptorSet setIndex
        
        /// Create the descriptor pool.
        static member private createDescriptorPool bulkDrawLimit (descriptorSetDefinitions : DescriptorSetDefinition array) (vkc : Hl.VulkanContext) =
            
            // process each descriptor set definition
            let resourceBindingsSets = Array.zeroCreate descriptorSetDefinitions.Length
            for i in 0 .. dec descriptorSetDefinitions.Length do
                resourceBindingsSets.[i] <-
                    Array.map
                        (fun binding ->
                             let bulk = if descriptorSetDefinitions.[i].BulkMode.IsBulkNone then 1 else bulkDrawLimit
                             let totalCount = bulk * binding.DescriptorCount * descriptorSetDefinitions.[i].SetCount * Constants.Vulkan.MaxFramesInFlight
                             (binding.DescriptorType.VkDescriptorType, uint totalCount))
                        descriptorSetDefinitions.[i].Descriptors
            
            // derive pool sizes
            let resourceBindings = Array.concat resourceBindingsSets
            let poolSizes = Array.zeroCreate<VkDescriptorPoolSize> resourceBindings.Length
            use poolSizesPin = new ArrayPin<_> (poolSizes)
            for i in [0 .. dec resourceBindings.Length] do
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- fst resourceBindings.[i]
                poolSize.descriptorCount <- snd resourceBindings.[i]
                poolSizes.[i] <- poolSize
            
            // count total descriptor usage and fail when hardware limit exceeded
            Hl.DescriptorsNeeded <- Hl.DescriptorsNeeded + Array.sumBy (fun x -> snd x) resourceBindings
            if Hl.DescriptorsNeeded > vkc.DescriptorIndexingProperties.maxUpdateAfterBindDescriptorsInAllPools
            then Log.fail "The current hardware cannot support the currently configured drawing maxes. Consider tuning down unneeded maxes, especially 3D drawing and light maps."
            
            // calculate total descriptor sets
            let setCount setDef = if setDef.BulkMode.IsBulkSetIndexed then setDef.SetCount * bulkDrawLimit else setDef.SetCount
            let maxSets = Array.sumBy setCount descriptorSetDefinitions * Constants.Vulkan.MaxFramesInFlight
            
            // create descriptor pool
            // NOTE: DJL: all descriptor pools should enable update after bind to avoid the *other*
            // maxes which a) would complicate calculations like above and b) may be lower. See
            // https://docs.vulkan.org/refpages/latest/refpages/source/VkPhysicalDeviceDescriptorIndexingProperties.html.
            let mutable info = VkDescriptorPoolCreateInfo ()
            info.flags <- VkDescriptorPoolCreateFlags.UpdateAfterBind
            info.maxSets <- uint maxSets
            info.poolSizeCount <- uint poolSizes.Length
            info.pPoolSizes <- poolSizesPin.Pointer
            let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
            Vulkan.vkCreateDescriptorPool (vkc.Device, &info, nullPtr, &descriptorPool) |> Hl.check
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
        
        /// Try to create the VkPipelines.
        static member private tryCreateVkPipelines
            shaderPath
            (pipelineSettings : (Blend * bool) array)
            (vertexBindings : VkVertexInputBindingDescription array)
            (vertexAttributes : VkVertexInputAttributeDescription array)
            pipelineLayout
            (colorAttachmentFormats : VkFormat array)
            depthTestFormatOpt
            device =
            
            // try to create shader modules
            let moduleResults =
                (Hl.tryCreateShaderModuleFromGlsl (shaderPath + ".vert") ShaderKind.VertexShader device,
                 Hl.tryCreateShaderModuleFromGlsl (shaderPath + ".frag") ShaderKind.FragmentShader device)

            // only proceed if shader module creation successful
            match moduleResults with
            | (Right vertModule, Right fragModule) ->

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
                match depthTestFormatOpt with
                | Some _ ->
                    dInfo.depthWriteEnable <- true
                | None -> ()

                // dynamic state info
                let dynamicStates =
                    match depthTestFormatOpt with
                    | Some _ -> [|VkDynamicState.Viewport; VkDynamicState.Scissor; VkDynamicState.DepthTestEnable; VkDynamicState.DepthCompareOp|]
                    | None -> [|VkDynamicState.Viewport; VkDynamicState.Scissor|]
                use dynamicStatesPin = new ArrayPin<_> (dynamicStates)
                let mutable dsInfo = VkPipelineDynamicStateCreateInfo ()
                dsInfo.dynamicStateCount <- uint dynamicStates.Length
                dsInfo.pDynamicStates <- dynamicStatesPin.Pointer

                // rendering info
                use colorAttachmentFormatsPin = new ArrayPin<_> (colorAttachmentFormats)
                let mutable rnInfo = VkPipelineRenderingCreateInfo ()
                rnInfo.colorAttachmentCount <- uint colorAttachmentFormats.Length
                rnInfo.pColorAttachmentFormats <- colorAttachmentFormatsPin.Pointer
                match depthTestFormatOpt with
                | Some depthTestFormat -> rnInfo.depthAttachmentFormat <- depthTestFormat
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
            
            // abort
            | (vertModuleResult, fragModuleResult) ->
                match vertModuleResult with
                | Right vertModule -> Vulkan.vkDestroyShaderModule (device, vertModule, nullPtr)
                | Left msg -> Log.warn msg
                match fragModuleResult with
                | Right fragModule -> Vulkan.vkDestroyShaderModule (device, fragModule, nullPtr)
                | Left msg -> Log.warn msg
                Log.warn "VkPipeline creation aborted."
                Map.empty
        
        static member private destroyVkPipelines pipeline (vkc : Hl.VulkanContext) =
            Map.iter (fun _ vkPipeline -> Vulkan.vkDestroyPipeline (vkc.Device, vkPipeline, nullPtr)) pipeline.VkPipelines_
        
        /// Try to get the VkPipeline built for the given settings.
        static member tryGetVkPipeline blend cullFace pipeline =
            Map.tryFind (blend, cullFace) pipeline.VkPipelines_
        
        /// Write a uniform buffer to the descriptor set. Must be used in-frame.
        static member writeDescriptorUniformBuffer setNumber (binding : int) setIndex (descriptorIndex : int) (buffer : VkBuffer) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorBuffer VkDescriptorType.UniformBuffer binding setIndex (descriptorIndex : int) buffer pipeline.DescriptorSets_.[setNumber] vkc

        /// Write a storage buffer to the descriptor set. Must be used in-frame.
        static member writeDescriptorStorageBuffer setNumber (binding : int) setIndex (descriptorIndex : int) (buffer : VkBuffer) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorBuffer VkDescriptorType.StorageBuffer binding setIndex (descriptorIndex : int) buffer pipeline.DescriptorSets_.[setNumber] vkc

        /// Write a sampled image to the descriptor set. Must be used in-frame.
        static member writeDescriptorSampledImage setNumber (binding : int) setIndex (descriptorIndex : int) (imageView : VkImageView) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorSampledImage binding setIndex descriptorIndex imageView pipeline.DescriptorSets_.[setNumber] vkc
        
        /// Write a sampler to the descriptor set. Must be used in-frame.
        static member writeDescriptorSampler setNumber (binding : int) setIndex (descriptorIndex : int) (sampler : Texture.Sampler) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorSampler binding setIndex descriptorIndex sampler pipeline.DescriptorSets_.[setNumber] vkc
        
        /// Write a combined image sampler to the descriptor set. Must be used in-frame.
        static member writeDescriptorCombinedImageSampler setNumber (binding : int) setIndex (descriptorIndex : int) (imageView : VkImageView) (sampler : Texture.Sampler) (pipeline : Pipeline) (vkc : Hl.VulkanContext) =
            DescriptorSet.writeDescriptorCombinedImageSampler binding setIndex descriptorIndex imageView sampler pipeline.DescriptorSets_.[setNumber] vkc
        
        /// Create a Pipeline.
        static member create
            shaderPath
            bulkDrawLimit
            (blends : Blend array)
            (vertexBindings : VertexBinding array)
            (descriptorSetDefinitions : DescriptorSetDefinition array)
            (pushConstants : PushConstant array)
            colorAttachmentFormats
            depthTestFormatOpt
            (vkc : Hl.VulkanContext) =
            
            // convert vertex and push constant data to vulkan objects
            let vertexBindingDescriptions = Array.map (fun (binding : VertexBinding) -> Hl.makeVertexBinding binding.Binding binding.Stride binding.InputRate ) vertexBindings
            let vertexAttributes =
                [|for i in 0 .. dec vertexBindings.Length do
                      for j in 0 .. dec vertexBindings.[i].Attributes.Length do
                          let attribute = vertexBindings.[i].Attributes.[j]
                          yield Hl.makeVertexAttribute attribute.Location vertexBindings.[i].Binding attribute.Format attribute.Offset |]
            let pushConstantRanges = Array.map (fun pushConstant -> Hl.makePushConstantRange pushConstant.Offset pushConstant.Size pushConstant.ShaderStage) pushConstants

            // create descriptor set layouts
            let layoutBindingsSets = Array.zeroCreate descriptorSetDefinitions.Length
            let descriptorSetLayouts = Array.zeroCreate descriptorSetDefinitions.Length
            for i in 0 .. dec descriptorSetDefinitions.Length do
                layoutBindingsSets.[i] <-
                    Array.map
                        (fun binding ->
                             let descriptorCount = if descriptorSetDefinitions.[i].BulkMode.IsBulkDescriptorIndexed then binding.DescriptorCount * bulkDrawLimit else binding.DescriptorCount
                             Hl.makeDescriptorBinding binding.Binding binding.DescriptorType descriptorCount binding.ShaderStage)
                        descriptorSetDefinitions.[i].Descriptors
                descriptorSetLayouts.[i] <- Pipeline.createDescriptorSetLayout descriptorSetDefinitions.[i].BulkMode.IsBulkDescriptorIndexed layoutBindingsSets.[i] vkc.Device
            
            // create descriptor pool
            let descriptorPool = Pipeline.createDescriptorPool bulkDrawLimit descriptorSetDefinitions vkc
            
            // create descriptor sets
            let descriptorSets = Array.zeroCreate descriptorSetDefinitions.Length
            for i in 0 .. dec descriptorSetDefinitions.Length do
                let setCount = if descriptorSetDefinitions.[i].BulkMode.IsBulkSetIndexed then descriptorSetDefinitions.[i].SetCount * bulkDrawLimit else descriptorSetDefinitions.[i].SetCount
                descriptorSets.[i] <- DescriptorSet.create setCount layoutBindingsSets.[i] descriptorSetLayouts.[i] descriptorPool vkc.Device
            
            // create pipeline layout and vkPipelines
            if blends.Length < 1 then Log.fail "No pipeline blend was specified."
            let pipelineSettings = Array.allPairs blends [|false; true|] // blend and cull modes
            let pipelineLayout = Pipeline.createPipelineLayout descriptorSetLayouts pushConstantRanges vkc.Device
            let vkPipelines = Pipeline.tryCreateVkPipelines shaderPath pipelineSettings vertexBindingDescriptions vertexAttributes pipelineLayout colorAttachmentFormats depthTestFormatOpt vkc.Device
            
            // make Pipeline
            let pipeline =
                { VkPipelines_ = vkPipelines
                  DescriptorPool_ = descriptorPool
                  DescriptorSets_ = descriptorSets
                  PipelineLayout_ = pipelineLayout
                  DescriptorSetLayouts_ = descriptorSetLayouts
                  ShaderPath_ = shaderPath
                  PipelineSettings_ = pipelineSettings
                  VertexBindings_ = vertexBindingDescriptions
                  VertexAttributes_ = vertexAttributes
                  ColorAttachmentFormats_ = colorAttachmentFormats
                  DepthTestFormatOpt_ = depthTestFormatOpt
                  BulkDrawLimit_ = bulkDrawLimit }

            // fin
            pipeline
        
        /// Try to recreate VkPipelines with updated shaders.
        static member reloadShaders pipeline (vkc : Hl.VulkanContext) =
            Hl.Queue.waitIdle vkc.RenderQueue // VkPipeline may still be in use by previous frame
            Pipeline.destroyVkPipelines pipeline vkc
            pipeline.VkPipelines_ <-
                Pipeline.tryCreateVkPipelines
                    pipeline.ShaderPath_
                    pipeline.PipelineSettings_
                    pipeline.VertexBindings_
                    pipeline.VertexAttributes_
                    pipeline.PipelineLayout_
                    pipeline.ColorAttachmentFormats_
                    pipeline.DepthTestFormatOpt_
                    vkc.Device
        
        /// Destroy a Pipeline.
        static member destroy pipeline (vkc : Hl.VulkanContext) =
            Pipeline.destroyVkPipelines pipeline vkc
            Vulkan.vkDestroyDescriptorPool (vkc.Device, pipeline.DescriptorPool_, nullPtr)
            Vulkan.vkDestroyPipelineLayout (vkc.Device, pipeline.PipelineLayout, nullPtr)
            for i in 0 .. dec pipeline.DescriptorSetLayouts_.Length do Vulkan.vkDestroyDescriptorSetLayout (vkc.Device, pipeline.DescriptorSetLayouts_.[i], nullPtr)
