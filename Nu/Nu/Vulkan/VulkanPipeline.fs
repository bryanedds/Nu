// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.Diagnostics
open FSharp.NativeInterop
open Vortice.ShaderCompiler
open Prime
open Vortice.Vulkan
open Nu

/// A blend setting for a Vulkan pipeline.
/// TODO: P1: review naming.
type VulkanBlend =
    | VulkanUnblended
    | VulkanTransparent
    | VulkanAdditive
    | VulkanOverwrite
    | VulkanImGui

    /// Make blend attachment.
    static member makeAttachment blend =
        match blend with
        | VulkanUnblended ->
            Hl.makeBlendAttachment None
        | VulkanTransparent ->
            Hl.makeBlendAttachment
                (Some
                    (VkBlendFactor.SrcAlpha, VkBlendFactor.OneMinusSrcAlpha,
                     VkBlendFactor.One, VkBlendFactor.Zero))
        | VulkanAdditive ->
            Hl.makeBlendAttachment
                (Some
                    (VkBlendFactor.SrcAlpha, VkBlendFactor.One,
                     VkBlendFactor.One, VkBlendFactor.Zero))
        | VulkanOverwrite ->
            Hl.makeBlendAttachment
                (Some
                    (VkBlendFactor.One, VkBlendFactor.Zero,
                     VkBlendFactor.One, VkBlendFactor.Zero))
        | VulkanImGui ->
            Hl.makeBlendAttachment
                (Some
                    (VkBlendFactor.SrcAlpha, VkBlendFactor.OneMinusSrcAlpha,
                     VkBlendFactor.One, VkBlendFactor.OneMinusSrcAlpha))

/// Describes a vertex attribute in the context of a vertex binding.
type VertexAttribute =
    { Location : int
      Format : VertexAttribFormat
      Offset : int }

/// Describes a binding for a vertex and its attributes.
type VertexBinding =
    { Binding : int
      Stride : int
      InputRate : VkVertexInputRate
      Attributes : VertexAttribute array }

/// Describes a binding for a resource descriptor (aka uniform).
type DescriptorBinding =
    { Binding : int
      DescriptorType : DescriptorType
      ShaderStage : ShaderStage
      DescriptorCount : int }

type DescriptorSet =
    interface
        abstract BeginFrame : unit -> unit
        abstract Specify : obj -> VulkanContext -> (VkDescriptorSet -> unit) -> VkDescriptorSet // TODO: P0: attempt to get rid of boxing here!
        abstract Destroy : VulkanContext -> unit
        end

and DescriptorSet<'k when 'k : equality> =
    private
        { DescriptorSetDefinition_ : DescriptorSetDefinition
          VkDescriptorSetLayout_ : VkDescriptorSetLayout
          VkDescriptorPools_ : VkDescriptorPool List
          VkDescriptorSets_ : Dictionary<'k, VkDescriptorSet> array
          mutable VkDescriptorSetsAvailable_ : VkDescriptorSet Queue array }

    static member private createDescriptorPool (capacity : int) (descriptorSetDefinition : DescriptorSetDefinition) (vkc : VulkanContext) =

        // derive pool sizes merged by descriptor type
        let poolSizes =
            descriptorSetDefinition.DescriptorBindings
            |> Array.groupBy (fun b -> b.DescriptorType.VkDescriptorType) 
            |> Array.map (fun (bindingType, bindings) ->
                let totalCount = Array.sumBy (fun b -> b.DescriptorCount) bindings
                let mutable poolSize = VkDescriptorPoolSize ()
                poolSize.``type`` <- bindingType
                poolSize.descriptorCount <- uint (totalCount * capacity)
                poolSize)
        use poolSizesPin = new ArrayPin<_> (poolSizes)

        // create descriptor pool
        let mutable info = VkDescriptorPoolCreateInfo ()
        info.maxSets <- uint capacity
        info.poolSizeCount <- uint poolSizes.Length
        info.pPoolSizes <- poolSizesPin.Pointer
        let mutable descriptorPool = Unchecked.defaultof<VkDescriptorPool>
        Vulkan.vkCreateDescriptorPool (vkc.Device, &info, nullPtr, &descriptorPool) |> Hl.check
        descriptorPool

    static member private allocateVkDescriptorSets capacity descriptorSetDefinitions descriptorSetLayout vkc =
        let vkDescriptorPool = DescriptorSet.createDescriptorPool (capacity * Constants.Vulkan.MaxFramesInFlight) descriptorSetDefinitions vkc
        let vkDescriptorSetLayouts = Array.create<VkDescriptorSetLayout> (capacity * Constants.Vulkan.MaxFramesInFlight) descriptorSetLayout
        use vkDescriptorSetLayoutsPin = new ArrayPin<_> (vkDescriptorSetLayouts)
        let mutable info = VkDescriptorSetAllocateInfo ()
        info.descriptorPool <- vkDescriptorPool
        info.descriptorSetCount <- uint vkDescriptorSetLayouts.Length
        info.pSetLayouts <- vkDescriptorSetLayoutsPin.Pointer
        let vkDescriptorSets = Array.zeroCreate<VkDescriptorSet> (capacity * Constants.Vulkan.MaxFramesInFlight)
        use vkDescriptorSetsPin = new ArrayPin<_> (vkDescriptorSets)
        Vulkan.vkAllocateDescriptorSets (vkc.Device, asPointer &info, vkDescriptorSetsPin.Pointer) |> Hl.check
        let vkDescriptorSets = vkDescriptorSets |> Array.chunkBySize capacity |> Array.map Queue
        (vkDescriptorPool, vkDescriptorSets)

    static member create<'a when 'a : equality> capacity (descriptorSetDefinition : 'a DescriptorSetDefinition) vkDescriptorSetLayout (vkc : VulkanContext) : 'a DescriptorSet =

        // allocate pool and use its descriptor sets
        let (vkDescriptorPool, vkDescriptorSets) =
            DescriptorSet<_>.allocateVkDescriptorSets capacity descriptorSetDefinition vkDescriptorSetLayout vkc

        // make DescriptorSet
        let descriptorSet =
            { DescriptorSetDefinition_ = descriptorSetDefinition
              VkDescriptorSetLayout_ = vkDescriptorSetLayout
              VkDescriptorPools_ = List [vkDescriptorPool]
              VkDescriptorSets_ = Array.init Constants.Vulkan.MaxFramesInFlight (fun _ -> dictPlus<'a, VkDescriptorSet> HashIdentity.Structural [])
              VkDescriptorSetsAvailable_ = vkDescriptorSets }

        // fin
        descriptorSet

    interface DescriptorSet with

        member this.BeginFrame () =
            for entry in this.VkDescriptorSets_[Hl.CurrentFrame] do
                this.VkDescriptorSetsAvailable_[Hl.CurrentFrame].Enqueue entry.Value
            this.VkDescriptorSets_[Hl.CurrentFrame].Clear ()

        member this.Specify (keyObj : obj) (vkc : VulkanContext) (specify : VkDescriptorSet -> unit) : VkDescriptorSet =
            let key = keyObj :?> 'k
            match this.VkDescriptorSets_[Hl.CurrentFrame].TryGetValue key with
            | (false, _) ->
                let mutable vkDescriptorSet = Unchecked.defaultof<_>
                let found = this.VkDescriptorSetsAvailable_[Hl.CurrentFrame].TryDequeue &vkDescriptorSet
                if not found then
                    let count = this.VkDescriptorSets_[Hl.CurrentFrame].Count
                    let (vkDescriptorPool, vkDescriptorSets) =
                        DescriptorSet<_>.allocateVkDescriptorSets count this.DescriptorSetDefinition_ this.VkDescriptorSetLayout_ vkc
                    this.VkDescriptorPools_.Add vkDescriptorPool
                    this.VkDescriptorSetsAvailable_ <- vkDescriptorSets
                    vkDescriptorSet <- this.VkDescriptorSetsAvailable_[Hl.CurrentFrame].Dequeue ()
                this.VkDescriptorSets_[Hl.CurrentFrame].Add (key, vkDescriptorSet)
                specify vkDescriptorSet
                vkDescriptorSet
            | (true, vkDescriptorSet) -> vkDescriptorSet

        member this.Destroy vkc =
            for pool in this.VkDescriptorPools_ do Vulkan.vkDestroyDescriptorPool (vkc.Device, pool, nullPtr)

and DescriptorSetDefinition =
    interface
        abstract DescriptorBindings : DescriptorBinding array
        abstract CreateDescriptorSet : VkDescriptorSetLayout -> VulkanContext -> DescriptorSet
        end

/// Describes a descriptor set.
and DescriptorSetDefinition<'k when 'k : equality> =
    { DescriptorBindings : DescriptorBinding array }
    interface DescriptorSetDefinition with
        member this.DescriptorBindings = this.DescriptorBindings
        member this.CreateDescriptorSet layout vkc = DescriptorSet<_>.create<'k> Constants.Vulkan.DescriptorSetCountDefault this layout vkc

/// Describes a push constant.
type PushConstant =
    { Offset : int
      Size : int
      ShaderStage : ShaderStage }
    
/// An abstraction of a rendering pipeline.
type Pipeline =
    private
        { mutable VkPipelines_ : Map<VulkanBlend * bool, VkPipeline> // TODO: P0: make sure no allocation happens on look-up.
          Buffers_ : Nu.Vulkan.Buffer array
          DescriptorSets_ : DescriptorSet array
          VkPipelineLayout_ : VkPipelineLayout
          VkDescriptorSetLayouts_ : VkDescriptorSetLayout array
          ShaderPath_ : string
          PipelineSettings_ : (VulkanBlend * bool) array
          VkVertexBindings_ : VkVertexInputBindingDescription array
          VkVertexAttributes_ : VkVertexInputAttributeDescription array
          VkColorAttachmentFormats_ : VkFormat array
          VkDepthTestFormatOpt_ : VkFormat option
          mutable DrawIndex_ : int }

    /// The pipeline layout.
    member this.PipelineLayout = this.VkPipelineLayout_

    /// The current draw index.
    member this.DrawIndex = this.DrawIndex_

    /// Begin use of the pipeline this frame.
    static member beginFrame pipeline =
        for buffer in pipeline.Buffers_ do Buffer.beginFrame buffer
        for set in pipeline.DescriptorSets_ do set.BeginFrame ()
        pipeline.DrawIndex_ <- 0

    /// Advance the state of the pipeline for additional drawing.
    static member advance drawInstances pipeline =
        pipeline.DrawIndex_ <- inc pipeline.DrawIndex_
        Hl.reportDrawCall drawInstances

    /// The descriptor set of the given number for the current frame.
    static member getDescriptorSet set pipeline =
        pipeline.DescriptorSets_[set]

    /// Create the descriptor set layout.
    static member private createDescriptorSetLayout (resourceBindings : VkDescriptorSetLayoutBinding array) (vkc : VulkanContext) =
        use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
        let mutable info = VkDescriptorSetLayoutCreateInfo ()
        info.bindingCount <- uint resourceBindings.Length
        info.pBindings <- resourceBindingsPin.Pointer
        let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
        Vulkan.vkCreateDescriptorSetLayout (vkc.Device, &info, nullPtr, &descriptorSetLayout) |> Hl.check
        descriptorSetLayout

    /// Create the pipeline layout.
    static member private createVkPipelineLayout (descriptorSetLayouts : VkDescriptorSetLayout array) (pushConstantRanges : VkPushConstantRange array) (vkc : VulkanContext) =
        use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
        use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
        let mutable info = VkPipelineLayoutCreateInfo ()
        info.setLayoutCount <- uint descriptorSetLayouts.Length
        info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
        info.pushConstantRangeCount <- uint pushConstantRanges.Length
        info.pPushConstantRanges <- pushConstantRangesPin.Pointer
        let mutable vkPipelineLayout = Unchecked.defaultof<VkPipelineLayout>
        Vulkan.vkCreatePipelineLayout (vkc.Device, &info, nullPtr, &vkPipelineLayout) |> Hl.check
        vkPipelineLayout
    
    /// Try to create the VkPipelines.
    static member private tryCreateVkPipelines
        shaderPath
        (pipelineSettings : (VulkanBlend * bool) array)
        (vertexBindings : VkVertexInputBindingDescription array)
        (vertexAttributes : VkVertexInputAttributeDescription array)
        pipelineLayout
        (colorAttachmentFormats : VkFormat array)
        depthTestFormatOpt
        (vkc : VulkanContext) =
        
        // try to create shader modules
        let moduleResults =
            (Hl.tryCreateShaderModuleFromGlsl (shaderPath + ".vert") ShaderKind.VertexShader vkc.Device,
             Hl.tryCreateShaderModuleFromGlsl (shaderPath + ".frag") ShaderKind.FragmentShader vkc.Device)

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
            
            // pipeline create infos
            let blendStates = NativePtr.stackalloc<VkPipelineColorBlendAttachmentState> (pipelineSettings.Length * colorAttachmentFormats.Length)
            let bInfos = NativePtr.stackalloc<VkPipelineColorBlendStateCreateInfo> pipelineSettings.Length
            let rInfos = NativePtr.stackalloc<VkPipelineRasterizationStateCreateInfo> pipelineSettings.Length
            let infos = NativePtr.stackalloc<VkGraphicsPipelineCreateInfo> pipelineSettings.Length
            for i in 0 .. dec pipelineSettings.Length do
            
                // extract settings
                let (blend, cullFace) = pipelineSettings[i]
                
                // blend info (specifying blend state for each color attachment)
                let blendState = VulkanBlend.makeAttachment blend
                for j in 0 .. dec colorAttachmentFormats.Length do NativePtr.set blendStates (i * colorAttachmentFormats.Length + j) blendState
                let mutable bInfo = VkPipelineColorBlendStateCreateInfo ()
                bInfo.attachmentCount <- uint colorAttachmentFormats.Length
                bInfo.pAttachments <- NativePtr.add blendStates (i * colorAttachmentFormats.Length)
                NativePtr.set bInfos i bInfo

                // cull mode
                rInfo.cullMode <- if cullFace then VkCullModeFlags.Back else VkCullModeFlags.None
                NativePtr.set rInfos i rInfo

                // create info
                let mutable info = VkGraphicsPipelineCreateInfo ()
                info.pNext <- asVoidPtr &rnInfo
                info.stageCount <- uint ssInfos.Length
                info.pStages <- ssInfosPin.Pointer
                info.pVertexInputState <- asPointer &viInfo
                info.pInputAssemblyState <- asPointer &iaInfo
                info.pViewportState <- asPointer &vInfo
                info.pRasterizationState <- NativePtr.add rInfos i
                info.pMultisampleState <- asPointer &mInfo
                info.pDepthStencilState <- asPointer &dInfo
                info.pColorBlendState <- NativePtr.add bInfos i
                info.pDynamicState <- asPointer &dsInfo
                info.layout <- pipelineLayout
                info.renderPass <- VkRenderPass.Null
                info.subpass <- 0u
                NativePtr.set infos i info
                
            // create vulkan pipelines
            // TODO: DJL: consider pipeline cache.
            let vkPipelines = Array.zeroCreate<VkPipeline> pipelineSettings.Length
            use vkPipelinesPin = new ArrayPin<_> (vkPipelines)
            Vulkan.vkCreateGraphicsPipelines (vkc.Device, VkPipelineCache.Null, uint vkPipelines.Length, infos, nullPtr, vkPipelinesPin.Pointer) |> Hl.check
            
            // destroy shader modules
            Vulkan.vkDestroyShaderModule (vkc.Device, vertModule, nullPtr)
            Vulkan.vkDestroyShaderModule (vkc.Device, fragModule, nullPtr)
            
            // pack vulkan pipelines with settings
            let vkPipelinesPacked = Array.zip pipelineSettings vkPipelines |> Map.ofArray
            vkPipelinesPacked
        
        // abort
        | (vertModuleResult, fragModuleResult) ->
            match vertModuleResult with
            | Right vertModule -> Vulkan.vkDestroyShaderModule (vkc.Device, vertModule, nullPtr)
            | Left msg -> Log.warn msg
            match fragModuleResult with
            | Right fragModule -> Vulkan.vkDestroyShaderModule (vkc.Device, fragModule, nullPtr)
            | Left msg -> Log.warn msg
            Log.warn "VkPipeline creation aborted."
            Map.empty

    static member private destroyVkPipelines pipeline (vkc : VulkanContext) =
        Map.iter (fun _ vkPipeline -> Vulkan.vkDestroyPipeline (vkc.Device, vkPipeline, nullPtr)) pipeline.VkPipelines_
    
    /// Try to get the VkPipeline built for the given settings.
    static member tryGetVkPipeline blend cullFace pipeline =
        Map.tryFind (blend, cullFace) pipeline.VkPipelines_

    static member writeDescriptorStorageBuffer (binding : int) (descriptorIndex : int) (buffer : Nu.Vulkan.Buffer) vkDescriptorSet (vkc : VulkanContext) =

        // buffer info
        let mutable info = VkDescriptorBufferInfo ()
        info.buffer <- buffer.VkBuffer
        info.range <- Vulkan.VK_WHOLE_SIZE

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.StorageBuffer
        write.pBufferInfo <- asPointer &info
        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

        // advance buffer
        Buffer.advance buffer

    static member writeDescriptorSampledImage (binding : int) (descriptorIndex : int) (texture : Texture) vkDescriptorSet (vkc : VulkanContext) =

        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.imageView <- texture.ImageView
        info.imageLayout <- ShaderRead.VkImageLayout

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.SampledImage
        write.pImageInfo <- asPointer &info
        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

    static member writeDescriptorSampledImages (binding : int) (descriptorIndex : int) (textures : Texture array) vkDescriptorSet (vkc : VulkanContext) =

        // image infos
        let infosPtr = NativePtr.stackalloc<VkDescriptorImageInfo> textures.Length
        for i in 0 .. dec textures.Length do
            let mutable info = VkDescriptorImageInfo ()
            info.imageView <- textures[i].ImageView
            info.imageLayout <- ShaderRead.VkImageLayout
            NativePtr.set infosPtr i info

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- uint textures.Length
        write.descriptorType <- VkDescriptorType.SampledImage
        write.pImageInfo <- infosPtr
        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

    static member writeDescriptorSampler (binding : int) (descriptorIndex : int) (sampler : Sampler) vkDescriptorSet (vkc : VulkanContext) =
        
        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.sampler <- sampler.VkSampler

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.Sampler
        write.pImageInfo <- asPointer &info
        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)
    
    static member writeDescriptorCombinedImageSampler (binding : int) (descriptorIndex : int) (texture : Texture) (sampler : Sampler) vkDescriptorSet (vkc : VulkanContext) =

        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.sampler <- sampler.VkSampler
        info.imageView <- texture.ImageView
        info.imageLayout <- ShaderRead.VkImageLayout

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.CombinedImageSampler
        write.pImageInfo <- asPointer &info
        Vulkan.vkUpdateDescriptorSets (vkc.Device, 1u, asPointer &write, 0u, nullPtr)

    /// Describes a vertex attribute in the context of a vertex binding.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member attribute location format offset =
        { Location = location
          Format = format
          Offset = offset }

    /// Describes a binding for a vertex and its attributes.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member vertex binding stride inputRate attributes =
        { Binding = binding
          Stride = stride
          InputRate = inputRate
          Attributes = attributes }

    /// Describes a binding for a resource descriptor (aka uniform).
    [<DebuggerHidden; DebuggerStepThrough>]
    static member descriptor binding descriptorType shaderStage descriptorCount =
        { Binding = binding
          DescriptorType = descriptorType
          ShaderStage = shaderStage
          DescriptorCount = descriptorCount }

    /// Describes a descriptor set.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member descriptorSet<'k when 'k : equality> descriptorBindings : 'k DescriptorSetDefinition =
#if DEBUG
        let ty = typeof<'k>
        if ty = typeof<obj> then failwith "Unexpected key type 'obj'. You probably forgot to explicitly specify descriptorSet type!"
        if ty = typeof<unit> then failwith "Unexpected key type 'unit'. You have to use the 'Unit' type instead since null semantics make 'unit' unusable as a key."
#endif
        { DescriptorBindings = descriptorBindings }
    
    /// Describes a push constant.
    [<DebuggerHidden; DebuggerStepThrough>]
    static member pushConstant offset size shaderStage =
        { Offset = offset
          Size = size
          ShaderStage = shaderStage }
    
    /// Convert DepthTest to VkCompareOp.
    /// TODO: BGE: since this is the odd one out in terms of being the only function directly in this module that is
    /// not an operator, perhaps it should either be privatized or moved elsewhere?
    static member depthTestToVkCompareOp depthTest =
        match depthTest with
        | LessThanTest -> VkCompareOp.Less
        | LessThanOrEqualTest -> VkCompareOp.LessOrEqual
        | EqualTest -> VkCompareOp.Equal
        | GreaterThanOrEqualTest -> VkCompareOp.GreaterOrEqual
        | GreaterThanTest -> VkCompareOp.Greater
        | NeverPassTest -> VkCompareOp.Never
        | AlwaysPassTest -> VkCompareOp.Always

    /// Specify a descriptor set.
    static member specifyDescriptorSet<'k when 'k : equality> set (key : 'k) pipeline vkc specify =
        let descriptorSet = Pipeline.getDescriptorSet set pipeline
        descriptorSet.Specify key vkc specify

    /// Try to recreate VkPipelines with updated shaders.
    static member reloadShaders pipeline (vkc : VulkanContext) =
        CommandQueue.waitIdle vkc.RenderQueue // VkPipeline may still be in use by previous frame
        Pipeline.destroyVkPipelines pipeline vkc
        pipeline.VkPipelines_ <-
            Pipeline.tryCreateVkPipelines
                pipeline.ShaderPath_
                pipeline.PipelineSettings_
                pipeline.VkVertexBindings_
                pipeline.VkVertexAttributes_
                pipeline.VkPipelineLayout_
                pipeline.VkColorAttachmentFormats_
                pipeline.VkDepthTestFormatOpt_
                vkc

    /// Create a Pipeline.
    static member create<'k when 'k : equality>
        shaderPath
        (blends : VulkanBlend array)
        (cullModes : bool array)
        (vertexBindings : VertexBinding array)
        (descriptorSetDefinitions : DescriptorSetDefinition array)
        (pushConstants : PushConstant array)
        colorAttachmentFormats
        depthTestFormatOpt
        buffers
        (vkc : VulkanContext) =
        
        // convert vertex and push constant data to vulkan objects
        let vertexBindingDescriptions = Array.map (fun (binding : VertexBinding) -> Hl.makeVertexBinding binding.Binding binding.Stride binding.InputRate ) vertexBindings
        let vertexAttributes =
            [|for i in 0 .. dec vertexBindings.Length do
                  for j in 0 .. dec vertexBindings[i].Attributes.Length do
                      let attribute = vertexBindings[i].Attributes[j]
                      yield Hl.makeVertexAttribute attribute.Location vertexBindings[i].Binding attribute.Format attribute.Offset |]
        let pushConstantRanges = Array.map (fun pushConstant -> Hl.makePushConstantRange pushConstant.Offset pushConstant.Size pushConstant.ShaderStage) pushConstants

        // create descriptor set layouts
        let layoutBindingsSets = Array.zeroCreate descriptorSetDefinitions.Length
        let descriptorSetLayouts = Array.zeroCreate descriptorSetDefinitions.Length
        for i in 0 .. dec descriptorSetDefinitions.Length do
            layoutBindingsSets[i] <-
                descriptorSetDefinitions[i].DescriptorBindings
                |> Array.map (fun binding -> Hl.makeDescriptorBinding binding.Binding binding.DescriptorType binding.DescriptorCount binding.ShaderStage)
            descriptorSetLayouts[i] <- Pipeline.createDescriptorSetLayout layoutBindingsSets[i] vkc
        
        // create descriptor sets
        let descriptorSets = Array.zeroCreate descriptorSetDefinitions.Length
        for i in 0 .. dec descriptorSetDefinitions.Length do
            let definition = descriptorSetDefinitions[i]
            descriptorSets[i] <- definition.CreateDescriptorSet descriptorSetLayouts[i] vkc
        
        // create pipeline layout and vkPipelines
        if blends.Length < 1 then Log.fail "No pipeline blend was specified."
        let pipelineSettings = Array.allPairs blends cullModes
        let vkPipelineLayout = Pipeline.createVkPipelineLayout descriptorSetLayouts pushConstantRanges vkc
        let vkPipelines = Pipeline.tryCreateVkPipelines shaderPath pipelineSettings vertexBindingDescriptions vertexAttributes vkPipelineLayout colorAttachmentFormats depthTestFormatOpt vkc
        
        // make Pipeline
        let pipeline =
            { VkPipelines_ = vkPipelines
              Buffers_ = buffers
              DescriptorSets_ = descriptorSets
              VkPipelineLayout_ = vkPipelineLayout
              VkDescriptorSetLayouts_ = descriptorSetLayouts
              ShaderPath_ = shaderPath
              PipelineSettings_ = pipelineSettings
              VkVertexBindings_ = vertexBindingDescriptions
              VkVertexAttributes_ = vertexAttributes
              VkColorAttachmentFormats_ = colorAttachmentFormats
              VkDepthTestFormatOpt_ = depthTestFormatOpt
              DrawIndex_ = 0 }

        // fin
        pipeline
    
    /// Destroy a Pipeline.
    static member destroy pipeline (vkc : VulkanContext) =
        Pipeline.destroyVkPipelines pipeline vkc
        Vulkan.vkDestroyPipelineLayout (vkc.Device, pipeline.PipelineLayout, nullPtr)
        for vkLayout in pipeline.VkDescriptorSetLayouts_ do Vulkan.vkDestroyDescriptorSetLayout (vkc.Device, vkLayout, nullPtr)
        for buffer in pipeline.Buffers_ do Buffer.destroy buffer vkc
        for set in pipeline.DescriptorSets_ do set.Destroy vkc