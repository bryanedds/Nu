// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

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
/// NOTE: this type's name is prefixed with "Vulkan" in order to reliably disambiguate it from Nu.Blend.
type VulkanBlend =
    | VulkanUnblended
    | VulkanTransparent
    | VulkanAdditive
    | VulkanSummation
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
        | VulkanSummation ->
            Hl.makeBlendAttachment
                (Some
                    (VkBlendFactor.One, VkBlendFactor.One,
                     VkBlendFactor.One, VkBlendFactor.One))
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
        abstract Specify : obj -> (VkDescriptorSet -> unit) -> VkDescriptorSet // TODO: P0: attempt to get rid of boxing here!
        abstract Destroy : unit -> unit
        end

and DescriptorSet<'k when 'k : equality> =
    private
        { DescriptorSetDefinition_ : DescriptorSetDefinition
          VkDescriptorSetLayout_ : VkDescriptorSetLayout
          VkDescriptorPools_ : VkDescriptorPool List
          VkDescriptorSets_ : Dictionary<'k, VkDescriptorSet>
          mutable VkDescriptorSetsAvailable_ : VkDescriptorSet Queue }

    static member private createDescriptorPool (capacity : int) (descriptorSetDefinition : DescriptorSetDefinition) =

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
        DeviceApi.vkCreateDescriptorPool (&info, nullPtr, &descriptorPool) |> Hl.check
        descriptorPool

    static member private allocateVkDescriptorSets capacity descriptorSetDefinitions descriptorSetLayout =
        let vkDescriptorPool = DescriptorSet.createDescriptorPool capacity descriptorSetDefinitions
        let vkDescriptorSetLayouts = Array.create<VkDescriptorSetLayout> capacity descriptorSetLayout
        use vkDescriptorSetLayoutsPin = new ArrayPin<_> (vkDescriptorSetLayouts)
        let mutable info = VkDescriptorSetAllocateInfo ()
        info.descriptorPool <- vkDescriptorPool
        info.descriptorSetCount <- uint vkDescriptorSetLayouts.Length
        info.pSetLayouts <- vkDescriptorSetLayoutsPin.Pointer
        let vkDescriptorSets = Array.zeroCreate<VkDescriptorSet> capacity
        use vkDescriptorSetsPin = new ArrayPin<_> (vkDescriptorSets)
        DeviceApi.vkAllocateDescriptorSets (&&info, vkDescriptorSetsPin.Pointer) |> Hl.check
        (vkDescriptorPool, Queue vkDescriptorSets)

    static member create<'a when 'a : equality> capacity (descriptorSetDefinition : 'a DescriptorSetDefinition) vkDescriptorSetLayout : 'a DescriptorSet =

        // allocate pool and use its descriptor sets
        let (vkDescriptorPool, vkDescriptorSets) =
            DescriptorSet<_>.allocateVkDescriptorSets capacity descriptorSetDefinition vkDescriptorSetLayout

        // make DescriptorSet
        let descriptorSet =
            { DescriptorSetDefinition_ = descriptorSetDefinition
              VkDescriptorSetLayout_ = vkDescriptorSetLayout
              VkDescriptorPools_ = List [vkDescriptorPool]
              VkDescriptorSets_ = dictPlus<'a, VkDescriptorSet> HashIdentity.Structural []
              VkDescriptorSetsAvailable_ = vkDescriptorSets }

        // fin
        descriptorSet

    interface DescriptorSet with

        member this.BeginFrame () =
            for entry in this.VkDescriptorSets_ do
                this.VkDescriptorSetsAvailable_.Enqueue entry.Value
            this.VkDescriptorSets_.Clear ()

        member this.Specify (keyObj : obj) (specify : VkDescriptorSet -> unit) : VkDescriptorSet =
            let key = keyObj :?> 'k
            match this.VkDescriptorSets_.TryGetValue key with
            | (false, _) ->
                let mutable vkDescriptorSet = Unchecked.defaultof<_>
                let found = this.VkDescriptorSetsAvailable_.TryDequeue &vkDescriptorSet
                if not found then
                    let count = this.VkDescriptorSets_.Count
                    let (vkDescriptorPool, vkDescriptorSets) =
                        DescriptorSet<_>.allocateVkDescriptorSets count this.DescriptorSetDefinition_ this.VkDescriptorSetLayout_
                    this.VkDescriptorPools_.Add vkDescriptorPool
                    this.VkDescriptorSetsAvailable_ <- vkDescriptorSets
                    vkDescriptorSet <- this.VkDescriptorSetsAvailable_.Dequeue ()
                this.VkDescriptorSets_.Add (key, vkDescriptorSet)
                specify vkDescriptorSet
                vkDescriptorSet
            | (true, vkDescriptorSet) -> vkDescriptorSet

        member this.Destroy () =
            for pool in this.VkDescriptorPools_ do
                DeviceApi.vkDestroyDescriptorPool (pool, nullPtr)

and DescriptorSetDefinition =
    interface
        abstract DescriptorBindings : DescriptorBinding array
        abstract CreateDescriptorSet : VkDescriptorSetLayout -> DescriptorSet
        end

/// Describes a descriptor set.
and DescriptorSetDefinition<'k when 'k : equality> =
    { DescriptorBindings : DescriptorBinding array }
    interface DescriptorSetDefinition with
        member this.DescriptorBindings = this.DescriptorBindings
        member this.CreateDescriptorSet layout = DescriptorSet<_>.create<'k> Constants.Vulkan.DescriptorSetCountDefault this layout

/// Describes a push constant.
type PushConstant =
    { Offset : int
      Size : int
      ShaderStage : ShaderStage }

/// An abstraction of a rendering pipeline.
type Pipeline =
    private
        { Buffers_ : VulkanBuffer array
          DescriptorSets_ : DescriptorSet array
          VkPipelineLayout_ : VkPipelineLayout
          VkDescriptorSetLayouts_ : VkDescriptorSetLayout array
          ShaderPathVert_ : string
          ShaderPathFrag_ : string
          PipelineSettings_ : (VulkanBlend * bool) array
          VkVertexBindings_ : VkVertexInputBindingDescription array
          VkVertexAttributes_ : VkVertexInputAttributeDescription array
          VkColorAttachmentFormats_ : VkFormat array
          VkDepthTestFormatOpt_ : VkFormat option
          mutable VkPipelines_ : Dictionary<VulkanBlend * bool, VkPipeline> // TODO: P0: make sure no allocation happens on look-up.
          mutable DrawIndex_ : int }

    /// The pipeline layout.
    member this.PipelineLayout = this.VkPipelineLayout_

    /// The current draw index.
    member this.DrawIndex = this.DrawIndex_

    /// Begin use of the pipeline this frame.
    static member beginFrame pipeline =
        for buffer in pipeline.Buffers_ do VulkanBuffer.beginFrame buffer
        for set in pipeline.DescriptorSets_ do set.BeginFrame ()
        pipeline.DrawIndex_ <- 0

    /// Advance the state of the pipeline for additional drawing.
    static member advance pipeline =
        pipeline.DrawIndex_ <- inc pipeline.DrawIndex_

    /// The descriptor set of the given number for the current frame.
    static member getDescriptorSet set pipeline =
        pipeline.DescriptorSets_[set]

    /// Create the descriptor set layout.
    static member private createDescriptorSetLayout (resourceBindings : VkDescriptorSetLayoutBinding array) =
        use resourceBindingsPin = new ArrayPin<_> (resourceBindings)
        let mutable info = VkDescriptorSetLayoutCreateInfo ()
        info.bindingCount <- uint resourceBindings.Length
        info.pBindings <- resourceBindingsPin.Pointer
        let mutable descriptorSetLayout = Unchecked.defaultof<VkDescriptorSetLayout>
        DeviceApi.vkCreateDescriptorSetLayout (&info, nullPtr, &descriptorSetLayout) |> Hl.check
        descriptorSetLayout

    /// Create the pipeline layout.
    static member private createVkPipelineLayout (descriptorSetLayouts : VkDescriptorSetLayout array) (pushConstantRanges : VkPushConstantRange array) =
        use descriptorSetLayoutsPin = new ArrayPin<_> (descriptorSetLayouts)
        use pushConstantRangesPin = new ArrayPin<_> (pushConstantRanges)
        let mutable info = VkPipelineLayoutCreateInfo ()
        info.setLayoutCount <- uint descriptorSetLayouts.Length
        info.pSetLayouts <- descriptorSetLayoutsPin.Pointer
        info.pushConstantRangeCount <- uint pushConstantRanges.Length
        info.pPushConstantRanges <- pushConstantRangesPin.Pointer
        let mutable vkPipelineLayout = Unchecked.defaultof<VkPipelineLayout>
        DeviceApi.vkCreatePipelineLayout (&info, nullPtr, &vkPipelineLayout) |> Hl.check
        vkPipelineLayout

    /// Try to create vert and frag VkPipelines.
    static member private tryCreateVertAndFragPipelines
        shaderPathVert
        shaderPathFrag
        (pipelineSettings : (VulkanBlend * bool) array)
        (vertexBindings : VkVertexInputBindingDescription array)
        (vertexAttributes : VkVertexInputAttributeDescription array)
        pipelineLayout
        (colorAttachmentFormats : VkFormat array)
        depthTestFormatOpt =
        
        // try to create shader modules
        let moduleResults =
            (Hl.tryCreateShaderModuleFromGlsl shaderPathVert ShaderKind.VertexShader,
             Hl.tryCreateShaderModuleFromGlsl shaderPathFrag ShaderKind.FragmentShader)

        // only proceed if shader module creation successful
        match moduleResults with
        | (Right moduleVert, Right moduleFrag) ->

            // shader stage infos
            use entryPoint = new StringWrap ("main")
            let ssInfos = Array.zeroCreate<VkPipelineShaderStageCreateInfo> 2
            ssInfos[0] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[0].stage <- VkShaderStageFlags.Vertex
            ssInfos[0].``module`` <- moduleVert
            ssInfos[0].pName <- entryPoint.Pointer
            ssInfos[1] <- VkPipelineShaderStageCreateInfo ()
            ssInfos[1].stage <- VkShaderStageFlags.Fragment
            ssInfos[1].``module`` <- moduleFrag
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
                info.pVertexInputState <- &&viInfo
                info.pInputAssemblyState <- &&iaInfo
                info.pViewportState <- &&vInfo
                info.pRasterizationState <- NativePtr.add rInfos i
                info.pMultisampleState <- &&mInfo
                info.pDepthStencilState <- &&dInfo
                info.pColorBlendState <- NativePtr.add bInfos i
                info.pDynamicState <- &&dsInfo
                info.layout <- pipelineLayout
                info.renderPass <- VkRenderPass.Null
                info.subpass <- 0u
                NativePtr.set infos i info
                
            // create vulkan pipelines
            let vkPipelines = Array.zeroCreate<VkPipeline> pipelineSettings.Length
            use vkPipelinesPin = new ArrayPin<_> (vkPipelines)
            DeviceApi.vkCreateGraphicsPipelines (VkPipelineCache.Null, uint vkPipelines.Length, infos, nullPtr, vkPipelinesPin.Pointer) |> Hl.check
            
            // destroy shader modules
            DeviceApi.vkDestroyShaderModule (moduleVert, nullPtr)
            DeviceApi.vkDestroyShaderModule (moduleFrag, nullPtr)
            
            // pack vulkan pipelines with settings
            Array.zip pipelineSettings vkPipelines
        
        // abort
        | (moduleVertResult, moduleFragResult) ->
            match moduleVertResult with
            | Right moduleVert -> DeviceApi.vkDestroyShaderModule (moduleVert, nullPtr)
            | Left msg -> Log.warn msg
            match moduleFragResult with
            | Right moduleFrag -> DeviceApi.vkDestroyShaderModule (moduleFrag, nullPtr)
            | Left msg -> Log.warn msg
            Log.warn "VkPipeline creation aborted."
            [||]

    /// Create the VkPipelines for use by the given pipeline.
    static member private createVkPipelines pipeline =
        let vkPipelines =
            Pipeline.tryCreateVertAndFragPipelines
                pipeline.ShaderPathVert_
                pipeline.ShaderPathFrag_
                pipeline.PipelineSettings_
                pipeline.VkVertexBindings_
                pipeline.VkVertexAttributes_
                pipeline.VkPipelineLayout_
                pipeline.VkColorAttachmentFormats_
                pipeline.VkDepthTestFormatOpt_
        for (config, vkPipeline) in vkPipelines do
            pipeline.VkPipelines_.Add (config, vkPipeline)

    /// Destroy the VkPipelines used by the given pipelin.
    static member private destroyVkPipelines pipeline =
        for vkPipeline in pipeline.VkPipelines_.Values do
            DeviceApi.vkDestroyPipeline (vkPipeline, nullPtr)
        pipeline.VkPipelines_.Clear ()
            

    /// Try to get the VkPipeline built for the given settings.
    static member tryGetVkPipeline blend cullFace pipeline =
        Dictionary.tryFind (blend, cullFace) pipeline.VkPipelines_

    ///
    static member writeDescriptorUniformBuffer (binding : int) (descriptorIndex : int) (buffer : VulkanBuffer) vkDescriptorSet =

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
        write.descriptorType <- VkDescriptorType.UniformBuffer
        write.pBufferInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

        // advance buffer
        VulkanBuffer.advance buffer

    static member writeDescriptorStorageBuffer (binding : int) (descriptorIndex : int) (buffer : VulkanBuffer) vkDescriptorSet =

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
        write.pBufferInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

        // advance buffer
        VulkanBuffer.advance buffer

    static member writeDescriptorSampledImageView (binding : int) (descriptorIndex : int) (imageView : VkImageView) vkDescriptorSet =

        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.imageView <- imageView
        info.imageLayout <- ColorAttachmentRead.VkImageLayout

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.SampledImage
        write.pImageInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    ///
    static member writeDescriptorSampledImageViews (binding : int) (descriptorIndex : int) (imageViews : VkImageView array) vkDescriptorSet =

        // image infos
        let infosPtr = NativePtr.stackalloc<VkDescriptorImageInfo> imageViews.Length
        for i in 0 .. dec imageViews.Length do
            let mutable info = VkDescriptorImageInfo ()
            info.imageView <- imageViews[i]
            info.imageLayout <- ColorAttachmentRead.VkImageLayout
            NativePtr.set infosPtr i info

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- uint imageViews.Length
        write.descriptorType <- VkDescriptorType.SampledImage
        write.pImageInfo <- infosPtr
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    ///
    static member writeDescriptorCombinedImageViewSampler (binding : int) (descriptorIndex : int) (imageView : VkImageView) (sampler : Sampler) vkDescriptorSet =

        // image info
        let mutable info = VkDescriptorImageInfo ()
        info.sampler <- sampler.VkSampler
        info.imageView <- imageView
        info.imageLayout <- ColorAttachmentRead.VkImageLayout

        // write descriptor set
        let mutable write = VkWriteDescriptorSet ()
        write.dstSet <- vkDescriptorSet
        write.dstBinding <- uint binding
        write.dstArrayElement <- uint descriptorIndex
        write.descriptorCount <- 1u
        write.descriptorType <- VkDescriptorType.CombinedImageSampler
        write.pImageInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

    ///
    static member writeDescriptorSampledTexture binding descriptorIndex (texture : Texture) vkDescriptorSet =
        Pipeline.writeDescriptorSampledImageView binding descriptorIndex texture.ImageView vkDescriptorSet

    ///
    static member writeDescriptorSampledTextures binding descriptorIndex (textures : Texture array) vkDescriptorSet =
        let imageViews = Array.map (fun (texture : Texture) -> texture.ImageView) textures
        Pipeline.writeDescriptorSampledImageViews binding descriptorIndex imageViews vkDescriptorSet

    ///
    static member writeDescriptorCombinedTextureSampler binding descriptorIndex (texture : Texture) sampler vkDescriptorSet =
        Pipeline.writeDescriptorCombinedImageViewSampler binding descriptorIndex texture.ImageView sampler vkDescriptorSet

    ///
    static member writeDescriptorSampler (binding : int) (descriptorIndex : int) (sampler : Sampler) vkDescriptorSet =
        
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
        write.pImageInfo <- &&info
        DeviceApi.vkUpdateDescriptorSets (1u, &&write, 0u, nullPtr)

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
    static member specifyDescriptorSet<'k when 'k : equality> set (key : 'k) pipeline specify =
        let descriptorSet = Pipeline.getDescriptorSet set pipeline
        descriptorSet.Specify key specify

    /// Try to recreate VkPipelines with updated shaders.
    static member reloadShaders pipeline (context : VulkanContext) =
        ConcurrentCommandQueue.waitIdle context.RenderQueue // VkPipeline may still be in use by previous frame
        Pipeline.destroyVkPipelines pipeline
        Pipeline.createVkPipelines pipeline

    /// Create a vertex + fragment shader pipeline.
    static member create<'k when 'k : equality>
        shaderPath
        (blends : VulkanBlend array)
        (cullModes : bool array)
        (vertexBindings : VertexBinding array)
        (descriptorSetDefinitions : DescriptorSetDefinition array)
        (pushConstants : PushConstant array)
        colorAttachmentFormats
        depthTestFormatOpt
        buffers =

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
            descriptorSetLayouts[i] <- Pipeline.createDescriptorSetLayout layoutBindingsSets[i]

        // create descriptor sets
        let descriptorSets = Array.zeroCreate descriptorSetDefinitions.Length
        for i in 0 .. dec descriptorSetDefinitions.Length do
            let definition = descriptorSetDefinitions[i]
            descriptorSets[i] <- definition.CreateDescriptorSet descriptorSetLayouts[i]

        // create pipeline layout and vkPipelines
        if blends.Length < 1 then Log.fail "No pipeline blend was specified."
        let shaderPathVert = shaderPath + ".vert"
        let shaderPathFrag = shaderPath + ".frag"
        let pipelineSettings = Array.allPairs blends cullModes
        let vkPipelineLayout = Pipeline.createVkPipelineLayout descriptorSetLayouts pushConstantRanges
        let vkPipelines =
            Pipeline.tryCreateVertAndFragPipelines
                shaderPathVert
                shaderPathFrag
                pipelineSettings
                vertexBindingDescriptions
                vertexAttributes
                vkPipelineLayout
                colorAttachmentFormats
                depthTestFormatOpt

        // make Pipeline
        let pipeline =
            { Buffers_ = buffers
              DescriptorSets_ = descriptorSets
              VkPipelineLayout_ = vkPipelineLayout
              VkDescriptorSetLayouts_ = descriptorSetLayouts
              ShaderPathVert_ = shaderPathVert
              ShaderPathFrag_ = shaderPathFrag
              PipelineSettings_ = pipelineSettings
              VkVertexBindings_ = vertexBindingDescriptions
              VkVertexAttributes_ = vertexAttributes
              VkColorAttachmentFormats_ = colorAttachmentFormats
              VkDepthTestFormatOpt_ = depthTestFormatOpt
              VkPipelines_ = dictPlus HashIdentity.Structural vkPipelines
              DrawIndex_ = 0 }

        // fin
        pipeline

    /// Destroy a pipeline.
    static member destroy pipeline context =
        Pipeline.destroyVkPipelines pipeline
        DeviceApi.vkDestroyPipelineLayout (pipeline.PipelineLayout, nullPtr)
        for vkLayout in pipeline.VkDescriptorSetLayouts_ do DeviceApi.vkDestroyDescriptorSetLayout (vkLayout, nullPtr)
        for buffer in pipeline.Buffers_ do VulkanBuffer.destroy buffer context
        for set in pipeline.DescriptorSets_ do set.Destroy ()