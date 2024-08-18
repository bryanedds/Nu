namespace Vortice.Vulkan
open System
open System.IO
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Vortice.ShaderCompiler
open type Vma
open type Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// Check the given Vulkan operation result, logging on non-Success.
    let check (result : VkResult) =
        if int result > 0 then Log.info ("Vulkan info: " + string result)
        elif int result < 0 then Log.error ("Vulkan error: " + string result)

    /// Abstraction for VmaAllocation usage.
    type Allocation =
        { mutable Buffer : VkBuffer
          mutable Allocation : VmaAllocation }

        /// Allocate an upload buffer.
        static member createUpload<'ub when 'ub : unmanaged> (usage : VkBufferUsageFlags, allocator : VmaAllocator) =

            // specify exclusive buffer
            let mutable bufferCreateInfo = VkBufferCreateInfo ()
            bufferCreateInfo.size <- uint64 sizeof<'ub>
            bufferCreateInfo.usage <- usage
            bufferCreateInfo.sharingMode <- VkSharingMode.Exclusive

            // specify cpu to gpu
            let mutable allocationCreateInfo = VmaAllocationCreateInfo ()
            allocationCreateInfo.usage <- VmaMemoryUsage.CpuToGpu

            // allocate buffer
            let mutable buffer = VkBuffer ()
            let mutable allocation = VmaAllocation ()
            vmaCreateBuffer (allocator, Interop.AsPointer &bufferCreateInfo, Interop.AsPointer &allocationCreateInfo, Interop.AsPointer &buffer, Interop.AsPointer &allocation, NativePtr.nullPtr) |> check
            { Buffer = buffer; Allocation = allocation }

        /// Map and write to allocated buffer, then unmap.
        static member write<'ub when 'ub : unmanaged> (values : 'ub inref, allocation : VmaAllocation, allocator : VmaAllocator) =
            let memoryPtrPtr = Unchecked.defaultof<nativeptr<voidptr>>
            vmaMapMemory (allocator, allocation, memoryPtrPtr) |> check
            Marshal.StructureToPtr<'ub> (values, NativePtr.toNativeInt memoryPtrPtr, false)
            vmaUnmapMemory (allocator, allocation)

    [<RequireQualifiedAccess>]
    module DescriptorSetLayoutBinding =

        /// Specify a VkDescriptorSetLayoutBinding.
        let make (binding, descriptorType, descriptorCount, stages) =
            let mutable descriptorSetLayoutBinding = VkDescriptorSetLayoutBinding ()
            descriptorSetLayoutBinding.binding <- binding
            descriptorSetLayoutBinding.descriptorType <- descriptorType
            descriptorSetLayoutBinding.descriptorCount <- descriptorCount
            descriptorSetLayoutBinding.stageFlags <- stages
            descriptorSetLayoutBinding

    [<RequireQualifiedAccess>]
    module DescriptorSetLayout =

        /// Create a VkDescriptorSetLayout.
        let create (bindings, device) =

            // specify layout
            use bindingsWrap = new ArrayPin<_> (bindings)
            let mutable descriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo ()
            descriptorSetLayoutCreateInfo.bindingCount <- uint bindings.Length
            descriptorSetLayoutCreateInfo.pBindings <- bindingsWrap.Pointer

            // create layout
            let mutable descriptorSetLayout = VkDescriptorSetLayout ()
            vkCreateDescriptorSetLayout (device, Interop.AsPointer &descriptorSetLayoutCreateInfo, NativePtr.nullPtr, Interop.AsPointer &descriptorSetLayout) |> check
            descriptorSetLayout

    /// Abstraction for VkDescriptorSet usage.
    type DescriptorSet =
        { mutable DescriptorSetLayout : VkDescriptorSetLayout
          mutable DescriptorSet : VkDescriptorSet }

        /// Create a VkDescriptorSet from a given layout.
        static member create (descriptorSetLayout : VkDescriptorSetLayout byref, descriptorPool, device) =

            // specify allocation
            let mutable descriptorSetAllocateInfo = VkDescriptorSetAllocateInfo ()
            descriptorSetAllocateInfo.descriptorPool <- descriptorPool
            descriptorSetAllocateInfo.descriptorSetCount <- 1u
            descriptorSetAllocateInfo.pSetLayouts <- Interop.AsPointer &descriptorSetLayout

            // create descriptor set
            let mutable descriptorSet = VkDescriptorSet ()
            vkAllocateDescriptorSets (device, Interop.AsPointer &descriptorSetAllocateInfo, Interop.AsPointer &descriptorSet) |> check
            { DescriptorSetLayout = descriptorSetLayout; DescriptorSet = descriptorSet }

    [<RequireQualifiedAccess>]
    module ShaderModule =

        /// Compile a vertex and a fragment into respective modules.
        let compileVertexAndFragmentShader (shaderVertexStr, shaderFragmentStr, shaderFilePath, device) =
            use compiler = new Compiler ()
            use resultVertex = compiler.Compile (shaderVertexStr, shaderFilePath, ShaderKind.VertexShader)
            if resultVertex.Status <> CompilationStatus.Success
            then Log.fail resultVertex.ErrorMessage
            else
                use resultFragment = compiler.Compile (shaderFragmentStr, shaderFilePath, ShaderKind.FragmentShader)
                if resultFragment.Status <> CompilationStatus.Success
                then Log.fail resultFragment.ErrorMessage
                else
                    let bytesVertex = resultVertex.GetBytecode().ToArray()
                    let mutable shaderModuleVertex = VkShaderModule ()
                    vkCreateShaderModule (device, bytesVertex, NativePtr.nullPtr, &shaderModuleVertex) |> check
                    let bytesFragment = resultFragment.GetBytecode().ToArray()
                    let mutable shaderModuleFragment = VkShaderModule ()
                    vkCreateShaderModule (device, bytesFragment, NativePtr.nullPtr, &shaderModuleFragment) |> check
                    (shaderModuleVertex, shaderModuleFragment)

        /// Create a shader from a single file with both a '#shader vertex' section and a '#shader fragment' section.
        let createWithVertexAndFragmentShaderFromFilePath (shaderFilePath : string, device) =
            use shaderStream = new StreamReader (File.OpenRead shaderFilePath)
            let shaderStr = shaderStream.ReadToEnd ()
            let vertexStrIndex = shaderStr.IndexOf "#shader vertex"
            let fragmentStrIndex = shaderStr.IndexOf "#shader fragment"
            if vertexStrIndex > -1 && fragmentStrIndex > -1 then
                let (vertexStr, fragmentStr) =
                    if vertexStrIndex < fragmentStrIndex then
                        (shaderStr.Substring (vertexStrIndex, fragmentStrIndex - vertexStrIndex),
                         shaderStr.Substring (fragmentStrIndex, shaderStr.Length - fragmentStrIndex))
                    else
                        (shaderStr.Substring (fragmentStrIndex, vertexStrIndex - fragmentStrIndex),
                         shaderStr.Substring (vertexStrIndex, shaderStr.Length - vertexStrIndex))
                compileVertexAndFragmentShader (shaderFilePath, vertexStr.Replace ("#shader vertex", ""), fragmentStr.Replace ("#shader fragment", ""), device)
            else failwith ("Invalid shader file '" + shaderFilePath + "'. Both vertex and fragment shader sections required.")

    [<RequireQualifiedAccess>]
    module PipelineDepthStencilStateCreateInfo =

        /// Specify depth filtering as `less than` without stenciling.
        let makeLessThanUnstenciled () =
            let mutable pipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo ()
            pipelineDepthStencilStateCreateInfo.depthCompareOp <- VkCompareOp.Less
            pipelineDepthStencilStateCreateInfo

    [<RequireQualifiedAccess>]
    module PipelineColorBlendStateCreateInfo =

        /// Specify overwrite rendering.
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

        /// Specify alpha blended rendering.
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
        static member createWithVertexAndFragmentStages<'ubv, 'ubf when 'ubv : unmanaged and 'ubf : unmanaged>
            (viewport : VkViewport byref,
             scissor : VkRect2D byref,
             primitiveTopology,
             vertexAndFragmentShaderFilePath,
             pipelineDepthStencilStateCreateInfo : VkPipelineDepthStencilStateCreateInfo byref,
             pipelineColorBlendStateCreateInfo : VkPipelineColorBlendStateCreateInfo byref,
             descriptorSet,
             allocator,
             device) =

            // create shader modules
            let (shaderModuleVertex, shaderModuleFragment) = ShaderModule.createWithVertexAndFragmentShaderFromFilePath (vertexAndFragmentShaderFilePath, device)

            // specify vertex shader stage
            let mutable pipelineShaderStageCreateInfoVertex = VkPipelineShaderStageCreateInfo ()
            use pipelineShaderStageCreateInfoVertexName = new VkString (nameof pipelineShaderStageCreateInfoVertex)
            pipelineShaderStageCreateInfoVertex.stage <- VkShaderStageFlags.Vertex
            pipelineShaderStageCreateInfoVertex.``module`` <- shaderModuleVertex
            pipelineShaderStageCreateInfoVertex.pName <- pipelineShaderStageCreateInfoVertexName

            // specify fragment shader stage
            let mutable pipelineShaderStageCreateInfoFragment = VkPipelineShaderStageCreateInfo ()
            use pipelineShaderStageCreateInfoFragmentName = new VkString (nameof pipelineShaderStageCreateInfoFragment)
            pipelineShaderStageCreateInfoFragment.stage <- VkShaderStageFlags.Fragment
            pipelineShaderStageCreateInfoFragment.``module`` <- shaderModuleFragment
            pipelineShaderStageCreateInfoFragment.pName <- pipelineShaderStageCreateInfoFragmentName

            //
            let mutable uniformBuffersVertex = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
            let mutable uniformAllocationsVertex = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
            for i in 0 .. dec Constants.Render.FrameCount do
                let allocation = Allocation.createUpload<'ubv> (VkBufferUsageFlags.UniformBuffer, allocator)
                uniformBuffersVertex.[i] <- allocation.Buffer
                uniformAllocationsVertex.[i] <- allocation.Allocation

            //
            let mutable uniformBuffersFragment = Array.init Constants.Render.FrameCount (fun _ -> VkBuffer ())
            let mutable uniformAllocationsFragment = Array.init Constants.Render.FrameCount (fun _ -> VmaAllocation ())
            for i in 0 .. dec Constants.Render.FrameCount do
                let allocation = Allocation.createUpload<'ubv> (VkBufferUsageFlags.UniformBuffer, allocator)
                uniformBuffersFragment.[i] <- allocation.Buffer
                uniformAllocationsFragment.[i] <- allocation.Allocation

            // specify position-only vertex info state
            // TODO: P0: specify PBR vertex data.
            let mutable pipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo ()
            pipelineVertexInputStateCreateInfo.vertexBindingDescriptionCount <- 0u
            pipelineVertexInputStateCreateInfo.pVertexBindingDescriptions <- NativePtr.nullPtr
            pipelineVertexInputStateCreateInfo.vertexAttributeDescriptionCount <- 0u
            pipelineVertexInputStateCreateInfo.pVertexAttributeDescriptions <- NativePtr.nullPtr

            // specify input assembly state
            let mutable pipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo ()
            pipelineInputAssemblyStateCreateInfo.topology <- primitiveTopology
            pipelineInputAssemblyStateCreateInfo.primitiveRestartEnable <- VkBool32.False

            // specify viewport state
            let mutable pipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo ()
            pipelineViewportStateCreateInfo.viewportCount <- 1u
            pipelineViewportStateCreateInfo.pViewports <- Interop.AsPointer &viewport
            pipelineViewportStateCreateInfo.scissorCount <- 1u
            pipelineViewportStateCreateInfo.pScissors <- Interop.AsPointer &scissor

            // specify raterization state
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

            // specify null multisampling
            let mutable pipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo ()
            pipelineMultisampleStateCreateInfo.sampleShadingEnable <- VkBool32.False
            pipelineMultisampleStateCreateInfo.rasterizationSamples <- VkSampleCountFlags.Count1
            pipelineMultisampleStateCreateInfo.minSampleShading <- 1.0f
            pipelineMultisampleStateCreateInfo.pSampleMask <- NativePtr.nullPtr
            pipelineMultisampleStateCreateInfo.alphaToCoverageEnable <- VkBool32.False
            pipelineMultisampleStateCreateInfo.alphaToOneEnable <- VkBool32.False

            // specify dynamic state
            let mutable pipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo ()
            pipelineDynamicStateCreateInfo.dynamicStateCount <- 0u
            pipelineDynamicStateCreateInfo.pDynamicStates <- NativePtr.nullPtr

            // specify pipeline layout
            let mutable pipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo ()
            pipelineLayoutCreateInfo.setLayoutCount <- 1u
            pipelineLayoutCreateInfo.pSetLayouts <- Interop.AsPointer &descriptorSet.DescriptorSetLayout

            // create pipeline layout
            let mutable pipelineLayout = VkPipelineLayout ()
            vkCreatePipelineLayout (device, Interop.AsPointer &pipelineLayoutCreateInfo, NativePtr.nullPtr, &pipelineLayout) |> check

            // specify pipeline
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

            // create pipeline
            let mutable pipeline = VkPipeline ()
            vkCreateGraphicsPipeline (device, graphicsPipelineCreateInfo, &pipeline) |> check
            { UniformBuffersVertex = uniformBuffersVertex
              UniformAllocationsVertex = uniformAllocationsVertex
              UniformBuffersFragment = uniformBuffersFragment
              UniformAllocationsFragment = uniformAllocationsFragment
              PipelineLayout = pipelineLayout
              Pipeline = pipeline }