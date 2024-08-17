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
            end

    /// Describes a skybox pipeline that's loaded into GPU.
    type SkyBoxPipeline =
        { DescriptorSet : Hl.DescriptorSet
          Pipeline : Hl.Pipeline }

        /// Create a skybox shader pipeline.
        static member createPipeline (shaderFilePath : string, viewport : VkViewport byref, scissor : VkRect2D byref, descriptorPool : VkDescriptorPool, allocator : VmaAllocator, device : VkDevice) =

            let mutable descriptorSetLayoutBindingVertex = VkDescriptorSetLayoutBinding ()
            descriptorSetLayoutBindingVertex.binding <- 0u
            descriptorSetLayoutBindingVertex.descriptorType <- VkDescriptorType.UniformBuffer
            descriptorSetLayoutBindingVertex.descriptorCount <- 1u
            descriptorSetLayoutBindingVertex.stageFlags <- VkShaderStageFlags.Vertex

            let mutable descriptorSetLayoutBindingFragment = VkDescriptorSetLayoutBinding ()
            descriptorSetLayoutBindingFragment.binding <- 1u
            descriptorSetLayoutBindingFragment.descriptorType <- VkDescriptorType.UniformBuffer
            descriptorSetLayoutBindingFragment.descriptorCount <- 1u
            descriptorSetLayoutBindingFragment.stageFlags <- VkShaderStageFlags.Fragment

            let mutable descriptorSetLayoutBindingCubeMap = VkDescriptorSetLayoutBinding ()
            descriptorSetLayoutBindingCubeMap.binding <- 2u
            descriptorSetLayoutBindingCubeMap.descriptorType <- VkDescriptorType.CombinedImageSampler
            descriptorSetLayoutBindingCubeMap.descriptorCount <- 1u
            descriptorSetLayoutBindingCubeMap.stageFlags <- VkShaderStageFlags.Fragment

            let bindings = [|descriptorSetLayoutBindingVertex; descriptorSetLayoutBindingFragment; descriptorSetLayoutBindingCubeMap|]
            use bindingsWrap = new ArrayPin<_> (bindings)

            let mutable descriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo ()
            let mutable descriptorSetLayout = VkDescriptorSetLayout ()
            descriptorSetLayoutCreateInfo.bindingCount <- uint bindings.Length
            descriptorSetLayoutCreateInfo.pBindings <- bindingsWrap.Pointer
            let result = vkCreateDescriptorSetLayout (device, Interop.AsPointer &descriptorSetLayoutCreateInfo, NativePtr.nullPtr, Interop.AsPointer &descriptorSetLayout)

            let descriptorSet = Hl.DescriptorSet.create (&descriptorSetLayout, descriptorPool, device)
            let lessThanDepthUnstenciled = Hl.PipelineDepthStencilStateCreateInfo.makeLessThanUnstenciled ()
            let alphaBlend = Hl.PipelineColorBlendStateCreateInfo.makeAlpha ()
            let pipeline = Hl.Pipeline.createVertexFragment<SkyBoxUniformBufferVertex, SkyBoxUniformBufferFragment> (&viewport, &scissor, VkPrimitiveTopology.TriangleList, lessThanDepthUnstenciled, alphaBlend, descriptorSet, allocator, device)

            (result, { DescriptorSet = descriptorSet; Pipeline = pipeline })

        /// Draw a skybox.
        static member draw
            (frame : int,
             commandBuffer : VkCommandBuffer,
             skyBoxPipeline : SkyBoxPipeline,
             geometry : CubeMap.CubeMapGeometry,
             view : Matrix4x4,
             projection : Matrix4x4,
             color : Vector3,
             brightness : single,
             cubeMap : VkImageView,
             allocator : VmaAllocator) =

            // Bind the pipeline
            vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, skyBoxPipeline.Pipeline.Pipeline)

            // Bind the descriptor set
            vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, skyBoxPipeline.Pipeline.PipelineLayout, 0u, ReadOnlySpan [|skyBoxPipeline.DescriptorSet.DescriptorSet|], ReadOnlySpan [||])

            let mutable uniformValuesVertex = SkyBoxUniformBufferVertex ()
            uniformValuesVertex.View <- view
            uniformValuesVertex.Projection <- projection
            Hl.Buffer.write (&uniformValuesVertex, skyBoxPipeline.Pipeline.UniformAllocationsVertex.[frame], allocator)

            let mutable uniformValuesFragment = SkyBoxUniformBufferFragment ()
            uniformValuesFragment.Color <- color
            uniformValuesFragment.Brightness <- brightness
            Hl.Buffer.write (&uniformValuesFragment, skyBoxPipeline.Pipeline.UniformAllocationsFragment.[frame], allocator)

            // TODO: P0: figure out how to write cube map sampler.

            // Bind the vertex and index buffers
            vkCmdBindVertexBuffer (commandBuffer, 0u, geometry.VertexBuffer, 0UL)
            vkCmdBindIndexBuffer (commandBuffer, geometry.IndexBuffer, 0UL, VkIndexType.Uint32)

            // Draw the geometry
            vkCmdDrawIndexed (commandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)