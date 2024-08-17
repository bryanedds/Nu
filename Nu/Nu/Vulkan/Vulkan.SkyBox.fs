namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
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
            
            // create bindings
            let mutable descriptorSetLayoutBindingVertex = Hl.DescriptorSetLayoutBinding.make (0u, VkDescriptorType.UniformBuffer, 1u, VkShaderStageFlags.Vertex)
            let mutable descriptorSetLayoutBindingFragment = Hl.DescriptorSetLayoutBinding.make (1u, VkDescriptorType.UniformBuffer, 1u, VkShaderStageFlags.Fragment)
            let mutable descriptorSetLayoutBindingCubeMap = Hl.DescriptorSetLayoutBinding.make (2u, VkDescriptorType.UniformBuffer, 1u, VkShaderStageFlags.Fragment)
            let descriptorSetLayoutBindings = [|descriptorSetLayoutBindingVertex; descriptorSetLayoutBindingFragment; descriptorSetLayoutBindingCubeMap|]

            // create descriptor set
            let mutable descriptorSetLayout = Hl.DescriptorSetLayout.make (descriptorSetLayoutBindings, device)
            let descriptorSet = Hl.DescriptorSet.create (&descriptorSetLayout, descriptorPool, device)

            // create pipeline
            let mutable lessThanDepthUnstenciled = Hl.PipelineDepthStencilStateCreateInfo.makeLessThanUnstenciled ()
            let mutable alphaBlend = Hl.PipelineColorBlendStateCreateInfo.makeAlpha ()
            let pipeline = Hl.Pipeline.createVertexFragment<SkyBoxUniformBufferVertex, SkyBoxUniformBufferFragment> (&viewport, &scissor, VkPrimitiveTopology.TriangleList, &lessThanDepthUnstenciled, &alphaBlend, descriptorSet, allocator, device)
            
            // fin
            { DescriptorSet = descriptorSet; Pipeline = pipeline }

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