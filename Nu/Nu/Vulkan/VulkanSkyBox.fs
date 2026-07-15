// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type SkyBox =
    [<FieldOffset(0)>] val mutable color : Vector3
    [<FieldOffset(12)>] val mutable brightness : single

/// Describes a sky box pipeline that's loaded into GPU.
type SkyBoxPipeline =
    { EyeUniform : VulkanBuffer
      SkyBoxPropertiesUniform : VulkanBuffer
      Pipeline : Pipeline }

[<RequireQualifiedAccess>]
module SkyBox =

    /// Create a SkyBoxPipeline.
    let createSkyBoxPipeline colorAttachmentFormat depthAttachmentFormat (vkc : VulkanContext) =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<Eye> vkc
        let skyBoxPropertiesUniform = VulkanBuffer.create Uniform sizeof<SkyBox> vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SkyBoxShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 CubeMap.VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Unit>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] (Some depthAttachmentFormat)
                [|eyeUniform; skyBoxPropertiesUniform|]
        
        // make SkyBoxPipeline
        let skyBoxPipeline =
            { EyeUniform = eyeUniform
              SkyBoxPropertiesUniform = skyBoxPropertiesUniform
              Pipeline = pipeline }

        // fin
        skyBoxPipeline

    /// Destroy a SkyBoxPipeline.
    let destroySkyBoxPipeline skyBoxPipeline vkc =
        Pipeline.destroy skyBoxPipeline.Pipeline vkc

    /// Draw a sky box.
    let drawSkyBox
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projection : Matrix4x4)
        (color : Color)
        (brightness : single)
        (cubeMap : Texture)
        (geometry : CubeMapGeometry)
        (sampler : Sampler)
        (viewport : Viewport)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (pipeline : SkyBoxPipeline)
        (vkc : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projection.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                    
                // specify eye
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify sky box
                let skyBox = SkyBox (color = color.V3, brightness = brightness)
                VulkanBuffer.uploadValue skyBox pipeline.SkyBoxPropertiesUniform vkc
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.SkyBoxPropertiesUniform vkSet

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 cubeMap vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 sampler vkSet

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
            let mutable vkViewport = VulkanHl.makeViewport false renderArea
            let mutable renderingInfo = VulkanHl.makeRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None
            VulkanDevice.vkCmdBeginRendering (vkc.RenderCommandBuffer, &&renderingInfo)
            VulkanDevice.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            VulkanDevice.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            VulkanDevice.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            VulkanDevice.vkCmdSetDepthTestEnable (vkc.RenderCommandBuffer, true)
            VulkanDevice.vkCmdSetDepthCompareOp (vkc.RenderCommandBuffer, VkCompareOp.LessOrEqual)
                
            // bind vertex and index buffers
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            VulkanDevice.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
            VulkanDevice.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            VulkanDevice.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
            VulkanDevice.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
            VulkanDevice.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)
                
            // draw
            VulkanDevice.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)
        
            // tear down render
            VulkanDevice.vkCmdEndRendering vkc.RenderCommandBuffer

            // report draw scope
            VulkanHl.reportDrawScope ()

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer vkc

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."