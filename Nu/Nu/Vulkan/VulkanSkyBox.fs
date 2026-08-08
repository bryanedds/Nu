// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type SkyBoxStruct =
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
    let createSkyBoxPipeline colorAttachmentFormat depthAttachmentFormat (context : VulkanContext) =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let skyBoxPropertiesUniform = VulkanBuffer.create Uniform sizeof<SkyBoxStruct> context

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
    let destroySkyBoxPipeline skyBoxPipeline context =
        Pipeline.destroy skyBoxPipeline.Pipeline context

    /// Draw a sky box.
    let drawSkyBox
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (color : Color)
        (brightness : single)
        (cubeMap : Texture)
        (geometry : CubeMapGeometry)
        (sampler : Sampler)
        (colorAttachment : Texture)
        (depthAttachment : Texture)
        (resolution : Vector2i)
        (pipeline : SkyBoxPipeline)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = projectionUnflipped.Flipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                    
                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify sky box
                let skyBox = SkyBoxStruct (color = color.V3, brightness = brightness)
                VulkanBuffer.uploadValue skyBox pipeline.SkyBoxPropertiesUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.SkyBoxPropertiesUniform vkSet

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 cubeMap vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 Unit pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 sampler vkSet

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint resolution.X, uint resolution.Y)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
            DeviceApi.vkCmdSetDepthTestEnable (context.RenderCommandBuffer, true)
            DeviceApi.vkCmdSetDepthCompareOp (context.RenderCommandBuffer, VkCompareOp.LessOrEqual)
                
            // bind vertex and index buffers
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
            DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)
        
            // tear down render
            DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer
            VulkanContext.advanceRenderCommandBuffer context

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")