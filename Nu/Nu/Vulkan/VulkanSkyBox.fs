// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type SkyBoxVert =
        [<FieldOffset(0)>] val mutable view : Matrix4x4
        [<FieldOffset(64)>] val mutable projection : Matrix4x4
        [<FieldOffset(128)>] val mutable viewProjection : Matrix4x4

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type SkyBoxFrag =
        [<FieldOffset(0)>] val mutable color : Vector3
        [<FieldOffset(12)>] val mutable brightness : single
    
    /// Describes a sky box pipeline that's loaded into GPU.
    type SkyBoxPipeline =
        { SkyBoxVertUniform : Nu.Vulkan.Buffer
          SkyBoxFragUniform : Nu.Vulkan.Buffer
          Pipeline : Pipeline }

    /// Create a SkyBoxPipeline.
    let CreateSkyBoxPipeline colorAttachmentFormat depthAttachmentFormat (vkc : VulkanContext) =

        // create uniform buffers
        let skyBoxVertUniform = Buffer.create sizeof<SkyBoxVert> Storage vkc
        let skyBoxFragUniform = Buffer.create sizeof<SkyBoxFrag> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SkyBoxShaderFilePath
                [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 CubeMap.VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] (Some depthAttachmentFormat)
                [|skyBoxVertUniform; skyBoxFragUniform|] vkc
        
        // make SkyBoxPipeline
        let skyBoxPipeline =
            { SkyBoxVertUniform = skyBoxVertUniform
              SkyBoxFragUniform = skyBoxFragUniform
              Pipeline = pipeline }

        // fin
        skyBoxPipeline

    /// Destroy a SkyBoxPipeline.
    let DestroySkyBoxPipeline skyBoxPipeline vkc =
        Pipeline.destroy skyBoxPipeline.Pipeline vkc

    /// Draw a sky box.
    let DrawSkyBox
        (view : Matrix4x4,
         projection : Matrix4x4,
         viewProjection : Matrix4x4,
         color : Color,
         brightness : single,
         cubeMap : Texture,
         geometry : CubeMap.CubeMapGeometry,
         sampler : Sampler,
         viewport : Viewport,
         colorAttachment : Texture,
         depthAttachment : Texture,
         pipeline : SkyBoxPipeline,
         vkc : VulkanContext) =

        // only draw if scissor (and therefore also viewport) is valid
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        if Hl.validateRect renderArea then

            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
            | Some vkPipeline ->

                // specify uniforms
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->
                    let skyBoxVert = SkyBoxVert (view = view, projection = projection, viewProjection = viewProjection)
                    let skyBoxFrag = SkyBoxFrag (color = color.V3, brightness = brightness)
                    Buffer.uploadValue skyBoxVert pipeline.SkyBoxVertUniform vkc
                    Buffer.uploadValue skyBoxFrag pipeline.SkyBoxFragUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.SkyBoxVertUniform vkSet vkc
                    Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.SkyBoxFragUniform vkSet vkc

                // specify material
                let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampledImage 0 0 cubeMap vkSet vkc

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline.Pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 sampler vkSet vkc

                // set up render
                let mutable rendering = Hl.makeRenderingInfo [|colorAttachment.ImageView|] (Some depthAttachment.ImageView) renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &renderArea)
                Vulkan.vkCmdSetDepthTestEnable (vkc.RenderCommandBuffer, true)
                Vulkan.vkCmdSetDepthCompareOp (vkc.RenderCommandBuffer, VkCompareOp.LessOrEqual)
                
                // bind vertex and index buffers
                let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
                let mutable vertexOffset = 0UL
                Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)
                
                // draw
                Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)
        
                // tear down render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // advance pipeline
                pipeline.Pipeline.Advance 1

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."