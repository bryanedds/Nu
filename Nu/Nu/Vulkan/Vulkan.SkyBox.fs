// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
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
        { SkyBoxVertUniform : Buffer.Buffer
          SkyBoxFragUniform : Buffer.Buffer
          SkyBoxPipeline : Pipeline.Pipeline }

    /// Destroy a SkyBoxPipeline.
    let DestroySkyBoxPipeline skyBoxPipeline vkc =
        Buffer.Buffer.destroy skyBoxPipeline.SkyBoxVertUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.SkyBoxFragUniform vkc
        Pipeline.Pipeline.destroy skyBoxPipeline.SkyBoxPipeline vkc

    /// Create a SkyBoxPipeline.
    let CreateSkyBoxPipeline colorAttachmentFormat depthAttachmentFormat (vkc : Hl.VulkanContext) =

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SkyBoxShaderFilePath
                [|Pipeline.NoBlend|]
                [|Pipeline.vertex 0 CubeMap.VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single3 0|]|]
                [|Pipeline.descriptorSet false
                    [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.UniformBuffer Hl.FragmentStage 1
                      Pipeline.descriptor 2 Hl.CombinedImageSampler Hl.FragmentStage 1|]|]
                [||] colorAttachmentFormat
                (Some (Pipeline.depthTest depthAttachmentFormat))
                vkc

        // create uniform buffers
        let skyBoxVertUniform = Buffer.Buffer.create sizeof<SkyBoxVert> Buffer.Uniform vkc
        let skyBoxFragUniform = Buffer.Buffer.create sizeof<SkyBoxFrag> Buffer.Uniform vkc
        
        // make SkyBoxPipeline
        let skyBoxPipeline =
            { SkyBoxVertUniform = skyBoxVertUniform
              SkyBoxFragUniform = skyBoxFragUniform
              SkyBoxPipeline = pipeline }

        // fin
        skyBoxPipeline

    /// Draw a sky box.
    let DrawSkyBox
        (view : Matrix4x4,
         projection : Matrix4x4,
         viewProjection : Matrix4x4,
         color : Color,
         brightness : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         viewport : Viewport,
         colorAttachment : Texture.Texture,
         depthAttachment : Texture.Texture,
         pipeline : SkyBoxPipeline,
         vkc : Hl.VulkanContext) =

        // upload uniforms
        let mutable skyBoxVert = SkyBoxVert ()
        let mutable skyBoxFrag = SkyBoxFrag ()
        skyBoxVert.view <- view
        skyBoxVert.projection <- projection
        skyBoxVert.viewProjection <- viewProjection
        skyBoxFrag.color <- v3 color.R color.G color.B
        skyBoxFrag.brightness <- brightness
        Buffer.Buffer.uploadValue 0 0 0 skyBoxVert pipeline.SkyBoxVertUniform vkc
        Buffer.Buffer.uploadValue 0 0 0 skyBoxFrag pipeline.SkyBoxFragUniform vkc
        
        // update uniform descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 0 pipeline.SkyBoxVertUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 1 pipeline.SkyBoxFragUniform pipeline.SkyBoxPipeline vkc
        
        // bind texture
        Pipeline.Pipeline.writeDescriptorTexture 0 0 2 cubeMap pipeline.SkyBoxPipeline vkc

        // make viewport and scissor
        let mutable renderArea = VkRect2D (0, 0, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea

        // only draw if scissor (and therefore also viewport) is valid
        if Hl.validateRect scissor then

            // init render
            let cb = vkc.RenderCommandBuffer
            let mutable rendering = Hl.makeRenderingInfo colorAttachment.ImageView (Some depthAttachment.ImageView) renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.NoBlend false pipeline.SkyBoxPipeline
            Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

            // set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // set depth test state
            Vulkan.vkCmdSetDepthTestEnable (cb, true)
            Vulkan.vkCmdSetDepthCompareOp (cb, VkCompareOp.LessOrEqual)
            
            // bind vertex and index buffer
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor set
            let mutable descriptorSet = pipeline.SkyBoxPipeline.VkDescriptorSet 0
            Vulkan.vkCmdBindDescriptorSets
                (cb, VkPipelineBindPoint.Graphics,
                 pipeline.SkyBoxPipeline.PipelineLayout, 0u,
                 1u, asPointer &descriptorSet,
                 0u, nullPtr)
            
            // draw
            Vulkan.vkCmdDrawIndexed (cb, uint geometry.ElementCount, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
        
            // end render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer
