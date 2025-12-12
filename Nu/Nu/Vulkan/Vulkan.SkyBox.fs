// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    /// Describes a sky box pipeline that's loaded into GPU.
    type SkyBoxPipeline =
        { ViewUniform : Buffer.Buffer
          ProjectionUniform : Buffer.Buffer
          ViewProjectionUniform : Buffer.Buffer
          ColorUniform : Buffer.Buffer
          BrightnessUniform : Buffer.Buffer
          SkyBoxPipeline : Pipeline.Pipeline }

    /// Destroy a SkyBoxPipeline.
    let DestroySkyBoxPipeline skyBoxPipeline vkc =
        Buffer.Buffer.destroy skyBoxPipeline.ViewUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ViewProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ColorUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.BrightnessUniform vkc
        Pipeline.Pipeline.destroy skyBoxPipeline.SkyBoxPipeline vkc

    /// Create a SkyBoxPipeline.
    let CreateSkyBoxPipeline (vkc : Hl.VulkanContext) =

        // TODO: DJL: enable depth testing.
        
        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SkyBoxShaderFilePath false false
                [|Pipeline.NoBlend|]
                [|Pipeline.vertex 0 CubeMap.VertexSize
                    [|Pipeline.attribute 0 Hl.Single3 0|]|]
                [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage
                  Pipeline.descriptor 1 Hl.UniformBuffer Hl.VertexStage
                  Pipeline.descriptor 2 Hl.UniformBuffer Hl.VertexStage
                  Pipeline.descriptor 3 Hl.UniformBuffer Hl.FragmentStage
                  Pipeline.descriptor 4 Hl.UniformBuffer Hl.FragmentStage
                  Pipeline.descriptor 5 Hl.CombinedImageSampler Hl.FragmentStage|]
                [||] vkc

        // create uniform buffers
        let viewUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let projectionUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let viewProjectionUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let colorUniform = Buffer.Buffer.create (sizeof<single> * 3) Buffer.Uniform vkc
        let brightnessUniform = Buffer.Buffer.create (sizeof<single> * 1) Buffer.Uniform vkc
        
        // make SkyBoxPipeline
        let skyBoxPipeline =
            { ViewUniform = viewUniform
              ProjectionUniform = projectionUniform
              ViewProjectionUniform = viewProjectionUniform
              ColorUniform = colorUniform
              BrightnessUniform = brightnessUniform
              SkyBoxPipeline = pipeline }

        // fin
        skyBoxPipeline

    /// Draw a sky box.
    let DrawSkyBox
        (view : single array,
         projection : single array,
         viewProjection : single array,
         color : Color,
         brightness : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         viewport : Viewport,
         pipeline : SkyBoxPipeline,
         vkc : Hl.VulkanContext) =

        // update uniform buffers
        Buffer.Buffer.uploadArray 0 0 view pipeline.ViewUniform vkc
        Buffer.Buffer.uploadArray 0 0 projection pipeline.ProjectionUniform vkc
        Buffer.Buffer.uploadArray 0 0 viewProjection pipeline.ViewProjectionUniform vkc
        Buffer.Buffer.uploadArray 0 0 [|color.R; color.G; color.B|] pipeline.ColorUniform vkc
        Buffer.Buffer.uploadArray 0 0 [|brightness|] pipeline.BrightnessUniform vkc

        // update descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 pipeline.ViewUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 1 pipeline.ProjectionUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 2 pipeline.ViewProjectionUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 3 pipeline.ColorUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 4 pipeline.BrightnessUniform pipeline.SkyBoxPipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 5 0 cubeMap.VulkanTexture pipeline.SkyBoxPipeline vkc
        
        // make viewport and scissor
        let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea

        // only draw if scissor (and therefore also viewport) is valid
        if Hl.validateRect scissor then

            // init render
            let cb = vkc.RenderCommandBuffer
            let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.NoBlend pipeline.SkyBoxPipeline
            Vulkan.vkCmdBindPipeline (cb, Hl.GraphicsBindPoint.VkPipelineBindPoint, vkPipeline)

            // set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // bind vertex and index buffer
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, geometry.IndexBuffer.VkBuffer, 0UL, Hl.Uint32Index.VkIndexType)

            // bind descriptor set
            let mutable descriptorSet = pipeline.SkyBoxPipeline.DescriptorSet
            Vulkan.vkCmdBindDescriptorSets
                (cb, Hl.GraphicsBindPoint.VkPipelineBindPoint,
                 pipeline.SkyBoxPipeline.PipelineLayout, 0u,
                 1u, asPointer &descriptorSet,
                 0u, nullPtr)
            
            // draw
            Vulkan.vkCmdDrawIndexed (cb, uint geometry.ElementCount, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
        
            // end render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer
