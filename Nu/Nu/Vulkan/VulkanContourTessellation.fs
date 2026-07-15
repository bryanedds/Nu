// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open Vortice.Vulkan
open Prime
open Nu

[<RequireQualifiedAccess>]
module ContourTessellation =

    /// Create pipeline for vector graphics contour rendering.
    let createContourTessellationPipeline context =

        // create buffers
        let count = 1024 // TODO: P1: make constant.
        let vertexBuffer = VulkanBuffer.create (Vertex true) (count * sizeof<ContourVertex>) context
        let indexBuffer = VulkanBuffer.create (VulkanBufferType.Index true) (count * sizeof<uint32>) context
        let modelViewProjectionUniform = VulkanBuffer.create Uniform sizeof<Matrix4x4> context
        
        // create pipeline
        let vertexSize = sizeof<ContourVertex> // = sizeof<Vector2> + sizeof<Color> = 2 * sizeof<single> + 4 * sizeof<single>
        let pipeline =
            Pipeline.create
                Constants.Paths.ContourShaderFilePath
                [|VulkanTransparent|] [|true|]
                [|Pipeline.vertex 0 vertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single2 0
                      Pipeline.attribute 1 Single4 sizeof<Vector2>|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1|]|]
                [||] [|context.SwapFormat|] None
                [|vertexBuffer; indexBuffer; modelViewProjectionUniform|]

        // fin
        (vertexBuffer, indexBuffer, modelViewProjectionUniform, pipeline)

    /// Draw a contour tessellation.
    let drawContourTessellation
        (tessellation : ContourTessellation,
         absolute : bool,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         modelViewProjection : Matrix4x4 inref,
         clipOpt : Box2 voption inref,
         viewport : Viewport,
         (vertexBuffer : VulkanBuffer, indexBuffer : VulkanBuffer, modelViewProjectionUniform : VulkanBuffer, pipeline : Pipeline),
         context : VulkanContext) =
            
        // only draw if scissor (and therefore also viewport) is valid
        let pixelDensity = Hl.getWindowPixelDensity context.Window
        let renderAreaLogical = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
        let mutable renderArea = Hl.scaleRectForPixelDensity pixelDensity renderAreaLogical
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea
        match clipOpt with
        | ValueSome clip ->
            let viewProjection = if absolute then &viewProjectionClipAbsolute else &viewProjectionClipRelative
            let minClip = Vector4.Transform(Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection).V2
            let minNdc = minClip * single viewport.DisplayScalar
            let minScissor = (minNdc + v2One) * 0.5f * viewport.Inner.Size.V2
            let sizeClip = Vector4.Transform(Vector4 (clip.Size, 0.0f, 1.0f), viewProjection).V2
            let sizeNdc = sizeClip * single viewport.DisplayScalar
            let sizeScissor = sizeNdc * 0.5f * viewport.Inner.Size.V2
            let offset = v2i viewport.Inner.Min.X (viewport.Outer.Max.Y - viewport.Inner.Max.Y)
            let scissorLogical =
                VkRect2D
                    ((minScissor.X |> round |> int) + offset.X,
                        (single renderAreaLogical.extent.height - minScissor.Y |> round |> int) + offset.Y,
                        uint sizeScissor.X,
                        uint sizeScissor.Y)
            scissor <- Hl.scaleRectForPixelDensity pixelDensity scissorLogical
            scissor <- Hl.clipRect renderArea scissor
        | ValueNone -> ()
        if Hl.validateRect scissor then
                
            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanTransparent true pipeline with
            | Some vkPipeline ->

                // update vertices and indices
                VulkanBuffer.uploadArray tessellation.Vertices vertexBuffer context
                VulkanBuffer.uploadArray tessellation.Indices indexBuffer context

                // specify uniforms
                let modelViewProjection = modelViewProjection
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline $ fun vkSet ->
                    VulkanBuffer.uploadValue modelViewProjection modelViewProjectionUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 modelViewProjectionUniform vkSet

                // set up render
                let mutable renderingInfo = Hl.makeRenderingInfo [|context.SwapchainImageView|] None renderArea None
                DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&scissor)

                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

                // bind vertex and index buffers
                let mutable vkVertexBuffer = vertexBuffer.VkBuffer
                let mutable vkVertexOffset = 0UL
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 1u, &&vkVertexBuffer, &&vkVertexOffset)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, indexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor set
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, uint32 tessellation.Indices.Length, 1u, 0u, 0, 0u)
                    
                // tear down render
                DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

                // advance vertex and index buffers
                VulkanBuffer.advance vertexBuffer
                VulkanBuffer.advance indexBuffer

                // advance pipeline
                Pipeline.advance 1 true pipeline

                // intermittently advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer context

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."