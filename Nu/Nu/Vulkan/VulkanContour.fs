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
    let createContourTessellationPipeline vkc =

        // create buffers
        let count = 1024 // TODO: P1: make constant.
        let vertexBuffer = Buffer.create (count * sizeof<ContourVertex>) (Vertex true) vkc
        let indexBuffer = Buffer.create (count * sizeof<uint32>) (BufferType.Index true) vkc
        let modelViewProjectionUniform = Buffer.create sizeof<Matrix4x4> Storage vkc
        
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
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1|]|]
                [||] [|vkc.SwapFormat|] None
                [|vertexBuffer; indexBuffer; modelViewProjectionUniform|]
                vkc

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
         (vertexBuffer : Nu.Vulkan.Buffer, indexBuffer : Nu.Vulkan.Buffer, modelViewProjectionUniform : Nu.Vulkan.Buffer, pipeline : Pipeline),
         vkc : VulkanContext) =
            
        // only draw if scissor is valid
        let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
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
            scissor <-
                VkRect2D
                    ((minScissor.X |> round |> int) + offset.X,
                     (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                     uint sizeScissor.X,
                     uint sizeScissor.Y)
            scissor <- Hl.clipRect renderArea scissor
        | ValueNone -> ()
        if Hl.validateRect scissor then
                
            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanTransparent true pipeline with
            | Some vkPipeline ->

                // update vertices and indices
                Buffer.uploadArray tessellation.Vertices vertexBuffer vkc
                Buffer.uploadArray tessellation.Indices indexBuffer vkc

                // specify uniforms
                let modelViewProjection = modelViewProjection
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline vkc $ fun vkSet ->
                    Buffer.uploadValue modelViewProjection modelViewProjectionUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 modelViewProjectionUniform vkSet vkc

                // set up render
                let mutable renderingInfo = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &renderingInfo)
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &scissor)

                // bind vertex and index buffers
                let mutable vkVertexBuffer = vertexBuffer.VkBuffer
                let mutable vkVertexOffset = 0UL
                Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkVertexBuffer, asPointer &vkVertexOffset)
                Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, indexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor set
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)

                // draw
                Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, uint32 tessellation.Indices.Length, 1u, 0u, 0, 0u)
                    
                // tear down render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // advance vertex and index buffers
                Buffer.advance vertexBuffer
                Buffer.advance indexBuffer

                // advance pipeline
                Pipeline.advance 1 pipeline

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

        // bulk draw limit exceeded
        else Log.warnOnce "Draw operations aborted because bulk draw limit has been reached. Increase relevant bulk draw limit as necessary for current application."