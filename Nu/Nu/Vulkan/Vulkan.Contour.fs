// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Vortice.Vulkan
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module ContourTessellation =

    /// Create pipeline for vector graphics contour rendering.
    let createPipeline vkc =

        // create uniform buffer for model-view-projection matrix
        let modelViewProjectionUniform = Buffer.Buffer.create sizeof<Matrix4x4> Buffer.Storage vkc
        
        // create the vertex and index buffers at init; size doesn't particularly matter here (VkBuffer will re-allocate itself with a larger size
        // if necessary, when Buffer.Buffer.uploadArray is called). just guess the likely maximum
        let size = 1024
        let vertexBuffer = Buffer.Buffer.create (size * sizeof<ContourVertex>) (Buffer.Vertex true) vkc
        let indexBuffer = Buffer.Buffer.create (size * sizeof<uint32>) (Buffer.Index true) vkc
        
        // create pipeline
        let vertexSize = sizeof<ContourVertex> // = sizeof<Vector2> + sizeof<Color> = 2 * sizeof<single> + 4 * sizeof<single>
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.ContourShaderFilePath
                Constants.Render.ContoursMax
                [|Pipeline.Transparent|]
                [|Pipeline.vertex 0 vertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single2 0 // Position
                      Pipeline.attribute 1 Hl.Single4 sizeof<Vector2>|]|] // Color
                [|Pipeline.descriptorSet Hl.BulkDescriptorIndexed 1 [|Pipeline.descriptor 0 Hl.StorageBuffer Hl.VertexStage 1|]|]
                [|Pipeline.pushConstant 0 sizeof<int> Hl.VertexFragmentStage|]
                [|vkc.SwapFormat|] None vkc
        
        // fin
        ((vertexBuffer, indexBuffer), (modelViewProjectionUniform, pipeline))

    /// Draw a contour tessellation.
    let draw
        (drawIndex : int)
        tessellation
        (absolute : bool)
        (viewProjectionClipAbsolute : Matrix4x4 inref)
        (viewProjectionClipRelative : Matrix4x4 inref)
        (modelViewProjection : Matrix4x4 inref)
        (clipOpt : Box2 voption inref)
        (viewport : Viewport)
        (vertexBuffer : Buffer.Buffer, indexBuffer : Buffer.Buffer)
        (modelViewProjectionUniform : Buffer.Buffer, pipeline : Pipeline.Pipeline)
        (vkc : Hl.VulkanContext) =
        
        // ensure bulk draw limit is not exceeded
        if drawIndex < pipeline.BulkDrawLimit then
        
            // upload vertex data
            Buffer.Buffer.uploadArray drawIndex 0 0 tessellation.Vertices vertexBuffer vkc
            Buffer.Buffer.uploadArray drawIndex 0 0 tessellation.Indices indexBuffer vkc
            
            // bind uniforms
            Buffer.Buffer.uploadValue drawIndex 0 0 modelViewProjection modelViewProjectionUniform vkc
            Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 0 drawIndex modelViewProjectionUniform.[drawIndex] pipeline vkc
            
            // make viewport and scissor
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
            
            // only draw if scissor is valid
            if Hl.validateRect scissor then
                
                // only draw if required vkPipeline exists
                match Pipeline.Pipeline.tryGetVkPipeline Pipeline.Transparent true pipeline with
                | Some vkPipeline ->
                    
                    // init render
                    let cb = vkc.RenderCommandBuffer
                    let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                    Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)
                    
                    // bind pipeline
                    Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)
                    
                    // set viewport and scissor
                    Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                    Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
                    
                    // bind vertex and index buffers
                    let mutable vertexBuf = vertexBuffer.[drawIndex]
                    let mutable vertexOffset = 0UL
                    Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuf, asPointer &vertexOffset)
                    Vulkan.vkCmdBindIndexBuffer (cb, indexBuffer.[drawIndex], 0UL, VkIndexType.Uint32)
                    
                    // bind descriptor set
                    let mutable descriptorSet = pipeline.VkDescriptorSet 0 0
                    Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, asPointer &descriptorSet, 0u, nullPtr)
                    
                    // push draw index
                    let mutable drawIdx = drawIndex
                    Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Hl.VertexFragmentStage.VkShaderStageFlags, 0u, 4u, asVoidPtr &drawIdx)
                    
                    // draw
                    Vulkan.vkCmdDrawIndexed (cb, uint32 tessellation.Indices.Length, 1u, 0u, 0, 0u)
                    Hl.reportDrawCall 1
                    
                    // end render
                    Vulkan.vkCmdEndRendering cb

                // abort
                | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

        // bulk draw limit exceeded
        else Log.warnOnce "Draw operations aborted because bulk draw limit has been reached. Increase relevant bulk draw limit as necessary for current application."