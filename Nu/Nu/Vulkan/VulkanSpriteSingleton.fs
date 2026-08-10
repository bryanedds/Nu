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
type SpriteVertStruct =
    [<FieldOffset(0)>] val mutable modelViewProjection : Matrix4x4
    [<FieldOffset(64)>] val mutable texCoords4 : Vector4
    
[<Struct; StructLayout (LayoutKind.Explicit)>]
type SpriteFragStruct =
    [<FieldOffset(0)>] val mutable color : Vector4

[<RequireQualifiedAccess>]
module SpriteSingleton =

    let VertexSize = sizeof<single> * 2
    
    /// Create a sprite singleton pipeline.
    let createSpriteSingletonPipeline (context : VulkanContext) =

        // create sprite uniform buffers
        let spriteVertUniform = VulkanBuffer.create Uniform sizeof<SpriteVertStruct> context
        let spriteFragUniform = VulkanBuffer.create Uniform sizeof<SpriteFragStruct> context
        
        // create sprite pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SpriteShaderFilePath
                [|VulkanTransparent|] [|true|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single2 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|context.SwapFormat|] None
                [|spriteVertUniform; spriteFragUniform|]

        // fin
        (spriteVertUniform, spriteFragUniform, pipeline)
    
    /// Create a sprite singleton quad for rendering to this pipeline.
    let createSpriteQuad onlyUpperRightQuadrant context =

        // build vertex data
        let vertexData =
            if onlyUpperRightQuadrant then
                [|+0.0f; +0.0f
                  +1.0f; +0.0f
                  +1.0f; +1.0f
                  +0.0f; +1.0f|]
            else
                [|-1.0f; -1.0f
                  +1.0f; -1.0f
                  +1.0f; +1.0f
                  -1.0f; +1.0f|]

        // build index data
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        
        // create buffers
        let vertexBuffer = VulkanBuffer.createVertexStagedFromArray vertexData context
        let indexBuffer = VulkanBuffer.createIndexStagedFromArray indexData context
        
        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a single sprite.
    let drawSpriteSingleton
        (vertices : VulkanBuffer,
         indices : VulkanBuffer,
         absolute,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         modelViewProjection : Matrix4x4,
         insetOpt : Box2 voption inref,
         clipOpt : Box2 voption inref,
         color : Color inref,
         flip,
         textureWidth,
         textureHeight,
         texture : Texture,
         sampler : Sampler,
         viewport : Viewport,
         spriteVertUniform : VulkanBuffer,
         spriteFragUniform : VulkanBuffer,
         pipeline : Pipeline,
         context : VulkanContext) =

        // only draw if scissor (and therefore also viewport) is valid
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

            // only draw when required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanTransparent true pipeline with
            | Some vkPipeline ->

                // compute unflipped tex coords
                let texCoordsUnflipped =
                    let texelWidth = 1.0f / single textureWidth
                    let texelHeight = 1.0f / single textureHeight
                    let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
                    let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
                    match insetOpt with
                    | ValueSome inset ->
                        let mx = inset.Min.X * texelWidth + borderWidth
                        let my = inset.Min.Y * texelHeight + inset.Size.Y * texelHeight - borderHeight // distributes texelHeight multiplication to preserve precision
                        let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                        let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                        Box2 (mx, my, sx, sy)
                    | ValueNone ->
                        let mx = borderWidth
                        let my = 1.0f - borderHeight
                        let sx = 1.0f - borderWidth * 2.0f
                        let sy = -1.0f + borderHeight * 2.0f
                        Box2 (mx, my, sx, sy)
            
                // compute a flipping flags
                let struct (flipH, flipV) =
                    match flip with
                    | Unflipped -> struct (false, false)
                    | Horizontal -> struct (true, false)
                    | Vertical -> struct (false, true)
                    | Diagonal -> struct (true, true)

                // compute tex coords
                let texCoords =
                    box2
                        (v2
                            (if flipH then texCoordsUnflipped.Min.X + texCoordsUnflipped.Size.X else texCoordsUnflipped.Min.X)
                            (if flipV then texCoordsUnflipped.Min.Y + texCoordsUnflipped.Size.Y else texCoordsUnflipped.Min.Y))
                        (v2
                            (if flipH then -texCoordsUnflipped.Size.X else texCoordsUnflipped.Size.X)
                            (if flipV then -texCoordsUnflipped.Size.Y else texCoordsUnflipped.Size.Y))

                // specify uniforms
                let color = color
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline $ fun vkSet ->
                    let spriteVert = SpriteVertStruct (modelViewProjection = modelViewProjection, texCoords4 = v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y)
                    let spriteFrag = SpriteFragStruct (color = color.V4)
                    VulkanBuffer.uploadValue spriteVert spriteVertUniform context
                    VulkanBuffer.uploadValue spriteFrag spriteFragUniform context
                    Pipeline.writeDescriptorUniformBuffer 0 0 spriteVertUniform vkSet
                    Pipeline.writeDescriptorUniformBuffer 1 0 spriteFragUniform vkSet

                // specify material
                let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 texture pipeline $ fun vkSet ->
                    Pipeline.writeDescriptorSampledTexture 0 0 texture vkSet

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 sampler vkSet
                    
                // set up render
                Hl.withRenderingInfo [|context.SwapchainImageView|] None renderArea None $ fun renderingInfo ->
                    let mutable renderingInfo = renderingInfo
                    DeviceApi.vkCmdBeginRendering (context.RenderCommandBuffer, &&renderingInfo)
                DeviceApi.vkCmdSetViewport (context.RenderCommandBuffer, 0u, 1u, &&vkViewport)
                DeviceApi.vkCmdSetScissor (context.RenderCommandBuffer, 0u, 1u, &&scissor)
                
                // set up pipeline
                DeviceApi.vkCmdBindPipeline (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                    
                // bind vertex and index buffers
                let mutable vertexBuffer = vertices.VkBuffer
                let mutable vertexOffset = 0UL
                DeviceApi.vkCmdBindVertexBuffers (context.RenderCommandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
                DeviceApi.vkCmdBindIndexBuffer (context.RenderCommandBuffer, indices.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
                DeviceApi.vkCmdBindDescriptorSets (context.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

                // draw
                DeviceApi.vkCmdDrawIndexed (context.RenderCommandBuffer, 6u, 1u, 0u, 0, 0u)
            
                // tear down render
                DeviceApi.vkCmdEndRendering context.RenderCommandBuffer

                // report drawing
                Hl.reportDrawCall 1 true

                // advance pipeline
                Pipeline.advance pipeline

                // advance rendering command buffer
                VulkanContext.advanceRenderCommandBuffer context

            // abort
            | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")