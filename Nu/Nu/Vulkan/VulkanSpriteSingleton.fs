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
type SpriteVert =
    [<FieldOffset(0)>] val mutable modelViewProjection : Matrix4x4
    [<FieldOffset(64)>] val mutable texCoords4 : Vector4
    
[<Struct; StructLayout (LayoutKind.Explicit)>]
type SpriteFrag =
    [<FieldOffset(0)>] val mutable color : Vector4

[<RequireQualifiedAccess>]
module SpriteSingleton =

    let VertexSize = sizeof<single> * 2
    
    /// Create a sprite singleton pipeline.
    let createSpriteSingletonPipeline (vkc : VulkanContext) =

        // create sprite uniform buffers
        let spriteVertUniform = Buffer.create sizeof<SpriteVert> Storage vkc
        let spriteFragUniform = Buffer.create sizeof<SpriteFrag> Storage vkc
        
        // create sprite pipeline
        let pipeline =
            Pipeline.create
                Constants.Paths.SpriteShaderFilePath
                [|VulkanTransparent|] [|true|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single2 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|vkc.SwapFormat|] None
                [|spriteVertUniform; spriteFragUniform|]
                vkc

        // fin
        (spriteVertUniform, spriteFragUniform, pipeline)
    
    /// Create a sprite singleton quad for rendering to this pipeline.
    let createSpriteQuad onlyUpperRightQuadrant vkc =

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
        let vertexBuffer = Buffer.createVertexStagedFromArray vertexData vkc
        let indexBuffer = Buffer.createIndexStagedFromArray indexData vkc
        
        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a single sprite.
    let drawSpriteSingleton
        (vertices : Nu.Vulkan.Buffer,
         indices : Nu.Vulkan.Buffer,
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
         spriteVertUniform : Nu.Vulkan.Buffer,
         spriteFragUniform : Nu.Vulkan.Buffer,
         pipeline : Pipeline,
         vkc : VulkanContext) =

        // only draw if scissor (and therefore also viewport) is valid
        let mutable renderArea = VkRect2D (viewport.Inner.Min.X, viewport.Outer.Max.Y - viewport.Inner.Max.Y, uint viewport.Inner.Size.X, uint viewport.Inner.Size.Y)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea
        match clipOpt with
        | ValueSome clip ->
            let viewProjection = if absolute then viewProjectionClipAbsolute else viewProjectionClipRelative
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
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.DrawIndex pipeline vkc $ fun vkSet ->
                    let spriteVert = SpriteVert (modelViewProjection = modelViewProjection, texCoords4 = v4 texCoords.Min.X texCoords.Min.Y texCoords.Size.X texCoords.Size.Y)
                    let spriteFrag = SpriteFrag (color = color.V4)
                    Buffer.uploadValue spriteVert spriteVertUniform vkc
                    Buffer.uploadValue spriteFrag spriteFragUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 spriteVertUniform vkSet vkc
                    Pipeline.writeDescriptorStorageBuffer 1 0 spriteFragUniform vkSet vkc

                // specify material
                let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 texture pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampledImage 0 0 texture vkSet vkc

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 sampler vkSet vkc
                    
                // set up render
                let mutable rendering = Hl.makeRenderingInfo [|vkc.SwapchainImageView|] None renderArea None
                Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
                Vulkan.vkCmdBindPipeline (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetViewport (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (vkc.RenderCommandBuffer, 0u, 1u, asPointer &scissor)
                    
                // bind vertex and index buffers
                let mutable vertexBuffer = vertices.VkBuffer
                let mutable vertexOffset = 0UL
                Vulkan.vkCmdBindVertexBuffers (vkc.RenderCommandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                Vulkan.vkCmdBindIndexBuffer (vkc.RenderCommandBuffer, indices.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (vkc.RenderCommandBuffer, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 2u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)

                // draw
                Vulkan.vkCmdDrawIndexed (vkc.RenderCommandBuffer, 6u, 1u, 0u, 0, 0u)
            
                // tear down render
                Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer

                // advance pipeline
                Pipeline.advance 1 pipeline

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."