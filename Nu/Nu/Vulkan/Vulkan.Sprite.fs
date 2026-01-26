// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Sprite =

    let VertexSize = sizeof<single> * 2
    
    /// Create a sprite pipeline.
    let CreateSpritePipeline (vkc : Hl.VulkanContext) =
        
        // create sprite pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SpriteShaderFilePath
                [|Pipeline.Transparent|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single2 0|]|]
                [|Pipeline.descriptorSet true
                    [|Pipeline.descriptor 0 Hl.CombinedImageSampler Hl.FragmentStage 1|]|]
                [|Pipeline.pushConstant 0 (20 * sizeof<single>) Hl.VertexStage
                  Pipeline.pushConstant (20 * sizeof<single>) (4 * sizeof<single> + sizeof<int>) Hl.FragmentStage|]
                vkc.SwapFormat None vkc
        
        // fin
        pipeline
    
    /// Create a sprite quad for rendering to a pipeline matching the one created with CreateSpritePipeline.
    let CreateSpriteQuad onlyUpperRightQuadrant vkc =

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
        let vertexBuffer = Buffer.Buffer.createVertexStagedFromArray vertexData vkc
        let indexBuffer = Buffer.Buffer.createIndexStagedFromArray indexData vkc
        
        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a sprite whose indices and vertices were created by Vulkan.CreateSpriteQuad and whose uniforms and pipeline match those of CreateSpritePipeline.
    let DrawSprite
        (drawIndex : int,
         vertices : Buffer.Buffer,
         indices : Buffer.Buffer,
         absolute,
         viewProjectionClipAbsolute : Matrix4x4 inref,
         viewProjectionClipRelative : Matrix4x4 inref,
         modelViewProjection : single array,
         insetOpt : Box2 voption inref,
         clipOpt : Box2 voption inref,
         color : Color inref,
         flip,
         textureWidth,
         textureHeight,
         texture : Texture.Texture,
         viewport : Viewport,
         pipeline : Pipeline.Pipeline,
         vkc : Hl.VulkanContext) =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            let texelWidth = 1.0f / single textureWidth
            let texelHeight = 1.0f / single textureHeight
            let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
            let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
            match insetOpt with
            | ValueSome inset ->
                let mx = inset.Min.X * texelWidth + borderWidth
                let my = (inset.Min.Y + inset.Size.Y) * texelHeight - borderHeight
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
            | FlipNone -> struct (false, false)
            | FlipH -> struct (true, false)
            | FlipV -> struct (false, true)
            | FlipHV -> struct (true, true)

        // compute tex coords
        let texCoords =
            box2
                (v2
                    (if flipH then texCoordsUnflipped.Min.X + texCoordsUnflipped.Size.X else texCoordsUnflipped.Min.X)
                    (if flipV then texCoordsUnflipped.Min.Y + texCoordsUnflipped.Size.Y else texCoordsUnflipped.Min.Y))
                (v2
                    (if flipH then -texCoordsUnflipped.Size.X else texCoordsUnflipped.Size.X)
                    (if flipV then -texCoordsUnflipped.Size.Y else texCoordsUnflipped.Size.Y))

        // bind texture
        Pipeline.Pipeline.writeDescriptorTexture drawIndex 0 0 texture pipeline vkc

        // make viewport and scissor
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
        
        // only draw if scissor (and therefore also viewport) is valid
        if Hl.validateRect scissor then

            // init render
            let cb = vkc.RenderCommandBuffer
            let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView None renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)
            
            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.Transparent true pipeline
            Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

            // set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // bind vertex and index buffer
            let mutable vertexBuffer = vertices.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, indices.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor set
            let mutable descriptorSet = pipeline.VkDescriptorSet 0
            Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, pipeline.PipelineLayout, 0u, 1u, asPointer &descriptorSet, 0u, nullPtr)
            
            // push constants
            let transformTexCoords = Array.append modelViewProjection [|texCoords.Min.X; texCoords.Min.Y; texCoords.Size.X; texCoords.Size.Y|]
            let transformTexCoordsPin = new ArrayPin<_> (transformTexCoords)
            let colorArray = [|color.R; color.G; color.B; color.A|]
            let colorArrayPin = new ArrayPin<_> (colorArray)
            let mutable drawIndex = drawIndex
            Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Hl.VertexStage.VkShaderStageFlags, 0u, 80u, transformTexCoordsPin.VoidPtr)
            
            // TODO: DJL: use stack alloc to upload all at once.
            Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Hl.FragmentStage.VkShaderStageFlags, 80u, 16u, colorArrayPin.VoidPtr)
            Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Hl.FragmentStage.VkShaderStageFlags, 96u, 4u, asVoidPtr &drawIndex)
            
            // draw
            Vulkan.vkCmdDrawIndexed (cb, 6u, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
        
            // end render
            Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer