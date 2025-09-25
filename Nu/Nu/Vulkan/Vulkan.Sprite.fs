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

    /// Create a sprite pipeline.
    let CreateSpritePipeline (vkc : Hl.VulkanContext) =
        
        // create sprite pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SpriteShaderFilePath
                true true [|Pipeline.Transparent|]
                [|Hl.makeVertexBindingVertex 0 (sizeof<single> * 2)|]
                [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT 0|]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 2 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
                  Hl.makeDescriptorBindingFragment 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1|]
                [||] vkc
        
        // create sprite uniform buffers
        let modelViewProjectionUniform = Buffer.BufferAccumulator.create (sizeof<single> * 16) Buffer.Uniform vkc
        let texCoords4Uniform = Buffer.BufferAccumulator.create (sizeof<single> * 4) Buffer.Uniform vkc
        let colorUniform = Buffer.BufferAccumulator.create (sizeof<single> * 4) Buffer.Uniform vkc

        // fin
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, pipeline)
    
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
         viewProjectionAbsolute : Matrix4x4 inref,
         viewProjectionClip : Matrix4x4 inref,
         modelViewProjection : single array,
         insetOpt : Box2 voption inref,
         clipOpt : Box2 voption inref,
         color : Color inref,
         flip,
         textureWidth,
         textureHeight,
         texture : Texture.VulkanTexture,
         viewport : Viewport,
         modelViewProjectionUniform : Buffer.BufferAccumulator,
         texCoords4Uniform : Buffer.BufferAccumulator,
         colorUniform : Buffer.BufferAccumulator,
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

        // init render
        let mutable renderArea = VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable rendering = Hl.makeRenderingInfo vkc.SwapchainImageView renderArea None
        Vulkan.vkCmdBeginRendering (vkc.RenderCommandBuffer, asPointer &rendering)
        
        // update uniform buffers
        Buffer.BufferAccumulator.uploadArray drawIndex 0 modelViewProjection modelViewProjectionUniform vkc
        Buffer.BufferAccumulator.uploadArray drawIndex 0 [|texCoords.Min.X; texCoords.Min.Y; texCoords.Size.X; texCoords.Size.Y|] texCoords4Uniform vkc
        Buffer.BufferAccumulator.uploadArray drawIndex 0 [|color.R; color.G; color.B; color.A|] colorUniform vkc

        // update descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 modelViewProjectionUniform pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 1 texCoords4Uniform pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 3 colorUniform pipeline vkc
        Pipeline.Pipeline.writeDescriptorTexture 2 drawIndex texture pipeline vkc
        
        // bind pipeline
        let cb = vkc.RenderCommandBuffer
        let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.Transparent pipeline
        Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

        // make viewport and scissor
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea
        match clipOpt with
        | ValueSome clip ->
            let viewProjection = if absolute then viewProjectionAbsolute else viewProjectionClip
            let minClip = Vector4.Transform (Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection)
            let minNdc = minClip / minClip.W * single viewport.DisplayScalar
            let minScissor = (minNdc.V2 + v2One) * 0.5f * viewport.Bounds.Size.V2
            let sizeScissor = clip.Size * v2Dup (single viewport.DisplayScalar)
            let offset = viewport.Bounds.Min
            scissor <-
                VkRect2D
                    ((minScissor.X |> round |> int) + offset.X,
                     (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                     uint sizeScissor.X,
                     uint sizeScissor.Y)
            scissor <- Hl.clampRectToRect renderArea scissor
        | ValueNone -> ()
        
        // only draw if scissor is valid
        if Hl.isValidRect scissor then

            // set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // push draw index
            let mutable drawIndex = drawIndex
            Vulkan.vkCmdPushConstants (cb, pipeline.PipelineLayout, Vulkan.VK_SHADER_STAGE_VERTEX_BIT ||| Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT, 0u, 4u, asVoidPtr &drawIndex)
            
            // bind vertex and index buffer
            let mutable vertexBuffer = vertices.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, indices.VkBuffer, 0UL, Vulkan.VK_INDEX_TYPE_UINT32)

            // bind descriptor set
            let mutable descriptorSet = pipeline.DescriptorSet
            Vulkan.vkCmdBindDescriptorSets
                (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS,
                 pipeline.PipelineLayout, 0u,
                 1u, asPointer &descriptorSet,
                 0u, nullPtr)
            
            // draw
            Vulkan.vkCmdDrawIndexed (cb, 6u, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
        
            // reset scissor
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &renderArea)

        // end render
        Vulkan.vkCmdEndRendering vkc.RenderCommandBuffer