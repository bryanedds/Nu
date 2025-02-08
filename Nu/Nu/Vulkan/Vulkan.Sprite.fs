// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open System
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Sprite =

    /// Create a sprite pipeline.
    let CreateSpritePipeline (vkg : Hl.VulkanGlobal) =
        
        // create sprite pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SpriteShaderFilePath
                true [|Pipeline.Transparent|]
                [|Hl.makeVertexBindingVertex 0 (sizeof<single> * 2)|]
                [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT 0|]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 2 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
                  Hl.makeDescriptorBindingFragment 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1|]
                [||] vkg.RenderPass vkg.Device
        
        // create sprite uniform buffers
        let modelViewProjectionUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 16) vkg.VmaAllocator
        let texCoords4Uniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 4) vkg.VmaAllocator
        let colorUniform = Hl.AllocatedBuffer.createUniform (sizeof<single> * 4) vkg.VmaAllocator

        // write sprite descriptor set
        Pipeline.Pipeline.writeDescriptorUniform 0 0 modelViewProjectionUniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 1 0 texCoords4Uniform pipeline vkg.Device
        Pipeline.Pipeline.writeDescriptorUniform 3 0 colorUniform pipeline vkg.Device

        // fin
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, pipeline)
    
    /// Create a sprite quad for rendering to a pipeline matching the one created with CreateSpritePipeline.
    let CreateSpriteQuad onlyUpperRightQuadrant vkg =

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
        let vertexBuffer = Hl.AllocatedBuffer.createVertexStagedFromArray vertexData vkg
        let indexBuffer = Hl.AllocatedBuffer.createIndexStagedFromArray indexData vkg
        
        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a sprite whose indices and vertices were created by Vulkan.CreateSpriteQuad and whose uniforms and pipeline match those of CreateSpritePipeline.
    let DrawSprite
        (vertices : Hl.AllocatedBuffer,
         indices : Hl.AllocatedBuffer,
         modelViewProjection : single array,
         insetOpt : Box2 ValueOption,
         color : Color,
         flip,
         textureWidth,
         textureHeight,
         texture : Texture.VulkanTexture,
         modelViewProjectionUniform : Hl.AllocatedBuffer,
         texCoords4Uniform : Hl.AllocatedBuffer,
         colorUniform : Hl.AllocatedBuffer,
         pipeline : Pipeline.Pipeline,
         vkg : Hl.VulkanGlobal) =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            let texelWidth = 1.0f / single textureWidth
            let texelHeight = 1.0f / single textureHeight
            let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
            let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
            match insetOpt with
            | ValueSome inset ->
                let px = inset.Min.X * texelWidth + borderWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight - borderHeight
                let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                Box2 (px, py, sx, sy)
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

        // update uniform buffers
        Hl.AllocatedBuffer.uploadArray 0 modelViewProjection modelViewProjectionUniform
        Hl.AllocatedBuffer.uploadArray 0 [|texCoords.Min.X; texCoords.Min.Y; texCoords.Size.X; texCoords.Size.Y|] texCoords4Uniform
        Hl.AllocatedBuffer.uploadArray 0 [|color.R; color.G; color.B; color.A|] colorUniform

        // write texture to descriptor set
        Pipeline.Pipeline.writeDescriptorTexture 2 0 texture pipeline vkg.Device
        
        // bind pipeline
        let cb = vkg.RenderCommandBuffer
        let vkPipeline = Pipeline.Pipeline.getPipeline Pipeline.Transparent pipeline
        Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

        // set viewport and scissor
        let mutable renderArea = VkRect2D (VkOffset2D.Zero, vkg.SwapExtent)
        let mutable viewport = Hl.makeViewport renderArea
        Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &viewport)
        Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &renderArea)
        
        // bind vertex and index buffer
        let mutable vertexBuffer = vertices.Buffer
        let mutable vertexOffset = 0UL
        Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
        Vulkan.vkCmdBindIndexBuffer (cb, indices.Buffer, 0UL, Vulkan.VK_INDEX_TYPE_UINT32)

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