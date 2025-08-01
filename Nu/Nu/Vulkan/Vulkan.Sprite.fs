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
                true [|Pipeline.Transparent|]
                [|Hl.makeVertexBindingVertex 0 (sizeof<single> * 2)|]
                [|Hl.makeVertexAttribute 0 0 Vulkan.VK_FORMAT_R32G32_SFLOAT 0|]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 2 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
                  Hl.makeDescriptorBindingFragment 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1|]
                [||] vkc.RenderPass vkc.Device
        
        // create sprite uniform buffers
        let modelViewProjectionUniform = VulkanMemory.FifBuffer.createUniform (sizeof<single> * 16) vkc
        let texCoords4Uniform = VulkanMemory.FifBuffer.createUniform (sizeof<single> * 4) vkc
        let colorUniform = VulkanMemory.FifBuffer.createUniform (sizeof<single> * 4) vkc

        // write sprite descriptor set
        Pipeline.Pipeline.writeDescriptorUniform 0 0 modelViewProjectionUniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 1 0 texCoords4Uniform.PerFrameBuffers pipeline vkc.Device
        Pipeline.Pipeline.writeDescriptorUniform 3 0 colorUniform.PerFrameBuffers pipeline vkc.Device

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
        let vertexBuffer = VulkanMemory.Buffer.createVertexStagedFromArray vertexData vkc
        let indexBuffer = VulkanMemory.Buffer.createIndexStagedFromArray indexData vkc
        
        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a sprite whose indices and vertices were created by Vulkan.CreateSpriteQuad and whose uniforms and pipeline match those of CreateSpritePipeline.
    let DrawSprite
        (vertices : VulkanMemory.Buffer,
         indices : VulkanMemory.Buffer,
         viewProjection : Matrix4x4 inref,
         modelViewProjection : single array,
         insetOpt : Box2 voption inref,
         clipOpt : Box2 voption inref,
         color : Color inref,
         flip,
         textureWidth,
         textureHeight,
         texture : Texture.VulkanTexture,
         viewport : Viewport,
         modelViewProjectionUniform : VulkanMemory.FifBuffer,
         texCoords4Uniform : VulkanMemory.FifBuffer,
         colorUniform : VulkanMemory.FifBuffer,
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

        // update uniform buffers
        VulkanMemory.FifBuffer.uploadArray 0 modelViewProjection modelViewProjectionUniform vkc
        VulkanMemory.FifBuffer.uploadArray 0 [|texCoords.Min.X; texCoords.Min.Y; texCoords.Size.X; texCoords.Size.Y|] texCoords4Uniform vkc
        VulkanMemory.FifBuffer.uploadArray 0 [|color.R; color.G; color.B; color.A|] colorUniform vkc

        // write texture to descriptor set
        Pipeline.Pipeline.writeDescriptorTextureSingleFrame 2 0 texture pipeline vkc.Device
        
        // bind pipeline
        let cb = vkc.RenderCommandBuffer
        let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.Transparent pipeline
        Vulkan.vkCmdBindPipeline (cb, Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS, vkPipeline)

        // set viewport and scissor
        let mutable renderArea = VkRect2D (viewport.Bounds.Min.X, viewport.Bounds.Min.Y, uint viewport.Bounds.Size.X, uint viewport.Bounds.Size.Y)
        let mutable vkViewport = Hl.makeViewport renderArea
        let mutable scissor = renderArea
        match clipOpt with
        | ValueSome clip ->
            let minClip = Vector4.Transform (Vector4 (clip.Min.X, clip.Max.Y, 0.0f, 1.0f), viewProjection)
            let minNdc = minClip / minClip.W * single viewport.DisplayScalar
            let minScissor = (minNdc.V2 + v2One) * 0.5f * viewport.Bounds.Size.V2 // TODO: DJL: clamp values.
            let sizeScissor = clip.Size * v2Dup (single viewport.DisplayScalar)
            let offset = viewport.Bounds.Min
            scissor <-
                VkRect2D
                    ((minScissor.X |> round |> int) + offset.X,
                     (single renderArea.extent.height - minScissor.Y |> round |> int) + offset.Y,
                     uint sizeScissor.X,
                     uint sizeScissor.Y)
        | ValueNone -> ()
        Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
        Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
        
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