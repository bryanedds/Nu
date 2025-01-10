// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Vortice.Vulkan
open System
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Sprite =

    /// Create a sprite quad for rendering to the sprite shader.
    let CreateSpriteQuad onlyUpperRightQuadrant allocator =

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

        // create vertex buffer
        let vertexSize = sizeof<single> * 2
        let vertexDataSize = vertexSize * 4
        let vertexBuffer = Hl.AllocatedBuffer.createVertex true vertexDataSize allocator
        let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        
        // TODO: DJL: confirm that this try block and the one for index buffer are still appropriate.
        try Hl.AllocatedBuffer.upload 0 vertexDataSize (vertexDataPtr.AddrOfPinnedObject ()) vertexBuffer allocator
        finally vertexDataPtr.Free ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        let indexDataSize = indexData.Length * sizeof<uint>
        let indexBuffer = Hl.AllocatedBuffer.createIndex true indexDataSize allocator
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try Hl.AllocatedBuffer.upload 0 indexDataSize (indexDataPtr.AddrOfPinnedObject ()) indexBuffer allocator
        finally indexDataPtr.Free ()

        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a sprite whose indices and vertices were created by Vulkan.CreateSpriteQuad using the sprite shader.
    let DrawSprite (vertices, indices, vao, modelViewProjection : single array, insetOpt : Box2 ValueOption, color : Color, flip, textureWidth, textureHeight, texture : Texture.Texture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, textureUniform, shader) =

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


        ()