// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace OpenGL
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Sprite =

    /// Create a sprite shader.
    let CreateSpriteShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let modelViewProjectionUniform = Gl.GetUniformLocation (shader, "modelViewProjection")
        let texCoords4Uniform = Gl.GetUniformLocation (shader, "texCoords4")
        let colorUniform = Gl.GetUniformLocation (shader, "color")
        let texUniform = Gl.GetUniformLocation (shader, "tex")

        // make sprite shader tuple
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader)

    let VertexSize = sizeof<single> * 2

    let CreateSpriteVao () =

        // create vao
        let vao =  [|0u|]
        Gl.CreateVertexArrays vao
        let vao = vao.[0]

        // per vertex
        Gl.VertexArrayAttribFormat (vao, 0u, 2, VertexAttribType.Float, false, uint 0)
        Gl.VertexArrayAttribBinding (vao, 0u, 0u)
        Gl.EnableVertexArrayAttrib (vao, 0u)

        // fin
        vao

    /// Create a sprite quad for rendering to a shader matching the one created with Hl.CreateSpriteShader.
    let CreateSpriteQuad onlyUpperRightQuadrant =

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
        let vertexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
        let vertexSize = sizeof<single> * 2
        let vertexDataSize = vertexSize * 4
        let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        try Gl.BufferData (BufferTarget.ArrayBuffer, uint vertexDataSize, vertexDataPtr.AddrOfPinnedObject (), BufferUsage.StaticDraw)
        finally vertexDataPtr.Free ()
        Hl.Assert ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        let indexBuffer = Gl.GenBuffer ()
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
        let indexDataSize = uint (indexData.Length * sizeof<uint>)
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), BufferUsage.StaticDraw)
        finally indexDataPtr.Free ()
        Hl.Assert ()

        // fin
        (vertexBuffer, indexBuffer)

    /// Draw a sprite whose indices and vertices were created by Gl.CreateSpriteQuad and whose uniforms and shader match those of CreateSpriteShader.
    let DrawSprite
        (vertices,
         indices,
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
         texture : Texture.Texture,
         viewport : Viewport,
         modelViewProjectionUniform,
         texCoords4Uniform,
         colorUniform,
         textureUniform,
         shader,
         vao) =

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

        // setup state
        Gl.BlendEquation BlendEquationMode.FuncAdd
        Gl.BlendFunc (BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha)
        Gl.Enable EnableCap.Blend
        Gl.Enable EnableCap.CullFace
        match clipOpt with
        | ValueSome clip ->
            let viewProjection = if absolute then viewProjectionAbsolute else viewProjectionClip
            let minClip = Vector4.Transform (Vector4 (clip.Min, 0.0f, 1.0f), viewProjection)
            let minNdc = minClip / minClip.W * single viewport.DisplayScalar
            let minScissor = (minNdc.V2 + v2One) * 0.5f * viewport.Inset.Size.V2
            let sizeScissor = clip.Size * v2Dup (single viewport.DisplayScalar)
            let offset = viewport.Inset.Min
            Gl.Enable EnableCap.ScissorTest
            Gl.Scissor
                ((minScissor.X |> round |> int) + offset.X,
                 (minScissor.Y |> round |> int) + offset.Y,
                 int sizeScissor.X,
                 int sizeScissor.Y)
        | ValueNone -> ()
        Hl.Assert ()

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader
        Gl.UniformMatrix4 (modelViewProjectionUniform, false, modelViewProjection)
        Gl.Uniform4 (texCoords4Uniform, texCoords.Min.X, texCoords.Min.Y, texCoords.Size.X, texCoords.Size.Y)
        Gl.Uniform4 (colorUniform, color.R, color.G, color.B, color.A)
        Gl.Uniform1 (textureUniform, 0)
        Hl.Assert ()
        
        // setup texture
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, texture.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, vertices, 0, VertexSize)
        Gl.VertexArrayElementBuffer (vao, indices)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (PrimitiveType.Triangles, 6, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown state
        Gl.BlendEquation BlendEquationMode.FuncAdd
        Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)
        Gl.Disable EnableCap.Blend
        Gl.Disable EnableCap.CullFace
        Gl.Disable EnableCap.ScissorTest