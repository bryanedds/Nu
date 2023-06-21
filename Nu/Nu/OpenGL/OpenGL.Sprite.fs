// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Sprite =

    /// Create a sprite shader with attributes:
    ///     0: vec2 position
    /// and uniforms:
    ///     a: mat4 modelViewProjection
    ///     b: vec2 texCoords4
    ///     c: vec4 color
    ///     d: sampler2D tex
    let CreateSpriteShader () =

        // vertex shader code
        // TODO: let's put this code into a .glsl file and load it from there.
        let vertexShaderStr =
            [Constants.OpenGl.GlslVersionPragma
             ""
             "const int VERTS = 4;"
             ""
             "const vec4 FILTERS[VERTS] ="
             "    vec4[VERTS]("
             "        vec4(1.0, 1.0, 0.0, 0.0),"
             "        vec4(1.0, 1.0, 1.0, 0.0),"
             "        vec4(1.0, 1.0, 1.0, 1.0),"
             "        vec4(1.0, 1.0, 0.0, 1.0));"
             ""
             "in vec2 position;"
             "uniform mat4 modelViewProjection;"
             "uniform vec4 texCoords4;"
             "out vec2 texCoords;"
             "void main()"
             "{"
             "    int vertexId = gl_VertexID % VERTS;"
             "    vec4 filt = FILTERS[vertexId];"
             "    gl_Position = modelViewProjection * vec4(position.x, position.y, 0, 1);"
             "    texCoords = vec2(texCoords4.x * filt.x + texCoords4.z * filt.z, texCoords4.y * filt.y + texCoords4.w * filt.w);"
             "}"] |> String.join "\n"

        // fragment shader code
        let fragmentShaderStr =
            [Constants.OpenGl.GlslVersionPragma
             "uniform sampler2D tex;"
             "uniform vec4 color;"
             "in vec2 texCoords;"
             "out vec4 frag;"
             "void main()"
             "{"
             "    frag = color * texture(tex, texCoords);"
             "}"] |> String.join "\n"

        // create shader
        let shader = Shader.CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr)
        let modelViewProjectionUniform = Gl.GetUniformLocation (shader, "modelViewProjection")
        let texCoords4Uniform = Gl.GetUniformLocation (shader, "texCoords4")
        let colorUniform = Gl.GetUniformLocation (shader, "color")
        let texUniform = Gl.GetUniformLocation (shader, "tex")
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader)

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

        // initialize vao
        let vao = Gl.GenVertexArray ()
        Gl.BindVertexArray vao
        Hl.Assert ()

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

        // finalize vao
        Gl.EnableVertexAttribArray 0u
        Gl.VertexAttribPointer (0u, 2, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // fin
        (vertexBuffer, indexBuffer, vao)

    /// Draw a sprite whose indices and vertices were created by Gl.CreateSpriteQuad and whose uniforms and shader match those of CreateSpriteShader.
    let DrawSprite (vertices, indices, vao, modelViewProjection : single array, insetOpt : Box2 ValueOption, color : Color, flip, textureWidth, textureHeight, texture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader) =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            match insetOpt with
            | ValueSome inset ->
                let texelWidth = 1.0f / single textureWidth
                let texelHeight = 1.0f / single textureHeight
                let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
                let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
                let px = inset.Min.X * texelWidth + borderWidth
                let py = (inset.Min.Y + inset.Size.Y) * texelHeight - borderHeight
                let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                Box2 (px, py, sx, sy)
            | ValueNone -> Box2 (0.0f, 1.0f, 1.0f, -1.0f) // TODO: shouldn't we still be using borders?
        
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
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader
        Gl.UniformMatrix4 (modelViewProjectionUniform, false, modelViewProjection)
        Gl.Uniform4 (texCoords4Uniform, texCoords.Min.X, texCoords.Min.Y, texCoords.Size.X, texCoords.Size.Y)
        Gl.Uniform4 (colorUniform, color.R, color.G, color.B, color.A)
        Gl.Uniform1 (texUniform, 0)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, texture)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray vao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, vertices)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indices)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (PrimitiveType.Triangles, 6, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.Texture2d, 0u)
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown state
        Gl.Disable EnableCap.CullFace
        Gl.Disable EnableCap.Blend
        Gl.BlendFunc (BlendingFactor.One, BlendingFactor.Zero)
        Gl.BlendEquation BlendEquationMode.FuncAdd