﻿// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace OpenGL
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    /// Describes a sky box shader that's loaded into GPU.
    type SkyBoxShader =
        { ViewUniform : int
          ProjectionUniform : int
          ColorUniform : int
          BrightnessUniform : int
          CubeMapUniform : int
          SkyBoxShader : uint }

    /// Create a sky box shader.
    let CreateSkyBoxShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let colorUniform = Gl.GetUniformLocation (shader, "color")
        let brightnessUniform = Gl.GetUniformLocation (shader, "brightness")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          ColorUniform = colorUniform
          BrightnessUniform = brightnessUniform
          CubeMapUniform = cubeMapUniform
          SkyBoxShader = shader }

    /// Draw a sky box.
    let DrawSkyBox
        (view : single array,
         projection : single array,
         color : Color,
         brightness : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         shader : SkyBoxShader,
         vao : uint) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Hl.Assert ()

        // setup vao
        Gl.BindVertexArray vao
        
        // setup shader
        Gl.UseProgram shader.SkyBoxShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform3 (shader.ColorUniform, color.R, color.G, color.B)
        Gl.Uniform1 (shader.BrightnessUniform, brightness)
        Gl.Uniform1 (shader.CubeMapUniform, 0)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, CubeMap.VertexSize)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown vao
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest