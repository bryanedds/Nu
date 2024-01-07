// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module Filter =

    /// Describes a box filter shader that's loaded into GPU.
    type FilterBoxShader =
        { InputTextureUniform : int
          FilterBoxShader : uint }

    /// Describes a gaussian filter shader that's loaded into GPU.
    type FilterGaussianShader =
        { ScaleUniform : int
          InputTextureUniform : int
          FilterGaussianShader : uint }

    /// Describes an fxaa shader that's loaded into GPU.
    type FilterFxaaShader =
        { InputTextureUniform : int
          FilterFxaaShader : uint }

    /// Create a filter box shader.
    let CreateFilterBoxShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { InputTextureUniform = inputTextureUniform
          FilterBoxShader = shader }

    /// Create a filter gaussian shader.
    let CreateFilterGaussianShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let scaleUniform = Gl.GetUniformLocation (shader, "scale")
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { ScaleUniform = scaleUniform
          InputTextureUniform = inputTextureUniform
          FilterGaussianShader = shader }

    /// Create a filter fxaa shader.
    let CreateFilterFxaaShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { InputTextureUniform = inputTextureUniform
          FilterFxaaShader = shader }