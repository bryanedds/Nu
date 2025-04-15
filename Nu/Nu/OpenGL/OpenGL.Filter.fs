// Nu Game Engine.
// Copyright (C) Bryan Edds.

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

    /// Describes an box down-sampling filter shader that's loaded into GPU.
    type FilterBilateralDownSampleShader =
        { ColorTextureUniform : int
          DepthTextureUniform : int
          FilterBilateralDownSampleShader : uint }

    /// Describes an bilateral up-sampling filter shader that's loaded into GPU.
    type FilterBilateralUpSampleShader =
        { ColorDownSampledTextureUniform : int
          DepthDownSampledTextureUniform : int
          DepthTextureUniform : int
          FilterBilateralUpSampleShader : uint }

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

    /// Create a filter bilateral down-sample shader.
    let CreateFilterBilateralDownSampleShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let colorTextureUniform = Gl.GetUniformLocation (shader, "colorTexture")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")

        // make shader record
        { ColorTextureUniform = colorTextureUniform
          DepthTextureUniform = depthTextureUniform
          FilterBilateralDownSampleShader = shader }

    /// Create a filter bilateral up-sample shader.
    let CreateFilterBilateralUpSampleShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let colorDownSampledTextureUniform = Gl.GetUniformLocation (shader, "colorDownSampledTexture")
        let depthDownSampledTextureUniform = Gl.GetUniformLocation (shader, "depthDownSampledTexture")
        let depthTextureUniform = Gl.GetUniformLocation (shader, "depthTexture")

        // make shader record
        { ColorDownSampledTextureUniform = colorDownSampledTextureUniform
          DepthDownSampledTextureUniform = depthDownSampledTextureUniform
          DepthTextureUniform = depthTextureUniform
          FilterBilateralUpSampleShader = shader }

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