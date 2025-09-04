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

    /// Describes a gaussian filter shader that operates on texture arrays that's loaded into GPU.
    type FilterGaussianArrayShader =
        { ScaleUniform : int
          InputIndexUniform : int
          InputTextureArrayUniform : int
          FilterGaussianArrayShader : uint }

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

    /// Create a filter gaussian shader that operates on texture arrays.
    let CreateFilterGaussianArrayShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let scaleUniform = Gl.GetUniformLocation (shader, "scale")
        let inputIndexUniform = Gl.GetUniformLocation (shader, "inputIndex")
        let inputTextureArrayUniform = Gl.GetUniformLocation (shader, "inputTextureArray")

        // make shader record
        { ScaleUniform = scaleUniform
          InputIndexUniform = inputIndexUniform
          InputTextureArrayUniform = inputTextureArrayUniform
          FilterGaussianArrayShader = shader }

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

    type FilterShaders =
        { FilterBox1dShader : FilterBoxShader
          FilterGaussian2dShader : FilterGaussianShader
          FilterGaussianArray2dShader : FilterGaussianArrayShader
          FilterBilateralDownSample4dShader : FilterBilateralDownSampleShader
          FilterBilateralUpSample4dShader : FilterBilateralUpSampleShader
          FilterFxaaShader : FilterFxaaShader
          FilterGaussian4dShader : FilterGaussianShader }

    let CreateFilterShaders () =

        // create individual shaders
        let filterBox1dShader = CreateFilterBoxShader Constants.Paths.FilterBox1dShaderFilePath
        let filterGaussian2dShader = CreateFilterGaussianShader Constants.Paths.FilterGaussian2dShaderFilePath
        let filterGaussianArray2dShader = CreateFilterGaussianArrayShader Constants.Paths.FilterGaussianArray2dShaderFilePath
        let filterBilateralDownSample4dShader = CreateFilterBilateralDownSampleShader Constants.Paths.FilterBilateralDownSample4dShaderFilePath
        let filterBilateralUpSample4dShader = CreateFilterBilateralUpSampleShader Constants.Paths.FilterBilateralUpSample4dShaderFilePath
        let filterFxaaShader = CreateFilterFxaaShader Constants.Paths.FilterFxaaShaderFilePath
        let filterGaussian4dShader = CreateFilterGaussianShader Constants.Paths.FilterGaussian4dShaderFilePath

        // fin
        { FilterBox1dShader = filterBox1dShader
          FilterGaussian2dShader = filterGaussian2dShader
          FilterGaussianArray2dShader = filterGaussianArray2dShader
          FilterBilateralDownSample4dShader = filterBilateralDownSample4dShader
          FilterBilateralUpSample4dShader = filterBilateralUpSample4dShader
          FilterFxaaShader = filterFxaaShader
          FilterGaussian4dShader = filterGaussian4dShader }

    let DestroyFilterShaders (shaders : FilterShaders) =
        Gl.DeleteProgram shaders.FilterBox1dShader.FilterBoxShader
        Gl.DeleteProgram shaders.FilterGaussian2dShader.FilterGaussianShader
        Gl.DeleteProgram shaders.FilterGaussianArray2dShader.FilterGaussianArrayShader
        Gl.DeleteProgram shaders.FilterBilateralDownSample4dShader.FilterBilateralDownSampleShader
        Gl.DeleteProgram shaders.FilterBilateralUpSample4dShader.FilterBilateralUpSampleShader
        Gl.DeleteProgram shaders.FilterFxaaShader.FilterFxaaShader
        Gl.DeleteProgram shaders.FilterGaussian4dShader.FilterGaussianShader