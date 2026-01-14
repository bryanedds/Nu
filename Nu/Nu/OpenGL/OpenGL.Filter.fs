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

    /// Describes a bloom extract filter shader that's loaded into GPU.
    type FilterBloomExtractShader =
        { ThresholdUniform : int
          ColorTextureUniform : int
          FilterBloomExtractShader : uint }

    /// Describes a bloom down-sampling filter shader that's loaded into GPU.
    type FilterBloomDownSampleShader =
        { SampleLevelUniform : int
          KarisAverageEnabledUniform : int
          SourceResolutionUniform : int
          SourceTextureUniform : int
          FilterBloomDownSampleShader : uint }

    /// Describes a bloom up-sampling filter shader that's loaded into GPU.
    type FilterBloomUpSampleShader =
        { FilterRadiusUniform : int
          SourceTextureUniform : int
          FilterBloomUpSampleShader : uint }

    /// Describes a bloom apply filter shader that's loaded into GPU.
    type FilterBloomApplyShader =
        { BloomStrengthUniform : int
          BloomFilterTextureUniform : int
          CompositionTextureUniform : int
          FilterBloomApplyShader : uint }

    /// Describes a depth of field filter shader that's loaded into GPU.
    type FilterDepthOfFieldShader =
        { ViewInverseUniform : int
          ProjectionInverseUniform : int
          NearDistanceUniform : int
          FarDistanceUniform : int
          FocalPointUniform : int
          FocalTypeUniform : int
          FocalDistanceUniform : int
          PositionTextureUniform : int
          BlurredTextureUniform : int
          UnblurredTextureUniform : int
          FilterDepthOfFieldShader : uint }

    /// Describes a tone mapping shader that's loaded into GPU.
    type FilterToneMappingShader =
        { LightExposureUniform : int
          ToneMapTypeUniform : int
          ToneMapSlopeUniform : int
          ToneMapOffsetUniform : int
          ToneMapPowerUniform : int
          ToneMapSaturationUniform : int
          ToneMapWhitePointUniform : int
          InputTextureUniform : int
          FilterToneMappingShader : uint  }

    /// Describes a chromatic aberration filter shader that's loaded into GPU.
    type FilterChromaticAberrationShader =
        { ChannelOffsetsUniform : int
          FocalPointUniform : int
          InputTextureUniform : int
          FilterChromaticAberrationShader : uint  }

    /// Describes an fxaa filter shader that's loaded into GPU.
    type FilterFxaaShader =
        { SpanMaxUniform : int
          ReduceMinDivisorUniform : int
          ReduceMulDivisorUniform : int
          InputTextureUniform : int
          FilterFxaaShader : uint }

    /// Describes a gamma correction filter shader that's loaded into GPU.
    type FilterGammaCorrectionShader =
        { InputTextureUniform : int
          FilterGammaCorrectionShader : uint  }

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

    /// Create a filter bloom extract shader.
    let CreateFilterBloomExtractShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let thresholdUniform = Gl.GetUniformLocation (shader, "threshold")
        let colorTextureUniform = Gl.GetUniformLocation (shader, "colorTexture")

        // make shader record
        { ThresholdUniform = thresholdUniform
          ColorTextureUniform = colorTextureUniform
          FilterBloomExtractShader = shader }

    /// Create a filter bloom down-sample shader.
    let CreateFilterBloomDownSampleShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let sampleLevelUniform = Gl.GetUniformLocation (shader, "sampleLevel")
        let karisAverageEnabledUniform = Gl.GetUniformLocation (shader, "karisAverageEnabled")
        let sourceResolutionUniform = Gl.GetUniformLocation (shader, "sourceResolution")
        let sourceTextureUniform = Gl.GetUniformLocation (shader, "sourceTexture")

        // make shader record
        { SampleLevelUniform = sampleLevelUniform
          KarisAverageEnabledUniform = karisAverageEnabledUniform
          SourceResolutionUniform = sourceResolutionUniform
          SourceTextureUniform = sourceTextureUniform
          FilterBloomDownSampleShader = shader }

    /// Create a filter bloom up-sample shader.
    let CreateFilterBloomUpSampleShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let filterRadiusUniform = Gl.GetUniformLocation (shader, "filterRadius")
        let sourceTextureUniform = Gl.GetUniformLocation (shader, "sourceTexture")

        // make shader record
        { FilterRadiusUniform = filterRadiusUniform
          SourceTextureUniform = sourceTextureUniform
          FilterBloomUpSampleShader = shader }

    /// Create a filter bloom apply shader.
    let CreateFilterBloomApplyShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let bloomStrengthUniform = Gl.GetUniformLocation (shader, "bloomStrength")
        let bloomFilterTextureUniform = Gl.GetUniformLocation (shader, "bloomFilterTexture")
        let compositionTextureUniform = Gl.GetUniformLocation (shader, "compositionTexture")

        // make shader record
        { BloomStrengthUniform = bloomStrengthUniform
          BloomFilterTextureUniform = bloomFilterTextureUniform
          CompositionTextureUniform = compositionTextureUniform
          FilterBloomApplyShader = shader }

    let CreateFilterDepthOfFieldShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewInverseUniform = Gl.GetUniformLocation (shader, "viewInverse")
        let projectionInverseUniform = Gl.GetUniformLocation (shader, "projectionInverse")
        let nearDistanceUniform = Gl.GetUniformLocation (shader, "nearDistance")
        let farDistanceUniform = Gl.GetUniformLocation (shader, "farDistance")
        let focalPointUniform = Gl.GetUniformLocation (shader, "focalPoint")
        let focalTypeUniform = Gl.GetUniformLocation (shader, "focalType")
        let focalDistanceUniform = Gl.GetUniformLocation (shader, "focalDistance")
        let positionTextureUniform = Gl.GetUniformLocation (shader, "positionTexture")
        let blurredTextureUniform = Gl.GetUniformLocation (shader, "blurredTexture")
        let unblurredTextureUniform = Gl.GetUniformLocation (shader, "unblurredTexture")

        // make shader record
        { ViewInverseUniform = viewInverseUniform
          ProjectionInverseUniform = projectionInverseUniform
          NearDistanceUniform = nearDistanceUniform
          FarDistanceUniform = farDistanceUniform
          FocalPointUniform = focalPointUniform
          FocalTypeUniform = focalTypeUniform
          FocalDistanceUniform = focalDistanceUniform
          PositionTextureUniform = positionTextureUniform
          BlurredTextureUniform = blurredTextureUniform
          UnblurredTextureUniform = unblurredTextureUniform
          FilterDepthOfFieldShader = shader }

    let CreateFilterToneMappingShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let lightExposureUniform = Gl.GetUniformLocation (shader, "lightExposure")
        let toneMapTypeUniform = Gl.GetUniformLocation (shader, "toneMapType")
        let toneMapSlopeUniform = Gl.GetUniformLocation (shader, "toneMapSlope")
        let toneMapOffsetUniform = Gl.GetUniformLocation (shader, "toneMapOffset")
        let toneMapPowerUniform = Gl.GetUniformLocation (shader, "toneMapPower")
        let toneMapSaturationUniform = Gl.GetUniformLocation (shader, "toneMapSaturation")
        let toneMapWhitePointUniform = Gl.GetUniformLocation (shader, "toneMapWhitePoint")
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { LightExposureUniform = lightExposureUniform
          ToneMapTypeUniform = toneMapTypeUniform
          ToneMapSlopeUniform = toneMapSlopeUniform
          ToneMapOffsetUniform = toneMapOffsetUniform
          ToneMapPowerUniform = toneMapPowerUniform
          ToneMapSaturationUniform = toneMapSaturationUniform
          ToneMapWhitePointUniform = toneMapWhitePointUniform
          InputTextureUniform = inputTextureUniform
          FilterToneMappingShader = shader }

    let CreateFilterChromaticAberrationShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let channelOffsetsUniform = Gl.GetUniformLocation (shader, "channelOffsets")
        let focalPointUniform = Gl.GetUniformLocation (shader, "focalPoint")
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { ChannelOffsetsUniform = channelOffsetsUniform
          FocalPointUniform = focalPointUniform
          InputTextureUniform = inputTextureUniform
          FilterChromaticAberrationShader = shader }

    /// Create a filter fxaa shader.
    let CreateFilterFxaaShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let spanMaxUniform = Gl.GetUniformLocation (shader, "spanMax")
        let reduceMinDivisorUniform = Gl.GetUniformLocation (shader, "reduceMinDivisor")
        let reduceMulDivisorUniform = Gl.GetUniformLocation (shader, "reduceMulDivisor")
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { SpanMaxUniform = spanMaxUniform
          ReduceMinDivisorUniform = reduceMinDivisorUniform
          ReduceMulDivisorUniform = reduceMulDivisorUniform
          InputTextureUniform = inputTextureUniform
          FilterFxaaShader = shader }

    let CreateFilterGammaCorrectionShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let inputTextureUniform = Gl.GetUniformLocation (shader, "inputTexture")

        // make shader record
        { InputTextureUniform = inputTextureUniform
          FilterGammaCorrectionShader = shader }

    type FilterShaders =
        { FilterBox1dShader : FilterBoxShader
          FilterGaussian1dShader : FilterGaussianShader
          FilterGaussianArray1dShader : FilterGaussianArrayShader
          FilterBilateralDownSample4dShader : FilterBilateralDownSampleShader
          FilterBilateralUpSample4dShader : FilterBilateralUpSampleShader
          FilterBloomExtractShader : FilterBloomExtractShader
          FilterBloomDownSampleShader : FilterBloomDownSampleShader
          FilterBloomUpSampleShader : FilterBloomUpSampleShader
          FilterBloomApplyShader : FilterBloomApplyShader
          FilterDepthOfFieldShader : FilterDepthOfFieldShader
          FilterToneMappingShader : FilterToneMappingShader
          FilterChromaticAberrationShader : FilterChromaticAberrationShader
          FilterFxaaShader : FilterFxaaShader
          FilterGaussian4dShader : FilterGaussianShader
          FilterGammaCorrectionShader : FilterGammaCorrectionShader }

    let CreateFilterShaders () =

        // create individual shaders
        let filterBox1dShader = CreateFilterBoxShader Constants.Paths.FilterBox1dShaderFilePath
        let filterGaussian1dShader = CreateFilterGaussianShader Constants.Paths.FilterGaussian1dShaderFilePath
        let filterGaussianArray1dShader = CreateFilterGaussianArrayShader Constants.Paths.FilterGaussianArray1dShaderFilePath
        let filterBilateralDownSample4dShader = CreateFilterBilateralDownSampleShader Constants.Paths.FilterBilateralDownSample4dShaderFilePath
        let filterBilateralUpSample4dShader = CreateFilterBilateralUpSampleShader Constants.Paths.FilterBilateralUpSample4dShaderFilePath
        let filterBloomExtractShader = CreateFilterBloomExtractShader Constants.Paths.FilterBloomExtractShaderFilePath
        let filterBloomDownSampleShader = CreateFilterBloomDownSampleShader Constants.Paths.FilterBloomDownSampleShaderFilePath
        let filterBloomUpSampleShader = CreateFilterBloomUpSampleShader Constants.Paths.FilterBloomUpSampleShaderFilePath
        let filterBloomApplyShader = CreateFilterBloomApplyShader Constants.Paths.FilterBloomApplyShaderFilePath
        let filterDepthOfFieldShader = CreateFilterDepthOfFieldShader Constants.Paths.FilterDepthOfFieldShaderFilePath
        let filterToneMappingShader = CreateFilterToneMappingShader Constants.Paths.FilterToneMappingShaderFilePath
        let filterChromaticAberrationShader = CreateFilterChromaticAberrationShader Constants.Paths.FilterChromaticAberrationShaderFilePath
        let filterFxaaShader = CreateFilterFxaaShader Constants.Paths.FilterFxaaShaderFilePath
        let filterGaussian4dShader = CreateFilterGaussianShader Constants.Paths.FilterGaussian4dShaderFilePath
        let filterGammaCorrectionShader = CreateFilterGammaCorrectionShader Constants.Paths.FilterGammaCorrectionShaderFilePath

        // fin
        { FilterBox1dShader = filterBox1dShader
          FilterGaussian1dShader = filterGaussian1dShader
          FilterGaussianArray1dShader = filterGaussianArray1dShader
          FilterBilateralDownSample4dShader = filterBilateralDownSample4dShader
          FilterBilateralUpSample4dShader = filterBilateralUpSample4dShader
          FilterBloomExtractShader = filterBloomExtractShader
          FilterBloomDownSampleShader = filterBloomDownSampleShader
          FilterBloomUpSampleShader = filterBloomUpSampleShader
          FilterBloomApplyShader = filterBloomApplyShader
          FilterDepthOfFieldShader = filterDepthOfFieldShader
          FilterToneMappingShader = filterToneMappingShader
          FilterChromaticAberrationShader = filterChromaticAberrationShader
          FilterFxaaShader = filterFxaaShader
          FilterGaussian4dShader = filterGaussian4dShader
          FilterGammaCorrectionShader = filterGammaCorrectionShader }

    let DestroyFilterShaders (shaders : FilterShaders) =
        Gl.DeleteProgram shaders.FilterBox1dShader.FilterBoxShader
        Gl.DeleteProgram shaders.FilterGaussian1dShader.FilterGaussianShader
        Gl.DeleteProgram shaders.FilterGaussianArray1dShader.FilterGaussianArrayShader
        Gl.DeleteProgram shaders.FilterBilateralDownSample4dShader.FilterBilateralDownSampleShader
        Gl.DeleteProgram shaders.FilterBilateralUpSample4dShader.FilterBilateralUpSampleShader
        Gl.DeleteProgram shaders.FilterBloomExtractShader.FilterBloomExtractShader
        Gl.DeleteProgram shaders.FilterBloomDownSampleShader.FilterBloomDownSampleShader
        Gl.DeleteProgram shaders.FilterBloomUpSampleShader.FilterBloomUpSampleShader
        Gl.DeleteProgram shaders.FilterBloomApplyShader.FilterBloomApplyShader
        Gl.DeleteProgram shaders.FilterDepthOfFieldShader.FilterDepthOfFieldShader
        Gl.DeleteProgram shaders.FilterToneMappingShader.FilterToneMappingShader
        Gl.DeleteProgram shaders.FilterChromaticAberrationShader.FilterChromaticAberrationShader
        Gl.DeleteProgram shaders.FilterFxaaShader.FilterFxaaShader
        Gl.DeleteProgram shaders.FilterGaussian4dShader.FilterGaussianShader
        Gl.DeleteProgram shaders.FilterGammaCorrectionShader.FilterGammaCorrectionShader