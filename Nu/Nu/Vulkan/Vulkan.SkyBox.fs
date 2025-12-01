// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    /// Describes a sky box pipeline that's loaded into GPU.
    type SkyBoxPipeline =
        { ViewUniform : Buffer.Buffer
          ProjectionUniform : Buffer.Buffer
          ViewProjectionUniform : Buffer.Buffer
          ColorUniform : Buffer.Buffer
          BrightnessUniform : Buffer.Buffer
          SkyBoxPipeline : Pipeline.Pipeline }

    /// Destroy a SkyBoxPipeline.
    let DestroySkyBoxPipeline skyBoxPipeline vkc =
        Buffer.Buffer.destroy skyBoxPipeline.ViewUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ViewProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ColorUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.BrightnessUniform vkc
        Pipeline.Pipeline.destroy skyBoxPipeline.SkyBoxPipeline vkc

    /// Create a SkyBoxPipeline.
    let CreateSkyBoxPipeline (vkc : Hl.VulkanContext) =

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                Constants.Paths.SkyBoxShaderFilePath
                false false [|Pipeline.NoBlend|]
                [|Hl.makeVertexBindingVertex 0 CubeMap.VertexSize|]
                [|Hl.makeVertexAttribute 0 0 Hl.Single3 0|]
                [|Hl.makeDescriptorBindingVertex 0 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 1 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingVertex 2 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 3 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 4 Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
                  Hl.makeDescriptorBindingFragment 5 Vulkan.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1|]
                [||] vkc

        // create uniform buffers
        let viewUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let projectionUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let viewProjectionUniform = Buffer.Buffer.create (sizeof<single> * 16) Buffer.Uniform vkc
        let colorUniform = Buffer.Buffer.create (sizeof<single> * 3) Buffer.Uniform vkc
        let brightnessUniform = Buffer.Buffer.create (sizeof<single> * 1) Buffer.Uniform vkc
        
        // make SkyBoxPipeline
        let skyBoxPipeline =
            { ViewUniform = viewUniform
              ProjectionUniform = projectionUniform
              ViewProjectionUniform = viewProjectionUniform
              ColorUniform = colorUniform
              BrightnessUniform = brightnessUniform
              SkyBoxPipeline = pipeline }

        // fin
        skyBoxPipeline