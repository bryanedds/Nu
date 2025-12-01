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

    /// Destroy SkyBoxPipeline.
    let DestroySkyBoxPipeline skyBoxPipeline vkc =
        Buffer.Buffer.destroy skyBoxPipeline.ViewUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ViewProjectionUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.ColorUniform vkc
        Buffer.Buffer.destroy skyBoxPipeline.BrightnessUniform vkc
        Pipeline.Pipeline.destroy skyBoxPipeline.SkyBoxPipeline vkc
