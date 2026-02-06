// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module LightMap =

    /// A collection of maps consisting a light map.
    type [<Struct>] LightMap =
        { Enabled : bool
          Origin : Vector3
          Bounds : Box3
          AmbientColor : Color
          AmbientBrightness : single
          IrradianceMap : Texture.Texture
          EnvironmentFilterMap : Texture.Texture }

    /// Create a light map with existing irradiance and environment filter maps.
    let CreateLightMap enabled origin ambientColor ambientBrightness bounds irradianceMap environmentFilterMap =
        { Enabled = enabled
          Origin = origin
          AmbientColor = ambientColor
          AmbientBrightness = ambientBrightness
          Bounds = bounds
          IrradianceMap = irradianceMap
          EnvironmentFilterMap = environmentFilterMap }

    /// Destroy a light map, including its irradiance environment filter maps.
    let DestroyLightMap lightMap vkc =
        lightMap.IrradianceMap.Destroy vkc
        lightMap.EnvironmentFilterMap.Destroy vkc