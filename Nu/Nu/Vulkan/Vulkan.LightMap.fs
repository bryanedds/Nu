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

    let CreateIrradianceMap (mapId, cb, resolution, cubeMapSurface : CubeMap.CubeMapSurface, colorFormat, irradiancePipeline, vkc) =

        // create irradiance cube map
        let metadata = Texture.TextureMetadata.make resolution resolution
        let cubeMapInternal =
            Texture.TextureInternal.create
                VkSamplerAddressMode.ClampToEdge VkFilter.Linear VkFilter.Linear false
                Texture.MipmapNone (Texture.AttachmentColor false) Texture.TextureCubeMap [|VkImageUsageFlags.Sampled|]
                colorFormat Hl.Rgba metadata vkc
        let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = cubeMapInternal }

        // compute views and projection
        let views =
            [|Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)
              Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)
              Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)|]
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)

        // render faces to irradiance cube map
        for i in 0 .. dec 6 do

            // render face
            let view = views.[i]
            let viewProjection = view * projection
            CubeMap.DrawCubeMap
                (mapId * 6 + i, cb, view, projection, viewProjection, cubeMapSurface.CubeMap, cubeMapSurface.CubeMapGeometry, resolution, cubeMap, irradiancePipeline, vkc)

            // take a snapshot for testing
            // TODO: DJL: implement.
            //Hl.SaveFramebufferRgbaToBitmap (resolution, resolution, "Irradiance." + string cubeMapId + "." + string i + ".bmp")

        // transition cubemap layout
        // TODO: DJL: implement once multi-layered transition is sorted out!
        
        // fin
        cubeMap
    
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