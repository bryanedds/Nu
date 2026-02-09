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
        Hl.recordTransitionLayout cb true 1 0 6 VkImageAspectFlags.Color Hl.ColorAttachmentWrite Hl.ShaderRead cubeMapInternal.Image
        
        // fin
        cubeMap
    
    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type Transform =
        [<FieldOffset(0)>] val mutable view : Matrix4x4
        [<FieldOffset(64)>] val mutable projection : Matrix4x4
        [<FieldOffset(128)>] val mutable viewProjection : Matrix4x4
    
    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type EnvironmentFilter =
        [<FieldOffset(0)>] val mutable roughness : single
        [<FieldOffset(4)>] val mutable resolution : single
    
    /// Describes an environment filter pipeline that's loaded into GPU.
    type EnvironmentFilterPipeline =
        { TransformUniform : Buffer.Buffer
          EnvironmentFilterUniform : Buffer.Buffer
          Pipeline : Pipeline.Pipeline }
    
    /// Create an EnvironmentFilterPipeline.
    let CreateEnvironmentFilterPipeline (shaderPath, colorAttachmentFormat, vkc : Hl.VulkanContext) =

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                shaderPath
                [|Pipeline.NoBlend|]
                [|Pipeline.vertex 0 ((3 (*position*)) * sizeof<single>) VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single3 0|]|]
                [|Pipeline.descriptorSet true
                    [|Pipeline.descriptor 0 Hl.UniformBuffer Hl.VertexStage (6 * Constants.Render.EnvironmentFilterMips)
                      Pipeline.descriptor 1 Hl.UniformBuffer Hl.FragmentStage (6 * Constants.Render.EnvironmentFilterMips)
                      Pipeline.descriptor 2 Hl.CombinedImageSampler Hl.FragmentStage (6 * Constants.Render.EnvironmentFilterMips)|]|]
                [|Pipeline.pushConstant 0 sizeof<int> Hl.VertexFragmentStage|]
                colorAttachmentFormat
                None
                vkc

        // create uniform buffers
        let transformUniform = Buffer.Buffer.create sizeof<Transform> Buffer.Uniform vkc
        let environmentFilterUniform = Buffer.Buffer.create sizeof<EnvironmentFilter> Buffer.Uniform vkc

        // fin
        { TransformUniform = transformUniform; EnvironmentFilterUniform = environmentFilterUniform; Pipeline = pipeline }
    
    /// Destroy an EnvironmentFilterPipeline.
    let DestroyEnvironmentFilterPipeline (environmentFilterPipeline, vkc) =
        Buffer.Buffer.destroy environmentFilterPipeline.TransformUniform vkc
        Buffer.Buffer.destroy environmentFilterPipeline.EnvironmentFilterUniform vkc
        Pipeline.Pipeline.destroy environmentFilterPipeline.Pipeline vkc
    
    /// Draw an environment filter.
    let DrawEnvironmentFilter
        (drawIndex : int,
         cb : VkCommandBuffer,
         view : Matrix4x4,
         projection : Matrix4x4,
         viewProjection : Matrix4x4,
         roughness : single,
         resolution : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         colorAttachment : VkImageView,
         pipeline : EnvironmentFilterPipeline,
         vkc : Hl.VulkanContext) =

        // upload uniforms
        let mutable transform = Transform ()
        let mutable environmentFilter = EnvironmentFilter ()
        transform.view <- view
        transform.projection <- projection
        transform.viewProjection <- viewProjection
        environmentFilter.roughness <- roughness
        environmentFilter.resolution <- resolution
        Buffer.Buffer.uploadValue drawIndex 0 0 transform pipeline.TransformUniform vkc
        Buffer.Buffer.uploadValue drawIndex 0 0 environmentFilter pipeline.EnvironmentFilterUniform vkc
    
        // update uniform descriptors
        Pipeline.Pipeline.updateDescriptorsUniform 0 0 pipeline.TransformUniform pipeline.Pipeline vkc
        Pipeline.Pipeline.updateDescriptorsUniform 0 1 pipeline.EnvironmentFilterUniform pipeline.Pipeline vkc

        // bind texture
        Pipeline.Pipeline.writeDescriptorTexture drawIndex 0 2 cubeMap pipeline.Pipeline vkc

        // make viewport and scissor
        let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
        let mutable vkViewport = Hl.makeViewport true renderArea
        let mutable scissor = renderArea

        // only draw if scissor (and therefore also viewport) is valid
        if Hl.validateRect scissor then

            // init render
            let mutable rendering = Hl.makeRenderingInfo colorAttachment None renderArea None
            Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

            // bind pipeline
            let vkPipeline = Pipeline.Pipeline.getVkPipeline Pipeline.NoBlend false pipeline.Pipeline
            Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

            // set viewport and scissor
            Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
            
            // bind vertex and index buffer
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (cb, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor set
            let mutable descriptorSet = pipeline.Pipeline.VkDescriptorSet 0
            Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &descriptorSet, 0u, nullPtr)
            
            // push draw index
            let mutable drawIndex = drawIndex
            Vulkan.vkCmdPushConstants (cb, pipeline.Pipeline.PipelineLayout, Hl.VertexFragmentStage.VkShaderStageFlags, 0u, 4u, asVoidPtr &drawIndex)
            
            // draw
            Vulkan.vkCmdDrawIndexed (cb, uint geometry.ElementCount, 1u, 0u, 0, 0u)
            Hl.reportDrawCall 1
        
            // end render
            Vulkan.vkCmdEndRendering cb

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