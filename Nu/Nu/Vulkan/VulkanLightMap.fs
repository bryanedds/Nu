// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type EnvironmentFilterStruct =
    [<FieldOffset(0)>] val mutable roughness : single
    [<FieldOffset(4)>] val mutable resolution : single

/// Describes an environment filter pipeline that's loaded into GPU.
type EnvironmentFilterPipeline =
    { EyeUniform : VulkanBuffer
      EnvironmentFilterUniform : VulkanBuffer
      Pipeline : Pipeline }

/// A collection of maps consisting a light map.
type [<Struct>] LightMap =
    { Enabled : bool
      Origin : Vector3
      Bounds : Box3
      AmbientColor : Color
      AmbientBrightness : single
      IrradianceMap : Texture
      EnvironmentFilterMap : Texture }

[<RequireQualifiedAccess>]
module LightMap =

    /// Create a reflection map.
    let createReflectionMap render resolution origin ambientColor ambientBrightness getCommandBuffer context =

        // create reflection cube map
        let metadata = TextureMetadata.make resolution resolution
        let reflectionCubeMapInternal =
            TextureInternal.create
                MipmapNone (AttachmentColor false) TextureCubeMap (VkImageUsageFlags.Sampled ||| VkImageUsageFlags.TransferDst)
                Rgba16f Rgba metadata context
        let reflectionCubeMap = EagerTexture reflectionCubeMapInternal

        // construct geometry viewport
        let bounds = box2i v2iZero (v2iDup resolution)
        let geometryViewport = Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds bounds

        // construct eye rotations
        let eyeRotations =
            [|(v3Right, v3Down)     // (+x)
              (v3Left, v3Down)      // (-x)
              (v3Up, v3Back)        // (+y)
              (v3Down, v3Forward)   // (-y)
              (v3Back, v3Down)      // (+z)
              (v3Forward, v3Down)|] // (-z)

        // begin reflection rendering
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentRead ColorAttachmentWrite reflectionCubeMap.Image (getCommandBuffer ())

        // render reflection cube map faces
        for i in 0 .. dec 6 do

            // render to reflection cube map face
            let lightAmbientOverride = Some (ambientColor, ambientBrightness)
            let (eyeForward, eyeUp) = eyeRotations[i]
            let eyeRotationMatrix = Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)
            let eyeRotation = Quaternion.CreateFromRotationMatrix eyeRotationMatrix
            let view = Matrix4x4.CreateLookAt (origin, origin + eyeForward, eyeUp)
            let viewSkyBox =
                match i with
                | 2 -> // NOTE: special case for sky box top.
                    let (eyeForward, eyeUp) = (v3Down, v3Forward)
                    let eyeRotationMatrix = Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)
                    Matrix4x4.Transpose eyeRotationMatrix
                | 3 -> // NOTE: special case for sky box bottom.
                    let (eyeForward, eyeUp) = (v3Up, v3Back)
                    let eyeRotationMatrix = Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)
                    Matrix4x4.Transpose eyeRotationMatrix
                | _ -> Matrix4x4.Transpose eyeRotationMatrix
            let frustum = Viewport.getFrustum origin eyeRotation MathF.PI_OVER_2 geometryViewport
            let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, geometryViewport.DistanceNear, geometryViewport.DistanceFar)
            let bounds = VkRect2D (0, 0, uint resolution, uint resolution)
            render false lightAmbientOverride origin view viewSkyBox frustum projection projection bounds i reflectionCubeMap.Image

            // take a snapshot for testing
            //Hl.saveFramebufferRgbaToBitmap resolution resolution ("Reflection." + string reflectionCubeMapId + "." + string i + ".bmp")

        // end reflection rendering
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead reflectionCubeMap.Image (getCommandBuffer ())

        // fin
        reflectionCubeMap

    let createIrradianceMap resolution (cubeMapSurface : CubeMapSurface) sampler colorFormat irradiancePipeline getCommandBuffer advanceCommandBufferWhenNeeded context =

        // create irradiance cube map
        let metadata = TextureMetadata.make resolution resolution
        let cubeMapInternal =
            TextureInternal.create
                MipmapNone (AttachmentColor false) TextureCubeMap VkImageUsageFlags.Sampled
                colorFormat Rgba metadata context
        let cubeMap = EagerTexture cubeMapInternal

        // construct eye rotations
        let eyeRotations =
            [|(v3Right, v3Down)     // (+x)
              (v3Left, v3Down)      // (-x)
              (v3Up, v3Back)        // (+y)
              (v3Down, v3Forward)   // (-y)
              (v3Back, v3Down)      // (+z)
              (v3Forward, v3Down)|] // (-z)

        // compute projection
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)

        // begin cubemap rendering
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentRead ColorAttachmentWrite cubeMap.Image (getCommandBuffer ())

        // render faces to irradiance cube map
        for i in 0 .. dec 6 do

            // render face
            let eyeCenter = v3Zero // assuming output
            let (eyeForward, eyeUp) = eyeRotations[i]
            let view = Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)
            CubeMap.drawCubeMap
                eyeCenter view projection cubeMapSurface.Flipped cubeMapSurface.CubeMap sampler
                cubeMapSurface.Geometry resolution cubeMap.SubViews[0, i]
                irradiancePipeline getCommandBuffer advanceCommandBufferWhenNeeded context

            // take a snapshot for testing
            //Hl.saveFramebufferRgbaToBitmap resolution resolution ("Irradiance." + string cubeMapId + "." + string i + ".bmp")

        // end cubemap rendering
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead cubeMap.Image (getCommandBuffer ())
        
        // fin
        cubeMap
    
    /// Create an EnvironmentFilterPipeline.
    let createEnvironmentFilterPipeline shaderPath colorAttachmentFormat (context : VulkanContext) =

        // create uniform buffers
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context
        let environmentFilterUniform = VulkanBuffer.create Uniform sizeof<EnvironmentFilterStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 ((3 (*position*)) * sizeof<single>) VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1
                      Pipeline.descriptor 1 UniformBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                      [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform; environmentFilterUniform|]

        // fin
        { EyeUniform = eyeUniform; EnvironmentFilterUniform = environmentFilterUniform; Pipeline = pipeline }
    
    /// Destroy an EnvironmentFilterPipeline.
    let destroyEnvironmentFilterPipeline environmentFilterPipeline context =
        Pipeline.destroy environmentFilterPipeline.Pipeline context
    
    /// Draw an environment filter.
    let drawEnvironmentFilter
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (roughness : single)
        (resolution : single)
        (flipped : bool)
        (cubeMap : Texture)
        (sampler : Sampler)
        (geometry : CubeMapGeometry)
        (colorAttachment : VkImageView)
        (pipeline : EnvironmentFilterPipeline)
        (getCommandBuffer : unit -> VkCommandBuffer)
        (advanceCommandBufferWhenNeeded : unit -> unit)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let viewInverse = view.Inverted
        let projection = if flipped then projectionUnflipped.Flipped else projectionUnflipped
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify uniforms
            let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->

                // specify eye
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

                // specify environment filter
                let environmentFilter = EnvironmentFilterStruct (roughness = roughness, resolution = resolution)
                VulkanBuffer.uploadValue environmentFilter pipeline.EnvironmentFilterUniform context
                Pipeline.writeDescriptorUniformBuffer 1 0 pipeline.EnvironmentFilterUniform vkSet

            // specify cube map
            let mutable cubeMapDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampledTexture 0 0 cubeMap vkSet

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline.Pipeline $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 sampler vkSet

            // set up render
            let commandBuffer = getCommandBuffer ()
            let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
            let mutable vkViewport = Hl.makeViewport false renderArea
            Hl.withRenderingInfo [|colorAttachment|] None renderArea None $ fun renderingInfo ->
                let mutable renderingInfo = renderingInfo
                DeviceApi.vkCmdBeginRendering (commandBuffer, &&renderingInfo)
            DeviceApi.vkCmdSetViewport (commandBuffer, 0u, 1u, &&vkViewport)
            DeviceApi.vkCmdSetScissor (commandBuffer, 0u, 1u, &&renderArea)

            // set up pipeline
            DeviceApi.vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            DeviceApi.vkCmdBindVertexBuffers (commandBuffer, 0u, 1u, &&vertexBuffer, &&vertexOffset)
            DeviceApi.vkCmdBindIndexBuffer (commandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&uniformDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&cubeMapDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, &&samplerDescriptorSet, 0u, nullPtr)

            // draw
            DeviceApi.vkCmdDrawIndexed (commandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

            // tear down render
            DeviceApi.vkCmdEndRendering commandBuffer

            // report drawing
            Hl.reportDrawCall 1 true

            // advance pipeline
            Pipeline.advance pipeline.Pipeline

            // advance rendering command buffer when needed
            advanceCommandBufferWhenNeeded ()

        // abort
        | None -> Log.warnOnce ("Cannot draw " + getTypeName pipeline + " because VkPipeline does not exist.")
    
    /// Create an environment filter map.
    let createEnvironmentFilterMap resolution (environmentFilterSurface : CubeMapSurface) sampler colorFormat environmentFilterPipeline getCommandBuffer advanceCommandBufferWhenNeeded context =

        // create environment filter cube map
        let metadata = TextureMetadata.make resolution resolution
        let cubeMapInternal =
            TextureInternal.create
                (MipmapManual Constants.Render.EnvironmentFilterMips) (AttachmentColor false) TextureCubeMap VkImageUsageFlags.Sampled
                colorFormat Rgba metadata context
        let cubeMap = EagerTexture cubeMapInternal
        
        // compute views and projection
        let views =
            [|Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)
              Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)
              Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)|]
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)

        // begin cubemap rendering
        Hl.recordTransitionLayout true cubeMap.MipLevels 0 6 VkImageAspectFlags.Color ColorAttachmentRead ColorAttachmentWrite cubeMap.Image (getCommandBuffer ())

        // render environment filter cube map mips
        for mip in 0 .. dec Constants.Render.EnvironmentFilterMips do
            let mipRoughness = single mip / single (dec Constants.Render.EnvironmentFilterMips)
            let mipResolution = single resolution * pown 0.5f mip
            for i in 0 .. dec 6 do

                // draw mip face
                let eyeCenter = v3Zero // assuming origin
                let view = views[i]
                drawEnvironmentFilter
                    eyeCenter view projection mipRoughness mipResolution environmentFilterSurface.Flipped environmentFilterSurface.CubeMap sampler
                    environmentFilterSurface.Geometry cubeMap.SubViews[mip, i]
                    environmentFilterPipeline getCommandBuffer advanceCommandBufferWhenNeeded context

                // take a snapshot for testing
                //Hl.saveFramebufferRgbaToBitmap (int mipResolution) (int mipResolution) ("EnvironmentFilter." + string i + "." + string mip + ".bmp")

        // end cubemap rendering
        Hl.recordTransitionLayout true cubeMap.MipLevels 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ColorAttachmentRead cubeMap.Image (getCommandBuffer ())

        // fin
        cubeMap

    /// Create a light map with existing irradiance and environment filter maps.
    let createLightMap enabled origin ambientColor ambientBrightness bounds irradianceMap environmentFilterMap =
        { Enabled = enabled
          Origin = origin
          AmbientColor = ambientColor
          AmbientBrightness = ambientBrightness
          Bounds = bounds
          IrradianceMap = irradianceMap
          EnvironmentFilterMap = environmentFilterMap }

    /// Destroy a light map, including its irradiance environment filter maps.
    let destroyLightMap lightMap context =
        Texture.destroy lightMap.IrradianceMap context
        Texture.destroy lightMap.EnvironmentFilterMap context