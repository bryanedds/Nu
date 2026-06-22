// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu.Vulkan
open System
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu
    
[<Struct; StructLayout (LayoutKind.Explicit)>]
type LightMapTransform =
    [<FieldOffset(0)>] val mutable view : Matrix4x4
    [<FieldOffset(64)>] val mutable projection : Matrix4x4
    [<FieldOffset(128)>] val mutable viewProjection : Matrix4x4

[<Struct; StructLayout (LayoutKind.Explicit)>]
type EnvironmentFilter =
    [<FieldOffset(0)>] val mutable roughness : single
    [<FieldOffset(4)>] val mutable resolution : single

/// Describes an environment filter pipeline that's loaded into GPU.
type EnvironmentFilterPipeline =
    { TransformUniform : Nu.Vulkan.Buffer
      EnvironmentFilterUniform : Nu.Vulkan.Buffer
      Pipeline : Pipeline }

/// A collection of maps consisting a light map.
type [<Struct>] LightMapping =
    { Enabled : bool
      Origin : Vector3
      Bounds : Box3
      AmbientColor : Color
      AmbientBrightness : single
      IrradianceMap : Texture
      EnvironmentFilterMap : Texture }

[<RequireQualifiedAccess>]
module LightMapping =

    /// Create a reflection map.
    let createReflectionMap render resolution origin ambientColor ambientBrightness commandBuffer vkc =

        // create reflection cube map
        let metadata = TextureMetadata.make resolution resolution
        let reflectionCubeMapParallel =
            TextureParallel.create
                MipmapNone (AttachmentColor false) TextureCubeMap [|VkImageUsageFlags.Sampled; VkImageUsageFlags.TransferDst|]
                Rgba16f Rgba metadata vkc
        let reflectionCubeMap = EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = reflectionCubeMapParallel }

        // construct geometry viewport
        let bounds = box2i v2iZero (v2iDup resolution)
        let geometryViewport = Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds bounds

        // construct eye rotations
        let eyeRotations =
            [|(v3Right, v3Down)     // (+x) right
              (v3Left, v3Down)      // (-x) left
              (v3Up, v3Back)        // (+y) top
              (v3Down, v3Forward)   // (-y) bottom
              (v3Back, v3Down)      // (+z) back
              (v3Forward, v3Down)|] // (-z) front

        // render reflection cube map faces
        for i in 0 .. dec 6 do

            // render to reflection cube map face
            let lightAmbientOverride = Some (ambientColor, ambientBrightness)
            let (eyeForward, eyeUp) = eyeRotations.[i]
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
            let viewProjection = view * projection
            let bounds = VkRect2D (0, 0, uint resolution, uint resolution)
            render false lightAmbientOverride origin view viewSkyBox frustum projection viewProjection projection bounds i reflectionCubeMapParallel.Image

            // take a snapshot for testing
            // TODO: DJL: implement.
            //Hl.saveFramebufferRgbaToBitmap resolution resolution ("Reflection." + string reflectionCubeMapId + "." + string i + ".bmp")

        // transition cubemap layout
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ShaderRead reflectionCubeMapParallel.Image commandBuffer

        // fin
        reflectionCubeMap

    let createIrradianceMap invertY resolution (cubeMapSurface : CubeMapSurface) sampler colorFormat irradiancePipeline commandBuffer vkc =

        // create irradiance cube map
        let metadata = TextureMetadata.make resolution resolution
        let cubeMapParallel =
            TextureParallel.create
                MipmapNone (AttachmentColor false) TextureCubeMap [|VkImageUsageFlags.Sampled|]
                colorFormat Rgba metadata vkc
        let cubeMap = EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = cubeMapParallel }

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
            CubeMap.drawCubeMap view projection viewProjection invertY cubeMapSurface.CubeMap sampler cubeMapSurface.CubeMapGeometry resolution cubeMap.SubViews.[0, i] irradiancePipeline commandBuffer vkc

            // take a snapshot for testing
            // TODO: DJL: implement.
            //Hl.saveFramebufferRgbaToBitmap resolution resolution ("Irradiance." + string cubeMapId + "." + string i + ".bmp")

        // transition cubemap layout
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ShaderRead cubeMapParallel.Image commandBuffer
        
        // fin
        cubeMap
    
    /// Create an EnvironmentFilterPipeline.
    let createEnvironmentFilterPipeline shaderPath colorAttachmentFormat (vkc : VulkanContext) =

        // create uniform buffers
        let transformUniform = Buffer.create sizeof<Transform> Storage vkc
        let environmentFilterUniform = Buffer.create sizeof<EnvironmentFilter> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 ((3 (*position*)) * sizeof<single>) VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1
                      Pipeline.descriptor 1 StorageBuffer FragmentStage 1|]
                  Pipeline.descriptorSet<Texture>
                      [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|transformUniform; environmentFilterUniform|]
                vkc

        // fin
        { TransformUniform = transformUniform; EnvironmentFilterUniform = environmentFilterUniform; Pipeline = pipeline }
    
    /// Destroy an EnvironmentFilterPipeline.
    let destroyEnvironmentFilterPipeline environmentFilterPipeline vkc =
        Pipeline.destroy environmentFilterPipeline.Pipeline vkc
    
    /// Draw an environment filter.
    let drawEnvironmentFilter
        (view : Matrix4x4)
        (projection : Matrix4x4)
        (viewProjection : Matrix4x4)
        (invertY : bool)
        (roughness : single)
        (resolution : single)
        (cubeMap : Texture)
        (sampler : Sampler)
        (geometry : CubeMapGeometry)
        (colorAttachment : VkImageView)
        (pipeline : EnvironmentFilterPipeline)
        (commandBuffer : VkCommandBuffer)
        (vkc : VulkanContext) =

        // only draw if render area is valid
        let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
        let mutable vkViewport = Hl.makeViewport invertY renderArea
        if Hl.validateRect renderArea then

            // only draw if required vkPipeline exists
            match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
            | Some vkPipeline ->

                // specify uniforms
                let mutable uniformDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->

                    // specify transform
                    let transform = LightMapTransform (view = view, projection = projection, viewProjection = viewProjection)
                    Buffer.uploadValue transform pipeline.TransformUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.TransformUniform vkSet vkc

                    // specify environment filter
                    let environmentFilter = EnvironmentFilter (roughness = roughness, resolution = resolution)
                    Buffer.uploadValue environmentFilter pipeline.EnvironmentFilterUniform vkc
                    Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.EnvironmentFilterUniform vkSet vkc

                // specify cube map
                let mutable cubeMapDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampledImage 0 0 cubeMap vkSet vkc

                // specify sampler
                let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline.Pipeline vkc $ fun vkSet ->
                    Pipeline.writeDescriptorSampler 0 0 sampler vkSet vkc

                // set up render
                let mutable rendering = Hl.makeRenderingInfo [|colorAttachment|] None renderArea None
                Vulkan.vkCmdBeginRendering (commandBuffer, asPointer &rendering)
                Vulkan.vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)
                Vulkan.vkCmdSetViewport (commandBuffer, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (commandBuffer, 0u, 1u, asPointer &renderArea)
                
                // bind vertex and index buffers
                let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
                let mutable vertexOffset = 0UL
                Vulkan.vkCmdBindVertexBuffers (commandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                Vulkan.vkCmdBindIndexBuffer (commandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &uniformDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &cubeMapDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)
                
                // draw
                Vulkan.vkCmdDrawIndexed (commandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)

                // tear down render
                Vulkan.vkCmdEndRendering commandBuffer

                // advance pipeline
                Pipeline.advance 1 pipeline.Pipeline

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."
    
    /// Create an environment filter map.
    let createEnvironmentFilterMap invertY resolution (environmentFilterSurface : CubeMapSurface) sampler colorFormat environmentFilterPipeline commandBuffer vkc =

        // create environment filter cube map
        let metadata = TextureMetadata.make resolution resolution
        let cubeMapParallel =
            TextureParallel.create
                (MipmapManual Constants.Render.EnvironmentFilterMips) (AttachmentColor false) TextureCubeMap [|VkImageUsageFlags.Sampled|]
                colorFormat Rgba metadata vkc
        let cubeMap = EagerTexture { TextureMetadata = TextureMetadata.empty; TextureParallel = cubeMapParallel }
        
        // compute views and projection
        let views =
            [|Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)
              Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)
              Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)|]
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)

        // render environment filter cube map mips
        for mip in 0 .. dec Constants.Render.EnvironmentFilterMips do
            let mipRoughness = single mip / single (dec Constants.Render.EnvironmentFilterMips)
            let mipResolution = single resolution * pown 0.5f mip
            for i in 0 .. dec 6 do

                // draw mip face
                let view = views.[i]
                let viewProjection = view * projection
                drawEnvironmentFilter
                    view
                    projection
                    viewProjection
                    invertY
                    mipRoughness
                    mipResolution
                    environmentFilterSurface.CubeMap
                    sampler
                    environmentFilterSurface.CubeMapGeometry
                    cubeMap.SubViews.[mip, i]
                    environmentFilterPipeline
                    commandBuffer
                    vkc

                // take a snapshot for testing
                // TODO: DJL: implement.
                //Hl.saveFramebufferRgbaToBitmap (int mipResolution) (int mipResolution) ("EnvironmentFilter." + string i + "." + string mip + ".bmp")

        // transition cubemap layout
        Hl.recordTransitionLayout true cubeMapParallel.MipLevels 0 6 VkImageAspectFlags.Color ColorAttachmentWrite ShaderRead cubeMapParallel.Image commandBuffer

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
    let destroyLightMap lightMap vkc =
        Texture.destroy lightMap.IrradianceMap vkc
        Texture.destroy lightMap.EnvironmentFilterMap vkc