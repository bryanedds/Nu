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

    /// Create a reflection map.
    let CreateReflectionMap (render, resolution, origin, ambientColor, ambientBrightness, commandBuffer, vkc) =

        // create reflection cube map
        let metadata = Texture.TextureMetadata.make resolution resolution
        let reflectionCubeMapInternal =
            Texture.TextureInternal.create
                Texture.MipmapNone (Texture.AttachmentColor false) Texture.TextureCubeMap [|VkImageUsageFlags.Sampled; VkImageUsageFlags.TransferDst|]
                Hl.Rgba16f Hl.Rgba metadata vkc
        let reflectionCubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = reflectionCubeMapInternal }

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
            render false lightAmbientOverride origin view viewSkyBox frustum projection viewProjection projection bounds i reflectionCubeMapInternal.Image

            // take a snapshot for testing
            // TODO: DJL: implement.
            //Hl.SaveFramebufferRgbaToBitmap (resolution, resolution, "Reflection." + string reflectionCubeMapId + "." + string i + ".bmp")
            //Hl.Assert ()

        // transition cubemap layout
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color Hl.ColorAttachmentWrite Hl.ShaderRead reflectionCubeMapInternal.Image commandBuffer

        // fin
        reflectionCubeMap

    let CreateIrradianceMap (invertY, resolution, cubeMapSurface : CubeMap.CubeMapSurface, sampler, colorFormat, irradiancePipeline, commandBuffer, vkc) =

        // create irradiance cube map
        let metadata = Texture.TextureMetadata.make resolution resolution
        let cubeMapInternal =
            Texture.TextureInternal.create
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
            CubeMap.DrawCubeMap (invertY, view, projection, viewProjection, cubeMapSurface.CubeMap, sampler, cubeMapSurface.CubeMapGeometry, resolution, cubeMap.SubViews.[0, i], irradiancePipeline, commandBuffer, vkc)

            // take a snapshot for testing
            // TODO: DJL: implement.
            //Hl.SaveFramebufferRgbaToBitmap (resolution, resolution, "Irradiance." + string cubeMapId + "." + string i + ".bmp")

        // transition cubemap layout
        Hl.recordTransitionLayout true 1 0 6 VkImageAspectFlags.Color Hl.ColorAttachmentWrite Hl.ShaderRead cubeMapInternal.Image commandBuffer
        
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

        // create uniform buffers
        let transformUniform = Buffer.Buffer.create sizeof<Transform> Buffer.Storage vkc
        let environmentFilterUniform = Buffer.Buffer.create sizeof<EnvironmentFilter> Buffer.Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                shaderPath [|Pipeline.NoBlend|] [|false|]
                [|Pipeline.vertex 0 ((3 (*position*)) * sizeof<single>) VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.StorageBuffer Hl.FragmentStage 1|]
                  Pipeline.descriptorSet<Texture.Texture>
                      [|Pipeline.descriptor 0 Hl.SampledImage Hl.FragmentStage 1|]
                  Pipeline.descriptorSet<Texture.Sampler>
                    [|Pipeline.descriptor 0 Hl.Sampler Hl.FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|transformUniform; environmentFilterUniform|]
                vkc

        // fin
        { TransformUniform = transformUniform; EnvironmentFilterUniform = environmentFilterUniform; Pipeline = pipeline }
    
    /// Destroy an EnvironmentFilterPipeline.
    let DestroyEnvironmentFilterPipeline (environmentFilterPipeline, vkc) =
        Pipeline.Pipeline.destroy environmentFilterPipeline.Pipeline vkc
    
    /// Draw an environment filter.
    let DrawEnvironmentFilter
        (invertY : bool,
         view : Matrix4x4,
         projection : Matrix4x4,
         viewProjection : Matrix4x4,
         roughness : single,
         resolution : single,
         cubeMap : Texture.Texture,
         sampler : Texture.Sampler,
         geometry : CubeMap.CubeMapGeometry,
         colorAttachment : VkImageView,
         pipeline : EnvironmentFilterPipeline,
         commandBuffer : VkCommandBuffer,
         vkc : Hl.VulkanContext) =

        // only draw if render area is valid
        let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
        let mutable vkViewport = Hl.makeViewport invertY renderArea
        if Hl.validateRect renderArea then

            // only draw if required vkPipeline exists
            match Pipeline.Pipeline.tryGetVkPipeline Pipeline.NoBlend false pipeline.Pipeline with
            | Some vkPipeline ->

                // specify uniforms
                let mutable uniformDescriptorSet = pipeline.Pipeline.SpecifyDescriptorSet 0 pipeline.Pipeline.DrawIndex vkc $ fun vkSet ->

                    // specify transform
                    let transform = Transform (view = view, projection = projection, viewProjection = viewProjection)
                    Buffer.Buffer.uploadValue transform pipeline.TransformUniform vkc
                    Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.TransformUniform vkSet vkc

                    // specify environment filter
                    let environmentFilter = EnvironmentFilter (roughness = roughness, resolution = resolution)
                    Buffer.Buffer.uploadValue environmentFilter pipeline.EnvironmentFilterUniform vkc
                    Pipeline.Pipeline.writeDescriptorStorageBuffer 1 0 pipeline.EnvironmentFilterUniform vkSet vkc

                // specify cube map
                let mutable cubeMapDescriptorSet = pipeline.Pipeline.SpecifyDescriptorSet 1 cubeMap vkc $ fun vkSet ->
                    Pipeline.Pipeline.writeDescriptorSampledImage 0 0 cubeMap vkSet vkc

                // specify sampler
                let mutable samplerDescriptorSet = pipeline.Pipeline.SpecifyDescriptorSet 2 sampler vkc $ fun vkSet ->
                    Pipeline.Pipeline.writeDescriptorSampler 0 0 sampler vkSet vkc

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
                pipeline.Pipeline.Advance 1

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."
    
    /// Create an environment filter map.
    let CreateEnvironmentFilterMap (invertY, resolution, environmentFilterSurface : CubeMap.CubeMapSurface, sampler, colorFormat, environmentFilterPipeline, commandBuffer, vkc) =

        // create environment filter cube map
        let metadata = Texture.TextureMetadata.make resolution resolution
        let cubeMapInternal =
            Texture.TextureInternal.create
                (Texture.MipmapManual Constants.Render.EnvironmentFilterMips) (Texture.AttachmentColor false) Texture.TextureCubeMap [|VkImageUsageFlags.Sampled|]
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

        // render environment filter cube map mips
        for mip in 0 .. dec Constants.Render.EnvironmentFilterMips do
            let mipRoughness = single mip / single (dec Constants.Render.EnvironmentFilterMips)
            let mipResolution = single resolution * pown 0.5f mip
            for i in 0 .. dec 6 do

                // draw mip face
                let view = views.[i]
                let viewProjection = view * projection
                DrawEnvironmentFilter
                    (invertY,
                     view,
                     projection,
                     viewProjection,
                     mipRoughness,
                     mipResolution,
                     environmentFilterSurface.CubeMap,
                     sampler,
                     environmentFilterSurface.CubeMapGeometry,
                     cubeMap.SubViews.[mip, i],
                     environmentFilterPipeline,
                     commandBuffer,
                     vkc)

                // take a snapshot for testing
                // TODO: DJL: implement.
                //Hl.SaveFramebufferRgbaToBitmap (int mipResolution, int mipResolution, "EnvironmentFilter." + string i + "." + string mip + ".bmp")

        // transition cubemap layout
        Hl.recordTransitionLayout true cubeMapInternal.MipLevels 0 6 VkImageAspectFlags.Color Hl.ColorAttachmentWrite Hl.ShaderRead cubeMapInternal.Image commandBuffer

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