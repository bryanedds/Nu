// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open Vortice.Vulkan
open Prime
open Nu

[<Struct; StructLayout (LayoutKind.Explicit)>]
type EyeStruct =
    [<FieldOffset(0)>] val mutable center : Vector3
    [<FieldOffset(16)>] val mutable view : Matrix4x4
    [<FieldOffset(80)>] val mutable viewInverse : Matrix4x4
    [<FieldOffset(144)>] val mutable projection : Matrix4x4
    [<FieldOffset(208)>] val mutable projectionInverse : Matrix4x4
    [<FieldOffset(272)>] val mutable viewProjection : Matrix4x4

/// Describes some cube map geometry that's loaded into VRAM.
type CubeMapGeometry =
    { Bounds : Box3
      PrimitiveTopology : VkPrimitiveTopology
      ElementCount : int
      Vertices : Vector3 array
      VertexBuffer : VulkanBuffer
      IndexBuffer : VulkanBuffer }

/// Describes a renderable cube map surface.
type [<Struct>] CubeMapSurface =
    { Flipped : bool
      CubeMap : Texture
      Geometry : CubeMapGeometry }

    static member make flipped cubeMap geometry =
        { Flipped = flipped
          CubeMap = cubeMap
          Geometry = geometry }

/// Describes a cube map pipeline that's loaded into GPU.
type CubeMapPipeline =
    { EyeUniform : VulkanBuffer
      Pipeline : Pipeline }

/// The key identifying a cube map.
type CubeMapKey =
    string * string * string * string * string * string

[<RequireQualifiedAccess>]
module CubeMap =

    let VertexSize =
        (3 (*position*)) * sizeof<single>

    /// Attempt to create a cube map from 6 files.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the included
    /// files can't be located.
    let tryCreateCubeMap faceRightFilePath faceLeftFilePath faceTopFilePath faceBottomFilePath faceBackFilePath faceFrontFilePath thread context =

        // load faces into cube map
        let mutable textureInternalOpt = None
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths[i]
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".png") else faceFilePath // in case of PsdToPng
                let faceFilePath =
                    if not (File.Exists faceFilePath) then // in case of BlockCompress
                        match Constants.Render.TextureBlockCompression with
                        | BcCompression -> PathF.ChangeExtension (faceFilePath, ".dds")
                        | AstcCompression -> PathF.ChangeExtension (faceFilePath, ".ktx")
                    else faceFilePath
                match TextureData.tryCreate false faceFilePath with
                | Some textureData ->
                    match textureData with
                    | TextureData.TextureDataDotNet (metadata, bytes) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    Uncompressed.ImageFormat Uncompressed.PixelFormat metadata context
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.uploadArray metadata 0 i bytes thread textureInternal context
                    | TextureData.TextureDataMipmap (metadata, compressed, bytes, _) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                let compression = if compressed then ColorCompression else Uncompressed
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    compression.ImageFormat compression.PixelFormat metadata context
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.uploadArray metadata 0 i bytes thread textureInternal context
                    | TextureData.TextureDataNative (metadata, bytesPtr, disposer) ->
                        use _ = disposer
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    Uncompressed.ImageFormat Uncompressed.PixelFormat metadata context
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.upload metadata 0 i bytesPtr thread textureInternal context
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            let cubeMap = EagerTexture textureInternalOpt.Value
            Right cubeMap
        | Some error ->
            match textureInternalOpt with
            | Some textureInternal -> TextureInternal.destroy textureInternal context
            | None -> ()
            Left error

    /// Create a mesh for a cube map.
    let createCubeMapMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)

                // right
                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; -1.0f
                +1.0f; -1.0f; -1.0f

                // left
                -1.0f; -1.0f; +1.0f
                -1.0f; -1.0f; -1.0f
                -1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; +1.0f
                -1.0f; -1.0f; +1.0f

                // top
                -1.0f; +1.0f; -1.0f
                +1.0f; +1.0f; -1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                -1.0f; +1.0f; +1.0f
                -1.0f; +1.0f; -1.0f

                // bottom
                -1.0f; -1.0f; -1.0f
                -1.0f; -1.0f; +1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                -1.0f; -1.0f; +1.0f
                +1.0f; -1.0f; +1.0f

                // back
                -1.0f; -1.0f; +1.0f
                -1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; -1.0f; +1.0f
                -1.0f; -1.0f; +1.0f

                // front
                -1.0f; +1.0f; -1.0f
                -1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; -1.0f
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -1.0f) (v3Dup 2.0f)

        // fin
        (vertexData, indexData, bounds)
    
    /// Create cube map geometry from a mesh.
    let createCubeMapGeometryFromMesh renderable (vertexData : single Memory) (indexData : int Memory) bounds context =

        // make buffers
        let (vertices, vertexBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create buffers
                let vertexBuffer = VulkanBuffer.createVertexStagedFromMemory vertexData context
                let indexBuffer = VulkanBuffer.createIndexStagedFromMemory indexData context

                // fin
                ([||], vertexBuffer, indexBuffer)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 3)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 3
                    let vertex = v3 vertexData[j] vertexData[j+1] vertexData[j+2]
                    vertices[i] <- vertex
                
                // fin
                (vertices, Unchecked.defaultof<VulkanBuffer>, Unchecked.defaultof<VulkanBuffer>)

        // make cube map geometry
        let geometry =
            { Bounds = bounds
              PrimitiveTopology = VkPrimitiveTopology.TriangleList
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              IndexBuffer = indexBuffer }

        // fin
        geometry

    /// Create cube map geometry.
    let createCubeMapGeometry renderable context =
        let (vertexData, indexData, bounds) = createCubeMapMesh ()
        createCubeMapGeometryFromMesh renderable (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds context

    /// Destroy cube map geometry.
    let destroyCubeMapGeometry geometry context =
        VulkanBuffer.destroy geometry.VertexBuffer context
        VulkanBuffer.destroy geometry.IndexBuffer context
    
    /// Create a CubeMapPipeline.
    let createCubeMapPipeline shaderPath colorAttachmentFormat (context : VulkanContext) =

        // create eye buffer
        let eyeUniform = VulkanBuffer.create Uniform sizeof<EyeStruct> context

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 UniformBuffer VertexStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|] None
                [|eyeUniform|]

        // fin
        { EyeUniform = eyeUniform; Pipeline = pipeline }
    
    /// Destroy a CubeMapPipeline.
    let destroyCubeMapPipeline cubeMapPipeline context =
        Pipeline.destroy cubeMapPipeline context
    
    /// Draw a cube map.
    let drawCubeMap
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (projectionUnflipped : Matrix4x4)
        (flipped : bool)
        (cubeMap : Texture)
        (sampler : Sampler)
        (geometry : CubeMapGeometry)
        (resolution : int)
        (colorAttachment : VkImageView)
        (pipeline : CubeMapPipeline)
        (getCommandBuffer : unit -> VkCommandBuffer)
        (advanceCommandBufferWhenNeeded : unit -> unit)
        (context : VulkanContext) =

        // compute vulkan-appropriate matrices
        let projection = if flipped then projectionUnflipped.Flipped else projectionUnflipped
        let viewInverse = view.Inverted
        let projectionInverse = projection.Inverted
        let viewProjection = view * projection

        // only draw when required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify eye
            let mutable eyeDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline $ fun vkSet ->
                let eye = EyeStruct (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                VulkanBuffer.uploadValue eye pipeline.EyeUniform context
                Pipeline.writeDescriptorUniformBuffer 0 0 pipeline.EyeUniform vkSet

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline $ fun vkSet ->
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
            DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, &&eyeDescriptorSet, 0u, nullPtr)
            DeviceApi.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, &&materialDescriptorSet, 0u, nullPtr)
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

/// Memoizes cube map loads (and may at some point potentially thread them).
type CubeMapClient () =

    let cubeMaps = Dictionary HashIdentity.Structural

    /// Memoized cube maps.
    member this.CubeMaps = cubeMaps

    /// Attempt to create a cube map from 6 files.
    member this.TryCreateCubeMap cubeMapKey thread context =

        // memoize cube map
        match cubeMaps.TryGetValue cubeMapKey with
        | (false, _) ->

            // attempt to create cube map
            let (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) = cubeMapKey
            match CubeMap.tryCreateCubeMap faceRightFilePath faceLeftFilePath faceTopFilePath faceBottomFilePath faceBackFilePath faceFrontFilePath thread context with
            | Right cubeMap ->
                cubeMaps.Add (cubeMapKey, cubeMap)
                Right cubeMap
            | Left error -> Left error

        // already exists
        | (true, cubeMap) -> Right cubeMap