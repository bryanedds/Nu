// Nu Game Engine.
// Copyright (C) Bryan Edds.

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
type Eye =
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
      VertexBuffer : Nu.Vulkan.Buffer
      IndexBuffer : Nu.Vulkan.Buffer }

/// Describes a renderable cube map surface.
type [<Struct>] CubeMapSurface =
    { CubeMap : Texture
      CubeMapGeometry : CubeMapGeometry }

    static member make cubeMap geometry =
        { CubeMap = cubeMap;
          CubeMapGeometry = geometry }

/// Describes a cube map pipeline that's loaded into GPU.
type CubeMapPipeline =
    { EyeUniform : Nu.Vulkan.Buffer
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
    let tryCreateCubeMap faceRightFilePath faceLeftFilePath faceTopFilePath faceBottomFilePath faceBackFilePath faceFrontFilePath thread vkc =

        // load faces into cube map
        // NOTE: DJL: opengl seems to allow individual faces to differ in compression or maybe even size, but vulkan does not, so these are now determined by the first face.
        // TODO: DJL: maybe check that size and compression match?
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
                match Hl.tryCreateTextureData false faceFilePath with
                | Some textureData ->
                    match textureData with
                    | TextureData.TextureDataDotNet (metadata, bytes) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    Uncompressed.ImageFormat Uncompressed.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.uploadArray metadata 0 i bytes thread textureInternal vkc
                    | TextureData.TextureDataMipmap (metadata, compressed, bytes, _) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                let compression = if compressed then ColorCompression else Uncompressed
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    compression.ImageFormat compression.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.uploadArray metadata 0 i bytes thread textureInternal vkc
                    | TextureData.TextureDataNative (metadata, bytesPtr, disposer) ->
                        use _ = disposer
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                TextureInternal.create
                                    MipmapNone AttachmentNone TextureCubeMap VkImageUsageFlags.None
                                    Uncompressed.ImageFormat Uncompressed.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        TextureInternal.upload metadata 0 i bytesPtr thread textureInternal vkc
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            // TODO: P0: review error handling.
            let cubeMap = EagerTexture textureInternalOpt.Value
            Right cubeMap
        | Some error ->
            match textureInternalOpt with
            | Some textureInternal -> TextureInternal.destroy textureInternal vkc
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
    let createCubeMapGeometryFromMesh renderable (vertexData : single Memory) (indexData : int Memory) bounds vkc =

        // make buffers
        let (vertices, vertexBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create buffers
                let vertexBuffer = Buffer.createVertexStagedFromMemory vertexData vkc
                let indexBuffer = Buffer.createIndexStagedFromMemory indexData vkc

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
                (vertices, Unchecked.defaultof<Nu.Vulkan.Buffer>, Unchecked.defaultof<Nu.Vulkan.Buffer>)

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
    let createCubeMapGeometry renderable vkc =
        let (vertexData, indexData, bounds) = createCubeMapMesh ()
        createCubeMapGeometryFromMesh renderable (vertexData.AsMemory ()) (indexData.AsMemory ()) bounds vkc

    /// Destroy cube map geometry.
    let destroyCubeMapGeometry geometry vkc =
        Buffer.destroy geometry.VertexBuffer vkc
        Buffer.destroy geometry.IndexBuffer vkc
    
    /// Create a CubeMapPipeline.
    let createCubeMapPipeline shaderPath colorAttachmentFormat (vkc : VulkanContext) =

        // create eye buffer
        let eyeUniform = Buffer.create sizeof<Eye> Storage vkc

        // create pipeline
        let pipeline =
            Pipeline.create
                shaderPath [|VulkanUnblended|] [|false|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Single3 0|]|]
                [|Pipeline.descriptorSet<int>
                    [|Pipeline.descriptor 0 StorageBuffer VertexStage 1|]
                  Pipeline.descriptorSet<Texture>
                    [|Pipeline.descriptor 0 SampledImage FragmentStage 1|]
                  Pipeline.descriptorSet<Sampler>
                    [|Pipeline.descriptor 0 Sampler FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|]
                None // NOTE: DJL: not porting currently meaningless depth test as it imposes complexity cost in vulkan.
                [|eyeUniform|]
                vkc

        // fin
        { EyeUniform = eyeUniform; Pipeline = pipeline }
    
    /// Destroy a CubeMapPipeline.
    let destroyCubeMapPipeline cubeMapPipeline vkc =
        Pipeline.destroy cubeMapPipeline vkc
    
    /// Draw a cube map.
    let drawCubeMap
        (eyeCenter : Vector3)
        (view : Matrix4x4)
        (viewInverse : Matrix4x4)
        (projection : Matrix4x4)
        (projectionInverse : Matrix4x4)
        (viewProjection : Matrix4x4)
        (invertY : bool)
        (cubeMap : Texture)
        (sampler : Sampler)
        (geometry : CubeMapGeometry)
        (resolution : int)
        (colorAttachment : VkImageView)
        (pipeline : CubeMapPipeline)
        (commandBuffer : VkCommandBuffer)
        (vkc : VulkanContext) =

        // only draw if required vkPipeline exists
        match Pipeline.tryGetVkPipeline VulkanUnblended false pipeline.Pipeline with
        | Some vkPipeline ->

            // specify eye
            let mutable eyeDescriptorSet = Pipeline.specifyDescriptorSet 0 pipeline.Pipeline.DrawIndex pipeline.Pipeline vkc $ fun vkSet ->
                let eye = Eye (center = eyeCenter, view = view, viewInverse = viewInverse, projection = projection, projectionInverse = projectionInverse, viewProjection = viewProjection)
                Buffer.uploadValue eye pipeline.EyeUniform vkc
                Pipeline.writeDescriptorStorageBuffer 0 0 pipeline.EyeUniform vkSet vkc

            // specify material
            let mutable materialDescriptorSet = Pipeline.specifyDescriptorSet 1 cubeMap pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampledImage 0 0 cubeMap vkSet vkc

            // specify sampler
            let mutable samplerDescriptorSet = Pipeline.specifyDescriptorSet 2 sampler pipeline.Pipeline vkc $ fun vkSet ->
                Pipeline.writeDescriptorSampler 0 0 sampler vkSet vkc

            // set up render
            let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
            let mutable vkViewport = Hl.makeViewport invertY renderArea
            let mutable renderingInfo = Hl.makeRenderingInfo [|colorAttachment|] None renderArea None
            Vulkan.vkCmdBeginRendering (commandBuffer, asPointer &renderingInfo)
            Vulkan.vkCmdSetViewport (commandBuffer, 0u, 1u, asPointer &vkViewport)
            Vulkan.vkCmdSetScissor (commandBuffer, 0u, 1u, asPointer &renderArea)
                
            // set up pipeline
            Vulkan.vkCmdBindPipeline (commandBuffer, VkPipelineBindPoint.Graphics, vkPipeline)

            // bind vertex and index buffers
            let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
            let mutable vertexOffset = 0UL
            Vulkan.vkCmdBindVertexBuffers (commandBuffer, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
            Vulkan.vkCmdBindIndexBuffer (commandBuffer, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

            // bind descriptor sets
            Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &eyeDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &materialDescriptorSet, 0u, nullPtr)
            Vulkan.vkCmdBindDescriptorSets (commandBuffer, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 2u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)

            // draw
            Vulkan.vkCmdDrawIndexed (commandBuffer, uint geometry.ElementCount, 1u, 0u, 0, 0u)
        
            // tear down render
            Vulkan.vkCmdEndRendering commandBuffer

            // advance pipeline
            Pipeline.advance 1 pipeline.Pipeline

        // abort
        | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

/// Memoizes cube map loads (and may at some point potentially thread them).
type CubeMapClient () =

    let cubeMaps = Dictionary HashIdentity.Structural

    /// Memoized cube maps.
    member this.CubeMaps = cubeMaps

    /// Attempt to create a cube map from 6 files.
    member this.TryCreateCubeMap cubeMapKey thread vkc =

        // memoize cube map
        match cubeMaps.TryGetValue cubeMapKey with
        | (false, _) ->

            // attempt to create cube map
            let (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) = cubeMapKey
            match CubeMap.tryCreateCubeMap faceRightFilePath faceLeftFilePath faceTopFilePath faceBottomFilePath faceBackFilePath faceFrontFilePath thread vkc with
            | Right cubeMap ->
                cubeMaps.Add (cubeMapKey, cubeMap)
                Right cubeMap
            | Left error -> Left error

        // already exists
        | (true, cubeMap) -> Right cubeMap