// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Vortice.Vulkan
open System
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module CubeMap =

    /// Attempt to create a cube map from 6 files.
    /// Uses file name-based inferences to look for texture files in case the ones that were hard-coded in the included
    /// files can't be located.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, thread, vkc) =

        // load faces into cube map
        // NOTE: DJL: opengl seems to allow individual faces to differ in compression or maybe even size, but vulkan does not, so these are now determined by the first face.
        // TODO: DJL: maybe check that size and compression match?
        let mutable textureInternalOpt = None
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".png") else faceFilePath // in case of PsdToPng
                let faceFilePath =
                    if not (File.Exists faceFilePath) then // in case of BlockCompress
                        match Constants.Render.TextureBlockCompression with
                        | BcCompression -> PathF.ChangeExtension (faceFilePath, ".dds")
                        | AstcCompression -> PathF.ChangeExtension (faceFilePath, ".ktx")
                    else faceFilePath
                match Texture.TryCreateTextureData (false, faceFilePath) with
                | Some textureData ->
                    match textureData with
                    | Texture.TextureData.TextureDataDotNet (metadata, bytes) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                Texture.TextureInternal.create
                                    Texture.MipmapNone Texture.AttachmentNone Texture.TextureCubeMap [||]
                                    Texture.Uncompressed.ImageFormat Texture.Uncompressed.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        Texture.TextureInternal.uploadArray metadata 0 i bytes thread textureInternal vkc
                    | Texture.TextureData.TextureDataMipmap (metadata, compressed, bytes, _) ->
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                let compression = if compressed then Texture.ColorCompression else Texture.Uncompressed
                                Texture.TextureInternal.create
                                    Texture.MipmapNone Texture.AttachmentNone Texture.TextureCubeMap [||]
                                    compression.ImageFormat compression.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        Texture.TextureInternal.uploadArray metadata 0 i bytes thread textureInternal vkc
                    | Texture.TextureData.TextureDataNative (metadata, bytesPtr, disposer) ->
                        use _ = disposer
                        let textureInternal =
                            match textureInternalOpt with
                            | Some textureInternal -> textureInternal
                            | None ->
                                Texture.TextureInternal.create
                                    Texture.MipmapNone Texture.AttachmentNone Texture.TextureCubeMap [||]
                                    Texture.Uncompressed.ImageFormat Texture.Uncompressed.PixelFormat metadata vkc
                        textureInternalOpt <- Some textureInternal
                        Texture.TextureInternal.upload metadata 0 i bytesPtr thread textureInternal vkc
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            // TODO: DJL: review error handling.
            let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureInternal = textureInternalOpt.Value }
            Right cubeMap
        | Some error ->
            match textureInternalOpt with
            | Some vulkanTexture -> Texture.TextureInternal.destroy vulkanTexture vkc
            | None -> ()
            Left error

    /// Describes some cube map geometry that's loaded into VRAM.
    type CubeMapGeometry =
        { Bounds : Box3
          PrimitiveTopology : VkPrimitiveTopology
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : Buffer.Buffer
          IndexBuffer : Buffer.Buffer }

    /// Describes a renderable cube map surface.
    type [<Struct>] CubeMapSurface =
        { CubeMap : Texture.Texture
          CubeMapGeometry : CubeMapGeometry }

        static member make cubeMap geometry =
            { CubeMap = cubeMap;
              CubeMapGeometry = geometry }

    /// Create a mesh for a cube map.
    let CreateCubeMapMesh () =

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

    let VertexSize =
        (3 (*position*)) * sizeof<single>
    
    /// Create cube map geometry from a mesh.
    let CreateCubeMapGeometryFromMesh (renderable, vertexData : single Memory, indexData : int Memory, bounds, vkc) =

        // make buffers
        let (vertices, vertexBuffer, indexBuffer) =

            // make renderable
            if renderable then

                // create buffers
                let vertexBuffer = Buffer.Buffer.createVertexStagedFromMemory vertexData vkc
                let indexBuffer = Buffer.Buffer.createIndexStagedFromMemory indexData vkc

                // fin
                ([||], vertexBuffer, indexBuffer)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 3)
                let vertexData = vertexData.Span
                for i in 0 .. dec vertices.Length do
                    let j = i * 3
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex
                
                // fin
                (vertices, Unchecked.defaultof<Buffer.Buffer>, Unchecked.defaultof<Buffer.Buffer>)

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
    let CreateCubeMapGeometry renderable vkc =
        let (vertexData, indexData, bounds) = CreateCubeMapMesh ()
        CreateCubeMapGeometryFromMesh (renderable, vertexData.AsMemory (), indexData.AsMemory (), bounds, vkc)

    /// Destroy cube map geometry.
    let DestroyCubeMapGeometry geometry vkc =
        Buffer.Buffer.destroy geometry.VertexBuffer vkc
        Buffer.Buffer.destroy geometry.IndexBuffer vkc

    [<Struct; StructLayout(LayoutKind.Explicit)>]
    type Transform =
        [<FieldOffset(0)>] val mutable view : Matrix4x4
        [<FieldOffset(64)>] val mutable projection : Matrix4x4
        [<FieldOffset(128)>] val mutable viewProjection : Matrix4x4

    /// Describes a cube map pipeline that's loaded into GPU.
    type CubeMapPipeline =
        { TransformUniform : Buffer.Buffer
          Pipeline : Pipeline.Pipeline }
    
    /// Create a CubeMapPipeline.
    let CreateCubeMapPipeline (shaderPath, maxCubes, colorAttachmentFormat, vkc : Hl.VulkanContext) =

        // create pipeline
        let pipeline =
            Pipeline.Pipeline.create
                shaderPath 0 [|Pipeline.NoBlend|] [|false|]
                [|Pipeline.vertex 0 VertexSize VkVertexInputRate.Vertex
                    [|Pipeline.attribute 0 Hl.Single3 0|]|]
                [|Pipeline.descriptorSet Hl.BulkNone (6 * maxCubes)
                    [|Pipeline.descriptor 0 Hl.StorageBuffer Hl.VertexStage 1
                      Pipeline.descriptor 1 Hl.SampledImage Hl.FragmentStage 1|]
                  Pipeline.descriptorSet Hl.BulkNone 1
                    [|Pipeline.descriptor 0 Hl.Sampler Hl.FragmentStage 1|]|]
                [||] [|colorAttachmentFormat|]
                None // NOTE: DJL: not porting currently meaningless depth test as it imposes complexity cost in vulkan.
                vkc

        // create uniform buffer
        let transformUniform = Buffer.Buffer.create sizeof<Transform> Buffer.Storage vkc

        // fin
        { TransformUniform = transformUniform; Pipeline = pipeline }
    
    /// Destroy a CubeMapPipeline.
    let DestroyCubeMapPipeline (cubeMapPipeline, vkc) =
        Buffer.Buffer.destroy cubeMapPipeline.TransformUniform vkc
        Pipeline.Pipeline.destroy cubeMapPipeline.Pipeline vkc
    
    /// Draw a cube map.
    let DrawCubeMap
        (drawIndex : int,
         cb : VkCommandBuffer,
         invertY : bool,
         view : Matrix4x4,
         projection : Matrix4x4,
         viewProjection : Matrix4x4,
         cubeMap : Texture.Texture,
         sampler : Texture.Sampler,
         geometry : CubeMapGeometry,
         resolution : int,
         colorAttachment : VkImageView,
         pipeline : CubeMapPipeline,
         vkc : Hl.VulkanContext) =

        // bind uniforms
        let mutable transform = Transform ()
        transform.view <- view
        transform.projection <- projection
        transform.viewProjection <- viewProjection
        Buffer.Buffer.uploadValue drawIndex 0 0 transform pipeline.TransformUniform vkc
        Pipeline.Pipeline.writeDescriptorStorageBuffer 0 0 drawIndex 0 pipeline.TransformUniform.[drawIndex] pipeline.Pipeline vkc

        // bind texture
        Pipeline.Pipeline.writeDescriptorSampledImage 0 1 drawIndex 0 cubeMap.ImageView pipeline.Pipeline vkc
        Pipeline.Pipeline.writeDescriptorSampler 1 0 0 0 sampler pipeline.Pipeline vkc

        // make viewport and scissor
        let mutable renderArea = VkRect2D (0, 0, uint resolution, uint resolution)
        let mutable vkViewport = Hl.makeViewport invertY renderArea
        let mutable scissor = renderArea

        // only draw if scissor (and therefore also viewport) is valid
        if Hl.validateRect scissor then

            // only draw if required vkPipeline exists
            match Pipeline.Pipeline.tryGetVkPipeline Pipeline.NoBlend false pipeline.Pipeline with
            | Some vkPipeline ->
            
                // init render
                let mutable rendering = Hl.makeRenderingInfo [|colorAttachment|] None renderArea None
                Vulkan.vkCmdBeginRendering (cb, asPointer &rendering)

                // bind pipeline
                Vulkan.vkCmdBindPipeline (cb, VkPipelineBindPoint.Graphics, vkPipeline)

                // set viewport and scissor
                Vulkan.vkCmdSetViewport (cb, 0u, 1u, asPointer &vkViewport)
                Vulkan.vkCmdSetScissor (cb, 0u, 1u, asPointer &scissor)
                
                // bind vertex and index buffer
                let mutable vertexBuffer = geometry.VertexBuffer.VkBuffer
                let mutable vertexOffset = 0UL
                Vulkan.vkCmdBindVertexBuffers (cb, 0u, 1u, asPointer &vertexBuffer, asPointer &vertexOffset)
                Vulkan.vkCmdBindIndexBuffer (cb, geometry.IndexBuffer.VkBuffer, 0UL, VkIndexType.Uint32)

                // bind descriptor sets
                let mutable mainDescriptorSet = pipeline.Pipeline.VkDescriptorSet 0 drawIndex
                let mutable samplerDescriptorSet = pipeline.Pipeline.VkDescriptorSet 1 0
                Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 0u, 1u, asPointer &mainDescriptorSet, 0u, nullPtr)
                Vulkan.vkCmdBindDescriptorSets (cb, VkPipelineBindPoint.Graphics, pipeline.Pipeline.PipelineLayout, 1u, 1u, asPointer &samplerDescriptorSet, 0u, nullPtr)
                
                // draw
                Vulkan.vkCmdDrawIndexed (cb, uint geometry.ElementCount, 1u, 0u, 0, 0u)
                Hl.reportDrawCall 1
        
                // end render
                Vulkan.vkCmdEndRendering cb

            // abort
            | None -> Log.warnOnce "Cannot draw because VkPipeline does not exist."

    /// The key identifying a cube map.
    type CubeMapKey =
        string * string * string * string * string * string

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
                match TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath, thread, vkc) with
                | Right cubeMap ->
                    cubeMaps.Add (cubeMapKey, cubeMap)
                    Right cubeMap
                | Left error -> Left error

            // already exists
            | (true, cubeMap) -> Right cubeMap