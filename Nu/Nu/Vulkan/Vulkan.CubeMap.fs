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
        let mutable vulkanTextureOpt = None
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".png") else faceFilePath // in case of PsdToPng
                let faceFilePath = if not (File.Exists faceFilePath) then PathF.ChangeExtension (faceFilePath, ".dds") else faceFilePath // in case of ConvertToDds
                match Texture.TryCreateTextureData (false, faceFilePath) with
                | Some textureData ->
                    match textureData with
                    | Texture.TextureData.TextureDataDotNet (metadata, bytes) ->
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None -> Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true Texture.Uncompressed metadata vkc
                        vulkanTextureOpt <- Some vulkanTexture
                        Texture.VulkanTexture.uploadArray metadata 0 i bytes thread vulkanTexture vkc
                    | Texture.TextureData.TextureDataMipmap (metadata, compressed, bytes, _) ->
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None ->
                                let compression = if compressed then Texture.ColorCompression else Texture.Uncompressed
                                Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true compression metadata vkc
                        vulkanTextureOpt <- Some vulkanTexture
                        Texture.VulkanTexture.uploadArray metadata 0 i bytes thread vulkanTexture vkc
                    | Texture.TextureData.TextureDataNative (metadata, bytesPtr, disposer) ->
                        use _ = disposer
                        let vulkanTexture =
                            match vulkanTextureOpt with
                            | Some vulkanTexture -> vulkanTexture
                            | None -> Texture.VulkanTexture.create Texture.Bgra Vulkan.VK_FILTER_LINEAR Vulkan.VK_FILTER_LINEAR false Texture.MipmapNone true Texture.Uncompressed metadata vkc
                        vulkanTextureOpt <- Some vulkanTexture
                        Texture.VulkanTexture.upload metadata 0 i bytesPtr thread vulkanTexture vkc
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            // TODO: DJL: review error handling.
            let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; VulkanTexture = vulkanTextureOpt.Value }
            Right cubeMap
        | Some error ->
            match vulkanTextureOpt with
            | Some vulkanTexture -> Texture.VulkanTexture.destroy vulkanTexture vkc
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
              PrimitiveTopology = Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
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