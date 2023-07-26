// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Collections.Generic
open System.Numerics
open System.IO
open FSharp.NativeInterop
open Prime
open Nu

[<RequireQualifiedAccess>]
module CubeMap =

    /// The key identifying a cube map.
    /// TODO: consider keeping the original cube map face list file path in key so that its faces can be changed when recreated.
    type CubeMapMemoKey =
        string * string * string * string * string * string

    /// Memoizes cube map loads.
    type [<ReferenceEquality>] CubeMapMemo =
        private
            { CubeMaps : Dictionary<CubeMapMemoKey, uint> }

        /// Make a cube map memoizer.
        static member make () =
            { CubeMaps = Dictionary HashIdentity.Structural }

    /// Attempt to create a cube map from 6 files.
    let private TryCreateCubeMapInternal (cubeMapOpt, faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =

        // bind new cube map
        let cubeMap = match cubeMapOpt with Some cubeMap -> cubeMap | None -> Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // load faces into cube map
        let mutable errorOpt = None
        let faceFilePaths = [|faceRightFilePath; faceLeftFilePath; faceTopFilePath; faceBottomFilePath; faceBackFilePath; faceFrontFilePath|]
        for i in 0 .. dec faceFilePaths.Length do
            if Option.isNone errorOpt then
                let faceFilePath = faceFilePaths.[i]
                match Texture.TryCreateImageData (Constants.OpenGl.UncompressedTextureFormat, false, faceFilePath) with
                | Some (metadata, imageData, disposer) ->
                    use _ = disposer
                    Gl.TexImage2D (LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i), 0, Constants.OpenGl.UncompressedTextureFormat, metadata.TextureWidth, metadata.TextureHeight, 0, PixelFormat.Bgra, PixelType.UnsignedByte, imageData)
                    Hl.Assert ()
                | None -> errorOpt <- Some ("Could not create surface for image from '" + faceFilePath + "'")

        // attempt to finalize cube map
        match errorOpt with
        | None ->
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
            Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
            Right cubeMap
        | Some error ->
            Gl.DeleteTextures [|cubeMap|]
            Left error

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) =
        TryCreateCubeMapInternal (None, faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath)

    /// Attempt to create a cube map from 6 files.
    let TryCreateCubeMapMemoized (cubeMapMemoKey, cubeMapMemo) =

        // deconstruct key
        let (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) = cubeMapMemoKey

        // memoize cube map
        let cubeMapKey =
            (Path.GetFullPath faceRightFilePath,
             Path.GetFullPath faceLeftFilePath,
             Path.GetFullPath faceTopFilePath,
             Path.GetFullPath faceBottomFilePath,
             Path.GetFullPath faceBackFilePath,
             Path.GetFullPath faceFrontFilePath)
        match cubeMapMemo.CubeMaps.TryGetValue cubeMapKey with
        | (false, _) ->

            // attempt to create cube map
            match TryCreateCubeMap (faceRightFilePath, faceLeftFilePath, faceTopFilePath, faceBottomFilePath, faceBackFilePath, faceFrontFilePath) with
            | Right cubeMap ->
                cubeMapMemo.CubeMaps.Add (cubeMapKey, cubeMap)
                Right cubeMap
            | Left error -> Left error

        // already exists
        | (true, cubeMap) -> Right cubeMap

    /// Recreate the memoized cube maps.
    let RecreateCubeMapsMemoized cubeMapMemo =
        for entry in cubeMapMemo.CubeMaps do
            let (f0, f1, f2, f3, f4, f5) = entry.Key
            let cubeMap = entry.Value
            TryCreateCubeMapInternal (Some cubeMap, f0, f1, f2, f3, f4, f5) |> ignore

    /// Delete a memoized cube map.
    let DeleteCubeMapMemoized cubeMapKey (cubeMapMemo : CubeMapMemo) =
        match cubeMapMemo.CubeMaps.TryGetValue cubeMapKey with
        | (true, cubeMap) ->
            Gl.DeleteTextures [|cubeMap|]
            cubeMapMemo.CubeMaps.Remove cubeMapKey |> ignore<bool>
        | (false, _) -> ()

    /// Delete memoized cube maps.
    let DeleteCubeMapsMemoized (cubeMapMemo) =
        for entry in cubeMapMemo.CubeMaps do
            Gl.DeleteTextures [|entry.Value|]
        cubeMapMemo.CubeMaps.Clear ()

    /// Describes some cube map geometry that's loaded into VRAM.
    type CubeMapGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          IndexBuffer : uint
          CubeMapVao : uint }

    /// Describes a renderable cube map surface.
    type [<StructuralEquality; NoComparison; Struct>] CubeMapSurface =
        { CubeMap : uint
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

    /// Create cube map geometry from a mesh.
    let CreateCubeMapGeometryFromMesh (renderable, vertexData : single Memory, indexData : int Memory, bounds) =

        // make buffers
        let (vertices, vertexBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = Gl.GenVertexArray ()
                Gl.BindVertexArray vao
                Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = Gl.GenBuffer ()
                let vertexSize = (3 (*position*)) * sizeof<single>
                Gl.BindBuffer (BufferTarget.ArrayBuffer, vertexBuffer)
                use vertexDataHnd = vertexData.Pin () in
                    let vertexDataNInt = vertexDataHnd.Pointer |> NativePtr.ofVoidPtr<single> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataNInt, BufferUsage.StaticDraw)
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribPointerType.Float, false, vertexSize, nativeint 0)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                use indexDataHnd = indexData.Pin () in
                    let indexDataNInt = indexDataHnd.Pointer |> NativePtr.ofVoidPtr<uint> |> NativePtr.toNativeInt
                    Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataNInt, BufferUsage.StaticDraw)
                Hl.Assert ()

                // finalize vao
                Gl.BindVertexArray 0u
                Hl.Assert ()

                // fin
                ([||], vertexBuffer, indexBuffer, vao)

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
                (vertices, 0u, 0u, 0u)

        // make cube map geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = PrimitiveType.Triangles
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              IndexBuffer = indexBuffer
              CubeMapVao = vao }

        // fin
        geometry

    /// Create cube map geometry.
    let CreateCubeMapGeometry renderable =
        let (vertexData, indexData, bounds) = CreateCubeMapMesh ()
        CreateCubeMapGeometryFromMesh (renderable, vertexData.AsMemory (), indexData.AsMemory (), bounds)

    /// Describes a cube map shader that's loaded into GPU.
    type CubeMapShader =
        { ViewUniform : int
          ProjectionUniform : int
          CubeMapUniform : int
          CubeMapShader : uint }

    /// Create a cube map shader.
    let CreateCubeMapShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          CubeMapUniform = cubeMapUniform
          CubeMapShader = shader }

    /// Draw a cube map.
    let DrawCubeMap
        (view : single array,
         projection : single array,
         cubeMap : uint,
         geometry : CubeMapGeometry,
         shader : CubeMapShader) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.CubeMapShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.CubeMapVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Gl.UseProgram 0u
        Hl.Assert ()

        // teardown state
        Gl.DepthFunc DepthFunction.Less
        Gl.Disable EnableCap.DepthTest