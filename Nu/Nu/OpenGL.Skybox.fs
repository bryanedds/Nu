// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Skybox =

    /// Describes some skybox geometry that's loaded into VRAM.
    type [<StructuralEquality; NoComparison>] SkyboxGeometry =
        { Bounds : Box3
          PrimitiveType : OpenGL.PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          IndexBuffer : uint
          SkyboxVao : uint }

    /// Describes a renderable skybox surface.
    type [<NoEquality; NoComparison; Struct>] SkyboxSurface =
        { CubeMap : uint
          SkyboxGeometry : SkyboxGeometry }

    /// Create a mesh for a skybox.
    let CreateSkyboxMesh () =

        // make vertex data
        let vertexData =
            [|
                (*   positions   *)

                -1.0f; +1.0f; -1.0f
                -1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; -1.0f

                -1.0f; -1.0f; +1.0f
                -1.0f; -1.0f; -1.0f
                -1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; -1.0f
                -1.0f; +1.0f; +1.0f
                -1.0f; -1.0f; +1.0f

                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; -1.0f
                +1.0f; -1.0f; -1.0f

                -1.0f; -1.0f; +1.0f
                -1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; -1.0f; +1.0f
                -1.0f; -1.0f; +1.0f

                -1.0f; +1.0f; -1.0f
                +1.0f; +1.0f; -1.0f
                +1.0f; +1.0f; +1.0f
                +1.0f; +1.0f; +1.0f
                -1.0f; +1.0f; +1.0f
                -1.0f; +1.0f; -1.0f

                -1.0f; -1.0f; -1.0f
                -1.0f; -1.0f; +1.0f
                +1.0f; -1.0f; -1.0f
                +1.0f; -1.0f; -1.0f
                -1.0f; -1.0f; +1.0f
                +1.0f; -1.0f; +1.0f
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // make bounds trivially
        let bounds = box3 (v3Dup -1.0f) (v3Dup 2.0f)

        // fin
        (vertexData, indexData, bounds)

    /// Create physically-based geometry from a mesh.
    let CreateSkyboxGeometry (renderable, vertexData : single array, indexData : int array, bounds) =

        // make buffers
        let (vertices, vertexBuffer, indexBuffer, vao) =

            // make renderable
            if renderable then

                // initialize vao
                let vao = OpenGL.Gl.GenVertexArray ()
                OpenGL.Gl.BindVertexArray vao
                OpenGL.Hl.Assert ()

                // create vertex buffer
                let vertexBuffer = OpenGL.Gl.GenBuffer ()
                let vertexSize = (3 (*position*)) * sizeof<single>
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
                finally vertexDataPtr.Free ()
                OpenGL.Gl.EnableVertexAttribArray 0u
                OpenGL.Gl.VertexAttribPointer (0u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
                OpenGL.Hl.Assert ()

                // create index buffer
                let indexBuffer = OpenGL.Gl.GenBuffer ()
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
                try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
                finally indexDataPtr.Free ()
                OpenGL.Hl.Assert ()

                // finalize vao
                OpenGL.Gl.BindVertexArray 0u
                OpenGL.Hl.Assert ()

                // fin
                ([||], vertexBuffer, indexBuffer, vao)

            // fake buffers
            else

                // compute vertices
                let vertices = Array.zeroCreate (vertexData.Length / 3)
                for i in 0 .. dec vertices.Length do
                    let j = i * 3
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex
                
                // fin
                (vertices, 0u, 0u, 0u)

        // make skybox geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = OpenGL.PrimitiveType.Triangles
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              IndexBuffer = indexBuffer
              SkyboxVao = vao }

        // fin
        geometry

    /// Create skybox.
    let CreateSkybox renderable =
        let (vertexData, indexData, bounds) = CreateSkyboxMesh ()
        CreateSkyboxGeometry (renderable, vertexData, indexData, bounds)