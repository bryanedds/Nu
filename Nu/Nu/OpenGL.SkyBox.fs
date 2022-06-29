// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module SkyBox =

    /// Describes some sky box geometry that's loaded into VRAM.
    type [<StructuralEquality; NoComparison>] SkyBoxGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          IndexBuffer : uint
          SkyBoxVao : uint }

    /// Describes a renderable sky box surface.
    type [<NoEquality; NoComparison; Struct>] SkyBoxSurface =
        { CubeMap : uint
          SkyBoxGeometry : SkyBoxGeometry }

        static member make cubeMap geometry =
            { CubeMap = cubeMap;
              SkyBoxGeometry = geometry }

    /// Describes a sky box shader that's loaded into GPU.
    type [<StructuralEquality; NoComparison>] SkyBoxShader =
        { ViewUniform : int
          ProjectionUniform : int
          CubeMapUniform : int
          SkyBoxShader : uint }

    /// Create a mesh for a sky box.
    let CreateSkyBoxMesh () =

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

    /// Create sky geometry from a mesh.
    let CreateSkyBoxGeometryFromMesh (renderable, vertexData : single array, indexData : int array, bounds) =

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
                let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexDataPtr.AddrOfPinnedObject (), BufferUsage.StaticDraw)
                finally vertexDataPtr.Free ()
                Gl.EnableVertexAttribArray 0u
                Gl.VertexAttribPointer (0u, 3, VertexAttribType.Float, false, vertexSize, nativeint 0)
                Hl.Assert ()

                // create index buffer
                let indexBuffer = Gl.GenBuffer ()
                Gl.BindBuffer (BufferTarget.ElementArrayBuffer, indexBuffer)
                let indexDataSize = uint (indexData.Length * sizeof<uint>)
                let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
                try Gl.BufferData (BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), BufferUsage.StaticDraw)
                finally indexDataPtr.Free ()
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
                for i in 0 .. dec vertices.Length do
                    let j = i * 3
                    let vertex = v3 vertexData.[j] vertexData.[j+1] vertexData.[j+2]
                    vertices.[i] <- vertex
                
                // fin
                (vertices, 0u, 0u, 0u)

        // make sky box geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = PrimitiveType.Triangles
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              IndexBuffer = indexBuffer
              SkyBoxVao = vao }

        // fin
        geometry

    /// Create sky box geometry.
    let CreateSkyBoxGeometry renderable =
        let (vertexData, indexData, bounds) = CreateSkyBoxMesh ()
        CreateSkyBoxGeometryFromMesh (renderable, vertexData, indexData, bounds)

    /// Create a sky box shader.
    let CreateSkyBoxShader (shaderFilePath : string) =

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
          SkyBoxShader = shader }

    /// Draw a sky box.
    let DrawSkyBox
        (view : single array,
         projection : single array,
         cubeMap : uint,
         geometry : SkyBoxGeometry,
         shader : SkyBoxShader) =

        // setup state
        Gl.DepthFunc DepthFunction.Lequal
        Gl.Enable EnableCap.DepthTest
        Hl.Assert ()

        // setup shader
        Gl.UseProgram shader.SkyBoxShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.SkyBoxVao
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

    let CreateIrradianceMap (viewportOffset : Box2i, renderbufferWidth, renderbufferHeight, renderbuffer, framebuffer, irradianceShader, skyBoxSurface) =

        // create irradiance map
        let irradianceMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, irradianceMap)
        Hl.Assert ()

        // setup irradiated cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba16f, renderbufferWidth, renderbufferHeight, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Hl.Assert ()

        // setup framebuffer
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // mutate viewport
        Gl.Viewport (0, 0, renderbufferWidth, renderbufferHeight)
        Hl.Assert ()

        // render faces to irradiant map
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Backward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Backward, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspective (MathHelper.PiOver2, 1.0f, 0.1f, 10.0f)).ToArray ()
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, irradianceMap, 0)
            Gl.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            DrawSkyBox (views.[i], projection, skyBoxSurface.CubeMap, skyBoxSurface.SkyBoxGeometry, irradianceShader)
            Hl.Assert ()

        // restore viewport
        Gl.Viewport (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
        Hl.Assert ()

        // teardown framebuffer
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        irradianceMap