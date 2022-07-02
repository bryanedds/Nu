// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace OpenGL
open System
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Environment =

    /// Describes some environment geometry that's loaded into VRAM.
    type [<StructuralEquality; NoComparison>] EnvironmentGeometry =
        { Bounds : Box3
          PrimitiveType : PrimitiveType
          ElementCount : int
          Vertices : Vector3 array
          VertexBuffer : uint
          IndexBuffer : uint
          EnvironmentVao : uint }

    /// Describes a renderable environment surface.
    type [<NoEquality; NoComparison; Struct>] EnvironmentSurface =
        { CubeMap : uint
          EnvironmentGeometry : EnvironmentGeometry }

        static member make cubeMap geometry =
            { CubeMap = cubeMap;
              EnvironmentGeometry = geometry }

    /// Describes a environment shader that's loaded into GPU.
    type [<StructuralEquality; NoComparison>] EnvironmentShader =
        { ViewUniform : int
          ProjectionUniform : int
          RoughnessUniform : int
          ResolutionUniform : int
          CubeMapUniform : int
          EnvironmentShader : uint }

    /// Create a mesh for a environment.
    let CreateEnvironmentMesh () =

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
    let CreateEnvironmentGeometryFromMesh (renderable, vertexData : single array, indexData : int array, bounds) =

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

        // make environment geometry
        let geometry =
            { Bounds = bounds
              PrimitiveType = PrimitiveType.Triangles
              ElementCount = indexData.Length
              Vertices = vertices
              VertexBuffer = vertexBuffer
              IndexBuffer = indexBuffer
              EnvironmentVao = vao }

        // fin
        geometry

    /// Create environment geometry.
    let CreateEnvironmentGeometry renderable =
        let (vertexData, indexData, bounds) = CreateEnvironmentMesh ()
        CreateEnvironmentGeometryFromMesh (renderable, vertexData, indexData, bounds)

    /// Create a environment shader.
    let CreateEnvironmentShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let roughnessUniform = Gl.GetUniformLocation (shader, "roughness")
        let resolutionUniform = Gl.GetUniformLocation (shader, "resolution")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          RoughnessUniform = roughnessUniform
          ResolutionUniform = resolutionUniform
          CubeMapUniform = cubeMapUniform
          EnvironmentShader = shader }

    /// Draw a environment.
    let DrawEnvironment
        (view : single array,
         projection : single array,
         roughness : single,
         resolution : single,
         cubeMap : uint,
         geometry : EnvironmentGeometry,
         shader : EnvironmentShader) =

        // setup shader
        Gl.UseProgram shader.EnvironmentShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform1 (shader.RoughnessUniform, roughness)
        Gl.Uniform1 (shader.ResolutionUniform, resolution)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.EnvironmentVao
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

    let CreateEnvironmentMap
        (currentViewportOffset : Box2i,
         currentFramebuffer : uint,
         renderbufferWidth,
         renderbufferHeight,
         renderbuffer,
         framebuffer,
         irradianceShader,
         environmentSurface) =

        // create environment map
        let environmentMap = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, environmentMap)
        Hl.Assert ()

        // setup irradiated cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba16f, renderbufferWidth, renderbufferHeight, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.LinearMipmapLinear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.GenerateMipmap TextureTarget.TextureCubeMap
        Hl.Assert ()

        // setup framebuffer
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Hl.Assert ()

        // compute views and projection
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Backward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Backward, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspectiveFieldOfView (MathHelper.PiOver2, 1.0f, 0.1f, 10.0f)).ToArray ()

        // render environment map mips
        for i in 0 .. dec Constants.Render.EnvironmentMipLevels do
            let roughness = single i / single (dec Constants.Render.EnvironmentMipLevels)
            let resolution = single Constants.Render.EnvironmentMip0Resolution * 2.0f * pown 0.5f i
            Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.DepthComponent24, int resolution, int resolution)
            Gl.Viewport (0, 0, int resolution, int resolution)
            for j in 0 .. dec 6 do
                let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + j)
                Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, environmentMap, i)
                DrawEnvironment (views.[j], projection, roughness, resolution, environmentSurface.CubeMap, environmentSurface.EnvironmentGeometry, irradianceShader)
                Hl.Assert ()

        // restore viewport
        Gl.Viewport (currentViewportOffset.Position.X, currentViewportOffset.Position.Y, currentViewportOffset.Size.X, currentViewportOffset.Size.Y)
        Hl.Assert ()

        // teardown framebuffer
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, currentFramebuffer)
        environmentMap