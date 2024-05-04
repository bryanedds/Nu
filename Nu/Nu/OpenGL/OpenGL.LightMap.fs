// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace OpenGL
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module LightMap =

    /// Create a reflection map.
    let CreateReflectionMap (render, geometryResolution, ssaoResolution, rasterResolution, origin) =

        // create reflection renderbuffer
        let rasterRenderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, rasterRenderbuffer)
        Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.Depth24Stencil8, rasterResolution, rasterResolution)
        Hl.Assert ()

        // create reflection framebuffer
        let rasterFramebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, rasterFramebuffer)
        Gl.FramebufferRenderbuffer (FramebufferTarget.Framebuffer, FramebufferAttachment.DepthStencilAttachment, RenderbufferTarget.Renderbuffer, rasterRenderbuffer)
        Hl.Assert ()

        // create reflection cube map
        let rasterCubeMapId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, rasterCubeMapId)
        Gl.FramebufferTexture (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, rasterCubeMapId, 0)
        Hl.Assert ()

        // setup reflection cube map textures
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba32f, rasterResolution, rasterResolution, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, rasterCubeMapId, 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // assert reflection framebuffer completion
        Log.debugIf (fun () -> Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete) "Reflection framebuffer is incomplete!"
        Hl.Assert ()

        // construct viewports
        let geometryViewport = Viewport (Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent, box2i v2iZero geometryResolution)
        let ssaoViewport = Viewport (Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent, box2i v2iZero ssaoResolution)
        let rasterViewport = Viewport (Constants.Render.NearPlaneDistanceOmnipresent, Constants.Render.FarPlaneDistanceOmnipresent, box2i v2iZero (v2iDup rasterResolution))

        // construct eye rotations
        let eyeRotations =
            [|(v3Right, v3Down)     // (+x) right
              (v3Left, v3Down)      // (-x) left
              (v3Up, v3Back)        // (+y) top
              (v3Down, v3Forward)   // (-y) bottom
              (v3Back, v3Down)      // (+z) back
              (v3Forward, v3Down)|] // (-z) front

        // construct projections
        let geometryProjection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, geometryViewport.NearDistance, geometryViewport.FarDistance)
        let rasterProjection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, rasterViewport.AspectRatio, rasterViewport.NearDistance, rasterViewport.FarDistance)

        // render reflection cube map faces
        for i in 0 .. dec 6 do

            // bind reflection cube map face for rendering
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, rasterCubeMapId, 0)
            Hl.Assert ()

            // render to reflection cube map face
            let (eyeForward, eyeUp) = eyeRotations.[i]
            let eyeRotationMatrix = Matrix4x4.CreateLookAt (v3Zero, eyeForward, eyeUp)
            let eyeRotation = Quaternion.CreateFromRotationMatrix eyeRotationMatrix
            let viewAbsolute = m4Identity
            let viewRelative = Matrix4x4.CreateLookAt (origin, origin + eyeForward, eyeUp)
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
            render
                false origin eyeRotation
                viewAbsolute viewRelative viewSkyBox
                geometryViewport geometryProjection
                ssaoViewport
                rasterViewport rasterProjection
                rasterRenderbuffer rasterFramebuffer
            Hl.Assert ()

            //// take a snapshot for testing
            //Hl.SaveFramebufferRgbaToBitmap rasterViewport.Bounds.Width rasterViewport.Bounds.Height ("Reflection." + string rasterCubeMapId + "." + string i + ".bmp")
            //Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        Gl.DeleteRenderbuffers [|rasterRenderbuffer|]
        Gl.DeleteFramebuffers [|rasterFramebuffer|]
        let rasterCubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = rasterCubeMapId }
        rasterCubeMap

    let CreateIrradianceMap
        (resolution,
         irradianceShader,
         cubeMapSurface : CubeMap.CubeMapSurface) =

        // create irradiance renderbuffer
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent16, resolution, resolution)
        Hl.Assert ()

        // create irradiance framebuffer
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create irradiance cube map
        let cubeMapId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMapId)
        Gl.FramebufferTexture (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, cubeMapId, 0)
        Hl.Assert ()

        // setup irradiance cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba32f, resolution, resolution, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // assert irradiance framebuffer completion
        Log.debugIf (fun () -> Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete) "Irradiance framebuffer is incomplete!"
        Hl.Assert ()

        // compute views and projection
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)).ToArray ()

        // mutate viewport
        Gl.Viewport (0, 0, resolution, resolution)
        Hl.Assert ()

        // render faces to irradiance cube map
        for i in 0 .. dec 6 do

            // render face
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, 0)
            CubeMap.DrawCubeMap (views.[i], projection, cubeMapSurface.CubeMap, cubeMapSurface.CubeMapGeometry, irradianceShader)
            Hl.Assert ()

            //// take a snapshot for testing
            //Hl.SaveFramebufferRgbaToBitmap resolution resolution ("Irradiance." + string cubeMapId + "." + string i + ".bmp")
            //Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = cubeMapId }
        cubeMap

    /// Describes an environment filter shader that's loaded into GPU.
    type EnvironmentFilterShader =
        { ViewUniform : int
          ProjectionUniform : int
          RoughnessUniform : int
          ResolutionUniform : int
          CubeMapUniform : int
          EnvironmentFilterShader : uint }

    /// Create an environment filter shader.
    let CreateEnvironmentFilterShader (shaderFilePath : string) =

        // create shader
        let shader = Shader.CreateShaderFromFilePath shaderFilePath
        Hl.Assert ()

        // retrieve uniforms
        let viewUniform = Gl.GetUniformLocation (shader, "view")
        let projectionUniform = Gl.GetUniformLocation (shader, "projection")
        let roughnessUniform = Gl.GetUniformLocation (shader, "roughness")
        let resolutionUniform = Gl.GetUniformLocation (shader, "resolution")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")
        Hl.Assert ()

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          RoughnessUniform = roughnessUniform
          ResolutionUniform = resolutionUniform
          CubeMapUniform = cubeMapUniform
          EnvironmentFilterShader = shader }

    /// Draw an environment filter.
    let DrawEnvironmentFilter
        (view : single array,
         projection : single array,
         roughness : single,
         resolution : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         shader : EnvironmentFilterShader) =

        // setup shader
        Gl.UseProgram shader.EnvironmentFilterShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.Uniform1 (shader.RoughnessUniform, roughness)
        Gl.Uniform1 (shader.ResolutionUniform, resolution)
        Gl.Uniform1 (shader.CubeMapUniform, 0)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.BindVertexArray geometry.CubeMapVao
        Gl.BindBuffer (BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        Gl.BindBuffer (BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown geometry
        Gl.BindVertexArray 0u
        Hl.Assert ()

        // teardown shader
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Gl.UseProgram 0u
        Hl.Assert ()

    /// Create an environment filter map.
    let CreateEnvironmentFilterMap
        (resolution,
         environmentFilterShader,
         environmentFilterSurface : CubeMap.CubeMapSurface) =

        // create environment filter renderbuffer
        let renderbuffer = Gl.GenRenderbuffer ()
        Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.DepthComponent16, resolution, resolution)
        Hl.Assert ()

        // create environment filter framebuffer
        let framebuffer = Gl.GenFramebuffer ()
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create environment filter cube map
        let cubeMapId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMapId)
        Gl.FramebufferTexture (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, cubeMapId, 0)
        Hl.Assert ()

        // setup environment filter cube map for rendering to
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, InternalFormat.Rgba32f, resolution, resolution, 0, PixelFormat.Rgba, PixelType.Float, nativeint 0)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, 0)
            Hl.Assert ()
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMinFilter, int TextureMinFilter.LinearMipmapLinear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
        Gl.TexParameter (TextureTarget.TextureCubeMap, TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge)
        Gl.GenerateMipmap TextureTarget.TextureCubeMap
        Gl.BindTexture (TextureTarget.TextureCubeMap, 0u)
        Hl.Assert ()

        // assert environment filter framebuffer completion
        Log.debugIf (fun () -> Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete) "Irradiance framebuffer is incomplete!"
        Hl.Assert ()

        // compute views and projection
        let views =
            [|(Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)).ToArray ()
              (Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)).ToArray ()|]
        let projection = (Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)).ToArray ()

        // render environment filter cube map mips
        for mip in 0 .. dec Constants.Render.EnvironmentFilterMips do
            let mipRoughness = single mip / single (dec Constants.Render.EnvironmentFilterMips)
            let mipResolution = single resolution * pown 0.5f mip
            Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, InternalFormat.DepthComponent16, int resolution, int resolution)
            Gl.Viewport (0, 0, int mipResolution, int mipResolution)
            for i in 0 .. dec 6 do

                // draw mip face
                let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
                Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, mip)
                DrawEnvironmentFilter (views.[i], projection, mipRoughness, mipResolution, environmentFilterSurface.CubeMap, environmentFilterSurface.CubeMapGeometry, environmentFilterShader)
                Hl.Assert ()

                //// take a snapshot for testing
                //if mip = 0 then
                //    Hl.SaveFramebufferRgbaToBitmap resolution resolution ("EnvironmentFilter." + string cubeMapId + "." + string mip + "." + string i + ".bmp")
                //    Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        Gl.DeleteRenderbuffers [|renderbuffer|]
        Gl.DeleteFramebuffers [|framebuffer|]
        let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = cubeMapId }
        cubeMap

    /// A collection of maps consisting a light map.
    type [<Struct>] LightMap =
        { Enabled : bool
          Origin : Vector3
          Bounds : Box3
          IrradianceMap : Texture.Texture
          EnvironmentFilterMap : Texture.Texture }

    /// Create a light map.
    let CreateLightMap enabled origin bounds irradianceMap environmentFilterMap =
        { Enabled = enabled
          Origin = origin
          Bounds = bounds
          IrradianceMap = irradianceMap
          EnvironmentFilterMap = environmentFilterMap }

    /// Destroy a light map.
    let DestroyLightMap lightMap =
        lightMap.IrradianceMap.Destroy ()
        lightMap.EnvironmentFilterMap.Destroy ()