// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace OpenGL
open System
open System.Numerics
open Prime
open Nu

[<RequireQualifiedAccess>]
module LightMap =

    /// Create a reflection map.
    let CreateReflectionMap (render, resolution, origin, ambientColor, ambientBrightness, renderbuffer, framebuffer) =

        // setup buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, framebuffer)
        Hl.Assert ()

        // create reflection cube map
        let rasterCubeMapId = Gl.GenTexture ()
        Gl.BindTexture (TextureTarget.TextureCubeMap, rasterCubeMapId)
        Gl.FramebufferTexture (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, rasterCubeMapId, 0)
        Hl.Assert ()

        // setup reflection cube map textures
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.TexImage2D (target, 0, Hl.CheckRenderFormat InternalFormat.Rgba16f, resolution, resolution, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete then Log.error "Reflection framebuffer is incomplete!"
        Hl.Assert ()

        // construct geometry viewport
        let bounds = box2i v2iZero (v2iDup resolution)
        let geometryViewport = Viewport.make Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent bounds bounds

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

            // bind reflection cube map face for rendering
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, rasterCubeMapId, 0)
            Hl.Assert ()

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
            let bounds = box2i v2iZero (v2iDup resolution)
            render false lightAmbientOverride origin view viewSkyBox frustum projection viewProjection bounds projection framebuffer
            Hl.Assert ()

            // take a snapshot for testing
            //Hl.SaveFramebufferRgbaToBitmap (resolution, resolution, "Reflection." + string rasterCubeMapId + "." + string i + ".bmp")
            //Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        let rasterCubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = rasterCubeMapId }
        rasterCubeMap

    let CreateIrradianceMap (resolution, cubeMapSurface : CubeMap.CubeMapSurface, irradianceShader, cubeMapVao, renderbuffer, framebuffer) =

        // setup buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
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
            Gl.TexImage2D (target, 0, Hl.CheckRenderFormat InternalFormat.Rgba16f, resolution, resolution, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete then Log.error "Irradiance framebuffer is incomplete!"
        Hl.Assert ()

        // compute views and projection
        let views =
            [|Matrix4x4.CreateLookAt (v3Zero, v3Right, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Left, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Up, v3Back)
              Matrix4x4.CreateLookAt (v3Zero, v3Down, v3Forward)
              Matrix4x4.CreateLookAt (v3Zero, v3Back, v3Down)
              Matrix4x4.CreateLookAt (v3Zero, v3Forward, v3Down)|]
        let projection = Matrix4x4.CreatePerspectiveFieldOfView (MathF.PI_OVER_2, 1.0f, 0.1f, 10.0f)

        // mutate viewport
        Gl.Viewport (0, 0, resolution, resolution)
        Hl.Assert ()

        // render faces to irradiance cube map
        for i in 0 .. dec 6 do

            // render face
            let view = views.[i]
            let viewProjection = view * projection
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, 0)
            CubeMap.DrawCubeMap (view.ToArray (), projection.ToArray (), viewProjection.ToArray (), cubeMapSurface.CubeMap, cubeMapSurface.CubeMapGeometry, irradianceShader, cubeMapVao)
            Hl.Assert ()

            // take a snapshot for testing
            //Hl.SaveFramebufferRgbaToBitmap (resolution, resolution, "Irradiance." + string cubeMapId + "." + string i + ".bmp")
            //Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = cubeMapId }
        cubeMap

    /// Describes an environment filter shader that's loaded into GPU.
    type EnvironmentFilterShader =
        { ViewUniform : int
          ProjectionUniform : int
          ViewProjectionUniform : int
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
        let viewProjectionUniform = Gl.GetUniformLocation (shader, "viewProjection")
        let roughnessUniform = Gl.GetUniformLocation (shader, "roughness")
        let resolutionUniform = Gl.GetUniformLocation (shader, "resolution")
        let cubeMapUniform = Gl.GetUniformLocation (shader, "cubeMap")
        Hl.Assert ()

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          ViewProjectionUniform = viewProjectionUniform
          RoughnessUniform = roughnessUniform
          ResolutionUniform = resolutionUniform
          CubeMapUniform = cubeMapUniform
          EnvironmentFilterShader = shader }

    /// Draw an environment filter.
    let DrawEnvironmentFilter
        (view : single array,
         projection : single array,
         viewProjection : single array,
         roughness : single,
         resolution : single,
         cubeMap : Texture.Texture,
         geometry : CubeMap.CubeMapGeometry,
         shader : EnvironmentFilterShader,
         vao : uint) =

        // setup vao
        Gl.BindVertexArray vao
        Hl.Assert ()
        
        // setup shader
        Gl.UseProgram shader.EnvironmentFilterShader
        Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        Gl.UniformMatrix4 (shader.ViewProjectionUniform, false, viewProjection)
        Gl.Uniform1 (shader.RoughnessUniform, roughness)
        Gl.Uniform1 (shader.ResolutionUniform, resolution)
        Gl.Uniform1 (shader.CubeMapUniform, 0)
        Gl.ActiveTexture TextureUnit.Texture0
        Gl.BindTexture (TextureTarget.TextureCubeMap, cubeMap.TextureId)
        Hl.Assert ()

        // setup geometry
        Gl.VertexArrayVertexBuffer (vao, 0u, geometry.VertexBuffer, 0, CubeMap.VertexSize)
        Gl.VertexArrayElementBuffer (vao, geometry.IndexBuffer)
        Hl.Assert ()

        // draw geometry
        Gl.DrawElements (geometry.PrimitiveType, geometry.ElementCount, DrawElementsType.UnsignedInt, nativeint 0)
        Hl.ReportDrawCall 1
        Hl.Assert ()

        // teardown shader
        Gl.UseProgram 0u
        Hl.Assert ()

        // tear down vao
        Gl.BindVertexArray vao

    /// Create an environment filter map.
    let CreateEnvironmentFilterMap (resolution, environmentFilterSurface : CubeMap.CubeMapSurface, environmentFilterShader, cubeMapVao, renderbuffer, framebuffer) =

        // setup buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, renderbuffer)
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
            Gl.TexImage2D (target, 0, Hl.CheckRenderFormat InternalFormat.Rgba16f, resolution, resolution, 0, PixelFormat.Rgba, PixelType.HalfFloat, nativeint 0)
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
        if Gl.CheckFramebufferStatus FramebufferTarget.Framebuffer <> FramebufferStatus.FramebufferComplete then Log.error "Irradiance framebuffer is incomplete!"
        Hl.Assert ()

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
            Gl.RenderbufferStorage (RenderbufferTarget.Renderbuffer, Hl.CheckRenderFormat InternalFormat.DepthComponent16, int resolution, int resolution)
            Gl.Viewport (0, 0, int mipResolution, int mipResolution)
            for i in 0 .. dec 6 do

                // draw mip face
                let view = views.[i]
                let viewProjection = view * projection
                let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
                Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, cubeMapId, mip)
                DrawEnvironmentFilter (view.ToArray (), projection.ToArray (), viewProjection.ToArray (), mipRoughness, mipResolution, environmentFilterSurface.CubeMap, environmentFilterSurface.CubeMapGeometry, environmentFilterShader, cubeMapVao)
                Hl.Assert ()

                // take a snapshot for testing
                //Hl.SaveFramebufferRgbaToBitmap (int mipResolution, int mipResolution, "EnvironmentFilter." + string i + "." + string mip + ".bmp")
                //Hl.Assert ()

        // teardown attachments
        for i in 0 .. dec 6 do
            let target = LanguagePrimitives.EnumOfValue (int TextureTarget.TextureCubeMapPositiveX + i)
            Gl.FramebufferTexture2D (FramebufferTarget.Framebuffer, FramebufferAttachment.ColorAttachment0, target, 0u, 0)
            Hl.Assert ()

        // teardown buffers
        Gl.BindRenderbuffer (RenderbufferTarget.Renderbuffer, 0u)
        Gl.BindFramebuffer (FramebufferTarget.Framebuffer, 0u)
        let cubeMap = Texture.EagerTexture { TextureMetadata = Texture.TextureMetadata.empty; TextureId = cubeMapId }
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
    let DestroyLightMap lightMap =
        lightMap.IrradianceMap.Destroy ()
        lightMap.EnvironmentFilterMap.Destroy ()