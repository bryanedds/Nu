// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open Prime
open Nu

////////////////////////////////////////////////
// TODO: find a better place for these types! //
////////////////////////////////////////////////

/// The flipness of a texture.
[<Syntax
    ("FlipNone FlipH FlipV FlipHV", "", "", "", "",
     Constants.PrettyPrinter.DefaultThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type [<StructuralEquality; NoComparison; Struct>] Flip =
    | FlipNone
    | FlipH
    | FlipV
    | FlipHV

/// A texture's metadata.
type TextureMetadata =
    { TextureWidth : int
      TextureHeight : int
      TextureTexelWidth : single
      TextureTexelHeight : single
      TextureInternalFormat : OpenGL.InternalFormat }

/// Force qualification of OpenGL namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Text
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// Describes some physically-based geometry that's loaded into VRAM.
    type [<StructuralEquality; NoComparison>] PhysicallyBasedGeometry =
        { VertexBuffer : uint
          IndexBuffer : uint
          PrimitiveType : OpenGL.PrimitiveType
          ElementCount : int
          PhysicallyBasedVao : uint }

    type [<CustomEquality; NoComparison; Struct>] PhysicallyBasedSurface =
        { mutable HashCode : int
          Transparent : bool
          AlbedoTexture : uint
          MetalnessTexture : uint
          RoughnessTexture : uint
          NormalTexture : uint
          AmbientOcclusionTexture : uint
          Geometry : PhysicallyBasedGeometry }

        static member inline hash surface =
            hash surface.Transparent ^^^
            hash surface.AlbedoTexture * hash surface.MetalnessTexture * hash surface.RoughnessTexture * hash surface.NormalTexture * hash surface.AmbientOcclusionTexture ^^^
            hash surface.Geometry

        static member inline make transparent albedoTexture metalnessTexture roughnessTexture normalTexture ambientOcclusionTexture geometry =
            let mutable result =
                { HashCode = 0
                  Transparent = transparent
                  AlbedoTexture = albedoTexture
                  MetalnessTexture = metalnessTexture
                  RoughnessTexture = roughnessTexture
                  NormalTexture = normalTexture
                  AmbientOcclusionTexture = ambientOcclusionTexture
                  Geometry = geometry }
            result.HashCode <- PhysicallyBasedSurface.hash result
            result

        static member inline equals left right =
            left.HashCode = right.HashCode &&
            left.Transparent = right.Transparent &&
            left.AlbedoTexture = right.AlbedoTexture &&
            left.MetalnessTexture = right.MetalnessTexture &&
            left.RoughnessTexture = right.RoughnessTexture &&
            left.NormalTexture = right.NormalTexture &&
            left.AmbientOcclusionTexture = right.AmbientOcclusionTexture &&
            left.Geometry.IndexBuffer = right.Geometry.IndexBuffer &&
            left.Geometry.VertexBuffer = right.Geometry.VertexBuffer &&
            left.Geometry.PhysicallyBasedVao = right.Geometry.PhysicallyBasedVao

        member this.Equals that =
            PhysicallyBasedSurface.equals this that

        override this.Equals (thatObj : obj) =
            match thatObj with
            | :? PhysicallyBasedSurface as that -> PhysicallyBasedSurface.equals this that
            | _ -> false

        override this.GetHashCode () =
            PhysicallyBasedSurface.hash this

    /// Type alias for an array of surfaces.
    type PhysicallyBasedModel = PhysicallyBasedSurface array

    /// Describes a physically-based shader that's loaded into GPU.
    type [<StructuralEquality; NoComparison>] PhysicallyBasedShader =
        { ViewUniform : int
          ProjectionUniform : int
          EyePositionUniform : int
          AlbedoTextureUniform : int
          MetalnessTextureUniform : int
          RoughnessTextureUniform : int
          NormalTextureUniform : int
          AmbientOcclusionTextureUniform : int
          LightPositionsUniform : int
          LightColorsUniform : int
          PhysicallyBasedShader : uint }

    /// Assert a lack of Gl error. Has an generic parameter to enable value pass-through.
    let Assert (a : 'a) =
#if DEBUG_RENDERING_ASSERT
        let error = OpenGL.Gl.GetError ()
        if error <> OpenGL.ErrorCode.NoError then
            Log.debug ("OpenGL assertion failed due to: " + string error)
#endif
        a

#if DEBUG_RENDERING_MESSAGE
    let private DebugMessageListener (_ : OpenGL.DebugSource) (_ : OpenGL.DebugType) (_ : uint) (severity : OpenGL.DebugSeverity) (length : int) (message : nativeint) (_ : nativeint) =
        let messageBytes = Array.zeroCreate<byte> length
        Marshal.Copy (message, messageBytes, 0, length)
        let messageStr = Encoding.ASCII.GetString (messageBytes, 0, length)
        match severity with
        | OpenGL.DebugSeverity.DebugSeverityNotification
        | OpenGL.DebugSeverity.DebugSeverityLow -> Log.info messageStr
        | OpenGL.DebugSeverity.DebugSeverityMedium
        | OpenGL.DebugSeverity.DebugSeverityHigh -> Log.debug messageStr
        | OpenGL.DebugSeverity.DontCare
        | _ -> ()

    let DebugMessageProc =
        OpenGL.Gl.DebugProc DebugMessageListener

    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        OpenGL.Gl.DebugMessageCallback (DebugMessageProc, nativeint 0)
#else
    /// Listen to the OpenGL error stream.
    let AttachDebugMessageCallback () =
        ()
#endif

    /// Create an SDL OpenGL context.
    let CreateSglContext window =
        OpenGL.Gl.Initialize ()
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_ACCELERATED_VISUAL, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, Constants.Render.OpenGlVersionMajor) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, Constants.Render.OpenGlVersionMinor) |> ignore<int>
        if Constants.Render.OpenGlCore then
            SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE) |> ignore<int>
#if DEBUG_RENDERING_MESSAGE
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_FLAGS, int SDL.SDL_GLcontext.SDL_GL_CONTEXT_DEBUG_FLAG) |> ignore<int>
#endif
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DOUBLEBUFFER, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_DEPTH_SIZE, 24) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_STENCIL_SIZE, 8) |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        SDL.SDL_GL_SetSwapInterval 1 |> ignore<int>
        OpenGL.Gl.BindAPI ()
        let version = OpenGL.Gl.GetString OpenGL.StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        Assert ()
        glContext

    /// Begin an OpenGL frame.
    let BeginFrame (viewportOffset : Box2i) =
    
        // set viewport
        OpenGL.Gl.Viewport (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
        Assert ()

        // bind buffer
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)
        Assert ()

        // clear inner viewport
        OpenGL.Gl.ClearColor (0.0f, 0.0f, 0.0f, 1.0f)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)
        Assert ()

        // clear drawing target
        OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
        OpenGL.Gl.Scissor (viewportOffset.Position.X, viewportOffset.Position.Y, viewportOffset.Size.X, viewportOffset.Size.Y)
        OpenGL.Gl.ClearColor (Constants.Render.WindowClearColor.R, Constants.Render.WindowClearColor.G, Constants.Render.WindowClearColor.B, Constants.Render.WindowClearColor.A)
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit)
        OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest

    /// End an OpenGL frame.
    let EndFrame () =
        () // nothing to do

    /// Vertically flip an SDL surface.
    let FlipSurface (surface : SDL.SDL_Surface inref) =
        let rowTop = Array.zeroCreate<byte> surface.pitch
        let rowBottom = Array.zeroCreate<byte> surface.pitch
        for i in 0 .. dec (surface.h / 2) do
            let offsetTop = i * surface.pitch
            let pixelsTop = surface.pixels + nativeint offsetTop
            let offsetBottom = (dec surface.h - i) * surface.pitch
            let pixelsBottom = surface.pixels + nativeint offsetBottom
            Marshal.Copy (pixelsTop, rowTop, 0, surface.pitch)
            Marshal.Copy (pixelsBottom, rowBottom, 0, surface.pitch)
            Marshal.Copy (rowTop, 0, pixelsBottom, surface.pitch)
            Marshal.Copy (rowBottom, 0, pixelsTop, surface.pitch)

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d (minFilter, magFilter, filePath : string) =

        // attempt to load the texture into an SDL surface, converting its format if needed
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let surfaceOpt =
            let unconvertedPtr = SDL_image.IMG_Load filePath
            if unconvertedPtr <> nativeint 0 then
                let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
                let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                if unconvertedFormat.format <> format then
                    let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                    SDL.SDL_FreeSurface unconvertedPtr
                    let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                    Some (convertedPtr, converted)
                else Some (unconvertedPtr, unconverted)
            else None

        // ensure surface was loaded into
        match surfaceOpt with
        | Some (surfacePtr, surface) ->

            // upload the texture to gl
            let texture = OpenGL.Gl.GenTexture ()
            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int minFilter)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int magFilter)
            let internalFormat = OpenGL.InternalFormat.Rgba8
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, internalFormat, surface.w, surface.h, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, surface.pixels)

            // teardown surface
            SDL.SDL_FreeSurface surfacePtr

            // check for errors
            match OpenGL.Gl.GetError () with
            | OpenGL.ErrorCode.NoError ->
                let metadata =
                    { TextureWidth = surface.w
                      TextureHeight = surface.h
                      TextureTexelWidth = 1.0f / single surface.w
                      TextureTexelHeight = 1.0f / single surface.h
                      TextureInternalFormat = internalFormat }
                Right (metadata, texture)
            | error -> Left (string error)

        // load error
        | None -> Left ("Missing file or unloadable image '" + filePath + "'.")

    /// Attempt to create a sprite texture.
    let TryCreateSpriteTexture filePath =
        TryCreateTexture2d (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, filePath)

    /// Delete a texture.
    let DeleteTexture (texture : uint) =
        OpenGL.Gl.DeleteTextures texture

    /// Attempt to create physically-based geometry from an assimp mesh.
    let TryCreatePhysicallyBasedGeometry (mesh : Assimp.Mesh) =

        // ensure required data is available
        if  mesh.HasVertices &&
            mesh.HasNormals &&
            mesh.HasTextureCoords 0 then

            // populate vertex data
            let vertexData = Array.zeroCreate<single> (mesh.Vertices.Count * 8)
            for i in 0 .. dec mesh.Vertices.Count do
                let v = i * 8
                let position = mesh.Vertices.[i]
                let normal = mesh.Normals.[i]
                let texCoords = mesh.TextureCoordinateChannels.[0].[i]
                vertexData.[v] <- position.X
                vertexData.[v+1] <- position.Y
                vertexData.[v+2] <- position.Z
                vertexData.[v+3] <- normal.X
                vertexData.[v+4] <- normal.Y
                vertexData.[v+5] <- normal.Z
                vertexData.[v+6] <- texCoords.X
                vertexData.[v+7] <- texCoords.Z

            // populate index data
            let indexData = mesh.GetIndices ()

            // initialize vao
            let vao = OpenGL.Gl.GenVertexArray ()
            OpenGL.Gl.BindVertexArray vao
            Assert ()

            // create vertex buffer
            let vertexBuffer = OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
            let vertexBufferPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
            try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexBufferPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StreamDraw)
            finally vertexBufferPtr.Free ()
            Assert ()

            // create index buffer
            let indexBuffer = OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
            let indexDataSize = uint (indexData.Length * sizeof<uint>)
            let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
            try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
            finally indexDataPtr.Free ()
            Assert ()

            // finalize vao
            let normalOffset =      (3 (*position*)) * sizeof<single>
            let texCoordsOffset =   (3 (*position*) + 3 (*normal*)) * sizeof<single>
            let vertexSize =        (3 (*position*) + 3 (*normal*) + 2 (*texCoords*)) * sizeof<single>
            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.VertexAttribPointer (0u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.VertexAttribPointer (1u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint normalOffset)
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (2u, 2, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint texCoordsOffset)
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
            OpenGL.Gl.BindVertexArray 0u
            Assert ()

            // make physically based geometry
            let geometry =
                { VertexBuffer = vertexBuffer
                  IndexBuffer = indexBuffer
                  PrimitiveType = OpenGL.PrimitiveType.Triangles
                  ElementCount = 36
                  PhysicallyBasedVao = vao }

            // fin
            Right geometry

        // error
        else Left "Mesh is missing vertices, normals, or texCoords."

    /// Attempt to create physically-based material from an assimp mesh.
    let TryCreatePhysicallyBasedMaterial (dirPath : string, material : Assimp.Material) =
        if  material.HasTextureDiffuse && // mapped to albedo
            material.HasTextureSpecular && // mapped to metalness
            material.HasTextureHeight && // mapped to roughness
            material.HasTextureNormal && // mapped to normal
            material.HasTextureAmbient then // mapped to ambient occlusion
            let (_, albedo) = material.GetMaterialTexture (Assimp.TextureType.Diffuse, 0)
            let (_, metalness) = material.GetMaterialTexture (Assimp.TextureType.Specular, 0)
            let (_, roughness) = material.GetMaterialTexture (Assimp.TextureType.Height, 0)
            let (_, normal) = material.GetMaterialTexture (Assimp.TextureType.Normals, 0)
            let (_, ambientOcclusion) = material.GetMaterialTexture (Assimp.TextureType.Ambient, 0)
            let albedoFilePath = Path.Combine (dirPath, albedo.FilePath)
            let metalnessFilePath = Path.Combine (dirPath, metalness.FilePath)
            let roughnessFilePath = Path.Combine (dirPath, roughness.FilePath)
            let normalFilePath = Path.Combine (dirPath, normal.FilePath)
            let ambientOcclusionFilePath = Path.Combine (dirPath, ambientOcclusion.FilePath)
            match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, albedoFilePath) with
            | Right albedoTexture ->
                match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, metalnessFilePath) with
                | Right metalnessTexture ->
                    match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, roughnessFilePath) with
                    | Right roughnessTexture ->
                        match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, normalFilePath) with
                        | Right normalTexture ->
                            match TryCreateTexture2d (OpenGL.TextureMinFilter.Linear, OpenGL.TextureMagFilter.Linear, ambientOcclusionFilePath) with
                            | Right ambientOcclusionTexture -> Right (albedoTexture, metalnessTexture, roughnessTexture, normalTexture, ambientOcclusionTexture)
                            | Left error -> Left ("Could not load texture '" + ambientOcclusionFilePath + "' due to '" + error + "'.")
                        | Left error -> Left ("Could not load texture '" + normalFilePath + "' due to '" + error + "'.")
                    | Left error -> Left ("Could not load texture '" + roughnessFilePath + "' due to '" + error + "'.")
                | Left error -> Left ("Could not load texture '" + metalnessFilePath + "' due to '" + error + "'.")
            | Left error -> Left ("Could not load texture '" + albedoFilePath + "' due to '" + error + "'.")
        else Left ("Could not create physically-based material due to missing diffuse/albedo, metalness, roughness, normal, or ambientOcclusion texture.")

    /// Attempt to create physically-based material from an assimp scene.
    let TryCreatePhysicallyBasedMaterials (dirPath : string, scene : Assimp.Scene) =
        let mutable errorOpt = None
        let materials = dictPlus<int, _> HashIdentity.Structural []
        for i in 0 .. dec scene.Materials.Count do
            if Option.isNone errorOpt then
                match TryCreatePhysicallyBasedMaterial (dirPath, scene.Materials.[i]) with
                | Right material -> materials.Add (i, material)
                | Left error -> errorOpt <- Some error
        match errorOpt with
        | Some error -> Left error
        | None -> Right materials

    let TryCreatePhysicallyBasedModel (filePath, assimp : Assimp.AssimpContext) : Either<string, PhysicallyBasedModel> =
        try let scene = assimp.ImportFile (filePath, Assimp.PostProcessSteps.CalculateTangentSpace ||| Assimp.PostProcessSteps.JoinIdenticalVertices ||| Assimp.PostProcessSteps.Triangulate ||| Assimp.PostProcessSteps.GenerateSmoothNormals ||| Assimp.PostProcessSteps.SplitLargeMeshes ||| Assimp.PostProcessSteps.LimitBoneWeights ||| Assimp.PostProcessSteps.RemoveRedundantMaterials ||| Assimp.PostProcessSteps.SortByPrimitiveType ||| Assimp.PostProcessSteps.FindDegenerates ||| Assimp.PostProcessSteps.FindInvalidData ||| Assimp.PostProcessSteps.GenerateUVCoords ||| Assimp.PostProcessSteps.FlipWindingOrder)
            let dirPath = Path.GetDirectoryName filePath
            match TryCreatePhysicallyBasedMaterials (dirPath, scene) with
            | Right materials ->
                let mutable errorOpt = None
                let model = SegmentedList.make ()
                for mesh in scene.Meshes do
                    if Option.isNone errorOpt then
                        match TryCreatePhysicallyBasedGeometry mesh with
                        | Right geometry ->
                            match materials.TryGetValue mesh.MaterialIndex with
                            | (true, (albedoTexture, metalnessTexture, roughnessTexture, normalTexture, ambientOcclusionTexture)) ->
                                let surface =
                                    PhysicallyBasedSurface.make
                                        false
                                        (snd albedoTexture)
                                        (snd metalnessTexture)
                                        (snd roughnessTexture)
                                        (snd normalTexture)
                                        (snd ambientOcclusionTexture)
                                        geometry
                                SegmentedList.add surface model
                            | (false, _) -> errorOpt <- Some ("Could not locate associated materials for mesh in file name '" + filePath + "'.")
                        | Left error -> errorOpt <- Some ("Could not load geometry for mesh in file name '" + filePath + "' due to: " + error)
                match errorOpt with
                | None -> Right (Array.ofSeq model)
                | Some error -> Left error
            | Left error -> Left ("Could not load materials for model in file name '" + filePath + "' due to: " + error)
        with exn -> Left ("Could not load model '" + filePath + "' due to: " + scstring exn)

    /// Create a texture frame buffer.
    let CreateTextureFramebuffer () =

        // create frame buffer object
        let framebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        Assert ()

        // create texture buffer
        let texture = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, OpenGL.InternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.Float, nativeint 0)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
        OpenGL.Gl.FramebufferTexture (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.ColorAttachment0, texture, 0)
        Assert ()

        // create depth and stencil buffers
        let depthBuffer = OpenGL.Gl.GenRenderbuffer ()
        OpenGL.Gl.BindRenderbuffer (OpenGL.RenderbufferTarget.Renderbuffer, depthBuffer)
        OpenGL.Gl.RenderbufferStorage (OpenGL.RenderbufferTarget.Renderbuffer, OpenGL.InternalFormat.Depth32fStencil8, Constants.Render.ResolutionX, Constants.Render.ResolutionY)
        OpenGL.Gl.FramebufferRenderbuffer (OpenGL.FramebufferTarget.Framebuffer, OpenGL.FramebufferAttachment.DepthStencilAttachment, OpenGL.RenderbufferTarget.Renderbuffer, depthBuffer)
        Assert ()

        // fin
        (texture, framebuffer)

    /// Create a shader from vertex and fragment code strings.
    let CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr) =

        // construct gl shader
        let shader = OpenGL.Gl.CreateProgram ()
        Assert ()

        // add vertex shader
        let vertexShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.VertexShader
        OpenGL.Gl.ShaderSource (vertexShader, [|vertexShaderStr|], null)
        OpenGL.Gl.CompileShader vertexShader
        OpenGL.Gl.AttachShader (shader, vertexShader)
        Assert ()

        // add fragement shader to program
        let fragmentShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.FragmentShader
        OpenGL.Gl.ShaderSource (fragmentShader, [|fragmentShaderStr|], null)
        OpenGL.Gl.CompileShader fragmentShader
        OpenGL.Gl.AttachShader (shader, fragmentShader)
        Assert ()

        // link shader
        OpenGL.Gl.LinkProgram shader
        shader

    /// Create a shader from a vertex stream and a fragment stream.
    let CreateShaderFromStreams (vertexStream : StreamReader, fragmentStream : StreamReader) =
        let vertexStr = vertexStream.ReadToEnd ()
        let fragmentStr = fragmentStream.ReadToEnd ()
        CreateShaderFromStrs (vertexStr, fragmentStr)

    /// Create a shader from a vertex file and a fragment file.
    let CreateShaderFromFilePaths (vertexFilePath : string, fragmentFilePath : string) =
        use vertexStream = new StreamReader (File.OpenRead vertexFilePath)
        use fragmentStream = new StreamReader (File.OpenRead fragmentFilePath)
        CreateShaderFromStreams (vertexStream, fragmentStream)

    /// Create a shader from a single file with both a '#shader vertex' section and a '#shader fragment' section.
    let CreateShaderFromFilePath (shaderFilePath : string) =
        use shaderStream = new StreamReader (File.OpenRead shaderFilePath)
        let shaderStr = shaderStream.ReadToEnd ()
        let vertexStrIndex = shaderStr.IndexOf "#shader vertex"
        let fragmentStrIndex = shaderStr.IndexOf "#shader fragment"
        if vertexStrIndex > -1 && fragmentStrIndex > -1 then
            let (vertexStr, fragmentStr) =
                if vertexStrIndex < fragmentStrIndex then
                    (shaderStr.Substring (vertexStrIndex, fragmentStrIndex - vertexStrIndex),
                     shaderStr.Substring (fragmentStrIndex, shaderStr.Length - fragmentStrIndex))
                else
                    (shaderStr.Substring (fragmentStrIndex, vertexStrIndex - fragmentStrIndex),
                     shaderStr.Substring (vertexStrIndex, shaderStr.Length - vertexStrIndex))
            CreateShaderFromStrs (vertexStr.Replace ("#shader vertex", ""), fragmentStr.Replace ("#shader fragment", ""))
        else failwith ("Invalid shader file '" + shaderFilePath + "'. Both vertex and fragment shader sections required.")

    /// Create a sprite shader with attributes:
    ///     0: vec2 position
    /// and uniforms:
    ///     a: mat4 modelViewProjection
    ///     b: vec2 texCoords4
    ///     c: vec4 color
    ///     d: sampler2D tex
    let CreateSpriteShader () =

        // vertex shader code
        let vertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "#define VERTS 4"
             ""
             "const vec4 filters[VERTS] ="
             "  vec4[4]("
             "      vec4(1,1,0,0),"
             "      vec4(1,1,1,0),"
             "      vec4(1,1,1,1),"
             "      vec4(1,1,0,1));"
             ""
             "in vec2 position;"
             "uniform mat4 modelViewProjection;"
             "uniform vec4 texCoords4;"
             "out vec2 texCoords;"
             "void main()"
             "{"
             "  int vertexId = gl_VertexID % VERTS;"
             "  vec4 filt = filters[vertexId];"
             "  gl_Position = modelViewProjection * vec4(position.x, position.y, 0, 1);"
             "  texCoords = vec2(texCoords4.x * filt.x + texCoords4.z * filt.z, texCoords4.y * filt.y + texCoords4.w * filt.w);"
             "}"] |> String.join "\n"

        // fragment shader code
        let fragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "uniform vec4 color;"
             "in vec2 texCoords;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = color * texture(tex, texCoords);"
             "}"] |> String.join "\n"

        // create shader
        let shader = CreateShaderFromStrs (vertexShaderStr, fragmentShaderStr)
        let modelViewProjectionUniform = OpenGL.Gl.GetUniformLocation (shader, "modelViewProjection")
        let texCoords4Uniform = OpenGL.Gl.GetUniformLocation (shader, "texCoords4")
        let colorUniform = OpenGL.Gl.GetUniformLocation (shader, "color")
        let texUniform = OpenGL.Gl.GetUniformLocation (shader, "tex")
        (modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader)

    let CreatePhysicallyBasedShader (shaderFilePath : string) =

        // create shader
        let shader = CreateShaderFromFilePath shaderFilePath

        // retrieve uniforms
        let viewUniform = OpenGL.Gl.GetUniformLocation (shader, "view")
        let projectionUniform = OpenGL.Gl.GetUniformLocation (shader, "projection")
        let eyePositionUniform = OpenGL.Gl.GetUniformLocation (shader, "eyePosition")
        let albedoTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "albedoTexture")
        let metalnessTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "metalnessTexture")
        let roughnessTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "roughnessTexture")
        let normalTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "normalTexture")
        let ambientOcclusionTextureUniform = OpenGL.Gl.GetUniformLocation (shader, "ambientOcclusionTexture")
        let lightPositionsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightPositions")
        let lightColorsUniform = OpenGL.Gl.GetUniformLocation (shader, "lightColors")

        // make shader record
        { ViewUniform = viewUniform
          ProjectionUniform = projectionUniform
          EyePositionUniform = eyePositionUniform
          AlbedoTextureUniform = albedoTextureUniform
          MetalnessTextureUniform = metalnessTextureUniform
          RoughnessTextureUniform = roughnessTextureUniform
          NormalTextureUniform = normalTextureUniform
          AmbientOcclusionTextureUniform = ambientOcclusionTextureUniform
          LightPositionsUniform = lightPositionsUniform
          LightColorsUniform = lightColorsUniform
          PhysicallyBasedShader = shader }

    /// Create a sprite quad for rendering to a shader matching the one created with OpenGL.Hl.CreateSpriteShader.
    let CreateSpriteQuad onlyUpperRightQuadrant =

        // build vertex data
        let vertexData =
            if onlyUpperRightQuadrant then
                [|+0.0f; +0.0f
                  +1.0f; +0.0f
                  +1.0f; +1.0f
                  +0.0f; +1.0f|]
            else
                [|-1.0f; -1.0f
                  +1.0f; -1.0f
                  +1.0f; +1.0f
                  -1.0f; +1.0f|]

        // initialize vao
        let vao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Gl.BindVertexArray vao
        Assert ()

        // create vertex buffer
        let vertexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
        let vertexSize = sizeof<single> * 2
        let vertexDataSize = vertexSize * 4
        let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint vertexDataSize, vertexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
        finally vertexDataPtr.Free ()
        Assert ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        let indexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
        let indexDataSize = uint (indexData.Length * sizeof<uint>)
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
        finally indexDataPtr.Free ()
        Assert ()

        // finalize vao
        OpenGL.Gl.EnableVertexAttribArray 0u
        OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // fin
        (vertexBuffer, indexBuffer, vao)

    /// Create a cube for rendering to a shader matching the one created with OpenGL.Hl.CreatePhysicallyBasedShader.
    let CreatePhysicallyBasedCube () =

        // make vertex data
        let vertexData =
            [|
              (*   positions   *)       (*    normals    *)         (* tex coords *)

              // back face
              -0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 0.0f; // bottom-left
              +0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 1.0f; // top-right
              +0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 0.0f; // bottom-right         
              +0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        1.0f; 1.0f; // top-right
              -0.5f; -0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 0.0f; // bottom-left
              -0.5f; +0.5f; -0.5f;       0.0f;  0.0f; -1.0f;        0.0f; 1.0f; // top-left

              // front face
              -0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left
              +0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 0.0f; // bottom-right
              +0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
              +0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        1.0f; 1.0f; // top-right
              -0.5f; +0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 1.0f; // top-left
              -0.5f; -0.5f; +0.5f;       0.0f;  0.0f; +1.0f;        0.0f; 0.0f; // bottom-left

              // left face
              -0.5f; +0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-right
              -0.5f; +0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 1.0f; // top-left
              -0.5f; -0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-left
              -0.5f; -0.5f; -0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-left
              -0.5f; -0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        0.0f; 0.0f; // bottom-right
              -0.5f; +0.5f; +0.5f;      -1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-right

              // right face
              +0.5f; +0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-left
              +0.5f; -0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-right
              +0.5f; +0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 1.0f; // top-right         
              +0.5f; -0.5f; -0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 1.0f; // bottom-right
              +0.5f; +0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        1.0f; 0.0f; // top-left
              +0.5f; -0.5f; +0.5f;      +1.0f;  0.0f;  0.0f;        0.0f; 0.0f; // bottom-left     

              // bottom face
              -0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 1.0f; // top-right
              +0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 1.0f; // top-left
              +0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 0.0f; // bottom-left
              +0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        1.0f; 0.0f; // bottom-left
              -0.5f; -0.5f; +0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 0.0f; // bottom-right
              -0.5f; -0.5f; -0.5f;       0.0f; -1.0f;  0.0f;        0.0f; 1.0f; // top-right

              // top face
              -0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 1.0f; // top-left
              +0.5f; +0.5f ;+0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 0.0f; // bottom-right
              +0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 1.0f; // top-right     
              +0.5f; +0.5f; +0.5f;       0.0f; +1.0f;  0.0f;        1.0f; 0.0f; // bottom-right
              -0.5f; +0.5f; -0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 1.0f; // top-left
              -0.5f; +0.5f; +0.5f;       0.0f; +1.0f;  0.0f;        0.0f; 0.0f  // bottom-left     
            |]

        // make index data trivially
        let indexData = Array.init 36 id

        // initialize vao
        let vao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Gl.BindVertexArray vao
        Assert ()

        // create vertex buffer
        let vertexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
        let vertexBufferPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (vertexData.Length * sizeof<single>), vertexBufferPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StreamDraw)
        finally vertexBufferPtr.Free ()
        Assert ()

        // create index buffer
        let indexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
        let indexDataSize = uint (indexData.Length * sizeof<uint>)
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject (), OpenGL.BufferUsage.StaticDraw)
        finally indexDataPtr.Free ()
        Assert ()

        // finalize vao
        let normalOffset =      (3 (*position*)) * sizeof<single>
        let texCoordsOffset =   (3 (*position*) + 3 (*normal*)) * sizeof<single>
        let vertexSize =        (3 (*position*) + 3 (*normal*) + 2 (*texCoords*)) * sizeof<single>
        OpenGL.Gl.EnableVertexAttribArray 0u
        OpenGL.Gl.VertexAttribPointer (0u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint 0)
        OpenGL.Gl.EnableVertexAttribArray 1u
        OpenGL.Gl.VertexAttribPointer (1u, 3, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint normalOffset)
        OpenGL.Gl.EnableVertexAttribArray 2u
        OpenGL.Gl.VertexAttribPointer (2u, 2, OpenGL.VertexAttribType.Float, false, vertexSize, nativeint texCoordsOffset)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // make physically based geometry
        { VertexBuffer = vertexBuffer
          IndexBuffer = indexBuffer
          PrimitiveType = OpenGL.PrimitiveType.Triangles
          ElementCount = 36
          PhysicallyBasedVao = vao }

    /// Draw a sprite whose indices and vertices were created by OpenGL.Gl.CreateSpriteQuad and whose uniforms and shader match those of OpenGL.CreateSpriteShader.
    let DrawSprite (vertices, indices, vao, modelViewProjection : single array, insetOpt : Box2 ValueOption, color : Color, flip, textureWidth, textureHeight, texture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader) =

        // compute unflipped tex coords
        let texCoordsUnflipped =
            match insetOpt with
            | ValueSome inset ->
                let texelWidth = 1.0f / single textureWidth
                let texelHeight = 1.0f / single textureHeight
                let borderWidth = texelWidth * Constants.Render.SpriteBorderTexelScalar
                let borderHeight = texelHeight * Constants.Render.SpriteBorderTexelScalar
                let px = inset.Position.X * texelWidth + borderWidth
                let py = (inset.Position.Y + inset.Size.Y) * texelHeight - borderHeight
                let sx = inset.Size.X * texelWidth - borderWidth * 2.0f
                let sy = -inset.Size.Y * texelHeight + borderHeight * 2.0f
                Box2 (px, py, sx, sy)
            | ValueNone -> Box2 (0.0f, 1.0f, 1.0f, -1.0f)
            
        // compute a flipping flags
        let struct (flipH, flipV) =
            match flip with
            | FlipNone -> struct (false, false)
            | FlipH -> struct (true, false)
            | FlipV -> struct (false, true)
            | FlipHV -> struct (true, true)

        // compute tex coords
        let texCoords =
            box2
                (v2
                    (if flipH then texCoordsUnflipped.Position.X + texCoordsUnflipped.Size.X else texCoordsUnflipped.Position.X)
                    (if flipV then texCoordsUnflipped.Position.Y + texCoordsUnflipped.Size.Y else texCoordsUnflipped.Position.Y))
                (v2
                    (if flipH then -texCoordsUnflipped.Size.X else texCoordsUnflipped.Size.X)
                    (if flipV then -texCoordsUnflipped.Size.Y else texCoordsUnflipped.Size.Y))

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Gl.CullFace OpenGL.CullFaceMode.Back
        Assert ()

        // setup shader
        OpenGL.Gl.UseProgram shader
        OpenGL.Gl.UniformMatrix4 (modelViewProjectionUniform, false, modelViewProjection)
        OpenGL.Gl.Uniform4 (texCoords4Uniform, texCoords.Position.X, texCoords.Position.Y, texCoords.Size.X, texCoords.Size.Y)
        OpenGL.Gl.Uniform4 (colorUniform, color.R, color.G, color.B, color.A)
        OpenGL.Gl.Uniform1 (texUniform, 0)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
        OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
        Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray vao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertices)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indices)
        Assert ()

        // draw geometry
        OpenGL.Gl.DrawElements (OpenGL.PrimitiveType.Triangles, 6, OpenGL.DrawElementsType.UnsignedInt, nativeint 0)
        Assert ()

        // teardown geometry
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // teardown shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        Assert ()

        // teardown state
        OpenGL.Gl.CullFace OpenGL.CullFaceMode.Back
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend

    /// Draw a physically-based surface.
    let DrawSurfaces
        (eyePosition : Vector3,
         modelsFields : single array,
         modelsCount : int,
         view : single array,
         projection : single array,
         albedoTexture : uint,
         metalnessTexture : uint,
         roughnessTexture : uint,
         normalTexture : uint,
         ambientOcclusionTexture : uint,
         lightPositions : single array,
         lightColors : single array,
         modelRow0Buffer : uint,
         modelRow1Buffer : uint,
         modelRow2Buffer : uint,
         modelRow3Buffer : uint,
         shader : PhysicallyBasedShader,
         geometry : PhysicallyBasedGeometry) =

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Gl.CullFace OpenGL.CullFaceMode.Back
        Assert ()

        // setup shader
        OpenGL.Gl.UseProgram shader.PhysicallyBasedShader
        OpenGL.Gl.UniformMatrix4 (shader.ViewUniform, false, view)
        OpenGL.Gl.UniformMatrix4 (shader.ProjectionUniform, false, projection)
        OpenGL.Gl.Uniform3 (shader.EyePositionUniform, eyePosition.X, eyePosition.Y, eyePosition.Z)
        OpenGL.Gl.Uniform1 (shader.AlbedoTextureUniform, 0)
        OpenGL.Gl.Uniform1 (shader.MetalnessTextureUniform, 1)
        OpenGL.Gl.Uniform1 (shader.RoughnessTextureUniform, 2)
        OpenGL.Gl.Uniform1 (shader.NormalTextureUniform, 3)
        OpenGL.Gl.Uniform1 (shader.AmbientOcclusionTextureUniform, 4)
        OpenGL.Gl.Uniform3 (shader.LightPositionsUniform, lightPositions)
        OpenGL.Gl.Uniform3 (shader.LightColorsUniform, lightColors)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, albedoTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, metalnessTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, roughnessTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, normalTexture)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture4
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, ambientOcclusionTexture)
        OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
        OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
        Assert ()

        // setup geometry
        OpenGL.Gl.BindVertexArray geometry.PhysicallyBasedVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, geometry.VertexBuffer)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, geometry.IndexBuffer)
        Assert ()

        // setup instancing
        OpenGL.Gl.EnableVertexArrayAttrib (geometry.PhysicallyBasedVao, 3u)
        OpenGL.Gl.EnableVertexArrayAttrib (geometry.PhysicallyBasedVao, 4u)
        OpenGL.Gl.EnableVertexArrayAttrib (geometry.PhysicallyBasedVao, 5u)
        OpenGL.Gl.EnableVertexArrayAttrib (geometry.PhysicallyBasedVao, 6u)
        let modelsRow0 = Array.zeroCreate<single> (modelsFields.Length / 4)
        let modelsRow1 = Array.zeroCreate<single> (modelsFields.Length / 4)
        let modelsRow2 = Array.zeroCreate<single> (modelsFields.Length / 4)
        let modelsRow3 = Array.zeroCreate<single> (modelsFields.Length / 4)
        let mutable i = 0
        while i < dec modelsFields.Length do
            let iOver4 = i / 4
            modelsRow0.[iOver4] <- modelsFields.[i]
            modelsRow1.[iOver4] <- modelsFields.[i+1]
            modelsRow2.[iOver4] <- modelsFields.[i+2]
            modelsRow3.[iOver4] <- modelsFields.[i+3]
            i <- i + 4
        let modelsRow0Ptr = GCHandle.Alloc (modelsRow0, GCHandleType.Pinned)
        let modelsRow1Ptr = GCHandle.Alloc (modelsRow1, GCHandleType.Pinned)
        let modelsRow2Ptr = GCHandle.Alloc (modelsRow2, GCHandleType.Pinned)
        let modelsRow3Ptr = GCHandle.Alloc (modelsRow3, GCHandleType.Pinned)
        try
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, modelRow0Buffer) // populate modelRow0Buffer
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsRow0.Length * sizeof<single>), modelsRow0Ptr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.VertexAttribPointer (3u, 4, OpenGL.VertexAttribType.Float, false, 0, nativeint 0)
            OpenGL.Gl.VertexAttribDivisor (3u, 1u)
            Assert ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, modelRow1Buffer) // populate modelRow1Buffer
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsRow1.Length * sizeof<single>), modelsRow1Ptr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.VertexAttribPointer (4u, 4, OpenGL.VertexAttribType.Float, false, 0, nativeint 0)
            OpenGL.Gl.VertexAttribDivisor (4u, 1u)
            Assert ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, modelRow2Buffer) // populate modelRow2Buffer
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsRow2.Length * sizeof<single>), modelsRow2Ptr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.VertexAttribPointer (5u, 4, OpenGL.VertexAttribType.Float, false, 0, nativeint 0)
            OpenGL.Gl.VertexAttribDivisor (5u, 1u)
            Assert ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, modelRow3Buffer) // populate modelRow3Buffer
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint (modelsRow3.Length * sizeof<single>), modelsRow3Ptr.AddrOfPinnedObject (), OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.VertexAttribPointer (6u, 4, OpenGL.VertexAttribType.Float, false, 0, nativeint 0)
            OpenGL.Gl.VertexAttribDivisor (6u, 1u)
            Assert ()
        finally
            modelsRow0Ptr.Free ()
            modelsRow1Ptr.Free ()
            modelsRow2Ptr.Free ()
            modelsRow3Ptr.Free ()

        // draw geometry
        OpenGL.Gl.DrawElementsInstanced (geometry.PrimitiveType, geometry.ElementCount, OpenGL.DrawElementsType.UnsignedInt, nativeint 0, modelsCount)
        Assert ()

        // teardown geometry
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, 0u)
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Gl.BindVertexArray 0u
        Assert ()

        // teardown shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture1
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture2
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture3
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture4
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        Assert ()

        // teardown state
        OpenGL.Gl.CullFace OpenGL.CullFaceMode.Back
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend
