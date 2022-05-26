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
open System.Numerics
open System.Runtime.InteropServices
open System.Text
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

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
    let BeginFrame (viewport : Box2i) =
        OpenGL.Gl.Viewport (viewport.Position.X, viewport.Position.Y, viewport.Size.X, viewport.Size.Y)
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, 0u)
        match Constants.Render.ScreenClearing with
        | ColorClear color -> OpenGL.Gl.ClearColor (color.R, color.G, color.B, color.A)
        | NoClear -> ()
        OpenGL.Gl.Clear (OpenGL.ClearBufferMask.ColorBufferBit ||| OpenGL.ClearBufferMask.DepthBufferBit ||| OpenGL.ClearBufferMask.StencilBufferBit)

    /// End an OpenGL frame.
    let EndFrame () =
        () // nothing to do

    /// Vertically flip an SDL surface.
    /// TODO: 3D: consider flipping tex coord y instead.
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

        // load the texture into an SDL surface, converting its format if needed
        let format = SDL.SDL_PIXELFORMAT_ABGR8888 // this is RGBA8888 on little-endian architectures
        let (surfacePtr, surface) =
            let unconvertedPtr = SDL_image.IMG_Load filePath
            let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
            let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
            if unconvertedFormat.format <> format then
                let convertedPtr = SDL.SDL_ConvertSurfaceFormat (unconvertedPtr, format, 0u)
                SDL.SDL_FreeSurface unconvertedPtr
                let converted = Marshal.PtrToStructure<SDL.SDL_Surface> convertedPtr
                (convertedPtr, converted)
            else (unconvertedPtr, unconverted)

        // flip the texture
        FlipSurface &surface

        // upload the texture to gl
        let texture = OpenGL.Gl.GenTexture ()
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

    /// Attempt to create a sprite texture.
    let TryCreateSpriteTexture filePath =
        TryCreateTexture2d (OpenGL.TextureMinFilter.Nearest, OpenGL.TextureMagFilter.Nearest, filePath)

    /// Delete a texture.
    let DeleteTexture (texture : uint) =
        OpenGL.Gl.DeleteTextures texture

    /// Create a texture frame buffer.
    let CreateTextureFramebuffer () =

        // create frame buffer object
        let framebuffer = OpenGL.Gl.GenFramebuffer ()
        OpenGL.Gl.BindFramebuffer (OpenGL.FramebufferTarget.Framebuffer, framebuffer)
        Assert ()

        // create texture buffer
        let texture = OpenGL.Gl.GenTexture ()
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

    /// Create a sprite shader with attributes:
    ///     0: vec2 position
    /// and uniforms:
    ///     a: mat4 modelViewProjection
    ///     b: vec2 texCoordsIn
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
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint vertexDataSize, vertexDataPtr.AddrOfPinnedObject(), OpenGL.BufferUsage.StaticDraw)
        finally vertexDataPtr.Free ()
        Assert ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 2u; 3u; 0u|]
        let indexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
        let indexDataSize = uint (indexData.Length * sizeof<uint>)
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        try OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject(), OpenGL.BufferUsage.StaticDraw)
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
        (indexBuffer, vertexBuffer, vao)

    /// Draw a sprite whose indices and vertices were created by OpenGL.Gl.CreateSpriteQuad and whose uniforms and shader match those of OpenGL.CreateSpriteShader.
    let DrawSprite (indices, vertices, vao, modelViewProjection : single array, texCoords4 : Box2, color : Color, texture, modelViewProjectionUniform, texCoords4Uniform, colorUniform, texUniform, shader) =

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        Assert ()

        // setup shader
        OpenGL.Gl.UseProgram shader
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.UniformMatrix4 (modelViewProjectionUniform, false, modelViewProjection)
        OpenGL.Gl.Uniform4 (texCoords4Uniform, texCoords4.Position.X, texCoords4.Position.Y, texCoords4.Size.X, texCoords4.Size.Y)
        OpenGL.Gl.Uniform4 (colorUniform, color.R, color.G, color.B, color.A)
        OpenGL.Gl.Uniform1 (texUniform, 0)
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
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u
        Assert ()

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace