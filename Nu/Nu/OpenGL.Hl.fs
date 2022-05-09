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
      TextureInternalFormat : OpenGL.InternalFormat }

/// Force qualification of OpenGL namespace unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System.Numerics
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    type [<StructuralEquality; NoComparison; StructLayout (LayoutKind.Sequential)>] SpriteBatchVertex =
        struct
            val mutable SbvPosition : Vector2
            val mutable SbvCoord : Vector2
            val mutable SbvColor : Color
            end

    type [<StructuralEquality; NoComparison>] SpriteBatchState =
        private
            { BlendingFactorSrc : OpenGL.BlendingFactor
              BlendingFactorDest : OpenGL.BlendingFactor
              Texture : uint }
    
    type [<NoEquality; NoComparison>] SpriteBatchEnv =
        private
            { SpriteIndex : int
              SpriteTexUniform : int
              SpriteProgram : uint
              CpuBuffer : nativeint
              GpuBuffer : uint
              GpuVao : uint
              BatchState : SpriteBatchState }

    /// Assert a lack of Gl error.
    let Assert () =
        let error = OpenGL.Gl.GetError ()
        if error <> OpenGL.ErrorCode.NoError then
            Log.debug ("Gl assertion failed due to: " + string error)

    /// Create an SDL OpenGL context.
    let CreateSgl410Context window =
        OpenGL.Gl.Initialize ()
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MAJOR_VERSION, 4) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_MINOR_VERSION, 1) |> ignore<int>
        SDL.SDL_GL_SetAttribute (SDL.SDL_GLattr.SDL_GL_CONTEXT_PROFILE_MASK, SDL.SDL_GLprofile.SDL_GL_CONTEXT_PROFILE_CORE) |> ignore<int>
        SDL.SDL_GL_SetSwapInterval 1 |> ignore<int>
        let glContext = SDL.SDL_GL_CreateContext window
        OpenGL.Gl.BindAPI ()
        let version = OpenGL.Gl.GetString OpenGL.StringName.Version
        Log.info ("Initialized OpenGL " + version + ".")
        Assert ()
        glContext

    /// Attempt to create a 2d texture from a file.
    let TryCreateTexture2d minFilter magFilter (filePath : string) =

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

        // vertically flip the rows of the texture
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

        // upload the texture to gl
        let texture = OpenGL.Gl.GenTexture ()
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int minFilter)
        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int magFilter)
        let internalFormat = OpenGL.InternalFormat.Rgba8
        OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, internalFormat, surface.w, surface.h, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, surface.pixels)

        // teardown surface
        SDL.SDL_FreeSurface surfacePtr

        // check for errors
        match OpenGL.Gl.GetError () with
        | OpenGL.ErrorCode.NoError ->
            let metadata = { TextureWidth = surface.w; TextureHeight = surface.h; TextureInternalFormat = internalFormat }
            Right (metadata, texture)
        | error -> Left (string error)

    /// Attempt to create a sprite texture.
    let TryCreateSpriteTexture filePath =
        TryCreateTexture2d
            OpenGL.TextureMinFilter.Nearest
            OpenGL.TextureMagFilter.Nearest
            filePath

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
        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
        OpenGL.Gl.TexParameteri (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
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

    /// Create a sprite quad.
    let CreateSpriteQuad () =

        // build vertex data
        let vertexData =
            [|-1.0f; -1.0f; 0.0f; 0.0f; 1.0f; 1.0f; 1.0f; 1.0f
              +1.0f; -1.0f; 1.0f; 0.0f; 1.0f; 1.0f; 1.0f; 1.0f
              +1.0f; +1.0f; 1.0f; 1.0f; 1.0f; 1.0f; 1.0f; 1.0f
              -1.0f; +1.0f; 0.0f; 1.0f; 1.0f; 1.0f; 1.0f; 1.0f|]

        // create vertex buffer
        let vertexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
        let vertexDataSize = 8u * 4u * uint sizeof<single>
        let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
        OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, vertexDataSize, vertexDataPtr.AddrOfPinnedObject(), OpenGL.BufferUsage.StaticDraw)
        Assert ()

        // create index buffer
        let indexData = [|0u; 1u; 2u; 3u|]
        let indexBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, indexBuffer)
        let indexDataSize = 4u * uint sizeof<uint>
        let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
        OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject(), OpenGL.BufferUsage.StaticDraw)
        Assert ()

        // fin
        (vertexBuffer, indexBuffer)

    /// Create a shader program from vertex and fragment code strings.
    let CreateShaderProgramFromStrs vertexShaderStr fragmentShaderStr =

        // construct gl program
        let program = OpenGL.Gl.CreateProgram ()
        Assert ()

        // add vertex shader to program
        let vertexShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.VertexShader
        OpenGL.Gl.ShaderSource (vertexShader, [|vertexShaderStr|], null)
        OpenGL.Gl.CompileShader vertexShader
        OpenGL.Gl.AttachShader (program, vertexShader)
        Assert ()

        // add fragement shader to program
        let fragmentShader = OpenGL.Gl.CreateShader OpenGL.ShaderType.FragmentShader
        OpenGL.Gl.ShaderSource (fragmentShader, [|fragmentShaderStr|], null)
        OpenGL.Gl.CompileShader fragmentShader
        OpenGL.Gl.AttachShader (program, fragmentShader)
        Assert ()

        // link program
        OpenGL.Gl.LinkProgram program
        Assert ()

        // fin
        program

    /// Create a sprite shader program with uniforms:
    ///     0: sampler2D tex
    /// and attributes:
    ///     0: vec2 pos
    ///     1: vec2 coordIn
    ///     2: vec4 colorIn
    let CreateSpriteProgram () =
    
        // vertex shader code
        let samplerVertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "layout(location = 0) in vec2 pos;"
             "layout(location = 1) in vec2 coordIn;"
             "layout(location = 2) in vec4 colorIn;"
             "out vec2 coord;"
             "out vec4 color;"
             "void main()"
             "{"
             "  gl_Position = vec4(pos.x, pos.y, 0, 1);"
             "  coord = coordIn;"
             "  color = colorIn;"
             "}"] |> String.join "\n"

        // fragment shader code
        let samplerFragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "in vec2 coord;"
             "in vec4 color;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = texture(tex, coord);"
             "}"] |> String.join "\n"

        // create shader program
        let program = CreateShaderProgramFromStrs samplerVertexShaderStr samplerFragmentShaderStr
        let texUniform = OpenGL.Gl.GetUniformLocation (program, "tex")
        Assert ()

        // fin
        (texUniform, program)

    let private BeginSpriteBatch spriteMax =

        // setup gpu vao
        let gpuVao = OpenGL.Gl.GenVertexArray ()
        OpenGL.Gl.BindVertexArray gpuVao
        Assert ()

        // setup gpu buffer
        let gpuBuffer = OpenGL.Gl.GenBuffer ()
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, gpuBuffer)
        OpenGL.Gl.EnableVertexAttribArray 0u
        OpenGL.Gl.EnableVertexAttribArray 1u
        OpenGL.Gl.EnableVertexAttribArray 2u
        OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvPosition"))
        OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvCoord"))
        OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvColor"))
        OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint sizeof<SpriteBatchVertex> * 6u * uint spriteMax, nativeint 0, OpenGL.BufferUsage.StaticDraw)
        Assert ()

        // map cpu buffer
        let cpuBuffer = OpenGL.Gl.MapBuffer (OpenGL.BufferTarget.ArrayBuffer, OpenGL.BufferAccess.WriteOnly)
        Assert ()

        // tear down gpu buffer
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)

        // tear down gpu vao
        OpenGL.Gl.BindVertexArray 0u

        // fin
        (cpuBuffer, gpuBuffer, gpuVao)

    let private EndSpriteBatch spriteCount spriteTexUniform spriteProgram bfs bfd texture gpuBuffer gpuVao =

        // setup state
        OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
        OpenGL.Gl.Enable OpenGL.EnableCap.Blend
        Assert ()

        // setup buffers
        OpenGL.Gl.BindVertexArray gpuVao
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, gpuBuffer)
        Assert ()

        // unmap cpu buffer
        OpenGL.Gl.UnmapBuffer OpenGL.BufferTarget.ArrayBuffer |> ignore<bool>
        Assert ()

        // setup program
        OpenGL.Gl.UseProgram spriteProgram
        OpenGL.Gl.Uniform1i (0, 1, spriteTexUniform)
        OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, texture)
        OpenGL.Gl.BlendFunc (bfs, bfd)
        Assert ()

        // draw geometry
        OpenGL.Gl.DrawArrays (OpenGL.PrimitiveType.Triangles, 0, 6 * spriteCount)
        Assert ()

        // teardown program
        OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
        OpenGL.Gl.UseProgram 0u

        // teardown buffers
        OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
        OpenGL.Gl.DeleteBuffers gpuBuffer
        OpenGL.Gl.BindVertexArray 0u
        OpenGL.Gl.DeleteVertexArrays gpuVao

        // teardown state
        OpenGL.Gl.Disable OpenGL.EnableCap.Blend
        OpenGL.Gl.Disable OpenGL.EnableCap.CullFace

    let AugmentSpriteBatch center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTextureUniform spriteProgram (envOpt : SpriteBatchEnv option) =

        let env =
            let batchState = { BlendingFactorSrc = bfs; BlendingFactorDest = bfd; Texture = texture }
            let batchStateObsolete =
                match envOpt with
                | Some env -> not (batchState.Equals env.BatchState) || env.SpriteIndex = Constants.Render.SpriteBatchSize
                | None -> true
            if batchStateObsolete then
                match envOpt with
                | Some env ->
                    EndSpriteBatch
                        env.SpriteIndex env.SpriteTexUniform env.SpriteProgram
                        env.BatchState.BlendingFactorSrc env.BatchState.BlendingFactorDest env.BatchState.Texture
                        env.GpuBuffer
                        env.GpuVao
                | None -> ()
                let (cpuBuffer, gpuBuffer, gpuVao) = BeginSpriteBatch Constants.Render.SpriteBatchSize
                let batchState = { BlendingFactorSrc = bfs; BlendingFactorDest = bfd; Texture = texture }
                { SpriteIndex = 0
                  SpriteTexUniform = spriteTextureUniform
                  SpriteProgram = spriteProgram
                  CpuBuffer = cpuBuffer
                  GpuBuffer = gpuBuffer
                  GpuVao = gpuVao
                  BatchState = batchState }
            else Option.get envOpt // guaranteed to exist

        let flipper =
            match flip with
            | FlipNone -> v2 1.0f 1.0f
            | FlipH -> v2 -1.0f 1.0f
            | FlipV -> v2 1.0f -1.0f
            | FlipHV -> v2 -1.0f -1.0f

        let position0 = (position - center).Rotate rotation + center
        let mutable vertex0 = Unchecked.defaultof<SpriteBatchVertex>
        vertex0.SbvPosition <- position0
        vertex0.SbvCoord <- coords.BottomLeft * flipper
        vertex0.SbvColor <- color

        let position1Unrotated = v2 (position.X + size.X) position.Y
        let position1 = (position1Unrotated - center).Rotate rotation + center
        let mutable vertex1 = Unchecked.defaultof<SpriteBatchVertex>
        vertex1.SbvPosition <- position1
        vertex1.SbvCoord <- coords.BottomRight * flipper
        vertex1.SbvColor <- color

        let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
        let position2 = (position2Unrotated - center).Rotate rotation + center
        let mutable vertex2 = Unchecked.defaultof<SpriteBatchVertex>
        vertex2.SbvPosition <- position2
        vertex2.SbvCoord <- coords.TopRight * flipper
        vertex2.SbvColor <- color

        let position3Unrotated = v2 position.X (position.Y + size.Y)
        let position3 = (position3Unrotated - center).Rotate rotation + center
        let mutable vertex3 = Unchecked.defaultof<SpriteBatchVertex>
        vertex3.SbvPosition <- position3
        vertex3.SbvCoord <- coords.TopLeft * flipper
        vertex3.SbvColor <- color

        let vertexSize = nativeint sizeof<SpriteBatchVertex>
        let cpuOffset = env.CpuBuffer + nativeint env.SpriteIndex * vertexSize * nativeint 6
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex0, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex1, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex2, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex3, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex0, cpuOffset, false)
        let cpuOffset = cpuOffset + vertexSize
        Marshal.StructureToPtr<SpriteBatchVertex> (vertex2, cpuOffset, false)

        Some { env with SpriteIndex = inc env.SpriteIndex }

    let FlushSpriteBatch envOpt =
        match envOpt with
        | Some env ->
            if env.SpriteIndex > 0 then
                EndSpriteBatch
                    env.SpriteIndex env.SpriteTexUniform env.SpriteProgram
                    env.BatchState.BlendingFactorSrc
                    env.BatchState.BlendingFactorDest
                    env.BatchState.Texture
                    env.GpuBuffer
                    env.GpuVao
        | None -> ()

    let RenderSprite center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTexUniform spriteProgram =
        let envOpt = AugmentSpriteBatch center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTexUniform spriteProgram None
        FlushSpriteBatch envOpt