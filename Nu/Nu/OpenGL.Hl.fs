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

/// Force qualification of OpenGL namespace in Nu unless opened explicitly.
[<RequireQualifiedAccess>]
module OpenGL = let _ = ()

namespace OpenGL
open System.Collections.Generic
open System.Numerics
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

[<RequireQualifiedAccess>]
module Hl =

    /// Assert a lack of Gl error. Has an generic parameter to enable value pass-through.
    let Assert (a : 'a) =
#if DEBUG
        let error = OpenGL.Gl.GetError ()
        if error <> OpenGL.ErrorCode.NoError then
            Log.debug ("Gl assertion failed due to: " + string error)
#endif
        a

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
    ///     0: vec2 position
    ///     1: vec2 coordsIn
    ///     2: vec4 colorIn
    let private CreateSpriteProgram () =

        // vertex shader code
        let samplerVertexShaderStr =
            [Constants.Render.GlslVersionPragma
             "layout(location = 0) in vec2 position;"
             "layout(location = 1) in vec2 coordsIn;"
             "layout(location = 2) in vec4 colorIn;"
             "out vec2 coords;"
             "out vec4 color;"
             "void main()"
             "{"
             "  gl_Position = vec4(position.x, position.y, 0, 1);"
             "  coords = coordsIn;"
             "  color = colorIn;"
             "}"] |> String.join "\n"

        // fragment shader code
        let samplerFragmentShaderStr =
            [Constants.Render.GlslVersionPragma
             "uniform sampler2D tex;"
             "in vec2 coords;"
             "in vec4 color;"
             "out vec4 frag;"
             "void main()"
             "{"
             "  frag = color * texture(tex, coords);"
             "}"] |> String.join "\n"

        // create shader program
        let program = CreateShaderProgramFromStrs samplerVertexShaderStr samplerFragmentShaderStr
        let texUniform = OpenGL.Gl.GetUniformLocation (program, "tex")
        Assert ()

        // fin
        (texUniform, program)

    [<RequireQualifiedAccess>]
    module SpriteBatch =

        type [<StructuralEquality; NoComparison; StructLayout (LayoutKind.Sequential)>] private Vertex =
            struct
                val mutable Position : Vector2
                val mutable Coords : Vector2
                val mutable Color : Color
                end

        type [<NoEquality; NoComparison>] private Context =
            { mutable CpuBufferOpt : nativeint ValueOption
              GpuBuffer : uint
              GpuVao : uint }

            static member bind context =
                OpenGL.Gl.BindVertexArray context.GpuVao
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, context.GpuBuffer)

            static member unbind (_ : Context) =
                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
                OpenGL.Gl.BindVertexArray 0u

            static member create spriteMax =

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
                OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Position"))
                OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Coords"))
                OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribType.Float, false, sizeof<Vertex>, Marshal.OffsetOf (typeof<Vertex>, "Color"))
                OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, uint sizeof<Vertex> * 6u * uint spriteMax, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                Assert ()

                // unbind
                let context = { CpuBufferOpt = ValueNone; GpuBuffer = gpuBuffer; GpuVao = gpuVao }
                Context.unbind context
                Assert ()
                context

            static member destroy context =
                OpenGL.Gl.DeleteBuffers context.GpuBuffer
                OpenGL.Gl.DeleteVertexArrays context.GpuVao
                Context.unbind context

            static member map context =
                Context.bind context
                let cpuBuffer = OpenGL.Gl.MapBuffer (OpenGL.BufferTarget.ArrayBuffer, OpenGL.BufferAccess.WriteOnly)
                context.CpuBufferOpt <- ValueSome cpuBuffer
                Context.unbind context
                cpuBuffer

            static member unmap context =
                let result = OpenGL.Gl.UnmapBuffer OpenGL.BufferTarget.ArrayBuffer
                context.CpuBufferOpt <- ValueNone
                result

        type [<NoEquality; NoComparison>] private Pool =
            { mutable ContextCurrent : int
              Contexts : Context List
              SpriteMax : int }

            static member create spriteMax prealloc =
                let contexts = Seq.init (max 1 prealloc) (fun _ -> Assert (Context.create spriteMax)) |> List
                { ContextCurrent = 0
                  Contexts = contexts
                  SpriteMax = spriteMax }

            static member destroy pool =
                for context in pool.Contexts do
                    Context.destroy context

            static member next pool =
                pool.ContextCurrent <- inc pool.ContextCurrent

            static member reset pool =
                pool.ContextCurrent <- 0

            member this.Current =
                while this.ContextCurrent >= this.Contexts.Count do this.Contexts.Add (Context.create this.SpriteMax)
                this.Contexts.[this.ContextCurrent]

        type [<StructuralEquality; NoComparison>] private State =
            { BlendingFactorSrc : OpenGL.BlendingFactor
              BlendingFactorDst : OpenGL.BlendingFactor
              BlendingEquation : OpenGL.BlendEquationMode
              Texture : uint }

            static member create bfs bfd beq texture =
                { BlendingFactorSrc = bfs; BlendingFactorDst = bfd; BlendingEquation = beq; Texture = texture }

        type [<NoEquality; NoComparison>] Env =
            private
                { mutable SpriteIndex : int
                  SpriteTexUniform : int
                  SpriteProgram : uint
                  Pool : Pool
                  mutable State : State }

        let private Flush env =

            // setup state
            OpenGL.Gl.Enable OpenGL.EnableCap.CullFace
            OpenGL.Gl.Enable OpenGL.EnableCap.Blend
            Assert ()

            // setup context
            let context = env.Pool.Current
            Context.bind context
            Assert ()

            // setup program
            OpenGL.Gl.UseProgram env.SpriteProgram
            OpenGL.Gl.Uniform1i (0, 1, env.SpriteTexUniform)
            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, env.State.Texture)
            OpenGL.Gl.BlendEquation env.State.BlendingEquation
            OpenGL.Gl.BlendFunc (env.State.BlendingFactorSrc, env.State.BlendingFactorDst)
            Assert ()

            // attempt to draw geometry
            if Context.unmap context then
                OpenGL.Gl.DrawArrays (OpenGL.PrimitiveType.Triangles, 0, 6 * env.SpriteIndex)
                Assert ()
            else Log.debug "Failed to draw sprite batch arrays due to inability to unmap cpu buffer."

            // teardown program
            OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero)
            OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, 0u)
            OpenGL.Gl.UseProgram 0u
            Assert ()

            // teardown context
            Context.unbind context
            Assert ()

            // teardown state
            OpenGL.Gl.Disable OpenGL.EnableCap.Blend
            OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
            Assert ()

            // next pool
            Pool.next env.Pool
            env.SpriteIndex <- 0

        let BeginEnv () =
            let (spriteTexUniform, spriteProgram) = CreateSpriteProgram ()
            let pool = Pool.create Constants.Render.SpriteBatchSize Constants.Render.SpriteBatchPoolSize
            let state = State.create OpenGL.BlendingFactor.SrcAlpha OpenGL.BlendingFactor.OneMinusSrcAlpha OpenGL.BlendEquationMode.FuncAdd 0u
            { SpriteIndex = 0; SpriteTexUniform = spriteTexUniform; SpriteProgram = spriteProgram; Pool = pool; State = state }

        let NextSprite center (position : Vector2) (size : Vector2) rotation (coords : Box2) color flip bfs bfd beq texture env =

            // adjust to potential sprite batch state changes
            let state = State.create bfs bfd beq texture
            if not (state.Equals env.State) || env.SpriteIndex = Constants.Render.SpriteBatchSize then
                if env.SpriteIndex > 0 then Flush env
                env.State <- state
            Assert ()

            // access the current context's cpu buffer
            let cpuBuffer =
                let context = env.Pool.Current
                match context.CpuBufferOpt with
                | ValueSome cpuBuffer -> cpuBuffer
                | ValueNone -> Assert (Context.map context)

            // compute a coord flipping value
            let flipper =
                match flip with
                | FlipNone -> v2 1.0f -1.0f
                | FlipH -> v2 -1.0f -1.0f
                | FlipV -> v2 1.0f 1.0f
                | FlipHV -> v2 -1.0f 1.0f

            // compute vertex positions
            let position0 = (position - center).Rotate rotation + center
            let position1Unrotated = v2 (position.X + size.X) position.Y
            let position1 = (position1Unrotated - center).Rotate rotation + center
            let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
            let position2 = (position2Unrotated - center).Rotate rotation + center
            let position3Unrotated = v2 position.X (position.Y + size.Y)
            let position3 = (position3Unrotated - center).Rotate rotation + center

            // compute vertices
            let mutable vertex0 = Unchecked.defaultof<Vertex>
            vertex0.Position <- position0
            vertex0.Coords <- coords.BottomLeft * flipper
            vertex0.Color <- color
            let mutable vertex1 = Unchecked.defaultof<Vertex>
            vertex1.Position <- position1
            vertex1.Coords <- coords.BottomRight * flipper
            vertex1.Color <- color
            let mutable vertex2 = Unchecked.defaultof<Vertex>
            vertex2.Position <- position2
            vertex2.Coords <- coords.TopRight * flipper
            vertex2.Color <- color
            let mutable vertex3 = Unchecked.defaultof<Vertex>
            vertex3.Position <- position3
            vertex3.Coords <- coords.TopLeft * flipper
            vertex3.Color <- color

            // upload vertices
            // TODO: 3D: consider using an EBO to reduce bus utilization.
            // TODO: 3D: consider using a single pre-allocated SpriteVectex[6] to reduce marshaling calls.
            let vertexSize = nativeint sizeof<Vertex>
            let cpuOffset = cpuBuffer + nativeint env.SpriteIndex * vertexSize * nativeint 6
            Marshal.StructureToPtr<Vertex> (vertex0, cpuOffset, false)
            let cpuOffset = cpuOffset + vertexSize
            Marshal.StructureToPtr<Vertex> (vertex1, cpuOffset, false)
            let cpuOffset = cpuOffset + vertexSize
            Marshal.StructureToPtr<Vertex> (vertex2, cpuOffset, false)
            let cpuOffset = cpuOffset + vertexSize
            Marshal.StructureToPtr<Vertex> (vertex3, cpuOffset, false)
            let cpuOffset = cpuOffset + vertexSize
            Marshal.StructureToPtr<Vertex> (vertex0, cpuOffset, false)
            let cpuOffset = cpuOffset + vertexSize
            Marshal.StructureToPtr<Vertex> (vertex2, cpuOffset, false)

            // advance sprite index
            env.SpriteIndex <-inc env.SpriteIndex

        let EndFrame env =
            if env.SpriteIndex > 0 then Flush env
            Pool.reset env.Pool