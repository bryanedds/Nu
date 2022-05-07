// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open System.Runtime.InteropServices
open SDL2
open Prime
open Nu

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
      TextureInternalFormat : Gl.PixelInternalFormat }

[<RequireQualifiedAccess>]
module Gl =

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
                { Texture : uint
                  BlendingFactorSrc : Gl.BlendingFactorSrc
                  BlendingFactorDest : Gl.BlendingFactorDest }
        
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
            let error = Gl.GetError ()
            if error <> Gl.ErrorCode.NoError then
                Log.debug ("Gl assertion failed due to: " + string error)

        /// Attempt to create a 2d texture from a file.
        let TryCreateTexture2d (minFilter, magFilter, filePath : string) =

            // load the texture into an SDL surface, converting its format if needed
            let (surfacePtr, surface) =
                let unconvertedPtr = SDL_image.IMG_Load filePath
                let unconverted = Marshal.PtrToStructure<SDL.SDL_Surface> unconvertedPtr
                let unconvertedFormat = Marshal.PtrToStructure<SDL.SDL_PixelFormat> unconverted.format
                if unconvertedFormat.format <> SDL.SDL_PIXELFORMAT_RGBA8888 then
                    let mutable convertedFormat = SDL.SDL_PixelFormat ()
                    convertedFormat.format <- SDL.SDL_PIXELFORMAT_RGBA8888
                    convertedFormat.BitsPerPixel <- byte 32 // this seems to be the only additional field that needs a definition to make a functioning SDL_PixelFormat...
                    let convertedFormatPtr = Marshal.AllocHGlobal sizeof<SDL.SDL_PixelFormat>
                    Marshal.StructureToPtr (convertedFormat, convertedFormatPtr, false)
                    let convertedPtr = SDL.SDL_ConvertSurface (unconvertedPtr, convertedFormatPtr, 0u)
                    Marshal.FreeHGlobal convertedFormatPtr
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
            let textures = [|0u|]
            Gl.GenTextures (1, textures)
            let texture = textures.[0]
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMagFilter, int magFilter)
            let internalFormat = Gl.PixelInternalFormat.Rgba8
            Gl.TexImage2D (Gl.TextureTarget.Texture2D, 0, internalFormat, surface.w, surface.h, 0, Gl.PixelFormat.Rgba, Gl.PixelType.UnsignedByte, surface.pixels)

            // teardown surface
            SDL.SDL_FreeSurface surfacePtr

            // check for errors
            match Gl.GetError () with
            | Gl.ErrorCode.NoError ->
                let metadata = { TextureWidth = surface.w; TextureHeight = surface.h; TextureInternalFormat = internalFormat }
                Right (metadata, texture)
            | error -> Left (string error)

        /// Attempt to create a sprite texture.
        let TryCreateSpriteTexture (filePath) =
            TryCreateTexture2d
                (Gl.TextureParameter.Nearest,
                 Gl.TextureParameter.Nearest,
                 filePath)

        let DeleteTexture (texture : uint) =
            Gl.DeleteTextures (1, [|texture|])

        /// Create a texture frame buffer.
        let CreateTextureFramebuffer () =

            // create frame buffer object
            let framebuffers = [|0u|]
            Gl.GenFramebuffers (1, framebuffers)
            let framebuffer = framebuffers.[0]
            Gl.BindFramebuffer (Gl.FramebufferTarget.Framebuffer, framebuffer)
            Assert ()

            // create texture buffer
            let textures = [|0u|]
            Gl.GenTextures (1, textures)
            let texture = textures.[0]
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Gl.TexImage2D (Gl.TextureTarget.Texture2D, 0, Gl.PixelInternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, Gl.PixelFormat.Rgba, Gl.PixelType.Float, nativeint 0)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMinFilter, int Gl.TextureParameter.Nearest)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMagFilter, int Gl.TextureParameter.Nearest)
            Gl.FramebufferTexture (Gl.FramebufferTarget.Framebuffer, Gl.FramebufferAttachment.ColorAttachment0, texture, 0)
            Assert ()

            // create depth and stencil buffers
            let depthBuffers = [|0u|]
            Gl.GenRenderbuffers (1, depthBuffers)
            let depthBuffer = depthBuffers.[0]
            Gl.BindRenderbuffer (Gl.RenderbufferTarget.Renderbuffer, depthBuffer)
            Gl.RenderbufferStorage (Gl.RenderbufferTarget.Renderbuffer, Gl.RenderbufferStorageEnum.Depth32fStencil8, Constants.Render.ResolutionX, Constants.Render.ResolutionY)
            Gl.FramebufferRenderbuffer (Gl.FramebufferTarget.Framebuffer, Gl.FramebufferAttachment.DepthStencilAttachment, Gl.RenderbufferTarget.Renderbuffer, depthBuffer)
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
            let vertexBuffers = [|0u|]
            Gl.GenBuffers (1, vertexBuffers)
            let vertexBuffer = vertexBuffers.[0]
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, vertexBuffer)
            let vertexDataSize = IntPtr (8 * 4 * sizeof<single>)
            let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
            Gl.BufferData (Gl.BufferTarget.ArrayBuffer, vertexDataSize, vertexDataPtr.AddrOfPinnedObject(), Gl.BufferUsageHint.StaticDraw)
            Assert ()

            // create index buffer
            let indexData = [|0u; 1u; 2u; 3u|]
            let indexBuffers = [|0u|]
            Gl.GenBuffers (1, indexBuffers)
            let indexBuffer = indexBuffers.[0]
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, indexBuffer)
            let indexDataSize = IntPtr (4 * sizeof<uint>)
            let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
            Gl.BufferData (Gl.BufferTarget.ArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject(), Gl.BufferUsageHint.StaticDraw)
            Assert ()

            // fin
            (vertexBuffer, indexBuffer)

        /// Create a shader program from vertex and fragment code strings.
        let CreateShaderProgramFromStrs vertexShaderStr fragmentShaderStr =

            // construct gl program
            let program = Gl.CreateProgram ()
            Assert ()

            // add vertex shader to program
            let vertexShader = Gl.CreateShader Gl.ShaderType.VertexShader
            Gl.ShaderSource (vertexShader, 1, [|vertexShaderStr|], null)
            Gl.CompileShader vertexShader
            Gl.AttachShader (program, vertexShader)
            Assert ()

            // add fragement shader to program
            let fragmentShader = Gl.CreateShader Gl.ShaderType.FragmentShader
            Gl.ShaderSource (fragmentShader, 1, [|fragmentShaderStr|], null)
            Gl.CompileShader fragmentShader
            Gl.AttachShader (program, fragmentShader)
            Assert ()

            // link program
            Gl.LinkProgram program
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
            let texUniform = Gl.GetUniformLocation (program, "tex")
            Assert ()

            // fin
            (texUniform, program)

        let private BeginSpriteBatch spriteMax =

            // setup draw state
            Gl.Enable Gl.EnableCap.Blend
            Gl.Enable Gl.EnableCap.CullFace
            Assert ()

            // setup gpu vao
            let gpuVaos = [|0u|]
            Gl.GenVertexArrays (1, gpuVaos)
            let gpuVao = gpuVaos.[0]
            Gl.BindVertexArray gpuVao
            Assert ()

            // setup gpu buffer
            let gpuBuffers = [|0u|]
            Gl.GenBuffers (1, gpuBuffers)
            let gpuBuffer = gpuBuffers.[0]
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, gpuBuffer)
            Gl.EnableVertexAttribArray 0
            Gl.EnableVertexAttribArray 1
            Gl.EnableVertexAttribArray 2
            Gl.VertexAttribPointer (0, 2, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvPosition"))
            Gl.VertexAttribPointer (1, 2, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvCoord"))
            Gl.VertexAttribPointer (2, 4, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, "SbvColor"))
            Gl.BufferData (Gl.BufferTarget.ArrayBuffer, nativeint (sizeof<SpriteBatchVertex> * 6 * spriteMax), nativeint 0, Gl.BufferUsageHint.StaticDraw)
            Assert ()

            // map cpu buffer
            let cpuBuffer = Gl.MapBuffer (Gl.BufferTarget.ArrayBuffer, Gl.BufferAccess.WriteOnly)
            Assert ()

            // tear down gpu buffer
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, 0u)

            // tear down gpu vao
            Gl.BindVertexArray 0u

            // teardown draw state
            Gl.Disable Gl.EnableCap.CullFace
            Gl.Disable Gl.EnableCap.Blend

            // fin
            (cpuBuffer, gpuBuffer, gpuVao)

        let private EndSpriteBatch spriteCount spriteTexUniform spriteProgram texture gpuBuffer gpuVao =

            // setup buffers
            Gl.BindVertexArray gpuVao
            Gl.EnableVertexAttribArray 0
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, gpuBuffer)
            Assert ()

            // unmap cpu buffer
            Gl.UnmapBuffer Gl.BufferTarget.ArrayBuffer |> ignore<bool>
            Assert ()

            // setup program
            Gl.UseProgram spriteProgram
            Gl.Uniform1i (spriteTexUniform, 0) // set uniform to texture slot 0
            Gl.ActiveTexture 0 // make texture slot 0 active
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Assert ()

            // draw geometry
            Gl.DrawArrays (Gl.BeginMode.Triangles, 0, 6 * spriteCount)
            Assert ()

            // teardown program
            Gl.BindTexture (Gl.TextureTarget.Texture2D, 0u)
            Gl.UseProgram 0u

            // teardown buffers
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, 0u)
            Gl.DeleteBuffers (1, [|gpuBuffer|])
            Gl.BindVertexArray 0u
            Gl.DeleteVertexArrays (1, [|gpuVao|])

        let AugmentSpriteBatch center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTextureUniform spriteProgram (envOpt : SpriteBatchEnv option) =

            let env =
                let batchState = { Texture = texture; BlendingFactorSrc = bfs; BlendingFactorDest = bfd }
                let batchStateObsolete =
                    match envOpt with
                    | Some env -> not (batchState.Equals env.BatchState) || env.SpriteIndex = Constants.Render.SpriteBatchSize
                    | None -> true
                if batchStateObsolete then
                    match envOpt with
                    | Some env -> EndSpriteBatch env.SpriteIndex env.SpriteTexUniform env.SpriteProgram env.BatchState.Texture env.GpuBuffer env.GpuVao
                    | None -> ()
                    let (cpuBuffer, gpuBuffer, gpuVao) = BeginSpriteBatch Constants.Render.SpriteBatchSize
                    let batchState = { Texture = texture; BlendingFactorSrc = bfs; BlendingFactorDest = bfd }
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

            let position0 = (position - center).Rotate rotation
            let mutable vertex0 = Unchecked.defaultof<SpriteBatchVertex>
            vertex0.SbvPosition <- position0
            vertex0.SbvCoord <- coords.BottomLeft * flipper
            vertex0.SbvColor <- color

            let position1Unrotated = v2 (position.X + size.X) position.Y
            let position1 = (position1Unrotated - center).Rotate rotation
            let mutable vertex1 = Unchecked.defaultof<SpriteBatchVertex>
            vertex1.SbvPosition <- position1
            vertex1.SbvCoord <- coords.BottomRight * flipper
            vertex1.SbvColor <- color

            let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
            let position2 = (position2Unrotated - center).Rotate rotation
            let mutable vertex2 = Unchecked.defaultof<SpriteBatchVertex>
            vertex2.SbvPosition <- position2
            vertex2.SbvCoord <- coords.TopRight * flipper
            vertex2.SbvColor <- color

            let position3Unrotated = v2 position.X (position.Y + size.Y)
            let position3 = (position3Unrotated - center).Rotate rotation
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
                    EndSpriteBatch env.SpriteIndex env.SpriteTexUniform env.SpriteProgram env.BatchState.Texture env.GpuBuffer env.GpuVao
            | None -> ()

        let RenderSprite center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTexUniform spriteProgram =
            let envOpt = AugmentSpriteBatch center (position : Vector2) (size : Vector2) rotation color (coords : Box2) texture flip bfs bfd spriteTexUniform spriteProgram None
            FlushSpriteBatch envOpt