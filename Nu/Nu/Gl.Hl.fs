// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Drawing
open System.Drawing.Imaging
open System.Numerics
open System.Runtime.InteropServices
open Prime
open Nu

/// A texture's metadata.
type TextureMetadata =
    { TextureWidth : int
      TextureHeight : int
      TextureInternalFormat : Gl.PixelInternalFormat }

[<RequireQualifiedAccess>]
module Gl =

    [<RequireQualifiedAccess>]
    module Hl =
    
        type [<NoEquality; NoComparison; Struct; StructLayout (LayoutKind.Sequential)>] SpriteBatchVertex =
            private
                { mutable SbvPosition : Vector4
                  mutable SbvColor : Color
                  mutable SbvTexCoord : Vector2 }

        type [<StructuralEquality; NoComparison>] SpriteBatchState =
            private
                { BlendingFactorSrc : Gl.BlendingFactorSrc
                  BlendingFactorDest : Gl.BlendingFactorDest
                  Texture : uint }
        
        type [<NoEquality; NoComparison>] SpriteBatchEnv =
            private
                { SpriteIndex : int
                  CpuBuffer : nativeint
                  GpuBuffer : uint
                  BatchState : SpriteBatchState }

        /// Create a texture frame buffer.
        let CreateTextureFramebuffer () =

            // create frame buffer object
            let framebuffers = [|0u|]
            Gl.GenFramebuffers (1, framebuffers)
            let framebuffer = framebuffers.[0]
            Gl.BindFramebuffer (Gl.FramebufferTarget.Framebuffer, framebuffer)

            // create texture buffer
            let textures = [|0u|]
            Gl.GenTextures (1, textures)
            let texture = textures.[0]
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Gl.TexImage2D (Gl.TextureTarget.Texture2D, 0, Gl.PixelInternalFormat.Rgba32f, Constants.Render.ResolutionX, Constants.Render.ResolutionY, 0, Gl.PixelFormat.Rgba, Gl.PixelType.Float, nativeint 0)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMinFilter, int Gl.TextureParameter.Nearest)
            Gl.TexParameteri (Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMagFilter, int Gl.TextureParameter.Nearest)
            Gl.FramebufferTexture (Gl.FramebufferTarget.Framebuffer, Gl.FramebufferAttachment.ColorAttachment0, texture, 0)

            // create depth and stencil buffers
            let depthBuffers = [|0u|]
            Gl.GenRenderbuffers (1, depthBuffers)
            let depthBuffer = depthBuffers.[0]
            Gl.BindRenderbuffer (Gl.RenderbufferTarget.Renderbuffer, depthBuffer)
            Gl.RenderbufferStorage (Gl.RenderbufferTarget.Renderbuffer, Gl.RenderbufferStorageEnum.Depth32fStencil8, Constants.Render.ResolutionX, Constants.Render.ResolutionY)
            Gl.FramebufferRenderbuffer (Gl.FramebufferTarget.Framebuffer, Gl.FramebufferAttachment.DepthStencilAttachment, Gl.RenderbufferTarget.Renderbuffer, depthBuffer)

            // fin
            (texture, framebuffer)

        /// Create a full screen quad.
        let CreateFullScreenQuad () =

            // build vertex data
            let vertexData =
                [|-1.0f; -1.0f;
                  +1.0f; -1.0f;
                  +1.0f; +1.0f;
                  -1.0f; +1.0f|]

            // create vertex buffer
            let vertexBuffers = [|0u|]
            Gl.GenBuffers (1, vertexBuffers)
            let vertexBuffer = vertexBuffers.[0]
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, vertexBuffer)
            let vertexDataSize = IntPtr (2 * 4 * sizeof<single>)
            let vertexDataPtr = GCHandle.Alloc (vertexData, GCHandleType.Pinned)
            Gl.BufferData (Gl.BufferTarget.ArrayBuffer, vertexDataSize, vertexDataPtr.AddrOfPinnedObject(), Gl.BufferUsageHint.StaticDraw)

            // create index buffer
            let indexData = [|0u; 1u; 2u; 3u|]
            let indexBuffers = [|0u|]
            Gl.GenBuffers (1, indexBuffers)
            let indexBuffer = indexBuffers.[0]
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, indexBuffer)
            let indexDataSize = IntPtr (4 * sizeof<uint>)
            let indexDataPtr = GCHandle.Alloc (indexData, GCHandleType.Pinned)
            Gl.BufferData (Gl.BufferTarget.ArrayBuffer, indexDataSize, indexDataPtr.AddrOfPinnedObject(), Gl.BufferUsageHint.StaticDraw)

            // fin
            (vertexBuffer, indexBuffer)

        /// Create a shader program from vertex and fragment code strings.
        let CreateShaderProgramFromStrs vertexShaderStr fragmentShaderStr =

            // construct gl program
            let program = Gl.CreateProgram ()

            // add vertex shader to program
            let vertexShader = Gl.CreateShader Gl.ShaderType.VertexShader
            Gl.ShaderSource (vertexShader, 1, [|vertexShaderStr|], null)
            Gl.CompileShader vertexShader
            Gl.AttachShader (program, vertexShader)

            // add fragement shader to program
            let fragmentShader = Gl.CreateShader Gl.ShaderType.FragmentShader
            Gl.ShaderSource (fragmentShader, 1, [|fragmentShaderStr|], null)
            Gl.CompileShader fragmentShader
            Gl.AttachShader (program, fragmentShader)

            // link program
            Gl.LinkProgram program
            program

        /// Create a blit shader.
        let CreateBlitShaderProgram () =
        
            // vertex shader code
            let samplerVertexShaderStr =
                [Constants.Render.GlslVersionPragma
                 "in vec2 pos;"
                 "out vec2 texCoord;"
                 "void main()"
                 "{"
                 "  gl_Position = vec4(pos.x, pos.y, 0, 1);"
                 "  texCoord = vec2((pos.x + 1) * 0.5, (pos.y + 1) * 0.5);"
                 "}"] |> String.join "\n"

            // fragment shader code
            let samplerFragmentShaderStr =
                [Constants.Render.GlslVersionPragma
                 "uniform sampler2D tex;"
                 "in vec2 texCoord;"
                 "out vec4 frag;"
                 "void main()"
                 "{"
                 "  frag = texture(tex, texCoord);"
                 "}"] |> String.join "\n"

            // create blit shader program
            let blitShaderProgram = CreateShaderProgramFromStrs samplerVertexShaderStr samplerFragmentShaderStr
            let texUniform = Gl.GetUniformLocation (blitShaderProgram, "tex")
            let posAttrib = Gl.GetAttribLocation (blitShaderProgram, "pos")
            (blitShaderProgram, texUniform, posAttrib)

        /// Create a blit shader.
        let Create2dShaderProgram () =
        
            // vertex shader code
            let samplerVertexShaderStr =
                [Constants.Render.GlslVersionPragma
                 "in vec2 pos;"
                 "out vec2 texCoord;"
                 "void main()"
                 "{"
                 "  gl_Position = vec4(pos.x, pos.y, 0, 1);"
                 "  texCoord = vec2((pos.x + 1) * 0.5, (pos.y + 1) * 0.5);"
                 "}"] |> String.join "\n"

            // fragment shader code
            let samplerFragmentShaderStr =
                [Constants.Render.GlslVersionPragma
                 "uniform sampler2D tex;"
                 "in vec2 texCoord;"
                 "out vec4 frag;"
                 "void main()"
                 "{"
                 "  frag = texture(tex, texCoord);"
                 "}"] |> String.join "\n"

            // create blit shader program
            let blitShaderProgram = CreateShaderProgramFromStrs samplerVertexShaderStr samplerFragmentShaderStr
            let texUniform = Gl.GetUniformLocation (blitShaderProgram, "tex")
            let posAttrib = Gl.GetAttribLocation (blitShaderProgram, "pos")
            (blitShaderProgram, texUniform, posAttrib)

        let TryCreateTexture2d (minFilter, magFilter, filePath : string) =
            let textures = [|0u|]
            Gl.GenTextures (1, textures)
            let texture = textures.[0]
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Gl.TexParameteri(Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMinFilter, int minFilter)
            Gl.TexParameteri(Gl.TextureTarget.Texture2D, Gl.TextureParameterName.TextureMagFilter, int magFilter) 
            try
                let internalFormat = Gl.PixelInternalFormat.Rgba8
                use bitmap = new Bitmap (filePath)
                let bitmapData = bitmap.LockBits (Rectangle (0,0,bitmap.Width, bitmap.Height), ImageLockMode.ReadOnly, PixelFormat.Canonical)
                Gl.TexImage2D (Gl.TextureTarget.Texture2D, 0, internalFormat, bitmap.Width, bitmap.Height, 0, Gl.PixelFormat.Rgba, Gl.PixelType.Byte, bitmapData.Scan0)
                let error = Gl.GetError ()
                if error = Gl.ErrorCode.NoError then
                    let metadata = { TextureWidth = bitmap.Width; TextureHeight = bitmap.Height; TextureInternalFormat = internalFormat }
                    Right (metadata, texture)
                else Left (string error)
            with exn -> Left (string exn)

        let TryCreateSpriteTexture (filePath) =
            TryCreateTexture2d
                (Gl.TextureParameter.Nearest,
                 Gl.TextureParameter.Nearest,
                 filePath)

        let DeleteTexture (texture : uint) =
            Gl.DeleteTextures (1, [|texture|])

        let private CreateSpriteBatchBuffer (maxSprites) =
            //Gl.GenVertexArrays(1, &this->vao);
            let buffers = [|0u|]
            Gl.GenBuffers(1, buffers);
            let buffer = buffers.[0]
            //Gl.BindVertexArray(this->vao);
            Gl.BindBuffer(Gl.BufferTarget.ArrayBuffer, buffer)
            Gl.BufferData(Gl.BufferTarget.ArrayBuffer, nativeint (sizeof<SpriteBatchVertex> * 4 * maxSprites), nativeint 0, Gl.BufferUsageHint.DynamicDraw);
            //Gl.EnableVertexAttribArray(0);
            //Gl.EnableVertexAttribArray(1);
            //Gl.EnableVertexAttribArray(2);
            Gl.VertexAttribPointer (0, 4, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, Property? SbvPosition));
            Gl.VertexAttribPointer (1, 4, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, Property? SbvColor));
            Gl.VertexAttribPointer (2, 2, Gl.VertexAttribPointerType.Float, false, sizeof<SpriteBatchVertex>, Marshal.OffsetOf (typeof<SpriteBatchVertex>, Property? SbvTexCoord));
            //Gl.BindBuffer(Gl.BufferTarget.ArrayBuffer, buffer);
            //Gl.BindVertexArray(0);
            buffer

        let private BeginSpriteBatch (buffer : uint) =
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, buffer)
            Gl.MapBuffer (Gl.BufferTarget.ArrayBuffer, Gl.BufferAccess.WriteOnly)

        let private EndSpriteBatch () =
            Gl.UnmapBuffer Gl.BufferTarget.ArrayBuffer |> ignore<bool>
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, 0u)

        let private RenderSpriteBatch numSprites texture cpuBuffer gpuBuffer =
            Gl.BindTexture (Gl.TextureTarget.Texture2D, texture)
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, gpuBuffer)
            Gl.BufferSubData (Gl.BufferTarget.ArrayBuffer, nativeint 0, nativeint (sizeof<SpriteBatchVertex> * 4 * numSprites), cpuBuffer)
            //Gl.BindVertexArray(this->vao);
            Gl.DrawArrays (Gl.BeginMode.Triangles, 0, 6 * numSprites)
            Gl.BindVertexArray 0u
            Gl.BindBuffer (Gl.BufferTarget.ArrayBuffer, 0u)

        let AugmentSpriteBatch center (position : Vector2) (size : Vector2) rotation color texture bfs bfd (envOpt : SpriteBatchEnv option) =

            let env =
                let batchState = { BlendingFactorSrc = bfs; BlendingFactorDest = bfd; Texture = texture }
                let batchStateObsolete =
                    match envOpt with
                    | Some env -> not (batchState.Equals env.BatchState) || env.SpriteIndex = Constants.Render.SpriteBatchSize
                    | None -> true
                if batchStateObsolete then
                    match envOpt with
                    | Some env ->
                        EndSpriteBatch ()
                        RenderSpriteBatch env.SpriteIndex env.BatchState.Texture env.CpuBuffer env.GpuBuffer
                    | None -> ()
                    let gpuBuffer = CreateSpriteBatchBuffer Constants.Render.SpriteBatchSize
                    let cpuBuffer = BeginSpriteBatch gpuBuffer
                    let batchState = { BlendingFactorSrc = bfs; BlendingFactorDest = bfd; Texture = texture }
                    { SpriteIndex = 0; CpuBuffer = cpuBuffer; GpuBuffer = gpuBuffer; BatchState = batchState }
                else Option.get envOpt // guaranteed to be

            let vertexSize = nativeint sizeof<SpriteBatchVertex>
            let cpuOffset = env.CpuBuffer + vertexSize * nativeint 4
            let mutable vertex0 = Marshal.PtrToStructure<SpriteBatchVertex> cpuOffset
            let position0 = (position - center).Rotate rotation
            vertex0.SbvPosition <- v4 position0.X position0.Y 0.0f 0.0f
            vertex0.SbvColor <- color
            vertex0.SbvTexCoord <- v2Zero
            Marshal.StructureToPtr<SpriteBatchVertex> (vertex0, cpuOffset, false)

            let cpuOffset = cpuOffset + vertexSize
            let mutable vertex1 = Marshal.PtrToStructure<SpriteBatchVertex> cpuOffset
            let position1Unrotated = v2 (position.X + size.X) position.Y
            let position1 = (position1Unrotated - center).Rotate rotation
            vertex1.SbvPosition <- v4 position1.X position1.Y 0.0f 0.0f
            vertex1.SbvColor <- color
            vertex1.SbvTexCoord <- v2Zero
            Marshal.StructureToPtr<SpriteBatchVertex> (vertex1, cpuOffset, false)

            let cpuOffset = cpuOffset + vertexSize
            let mutable vertex2 = Marshal.PtrToStructure<SpriteBatchVertex> cpuOffset
            let position2Unrotated = v2 (position.X + size.X) (position.Y + size.Y)
            let position2 = (position2Unrotated - center).Rotate rotation
            vertex2.SbvPosition <- v4 position2.X position2.Y 0.0f 0.0f
            vertex2.SbvColor <- color
            vertex2.SbvTexCoord <- v2Zero
            Marshal.StructureToPtr<SpriteBatchVertex> (vertex2, cpuOffset, false)

            let cpuOffset = cpuOffset + vertexSize
            let mutable vertex3 = Marshal.PtrToStructure<SpriteBatchVertex> cpuOffset
            let position3Unrotated = v2 position.X (position.Y + size.Y)
            let position3 = (position3Unrotated - center).Rotate rotation
            vertex3.SbvPosition <- v4 position3.X position3.Y 0.0f 0.0f
            vertex3.SbvColor <- color
            vertex3.SbvTexCoord <- v2Zero
            Marshal.StructureToPtr<SpriteBatchVertex> (vertex3, cpuOffset, false)

            { env with SpriteIndex = inc env.SpriteIndex }

        let FlushSpriteBatch envOpt =
            match envOpt with
            | Some env ->
                if env.SpriteIndex > 0 then
                    EndSpriteBatch ()
                    RenderSpriteBatch env.SpriteIndex env.BatchState.Texture env.CpuBuffer env.GpuBuffer
                else ()
            | None -> ()