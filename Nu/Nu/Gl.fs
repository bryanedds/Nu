// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Runtime.InteropServices
open Prime
open Nu

[<RequireQualifiedAccess>]
module Gl =

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
