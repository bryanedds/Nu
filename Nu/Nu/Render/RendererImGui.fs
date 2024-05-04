namespace Nu
open System
open System.Numerics
open System.Runtime.CompilerServices
open ImGuiNET
open Prime

/// Renders an imgui view.
/// NOTE: API is object-oriented / mutation-based because it's ported from a port.
type RendererImGui =
    abstract Initialize : ImFontAtlasPtr -> unit
    abstract Render : ImDrawDataPtr -> unit
    abstract CleanUp : unit -> unit

/// A stub imgui renderer.
type StubRendererImGui () =
    interface RendererImGui with
        member this.Initialize fonts =
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable fontTextureWidth = 0
            let mutable fontTextureHeight = 0
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            fonts.ClearTexData ()
        member this.Render _ = ()
        member this.CleanUp () = ()

[<RequireQualifiedAccess>]
module StubRendererImGui =

    /// Make a stub imgui renderer.
    let make fonts =
        let rendererImGui = StubRendererImGui ()
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui

/// Renders an imgui view via OpenGL.
type GlRendererImGui (windowWidth : int, windowHeight : int) =

    let mutable vertexArrayObject = 0u
    let mutable vertexBufferSize = 8192u
    let mutable vertexBuffer = 0u
    let mutable indexBufferSize = 1024u
    let mutable indexBuffer = 0u
    let mutable shader = 0u
    let mutable shaderProjectionMatrixUniform = 0
    let mutable shaderFontTextureUniform = 0
    let mutable fontTextureWidth = 0
    let mutable fontTextureHeight = 0
    let mutable fontTexture = Unchecked.defaultof<OpenGL.Texture.Texture>
    do ignore windowWidth

    interface RendererImGui with

        member this.Initialize (fonts : ImFontAtlasPtr) =
        
            // initialize vao
            vertexArrayObject <- OpenGL.Gl.GenVertexArray ()
            OpenGL.Gl.BindVertexArray vertexArrayObject
            OpenGL.Hl.Assert ()

            // create vertex buffer
            vertexBuffer <- OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, vertexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Hl.Assert ()

            // create index buffer
            indexBuffer <- OpenGL.Gl.GenBuffer ()
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ElementArrayBuffer, indexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ElementArrayBuffer, indexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Hl.Assert ()

            // configure vao
            let stride = Unsafe.SizeOf<ImDrawVert> ()
            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 0)
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 8)
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribPointerType.UnsignedByte, true, stride, 16)
            OpenGL.Hl.Assert ()

            // finalize vao
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Hl.Assert ()

            // vertex shader code
            // TODO: let's put this code into a .glsl file and load it from there.
            let vertexStr =
                [Constants.OpenGL.GlslVersionPragma
                 ""
                 "uniform mat4 projection;"
                 ""
                 "layout (location = 0) in vec2 position;"
                 "layout (location = 1) in vec2 texCoords;"
                 "layout (location = 2) in vec4 color;"
                 ""
                 "out vec4 colorOut;"
                 "out vec2 texCoordsOut;"
                 ""
                 "void main()"
                 "{"
                 "    colorOut = color;"
                 "    texCoordsOut = texCoords;"
                 "    gl_Position = projection * vec4(position, 0, 1);"
                 "}"] |> String.join "\n"

            // fragment shader code
            let fragmentStr =
                [Constants.OpenGL.GlslVersionPragma
                 ""
                 "uniform sampler2D fontTexture;"
                 ""
                 "in vec4 colorOut;"
                 "in vec2 texCoordsOut;"
                 ""
                 "out vec4 frag;"
                 ""
                 "void main()"
                 "{"
                 "    frag = colorOut * texture(fontTexture, texCoordsOut);"
                 "}"] |> String.join "\n"

            // create shader
            shader <- OpenGL.Shader.CreateShaderFromStrs (vertexStr, fragmentStr)
            shaderProjectionMatrixUniform <- OpenGL.Gl.GetUniformLocation (shader, "projection")
            shaderFontTextureUniform <- OpenGL.Gl.GetUniformLocation (shader, "fontTexture")
            OpenGL.Hl.Assert ()

            // create font texture
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            fonts.GetTexDataAsRGBA32 (&pixels, &fontTextureWidth, &fontTextureHeight, &bytesPerPixel)
            let fontTextureId = OpenGL.Gl.GenTexture ()
            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, fontTextureId)
            OpenGL.Gl.TexImage2D (OpenGL.TextureTarget.Texture2d, 0, Constants.OpenGL.UncompressedTextureFormat, fontTextureWidth, fontTextureHeight, 0, OpenGL.PixelFormat.Rgba, OpenGL.PixelType.UnsignedByte, pixels)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMinFilter, int OpenGL.TextureMinFilter.Nearest)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureMagFilter, int OpenGL.TextureMagFilter.Nearest)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapS, int OpenGL.TextureWrapMode.Repeat)
            OpenGL.Gl.TexParameter (OpenGL.TextureTarget.Texture2d, OpenGL.TextureParameterName.TextureWrapT, int OpenGL.TextureWrapMode.Repeat)
            let fontTextureMetadata = OpenGL.Texture.TextureMetadata.make fontTextureWidth fontTextureHeight
            fontTexture <- OpenGL.Texture.EagerTexture { TextureMetadata = fontTextureMetadata; TextureId = fontTextureId }
            fonts.SetTexID (nativeint fontTexture.TextureId)
            fonts.ClearTexData ()

        member this.Render (drawData : ImDrawDataPtr) =

            // attempt to draw imgui draw data
            let mutable vertexOffsetInVertices = 0
            let mutable indexOffsetInElements = 0
            if drawData.CmdListsCount <> 0 then

                // resize vertex buffer if necessary
                let vertexBufferSizeNeeded = uint (drawData.TotalVtxCount * Unsafe.SizeOf<ImDrawVert>())
                if vertexBufferSizeNeeded > vertexBufferSize then
                    vertexBufferSize <- max (vertexBufferSize * 2u) vertexBufferSizeNeeded
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, vertexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
                    OpenGL.Hl.Assert ()

                // resize index buffer if necessary
                let indexBufferSizeNeeded = uint (drawData.TotalIdxCount * sizeof<uint16>)
                if indexBufferSizeNeeded > indexBufferSize then
                    indexBufferSize <- max (indexBufferSize * 2u) indexBufferSizeNeeded
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, indexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, indexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
                    OpenGL.Hl.Assert ()

                // compute offsets
                let cmdsRange = drawData.CmdListsRange
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmds = cmdsRange.[i]
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, vertexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, nativeint (vertexOffsetInVertices * Unsafe.SizeOf<ImDrawVert> ()), uint (cmds.VtxBuffer.Size * Unsafe.SizeOf<ImDrawVert> ()), cmds.VtxBuffer.Data)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, indexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, nativeint (indexOffsetInElements * sizeof<uint16>), uint (cmds.IdxBuffer.Size * sizeof<uint16>), cmds.IdxBuffer.Data)
                    vertexOffsetInVertices <- vertexOffsetInVertices + cmds.VtxBuffer.Size
                    indexOffsetInElements <- indexOffsetInElements + cmds.IdxBuffer.Size
                    OpenGL.Hl.Assert ()

                // compute orthographic projection
                let projection = Matrix4x4.CreateOrthographicOffCenter (0.0f, single windowWidth, single windowHeight, 0.0f, -1.0f, 1.0f)
                let projectionArray = projection.ToArray ()

                // setup state
                OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
                OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
                OpenGL.Gl.Enable OpenGL.EnableCap.Blend
                OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
                OpenGL.Hl.Assert ()

                // setup vao
                OpenGL.Gl.BindVertexArray vertexArrayObject
                OpenGL.Hl.Assert ()

                // setup shader
                OpenGL.Gl.UseProgram shader
                OpenGL.Gl.UniformMatrix4 (shaderProjectionMatrixUniform, false, projectionArray)
                OpenGL.Gl.Uniform1 (shaderFontTextureUniform, 0) // TODO: use bindless textures for imgui?
                OpenGL.Hl.Assert ()

                // draw command lists
                let mutable vertexOffset = 0
                let mutable indexOffset = 0
                for i in 0 .. dec drawData.CmdListsCount do
                    let cmdsRange = drawData.CmdListsRange
                    let cmds = cmdsRange.[i]
                    for cmd in 0 .. dec cmds.CmdBuffer.Size do
                        let pcmds = cmds.CmdBuffer
                        let pcmd = pcmds.[cmd]
                        if pcmd.UserCallback = nativeint 0 then
                            let clip = pcmd.ClipRect
                            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
                            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, uint pcmd.TextureId)
                            OpenGL.Gl.Scissor (int clip.X, windowHeight - int clip.W, int (clip.Z - clip.X), int (clip.W - clip.Y))
                            OpenGL.Gl.DrawElementsBaseVertex (OpenGL.PrimitiveType.Triangles, int pcmd.ElemCount, OpenGL.DrawElementsType.UnsignedShort, nativeint (indexOffset * sizeof<uint16>), int pcmd.VtxOffset + vertexOffset)
                            OpenGL.Hl.ReportDrawCall 1
                            OpenGL.Hl.Assert ()
                        else raise (NotImplementedException ())
                        indexOffset <- indexOffset + int pcmd.ElemCount
                    vertexOffset <- vertexOffset + cmds.VtxBuffer.Size

                // teardown shader
                OpenGL.Gl.UseProgram 0u
                OpenGL.Hl.Assert ()

                // teardown vao
                OpenGL.Gl.BindVertexArray 0u
                OpenGL.Hl.Assert ()

                // teardown state
                OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
                OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.One, OpenGL.BlendingFactor.Zero)
                OpenGL.Gl.Disable OpenGL.EnableCap.Blend
                OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest

        member this.CleanUp () =

            // destroy vao
            OpenGL.Gl.BindVertexArray vertexArrayObject
            OpenGL.Gl.DeleteBuffers [|vertexBuffer|]
            OpenGL.Gl.DeleteBuffers [|indexBuffer|]
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Gl.DeleteVertexArrays [|vertexArrayObject|]
            OpenGL.Hl.Assert ()

            // destroy shader
            OpenGL.Gl.DeleteProgram shader
            OpenGL.Hl.Assert ()

            // destroy font texture
            fontTexture.Destroy ()

[<RequireQualifiedAccess>]
module GlRendererImGui =

    /// Make a gl-based imgui renderer.
    let make fonts =
        let rendererImGui = GlRendererImGui (Constants.Render.Resolution.X, Constants.Render.Resolution.Y)
        (rendererImGui :> RendererImGui).Initialize fonts
        rendererImGui