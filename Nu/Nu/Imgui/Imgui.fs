namespace rescuePars.GUI
open ImGuiNET
open System
open System.Collections.Generic
open System.Numerics
open System.Runtime.CompilerServices
open Prime
open Nu

    /// <summary>
    /// A modified version of Veldrid.ImGui's ImGuiRenderer.
    /// Manages input for ImGui and handles rendering ImGui's DrawLists with Veldrid.
    /// </summary>
    type ImGuiController (initialWidth : int, initialHeight : int) as this =

        let mutable _windowWidth = initialWidth
        let mutable _windowHeight = initialHeight
        let mutable _frameBegun = true
        let mutable _vertexArray = 0u
        let mutable _vertexBuffer = 0u
        let mutable _vertexBufferSize = 0u
        let mutable _indexBuffer = 0u
        let mutable _indexBufferSize = 0u
        let mutable _fontTexture : Texture = null
        let mutable _guiShader : GUIShader = null
        let mutable _windowWidth = initialWidth
        let mutable _windowHeight = initialHeight
        let mutable _scaleFactor = Vector2.One
        let mutable PrevMouseState : MouseState = null
        let mutable PrevKeyboardState : KeyboardState = null
        let PressedChars = List<char> ()
        do  let context = ImGui.CreateContext ()
            ImGui.SetCurrentContext context
            let io = ImGui.GetIO ()
            io.Fonts.AddFontDefault () |> ignore
            io.BackendFlags <- io.BackendFlags ||| ImGuiBackendFlags.RendererHasVtxOffset
            this.CreateDeviceResources ()
            this.SetKeyMappings ()
            this.SetPerFrameImGuiData (1f / 60f)
            ImGui.NewFrame ()

        member this.WindowResized (width, height) =
            _windowWidth <- width
            _windowHeight <- height

        member this.DestroyDeviceObjects () =
            this.Dispose ()

        member this.CreateDeviceResources () =

            Util.CreateVertexArray ("ImGui", out _vertexArray)

            _vertexBufferSize <- 10000u
            _indexBufferSize <- 2000u

            Util.CreateVertexBuffer ("ImGui", out _vertexBuffer)
            Util.CreateElementBuffer ("ImGui", out _indexBuffer)

            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _vertexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, _vertexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _indexBuffer)
            OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, _indexBufferSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)

            this.RecreateFontDeviceTexture ()

            let VertexSource =
                [Constants.OpenGl.GlslVersionPragma
                 ""
                 "uniform mat4 projection_matrix;"
                 ""
                 "layout(location = 0) in vec2 in_position;"
                 "layout(location = 1) in vec2 in_texCoord;"
                 "layout(location = 2) in vec4 in_color;"
                 ""
                 "out vec4 color;"
                 "out vec2 texCoord;"
                 ""
                 "void main()"
                 "{"
                 "    gl_Position = projection_matrix * vec4(in_position, 0, 1);"
                 "    color = in_color;"
                 "    texCoord = in_texCoord;"
                 "}"] |> String.join "\n"

            let FragmentSource =
                [Constants.OpenGl.GlslVersionPragma
                 ""
                 "uniform sampler2D in_fontTexture;"
                 ""
                 "in vec4 color;"
                 "in vec2 texCoord;"
                 ""
                 "out vec4 outputColor;"
                 ""
                 "void main()"
                 "{"
                 "    outputColor = color * texture(in_fontTexture, texCoord);"
                 "}"] |> String.join "\n"

            _guiShader = GUIShader ("ImGui", VertexSource, FragmentSource)

            OpenGL.Gl.BindVertexArray _vertexArray

            OpenGL.Gl.BindBuffer(OpenGL.BufferTarget.ArrayBuffer, _vertexBuffer)
            OpenGL.Gl.BindBuffer(OpenGL.BufferTarget.ElementArrayBuffer, _indexBuffer)

            let stride = Unsafe.SizeOf<ImDrawVert> ()

            OpenGL.Gl.EnableVertexAttribArray 0u
            OpenGL.Gl.VertexAttribPointer (0u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 0)
            OpenGL.Gl.EnableVertexAttribArray 1u
            OpenGL.Gl.VertexAttribPointer (1u, 2, OpenGL.VertexAttribPointerType.Float, false, stride, 8)
            OpenGL.Gl.EnableVertexAttribArray 2u
            OpenGL.Gl.VertexAttribPointer (2u, 4, OpenGL.VertexAttribPointerType.UnsignedByte, true, stride, 16)
            OpenGL.Gl.BindVertexArray 0u
            OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0u)
            // We don't need to unbind the element buffer as that is connected to the vertex array
            // And you should not touch the element buffer when there is no vertex array bound.

            Util.CheckGLError("End of ImGui setup")

        /// Recreates the device texture used to render text.
        member this.RecreateFontDeviceTexture () =
            let io = ImGui.GetIO ()
            let mutable pixels = Unchecked.defaultof<nativeint>
            let mutable width = Unchecked.defaultof<_>
            let mutable height = Unchecked.defaultof<_>
            let mutable bytesPerPixel = Unchecked.defaultof<_>
            io.Fonts.GetTexDataAsRGBA32 (&pixels, &width, &height, &bytesPerPixel)
            _fontTexture = Texture ("ImGui Text Atlas", width, height, pixels)
            _fontTexture.SetMagFilter TextureMagFilter.Linear
            _fontTexture.SetMinFilter TextureMinFilter.Linear
            io.Fonts.SetTexID (nativeint _fontTexture.GLTexture)
            io.Fonts.ClearTexData ()

        /// Renders the ImGui draw list data.
        /// This method requires a <see cref="GraphicsDevice"/> because it may create new DeviceBuffers if the size of vertex
        /// or index data has increased beyond the capacity of the existing buffers.
        /// A <see cref="CommandList"/> is needed to submit drawing and resource update commands.
        member this.Render () =
            if  _frameBegun then
                _frameBegun <- false
                ImGui.Render ()
                this.RenderImDrawData (ImGui.GetDrawData ())
                Util.CheckGLError "Imgui Controller"

        /// Updates ImGui input and IO configuration state.
        member this.Update (wnd : GameWindow, deltaSeconds : single) =
            if _frameBegun then ImGui.Render ()
            this.SetPerFrameImGuiData deltaSeconds
            this.UpdateImGuiInput wnd
            _frameBegun <- true
            ImGui.NewFrame ()

        /// Sets per-frame data based on the associated window.
        /// This is called by Update(float).
        member this.SetPerFrameImGuiData (deltaSeconds : single) =
            let io = ImGui.GetIO ()
            io.DisplaySize <- v2 (_windowWidth / _scaleFactor.X) (_windowHeight / _scaleFactor.Y)
            io.DisplayFramebufferScale <- _scaleFactor
            io.DeltaTime <- deltaSeconds

        member this.UpdateImGuiInput (wnd : GameWindow) =

            let io = ImGui.GetIO ()

            let MouseState = Mouse.GetCursorState ()
            let KeyboardState = Keyboard.GetState ()

            io.MouseDown.[0] <- MouseState.LeftButton = ButtonState.Pressed
            io.MouseDown.[1] <- MouseState.RightButton = ButtonState.Pressed
            io.MouseDown.[2] <- MouseState.MiddleButton = ButtonState.Pressed

            let screenPoint = System.Drawing.Point (MouseState.X, MouseState.Y)
            let point = wnd.PointToClient screenPoint
            io.MousePos <- v2 (single point.X) (single point.Y)
            io.MouseWheel <- MouseState.Scroll.Y - PrevMouseState.Scroll.Y
            io.MouseWheelH <- MouseState.Scroll.X - PrevMouseState.Scroll.X

            for key in Enum.GetValues typeof<Key> do
                io.KeysDown.[int key] <- KeyboardState.IsKeyDown key

            for c in PressedChars do
                io.AddInputCharacter (uint32 c)

            PressedChars.Clear ()

            io.KeyCtrl <- KeyboardState.IsKeyDown Key.ControlLeft || KeyboardState.IsKeyDown Key.ControlRight
            io.KeyAlt <- KeyboardState.IsKeyDown Key.AltLeft || KeyboardState.IsKeyDown Key.AltRight
            io.KeyShift <- KeyboardState.IsKeyDown Key.ShiftLeft || KeyboardState.IsKeyDown Key.ShiftRight
            io.KeySuper <- KeyboardState.IsKeyDown Key.WinLeft || KeyboardState.IsKeyDown Key.WinRight

            PrevMouseState <- MouseState
            PrevKeyboardState <- KeyboardState

        member this.PressChar (keyChar : char) =
            PressedChars.Add keyChar

        member private this.SetKeyMappings () =
            let io = ImGui.GetIO ()
            io.KeyMap.[int ImGuiKey.Tab] <- int Key.Tab
            io.KeyMap.[int ImGuiKey.LeftArrow] <- int Key.Left
            io.KeyMap.[int ImGuiKey.RightArrow] <- int Key.Right
            io.KeyMap.[int ImGuiKey.UpArrow] <- int Key.Up
            io.KeyMap.[int ImGuiKey.DownArrow] <- int Key.Down
            io.KeyMap.[int ImGuiKey.PageUp] <- int Key.PageUp
            io.KeyMap.[int ImGuiKey.PageDown] <- int Key.PageDown
            io.KeyMap.[int ImGuiKey.Home] <- int Key.Home
            io.KeyMap.[int ImGuiKey.End] <- int Key.End
            io.KeyMap.[int ImGuiKey.Delete] <- int Key.Delete
            io.KeyMap.[int ImGuiKey.Backspace] <- int Key.BackSpace
            io.KeyMap.[int ImGuiKey.Enter] <- int Key.Enter
            io.KeyMap.[int ImGuiKey.Escape] <- int Key.Escape
            io.KeyMap.[int ImGuiKey.A] <- int Key.A
            io.KeyMap.[int ImGuiKey.C] <- int Key.C
            io.KeyMap.[int ImGuiKey.V] <- int Key.V
            io.KeyMap.[int ImGuiKey.X] <- int Key.X
            io.KeyMap.[int ImGuiKey.Y] <- int Key.Y
            io.KeyMap.[int ImGuiKey.Z] <- int Key.Z

        member this.RenderImDrawData (draw_data : ImDrawDataPtr) =

            let mutable vertexOffsetInVertices = 0
            let mutable indexOffsetInElements = 0

            if draw_data.CmdListsCount <> 0 then

                let totalVBSize = uint (draw_data.TotalVtxCount * Unsafe.SizeOf<ImDrawVert>())
                if totalVBSize > _vertexBufferSize then
                    let newSize =  int (Math.Max (_vertexBufferSize * 1.5f, totalVBSize))
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _vertexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, newSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0)
                    _vertexBufferSize <- newSize
                    Console.WriteLine $"Resized vertex buffer to new size {_vertexBufferSize}"

                let totalIBSize = uint (draw_data.TotalIdxCount * sizeof(ushort))
                if totalIBSize > _indexBufferSize then
                    let newSize = int (Math.Max (_indexBufferSize * 1.5f, totalIBSize))
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _indexBuffer)
                    OpenGL.Gl.BufferData (OpenGL.BufferTarget.ArrayBuffer, newSize, nativeint 0, OpenGL.BufferUsage.DynamicDraw)
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0)
                    _indexBufferSize <- newSize
                    Console.WriteLine $"Resized index buffer to new size {_indexBufferSize}"


                for i in 0 .. dec draw_data.CmdListsCount do
                    let cmd_list = draw_data.CmdListsRange.[i]
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _vertexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, (IntPtr)(vertexOffsetInVertices * Unsafe.SizeOf<ImDrawVert>()), cmd_list.VtxBuffer.Size * Unsafe.SizeOf<ImDrawVert>(), cmd_list.VtxBuffer.Data);
                    Util.CheckGLError $"Data Vert {i}"
                    OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, _indexBuffer)
                    OpenGL.Gl.BufferSubData (OpenGL.BufferTarget.ArrayBuffer, (IntPtr)(indexOffsetInElements * sizeof(ushort)), cmd_list.IdxBuffer.Size * sizeof(ushort), cmd_list.IdxBuffer.Data);
                    Util.CheckGLError $"Data Idx {i}"
                    vertexOffsetInVertices <- vertexOffsetInVertices + uint cmd_list.VtxBuffer.Size
                    indexOffsetInElements <- indexOffsetInElements + uint cmd_list.IdxBuffer.Size

                OpenGL.Gl.BindBuffer (OpenGL.BufferTarget.ArrayBuffer, 0)

                // Setup orthographic projection matrix into our constant buffer
                let io = ImGui.GetIO ()
                let mvp = Matrix4x4.CreateOrthographicOffCenter (0.0f, io.DisplaySize.X, io.DisplaySize.Y, 0.0f, -1.0f, 1.0f)

                _guiShader.UseShader ()
                OpenGL.Gl.UniformMatrix4 (_guiShader.GetUniformLocation "projection_matrix", false, ref mvp)
                OpenGL.Gl.Uniform1 (_guiShader.GetUniformLocation"in_fontTexture", 0)
                Util.CheckGLError "Projection"

                OpenGL.Gl.BindVertexArray _vertexArray
                Util.CheckGLError "VAO"

                draw_data.ScaleClipRects io.DisplayFramebufferScale

                OpenGL.Gl.Enable OpenGL.EnableCap.Blend
                OpenGL.Gl.Enable OpenGL.EnableCap.ScissorTest
                OpenGL.Gl.BlendEquation OpenGL.BlendEquationMode.FuncAdd
                OpenGL.Gl.BlendFunc (OpenGL.BlendingFactor.SrcAlpha, OpenGL.BlendingFactor.OneMinusSrcAlpha)
                OpenGL.Gl.Disable OpenGL.EnableCap.CullFace
                OpenGL.Gl.Disable OpenGL.EnableCap.DepthTest

                // Render command lists
                let mutable vtx_offset = 0
                let mutable idx_offset = 0
                for n in 0 .. dec draw_data.CmdListsCount do

                    let cmd_list = draw_data.CmdListsRange.[n]
                    for cmd_i in 0 .. dec cmd_list.CmdBuffer.Size do

                        let pcmd = cmd_list.CmdBuffer.[cmd_i]
                        if pcmd.UserCallback <> nativeint 0 then
                            raise (NotImplementedException ())
                        else

                            OpenGL.Gl.ActiveTexture OpenGL.TextureUnit.Texture0
                            OpenGL.Gl.BindTexture (OpenGL.TextureTarget.Texture2d, int pcmd.TextureId)
                            Util.CheckGLError "Texture"

                            // We do _windowHeight - (int)clip.W instead of (int)clip.Y because gl has flipped Y when it comes to these coordinates
                            let clip = pcmd.ClipRect
                            OpenGL.Gl.Scissor(int clip.X, _windowHeight - int clip.W, int (clip.Z - clip.X), int (clip.W - clip.Y))
                            Util.CheckGLError "Scissor"

                            OpenGL.Gl.DrawElementsBaseVertex(OpenGL.PrimitiveType.Triangles, int pcmd.ElemCount, OpenGL.DrawElementsType.UnsignedShort, (IntPtr)(idx_offset * sizeof(ushort)), vtx_offset);
                            Util.CheckGLError "Draw"

                        idx_offset <- idx_offset + int pcmd.ElemCount
                    vtx_offset <- vtx_offset + cmd_list.VtxBuffer.Size

                OpenGL.Gl.Disable OpenGL.EnableCap.Blend
                OpenGL.Gl.Disable OpenGL.EnableCap.ScissorTest

        /// Frees all graphics resources used by the renderer.
        member this.Dispose () =
            _fontTexture.Dispose ()
            _guiShader.Dispose ()