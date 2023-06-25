namespace rescuePars.GUI
open System
open System.Collections.Generic
open System.Numerics
open ImGuiNET
open Nu

    ///
    /// Code is shite because it's ported from a port of a port...
    type ImGuiController (windowWidth : int, windowHeight : int) as this =

        let mutable scaleFactor = Vector2.One
        let mutable mouseStateOld : MouseState = null
        let mutable keyboardStateOld : KeyboardState = null
        let charsPressed = List<char> ()
        do  let context = ImGui.CreateContext ()
            ImGui.SetCurrentContext context
            let io = ImGui.GetIO ()
            let fonts = io.Fonts
            fonts.AddFontDefault () |> ignore
            io.BackendFlags <- io.BackendFlags ||| ImGuiBackendFlags.RendererHasVtxOffset
            this.SetKeyMappings ()
            this.SetPerFrameImGuiData (1f / 60f)
            ImGui.NewFrame ()

        member this.Render (drawData : ImDrawDataPtr) =
            ImGui.Render ()
            ImGui.GetDrawData ()

        member this.NewFrame () =
            ImGui.NewFrame ()

        member this.SetPerFrameImGuiData (deltaSeconds : single) =
            let io = ImGui.GetIO ()
            io.DisplaySize <- v2 (single windowWidth / scaleFactor.X) (single windowHeight / scaleFactor.Y)
            io.DisplayFramebufferScale <- scaleFactor
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
            io.MouseWheel <- MouseState.Scroll.Y - mouseStateOld.Scroll.Y
            io.MouseWheelH <- MouseState.Scroll.X - mouseStateOld.Scroll.X

            for key in Enum.GetValues typeof<Key> do
                io.KeysDown.[int key] <- KeyboardState.IsKeyDown key

            for c in charsPressed do
                io.AddInputCharacter (uint32 c)

            charsPressed.Clear ()

            io.KeyCtrl <- KeyboardState.IsKeyDown Key.ControlLeft || KeyboardState.IsKeyDown Key.ControlRight
            io.KeyAlt <- KeyboardState.IsKeyDown Key.AltLeft || KeyboardState.IsKeyDown Key.AltRight
            io.KeyShift <- KeyboardState.IsKeyDown Key.ShiftLeft || KeyboardState.IsKeyDown Key.ShiftRight
            io.KeySuper <- KeyboardState.IsKeyDown Key.WinLeft || KeyboardState.IsKeyDown Key.WinRight

            mouseStateOld <- MouseState
            keyboardStateOld <- KeyboardState

        member this.PressChar (keyChar : char) =
            charsPressed.Add keyChar

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