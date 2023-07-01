namespace Nu
open System
open System.Collections.Generic
open Prime
open ImGuiNET
open ImGuizmoNET
open Nu

/// Wraps ImGui context, state, and calls.
/// TODO: Make ImGui a mockable interface.
/// NOTE: API is object-oriented / mutation-based because it's ported from a port of a port.
type ImGui (windowWidth : int, windowHeight : int) =

    let charsPressed =
        List<char> ()

    let keyboardKeys =
        Enum.GetValues typeof<KeyboardKey> |>
        enumerable |>
        Seq.map cast<KeyboardKey> |>
        Array.ofSeq

    let context =
        ImGui.CreateContext ()

    do
        // make context current
        ImGui.SetCurrentContext context

        // set guizmo context
        ImGuizmo.SetImGuiContext context

        // enable guizmo
        ImGuizmo.Enable true

        // retrieve configuration targets
        let io = ImGui.GetIO ()
        let keyMap = io.KeyMap
        let fonts = io.Fonts

        // configure the imgui backend to presume the use of vertex offsets (necessary since we're using 16 bit indices)
        io.BackendFlags <- io.BackendFlags ||| ImGuiBackendFlags.RendererHasVtxOffset

        // configure initial display size
        io.DisplaySize <- v2 (single windowWidth) (single windowHeight)

        // configure docking enabled
        io.ConfigFlags <- io.ConfigFlags ||| ImGuiConfigFlags.DockingEnable

        // configure key mappings
        keyMap.[int ImGuiKey.Tab] <- int KeyboardKey.Tab
        keyMap.[int ImGuiKey.LeftArrow] <- int KeyboardKey.Left
        keyMap.[int ImGuiKey.RightArrow] <- int KeyboardKey.Right
        keyMap.[int ImGuiKey.UpArrow] <- int KeyboardKey.Up
        keyMap.[int ImGuiKey.DownArrow] <- int KeyboardKey.Down
        keyMap.[int ImGuiKey.PageUp] <- int KeyboardKey.Pageup
        keyMap.[int ImGuiKey.PageDown] <- int KeyboardKey.Pagedown
        keyMap.[int ImGuiKey.Home] <- int KeyboardKey.Home
        keyMap.[int ImGuiKey.End] <- int KeyboardKey.End
        keyMap.[int ImGuiKey.Delete] <- int KeyboardKey.Delete
        keyMap.[int ImGuiKey.Backspace] <- int KeyboardKey.Backspace
        keyMap.[int ImGuiKey.Enter] <- int KeyboardKey.Return
        keyMap.[int ImGuiKey.Escape] <- int KeyboardKey.Escape
        keyMap.[int ImGuiKey.LeftCtrl] <- int KeyboardKey.Lctrl
        keyMap.[int ImGuiKey.RightCtrl] <- int KeyboardKey.Rctrl
        keyMap.[int ImGuiKey.LeftAlt] <- int KeyboardKey.Lalt
        keyMap.[int ImGuiKey.RightAlt] <- int KeyboardKey.Ralt
        keyMap.[int ImGuiKey.LeftShift] <- int KeyboardKey.Lshift
        keyMap.[int ImGuiKey.RightShift] <- int KeyboardKey.Rshift
        for i in 0 .. dec 10 do keyMap.[int ImGuiKey._1 + i] <- int KeyboardKey.Num1 + i
        for i in 0 .. dec 26 do keyMap.[int ImGuiKey.A + i] <- int KeyboardKey.A + i

        // add default font
        fonts.AddFontDefault () |> ignore<ImFontPtr>

        // configure styling theme to dark
        ImGui.StyleColorsDarkPlus ()

    member this.Fonts =
        let io = ImGui.GetIO ()
        io.Fonts

    member this.HandleMouseWheelChange change =
        let io = ImGui.GetIO ()
        io.MouseWheel <- io.MouseWheel + change

    member this.HandleKeyChar (keyChar : char) =
        charsPressed.Add keyChar

    member this.BeginFrame () =
        ImGui.NewFrame ()
        ImGuizmo.BeginFrame ()

    member this.EndFrame (deltaSeconds : single) =
        let io = ImGui.GetIO ()
        io.DisplaySize <- v2 (single windowWidth) (single windowHeight)
        io.DeltaTime <- deltaSeconds

    member this.InputFrame () =

        // update mouse states
        let io = ImGui.GetIO ()
        let mouseDown = io.MouseDown
        mouseDown.[0] <- MouseState.isButtonDown MouseLeft
        mouseDown.[1] <- MouseState.isButtonDown MouseRight
        mouseDown.[2] <- MouseState.isButtonDown MouseMiddle
        io.MousePos <- MouseState.getPosition ()

        // update keyboard states.
        // NOTE: using modifier detection from sdl since it works better given how things have been configued.
        io.KeyCtrl <- KeyboardState.isCtrlDown ()
        io.KeyAlt <- KeyboardState.isAltDown ()
        io.KeyShift <- KeyboardState.isShiftDown ()
        let keysDown = io.KeysDown
        for keyboardKey in keyboardKeys do
            keysDown.[int keyboardKey] <- KeyboardState.isKeyDown keyboardKey

        // register key char input
        for c in charsPressed do
            io.AddInputCharacter (uint32 c)
        charsPressed.Clear ()

    member this.RenderFrame () =
        ImGui.Render ()
        ImGui.GetDrawData ()

    member this.CleanUp () =
        ImGui.DestroyContext context

    static member StyleColorsDarkPlus () =
        ImGui.StyleColorsDark ()
        let style = ImGui.GetStyle ()
        let colors = style.Colors
        colors.[int ImGuiCol.MenuBarBg] <- v4 0.0f 0.0f 0.0f 0.667f
        colors.[int ImGuiCol.WindowBg] <- v4 0.0f 0.0f 0.0f 0.333f
        colors.[int ImGuiCol.TitleBg] <- v4 0.0f 0.0f 0.0f 0.5f

    static member StyleColorsLightPlus () =
        ImGui.StyleColorsLight ()
        let style = ImGui.GetStyle ()
        let colors = style.Colors
        colors.[int ImGuiCol.MenuBarBg] <- v4 1.0f 1.0f 1.0f 0.667f
        colors.[int ImGuiCol.WindowBg] <- v4 1.0f 1.0f 1.0f 0.333f
        colors.[int ImGuiCol.TitleBg] <- v4 1.0f 1.0f 1.0f 0.5f

    static member IsCtrlPressed () =
        // HACK: using modifier detection from sdl since it works better given how things have been configued.
        KeyboardState.isCtrlDown ()
        //ImGui.IsKeyPressed ImGuiKey.LeftCtrl ||
        //ImGui.IsKeyPressed ImGuiKey.RightCtrl

    static member IsAltPressed () =
        // HACK: using modifier detection from sdl since it works better given how things have been configued.
        KeyboardState.isAltDown ()
        //ImGui.IsKeyPressed ImGuiKey.LeftAlt ||
        //ImGui.IsKeyPressed ImGuiKey.RightAlt

    static member IsShiftPressed () =
        // HACK: using modifier detection from sdl since it works better given how things have been configued.
        KeyboardState.isShiftDown ()
        //ImGui.IsKeyPressed ImGuiKey.LeftShift ||
        //ImGui.IsKeyPressed ImGuiKey.RightShift

    static member IsCtrlPlusKeyPressed (key : ImGuiKey) =
        ImGui.IsCtrlPressed () && ImGui.IsKeyPressed key