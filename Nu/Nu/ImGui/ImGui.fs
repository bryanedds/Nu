namespace Nu
open System
open System.Collections.Generic
open Prime
open ImGuiNET
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
        keyMap.[int ImGuiKey.A] <- int KeyboardKey.A
        keyMap.[int ImGuiKey.C] <- int KeyboardKey.C
        keyMap.[int ImGuiKey.V] <- int KeyboardKey.V
        keyMap.[int ImGuiKey.X] <- int KeyboardKey.X
        keyMap.[int ImGuiKey.Y] <- int KeyboardKey.Y
        keyMap.[int ImGuiKey.Z] <- int KeyboardKey.Z

        // add default font
        fonts.AddFontDefault () |> ignore<ImFontPtr>

        // configure styling theme to light
        ImGui.StyleColorsLight ()

    member this.Fonts =
        let io = ImGui.GetIO ()
        io.Fonts

    member this.HandleKeyChar (keyChar : char) =
        charsPressed.Add keyChar

    member this.BeginFrame () =
        ImGui.NewFrame ()

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

        // update keyboard states
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

[<RequireQualifiedAccess>]
module ImGui =

    let IsCtrlPressed () =
        ImGui.IsKeyPressed ImGuiKey.LeftCtrl ||
        ImGui.IsKeyPressed ImGuiKey.RightCtrl

    let IsAltPressed () =
        ImGui.IsKeyPressed ImGuiKey.LeftAlt ||
        ImGui.IsKeyPressed ImGuiKey.RightAlt

    let IsShiftPressed () =
        ImGui.IsKeyPressed ImGuiKey.LeftShift ||
        ImGui.IsKeyPressed ImGuiKey.RightShift

    let IsCtrlPlusKeyPressed (key : ImGuiKey) =
        IsCtrlPressed () && ImGui.IsKeyPressed key