// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open ImGuiNET
open ImGuizmoNET
open ImPlotNET
open Prime

/// The result of an imgui editing operation.
type ImGuiEditResult =
    | ImGuiEditActive of Started : bool
    | ImGuiEditInactive

/// Wraps ImGui context, state, and calls. Also extends the ImGui interface with static methods.
/// NOTE: API is primarily object-oriented / mutation-based because it's ported from a port.
type ImGui (stub : bool, displaySize : Vector2i) =

    static let mutable MouseLeftIdInternal = 0L

    let charsPressed =
        List<char> ()

    let context =
        ImGui.CreateContext ()

    let plot =
        ImPlot.CreateContext ()

    do
        // make context current
        ImGui.SetCurrentContext context

        // set guizmo context
        ImGuizmo.SetImGuiContext context

        // configure guizmo settings
        ImGuizmo.AllowAxisFlip false
        ImGuizmo.SetAxisLimit 0.001f
        ImGuizmo.SetPlaneLimit 0.001f

        // enable guizmo
        ImGuizmo.Enable true

        // set plot context
        ImPlot.SetImGuiContext context

        // retrieve configuration targets
        let io = ImGui.GetIO ()
        let fonts = io.Fonts

        // configure imgui error handling to NOT crash the .NET Runtime!
        io.ConfigErrorRecoveryEnableAssert <- false

        // configure the imgui backend to presume the use of vertex offsets (necessary since we're using 16 bit indices)
        io.BackendFlags <- io.BackendFlags ||| ImGuiBackendFlags.RendererHasVtxOffset

        // configure initial display size
        io.DisplaySize <- displaySize.V2

        // configure docking enabled
        io.ConfigFlags <- io.ConfigFlags ||| ImGuiConfigFlags.DockingEnable

        // add default font
        fonts.AddFontDefault () |> ignore<ImFontPtr>

        // configure styling theme to nu
        ImGui.StyleColorsNu ()

    static member MouseLeftId =
        MouseLeftIdInternal

    static member MainViewportCenter =
        let mainViewport = ImGui.GetMainViewport ()
        mainViewport.GetCenter ()

    member this.Fonts =
        let io = ImGui.GetIO ()
        io.Fonts

    member this.HandleMouseWheelChange change =
        let io = ImGui.GetIO ()
        io.MouseWheel <- io.MouseWheel + change

    member this.HandleKeyChar (keyChar : char) =
        charsPressed.Add keyChar

    member this.BeginFrame deltaTime =
        if not stub then
            let io = ImGui.GetIO ()
            let deltaTimeBounded =
                if deltaTime <= 0.001f || deltaTime >= 0.1f // when time is close to zero or negative (such as with undo), suppose default time delta
                then 1.0f / 60.0f
                else deltaTime
            io.DeltaTime <- deltaTimeBounded
            ImGui.NewFrame ()
            ImGuiIOPtr.BeginFrame ()
            ImGuizmo.BeginFrame ()
        if ImGui.IsMouseClicked ImGuiMouseButton.Left then
            MouseLeftIdInternal <- inc MouseLeftIdInternal

    member this.EndFrame () =
        () // nothing to do

    member this.InputFrame () =
        let io = ImGui.GetIO ()
        for c in charsPressed do
            io.AddInputCharacter (uint32 c)
        charsPressed.Clear ()

    member this.RenderFrame () =
        if not stub then ImGui.Render ()
        ImGui.GetDrawData ()

    member this.CleanUp () =
        ImPlot.DestroyContext plot
        ImGuizmo.Enable false // NOTE: guessing that this is how imguizmo is torn down...
        ImGui.DestroyContext context

    static member StyleColorsNu () =
        ImGui.StyleColorsDark ()
        let style = ImGui.GetStyle ()
        let colors = style.Colors
        colors.[int ImGuiCol.MenuBarBg] <- v4 0.0f 0.0f 0.0f 0.5f
        colors.[int ImGuiCol.TitleBg] <- v4 0.0f 0.0f 0.0f 0.5f
        colors.[int ImGuiCol.WindowBg] <- v4 0.0f 0.0f 0.0f 0.333f

    static member IsKeyUp key =
        not (ImGui.IsKeyDown key)

    static member IsEnterDown () =
        ImGui.IsKeyDown ImGuiKey.Enter ||
        ImGui.IsKeyDown ImGuiKey.KeypadEnter

    static member IsEnterUp () =
        ImGui.IsKeyUp ImGuiKey.Enter ||
        ImGui.IsKeyUp ImGuiKey.KeypadEnter

    static member IsCtrlDown () =
        ImGui.IsKeyDown ImGuiKey.LeftCtrl ||
        ImGui.IsKeyDown ImGuiKey.RightCtrl

    static member IsCtrlUp () =
        not (ImGui.IsCtrlDown ())

    static member IsAltDown () =
        ImGui.IsKeyDown ImGuiKey.LeftAlt ||
        ImGui.IsKeyDown ImGuiKey.RightAlt

    static member IsAltUp () =
        not (ImGui.IsAltDown ())

    static member IsShiftDown () =
        ImGui.IsKeyDown ImGuiKey.LeftShift ||
        ImGui.IsKeyDown ImGuiKey.RightShift

    static member IsShiftUp () =
        not (ImGui.IsShiftDown ())

    static member IsCtrlPlusKeyPressed (key : ImGuiKey) =
        ImGui.IsCtrlDown () && ImGui.IsKeyPressed key

    static member Position2dToWindow (absolute, eyeCenter, eyeSize : Vector2, viewport, position) =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        if absolute
        then position * virtualScalar * v2 1.0f -1.0f + eyeSize * 0.5f * virtualScalar
        else position * virtualScalar * v2 1.0f -1.0f - eyeCenter * virtualScalar + eyeSize * 0.5f * virtualScalar

    static member WindowToPosition2d (absolute, eyeCenter, eyeSize : Vector2, viewport, position) =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        if absolute
        then position / virtualScalar * v2 1.0f -1.0f - eyeSize * 0.5f * virtualScalar
        else position / virtualScalar * v2 1.0f -1.0f + eyeCenter * virtualScalar - eyeSize * 0.5f * virtualScalar

    // OPTIMIZATION: requiring window position and size to be passed in so that expensive calls to them not need be repeatedly made.
    // TODO: the calling convention here is very inconsistent with Position2dToWindow, so let's see if we can converge them.
    static member Position3dToWindow (windowPosition : Vector2, windowSize : Vector2, modelViewProjection : Matrix4x4, position : Vector3) =

        // transform the position from world coordinates to clip space coordinates
        let mutable position = (Vector4 (position, 1.0f)).Transform modelViewProjection
        position <- position * 0.5f / position.W

        // transform the position from normalized device coordinates to window coordinates
        position <- position + v4 0.5f 0.5f 0.0f 0.0f
        position.Y <- 1.0f - position.Y
        position.X <- position.X * windowSize.X
        position.Y <- position.Y * windowSize.Y

        // adjust the position to be relative to the window
        position.X <- position.X + windowPosition.X
        position.Y <- position.Y + windowPosition.Y
        v2 position.X position.Y

    // OPTIMIZATION: requiring window position and size to be passed in so that expensive calls to them not need be repeatedly made.
    // TODO: the calling convention here is very inconsistent with WindowToPosition2d, so let's see if we can converge them.
    static member WindowToPosition3d (windowPosition : Vector2, windowSize : Vector2, model : Matrix4x4, view : Matrix4x4, projection : Matrix4x4) =

        // grab dependencies
        let io = ImGui.GetIO ()

        // map mouse position from window coordinates to normalized device coordinates
        let mouseXNdc = ((io.MousePos.X - windowPosition.X) / windowSize.X) * 2.0f - 1.0f
        let mouseYNdc = (1.0f - ((io.MousePos.Y - windowPosition.Y) / windowSize.Y)) * 2.0f - 1.0f

        // transform near and far positions of the clip space to world coordinates
        let nearPos = (v4 0.0f 0.0f 1.0f 1.0f).Transform projection
        let farPos = (v4 0.0f 0.0f 2.0f 1.0f).Transform projection

        // determine if the near and far planes are reversed
        let reversed = nearPos.Z / nearPos.W > farPos.Z / farPos.W

        // set the near and far clip distances accordingly based on the reversed flag
        let (zNear, zFar) = if reversed then (1.0f - 0.0001f, 0.0f) else (0.0f, 1.0f - 0.0001f)

        // calculate the ray origin in world coordinates by transforming the normalized device coordinates
        let modelViewProjectionInverse = (model * view * projection).Inverted
        let mutable rayOrigin = (v4 mouseXNdc mouseYNdc zNear 1.0f).Transform modelViewProjectionInverse
        rayOrigin <- rayOrigin * (1.0f / rayOrigin.W)

        // calculate the ray end in world coordinates by transforming the normalized device coordinates
        let mutable rayEnd = (v4 mouseXNdc mouseYNdc zFar 1.0f).Transform modelViewProjectionInverse
        rayEnd <- rayEnd * (1.0f / rayEnd.W)

        // calculate the ray direction by normalizing the vector between the ray end and ray origin
        let rayDir = (rayEnd.V3 - rayOrigin.V3).Normalized
        (rayOrigin.V3, rayDir)