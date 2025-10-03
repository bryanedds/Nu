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

    static let mutable Font = Unchecked.defaultof<ImFontPtr>
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

        // add default font with the configured font size
        let fontConfig = ImGuiNative.ImFontConfig_ImFontConfig ()
        let fontConfigPtr = ImFontConfigPtr fontConfig
        fontConfigPtr.SizePixels <- Constants.ImGui.FontSize
        Font <- fonts.AddFontDefault fontConfigPtr

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

    static member StyleAdobeInspired (alpha : bool) =

        // use default dark style as base (in case any styling is missed)
        ImGui.StyleColorsDark ()

        // AdobeInspired stylenexacopic from ImThemes (w/ Nu-specific modifications for alpha UI coloring)
        let style = ImGui.GetStyle ()

        // styling
        style.Alpha <- 1.0f
        style.DisabledAlpha <- 0.6000000238418579f
        style.WindowPadding <- Vector2 (8.0f, 8.0f)
        style.WindowRounding <- 4.0f
        style.WindowBorderSize <- 1.0f
        style.WindowMinSize <- Vector2 (32.0f, 32.0f)
        style.WindowTitleAlign <- Vector2 (0.0f, 0.5f)
        style.WindowMenuButtonPosition <- ImGuiDir.None
        style.ChildRounding <- 4.0f
        style.ChildBorderSize <- 1.0f
        style.PopupRounding <- 4.0f
        style.PopupBorderSize <- 1.0f
        style.FramePadding <- Vector2 (4.0f, 3.0f)
        style.FrameRounding <- 4.0f
        style.FrameBorderSize <- 1.0f
        style.ItemSpacing <- Vector2 (8.0f, 4.0f)
        style.ItemInnerSpacing <- Vector2 (4.0f, 4.0f)
        style.CellPadding <- Vector2 (4.0f, 2.0f)
        style.IndentSpacing <- 21.0f
        style.ColumnsMinSpacing <- 6.0f
        style.ScrollbarSize <- 14.0f
        style.ScrollbarRounding <- 4.0f
        style.GrabMinSize <- 10.0f
        style.GrabRounding <- 20.0f
        style.TabRounding <- 4.0f
        style.TabBorderSize <- 1.0f
        style.TabMinWidthForCloseButton <- 0.0f
        style.ColorButtonPosition <- ImGuiDir.Right
        style.ButtonTextAlign <- Vector2 (0.5f, 0.5f)
        style.SelectableTextAlign <- Vector2 (0.0f, 0.0f)

        // colors
        let colors = style.Colors
        colors.[int ImGuiCol.Text] <- Vector4 (1.0f, 1.0f, 1.0f, 1.0f)
        colors.[int ImGuiCol.TextDisabled] <- Vector4 (0.4980392158031464f, 0.4980392158031464f, 0.4980392158031464f, 1.0f)
        colors.[int ImGuiCol.WindowBg] <- Vector4 (0.1137254908680916f, 0.1137254908680916f, 0.1137254908680916f, if alpha then 0.25f else 1.0f)
        colors.[int ImGuiCol.ChildBg] <- Vector4 (0.0f, 0.0f, 0.0f, 0.0f)
        colors.[int ImGuiCol.PopupBg] <- Vector4 (0.0784313753247261f, 0.0784313753247261f, 0.0784313753247261f, 0.9399999976158142f)
        colors.[int ImGuiCol.Border] <- Vector4 (1.0f, 1.0f, 1.0f, 0.1630901098251343f)
        colors.[int ImGuiCol.BorderShadow] <- Vector4 (0.0f, 0.0f, 0.0f, 0.0f)
        colors.[int ImGuiCol.FrameBg] <- Vector4 (0.08627451211214066f, 0.08627451211214066f, 0.08627451211214066f, if alpha then 0.5f else 1.0f)
        colors.[int ImGuiCol.FrameBgHovered] <- Vector4 (0.1529411822557449f, 0.1529411822557449f, 0.1529411822557449f, 1.0f)
        colors.[int ImGuiCol.FrameBgActive] <- Vector4 (0.1882352977991104f, 0.1882352977991104f, 0.1882352977991104f, 1.0f)
        colors.[int ImGuiCol.TitleBg] <- Vector4 (0.1137254908680916f, 0.1137254908680916f, 0.1137254908680916f, if alpha then 0.5f else 1.0f)
        colors.[int ImGuiCol.TitleBgActive] <- Vector4 (0.105882354080677f, 0.105882354080677f, 0.105882354080677f, 1.0f)
        colors.[int ImGuiCol.TitleBgCollapsed] <- Vector4 (0.0f, 0.0f, 0.0f, 0.5099999904632568f)
        colors.[int ImGuiCol.MenuBarBg] <- Vector4 (0.1137254908680916f, 0.1137254908680916f, 0.1137254908680916f, if alpha then 0.5f else 1.0f)
        colors.[int ImGuiCol.ScrollbarBg] <- Vector4 (0.01960784383118153f, 0.01960784383118153f, 0.01960784383118153f, 0.5299999713897705f)
        colors.[int ImGuiCol.ScrollbarGrab] <- Vector4 (0.3098039329051971f, 0.3098039329051971f, 0.3098039329051971f, 1.0f)
        colors.[int ImGuiCol.ScrollbarGrabHovered] <- Vector4 (0.407843142747879f, 0.407843142747879f, 0.407843142747879f, 1.0f)
        colors.[int ImGuiCol.ScrollbarGrabActive] <- Vector4 (0.5098039507865906f, 0.5098039507865906f, 0.5098039507865906f, 1.0f)
        colors.[int ImGuiCol.CheckMark] <- Vector4 (1.0f, 1.0f, 1.0f, 1.0f)
        colors.[int ImGuiCol.SliderGrab] <- Vector4 (0.8784313797950745f, 0.8784313797950745f, 0.8784313797950745f, 1.0f)
        colors.[int ImGuiCol.SliderGrabActive] <- Vector4 (0.9803921580314636f, 0.9803921580314636f, 0.9803921580314636f, 1.0f)
        colors.[int ImGuiCol.Button] <- Vector4 (0.2980392277240754f, 0.2980392277240754f, 0.2980392277240754f, 0.6015625f)
        colors.[int ImGuiCol.ButtonHovered] <- Vector4 (0.494117647409439f, 0.494117647409439f, 0.494117647409439f, 0.6015625f)
        colors.[int ImGuiCol.ButtonActive] <- Vector4 (0.658823549747467f, 0.658823549747467f, 0.658823549747467f, 0.6015625f)
        colors.[int ImGuiCol.Header] <- Vector4 (0.9764705896377563f, 0.9764705896377563f, 0.9764705896377563f, 0.3098039329051971f)
        colors.[int ImGuiCol.HeaderHovered] <- Vector4 (0.9764705896377563f, 0.9764705896377563f, 0.9764705896377563f, 0.3098039329051971f)
        colors.[int ImGuiCol.HeaderActive] <- Vector4 (0.9764705896377563f, 0.9764705896377563f, 0.9764705896377563f, 0.5f)
        colors.[int ImGuiCol.Separator] <- Vector4 (0.4274509847164154f, 0.4274509847164154f, 0.4980392158031464f, 0.5f)
        colors.[int ImGuiCol.SeparatorHovered] <- Vector4 (0.7490196228027344f, 0.7490196228027344f, 0.7490196228027344f, 0.7803921699523926f)
        colors.[int ImGuiCol.SeparatorActive] <- Vector4 (0.7490196228027344f, 0.7490196228027344f, 0.7490196228027344f, 1.0f)
        colors.[int ImGuiCol.ResizeGrip] <- Vector4 (0.9764705896377563f, 0.9764705896377563f, 0.9764705896377563f, 0.2000000029802322f)
        colors.[int ImGuiCol.ResizeGripHovered] <- Vector4 (0.9372549057006836f, 0.9372549057006836f, 0.9372549057006836f, 0.6705882549285889f)
        colors.[int ImGuiCol.ResizeGripActive] <- Vector4 (0.9764705896377563f, 0.9764705896377563f, 0.9764705896377563f, 0.9490196108818054f)
        colors.[int ImGuiCol.Tab] <- Vector4 (0.2235294133424759f, 0.2235294133424759f, 0.2235294133424759f, 0.8627451062202454f)
        colors.[int ImGuiCol.TabDimmed] <- Vector4 (0.2745098173618317f, 0.2745098173618317f, 0.2745098173618317f, 1.0f)
        colors.[int ImGuiCol.TabDimmedSelected] <- Vector4 (0.1450980454683304f, 0.1450980454683304f, 0.1450980454683304f, 0.9725490212440491f)
        colors.[int ImGuiCol.TabDimmedSelectedOverline] <- Vector4 (0.4235294163227081f, 0.4235294163227081f, 0.4235294163227081f, 1.0f)
        colors.[int ImGuiCol.TabHovered] <- Vector4 (0.321568638086319f, 0.321568638086319f, 0.321568638086319f, 0.800000011920929f)
        colors.[int ImGuiCol.PlotLines] <- Vector4 (0.6078431606292725f, 0.6078431606292725f, 0.6078431606292725f, 1.0f)
        colors.[int ImGuiCol.PlotLinesHovered] <- Vector4 (1.0f, 0.4274509847164154f, 0.3490196168422699f, 1.0f)
        colors.[int ImGuiCol.PlotHistogram] <- Vector4 (0.8980392217636108f, 0.6980392336845398f, 0.0f, 1.0f)
        colors.[int ImGuiCol.PlotHistogramHovered] <- Vector4 (1.0f, 0.6000000238418579f, 0.0f, 1.0f)
        colors.[int ImGuiCol.TableHeaderBg] <- Vector4 (0.1882352977991104f, 0.1882352977991104f, 0.2000000029802322f, 1.0f)
        colors.[int ImGuiCol.TableBorderStrong] <- Vector4 (0.3098039329051971f, 0.3098039329051971f, 0.3490196168422699f, 1.0f)
        colors.[int ImGuiCol.TableBorderLight] <- Vector4 (0.2274509817361832f, 0.2274509817361832f, 0.2470588237047195f, 1.0f)
        colors.[int ImGuiCol.TableRowBg] <- Vector4 (0.0f, 0.0f, 0.0f, 0.0f)
        colors.[int ImGuiCol.TableRowBgAlt] <- Vector4 (1.0f, 1.0f, 1.0f, 0.05999999865889549f)
        colors.[int ImGuiCol.TextSelectedBg] <- Vector4 (0.2588235437870026f, 0.5882353186607361f, 0.9764705896377563f, 0.3499999940395355f)
        colors.[int ImGuiCol.DragDropTarget] <- Vector4 (1.0f, 1.0f, 0.0f, 0.8999999761581421f)
        colors.[int ImGuiCol.NavCursor] <- Vector4 (0.2588235437870026f, 0.5882353186607361f, 0.9764705896377563f, 1.0f)
        colors.[int ImGuiCol.NavWindowingHighlight] <- Vector4 (1.0f, 1.0f, 1.0f, 0.699999988079071f)
        colors.[int ImGuiCol.NavWindowingDimBg] <- Vector4 (0.800000011920929f, 0.800000011920929f, 0.800000011920929f, 0.2000000029802322f)
        colors.[int ImGuiCol.ModalWindowDimBg] <- Vector4 (0.800000011920929f, 0.800000011920929f, 0.800000011920929f, 0.3499999940395355f)

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

    static member Position2dToInset (absolute, eyeCenter, eyeSize : Vector2, viewport, position) =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        let invertY = v2 1.0f -1.0f
        let positionWindow =
            if absolute
            then position * virtualScalar * invertY + eyeSize * 0.5f * virtualScalar
            else position * virtualScalar * invertY - eyeCenter * virtualScalar * invertY + eyeSize * 0.5f * virtualScalar
        let boundsRatio = viewport.Bounds.Size.V2 / viewport.Inset.Size.V2
        let offsetX = -(single viewport.Bounds.Min.X - single viewport.Inset.Min.X)
        let offsetY = single viewport.Bounds.Max.Y - single viewport.Inset.Max.Y
        let offset = v2 offsetX offsetY
        let positionInset = positionWindow / boundsRatio + offset
        positionInset

    static member Size2dToInset (viewport, size) =
        let virtualScalar = (v2iDup viewport.DisplayScalar).V2
        let sizeVirtual = size * virtualScalar
        let boundsRatio = viewport.Bounds.Size.V2 / viewport.Inset.Size.V2
        let sizeInset = sizeVirtual / boundsRatio
        sizeInset

    // NOTE: I lazily dummied out this code until I feel like navigating through the metaphorical hedge maze required
    // to convert its output to Inset space.
    //static member WindowToPosition2d (absolute, eyeCenter, eyeSize : Vector2, viewport, position) =
    //    let virtualScalar = (v2iDup viewport.DisplayScalar).V2
    //    let invertY = v2 1.0f -1.0f
    //    if absolute
    //    then position / virtualScalar * invertY - eyeSize * 0.5f * virtualScalar
    //    else position / virtualScalar * invertY + eyeCenter * virtualScalar * invertY - eyeSize * 0.5f * virtualScalar

    // OPTIMIZATION: requiring window position and size to be passed in so that expensive calls to them not need be repeatedly made.
    // TODO: the calling convention here is very inconsistent with Position2dToWindow, so let's see if we can converge them.
    static member Position3dToInset (windowPosition : Vector2, windowSize : Vector2, modelViewProjection : Matrix4x4, viewport, position : Vector3) =

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
        let positionWindow = v2 position.X position.Y

        // convert to inset
        let boundsRatio = viewport.Bounds.Size.V2 / viewport.Inset.Size.V2
        let offsetX = -(single viewport.Bounds.Min.X - single viewport.Inset.Min.X)
        let offsetY = single viewport.Bounds.Max.Y - single viewport.Inset.Max.Y
        let offset = v2 offsetX offsetY
        let positionInset = positionWindow / boundsRatio + offset
        positionInset

    // NOTE: I lazily dummied out this code until I feel like navigating through the metaphorical hedge maze required
    // to convert its output to Inset space.
    // OPTIMIZATION: requiring window position and size to be passed in so that expensive calls to them not need be repeatedly made.
    // TODO: the calling convention here is very inconsistent with WindowToPosition2d, so let's see if we can converge them.
    //static member WindowToPosition3d (windowPosition : Vector2, windowSize : Vector2, model : Matrix4x4, view : Matrix4x4, projection : Matrix4x4) =
    //
    //    // grab dependencies
    //    let io = ImGui.GetIO ()
    //
    //    // map mouse position from window coordinates to normalized device coordinates
    //    let mouseXNdc = ((io.MousePos.X - windowPosition.X) / windowSize.X) * 2.0f - 1.0f
    //    let mouseYNdc = (1.0f - ((io.MousePos.Y - windowPosition.Y) / windowSize.Y)) * 2.0f - 1.0f
    //
    //    // transform near and far positions of the clip space to world coordinates
    //    let nearPos = (v4 0.0f 0.0f 1.0f 1.0f).Transform projection
    //    let farPos = (v4 0.0f 0.0f 2.0f 1.0f).Transform projection
    //
    //    // determine if the near and far planes are reversed
    //    let reversed = nearPos.Z / nearPos.W > farPos.Z / farPos.W
    //
    //    // set the near and far clip distances accordingly based on the reversed flag
    //    let (zNear, zFar) = if reversed then (1.0f - 0.0001f, 0.0f) else (0.0f, 1.0f - 0.0001f)
    //
    //    // calculate the ray origin in world coordinates by transforming the normalized device coordinates
    //    let modelViewProjectionInverse = (model * view * projection).Inverted
    //    let mutable rayOrigin = (v4 mouseXNdc mouseYNdc zNear 1.0f).Transform modelViewProjectionInverse
    //    rayOrigin <- rayOrigin * (1.0f / rayOrigin.W)
    //
    //    // calculate the ray end in world coordinates by transforming the normalized device coordinates
    //    let mutable rayEnd = (v4 mouseXNdc mouseYNdc zFar 1.0f).Transform modelViewProjectionInverse
    //    rayEnd <- rayEnd * (1.0f / rayEnd.W)
    //
    //    // calculate the ray direction by normalizing the vector between the ray end and ray origin
    //    let rayDir = (rayEnd.V3 - rayOrigin.V3).Normalized
    //    (rayOrigin.V3, rayDir)