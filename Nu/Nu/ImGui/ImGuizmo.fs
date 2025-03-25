// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open ImGuiNET
open Prime

[<RequireQualifiedAccess>]
module ImGuizmo =

    /// TODO: P1: see if we need to store these as a dictionary of name-keyed states so more than one of these can be
    /// in use at once.
    let mutable private BoxCenterSelectedOpt = Option<int>.None

    /// Manipulate a Box3 value via ImGuizmo.
    let ManipulateBox3 (eyeCenter, eyeRotation, eyeFieldOfView, viewport : Viewport, snap, box : Box3 byref) =

        // render segments
        let mutable result = ImGuiEditInactive
        let io = ImGui.GetIO ()
        let drawList = ImGui.GetBackgroundDrawList ()
        let windowPosition = ImGui.GetWindowPos ()
        let windowSize = ImGui.GetWindowSize ()
        let eyeFrustum = Viewport.getFrustum eyeCenter eyeRotation eyeFieldOfView viewport
        let view = Viewport.getView3d eyeCenter eyeRotation
        let projection = Viewport.getProjection3d eyeFieldOfView viewport
        let viewProjection = view * projection
        let corners = box.Corners
        let segments =
            [|(corners.[0], corners.[1])
              (corners.[1], corners.[2])
              (corners.[2], corners.[3])
              (corners.[3], corners.[0])
              (corners.[4], corners.[5])
              (corners.[5], corners.[6])
              (corners.[6], corners.[7])
              (corners.[7], corners.[4])
              (corners.[0], corners.[6])
              (corners.[1], corners.[5])
              (corners.[2], corners.[4])
              (corners.[3], corners.[7])|]
        for (a, b) in segments do
            for (a', b') in Math.TryUnionSegmentAndFrustum' (a, b, eyeFrustum) do
                let aWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, a')
                let bWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, b')
                drawList.AddLine (aWindow, bWindow, uint 0xFF00CFCF)

        // manipulate centers
        let centers = box.FaceCenters
        let mutable draggingFound = false
        let mutable hoveringFound = false
        for i in 0 .. dec centers.Length do
            let center = centers.[i]
            let centerWindow = ImGui.Position3dToWindow (windowPosition, windowSize, viewProjection, center)
            let mouseAvailable = not io.WantCaptureMouseGlobal
            let mouseWindow = ImGui.GetMousePos ()
            let mouseDelta = mouseWindow - centerWindow
            let mouseDistance = mouseDelta.Magnitude
            let mouseClicked = ImGui.IsMouseClicked ImGuiMouseButton.Left
            let mouseDown = ImGui.IsMouseDown ImGuiMouseButton.Left
            let mouseHeld = not mouseClicked && mouseDown
            let inView = eyeFrustum.Contains center <> ContainmentType.Disjoint
            let inRange = mouseDistance < 10.0f // TODO: make constant.
            let dragging = not draggingFound && mouseAvailable && inView && mouseHeld && BoxCenterSelectedOpt = Some i
            let selecting = not draggingFound && mouseAvailable && inView && mouseClicked && inRange
            let hovering = not draggingFound && not hoveringFound && mouseAvailable && inView && not mouseDown && inRange
            let viewing = inView
            if dragging then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF0000CF)
                let direction = (center - box.Center).Absolute.Normalized
                let ray = Viewport.mouseToWorld3d eyeCenter eyeRotation eyeFieldOfView mouseWindow viewport
                let forward = eyeRotation.Forward
                let plane = plane3 center -forward
                let mouse = (ray.Intersection plane).Value
                let delta = mouse - center
                let movement = delta * direction
                let center = Math.SnapF3d (snap, centers.[i] + movement)
                centers.[i] <- center
                io.SwallowMouse ()
                draggingFound <- true
                box <- Box3.Enclose centers
                box.Size <- Vector3.Max (v3Dup (max 0.1f snap), box.Size)
                result <- ImGuiEditActive false
            elif selecting then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF0000CF)
                io.SwallowMouse ()
                draggingFound <- true
                BoxCenterSelectedOpt <- Some i
                result <- ImGuiEditActive true
            elif hovering then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CF00)
                io.SwallowMouse ()
                hoveringFound <- true
            elif viewing then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CFCF)
        if not draggingFound then
            BoxCenterSelectedOpt <- None

        // fin
        result