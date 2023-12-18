// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open ImGuiNET
open ImGuizmoNET
open Prime

[<RequireQualifiedAccess>]
module ImGuizmo =

    /// Manipulate a Box3 value via ImGuizmo.
    let ManipulateBox3 (eyeCenter, eyeRotation, eyeFrustum, absolute, snap, box : Box3 byref) =

        // render segments
        let mutable result = ImGuiEditInactive
        let io = ImGui.GetIO ()
        let drawList = ImGui.GetBackgroundDrawList ()
        let viewport = Constants.Render.Viewport
        let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
        let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
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
            match Math.tryUnionSegmentAndFrustum a b eyeFrustum with
            | Some (a, b) ->
                let aWindow = ImGui.PositionToWindow (viewProjection, a)
                let bWindow = ImGui.PositionToWindow (viewProjection, b)
                let xWindow = box2 v2Zero Constants.Render.ResolutionF
                if  xWindow.Contains aWindow <> ContainmentType.Disjoint &&
                    xWindow.Contains bWindow <> ContainmentType.Disjoint then
                    drawList.AddLine (aWindow, bWindow, uint 0xFF00CFCF)
            | None -> ()

        // manipulate centers
        let centers =
            Array.sortBy (fun center ->
                Vector3.DistanceSquared (center, eyeCenter))
                box.FaceCenters
        let mutable found = false
        for i in 0 .. dec centers.Length do
            let center = centers.[i]
            let centerWindow = ImGui.PositionToWindow (viewProjection, center)
            let canCapture = not io.WantCaptureMousePlus
            let inView = eyeFrustum.Contains center <> ContainmentType.Disjoint
            let mouseWindow = ImGui.GetMousePos ()
            let mouseDelta = mouseWindow - centerWindow
            let mouseDistance = mouseDelta.Magnitude
            let mouseUp = not (ImGui.IsMouseDown ImGuiMouseButton.Left)
            let mouseClicked = ImGui.IsMouseClicked ImGuiMouseButton.Left
            let mouseDragging = ImGui.IsMouseDragging ImGuiMouseButton.Left
            let hovering = canCapture && inView && mouseUp && mouseDistance < 40.0f
            let dragging = canCapture && inView && (mouseClicked && mouseDistance < 40.0f || mouseDragging && mouseDistance < 80.0f) && not found
            let viewing = inView
            if hovering then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CF00)
            elif dragging then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF0000CF)
                let direction = (center - box.Center).Absolute.Normalized
                let ray = viewport.MouseToWorld3d (absolute, mouseWindow, eyeCenter, eyeRotation)
                let forward = eyeRotation.Forward
                let plane = plane3 center -forward
                let mouse = (ray.Intersection plane).Value
                let delta = mouse - center
                let movement = delta * direction
                let center = Math.SnapF3d snap (centers.[i] + movement)
                centers.[i] <- center
                box <- Box3.Enclose centers
                result <- ImGuiEditActive mouseClicked
                found <- true
                io.SwallowMouse ()
            elif viewing then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CFCF)
        
        // fin
        result