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
        let mutable result = ImGuiEditInactive
        let io = ImGui.GetIO ()
        let drawList = ImGui.GetBackgroundDrawList ()
        let viewport = Constants.Render.Viewport
        let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
        let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
        let viewProjection = view * projection
        let corners = box.Corners
        let centers = box.Centers
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
                drawList.AddLine (aWindow, bWindow, uint 0xFF00CFCF)
            | None -> ()
        let mousePosition = ImGui.GetMousePos ()
        let mutable found = false
        for i in 0 .. dec centers.Length do
            let center = centers.[i]
            let centerWindow = ImGui.PositionToWindow (viewProjection, center)
            let inView = eyeFrustum.Contains center <> ContainmentType.Disjoint
            let mouseDelta = mousePosition - centerWindow
            let mouseDistance = mouseDelta.Magnitude
            let mouseUp = not (ImGui.IsMouseDown ImGuiMouseButton.Left)
            let mouseClicked = ImGui.IsMouseClicked ImGuiMouseButton.Left
            let mouseDragging = ImGui.IsMouseDragging ImGuiMouseButton.Left
            let previewing = inView && mouseUp && mouseDistance < 32.0f
            let dragging = inView && (mouseClicked && mouseDistance < 32.0f || mouseDragging && mouseDistance < 64.0f) && not found
            if previewing then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CF00)
            elif dragging then
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF0000CF)
                let direction = match i with | 0 | 1 -> v3Right | 2 | 3 -> v3Up | 4 | 5 -> v3Back | _ -> failwithumf ()
                let ray = viewport.MouseToWorld3d (absolute, mousePosition, eyeCenter, eyeRotation)
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
            else
                drawList.AddCircleFilled (centerWindow, 5.0f, uint 0xFF00CFCF)
        result