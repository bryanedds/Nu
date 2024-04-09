// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Numerics
open ImGuiNET
open Prime

[<AutoOpen>]
module WorldImGui =

    type World with

        static member internal getImGui world =
            world.Subsystems.ImGui

        /// Render circles via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiCircles3dPlus absolute (positions : Vector3 seq) radius filled (computeColor : Vector3 -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFrustum = World.getEye3dFrustumView world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d
            let viewProjection = view * projection
            for position in positions do
                if eyeFrustum.Contains position = ContainmentType.Contains then
                    let color = computeColor position
                    let positionWindow = ImGui.PositionToWindow (windowPosition, windowSize, viewProjection, position)
                    if filled
                    then drawList.AddCircleFilled (positionWindow, radius, color.Abgr)
                    else drawList.AddCircle (positionWindow, radius, color.Abgr)

        /// Render segments via ImGui in the current eye 3d space.
        static member imGuiCircles3d absolute position radius filled color world =
            World.imGuiCircles3dPlus absolute position radius filled (constant color) world

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle3d absolute position radius filled color world =
            World.imGuiCircles3d absolute (SArray.singleton position) radius filled color world

        /// Render segments via ImGui in the current eye 3d space, computing color as specified.
        static member imGuiSegments3dPlus absolute (segments : struct (Vector3 * Vector3) seq) thickness (computeColor : struct (Vector3 * Vector3) -> Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let windowPosition = ImGui.GetWindowPos ()
            let windowSize = ImGui.GetWindowSize ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFrustum = World.getEye3dFrustumView world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d
            let viewProjection = view * projection
            for segment in segments do
                match Math.tryUnionSegmentAndFrustum (fst' segment) (snd' segment) eyeFrustum with
                | Some (start, stop) ->
                    let color = computeColor segment
                    let startWindow = ImGui.PositionToWindow (windowPosition, windowSize, viewProjection, start)
                    let stopWindow = ImGui.PositionToWindow (windowPosition, windowSize, viewProjection, stop)
                    drawList.AddLine (startWindow, stopWindow, color.Abgr, thickness)
                | None -> ()

        /// Render segments via ImGui in the current eye 3d space.
        static member imGuiSegments3d absolute segments thickness color world =
            World.imGuiSegments3dPlus absolute segments thickness (constant color) world

        /// Render a segment via ImGui in the current eye 3d space.
        static member imGuiSegment3d absolute segment thickness color world =
            World.imGuiSegments3d absolute (SArray.singleton segment) thickness color world