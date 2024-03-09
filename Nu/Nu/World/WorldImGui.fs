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

        /// Render circles via ImGui in the current eye 3d space.
        static member imGuiCircles3d absolute positions radius (color : Color) filled world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
            let viewProjection = view * projection
            for position in positions do
                let positionWindow = ImGui.PositionToWindow (viewProjection, position)
                if filled
                then drawList.AddCircleFilled (positionWindow, radius, color.Abgr)
                else drawList.AddCircle (positionWindow, radius, color.Abgr)

        /// Render a circle via ImGui in the current eye 3d space.
        static member imGuiCircle3d absolute position radius color filled world =
            World.imGuiCircles3d absolute (SArray.singleton position) radius color filled world

        /// Render segments via ImGui in the current eye 3d space.
        static member imGuiSegments3d absolute (segments : struct (Vector3 * Vector3) seq) thickness (color : Color) world =
            let drawList = ImGui.GetBackgroundDrawList ()
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            let eyeFrustum = World.getEye3dFrustumView world
            let viewport = Constants.Render.Viewport
            let view = viewport.View3d (absolute, eyeCenter, eyeRotation)
            let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
            let viewProjection = view * projection
            for segment in segments do
                match Math.tryUnionSegmentAndFrustum (fst' segment) (snd' segment) eyeFrustum with
                | Some (begin_, end_) ->
                    let beginWindow = ImGui.PositionToWindow (viewProjection, begin_)
                    let endWindow = ImGui.PositionToWindow (viewProjection, end_)
                    drawList.AddLine (beginWindow, endWindow, color.Abgr, thickness)
                | None -> ()

        /// Render a segment via ImGui in the current eye 3d space.
        static member imGuiSegment3d absolute segment thickness color world =
            World.imGuiSegments3d absolute (SArray.singleton segment) thickness color world