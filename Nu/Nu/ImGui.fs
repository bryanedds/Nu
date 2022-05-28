// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open ImGuiNET
open ImGuizmoNET
open Prime
open Nu

[<RequireQualifiedAccess>]
module ImGui =

    type [<NoEquality; NoComparison>] ImGui =
        private
            { ImContext : nativeint }

    let make () =
        let context = ImGui.CreateContext ()
        { ImContext = context }

    let setCurrent imGui =
        ImGui.SetCurrentContext imGui.ImContext
        ImGuizmo.SetImGuiContext imGui.ImContext

type ImGui = ImGui.ImGui