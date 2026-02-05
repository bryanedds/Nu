// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace ImGuiNET
open System
open ImGuizmoNET

[<AutoOpen>]
module ImGuiIOPtr =

    // HACK: allows manual tracking of mouse and keyboard event swallowing since Dear ImGui doesn't seem to yet have
    // it worked out - https://github.com/ocornut/imgui/issues/3370
    let mutable private WantCaptureMouseLocal = false
    let mutable private WantCaptureKeyboardLocal = false

    let internal BeginFrame () =
        WantCaptureMouseLocal <- false
        WantCaptureKeyboardLocal <- false

    type ImGuiIOPtr with

        member this.WantCaptureMouseLocal = WantCaptureMouseLocal || ImGuizmo.IsViewManipulateHovered ()
        member this.WantCaptureMouseGlobal = WantCaptureMouseLocal || ImGuizmo.IsViewManipulateHovered () || this.WantCaptureMouse
        member this.SwallowMouse () = WantCaptureMouseLocal <- true

        member this.WantCaptureKeyboardLocal = WantCaptureKeyboardLocal
        member this.WantCaptureKeyboardGlobal = WantCaptureKeyboardLocal || this.WantCaptureKeyboard
        member this.SwallowKeyboard () = WantCaptureKeyboardLocal <- true