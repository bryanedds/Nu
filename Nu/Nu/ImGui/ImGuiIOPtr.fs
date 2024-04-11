// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace ImGuiNET
open System

[<AutoOpen>]
module ImGuiIOPtr =

    // HACK: allows manual tracking of mouse and keyboard event swallowing since Dear ImGui doesn't seem to yet have
    // it worked out - https://github.com/ocornut/imgui/issues/3370
    let mutable internal wantCaptureMouseLocal = false
    let mutable internal wantCaptureKeyboardLocal = false

    let internal BeginFrame () =
        wantCaptureMouseLocal <- false
        wantCaptureKeyboardLocal <- false

    type ImGuiIOPtr with

        member this.WantCaptureMouseLocal = wantCaptureMouseLocal
        member this.WantCaptureKeyboardLocal = wantCaptureKeyboardLocal
        member this.WantCaptureMouseGlobal = wantCaptureMouseLocal || this.WantCaptureMouse
        member this.WantCaptureKeyboardGlobal = wantCaptureKeyboardLocal || this.WantCaptureKeyboard
        member this.SwallowMouse () = wantCaptureMouseLocal <- true
        member this.SwallowKeyboard () = wantCaptureKeyboardLocal <- true