// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace ImGuiNET
open System

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

        member this.WantCaptureMouseLocal = WantCaptureMouseLocal
        member this.WantCaptureKeyboardLocal = WantCaptureKeyboardLocal
        member this.WantCaptureMouseGlobal = WantCaptureMouseLocal || this.WantCaptureMouse
        member this.WantCaptureKeyboardGlobal = WantCaptureKeyboardLocal || this.WantCaptureKeyboard
        member this.SwallowMouse () = WantCaptureMouseLocal <- true
        member this.SwallowKeyboard () = WantCaptureKeyboardLocal <- true