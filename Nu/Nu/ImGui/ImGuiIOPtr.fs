// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace ImGuiNET
open System
open Prime
open Nu

[<AutoOpen>]
module ImGuiIOPtr =

    // HACK: allows manual tracking of mouse and keyboard event swallowing since Dead ImGui doesn't seem to yet have
    // it worked out - https://github.com/ocornut/imgui/issues/3370
    let mutable internal wantCaptureMousePlus = false
    let mutable internal wantCaptureKeyboardPlus = false

    let internal BeginFrame () =
        wantCaptureMousePlus <- false
        wantCaptureKeyboardPlus <- false

    type ImGuiIOPtr with

        member this.WantCaptureMousePlus = wantCaptureMousePlus || this.WantCaptureMouse
        member this.WantCaptureKeyboardPlus = wantCaptureKeyboardPlus || this.WantCaptureKeyboard
        member this.SwallowMouse () = wantCaptureMousePlus <- true
        member this.SwallowKeyboard () = wantCaptureKeyboardPlus <- true