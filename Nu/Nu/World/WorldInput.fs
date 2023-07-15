// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open ImGuiNET
open Prime
open Nu

[<AutoOpen; ModuleBinding>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member internal toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member internal toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Check that the given mouse button is down.
        [<FunctionBinding>]
        static member isMouseButtonDown mouseButton world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMousePlus) then
                MouseState.isButtonDown mouseButton
            else false

        /// Check that the given mouse button is up.
        [<FunctionBinding>]
        static member isMouseButtonUp mouseButton world =
            ignore (world : World)
            MouseState.isButtonUp mouseButton

        /// Get the position of the mouse.
        [<FunctionBinding>]
        static member getMousePosition world =
            match World.tryGetWindowSize world with
            | Some windowSize ->
                let marginI = Constants.Render.ViewportMargin windowSize
                let margin = v2 (single marginI.X) (single marginI.Y)
                MouseState.getPosition () - margin
            | None -> MouseState.getPosition ()

        /// Get the 2d screen position of the mouse.
        [<FunctionBinding>]
        static member getMousePosition2dScreen world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEyeCenter2d world
            let eyeSize = World.getEyeSize2d world
            viewport.MouseTo2dScreen (World.getMousePosition world, eyeCenter, eyeSize)

        /// Get the 2d world position of the mouse.
        [<FunctionBinding>]
        static member getMousePostion2dWorld absolute world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEyeCenter2d world
            let eyeSize = World.getEyeSize2d world
            viewport.MouseToWorld2d (absolute, World.getMousePosition world, eyeCenter, eyeSize)

        /// Get the 3d screen position of the mouse.
        [<FunctionBinding>]
        static member getMousePosition3dScreen world =
            let viewport = World.getViewport world
            viewport.MouseTo3dScreen (World.getMousePosition world)

        /// Get the 3d world ray of the mouse.
        [<FunctionBinding>]
        static member getMouseRay3dWorld absolute world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEyeCenter3d world
            let eyeRotation = World.getEyeRotation3d world
            viewport.MouseToWorld3d (absolute, World.getMousePosition world, eyeCenter, eyeRotation)

        /// Check that the given keyboard key is down.
        [<FunctionBinding>]
        static member isKeyboardKeyDown key world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardPlus) then
                KeyboardState.isKeyDown key
            else false

        /// Check that the given keyboard key is up.
        [<FunctionBinding>]
        static member isKeyboardKeyUp key world =
            ignore (world : World)
            KeyboardState.isKeyUp key

        /// Check that a keyboard alt key is down.
        [<FunctionBinding>]
        static member isKeyboardAltDown world =
            ignore (world : World)
            KeyboardState.isAltDown ()

        /// Check that a keyboard alt key is up.
        [<FunctionBinding>]
        static member isKeyboardAltUp world =
            ignore (world : World)
            KeyboardState.isAltUp ()

        /// Check that a keyboard ctrl key is down.
        [<FunctionBinding>]
        static member isKeyboardCtrlDown world =
            ignore (world : World)
            KeyboardState.isCtrlDown ()

        /// Check that a keyboard ctrl key is up.
        [<FunctionBinding>]
        static member isKeyboardCtrlUp world =
            ignore (world : World)
            KeyboardState.isCtrlUp ()

        /// Check that a keyboard shift key is down.
        [<FunctionBinding>]
        static member isKeyboardShiftDown world =
            ignore (world : World)
            KeyboardState.isShiftDown ()

        /// Check that a keyboard shift key is up.
        [<FunctionBinding>]
        static member isKeyboardShiftUp world =
            ignore (world : World)
            KeyboardState.isShiftUp ()