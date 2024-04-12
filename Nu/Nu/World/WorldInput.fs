// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open ImGuiNET
open Prime

[<AutoOpen>]
module WorldInputModule =

    type World with

        /// Convert a MouseButton to SDL's representation.
        static member internal toSdlMouseButton mouseButton =
            MouseState.toSdlButton mouseButton

        /// Convert SDL's representation of a mouse button to a MouseButton.
        static member internal toNuMouseButton mouseButton =
            MouseState.toNuButton mouseButton

        /// Check that the given mouse button is down.
        static member isMouseButtonDown mouseButton world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMouseGlobal)
            then MouseState.isButtonDown mouseButton
            else false

        /// Check that the given mouse button is up.
        static member isMouseButtonUp mouseButton world =
            ignore (world : World)
            MouseState.isButtonUp mouseButton

        /// Get the position of the mouse.
        static member getMousePosition world =
            match World.tryGetWindowSize world with
            | Some windowSize ->
                let marginI = Constants.Render.ViewportMargin windowSize
                let margin = v2 (single marginI.X) (single marginI.Y)
                MouseState.getPosition () - margin
            | None -> MouseState.getPosition ()

        /// Get the 2d screen position of the mouse.
        static member getMousePosition2dScreen world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEye2dCenter world
            let eyeSize = World.getEye2dSize world
            viewport.MouseTo2dScreen (World.getMousePosition world, eyeCenter, eyeSize)

        /// Get the 2d world position of the mouse.
        static member getMousePostion2dWorld absolute world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEye2dCenter world
            let eyeSize = World.getEye2dSize world
            viewport.MouseToWorld2d (absolute, World.getMousePosition world, eyeCenter, eyeSize)

        /// Get the 3d screen position of the mouse.
        static member getMousePosition3dScreen world =
            let viewport = World.getViewport world
            viewport.MouseToScreen3d (World.getMousePosition world)

        /// Get the 3d world ray of the mouse.
        static member getMouseRay3dWorld absolute world =
            let viewport = World.getViewport world
            let eyeCenter = World.getEye3dCenter world
            let eyeRotation = World.getEye3dRotation world
            viewport.MouseToWorld3d (absolute, World.getMousePosition world, eyeCenter, eyeRotation)

        /// Check that the given keyboard key is down.
        static member isKeyboardKeyDown key world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardGlobal)
            then KeyboardState.isKeyDown key
            else false

        /// Check that the given keyboard key is up.
        static member isKeyboardKeyUp key world =
            ignore (world : World)
            KeyboardState.isKeyUp key

        /// Check that a keyboard alt key is down.
        static member isKeyboardAltDown world =
            ignore (world : World)
            KeyboardState.isAltDown ()

        /// Check that a keyboard alt key is up.
        static member isKeyboardAltUp world =
            ignore (world : World)
            KeyboardState.isAltUp ()

        /// Check that a keyboard enter key is down.
        static member isKeyboardEnterDown world =
            ignore (world : World)
            KeyboardState.isEnterDown ()

        /// Check that a keyboard enter key is up.
        static member isKeyboardEnterUp world =
            ignore (world : World)
            KeyboardState.isEnterUp ()

        /// Check that a keyboard ctrl key is down.
        static member isKeyboardCtrlDown world =
            ignore (world : World)
            KeyboardState.isCtrlDown ()

        /// Check that a keyboard ctrl key is up.
        static member isKeyboardCtrlUp world =
            ignore (world : World)
            KeyboardState.isCtrlUp ()

        /// Check that a keyboard shift key is down.
        static member isKeyboardShiftDown world =
            ignore (world : World)
            KeyboardState.isShiftDown ()

        /// Check that a keyboard shift key is up.
        static member isKeyboardShiftUp world =
            ignore (world : World)
            KeyboardState.isShiftUp ()

        /// Check that an SDL gamepad button is supported.
        static member isSdlButtonSupported button world =
            ignore (world : World)
            GamepadState.isSdlButtonSupported button

        /// Get the number of open gamepad.
        static member getGamepadCount world =
            ignore (world : World)
            GamepadState.getGamepadCount ()

        /// Convert a GamepadButton to SDL's representation.
        static member toSdlButton gamepadButton world =
            ignore (world : World)
            GamepadState.toSdlButton gamepadButton

        /// Convert SDL's representation of a joystick button to a GamepadButton.
        static member toNuButton gamepadButton world =
            ignore (world : World)
            GamepadState.toNuButton gamepadButton

        /// Convert a GamepadDirection to SDL's representation.
        static member toSdlDirection gamepadDirection world =
            ignore (world : World)
            GamepadState.toSdlDirection gamepadDirection

        /// Convert SDL's representation of a hat direction to a GamepadDirection.
        static member toNuDirection gamepadDirection world =
            ignore (world : World)
            GamepadState.toNuDirection gamepadDirection

        /// Get the given gamepad's current direction.
        static member getDirection index world =
            ignore (world : World)
            GamepadState.getDirection index

        /// Check that the given gamepad's button is down.
        static member isButtonDown index button world =
            ignore (world : World)
            GamepadState.isButtonDown index button