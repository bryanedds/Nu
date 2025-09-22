// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open ImGuiNET
open Prime

/// HID input functions for the world.
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

        /// Check that the given mouse button was just pressed.
        static member isMouseButtonPressed mouseButton world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMouseGlobal)
            then MouseState.isButtonPressed mouseButton
            else false

        /// Check that the given mouse button was just clicked.
        static member isMouseButtonClicked mouseButton world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardGlobal)
            then MouseState.isButtonClicked mouseButton
            else false

        /// Get the position of the mouse.
        static member getMousePosition (world : World) =
            let viewport = world.RasterViewport
            let offset = viewport.Bounds.Min
            let margin = v2 (single offset.X) (single offset.Y)
            MouseState.getPosition () - margin

        /// Get the 2d screen position of the mouse.
        static member getMousePosition2dScreen (world : World) =
            let viewport = world.RasterViewport
            let mousePosition = World.getMousePosition world
            Viewport.mouseTo2dScreen world.Eye2dCenter world.Eye2dSize mousePosition viewport

        /// Get the 2d world position of the mouse.
        static member getMousePosition2dWorld absolute (world : World) =
            let viewport = world.RasterViewport
            let mousePosition = World.getMousePosition world
            Viewport.mouseToWorld2d absolute world.Eye2dCenter world.Eye2dSize mousePosition viewport

        /// Get the 3d screen position of the mouse.
        static member getMousePosition3dScreen (world : World) =
            Viewport.mouseToScreen3d (World.getMousePosition world) world.RasterViewport

        /// Get the 3d world ray of the mouse.
        static member getMouseRay3dWorld (world : World) =
            let mousePosition = World.getMousePosition world
            Viewport.mouseToWorld3d world.Eye3dCenter world.Eye3dRotation world.Eye3dFieldOfView mousePosition world.RasterViewport

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

        /// Check that the given keyboard key was just pressed.
        static member isKeyboardKeyPressed key world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureMouseGlobal)
            then KeyboardState.isKeyPressed key
            else false

        /// Check that a keyboard alt key is down.
        static member isKeyboardAltDown world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardGlobal)
            then KeyboardState.isAltDown ()
            else false

        /// Check that a keyboard alt key is up.
        static member isKeyboardAltUp world =
            ignore (world : World)
            KeyboardState.isAltUp ()

        /// Check that a keyboard enter key is down.
        static member isKeyboardEnterDown world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardGlobal)
            then KeyboardState.isEnterDown ()
            else false

        /// Check that a keyboard enter key is up.
        static member isKeyboardEnterUp world =
            ignore (world : World)
            KeyboardState.isEnterUp ()

        /// Check that a keyboard enter key was just pressed.
        static member isKeyboardEnterPressed world =
            ignore (world : World)
            let io = ImGui.GetIO ()
            if not (io.WantCaptureKeyboardGlobal)
            then KeyboardState.isEnterPressed ()
            else false

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