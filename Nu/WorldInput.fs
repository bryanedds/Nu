// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open OpenTK
open Prime
open Nu

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
        static member isMouseButtonDown mouseButton (_ : World) =
            MouseState.isButtonDown mouseButton

        /// Get the position of the mouse.
        static member getMousePosition (_ : World) =
            MouseState.getPosition ()

        /// Get the position of the mouse in floating-point coordinates.
        static member getMousePositionF (_ : World) =
            MouseState.getPositionF ()

        /// Check that the given keyboard key is down.
        static member isKeyboardKeyDown scanCode (_ : World) =
            KeyboardState.isKeyDown scanCode

        // TODO: implement isKeyboardModifierActive.