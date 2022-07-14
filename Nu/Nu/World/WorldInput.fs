// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
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
            MouseState.isButtonDown mouseButton

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
        static member getMousePositionScreen2d world =
            let viewport = World.getViewport world
            let eyePosition = World.getEye2dPosition world
            let eyeSize = World.getEye2dSize world
            viewport.MouseToScreen2d (World.getMousePosition world, eyePosition, eyeSize)

        /// Get the 2d world position of the mouse.
        [<FunctionBinding>]
        static member getMousePositionWorld2d absolute world =
            let viewport = World.getViewport world
            let eyePosition = World.getEye2dPosition world
            let eyeSize = World.getEye2dSize world
            viewport.MouseToWorld2d (absolute, World.getMousePosition world, eyePosition, eyeSize)

        /// Get the 3d screen position of the mouse.
        [<FunctionBinding>]
        static member getMousePositionScreen3d world =
            let viewport = World.getViewport world
            viewport.MouseToScreen3d (World.getMousePosition world)

        /// Get the 3d world ray of the mouse.
        [<FunctionBinding>]
        static member getMouseRayWorld3d absolute world =
            let viewport = World.getViewport world
            let eyePosition = World.getEye3dPosition world
            let eyeRotation = World.getEye3dRotation world
            viewport.MouseToWorld3d (absolute, World.getMousePosition world, eyePosition, eyeRotation)

        /// Check that the given keyboard key is down.
        [<FunctionBinding>]
        static member isKeyboardKeyDown key world =
            ignore (world : World)
            KeyboardState.isKeyDown key

        // TODO: implement isKeyboardModifierActive.