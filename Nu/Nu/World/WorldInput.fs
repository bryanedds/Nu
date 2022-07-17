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
        static member getMousePosition2dScreen world =
            let viewport = World.getViewport world
            let eyePosition = World.getEyePosition2d world
            let eyeSize = World.getEyeSize2d world
            viewport.MouseTo2dScreen (World.getMousePosition world, eyePosition, eyeSize)

        /// Get the 2d world position of the mouse.
        [<FunctionBinding>]
        static member getMousePostion2dWorld absolute world =
            let viewport = World.getViewport world
            let eyePosition = World.getEyePosition2d world
            let eyeSize = World.getEyeSize2d world
            viewport.MouseToWorld2d (absolute, World.getMousePosition world, eyePosition, eyeSize)

        /// Get the 3d screen position of the mouse.
        [<FunctionBinding>]
        static member getMousePosition3dScreen world =
            let viewport = World.getViewport world
            viewport.MouseTo3dScreen (World.getMousePosition world)

        /// Get the 3d world ray of the mouse.
        [<FunctionBinding>]
        static member getMouseRay3dWorld absolute world =
            let viewport = World.getViewport world
            let eyePosition = World.getEyePosition3d world
            let eyeRotation = World.getEyeRotation3d world
            viewport.MouseToWorld3d (absolute, World.getMousePosition world, eyePosition, eyeRotation)

        /// Check that the given keyboard key is down.
        [<FunctionBinding>]
        static member isKeyboardKeyDown key world =
            ignore (world : World)
            KeyboardState.isKeyDown key

        // TODO: implement isKeyboardModifierActive.