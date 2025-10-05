// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.Numerics
open ImGuiNET
open Prime

/// Cursor manipulation functions for the world.
[<AutoOpen>]
module WorldCursorModule =

    type World with

        /// Get the cursor type.
        static member getCursorType (world : World) =
            world.Subsystems.CursorClient.CursorType

        /// Set the cursor type .
        static member setCursorType cursorType (world : World) =
            world.Subsystems.CursorClient.CursorType <- cursorType

        /// Check cursor visibility.
        static member getCursorVisible (world : World) =
            world.Subsystems.CursorClient.CursorVisible

        /// Set cursor visibility.
        static member setCursorVisible visible (world : World) =
            world.Subsystems.CursorClient.CursorVisible <- visible

        /// Load a cursor asset package. Should be used to avoid loading assets at inconvenient times (such as in the
        /// middle of game play!)
        static member loadCursorPackage packageName (world : World) =
            world.Subsystems.CursorClient.LoadCursorPackage packageName
            
        /// Unload an cursor package since its assets will not be used again soon.
        static member unloadCursorPackage packageName (world : World) =
            world.Subsystems.CursorClient.UnloadCursorPackage packageName

        /// Reload all cursor assets.
        static member reloadCursorAssets (world : World) =
            world.Subsystems.CursorClient.ReloadCursorAssets ()