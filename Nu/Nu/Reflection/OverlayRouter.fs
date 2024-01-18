// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open Prime

// TODO: inline OverlayRouter into Overlayer.
[<RequireQualifiedAccess>]
module OverlayRouter =

    /// Maps from type names to overlay descriptors.
    type OverlayRouter =
        private
            { Routes : Map<string, string> }

    /// Try to find an optional overlay name for a given classification.
    let tryGetOverlayNameOpt dispatcherName overlayRouter =
        Map.tryFind dispatcherName overlayRouter.Routes

    /// Make an OverlayRouter.
    let make userRoutes =
        { Routes = Map.ofList userRoutes }

    /// The empty OverlayRouter.
    let empty =
        { Routes = Map.empty }
        
/// Maps from type names to overlay descriptors.
type OverlayRouter = OverlayRouter.OverlayRouter