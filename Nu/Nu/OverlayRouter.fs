// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open Prime
open Nu

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from type names to overlay descriptors.
    type OverlayRouter =
        private
            { Routes : Map<string, string option> }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module OverlayRouter =

        /// Try to find an optional overlay name for a given classification.
        let tryFindOverlayNameOpt dispatcherName overlayRouter =
            Map.tryFind dispatcherName overlayRouter.Routes

        /// Make an OverlayRouter.
        let make userRoutes =
            { Routes = Map.ofList userRoutes }

/// Maps from dispatcher names to optional overlay names.
type OverlayRouter = OverlayRouterModule.OverlayRouter