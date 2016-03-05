// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

[<RequireQualifiedAccess>]
module OverlayRouter =

    /// Maps from dispatcher names to optional overlay names.
    type OverlayRouter =
        private
            { Routes : Map<string, string option> }

    /// Find an optional overlay name for a given dispatcher name.
    let findOptOverlayName overlayName overlayRouter =
        Map.find overlayName overlayRouter.Routes

    /// Try to find an optional overlay name for a given dispatcher name.
    let tryFindOptOverlayName overlayName overlayRouter =
        Map.tryFind overlayName overlayRouter.Routes

    /// Make an OverlayRouter.
    let make dispatchers userRoutes =
        let router = 
            Map.fold
                (fun overlayRouter _ dispatcher ->
                    let dispatcherName = (dispatcher.GetType ()).Name
                    Map.add dispatcherName (Some dispatcherName) overlayRouter)
                Map.empty
                dispatchers
        { Routes = Map.addMany userRoutes router }

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from dispatcher names to optional overlay names.
    type OverlayRouter = OverlayRouter.OverlayRouter