// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from dispatcher names to opt overlay names.
    type OverlayRouter = Map<string, string option>

[<RequireQualifiedAccess>]
module OverlayRouter =

    /// Make an OverlayRouter.
    let make dispatchers userRoutes =
        let router = 
            Map.fold
                (fun overlayRouter _ dispatcher ->
                    let dispatcherName = (dispatcher.GetType ()).Name
                    Map.add dispatcherName (Some dispatcherName) overlayRouter)
                Map.empty
                dispatchers
        Map.addMany userRoutes router