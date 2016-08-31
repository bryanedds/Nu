// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open Prime
open Nu

/// The classification of a Simulant.
/// TODO: see if we can find a better place for this.
type Classification =
    { DispatcherName : string
      Specialization : string }

    static member make dispatcherName specialization =
        { DispatcherName = dispatcherName
          Specialization = specialization }

    static member makeVanilla dispatcherName =
        Classification.make dispatcherName Constants.Engine.VanillaSpecialization

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from dispatcher names to optional overlay names.
    type OverlayRouter =
        private
            { Routes : Map<Classification, string option> }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module OverlayRouter =
    
        /// Find an optional overlay name for a given overlay key.
        let findOptOverlayName overlayKey overlayRouter =
            Map.find overlayKey overlayRouter.Routes
    
        /// Try to find an optional overlay name for a given overlay key.
        let tryFindOptOverlayName overlayKey overlayRouter =
            Map.tryFind overlayKey overlayRouter.Routes
    
        /// Make an OverlayRouter.
        let make dispatchers userRoutes =
            let router = 
                Map.fold
                    (fun overlayRouter _ dispatcher ->
                        let classification = Classification.makeVanilla (dispatcher.GetType ()).Name
                        Map.add classification (Some classification.DispatcherName) overlayRouter)
                    Map.empty
                    dispatchers
            { Routes = Map.addMany userRoutes router }

/// Maps from dispatcher names to optional overlay names.
type OverlayRouter = OverlayRouterModule.OverlayRouter