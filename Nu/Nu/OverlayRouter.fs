// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open Prime
open Nu

/// The classification of an overlay target.
type Classification =
    { TypeName : string
      Specialization : string }

    static member make typeName specialization =
        { TypeName = typeName
          Specialization = specialization }

    static member makeUnspecialized typeName =
        Classification.make typeName Constants.Engine.EmptySpecialization

type OverlayDescriptor =
    { SpecializedOverlayNameOpts : Map<string, string option>
      VanillaOverlayNameOpt : string option }

    static member make specializedOverlayNameOpts vanillaOverlayNameOpt =
        { SpecializedOverlayNameOpts = specializedOverlayNameOpts
          VanillaOverlayNameOpt = vanillaOverlayNameOpt }

    static member makeVanilla vanillaOverlayNameOpt =
        OverlayDescriptor.make Map.empty vanillaOverlayNameOpt

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from type names to overlay descriptors.
    type OverlayRouter =
        private
            { Routes : Map<string, OverlayDescriptor> }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module OverlayRouter =

        /// Try to find an optional overlay name for a given classification.
        let tryFindOverlayNameOpt classification overlayRouter =
            match Map.tryFind classification.TypeName overlayRouter.Routes with
            | Some descriptor ->
                match Map.tryFind classification.Specialization descriptor.SpecializedOverlayNameOpts with
                | Some specialization -> Some specialization
                | None -> Some descriptor.VanillaOverlayNameOpt
            | None -> None
    
        /// Find an optional overlay name for a given classification.
        let findOverlayNameOpt classification overlayRouter =
            let overlayNameOpt = tryFindOverlayNameOpt classification overlayRouter
            Option.get overlayNameOpt

        /// Make an OverlayRouter.
        let make userRoutes =
            { Routes = Map.ofList userRoutes }

/// Maps from dispatcher names to optional overlay names.
type OverlayRouter = OverlayRouterModule.OverlayRouter