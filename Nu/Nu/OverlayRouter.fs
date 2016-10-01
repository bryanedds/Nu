// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

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

    static member makeVanilla typeName =
        Classification.make typeName Constants.Engine.VanillaSpecialization

type OverlayDescriptor =
    { SpecializedOptOverlayNames : Map<string, string option>
      VanillaOptOverlayName : string option }

    static member make specializedOptOverlayNames vanillavanillaOptOverlayName =
        { SpecializedOptOverlayNames = specializedOptOverlayNames
          VanillaOptOverlayName = vanillavanillaOptOverlayName }

    static member makeVanilla vanillaOptOverlayName =
        OverlayDescriptor.make Map.empty vanillaOptOverlayName

[<AutoOpen>]
module OverlayRouterModule =

    /// Maps from type names to overlay descriptors.
    type OverlayRouter =
        private
            { Routes : Map<string, OverlayDescriptor> }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module OverlayRouter =

        /// Try to find an optional overlay name for a given classification.
        let tryFindOptOverlayName classification overlayRouter =
            match Map.tryFind classification.TypeName overlayRouter.Routes with
            | Some descriptor ->
                match Map.tryFind classification.Specialization descriptor.SpecializedOptOverlayNames with
                | Some specialization -> Some specialization
                | None -> Some descriptor.VanillaOptOverlayName
            | None -> None
    
        /// Find an optional overlay name for a given classification.
        let findOptOverlayName classification overlayRouter =
            let optOverlayName = tryFindOptOverlayName classification overlayRouter
            Option.get optOverlayName

        /// Make an OverlayRouter.
        let make userRoutes =
            { Routes = Map.ofList userRoutes }

/// Maps from dispatcher names to optional overlay names.
type OverlayRouter = OverlayRouterModule.OverlayRouter