// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.IO
open System.Reflection
open System.ComponentModel
open System.Collections.Generic
open Prime
open Nu

/// An overlay.
type Overlay =
    { OverlayName : string
      OverlayIncludeNames : string list
      OverlayProperties : Map<string, Symbol> }

/// Describes the overlay state of a property.
type internal OverlayState =
    | Bare
    | Altered
    | Overlaid

[<AutoOpen>]
module OverlayerModule =

    /// Defines the manner in which overlays are applied to targets.
    type [<ReferenceEquality>] Overlayer =
        private
            { MappedOverlays : Map<string, Overlay>
              IntrinsicOverlays : Overlay list
              ExtrinsicOverlays : Overlay list }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Overlayer =

        let rec private tryFindPropertySymbol overlayName propertyName overlayer =
            match Map.tryFind overlayName overlayer.MappedOverlays with
            | Some overlay ->
                match Map.tryFind propertyName overlay.OverlayProperties with
                | Some _ as someSymbol -> someSymbol
                | None -> List.tryFindPlus (flip3 tryFindPropertySymbol propertyName overlayer) overlay.OverlayIncludeNames
            | None -> None

        let private getPropertyState overlayName propertyName propertyType target overlayer =
            match tryFindPropertySymbol overlayName propertyName overlayer with
            | Some propertySymbol -> 
                let targetType = target.GetType ()
                let optProperty = targetType.GetProperty propertyName
                let optXtension =
                    match targetType.GetProperty "Xtension" with
                    | null -> None
                    | xtensionProperty ->
                        match xtensionProperty.GetValue target with
                        | :? Xtension as xtension -> Some xtension
                        | _ -> None
                let optPropertyValue =
                    match (optProperty, optXtension) with
                    | (null, None) -> None
                    | (null, Some xtension) -> Some (Xtension.getProperty propertyName xtension).PropertyValue
                    | (targetProperty, _) -> Some ^ targetProperty.GetValue target
                match optPropertyValue with
                | Some propertyValue ->
                    let converter = SymbolicConverter propertyType
                    if converter.CanConvertFrom typeof<Symbol> then
                        let overlayValue = converter.ConvertFrom propertySymbol
                        if overlayValue = propertyValue then Overlaid else Altered
                    else Bare
                | None -> Bare
            | None -> Bare

        let private isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer =
            getPropertyState overlayName propertyName propertyType target overlayer = Overlaid ||
            List.exists (fun facetName -> getPropertyState facetName propertyName propertyType target overlayer = Overlaid) facetNames

        let private isPropertyOverlaid5 facetNames propertyName propertyType target overlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "OptOverlayName" with
            | null -> false
            | optOverlayNameProperty ->
                match optOverlayNameProperty.GetValue target with
                | :? (string option) as optOverlayName ->
                    match optOverlayName with
                    | Some overlayName -> isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer
                    | None -> false
                | _ -> false

        let private tryApplyOverlayToRecordProperty facetNames (property : PropertyInfo) (propertySymbol : Symbol) oldOverlayName target oldOverlayer =
            let shouldApplyOverlay =
                property.PropertyType <> typeof<Xtension> &&
                isPropertyOverlaid oldOverlayName facetNames property.Name property.PropertyType target oldOverlayer
            if shouldApplyOverlay then
                let converter = SymbolicConverter property.PropertyType
                if converter.CanConvertFrom typeof<Symbol> then
                    let propertyValue = converter.ConvertFrom propertySymbol
                    property.SetValue (target, propertyValue)

        let private applyOverlayToProperties oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            let targetType = target.GetType ()
            let targetProperties = targetType.GetProperties ()
            for property in targetProperties do
                if property.Name <> "FacetNames" && property.PropertyType <> typeof<string Set> then
                    match tryFindPropertySymbol newOverlayName property.Name newOverlayer with
                    | Some propertySymbol -> tryApplyOverlayToRecordProperty facetNames property propertySymbol oldOverlayName target oldOverlayer
                    | None -> ()

        // TODO: see if this can be decomposed
        let private applyOverlayToXtension oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "Xtension" with
            | null -> ()
            | xtensionProperty ->
                match xtensionProperty.GetValue target with
                | :? Xtension as xtension ->
                    let nodes =
                        Seq.foldBack (fun (xPropertyName, xProperty) optNodes ->
                            match tryFindPropertySymbol newOverlayName xPropertyName newOverlayer with
                            | Some xPropertySymbol -> (xPropertyName, xProperty.PropertyType, xPropertySymbol) :: optNodes
                            | None -> optNodes)
                            (Xtension.toSeq xtension)
                            []
                    let xtension =
                        List.foldBack (fun (xPropertyName, xPropertyType, xPropertySymbol : Symbol) xtension ->
                            if isPropertyOverlaid oldOverlayName facetNames xPropertyName xPropertyType target oldOverlayer then
                                let xPropertyValue = SymbolicDescriptor.convertFrom xPropertySymbol xPropertyType
                                let xProperty = { PropertyValue = xPropertyValue; PropertyType = xPropertyType }
                                Xtension.attachProperty xPropertyName xProperty xtension
                            else xtension)
                            nodes
                            xtension
                    xtensionProperty.SetValue (target, xtension)
                | _ -> ()

        /// Apply an overlay to the FacetNames property of the given target.
        let applyOverlayToFacetNames oldOverlayName newOverlayName target oldOverlayer newOverlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "FacetNames" with
            | null -> ()
            | facetNamesProperty ->
                match tryFindPropertySymbol newOverlayName facetNamesProperty.Name newOverlayer with
                | Some propertySymbol -> tryApplyOverlayToRecordProperty [] facetNamesProperty propertySymbol oldOverlayName target oldOverlayer
                | None -> ()

        /// Apply an overlay to the given target (except for any FacetNames property).
        /// Only the properties that are overlaid by the old overlay as specified by the old
        /// overlayer will be changed.
        let applyOverlay6 oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            applyOverlayToProperties oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer
            applyOverlayToXtension oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer

        /// Apply an overlay to the given target.
        /// Only the properties that are overlaid by the old overlay will be changed.
        let applyOverlay oldOverlayName newOverlayName facetNames target overlayer =
            applyOverlay6 oldOverlayName newOverlayName facetNames target overlayer overlayer

        /// Query that a property should be serialized.
        let shouldPropertySerialize overlayName facetNames propertyName propertyType target overlayer =
            not ^ isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer

        /// Query that a property should be serialized.
        let shouldPropertySerialize5 facetNames propertyName propertyType target overlayer =
            not ^ isPropertyOverlaid5 facetNames propertyName propertyType target overlayer

        /// Get extrinsic overlays.
        let getExtrinsicOverlays overlayer =
            overlayer.ExtrinsicOverlays

        /// The empty overlayer.
        let empty =
            { MappedOverlays = Map.empty
              IntrinsicOverlays = List.empty
              ExtrinsicOverlays = List.empty }

        /// Make an overlayer.
        let make intrinsicOverlays extrinsicOverlays =
            let intrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) intrinsicOverlays
            let extrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) extrinsicOverlays
            { MappedOverlays = Map.concat intrinsicOverlaysMap extrinsicOverlaysMap
              IntrinsicOverlays = intrinsicOverlays
              ExtrinsicOverlays = extrinsicOverlays }

        /// Attempt to make an overlayer by loading overlays from a file and then combining it with
        /// the given intrinsic overlays.
        let tryMakeFromFile intrinsicOverlays (filePath : string) =
            try let extrinsicOverlays =
                    File.ReadAllText filePath |>
                    String.unescape |>
                    scvalue<Overlay list>
                make intrinsicOverlays extrinsicOverlays |> Right
            with exn -> Left ^ "Could not make overlayer from file '" + filePath + "' due to: " + scstring exn