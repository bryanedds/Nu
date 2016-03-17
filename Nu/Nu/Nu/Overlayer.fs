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

/// Describes the overlay state of a field.
type OverlayState =
    | Bare
    | Altered
    | Overlaid

/// An overlay.
type Overlay =
    { OverlayName : string
      OverlayIncludeNames : string list
      OverlayProperties : Map<string, Symbol> }

/// A map of overlays.
type Overlays = Map<string, Overlay>

[<AutoOpen>]
module OverlayerModule =

    /// Defines the manner in which overlays are applied to targets.
    type [<ReferenceEquality>] Overlayer =
        private
            { Overlays : Overlays }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Overlayer =

        let rec private tryFindPropertySymbol overlayName propertyName overlayer =
            match Map.tryFind overlayName overlayer.Overlays with
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
                    | (null, Some xtension) -> Some (Xtension.getField propertyName xtension).FieldValue
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

        let private tryApplyOverlayToRecordField facetNames (property : PropertyInfo) (propertySymbol : Symbol) oldOverlayName target oldOverlayer =
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
                    | Some propertySymbol -> tryApplyOverlayToRecordField facetNames property propertySymbol oldOverlayName target oldOverlayer
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
                        Seq.foldBack (fun (xFieldName, xField) optNodes ->
                            match tryFindPropertySymbol newOverlayName xFieldName newOverlayer with
                            | Some xFieldSymbol -> (xFieldName, xField.FieldType, xFieldSymbol) :: optNodes
                            | None -> optNodes)
                            (Xtension.toSeq xtension)
                            []
                    let xtension =
                        List.foldBack (fun (xFieldName, xFieldType, xFieldSymbol : Symbol) xtension ->
                            if isPropertyOverlaid oldOverlayName facetNames xFieldName xFieldType target oldOverlayer then
                                let xFieldValue = SymbolicDescriptor.convertFrom xFieldSymbol xFieldType
                                let xField = { FieldValue = xFieldValue; FieldType = xFieldType }
                                Xtension.attachField xFieldName xField xtension
                            else xtension)
                            nodes
                            xtension
                    xtensionProperty.SetValue (target, xtension)
                | _ -> ()

        /// Apply an overlay to the FacetNames field of the given target.
        let applyOverlayToFacetNames oldOverlayName newOverlayName target oldOverlayer newOverlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "FacetNames" with
            | null -> ()
            | facetNamesProperty ->
                match tryFindPropertySymbol newOverlayName facetNamesProperty.Name newOverlayer with
                | Some propertySymbol -> tryApplyOverlayToRecordField [] facetNamesProperty propertySymbol oldOverlayName target oldOverlayer
                | None -> ()

        /// Apply an overlay to the given target (except for any FacetNames field).
        /// Only the properties / fields that are overlaid by the old overlay as specified by the old
        /// overlayer will be changed.
        let applyOverlay6 oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            applyOverlayToProperties oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer
            applyOverlayToXtension oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer

        /// Apply an overlay to the given target.
        /// Only the properties / fields that are overlaid by the old overlay will be changed.
        let applyOverlay oldOverlayName newOverlayName facetNames target overlayer =
            applyOverlay6 oldOverlayName newOverlayName facetNames target overlayer overlayer

        /// Query that a property should be serialized.
        let shouldPropertySerialize overlayName facetNames propertyName propertyType target overlayer =
            not ^ isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer

        /// Query that a property should be serialized.
        let shouldPropertySerialize5 facetNames propertyName propertyType target overlayer =
            not ^ isPropertyOverlaid5 facetNames propertyName propertyType target overlayer

        /// Make an empty overlay.
        let makeEmpty () =
            { Overlays = Map.empty }

        /// Make an Overlayer by loading overlays from a file and then combining it with the given
        /// intrinsic overlays.
        let make (filePath : string) (intrinsicOverlays : Overlays) =
            let loadedOverlaysStr = File.ReadAllText filePath
            let loadedOverlaysList = scvalue<Overlay list> loadedOverlaysStr
            let loadedOverlays = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) loadedOverlaysList
            let overlays = intrinsicOverlays @@ loadedOverlays
            { Overlays = overlays }