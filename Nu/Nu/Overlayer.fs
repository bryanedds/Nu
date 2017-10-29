// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2017.

namespace Nu
open System
open System.IO
open System.Reflection
open Prime
open Nu

/// Describes the overlay state of a property.
type [<Struct>] internal OverlayState =
    | Bare
    | Altered
    | Overlaid

/// An overlay.
[<Syntax
    ("EntityDispatcher GuiDispatcher",
     "NodeFacet EffectFacet ScriptFacet RigidBodyFacet StaticSpriteFacet AnimatedSpriteFacet",
     "", "", "",
     Constants.PrettyPrinter.StructuredThresholdMin,
     Constants.PrettyPrinter.DefaultThresholdMax)>]
type Overlay =
    { OverlayName : string
      OverlayIncludeNames : string list
      OverlayProperties : Map<string, Symbol> }

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

        let private getPropertyStateInner overlayName propertyName propertyType target overlayer =
            match tryFindPropertySymbol overlayName propertyName overlayer with
            | Some propertySymbol -> 
                let targetType = target.GetType ()
                let propertyOpt = targetType.GetProperty propertyName
                let xtensionOpt =
                    match targetType.GetProperty "Xtension" with
                    | null -> None
                    | xtensionProperty ->
                        match xtensionProperty.GetValue target with
                        | :? Xtension as xtension -> Some xtension
                        | _ -> None
                let propertyValueOpt =
                    match (propertyOpt, xtensionOpt) with
                    | (null, None) -> None
                    | (null, Some xtension) -> Some (Xtension.getProperty propertyName xtension).PropertyValue
                    | (targetProperty, _) -> Some (targetProperty.GetValue target)
                match propertyValueOpt with
                | Some propertyValue ->
                    let converter = SymbolicConverter (false, propertyType)
                    if converter.CanConvertFrom typeof<Symbol> then
                        let overlayValue = converter.ConvertFrom propertySymbol
                        if overlayValue = propertyValue then Overlaid else Altered
                    else Bare
                | None -> Bare
            | None -> Bare

        let rec private getPropertyState overlayName facetNames propertyName propertyType target overlayer =
            match getPropertyStateInner overlayName propertyName propertyType target overlayer with
            | Bare ->
                let states = Seq.map (fun facetName -> getPropertyStateInner facetName propertyName propertyType target overlayer) facetNames
                match Seq.tryFind (function Bare -> false | Altered -> true | Overlaid -> true) states with
                | Some state -> state
                | None -> Bare
            | Altered -> Altered
            | Overlaid -> Overlaid

        let private isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer =
            getPropertyState overlayName facetNames propertyName propertyType target overlayer = Overlaid

        let private isPropertyOverlaid5 facetNames propertyName propertyType target overlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "OverlayNameOpt" with
            | null -> false
            | overlayNameOptProperty ->
                match overlayNameOptProperty.GetValue target with
                | :? (string option) as overlayNameOpt ->
                    match overlayNameOpt with
                    | Some overlayName -> isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer
                    | None -> false
                | _ -> false

        let private shouldApplyOverlay oldOverlayName facetNames (property : PropertyInfo) target oldOverlayer =
            if property.PropertyType <> typeof<Xtension> then
                match getPropertyState oldOverlayName facetNames property.Name property.PropertyType target oldOverlayer with
                | Bare -> true
                | Altered -> false
                | Overlaid -> true
            else false

        let private tryApplyOverlayToRecordProperty facetNames property (propertySymbol : Symbol) oldOverlayName target oldOverlayer =
            if shouldApplyOverlay oldOverlayName facetNames property target oldOverlayer then
                let converter = SymbolicConverter (false, property.PropertyType)
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

        let private applyOverlayToXtension oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            let targetType = target.GetType ()
            match targetType.GetProperty "Xtension" with
            | null -> ()
            | xtensionProperty ->
                match xtensionProperty.GetValue target with
                | :? Xtension as xtension ->
                    let nodes =
                        Seq.foldBack (fun (propertyName, property : Property) nodeOpts ->
                            match tryFindPropertySymbol newOverlayName propertyName newOverlayer with
                            | Some propertySymbol -> (propertyName, property.PropertyType, propertySymbol) :: nodeOpts
                            | None -> nodeOpts)
                            (Xtension.toSeq xtension)
                            []
                    let xtension =
                        List.foldBack (fun (propertyName, propertyType, propertySymbol : Symbol) xtension ->
                            if isPropertyOverlaid oldOverlayName facetNames propertyName propertyType target oldOverlayer then
                                let converter = SymbolicConverter (true, propertyType)
                                let propertyValue = converter.ConvertFrom propertySymbol
                                let property = { PropertyType = propertyType; PropertyValue = propertyValue;  }
                                Xtension.attachProperty propertyName property xtension
                            else xtension)
                            nodes
                            xtension
                    xtensionProperty.SetValue (target, xtension)
                | _ -> ()

        /// Apply an overlay to the FacetNames property of the given target.
        let applyOverlayToFacetNames (copyTarget : 'a -> 'a) oldOverlayName newOverlayName target oldOverlayer newOverlayer =
            let target = copyTarget target
            let targetType = target.GetType ()
            match targetType.GetProperty "FacetNames" with
            | null -> target
            | facetNamesProperty ->
                match tryFindPropertySymbol newOverlayName facetNamesProperty.Name newOverlayer with
                | Some propertySymbol -> tryApplyOverlayToRecordProperty Set.empty facetNamesProperty propertySymbol oldOverlayName target oldOverlayer; target
                | None -> target

        /// Apply an overlay to the given target (except for any FacetNames property).
        /// Only the properties that are overlaid by the old overlay as specified by the old
        /// overlayer will be changed.
        let applyOverlay6 (copyTarget : 'a -> 'a) oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
            let target = copyTarget target
            applyOverlayToProperties oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer
            applyOverlayToXtension oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer
            target

        /// Apply an overlay to the given target.
        /// Only the properties that are overlaid by the old overlay will be changed.
        let applyOverlay copyTarget oldOverlayName newOverlayName facetNames target overlayer =
            applyOverlay6 copyTarget oldOverlayName newOverlayName facetNames target overlayer overlayer

        /// Check that a property should be serialized.
        let shouldPropertySerialize overlayName facetNames propertyName propertyType target overlayer =
            not (isPropertyOverlaid overlayName facetNames propertyName propertyType target overlayer)

        /// Check that a property should be serialized.
        let shouldPropertySerialize5 facetNames propertyName propertyType target overlayer =
            not (isPropertyOverlaid5 facetNames propertyName propertyType target overlayer)

        /// Get intrinsic overlays.
        let getIntrinsicOverlays overlayer =
            overlayer.IntrinsicOverlays

        /// Get extrinsic overlays.
        let getExtrinsicOverlays overlayer =
            overlayer.ExtrinsicOverlays

        /// The empty overlayer.
        let empty =
            { IntrinsicOverlays = List.empty
              ExtrinsicOverlays = List.empty
              MappedOverlays = Map.empty }

        /// Make an overlayer.
        let make intrinsicOverlays extrinsicOverlays =
            let intrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) intrinsicOverlays
            let extrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) extrinsicOverlays
            { IntrinsicOverlays = intrinsicOverlays
              ExtrinsicOverlays = extrinsicOverlays
              MappedOverlays = Map.concat intrinsicOverlaysMap extrinsicOverlaysMap }

        /// Attempt to make an overlayer by loading overlays from a file and then combining it with
        /// the given intrinsic overlays.
        let tryMakeFromFile intrinsicOverlays (filePath : string) =
            try let extrinsicOverlays =
                    File.ReadAllText filePath |>
                    String.unescape |>
                    scvalue<Overlay list>
                make intrinsicOverlays extrinsicOverlays |> Right
            with exn -> Left ("Could not make overlayer from file '" + filePath + "' due to: " + scstring exn)

/// Defines the manner in which overlays are applied to targets.
type Overlayer = OverlayerModule.Overlayer