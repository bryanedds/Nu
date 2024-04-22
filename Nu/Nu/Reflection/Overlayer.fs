// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open Prime

/// Describes the overlay state of a property.
type internal OverlayState =
    | Bare
    | Altered
    | Overlaid
    | NonPersistent

/// An overlay.
[<Syntax
    ("", "", "", "", "",
     Constants.PrettyPrinter.StructuredThresholdMin,
     Constants.PrettyPrinter.DetailedThresholdMax)>]
type Overlay =
    { OverlayName : string
      OverlaysInherited : string list
      OverlaidTypeNames : string list
      OverlayProperties : Map<string, Symbol> }

    /// Convert a dispatcher (entity facet or simulant dispatcher) name to an overlay name.
    static member dispatcherNameToOverlayName (typeName : string) =
        typeName
            .Replace("Dispatcher", "Overlay")
            .Replace("Facet", "FacetOverlay")

    /// Make intrinsic overlays.
    static member makeIntrinsicOverlays requiresFacetNames sourceTypes =

        // get the unique, decomposed source types
        let decomposedTypes =
            [|for sourceType in sourceTypes do
                yield sourceType
                for sourceTypeDecomposed in Reflection.getBaseTypesExceptObject sourceType do
                    yield sourceTypeDecomposed|] |>
            HashSet |>
            Seq.toList

        // get the descriptors needed to construct the overlays
        let overlayDescriptors =
            List.map (fun (sourceType : Type) ->
                let includeNames =
                    if sourceType.BaseType <> typeof<obj>
                    then [Overlay.dispatcherNameToOverlayName sourceType.BaseType.Name]
                    else []
                let definitions = Reflection.getPropertyDefinitionsNoInherit sourceType
                let requiresFacetNames = requiresFacetNames sourceType
                (Overlay.dispatcherNameToOverlayName sourceType.Name, sourceType.Name, includeNames, definitions, requiresFacetNames))
                decomposedTypes

        // create the intrinsic overlays with the above descriptors
        let overlays =
            List.map (fun (overlayName, overlaidTypeName, includeNames, definitions, requiresFacetNames) ->
                let overlayProperties =
                    List.foldBack
                        (fun definition overlayProperties ->
                            match definition.PropertyExpr with
                            | DefineExpr value ->
                                let converter = Reflection.makeSymbolicConverterMemo false None definition.PropertyType
                                let overlayProperty = converter.ConvertTo (value, typeof<Symbol>) :?> Symbol
                                Map.add definition.PropertyName overlayProperty overlayProperties
                            | VariableExpr _ -> overlayProperties
                            | ComputedExpr _ -> overlayProperties)
                        definitions
                        Map.empty
                let overlayProperties =
                    if requiresFacetNames
                    then Map.add Constants.Engine.FacetNamesPropertyName (Symbols ([], ValueNone)) overlayProperties
                    else overlayProperties
                { OverlayName = overlayName
                  OverlaysInherited = includeNames
                  OverlaidTypeNames = [overlaidTypeName]
                  OverlayProperties = overlayProperties })
                overlayDescriptors

        // fin
        overlays

[<RequireQualifiedAccess>]
module Overlayer =

    /// Defines the manner in which overlays are applied to targets.
    type [<ReferenceEquality>] Overlayer =
        private
            { IntrinsicOverlays : Overlay list
              ExtrinsicOverlays : Overlay list
              Overlays : Map<string, Overlay>
              Routes : Map<string, string> }

    let rec private getOverlaySymbols2 overlayName overlayer =
        match Map.tryFind overlayName overlayer.Overlays with
        | Some overlay ->
            List.fold (fun overlaySymbols overlayName ->
                let overlaySymbols' = getOverlaySymbols2 overlayName overlayer
                Map.concat overlaySymbols' overlaySymbols)
                overlay.OverlayProperties
                overlay.OverlaysInherited
        | None -> Map.empty

    let internal getOverlaySymbols overlayName facetNames overlayer =
        let overlaySymbols = getOverlaySymbols2 overlayName overlayer
        Seq.fold (fun overlaySymbols facetName ->
            let facetOverlayName = Overlay.dispatcherNameToOverlayName facetName
            let overlaySymbols' = getOverlaySymbols2 facetOverlayName overlayer
            Map.concat overlaySymbols' overlaySymbols)
            overlaySymbols
            facetNames

    let internal getPropertyState propertyName (propertyType : Type) target overlaySymbols =
        if not (Reflection.isPropertyNonPersistentByName propertyName) then
            match Map.tryFind propertyName overlaySymbols with
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
                    | (null, Some xtension) ->
                        let mutable property = Unchecked.defaultof<_>
                        match Xtension.tryGetProperty (propertyName, xtension, &property) with
                        | true -> Some property.PropertyValue
                        | false -> None
                    | (null, None) -> None
                    | (targetProperty, _) -> Some (targetProperty.GetValue target)
                match propertyValueOpt with
                | Some propertyValue ->
                    match propertySymbol with
                    | Symbols ([Text (str, _); _], _) when isNull (Type.GetType str) -> Bare
                    | _ ->
                        let converter = Reflection.makeSymbolicConverterMemo false None propertyType
                        let overlayValue = converter.ConvertFrom propertySymbol
                        if overlayValue =/= propertyValue then Altered else Overlaid
                | None -> Bare
            | None -> Bare
        else NonPersistent

    let internal shouldPropertySerialize propertyName propertyType target overlaySymbols =
        match getPropertyState propertyName propertyType target overlaySymbols with
        | Altered | Bare -> true
        | Overlaid | NonPersistent -> false

    let internal shouldApplyOverlay (property : PropertyInfo) target overlaySymbolsOld =
        if property.PropertyType <> typeof<Xtension> then
            match getPropertyState property.Name property.PropertyType target overlaySymbolsOld with
            | Bare | Overlaid -> true
            | Altered | NonPersistent -> false
        else false

    let internal tryApplyOverlayToRecordProperty property (propertySymbol : Symbol) target overlaySymbolsOld =
        if shouldApplyOverlay property target overlaySymbolsOld then
            let converter = Reflection.makeSymbolicConverterMemo false None property.PropertyType
            let propertyValue = converter.ConvertFrom propertySymbol
            property.SetValue (target, propertyValue)

    let internal applyOverlayToProperties target overlaySymbolsOld overlaySymbolsNew =
        let targetType = target.GetType ()
        let recordProperties = targetType.GetProperties ()
        for property in recordProperties do
            if property.Name <> Constants.Engine.FacetNamesPropertyName && property.PropertyType <> typeof<string Set> then
                match Map.tryFind property.Name overlaySymbolsNew with
                | Some propertySymbol -> tryApplyOverlayToRecordProperty property propertySymbol target overlaySymbolsOld
                | None -> ()

    let internal applyOverlayToXtension target overlaySymbolsOld overlaySymbolsNew =
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null -> ()
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let xtension =
                    Map.foldBack (fun propertyName propertySymbol xtension ->
                        let mutable property = Unchecked.defaultof<_>
                        match Xtension.tryGetProperty (propertyName, xtension, &property) with
                        | true ->
                            let propertyType = property.PropertyType
                            match getPropertyState propertyName propertyType target overlaySymbolsOld with
                            | Bare | Overlaid ->
                                let converter = Reflection.makeSymbolicConverterMemo false None propertyType
                                let propertyValue = converter.ConvertFrom propertySymbol
                                let property = { PropertyType = propertyType; PropertyValue = propertyValue;  }
                                Xtension.attachProperty propertyName property xtension
                            | Altered | NonPersistent -> xtension
                        | false ->
                            let recordProperties = targetType.GetProperties ()
                            if Array.notExists (fun (property : PropertyInfo) -> property.Name = propertyName) recordProperties then
                                match propertySymbol with
                                | Symbols ([Text (str, _); _], _) when notNull (Type.GetType str) ->
                                    let propertyType = typeof<DesignerProperty>
                                    match getPropertyState propertyName propertyType target overlaySymbolsOld with
                                    | Bare | Overlaid ->
                                        let converter = Reflection.makeSymbolicConverterMemo false None propertyType
                                        let propertyValue = converter.ConvertFrom propertySymbol
                                        let property = { PropertyType = propertyType; PropertyValue = propertyValue;  }
                                        Xtension.attachProperty propertyName property xtension
                                    | Altered | NonPersistent -> xtension
                                | _ -> xtension
                            else xtension)
                        overlaySymbolsNew
                        xtension
                xtensionProperty.SetValue (target, xtension)
            | _ -> ()

    let internal applyOverlayToFacetNames4 (copyTarget : 'a -> 'a) target overlaySymbolsOld overlaySymbolsNew =
        let target = copyTarget target
        let targetType = target.GetType ()
        match targetType.GetProperty Constants.Engine.FacetNamesPropertyName with
        | null -> target
        | facetNamesProperty ->
            match Map.tryFind facetNamesProperty.Name overlaySymbolsNew with
            | Some propertySymbol ->
                tryApplyOverlayToRecordProperty facetNamesProperty propertySymbol target overlaySymbolsOld
                target
            | None -> target

    /// Apply an overlay to the FacetNames property of the given target.
    let applyOverlayToFacetNames (copyTarget : 'a -> 'a) overlayNameOld overlayNameNew target overlayerOld overlayerNew =
        let overlaySymbolsOld = getOverlaySymbols overlayNameOld Seq.empty overlayerOld
        let overlaySymbolsNew = getOverlaySymbols overlayNameNew Seq.empty overlayerNew
        applyOverlayToFacetNames4 copyTarget target overlaySymbolsOld overlaySymbolsNew

    /// Apply an overlay to the given target (except for any FacetNames property).
    /// Only the properties that are overlaid by the old overlay as specified by the old
    /// overlayer will be changed.
    let applyOverlay6 (copyTarget : 'a -> 'a) overlayNameOld overlayNameNew facetNames target overlayerOld overlayerNew =
        let target = copyTarget target
        let overlaySymbolsOld = getOverlaySymbols overlayNameOld facetNames overlayerOld
        let overlaySymbolsNew = getOverlaySymbols overlayNameNew Seq.empty overlayerNew
        applyOverlayToProperties target overlaySymbolsOld overlaySymbolsNew
        applyOverlayToXtension target overlaySymbolsOld overlaySymbolsNew
        target

    /// Apply an overlay to the given target (except for any FacetNames property).
    /// Only the properties that are overlaid by the old overlay will be changed.
    let applyOverlay copyTarget overlayNameOld overlayNameNew facetNames target overlayer =
        applyOverlay6 copyTarget overlayNameOld overlayNameNew facetNames target overlayer overlayer

    /// Get intrinsic overlays.
    let getIntrinsicOverlays overlayer =
        overlayer.IntrinsicOverlays

    /// Get extrinsic overlays.
    let getExtrinsicOverlays overlayer =
        overlayer.ExtrinsicOverlays

    /// Get overlays.
    let getOverlays overlayer =
        overlayer.Overlays

    /// Try to find an optional overlay name for a given classification.
    let tryGetOverlayNameOpt dispatcherName overlayRouter =
        Map.tryFind dispatcherName overlayRouter.Routes

    /// The empty overlayer.
    let empty =
        { IntrinsicOverlays = List.empty
          ExtrinsicOverlays = List.empty
          Overlays = Map.empty
          Routes = Map.empty }

    /// Make an overlayer.
    let make intrinsicOverlays extrinsicOverlays =
        let intrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) intrinsicOverlays
        let extrinsicOverlaysMap = Map.ofListBy (fun overlay -> (overlay.OverlayName, overlay)) extrinsicOverlays
        let overlays = Map.concat intrinsicOverlaysMap extrinsicOverlaysMap
        let routes =
            (intrinsicOverlays @ extrinsicOverlays) |>
            List.map (fun overlay -> overlay.OverlaidTypeNames |> List.map (fun typeName -> (typeName, overlay.OverlayName))) |>
            List.concat |>
            Map.ofList
        { IntrinsicOverlays = intrinsicOverlays
          ExtrinsicOverlays = extrinsicOverlays
          Overlays = overlays
          Routes = routes }

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
type Overlayer = Overlayer.Overlayer