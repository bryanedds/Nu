// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Xml
open System.Reflection
open System.ComponentModel
open System.Collections.Generic
open Prime
open Nu

/// Describes the overlay state of a field.
type internal OverlayState =
    | Bare
    | Altered
    | Overlaid

/// Defines the manner in which overlays are applied to targets.
type [<ReferenceEquality>] Overlayer =
    private
        { Overlays : XmlDocument }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Overlayer =

    let rec private trySelectNode overlayName propertyName overlayer =
        match overlayer.Overlays.DocumentElement with
        | null -> None
        | documentElement ->
            let optBranch = documentElement.SelectSingleNode overlayName
            match optBranch with
            | null -> None
            | branch ->
                let optLeaf = branch.SelectSingleNode propertyName
                match optLeaf with
                | null ->
                    let optIncludeNames = branch.Attributes.[Constants.Xml.IncludesAttributeName]
                    match optIncludeNames with
                    | null -> None
                    | includeNames ->
                        let includeNames = symvalue<string list> includeNames.InnerXml
                        let mutable optNode = None
                        let mutable enr = (includeNames :> _ seq).GetEnumerator ()
                        while enr.MoveNext () && Option.isNone optNode do
                            let includeName = enr.Current.Trim ()
                            optNode <- trySelectNode includeName propertyName overlayer
                        optNode
                | leaf -> Some leaf

    let private getPropertyState overlayName propertyName propertyType target overlayer =
        match trySelectNode overlayName propertyName overlayer with
        | Some overlayNode -> 
            let targetType = target.GetType ()
            let optProperty = targetType.GetProperty overlayNode.Name
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
                | (null, Some xtension) ->
                    let xFields = Xtension.getFields xtension
                    Some (Vmap.find overlayNode.Name xFields).FieldValue
                | (targetProperty, _) -> Some ^ targetProperty.GetValue target
            match optPropertyValue with
            | Some propertyValue ->
                let converter = SymbolicConverter propertyType
                if converter.CanConvertFrom typeof<string> then
                    let overlayValue = converter.ConvertFromString overlayNode.InnerText
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

    let private tryApplyOverlayToRecordField facetNames (property : PropertyInfo) (valueNode : XmlNode) oldOverlayName target oldOverlayer =
        let shouldApplyOverlay =
            property.PropertyType <> typeof<Xtension> &&
            isPropertyOverlaid oldOverlayName facetNames property.Name property.PropertyType target oldOverlayer
        if shouldApplyOverlay then
            let valueStr = valueNode.InnerText
            let converter = SymbolicConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFromString valueStr
                property.SetValue (target, value)

    let private applyOverlayToProperties oldOverlayName newOverlayName facetNames target oldOverlayer newOverlayer =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        for property in targetProperties do
            if property.Name <> "FacetNames" && property.PropertyType <> typeof<string Set> then
                match trySelectNode newOverlayName property.Name newOverlayer with
                | Some fieldNode -> tryApplyOverlayToRecordField facetNames property fieldNode oldOverlayName target oldOverlayer
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
                    List.foldBack (fun (xFieldName, xField) optNodes ->
                        match trySelectNode newOverlayName xFieldName newOverlayer with
                        | Some node -> (xField.FieldType, node) :: optNodes
                        | None -> optNodes)
                        (List.ofSeq ^ Xtension.getFields xtension)
                        []
                let xtension =
                    List.foldBack (fun (ty, node : XmlNode) xtension ->
                        if isPropertyOverlaid oldOverlayName facetNames node.Name ty target oldOverlayer then
                            let value = SymbolicDescriptor.convertFrom node.InnerText ty
                            let field = { FieldValue = value; FieldType = ty }
                            Xtension.attachField node.Name field xtension
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
            match trySelectNode newOverlayName facetNamesProperty.Name newOverlayer with
            | Some fieldNode -> tryApplyOverlayToRecordField [] facetNamesProperty fieldNode oldOverlayName target oldOverlayer
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
        { Overlays = XmlDocument () }

    /// Make an Overlayer by loading overlays from a file and then combining it with the given
    /// intrinsic overlays.
    let make (filePath : string) (intrinsicOverlays : XmlDocument) =

        // create new overlay document into which all nodes will be inserted
        let overlays = XmlDocument ()
        let overlaysRoot = overlays.CreateElement Constants.Xml.RootNodeName
        overlays.AppendChild overlaysRoot |> ignore

        // load the user-defined overlay document from file
        let loadedOverlays = XmlDocument ()
        loadedOverlays.Load filePath

        // add both intrinsic and loaded overlay nodes to document
        let childNodes =
            Seq.append
                (enumerable intrinsicOverlays.DocumentElement.ChildNodes)
                (enumerable loadedOverlays.DocumentElement.ChildNodes)
        for node in childNodes do
            let imported = overlays.ImportNode (node, true)
            overlays.DocumentElement.AppendChild imported |> ignore

        // make overlay
        { Overlays = overlays }

/// Maps from dispatcher names to optional overlay names.
type OverlayRouter =
    private
        { Routes : Map<string, string option> }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OverlayRouter =

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