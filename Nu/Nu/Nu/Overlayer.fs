namespace Nu
open System
open System.Xml
open System.Reflection
open System.ComponentModel
open System.Collections.Generic
open Prime
open Nu
open Nu.Constants

[<AutoOpen>]
module OverlayerModule =

    /// Describes the overlay state of a field.
    type OverlayState =
        | Bare
        | Altered
        | Overlaid

    /// Defines the manner in which overlays are applied to targets.
    type [<ReferenceEquality>] Overlayer =
        { Overlays : XmlDocument }

[<RequireQualifiedAccess>]
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
                    let optIncludeNames = branch.Attributes.[IncludeAttributeName]
                    match optIncludeNames with
                    | null -> None
                    | includeNames ->
                        let includeNames = AlgebraicDescriptor.convertFromString includeNames.InnerXml typeof<string list>
                        let includeNames = includeNames :?> obj list |> List.map (fun obj -> obj :?> string) |> Array.ofList
                        let mutable optNode = None
                        let mutable enr = includeNames.GetEnumerator ()
                        while enr.MoveNext () && Option.isNone optNode do
                            let includeName = enr.Current :?> string // must be cast since Array.GetEnumerator is not generic...
                            let includeName = includeName.Trim ()
                            optNode <- trySelectNode includeName propertyName overlayer
                        optNode
                | leaf -> Some leaf

    let private getPropertyState overlayName propertyName target overlayer =
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
                | (null, Some xtension) -> Some <| Map.find overlayNode.Name xtension.XFields
                | (targetProperty, _) -> Some <| targetProperty.GetValue target
            match optPropertyValue with
            | Some propertyValue ->
                let propertyValueType = propertyValue.GetType ()
                let converter = AlgebraicConverter propertyValueType
                if converter.CanConvertFrom typeof<string> then
                    let overlayValue = converter.ConvertFromString overlayNode.InnerText
                    if overlayValue = propertyValue then Overlaid else Altered
                else Bare
            | None -> Bare
        | None -> Bare

    let private isPropertyOverlaid overlayName propertyName target overlayer =
        getPropertyState overlayName propertyName target overlayer = Overlaid

    let private isPropertyOverlaid3 propertyName target overlayer =
        let targetType = target.GetType ()
        match targetType.GetProperty "OptOverlayName" with
        | null -> false
        | optOverlayNameProperty ->
            match optOverlayNameProperty.GetValue target with
            | :? (string option) as optOverlayName ->
                match optOverlayName with
                | Some overlayName -> isPropertyOverlaid overlayName propertyName target overlayer
                | None -> false
            | _ -> false

    let private tryApplyOverlayToRecordField (property : PropertyInfo) (valueNode : XmlNode) optOldOverlayName target oldOverlayer =
        let shouldApplyOverlay =
            property.PropertyType <> typeof<Xtension> &&
            (match optOldOverlayName with
             | Some oldOverlayName -> isPropertyOverlaid oldOverlayName property.Name target oldOverlayer
             | None -> false)
        if shouldApplyOverlay then
            let valueStr = valueNode.InnerText
            let converter = AlgebraicConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFromString valueStr
                property.SetValue (target, value)

    let private applyOverlayToDotNetProperties optOldOverlayName newOverlayName target oldOverlayer newOverlayer =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        for property in targetProperties do
            if property.Name <> "FacetNames" && property.PropertyType <> typeof<string list> then
                match trySelectNode newOverlayName property.Name newOverlayer with
                | Some fieldNode -> tryApplyOverlayToRecordField property fieldNode optOldOverlayName target oldOverlayer
                | None -> ()

    // TODO: see if this can be decomposed
    let private applyOverlayToXtension optOldOverlayName newOverlayName target oldOverlayer newOverlayer =
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null -> ()
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let optNodes =
                    Seq.fold
                        (fun optNodes (kvp : KeyValuePair<string, obj>) ->
                            match trySelectNode newOverlayName kvp.Key newOverlayer with
                            | Some node -> (kvp.Value.GetType (), node) :: optNodes
                            | None -> optNodes)
                        []
                        xtension.XFields
                let nodes = List.ofSeq optNodes
                let xFields =
                    List.fold
                        (fun xFields (aType, node : XmlNode) ->
                            let shouldApplyOverlay =
                                match optOldOverlayName with
                                | Some oldOverlayName -> isPropertyOverlaid oldOverlayName node.Name target oldOverlayer
                                | None -> false
                            if shouldApplyOverlay then
                                let value = AlgebraicDescriptor.convertFromString node.InnerText aType
                                (node.Name, value) :: xFields
                            else xFields)
                        []
                        nodes
                let xFields = Map.addMany xFields xtension.XFields
                let xtension = { xtension with XFields = xFields }
                xtensionProperty.SetValue (target, xtension)
            | _ -> ()

    /// Apply an overlay to the FacetName field of the given target.
    let applyOverlayToFacetNames optOldOverlayName newOverlayName target oldOverlayer newOverlayer =
        let targetType = target.GetType ()
        match targetType.GetProperty "FacetNames" with
        | null -> ()
        | facetNamesProperty ->
            match trySelectNode newOverlayName facetNamesProperty.Name newOverlayer with
            | Some fieldNode -> tryApplyOverlayToRecordField facetNamesProperty fieldNode optOldOverlayName target oldOverlayer
            | None -> ()

    /// Apply an overlay to the given target (except for any FacetNames field).
    /// Only the properties / fields that are overlaid by the old overlay as specified by the old
    /// overlayer will be changed.
    let applyOverlay5 optOldOverlayName newOverlayName target oldOverlayer newOverlayer =
        applyOverlayToDotNetProperties optOldOverlayName newOverlayName target oldOverlayer newOverlayer
        applyOverlayToXtension optOldOverlayName newOverlayName target oldOverlayer newOverlayer

    /// Apply an overlay to the given target.
    /// Only the properties / fields that are overlaid by the old overlay will be changed.
    let applyOverlay optOldOverlayName newOverlayName target overlayer =
        applyOverlay5 optOldOverlayName newOverlayName target overlayer overlayer

    /// Query that a property should be serialized.
    let shouldPropertySerialize overlayName propertyName entity overlayer =
        not <| isPropertyOverlaid overlayName propertyName entity overlayer

    /// Query that a property should be serialized.
    let shouldPropertySerialize3 propertyName entity overlayer =
        not <| isPropertyOverlaid3 propertyName entity overlayer

    /// Make an Overlayer by loading overlays from a file and then combining it with the given
    /// intrinsic overlays.
    let make (filePath : string) (intrinsicOverlays : XmlDocument) =
        let overlays = XmlDocument ()
        overlays.Load filePath
        for node in intrinsicOverlays.DocumentElement.ChildNodes do
            let imported = overlays.ImportNode (node, true)
            ignore <| overlays.DocumentElement.AppendChild imported
        { Overlays = overlays }

    /// Make an empty Overlayer.
    let makeEmpty () = { Overlays = XmlDocument () }