namespace Nu
open System
open System.Xml
open System.Reflection
open System.ComponentModel
open System.Collections.Generic
open Prime
open Nu.NuConstants

[<AutoOpen>]
module OverlayerModule =

    // TODO: generate default overlays for dispatchers and facets.

    type OverlayState =
        | Bare
        | Altered
        | Overlaid

    /// Defines the manner in which overlays are applied to targets.
    type [<ReferenceEquality>] Overlayer =
        { OverlayDocument : XmlDocument
          RootNode : XmlNode }

[<RequireQualifiedAccess>]
module Overlayer =

    let rec private trySelectNode overlayName propertyName overlayer =
        let optBranch = overlayer.RootNode.SelectSingleNode overlayName
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
                    let includeNames = includeNames.InnerText.Split ';'
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
        | None -> Bare
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
            | None -> Bare
            | Some propertyValue ->
                let converter = TypeDescriptor.GetConverter <| propertyValue.GetType ()
                if converter.CanConvertFrom typeof<string> then
                    let overlayValue = converter.ConvertFrom overlayNode.InnerText
                    if overlayValue = propertyValue then Overlaid else Altered
                else Bare

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
                | None -> false
                | Some overlayName -> isPropertyOverlaid overlayName propertyName target overlayer
            | _ -> false

    let private tryApplyOverlayToRecordField (property : PropertyInfo) (valueNode : XmlNode) optOldOverlayName (target : 'a) oldOverlayer =
        let shouldApplyOverlay =
            property.PropertyType <> typeof<Xtension> &&
            (match optOldOverlayName with
             | None -> false
             | Some oldOverlayName -> isPropertyOverlaid oldOverlayName property.Name target oldOverlayer)
        if shouldApplyOverlay then
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (target, value)

    let private applyOverlayToDotNetProperties optOldOverlayName newOverlayName (target : 'a) oldOverlayer newOverlayer =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        for property in targetProperties do
            match trySelectNode newOverlayName property.Name newOverlayer with
            | None -> ()
            | Some fieldNode -> tryApplyOverlayToRecordField property fieldNode optOldOverlayName target oldOverlayer

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
                            | None -> optNodes
                            | Some node -> (kvp.Value.GetType (), node) :: optNodes)
                        []
                        xtension.XFields
                let nodes = List.ofSeq optNodes
                let xFields =
                    List.fold
                        (fun xFields (aType, node : XmlNode) ->
                            let shouldApplyOverlay =
                                match optOldOverlayName with
                                | None -> false
                                | Some oldOverlayName -> isPropertyOverlaid oldOverlayName node.Name target oldOverlayer
                            if shouldApplyOverlay then
                                let converter = TypeDescriptor.GetConverter aType
                                let value = converter.ConvertFrom node.InnerText
                                (node.Name, value) :: xFields
                            else xFields)
                        []
                        nodes
                let xFields = Map.addMany xFields xtension.XFields
                let xtension = { xtension with XFields = xFields }
                xtensionProperty.SetValue (target, xtension)
            | _ -> ()

    /// Apply an overlay to the given target.
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

    /// Make an Overlayer.
    let make (fileName : string) =
        let document = XmlDocument ()
        document.Load fileName
        let root = document.SelectSingleNode RootNodeName
        { OverlayDocument = document; RootNode = root }