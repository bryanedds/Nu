namespace Prime
open System
open System.Xml
open System.Reflection
open System.ComponentModel
open System.Collections.Generic
open Prime.PrimeConstants

[<AutoOpen>]
module OverlayerModule =

    type [<ReferenceEquality>] Overlayer =
        { OverlayDocument : XmlDocument
          RootNode : XmlNode }

[<RequireQualifiedAccess>]
module Overlayer =

    let make (fileName : string) =
        let document = XmlDocument ()
        document.Load fileName
        let root = document.SelectSingleNode RootNodeName
        { OverlayDocument = document; RootNode = root }

    let rec trySelectNode overlayName propertyName overlayer =
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

    let isPropertyOverlaid overlayName propertyName overlayer =
        Option.isSome <| trySelectNode overlayName propertyName overlayer

    let tryApplyOverlayToDotNetProperty (property : PropertyInfo) (valueNode : XmlNode) (target : 'a) =
        if property.PropertyType <> typeof<Xtension> then
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (target, value)

    let applyOverlayToDotNetProperties overlayName (target : 'a) overlayer =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties (BindingFlags.Public ||| BindingFlags.Instance)
        for property in targetProperties do
            match trySelectNode overlayName property.Name overlayer with
            | None -> ()
            | Some fieldNode -> tryApplyOverlayToDotNetProperty property fieldNode target

    let applyOverlayToXtension overlayName target overlayer =
        let targetType = target.GetType ()
        match targetType.GetProperty "Xtension" with
        | null -> ()
        | xtensionProperty ->
            match xtensionProperty.GetValue target with
            | :? Xtension as xtension ->
                let optNodes =
                    Seq.map
                        (fun (kvp : KeyValuePair<string, obj>) ->
                            match trySelectNode overlayName kvp.Key overlayer with
                            | None -> None
                            | Some node -> Some (kvp.Value.GetType (), node))
                        xtension.XFields
                let nodes = Seq.definitize optNodes
                let xFields =
                    Seq.map
                        (fun (aType, node : XmlNode) ->
                            let converter = TypeDescriptor.GetConverter aType
                            let value = converter.ConvertFrom node.InnerText
                            (node.Name, value))
                        nodes
                let xFields = Map.addMany xFields xtension.XFields
                let xtension = { xtension with XFields = xFields }
                xtensionProperty.SetValue (target, xtension)
            | _ -> ()

    let applyOverlay overlayName target overlayer =
        applyOverlayToDotNetProperties overlayName target overlayer
        applyOverlayToXtension overlayName target overlayer

    let shouldWriteOverlaidProperty (overlayNode : XmlNode) target =
        let targetType = target.GetType ()
        let optProperty = targetType.GetProperty (overlayNode.Name, BindingFlags.Public ||| BindingFlags.Instance)
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
        | None -> true
        | Some propertyValue ->
            let converter = TypeDescriptor.GetConverter <| propertyValue.GetType ()
            if converter.CanConvertFrom typeof<string> then
                let overlayValue = converter.ConvertFrom overlayNode.InnerText
                overlayValue <> propertyValue
            else true

    let shouldWriteProperty propertyName target overlayer =
        let targetType = target.GetType ()
        match targetType.GetProperty "OptOverlayName" with
        | null -> true
        | optOverlayNameProperty ->
            match optOverlayNameProperty.GetValue target with
            | :? (string option) as optOverlayName ->
                match optOverlayName with
                | None -> true
                | Some overlayName ->
                    match trySelectNode overlayName propertyName overlayer with
                    | None -> true
                    | Some overlayNode -> shouldWriteOverlaidProperty overlayNode target
            | _ -> true