namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open Prime
open Nu.NuConstants

[<RequireQualifiedAccess>]
module Serialization =

    /// Is a property with the given name persistent?
    let isPropertyPersistentByName (propertyName : string) =
        not <| propertyName.EndsWith "Id" && // don't write an Id
        not <| propertyName.EndsWith "Ids" && // don't write multiple Ids
        not <| propertyName.EndsWith "Np" // don't write non-persistent properties

    /// Is a property with the given name persistent?
    let isPropertyPersistent (property : PropertyInfo) =
        isPropertyPersistentByName property.Name

    /// Read an Xtension's fields from Xml.
    let readXFields (valueNode : XmlNode) =
        let childNodes = enumerable valueNode.ChildNodes
        Seq.map
            (fun (xNode : XmlNode) ->
                let typeName = xNode.Attributes.[TypeAttributeName].InnerText
                let aType = findType typeName
                let xValueStr = xNode.InnerText
                let converter = TypeDescriptor.GetConverter aType
                if converter.CanConvertFrom typeof<string> then (xNode.Name, converter.ConvertFrom xValueStr)
                else failwith <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'.")
            childNodes

    /// Read an optXDispatcherName from an xml node.
    let readOptXDispatcherName (node : XmlNode) =
        match node.Attributes.[OptXDispatcherNameAttributeName] with
        | null -> None
        | optXDispatcherNameAttribute ->
            let optXDispatcherNameStr = optXDispatcherNameAttribute.InnerText
            let optStrConverter = StringOptionTypeConverter ()
            optStrConverter.ConvertFrom optXDispatcherNameStr :?> string option

    /// Read opt overlay name from an xml node.
    let readOptOverlayName (node : XmlNode) =
        let optXDispatcherNameStr = node.InnerText
        let strOptConverter = StringOptionTypeConverter ()
        strOptConverter.ConvertFrom optXDispatcherNameStr :?> string option

    /// Read facet names from an xml node.
    let readFacetNames (node : XmlNode) =
        let facetNamesStr = node.InnerText
        let strListConverter = StringListTypeConverter ()
        strListConverter.ConvertFrom facetNamesStr :?> string list

    /// Read an Xtension from Xml.
    let readXtension valueNode =
        let xFields = Map.ofSeq <| readXFields valueNode
        let optXDispatcherName = readOptXDispatcherName valueNode
        { XFields = xFields; OptXDispatcherName = optXDispatcherName; CanDefault = true; Sealed = false }

    /// Attempt to read a target's property from Xml.
    let tryReadTargetProperty (property : PropertyInfo) (valueNode : XmlNode) (target : 'a) =
        if property.PropertyType = typeof<Xtension> then
            let xtension = property.GetValue target :?> Xtension
            let xFields = readXFields valueNode
            let optXDispatcherName = readOptXDispatcherName valueNode
            let xtension = { xtension with XFields = Map.addMany xFields xtension.XFields; OptXDispatcherName = optXDispatcherName }
            property.SetValue (target, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (target, value)

    /// Read a target's property from Xml if possible.
    let readTargetProperty (fieldNode : XmlNode) (target : 'a) =
        match typeof<'a>.GetPropertyWritable fieldNode.Name with
        | null -> ()
        | property -> tryReadTargetProperty property fieldNode target

    /// Read just the target's OptXDispatcherName from Xml.
    let readTargetOptXDispatcherName (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let xtensionProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "Xtension" &&
                    property.PropertyType = typeof<Xtension> &&
                    property.CanWrite)
                targetProperties
        let xtensionNode = targetNode.[xtensionProperty.Name]
        let optXDispatcherName = readOptXDispatcherName xtensionNode
        let xtension = xtensionProperty.GetValue target :?> Xtension
        let xtension = { xtension with OptXDispatcherName = optXDispatcherName }
        xtensionProperty.SetValue (target, xtension)

    /// Read just the target's OptOverlayName from Xml.
    let readTargetOptOverlayName (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let optOverlayNameProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "OptOverlayName" &&
                    property.PropertyType = typeof<string option> &&
                    property.CanWrite)
                targetProperties
        match targetNode.[optOverlayNameProperty.Name] with
        | null -> ()
        | optOverlayNameNode ->
            let optOverlayName = readOptOverlayName optOverlayNameNode
            optOverlayNameProperty.SetValue (target, optOverlayName)

    /// Read just the target's FacetNames from Xml.
    let readTargetFacetNames (targetNode : XmlNode) target =
        let targetType = target.GetType ()
        let targetProperties = targetType.GetProperties ()
        let facetNamesProperty =
            Array.find
                (fun (property : PropertyInfo) ->
                    property.Name = "FacetNames" &&
                    property.PropertyType = typeof<string list> &&
                    property.CanWrite)
                targetProperties
        match targetNode.[facetNamesProperty.Name] with
        | null -> ()
        | facetNamesNode ->
            let facetNames = readFacetNames facetNamesNode
            facetNamesProperty.SetValue (target, facetNames)

    /// Read all of a target's properties from Xml.
    let readTargetProperties (targetNode : XmlNode) target =
        for node in targetNode.ChildNodes do
            if  node.Name <> "OptOverlayName" &&
                node.Name <> "FacetNames" then
                readTargetProperty node target

    /// Write an Xtension to Xml.
    // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writeXtension shouldWriteProperty (writer : XmlWriter) xtension =
        writer.WriteAttributeString (OptXDispatcherNameAttributeName, string xtension.OptXDispatcherName)
        for xField in xtension.XFields do
            let xFieldName = xField.Key
            if  isPropertyPersistentByName xFieldName &&
                shouldWriteProperty xFieldName then
                let xValue = xField.Value
                let xDispatcher = xValue.GetType ()
                let xConverter = TypeDescriptor.GetConverter xDispatcher
                let xValueStr = xConverter.ConvertTo (xValue, typeof<string>) :?> string
                writer.WriteStartElement xFieldName
                writer.WriteAttributeString (TypeAttributeName, xDispatcher.FullName)
                writer.WriteString xValueStr
                writer.WriteEndElement ()

    /// Write all of a target's properties to Xml.
    /// NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writeTargetProperties shouldWriteProperty (writer : XmlWriter) (source : 'a) =
        let aType = source.GetType ()
        let properties = aType.GetProperties ()
        for property in properties do
            let propertyValue = property.GetValue source
            match propertyValue with
            | :? Xtension as xtension ->
                writer.WriteStartElement property.Name
                writeXtension shouldWriteProperty writer xtension
                writer.WriteEndElement ()
            | _ ->
                if  isPropertyPersistent property &&
                    shouldWriteProperty property.Name then
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    let valueStr = converter.ConvertTo (propertyValue, typeof<string>) :?> string
                    writer.WriteElementString (property.Name, valueStr)