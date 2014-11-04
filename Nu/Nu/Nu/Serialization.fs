namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open Prime
open Nu
open Nu.Constants

[<RequireQualifiedAccess>]
module Serialization =

    /// Is a property with the given name persistent?
    let isPropertyPersistentByName (propertyName : string) =
        not <| propertyName.EndsWith "Id" && // don't write an Id
        not <| propertyName.EndsWith "Ids" && // don't write multiple Ids
        not <| propertyName.EndsWith "Np" // don't write non-persistent properties

    /// Is a property with the given name persistent?
    let isPropertyPersistent target (property : PropertyInfo) =
        isPropertyPersistentByName property.Name &&
        not (
            property.Name = NameFieldName &&
            property.PropertyType = typeof<string> &&
            Guid.TryParse (property.GetValue target :?> string, ref Guid.Empty))

    /// Read an Xtension's fields from Xml.
    let readXFields (valueNode : XmlNode) =
        let childNodes = enumerable valueNode.ChildNodes
        Seq.fold
            (fun xFields (xNode : XmlNode) ->
                let typeName = xNode.Attributes.[TypeAttributeName].InnerText
                let aType = Type.GetTypeUnqualified typeName
                let xValueStr = xNode.InnerText
                let converter = AlgebraicConverter aType
                if converter.CanConvertFrom typeof<string>
                then Map.add xNode.Name (converter.ConvertFromString xValueStr) xFields
                else debug <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'."; xFields)
            Map.empty
            childNodes

    /// Read dispatcherName from an xml node.
    let readDispatcherName defaultDispatcherName (node : XmlNode) =
        match node.Attributes.[DispatcherNameAttributeName] with
        | null -> defaultDispatcherName
        | dispatcherNameAttribute -> dispatcherNameAttribute.InnerText

    /// Read opt overlay name from an xml node.
    let readOptOverlayName (node : XmlNode) =
        let optOverlayNameStr = node.InnerText
        AlgebraicDescriptor.ConvertFromString optOverlayNameStr typeof<string option> :?> string option

    /// Read facet names from an xml node.
    let readFacetNames (node : XmlNode) =
        let facetNamesStr = node.InnerText
        let facetNames = AlgebraicDescriptor.ConvertFromString facetNamesStr typeof<string list>
        facetNames :?> obj list |> List.map (fun obj -> obj :?> string)

    /// Read an Xtension from Xml.
    let readXtension valueNode =
        let xFields = readXFields valueNode
        { XFields = xFields; CanDefault = false; Sealed = true }

    /// Attempt to read a target's property from Xml.
    let tryReadPropertyToTarget (property : PropertyInfo) (valueNode : XmlNode) (target : 'a) =
        if property.PropertyType = typeof<Xtension> then
            let xtension = property.GetValue target :?> Xtension
            let xFields = readXFields valueNode
            let xtension = { xtension with XFields = xtension.XFields @@ xFields }
            property.SetValue (target, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = AlgebraicConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFromString valueStr
                property.SetValue (target, value)

    /// Read a target's property from Xml if possible.
    let readPropertyToTarget (fieldNode : XmlNode) (target : 'a) =
        match typeof<'a>.GetPropertyWritable fieldNode.Name with
        | null -> ()
        | property -> tryReadPropertyToTarget property fieldNode target

    /// Read just the target's OptOverlayName from Xml.
    let readOptOverlayNameToTarget (targetNode : XmlNode) target =
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
    let readFacetNamesToTarget (targetNode : XmlNode) target =
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

    /// Read all of a target's properties from Xml (except OptOverlayName and FacetNames).
    let readPropertiesToTarget (targetNode : XmlNode) target =
        for node in targetNode.ChildNodes do
            if  node.Name <> "OptOverlayName" &&
                node.Name <> "FacetNames" then
                readPropertyToTarget node target

    /// Write an Xtension to Xml.
    // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writeXtension shouldWriteProperty (writer : XmlWriter) xtension =
        for xField in xtension.XFields do
            let xFieldName = xField.Key
            if  isPropertyPersistentByName xFieldName &&
                shouldWriteProperty xFieldName then
                let xValue = xField.Value
                let xValueType = xValue.GetType ()
                let xValueStr = (AlgebraicConverter xValueType).ConvertToString xValue
                writer.WriteStartElement xFieldName
                writer.WriteAttributeString (TypeAttributeName, xValueType.FullName)
                writer.WriteString xValueStr
                writer.WriteEndElement ()

    /// Write all of a target's properties to Xml.
    /// NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
    /// XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
    let writePropertiesFromTarget shouldWriteProperty (writer : XmlWriter) (target : 'a) =
        let targetType = target.GetType ()
        let properties = targetType.GetProperties ()
        for property in properties do
            let propertyValue = property.GetValue target
            match propertyValue with
            | :? Xtension as xtension ->
                writer.WriteStartElement property.Name
                writeXtension shouldWriteProperty writer xtension
                writer.WriteEndElement ()
            | _ ->
                if  isPropertyPersistent target property &&
                    shouldWriteProperty property.Name then
                    let converter = AlgebraicConverter property.PropertyType
                    let valueStr = converter.ConvertToString propertyValue
                    writer.WriteElementString (property.Name, valueStr)