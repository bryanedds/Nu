namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open Prime
open Nu
open Nu.NuCore
module DomainModel =

    let getOptChild optChildFinder address parent =
        let optChild = optChildFinder address parent
        match optChild with
        | None -> None
        | Some child -> Some child

    let setOptChild addChild removeChild address parent optChild =
        match optChild with
        | None -> removeChild address parent
        | Some child -> addChild address parent child

    let getChild optChildFinder address parent =
        Option.get <| optChildFinder address parent

    let setChild childAdder childRemover address parent child =
        setOptChild childAdder childRemover address parent (Some child)

    let trySetProperty (property : PropertyInfo) (valueNode : XmlNode) obj =
        if property.PropertyType = typeof<Xtension> then
            // TODO: move xtension serialization code out to Prime.
            let optXTypeName = match valueNode.Attributes.["xType"].InnerText with "" -> None | str -> Some <| Lun.make str
            let childNodes = enbCast valueNode.ChildNodes
            let xFields =
                Seq.map
                    (fun (xNode : XmlNode) ->
                        let typeName = xNode.Attributes.["type"].InnerText
                        let aType = findType typeName
                        let xValueStr = xNode.InnerText
                        let converter = TypeDescriptor.GetConverter aType
                        if not <| converter.CanConvertFrom typeof<string>
                        then failwith <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'."
                        else (Lun.make xNode.Name, converter.ConvertFrom xValueStr))
                    childNodes
            let xtension = { OptXTypeName = optXTypeName; XFields = Map.ofSeq xFields; IsSealed = false }
            property.SetValue (obj, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (obj, value)

    let setModelProperty<'a> (fieldNode : XmlNode) (obj : 'a) =
        let fieldName = fieldNode.Name
        let optProperty_ = typeof<'a>.GetProperty fieldName
        match optProperty_ with
        | null -> ()
        | property -> trySetProperty property fieldNode <| obj

    let setModelProperties<'a> (modelNode : XmlNode) (obj : 'a) =
        for node in modelNode.ChildNodes do
            setModelProperty<'a> node obj

    let writeModelProperties (writer : XmlWriter) (obj : obj) =
        let aType = obj.GetType ()
        let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
        for property in publicProperties do
            // TODO: find an remove duplication of this expression
            if not (property.Name.EndsWith "Id" || property.Name.EndsWith "Ids" || property.Name.EndsWith "Ns") then
                let propertyValue = property.GetValue obj
                match propertyValue with
                | :? Xtension as xtension ->
                    // TODO: move xtension serialization code out to Prime.
                    writer.WriteStartElement property.Name
                    writer.WriteAttributeString ("xType", match xtension.OptXTypeName with None -> String.Empty | Some name -> name.LunStr)
                    for xField in xtension.XFields do
                        let xFieldName = xField.Key.LunStr
                        // TODO: find an remove duplication of this expression
                        if (xFieldName.EndsWith "Id" || xFieldName.EndsWith "Ids" || property.Name.EndsWith "Ns") then ()
                        else
                            let xValue = xField.Value
                            let xType = xValue.GetType ()
                            let xConverter = TypeDescriptor.GetConverter xType
                            let xValueStr = xConverter.ConvertTo (xValue, typeof<string>) :?> string
                            writer.WriteStartElement xField.Key.LunStr
                            writer.WriteAttributeString ("type", xType.FullName)
                            writer.WriteString xValueStr
                            writer.WriteEndElement ()
                    writer.WriteEndElement ()
                | _ ->
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    let valueStr = converter.ConvertTo (propertyValue, typeof<string>) :?> string
                    writer.WriteElementString (property.Name, valueStr)