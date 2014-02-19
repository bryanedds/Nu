namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open Nu
open Nu.Core

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
            let childNodes = seq { for node in valueNode.ChildNodes do yield node } // manual seq conversion... (ugh)
            let xFields =
                Seq.map
                    (fun (xNode : XmlNode) ->
                        let typeName = xNode.Attributes.["type"].InnerText
                        match Type.GetType typeName with
                        | null -> failwith <| "Could not find type with name '" + typeName + "'."
                        | aType ->
                            let xValueStr = xNode.InnerText
                            let converter = TypeDescriptor.GetConverter aType
                            if not <| converter.CanConvertFrom typeof<string>
                            then failwith <| "Cannot convert string '" + xValueStr + "' to type '" + typeName + "'."
                            else (Lun.make xNode.Name, converter.ConvertFrom xValueStr))
                    childNodes
            let xtension = { OptXTypeName = optXTypeName; XFields = Map.ofSeq xFields }
            property.SetValue (obj, xtension)
        else
            let valueStr = valueNode.InnerText
            let converter = TypeDescriptor.GetConverter property.PropertyType
            if converter.CanConvertFrom typeof<string> then
                let value = converter.ConvertFrom valueStr
                property.SetValue (obj, value)

    let setModelProperty2<'a>
        (fieldNode : XmlNode)
        (obj : 'a) =
        let fieldName = fieldNode.Name
        let optProperty_ = typeof<'a>.GetProperty fieldName
        match optProperty_ with
        | null -> ()
        | property -> trySetProperty property fieldNode <| obj

    let setModelProperties2<'a> (modelNode : XmlNode) (obj : 'a) =
        for node in modelNode.ChildNodes do
            setModelProperty2<'a> node obj

    let setModelProperty3<'a, 'b>
        (getterB : 'a -> 'b)
        (fieldNode : XmlNode)
        (obj : 'a) =
        let fieldName = fieldNode.Name
        let optProperty_ = typeof<'a>.GetProperty fieldName
        match optProperty_ with
        | null ->
            let optProperty_ = typeof<'b>.GetProperty fieldName
            match optProperty_ with
            | null -> ()
            | property -> trySetProperty property fieldNode <| getterB obj
        | property -> trySetProperty property fieldNode <| obj

    let setModelProperties3<'a, 'b> getterB (modelNode : XmlNode) (obj : 'a) =
        for node in modelNode.ChildNodes do
            setModelProperty3<'a, 'b> getterB node obj

    let writeModelProperties (writer : XmlWriter) (obj : obj) =
        let aType = obj.GetType ()
        let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
        for property in publicProperties do
            if not (property.Name.EndsWith "Id" || property.Name.EndsWith "Ids") then
                let propertyValue = property.GetValue obj
                match propertyValue with
                | :? Xtension as xtension ->
                    // TODO: move xtension serialization code out to Prime.
                    writer.WriteStartElement property.Name
                    writer.WriteAttributeString ("xType", match xtension.OptXTypeName with None -> String.Empty | Some name -> name.LunStr)
                    for xField in xtension.XFields do
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

    let writeModelPropertiesMany (writer : XmlWriter) modelTypeName objs =
        writer.WriteElementString ("ModelType", modelTypeName)
        for obj in objs do
            writeModelProperties writer obj