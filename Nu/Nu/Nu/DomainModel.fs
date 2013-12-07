namespace Nu
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
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

    let trySetProperty (property : PropertyInfo) valueStr obj =
        let converter = TypeDescriptor.GetConverter property.PropertyType
        if converter.CanConvertFrom (valueStr.GetType ()) then
            let value = converter.ConvertFrom valueStr
            property.SetValue (obj, value)

    let setModelProperty3<'a, 'b>
        (getterB : 'a -> 'b)
        (modelNode : XmlNode)
        (obj : 'a) =
        let modelName = modelNode.Name
        for node in modelNode.ChildNodes do
            let valueStr = node.InnerText
            let optProperty_ = typeof<'a>.GetProperty modelName
            match optProperty_ with
            | null ->
                let optProperty_ = typeof<'b>.GetProperty modelName
                match optProperty_ with
                | null -> ()
                | property -> trySetProperty property valueStr <| getterB obj
            | property -> trySetProperty property valueStr <| obj

    let setModelProperties3<'a, 'b> getterB (groupModelNode : XmlNode) (obj : 'a) =
        for node in groupModelNode.ChildNodes do
            setModelProperty3<'a, 'b> getterB node obj

    let setModelProperty4<'a, 'b, 'c>
        (getterB : 'a -> 'b)
        (getterC : 'a -> 'c)
        (modelNode : XmlNode)
        (obj : 'a) =
        let modelName = modelNode.Name
        for node in modelNode.ChildNodes do
            let valueStr = node.InnerText
            let optProperty_ = typeof<'a>.GetProperty modelName
            match optProperty_ with
            | null ->
                let optProperty_ = typeof<'b>.GetProperty modelName
                match optProperty_ with
                | null ->
                    let optProperty_ = typeof<'c>.GetProperty modelName
                    match optProperty_ with
                    | null -> ()
                    | property -> trySetProperty property valueStr <| getterC obj
                | property -> trySetProperty property valueStr <| getterB obj
            | property -> trySetProperty property valueStr <| obj

    let setModelProperties4<'a, 'b, 'c> getterB getterC (modelNode : XmlNode) (obj : 'a) =
        for node in modelNode.ChildNodes do
            setModelProperty4<'a, 'b, 'c> getterB getterC node obj

    let writeModelProperties (writer : XmlWriter) obj =
        let aType = obj.GetType ()
        let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
        for property in publicProperties do
            if not <| property.Name.Contains "Id" then
                let converter = TypeDescriptor.GetConverter property.PropertyType
                let valueStr = converter.ConvertTo (property.GetValue obj, typeof<string>) :?> string
                writer.WriteElementString (property.Name, valueStr)

    let writeModelPropertiesMany (writer : XmlWriter) modelTypeName objs =
        writer.WriteElementString ("ModelType", modelTypeName)
        for obj in objs do
            writeModelProperties writer obj