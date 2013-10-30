module Nu.Serialization
open System
open System.Reflection
open System.ComponentModel
open System.Xml
open System.Xml.Serialization
open OpenTK
open Nu.Core

let writeNuProperties (writer : XmlWriter) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not (property.Name.EndsWith "Semantic") && property.Name <> "Id" then // TODO: use attribute to filter, not name pattern!
            let converter = TypeDescriptor.GetConverter property.PropertyType
            let valueStr = converter.ConvertTo (property.GetValue obj, typeof<string>) :?> string
            writer.WriteElementString (property.Name, valueStr)

let readNuProperties (reader : XmlReader) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not (property.Name.EndsWith "Semantic") then // TODO: use attribute to filter, not name pattern!
            let value =
                if property.Name = "Id" then getNuId () :> obj
                else
                    let valueStr = reader.ReadElementString property.Name
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    converter.ConvertFrom valueStr
            property.SetValue (obj, value)