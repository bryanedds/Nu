module Nu.Serialization
open System
open System.Reflection
open System.Xml
open System.Xml.Serialization

let writeNuProperties (writer : XmlWriter) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not <| property.Name.EndsWith "Semantic" then // TODO: use attribute to filter, not name pattern!
            writer.WriteElementString (property.Name, str <| property.GetValue obj)

let readNuProperties (reader : XmlReader) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not <| property.Name.EndsWith "Semantic" then // TODO: use attribute to filter, not name pattern!
            let valueStr = reader.ReadElementString property.Name
            let value = Convert.ChangeType (valueStr, property.PropertyType)
            property.SetValue (obj, value)