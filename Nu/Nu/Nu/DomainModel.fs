module Nu.DomainModel
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open Nu.Core

let getOptChild optChildFinder parent address =
    match address with
    | [] -> None
    | [head] ->
        let optChild = optChildFinder head parent
        match optChild with
        | None -> None
        | Some child -> Some child
    | _ :: _ -> None

let setOptChild addChild removeChild parent address optChild =
    match address with
    | [head] ->
        match optChild with
        | None -> removeChild head parent
        | Some child -> addChild head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")
                        
let getChild optChildFinder parent address =
    Option.get <| getOptChild optChildFinder parent address

let setChild childAdder childRemover parent address child =
    setOptChild childAdder childRemover parent address (Some child)

let writeNuProperties (writer : XmlWriter) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if property.Name <> "Id" then
            let converter = TypeDescriptor.GetConverter property.PropertyType
            let valueStr = converter.ConvertTo (property.GetValue obj, typeof<string>) :?> string
            writer.WriteElementString (property.Name, valueStr)

let writeNuPropertiesMany (writer : XmlWriter) modelTypeName objs =
    writer.WriteElementString ("ModelType", modelTypeName)
    for obj in objs do
        writeNuProperties writer obj

let readNuProperties (reader : XmlReader) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not <| property.Name.Contains "Subtype" then
            let value =
                if property.Name = "Id" then getNuId () :> obj
                else
                    let valueStr = reader.ReadElementString property.Name
                    let converter = TypeDescriptor.GetConverter property.PropertyType
                    converter.ConvertFrom valueStr
            property.SetValue (obj, value)

let readSubtypeFromXml (reader : XmlReader) assemblyName (subs : string) record =

    // read record and DU names
    let subtypeRecordName = reader.ReadElementString <| subs + "SubtypeDu"
    let subtypeDuName = reader.ReadElementString  <| subs + "SubtypeRcd"
    
    // read record
    let subtypeRecordValue = (Activator.CreateInstance (assemblyName, subtypeRecordName)).Unwrap ()
    readNuProperties reader subtypeRecordValue

    // create subtype DU
    let bindingFlags = BindingFlags.Instance ||| BindingFlags.NonPublic
    let subtypeDuCtorArgs = [|subtypeRecordValue|]
    let subtypeDuValue = (Activator.CreateInstance (assemblyName, subtypeDuName, false, bindingFlags, null, subtypeDuCtorArgs, null, null)).Unwrap ()
    
    // set subtype property to DU
    let recordType = record.GetType ()
    let subtypePropertyName = subs + "Subtype"
    let subtypeProperty = recordType.GetProperty subtypePropertyName
    subtypeProperty.SetValue (record, subtypeDuValue)
    subtypeRecordValue