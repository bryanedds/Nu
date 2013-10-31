module Nu.DataModel
open System
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open Nu.Core
                        
let getChild childFinder parent address =
    match address with
    | [head] -> childFinder head parent
    | _ -> failwith ("Invalid address '" + str address + "'.")

let setChild childAdder parent address child =
    match address with
    | [head] -> childAdder head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getChildSubtype childFinder childToSubtype address parent =
    let child = getChild childFinder parent address
    let subtype = childToSubtype child
    (child, subtype)

let setChildSubtype childAdder childSubtypeSetter address parent child subtype =
    let child2 = childSubtypeSetter child subtype
    setChild childAdder parent address child2

let getChildSubSubtype childFinder childToSubSubtype address parent =
    let child = getChild childFinder parent address
    let (subtype, subSubtype) = childToSubSubtype child
    (child, subtype, subSubtype)

let setChildSubSubtype childAdder childSubSubtypeSetter address parent child subtype subSubtype =
    let child2 = childSubSubtypeSetter child subtype subSubtype
    setChild childAdder parent address child2

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

let getOptChildSubtype optChildFinder childToOptSubtype parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSubtype = childToOptSubtype child
        match optSubtype with
        | None -> None
        | Some subtype -> Some (child, subtype)

let setOptChildSubtype childAdder childRemover childSubtypeSetter optChildSubtype parent address =
    match optChildSubtype with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, subtype) -> setChildSubtype childAdder childSubtypeSetter address parent child subtype
    
let getOptChildSubSubtype optChildFinder childToOptSubSubtype parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSubSubtype = childToOptSubSubtype child
        match optSubSubtype with
        | None -> None
        | Some (subtype, subSubtype) -> Some (child, subtype, subSubtype)

let setOptChildSubSubtype childAdder childRemover childSubSubtypeSetter optChildSubSubtype parent address =
    match optChildSubSubtype with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, subtype, subSubtype) -> setChildSubSubtype childAdder childSubSubtypeSetter address parent child subtype subSubtype

let writeNuProperties (writer : XmlWriter) obj =
    let aType = obj.GetType ()
    let publicProperties = aType.GetProperties (BindingFlags.Instance ||| BindingFlags.Public)
    for property in publicProperties do
        if not <| property.Name.Contains "Subtype" && property.Name <> "Id" then
            let converter = TypeDescriptor.GetConverter property.PropertyType
            let valueStr = converter.ConvertTo (property.GetValue obj, typeof<string>) :?> string
            writer.WriteElementString (property.Name, valueStr)

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

let writeSubtypeToXml (writer : XmlWriter) subtypeRecordName subtypeDuName subs subtype =
    writer.WriteElementString (subs + "SubtypeDu", subtypeRecordName)
    writer.WriteElementString (subs + "SubtypeRcd", subtypeDuName)
    writeNuProperties writer subtype

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