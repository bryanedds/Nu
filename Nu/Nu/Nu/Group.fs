module Nu.Group
open System
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entity

type [<StructuralEquality; NoComparison; CLIMutable>] Group =
    { Id : Id
      EntityModels : Map<Lun, EntityModel> }

type [<StructuralEquality; NoComparison; CLIMutable>] TestGroup =
    { Group : Group }

type [<StructuralEquality; NoComparison>] GroupModel =
    | Group of Group
    | TestGroup of TestGroup
       

let groupLens =
    { Get = fun this ->
        match this with
        | Group group -> group
        | TestGroup testGroup -> testGroup.Group
      Set = fun group this ->
        match this with
        | Group _ -> Group group
        | TestGroup testGroup -> TestGroup { testGroup with Group = group }}
        
let private groupModelOptChildModelFinder addressHead this =
    let group = get this groupLens
    Map.tryFind addressHead group.EntityModels

let private groupModelChildModelAdder addressHead this (child : EntityModel) =
    let group = get this groupLens
    let group2 = { group with EntityModels = Map.add addressHead child group.EntityModels }
    set group2 this groupLens

let private groupModelChildModelRemover addressHead this =
    let group = get this groupLens
    let group2 = { group with EntityModels = Map.remove addressHead group.EntityModels }
    set group2 this groupLens

let private groupModelGetChildWithLens this address lens =
    get (getChild groupModelOptChildModelFinder this address) lens

let private groupModelSetChildWithLens child this address lens =
    let entity = getChild groupModelOptChildModelFinder this address
    let entity2 = set child entity lens
    setChild groupModelChildModelAdder groupModelChildModelRemover this address entity2

let private groupModelGetOptChildWithLens this address lens =
    let optChild = getOptChild groupModelOptChildModelFinder this address
    match optChild with
    | None -> None
    | Some child -> Some (get child lens)

let private groupModelSetOptChildWithLens optChild this address lens =
    match optChild with
    | None -> setOptChild groupModelChildModelAdder groupModelChildModelRemover this address None
    | Some child ->
        let optChildModel = getOptChild groupModelOptChildModelFinder this address
        match optChildModel with
        | None -> failwith "Cannot change a non-existent entity."
        | Some childModel ->
            let childModel2 = set child childModel lens
            setChild groupModelChildModelAdder groupModelChildModelRemover this address childModel2

let entityModelsLens =
    { Get = fun this -> (get this groupLens).EntityModels
      Set = fun entityModels this -> set { (get this groupLens) with EntityModels = entityModels } this groupLens }

let entityModelLens address =
    { Get = fun this -> Option.get <| groupModelOptChildModelFinder (List.head address) this
      Set = fun entity this -> groupModelChildModelAdder (List.head address) this entity }

let optEntityModelLens address =
    { Get = fun this -> groupModelOptChildModelFinder (List.head address) this
      Set = fun optEntity this -> match optEntity with None -> groupModelChildModelRemover (List.head address) this | Some entity -> groupModelChildModelAdder (List.head address) this entity }

let groupModelEntityLens address =
    { Get = fun this -> groupModelGetChildWithLens this address entityLens
      Set = fun entity this -> groupModelSetChildWithLens entity this address entityLens }

let groupModelOptEntityLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityLens
      Set = fun optEntity this -> groupModelSetOptChildWithLens optEntity this address entityLens }

let groupModelGuiLens address =
    { Get = fun this -> groupModelGetChildWithLens this address guiLens
      Set = fun gui this -> groupModelSetChildWithLens gui this address guiLens }

let groupModelOptGuiLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address guiLens
      Set = fun optGui this -> groupModelSetOptChildWithLens optGui this address guiLens }

let groupModelButtonLens address =
    { Get = fun this -> groupModelGetChildWithLens this address buttonLens
      Set = fun button this -> groupModelSetChildWithLens button this address buttonLens }

let groupModelOptButtonLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address buttonLens
      Set = fun button this -> groupModelSetOptChildWithLens button this address buttonLens }

let groupModelLabelLens address =
    { Get = fun this -> groupModelGetChildWithLens this address labelLens
      Set = fun label this -> groupModelSetChildWithLens label this address labelLens }

let groupModelOptLabelLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address labelLens
      Set = fun label this -> groupModelSetOptChildWithLens label this address labelLens }

let groupModelTextBoxLens address =
    { Get = fun this -> groupModelGetChildWithLens this address textBoxLens
      Set = fun textBox this -> groupModelSetChildWithLens textBox this address textBoxLens }

let groupModelOptTextBoxLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address textBoxLens
      Set = fun textBox this -> groupModelSetOptChildWithLens textBox this address textBoxLens }

let groupModelToggleLens address =
    { Get = fun this -> groupModelGetChildWithLens this address toggleLens
      Set = fun toggle this -> groupModelSetChildWithLens toggle this address toggleLens }

let groupModelOptToggleLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address toggleLens
      Set = fun toggle this -> groupModelSetOptChildWithLens toggle this address toggleLens }

let groupModelFeelerLens address =
    { Get = fun this -> groupModelGetChildWithLens this address feelerLens
      Set = fun feeler this -> groupModelSetChildWithLens feeler this address feelerLens }

let groupModelOptFeelerLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address feelerLens
      Set = fun feeler this -> groupModelSetOptChildWithLens feeler this address feelerLens }

let groupModelActorLens address =
    { Get = fun this -> groupModelGetChildWithLens this address actorLens
      Set = fun actor this -> groupModelSetChildWithLens actor this address actorLens }

let groupModelOptActorLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address optActorLens
      Set = fun optActor this -> groupModelSetOptChildWithLens optActor this address optActorLens }

let groupModelBlockLens address =
    { Get = fun this -> groupModelGetChildWithLens this address blockLens
      Set = fun block this -> groupModelSetChildWithLens block this address blockLens }

let groupModelOptBlockLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address blockLens
      Set = fun block this -> groupModelSetOptChildWithLens block this address blockLens }

let groupModelAvatarLens address =
    { Get = fun this -> groupModelGetChildWithLens this address avatarLens
      Set = fun avatar this -> groupModelSetChildWithLens avatar this address avatarLens }

let groupModelOptAvatarLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address avatarLens
      Set = fun avatar this -> groupModelSetOptChildWithLens avatar this address avatarLens }

let groupModelTileMapLens address =
    { Get = fun this -> groupModelGetChildWithLens this address tileMapLens
      Set = fun tileMap this -> groupModelSetChildWithLens tileMap this address tileMapLens }

let groupModelOptTileMapLens address =
    { Get = fun this -> groupModelGetOptChildWithLens this address tileMapLens
      Set = fun tileMap this -> groupModelSetOptChildWithLens tileMap this address tileMapLens }
          
let makeDefaultGroup () =
    { Id = getNuId ()
      EntityModels = Map.empty }

let makeDefaultGroupModel typeName =
    let groupModel = (Activator.CreateInstance ("Nu", typeName, false, BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|null|], null, null)).Unwrap () :?> GroupModel
    match groupModel with
    | Group _ ->
        Group <|
            makeDefaultGroup ()
    | TestGroup _ ->
        TestGroup
            { Group = makeDefaultGroup () }

let writeGroupEntitiesToXml (writer : XmlWriter) group =
    for entityModelKvp in group.EntityModels do
        writeEntityModelToXml writer entityModelKvp.Value

let writeGroupModelToXml (writer : XmlWriter) groupModel =
    writer.WriteStartElement typeof<GroupModel>.Name
    match groupModel with
    | Group group ->
        writeModelPropertiesMany writer "Nu.Group+GroupModel+Group" [group :> obj]
        writeGroupEntitiesToXml writer group
    | TestGroup testGroup ->
        writeModelPropertiesMany writer "Nu.Group+GroupModel+TestGroup" [testGroup :> obj; testGroup.Group :> obj]
        writeGroupEntitiesToXml writer testGroup.Group

let loadEntityModelsFromXml (groupModelNode : XmlNode) =
    let entityModelNodes = groupModelNode.SelectNodes "EntityModel"
    Seq.toList <| Seq.map loadEntityModelFromXml (System.Linq.Enumerable.Cast entityModelNodes)

let loadGroupModelFromXml (groupModelNode : XmlNode) =
    let groupModelTypeNode = groupModelNode.Item "ModelType"
    let groupModelTypeName = groupModelTypeNode.InnerText
    let groupModel = makeDefaultGroupModel groupModelTypeName
    let entityModels = loadEntityModelsFromXml (groupModelNode : XmlNode)
    match groupModel with
    | Group group -> setModelProperties3 (fun _ -> ()) groupModelNode group
    | TestGroup testGroup -> setModelProperties3 (fun _ -> testGroup.Group) groupModelNode testGroup
    (groupModel, entityModels)