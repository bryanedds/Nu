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
       

let groupModelGroup =
    { Get = fun this ->
        match this with
        | Group group -> group
        | TestGroup testGroup -> testGroup.Group
      Set = fun group this ->
        match this with
        | Group _ -> Group group
        | TestGroup testGroup -> TestGroup { testGroup with Group = group }}
        
let private groupModelOptChildModelFinder addressHead this =
    let group = get this groupModelGroup
    Map.tryFind addressHead group.EntityModels

let private groupModelChildModelAdder addressHead this (child : EntityModel) =
    let group = get this groupModelGroup
    let group2 = { group with EntityModels = Map.add addressHead child group.EntityModels }
    set group2 this groupModelGroup

let private groupModelChildModelRemover addressHead this =
    let group = get this groupModelGroup
    let group2 = { group with EntityModels = Map.remove addressHead group.EntityModels }
    set group2 this groupModelGroup

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

let groupModelEntityModels =
    { Get = fun this -> (get this groupModelGroup).EntityModels
      Set = fun entityModels this -> set { (get this groupModelGroup) with EntityModels = entityModels } this groupModelGroup }

let groupModelEntityModel address =
    { Get = fun this -> Option.get <| groupModelOptChildModelFinder (List.head address) this
      Set = fun entity this -> groupModelChildModelAdder (List.head address) this entity }

let groupModelOptEntityModel address =
    { Get = fun this -> groupModelOptChildModelFinder (List.head address) this
      Set = fun optEntity this -> match optEntity with None -> groupModelChildModelRemover (List.head address) this | Some entity -> groupModelChildModelAdder (List.head address) this entity }

let groupModelEntity address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelEntity
      Set = fun entity this -> groupModelSetChildWithLens entity this address entityModelEntity }

let groupModelOptEntity address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelEntity
      Set = fun optEntity this -> groupModelSetOptChildWithLens optEntity this address entityModelEntity }

let groupModelGui address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelGui
      Set = fun gui this -> groupModelSetChildWithLens gui this address entityModelGui }

let groupModelOptGui address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelGui
      Set = fun optGui this -> groupModelSetOptChildWithLens optGui this address entityModelGui }

let groupModelButton address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelButton
      Set = fun button this -> groupModelSetChildWithLens button this address entityModelButton }

let groupModelOptButton address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelButton
      Set = fun button this -> groupModelSetOptChildWithLens button this address entityModelButton }

let groupModelLabel address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelLabel
      Set = fun label this -> groupModelSetChildWithLens label this address entityModelLabel }

let groupModelOptLabel address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelLabel
      Set = fun label this -> groupModelSetOptChildWithLens label this address entityModelLabel }

let groupModelTextBox address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelTextBox
      Set = fun textBox this -> groupModelSetChildWithLens textBox this address entityModelTextBox }

let groupModelOptTextBox address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelTextBox
      Set = fun textBox this -> groupModelSetOptChildWithLens textBox this address entityModelTextBox }

let groupModelToggle address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelToggle
      Set = fun toggle this -> groupModelSetChildWithLens toggle this address entityModelToggle }

let groupModelOptToggle address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelToggle
      Set = fun toggle this -> groupModelSetOptChildWithLens toggle this address entityModelToggle }

let groupModelFeeler address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelFeeler
      Set = fun feeler this -> groupModelSetChildWithLens feeler this address entityModelFeeler }

let groupModelOptFeeler address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelFeeler
      Set = fun feeler this -> groupModelSetOptChildWithLens feeler this address entityModelFeeler }

let groupModelActor address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelActor
      Set = fun actor this -> groupModelSetChildWithLens actor this address entityModelActor }

let groupModelOptActor address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelOptActor
      Set = fun optActor this -> groupModelSetOptChildWithLens optActor this address entityModelOptActor }

let groupModelBlock address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelBlock
      Set = fun block this -> groupModelSetChildWithLens block this address entityModelBlock }

let groupModelOptBlock address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelBlock
      Set = fun block this -> groupModelSetOptChildWithLens block this address entityModelBlock }

let groupModelAvatar address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelAvatar
      Set = fun avatar this -> groupModelSetChildWithLens avatar this address entityModelAvatar }

let groupModelOptAvatar address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelAvatar
      Set = fun avatar this -> groupModelSetOptChildWithLens avatar this address entityModelAvatar }

let groupModelTileMap address =
    { Get = fun this -> groupModelGetChildWithLens this address entityModelTileMap
      Set = fun tileMap this -> groupModelSetChildWithLens tileMap this address entityModelTileMap }

let groupModelOptTileMap address =
    { Get = fun this -> groupModelGetOptChildWithLens this address entityModelTileMap
      Set = fun tileMap this -> groupModelSetOptChildWithLens tileMap this address entityModelTileMap }
          
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