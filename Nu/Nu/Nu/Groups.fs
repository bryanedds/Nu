namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
open Nu.Entities

type [<StructuralEquality; NoComparison; CLIMutable>] Group =
    { Id : Id }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniFieldGroup =
    { Group : Group }

type [<StructuralEquality; NoComparison; CLIMutable>] OmniBattleGroup =
    { Group : Group }

type [<StructuralEquality; NoComparison>] GroupModel =
    | Group of Group
    | OmniFieldGroup of OmniFieldGroup
    | OmniBattleGroup of OmniBattleGroup

module Groups =
          
    let makeDefaultGroup () =
        { Id = getNuId () }

    let makeDefaultGroupModel typeName =
        let assemblyName = (Assembly.GetExecutingAssembly ()).FullName
        let groupModel = (Activator.CreateInstance (assemblyName, typeName, false, BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|null|], null, null)).Unwrap () :?> GroupModel
        match groupModel with
        | Group _ -> Group <| makeDefaultGroup ()
        | OmniFieldGroup _ -> OmniFieldGroup { Group = makeDefaultGroup () }
        | OmniBattleGroup _ -> OmniBattleGroup { Group = makeDefaultGroup () }

    let writeGroupEntitiesToXml (writer : XmlWriter) (entityModels : Map<Lun, EntityModel>) =
        for entityModelKvp in entityModels do
            writeEntityModelToXml writer entityModelKvp.Value

    let writeGroupModelToXml (writer : XmlWriter) groupModel entityModels =
        writer.WriteStartElement typeof<GroupModel>.Name
        match groupModel with
        | Group group ->
            writeModelPropertiesMany writer "Nu.GroupModel+Group" [group :> obj]
            writeGroupEntitiesToXml writer entityModels
        | OmniFieldGroup omniFieldGroup ->
            writeModelPropertiesMany writer "Nu.GroupModel+OmniFieldGroup" [omniFieldGroup :> obj; omniFieldGroup.Group :> obj]
            writeGroupEntitiesToXml writer entityModels
        | OmniBattleGroup omniBattleGroup ->
            writeModelPropertiesMany writer "Nu.GroupModel+OmniBattleGroup" [omniBattleGroup :> obj; omniBattleGroup.Group :> obj]
            writeGroupEntitiesToXml writer entityModels

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
        | OmniFieldGroup omniFieldGroup -> setModelProperties3 (fun _ -> omniFieldGroup.Group) groupModelNode omniFieldGroup
        | OmniBattleGroup omniBattleGroup -> setModelProperties3 (fun _ -> omniBattleGroup.Group) groupModelNode omniBattleGroup
        (groupModel, entityModels)

    let writeGroupModelFile groupModel entityModels fileName world =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        writeGroupModelToXml writer groupModel entityModels
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupModelFile (fileName : string) world =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.Item "Root"
        let groupModelNode = rootNode.FirstChild
        loadGroupModelFromXml groupModelNode