namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Nu.Core
open Nu.DomainModel
module Groups =
          
    let makeDefaultGroup () =
        { Group.Id = getNuId () }

    let makeDefaultGroupModel typeName =
        let assemblyName = (Assembly.GetExecutingAssembly ()).FullName
        let groupModel = (Activator.CreateInstance (assemblyName, typeName, false, BindingFlags.Instance ||| BindingFlags.NonPublic, null, [|null|], null, null)).Unwrap () :?> GroupModel
        match groupModel with
        | Group _ -> Group <| makeDefaultGroup ()
        | OmniFieldGroup _ -> OmniFieldGroup { Group = makeDefaultGroup () }
        | OmniBattleGroup _ -> OmniBattleGroup { Group = makeDefaultGroup () }

    let writeGroupEntitiesToXml (writer : XmlWriter) (entityModels : Map<Lun, EntityModel>) =
        for entityModelKvp in entityModels do
            Entities.writeEntityModelToXml writer entityModelKvp.Value

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
        Seq.toList <| Seq.map Entities.loadEntityModelFromXml (System.Linq.Enumerable.Cast entityModelNodes)

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

    let groupLens =
        { Get = fun groupModel ->
            match groupModel with
            | Group group -> group
            | OmniFieldGroup omniFieldGroup -> omniFieldGroup.Group
            | OmniBattleGroup omniBattleGroup -> omniBattleGroup.Group
          Set = fun group groupModel ->
            match groupModel with
            | Group _ -> Group group
            | OmniFieldGroup omniFieldGroup -> OmniFieldGroup { omniFieldGroup with Group = group }
            | OmniBattleGroup omniBattleGroup -> OmniBattleGroup { omniBattleGroup with Group = group }}

    let groupIdLens =
        { Get = fun groupModel -> (get groupModel groupLens).Id
          Set = fun value groupModel -> set { get groupModel groupLens with Id = value } groupModel groupLens}

    let worldOptGroupModelFinder (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.GroupModels
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind address.[1] groupMap

    let worldGroupModelAdder (address : Address) world (child : GroupModel) =
        let optGroupMap = Map.tryFind address.[0] world.GroupModels
        match optGroupMap with
        | None ->
            { world with GroupModels = Map.singleton address.[0] <| Map.singleton address.[1] child }
        | Some groupMap ->
            let groupMap' = Map.add address.[1] child groupMap
            { world with GroupModels = Map.add address.[0] groupMap' world.GroupModels }

    let worldGroupModelRemover (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.GroupModels
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let groupMap' = Map.remove address.[1] groupMap
            { world with GroupModels = Map.add address.[0] groupMap' world.GroupModels }

    let getWorldGroupModelWithLens address world lens =
        get (getChild worldOptGroupModelFinder address world) lens

    let setWorldGroupModelWithLens child address world lens =
        let group = getChild worldOptGroupModelFinder address world
        let group' = set child group lens
        setChild worldGroupModelAdder worldGroupModelRemover address world group'

    let getWorldOptGroupModelWithLens address world lens =
        let optChild = getOptChild worldOptGroupModelFinder address world
        match optChild with None -> None | Some child -> Some (get child lens)

    let setWorldOptGroupModelWithLens optChild address world lens =
        match optChild with
        | None -> setOptChild worldGroupModelAdder worldGroupModelRemover address world None
        | Some child ->
            let optChildModel = getOptChild worldOptGroupModelFinder address world
            match optChildModel with
            | None -> failwith "Cannot change a non-existent group."
            | Some childModel ->
                let childModel' = set child childModel lens
                setChild worldGroupModelAdder worldGroupModelRemover address world childModel'

    let worldGroupModelLens address =
        { Get = fun world -> Option.get <| worldOptGroupModelFinder address world
          Set = fun group world -> worldGroupModelAdder address world group }

    let worldOptGroupModelLens address =
        { Get = fun world -> worldOptGroupModelFinder address world
          Set = fun optGroup world -> match optGroup with None -> worldGroupModelRemover address world | Some entity -> worldGroupModelAdder address world entity }

    let worldGroupModelsLens address =
        { Get = fun world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.GroupModels with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group model address '" + str address + "'."
          Set = fun groupModels world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.GroupModels with
                | None -> { world with GroupModels = Map.add screenLun groupModels world.GroupModels }
                | Some groupMap -> { world with GroupModels = Map.add screenLun (Map.addMany (Map.toSeq groupModels) groupMap) world.GroupModels }
            | _ -> failwith <| "Invalid group model address '" + str address + "'." }

    let worldGroupLens address =
        { Get = fun world -> getWorldGroupModelWithLens address world groupLens
          Set = fun group world -> setWorldGroupModelWithLens group address world groupLens }

    let worldOptGroupLens address =
        { Get = fun world -> getWorldOptGroupModelWithLens address world groupLens
          Set = fun optGroup world -> setWorldOptGroupModelWithLens optGroup address world groupLens }