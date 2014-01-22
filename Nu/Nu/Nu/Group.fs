namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Nu
open Nu.Core
open Nu.DomainModel
module GroupModule =

    let groupIdLens =
        { Get = fun group -> group.Id
          Set = fun value group -> { group with Id = value }}

    let groupXtensionLens =
        { Get = fun group -> group.Xtension
          Set = fun value group -> { group with Xtension = value }}

    let groupDynamicLens memberName =
        { Get = fun (group : Group) -> (?) group memberName
          Set = fun value group -> (?<-) group memberName value }

    let worldOptGroupFinder (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.Groups
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind address.[1] groupMap

    let worldGroupAdder (address : Address) world (child : Group) =
        let optGroupMap = Map.tryFind address.[0] world.Groups
        match optGroupMap with
        | None ->
            { world with Groups = Map.singleton address.[0] <| Map.singleton address.[1] child }
        | Some groupMap ->
            let groupMap' = Map.add address.[1] child groupMap
            { world with Groups = Map.add address.[0] groupMap' world.Groups }

    let worldGroupRemover (address : Address) world =
        let optGroupMap = Map.tryFind address.[0] world.Groups
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let groupMap' = Map.remove address.[1] groupMap
            { world with Groups = Map.add address.[0] groupMap' world.Groups }

    let worldGroupLens address =
        { Get = fun world -> Option.get <| worldOptGroupFinder address world
          Set = fun group world -> worldGroupAdder address world group }

    let worldOptGroupLens address =
        { Get = fun world -> worldOptGroupFinder address world
          Set = fun optGroup world -> match optGroup with None -> worldGroupRemover address world | Some entity -> worldGroupAdder address world entity }

    let worldGroupsLens address =
        { Get = fun world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.Groups with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group address '" + str address + "'."
          Set = fun groups world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.Groups with
                | None -> { world with Groups = Map.add screenLun groups world.Groups }
                | Some groupMap -> { world with Groups = Map.add screenLun (Map.addMany (Map.toSeq groups) groupMap) world.Groups }
            | _ -> failwith <| "Invalid group address '" + str address + "'." }

    let makeDefaultGroup () =
        { Group.Id = getNuId ()
          Xtension = { OptName = Some <| Lun.make "groupDispatcher"; Fields = Map.empty }}

    let writeGroupEntitiesToXml (writer : XmlWriter) (entityModels : Map<Lun, EntityModel>) =
        for entityModelKvp in entityModels do
            Entities.writeEntityModelToXml writer entityModelKvp.Value

    let writeGroupToXml (writer : XmlWriter) group entityModels =
        writer.WriteStartElement typeof<Group>.Name
        writeModelPropertiesMany writer "Nu.Group+Group" [group :> obj]
        writeGroupEntitiesToXml writer entityModels

    let loadEntityModelsFromXml (groupNode : XmlNode) =
        let entityModelNodes = groupNode.SelectNodes "EntityModel"
        Seq.toList <| Seq.map Entities.loadEntityModelFromXml (System.Linq.Enumerable.Cast entityModelNodes)

    let loadGroupFromXml (groupNode : XmlNode) =
        let group = makeDefaultGroup ()
        let entityModels = loadEntityModelsFromXml (groupNode : XmlNode)
        setModelProperties3 (fun _ -> ()) groupNode group
        (group, entityModels)

    let writeGroupFile group entityModels fileName world =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        writeGroupToXml writer group entityModels
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFile (fileName : string) world =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.Item "Root"
        let groupNode = rootNode.FirstChild
        loadGroupFromXml groupNode