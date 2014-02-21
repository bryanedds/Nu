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
          Set = fun optGroup world -> match optGroup with None -> worldGroupRemover address world | Some group -> worldGroupAdder address world group }

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
          Xtension = { OptXTypeName = Some <| Lun.make "GroupDispatcher"; XFields = Map.empty }}

    let writeGroupEntitiesToXml (writer : XmlWriter) (entities : Map<Lun, Entity>) =
        for entityKvp in entities do
            Entities.writeEntityToXml writer entityKvp.Value

    let writeGroupToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        writeModelProperties writer group
        writeGroupEntitiesToXml writer entities

    let loadEntitiesFromXml (groupNode : XmlNode) world =
        let entityNodes = groupNode.SelectNodes "Entity"
        let entities =
            Seq.map
                (fun entityNode -> Entities.loadEntityFromXml entityNode world)
                (System.Linq.Enumerable.Cast entityNodes) // TODO: create Miscellanea.enumCast function
        Seq.toList entities

    let loadGroupFromXml (groupNode : XmlNode) world =
        let group = makeDefaultGroup ()
        let entities = loadEntitiesFromXml (groupNode : XmlNode) world
        setModelProperties groupNode group
        (group, entities)

    let writeGroupFile group entities fileName world =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        writeGroupToXml writer group entities
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFile (fileName : string) world =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.Item "Root"
        let groupNode = rootNode.FirstChild
        loadGroupFromXml groupNode world