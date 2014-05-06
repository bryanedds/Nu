namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
open Prime
open Nu
open Nu.NuCore
open Nu.Sim
open Nu.Entity

[<AutoOpen>]
module GroupModule =

    type GroupDispatcher () =
        
        abstract member Register : Address * Group * Entity list * World -> World
        default this.Register (address, _, entities, world) = addEntities address entities world

        abstract member Unregister : Address * Group * World -> World
        default this.Unregister (address, _, world) = removeEntities address world

module Group =

    let groupIdLens =
        { Get = fun group -> group.Id
          Set = fun value group -> { group with Id = value }}

    let groupXtensionLens =
        { Get = fun group -> group.Xtension
          Set = fun value group -> { group with Xtension = value }}

    let groupDynamicLens memberName =
        { Get = fun (group : Group) -> (?) group memberName
          Set = fun value group -> (?<-) group memberName value }

    let private worldOptGroupFinder address world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind (List.at 1 address) groupMap

    let private worldGroupAdder address world child =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None ->
            { world with Groups = Map.singleton (List.at 0 address) <| Map.singleton (List.at 1 address) child }
        | Some groupMap ->
            let groupMap' = Map.add (List.at 1 address) child groupMap
            { world with Groups = Map.add (List.at 0 address) groupMap' world.Groups }

    let private worldGroupRemover address world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None -> world
        | Some groupMap ->
            let groupMap' = Map.remove (List.at 1 address) groupMap
            { world with Groups = Map.add (List.at 0 address) groupMap' world.Groups }

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
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'."
          Set = fun groups world ->
            match address with
            | [screenLun] ->
                match Map.tryFind screenLun world.Groups with
                | None -> { world with Groups = Map.add screenLun groups world.Groups }
                | Some groupMap -> { world with Groups = Map.add screenLun (Map.addMany (Map.toSeq groups) groupMap) world.Groups }
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'." }

    let makeDefaultGroup () =
        { Group.Id = getNuId ()
          Xtension = { OptXTypeName = Some <| Lun.make typeof<GroupDispatcher>.Name; XFields = Map.empty; IsSealed = false }}

    let registerGroup address (group : Group) entities world =
        group.Register (address, entities, world)

    let unregisterGroup address world =
        let group = get world <| worldGroupLens address
        group.Unregister (address, world)

    let removeGroup address world =
        let world' = unregisterGroup address world
        set None world' (worldOptGroupLens address)

    let removeGroups address world =
        let groups = get world <| worldGroupsLens address
        Map.fold
            (fun world' groupName _ -> removeGroup (address @ [groupName]) world')
            world
            groups

    let addGroup address (group : Group) entities world =
        let world' =
            match get world <| worldOptGroupLens address with
            | None -> world
            | Some _ -> removeGroup address world
        let world'' = registerGroup address group entities world'
        set group world'' <| worldGroupLens address

    let addGroups address groupDescriptors world =
        List.fold
            (fun world' (groupName, group, entities) -> addGroup (address @ [groupName]) group entities world')
            world
            groupDescriptors

    let writeGroupEntitiesToXml (writer : XmlWriter) (entities : Map<Lun, Entity>) =
        for entityKvp in entities do
            writeEntityToXml writer entityKvp.Value

    let writeGroupToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        Xtension.writeProperties writer group
        writeGroupEntitiesToXml writer entities

    let readEntitiesFromXml (groupNode : XmlNode) seal world =
        let entityNodes = groupNode.SelectNodes "Entity"
        let entities =
            Seq.map
                (fun entityNode -> readEntityFromXml entityNode seal world)
                (enumerable entityNodes)
        Seq.toList entities

    let readGroupFromXml (groupNode : XmlNode) seal world =
        let group = makeDefaultGroup ()
        let entities = readEntitiesFromXml (groupNode : XmlNode) seal world
        Xtension.readProperties groupNode group
        (group, entities)

    let saveGroupFile optGameDispatcherDescriptor group entities fileName world =
        use file = File.Open (fileName, FileMode.Create)
        let writerSettings = XmlWriterSettings ()
        writerSettings.Indent <- true
        use writer = XmlWriter.Create (file, writerSettings)
        writer.WriteStartDocument ()
        writer.WriteStartElement "Root"
        match optGameDispatcherDescriptor with
        | None -> ()
        | Some node ->
            writer.WriteStartElement "GameDispatcher"
            writer.WriteElementString ("AssemblyFileName", fst node)
            writer.WriteElementString ("FullName", snd node)
            writer.WriteEndElement ()
        writeGroupToXml writer group entities
        writer.WriteEndElement ()
        writer.WriteEndDocument ()

    let loadGroupFile (fileName : string) world seal activatesGameDispatcher =
        let document = XmlDocument ()
        document.Load fileName
        let rootNode = document.["Root"]
        let (someDescriptor, world') =
                match Seq.tryFind (fun (node : XmlNode) -> node.Name = "GameDispatcher") <| enumerable rootNode.ChildNodes with
                | None -> (None, world)
                | Some gameDispatcherNode ->
                    let assemblyFileName = gameDispatcherNode.["AssemblyFileName"].InnerText
                    let gameDispatcherFullName = gameDispatcherNode.["FullName"].InnerText
                    let someDescriptor = Some (assemblyFileName, gameDispatcherFullName)
                    if activatesGameDispatcher then
                        let world' = activateGameDispatcher assemblyFileName gameDispatcherFullName world
                        (someDescriptor, world')
                    else (someDescriptor, world)
        let groupNode = rootNode.["Group"]
        let (group, entities) = readGroupFromXml groupNode seal world'
        (someDescriptor, group, entities, world')