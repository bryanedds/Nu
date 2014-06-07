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

    type Group with
        member this.Register (address : Address, entities : Entity list, world : World) : World = this?Register (address, entities, world)
        member this.Unregister (address : Address, world : World) : World = this?Unregister (address, world)

    type GroupDispatcher () =
        
        abstract member Register : Group * Address * Entity list * World -> World
        default this.Register (_, address, entities, world) = addEntities address entities world

        abstract member Unregister : Group * Address * World -> World
        default this.Unregister (_, address, world) = removeEntities address world

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

    let private worldOptGroupFinder (address : Address) world =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None -> None
        | Some groupMap -> Map.tryFind (List.at 1 address) groupMap

    let private worldGroupAdder (address : Address) world child =
        let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
        match optGroupMap with
        | None ->
            { world with Groups = Map.singleton (List.at 0 address) <| Map.singleton (List.at 1 address) child }
        | Some groupMap ->
            let groupMap' = Map.add (List.at 1 address) child groupMap
            { world with Groups = Map.add (List.at 0 address) groupMap' world.Groups }

    let private worldGroupRemover (address : Address) world =
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
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'."
          Set = fun groups world ->
            match address with
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | None -> { world with Groups = Map.add screenStr groups world.Groups }
                | Some groupMap -> { world with Groups = Map.add screenStr (Map.addMany (Map.toSeq groups) groupMap) world.Groups }
            | _ -> failwith <| "Invalid group address '" + addrToStr address + "'." }
            
    let withWorldGroup fn address world = withWorldSimulant worldGroupLens
    let withWorldOptGroup fn address world = withWorldOptSimulant worldOptGroupLens
    let tryWithWorldGroup fn address world = tryWithWorldSimulant worldOptGroupLens worldGroupLens

    let makeDefaultGroup () =
        { Group.Id = getNuId ()
          Xtension = { XFields = Map.empty; OptXDispatcherName = Some typeof<GroupDispatcher>.Name; CanDefault = true; Sealed = false }}

    let registerGroup address entities (group : Group) world =
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
        let world'' = registerGroup address entities group world'
        set group world'' <| worldGroupLens address

    let addGroups screenAddress groupDescriptors world =
        List.fold
            (fun world' (groupName, group, entities) -> addGroup (screenAddress @ [groupName]) group entities world')
            world
            groupDescriptors

    let writeGroupEntitiesToXml (writer : XmlWriter) (entities : Map<string, Entity>) =
        for entityKvp in entities do
            writeEntityToXml writer entityKvp.Value

    let writeGroupToXml (writer : XmlWriter) group entities =
        writer.WriteStartElement typeof<Group>.Name
        Xtension.writePropertiesToXmlWriter writer group
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