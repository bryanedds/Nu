namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module GroupModule =

    type Group with

        static member register address (group : Group) (world : World) : Group * World =
            group.DispatcherNp.Register (address, group, world)
        
        static member unregister address (group : Group) (world : World) : Group * World =
            group.DispatcherNp.Unregister (address, group, world)

        static member make dispatcher optName =
            let id = Core.makeId ()
            { Group.Id = id
              Name = match optName with None -> acstring id | Some name -> name
              Persistent = true
              CreationTimeNp = DateTime.UtcNow
              DispatcherNp = dispatcher
              Xtension = { XFields = Map.empty; CanDefault = false; Sealed = true } }

[<AutoOpen>]
module WorldGroupModule =

    type World with

        static member private optGroupFinder (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap -> Map.tryFind groupName groupMap
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupAdder (address : Group Address) world child =
            match address.Names with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap ->
                    let groupMap = Map.add groupName child groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None -> { world with Groups = Map.singleton screenName <| Map.singleton groupName child }
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupRemover (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap ->
                    let groupMap = Map.remove groupName groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member getGroup address world = Option.get <| World.optGroupFinder address world
        static member setGroup address group world = World.groupAdder address world group
        static member getOptGroup address world = World.optGroupFinder address world
        static member containsGroup address world = Option.isSome <| World.getOptGroup address world
        static member private setOptGroup address optGroup world =
            match optGroup with
            | Some group -> World.setGroup address group world
            | None -> World.groupRemover address world

        static member getOptGroup' address world =
            match World.getOptGroup address world with
            | Some group ->
                let entities = World.getEntities address world
                Some (group, entities)
            | None -> None
        
        static member getGroup' address world =
            Option.get <| World.getOptGroup' address world

        static member getGroups (screenAddress : Screen Address) world =
            match screenAddress.Names with
            | [screenName] ->
                match Map.tryFind screenName world.Groups with
                | Some groupMap -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenAddress + "'."

        static member getGroups3 (screenAddress : Screen Address) groupNames world =
            let groupNames = Set.ofSeq groupNames
            let groups = World.getGroups screenAddress world
            Map.filter (fun groupName _ -> Set.contains groupName groupNames) groups

        static member getGroups' screenAddress world =
            let groups = World.getGroups screenAddress world
            Map.map
                (fun groupName group ->
                    let groupAddress = satoga screenAddress groupName
                    let entities = World.getEntities groupAddress world
                    (group, entities))
                groups

        static member private registerGroup address group world =
            Group.register address group world

        static member private unregisterGroup address group world =
            Group.unregister address group world

        static member removeGroupImmediate address group world =
            let world = World.publish4 address (RemovingEventAddress ->>- address) () world
            let (group, world) = World.unregisterGroup address group world
            let entities = World.getEntities address world
            let world = snd <| World.removeEntitiesImmediate address entities world
            let world = World.setOptGroup address None world
            (group, world)

        static member removeGroup address (group : Group) world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world ->
                    match World.getOptGroup address world with
                    | Some group -> snd <| World.removeGroupImmediate address group world
                    | None -> world }
            let world = World.addTask task world
            (group, world)

        static member removeGroupsImmediate (screenAddress : Screen Address) groups world =
            World.transformSimulants World.removeGroupImmediate satoga screenAddress groups world

        static member removeGroups (screenAddress : Screen Address) groups world =
            World.transformSimulants World.removeGroup satoga screenAddress groups world

        static member addGroup address groupHierarchy world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup address world then
                let (group, world) =
                    match World.getOptGroup address world with
                    | Some _ -> World.removeGroupImmediate address group world
                    | None -> (group, world)
                let world = World.setGroup address group world
                let world = snd <| World.addEntities address entities world
                let (group, world) = World.registerGroup address group world
                let world = World.publish4 address (AddEventAddress ->>- address) () world
                (group, world)
            else failwith <| "Adding a group that the world already contains at address '" + acstring address + "'."

        static member addGroups screenAddress groupsHierarchy world =
            Map.fold
                (fun (groups, world) groupName groupHierarchy ->
                    let (group, world) = World.addGroup (satoga screenAddress groupName) groupHierarchy world
                    (group :: groups, world))
                ([], world)
                groupsHierarchy
    
        static member writeGroup (writer : XmlWriter) groupHierarchy world =
            let (group : Group, entities) = groupHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (group.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology writer group
            writer.WriteStartElement EntitiesNodeName
            World.writeEntities writer entities world
            writer.WriteEndElement ()

        static member writeGroupToFile (filePath : string) groupHierarchy world =
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (filePath, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GroupNodeName
            World.writeGroup writer groupHierarchy world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.WriteEndDocument ()

        static member writeGroups (writer : XmlWriter) groupsHierarchy world =
            let groupsHierarchy =
                List.sortBy
                    (fun (group : Group, _) -> group.CreationTimeNp)
                    (Map.toValueList groupsHierarchy)
            let groupsHierarchy = List.filter (fun (group : Group, _) -> group.Persistent) groupsHierarchy
            for groupHierarchy in groupsHierarchy do
                writer.WriteStartElement GroupNodeName
                World.writeGroup writer groupHierarchy world
                writer.WriteEndElement ()

        static member readGroup (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Serialization.readDispatcherName defaultDispatcherName groupNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GroupDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note <| "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName world.Components.GroupDispatchers
            
            // make the bare group with name as id
            let group = Group.make dispatcher None
            
            // attach the group's instrinsic fields from its dispatcher if any
            Reflection.attachFields group.DispatcherNp group

            // read the groups's properties
            Serialization.readPropertiesToTarget groupNode group
            
            // read the group's entities
            let entities = World.readEntities (groupNode : XmlNode) defaultEntityDispatcherName world

            // return the initialized group and entities
            (group, entities)

        static member readGroupFromFile filePath world =
            let document = XmlDocument ()
            document.Load (filePath : string)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

        static member readGroups (parentNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =
            match parentNode.SelectSingleNode GroupsNodeName with
            | null -> Map.empty
            | groupsNode ->
                let groupNodes = groupsNode.SelectNodes GroupNodeName
                Seq.fold
                    (fun groupsHierarchy groupNode ->
                        let groupHierarchy = World.readGroup groupNode defaultDispatcherName defaultEntityDispatcherName world
                        let groupName = (fst groupHierarchy).Name
                        Map.add groupName groupHierarchy groupsHierarchy)
                    Map.empty
                    (enumerable groupNodes)

        static member makeGroup dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let group = Group.make dispatcher optName
            Reflection.attachFields dispatcher group
            group