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

        static member getPublishChanges (group : Group) = group.PublishChanges
        static member setPublishChanges value (group : Group) = { group with PublishChanges = value }
        static member getPersistent (group : Group) = group.Persistent
        static member setPersistent value (group : Group) = { group with Persistent = value }

        static member register (group : Group) address world =
            group.DispatcherNp.Register (group, address, world)
        
        static member unregister (group : Group) address world =
            group.DispatcherNp.Unregister (group, address, world)

        static member dispatchesAs (dispatcherTargetType : Type) (group : Group) =
            Reflection.dispatchesAs dispatcherTargetType group.DispatcherNp

        static member make dispatcher optName =
            let id = Core.makeId ()
            { Group.Id = id
              Name = match optName with None -> acstring id | Some name -> name
              PublishChanges = true
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
                let (_, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (group, _) -> Some group
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupAdder (group : Group) (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        let groupMap = Map.add groupName (group, entityMap) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                    | None ->
                        let groupMap = Map.add groupName (group, Map.empty) groupMap
                        let screenMap = Map.add screenName (screen, groupMap) screenMap
                        { world with Simulants = (game, screenMap) }
                | None -> failwith <| "Cannot add group '" + acstring address + "' to non-existent screen."
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupRemover (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                let (game, screenMap) = world.Simulants 
                match Map.tryFind screenName screenMap with
                | Some (screen, groupMap) ->
                    match Map.tryFind groupName groupMap with
                    | Some (_, entityMap) ->
                        if Map.isEmpty entityMap then
                            let groupMap = Map.remove groupName groupMap
                            let screenMap = Map.add screenName (screen, groupMap) screenMap
                            { world with Simulants = (game, screenMap) }
                        else failwith <| "Cannot remove group " + acstring address + ", which still contains entities."
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member containsGroup address world =
            Option.isSome <| World.optGroupFinder address world

        static member getOptGroup address world =
            World.optGroupFinder address world

        static member getGroupBy by address world =
            by ^^ Option.get ^^ World.getOptGroup address world
        
        static member getGroup address world =
            World.getGroupBy id address world

        static member getOptGroupHierarchy address world =
            match World.getOptGroup address world with
            | Some group ->
                let entityMap = World.getEntityMapInGroup address world
                Some (group, entityMap)
            | None -> None
        
        static member getGroupHierarchy address world =
            Option.get <| World.getOptGroupHierarchy address world

        static member getGroupAddressInScreen groupName screenAddress world =
            let address = satoga screenAddress groupName
            ignore <| World.getGroup address world // ensures address is valid
            address

        static member private setGroupWithoutEvent group address world =
            World.groupAdder group address world

        static member private setOptGroupWithoutEvent optGroup address world =
            match optGroup with 
            | Some group -> World.groupAdder group address world
            | None -> World.groupRemover address world

        static member setGroup group address world =
            let oldGroup = Option.get <| World.optGroupFinder address world
            let world = World.groupAdder group address world
            if group.PublishChanges
            then World.publish4 { OldSimulant = oldGroup } (GroupChangeEventAddress ->>- address) address world
            else world

        static member updateOptGroupW updater address world =
            match World.getOptGroup address world with
            | Some group ->
                let group = updater group world
                World.setGroup group address world
            | None -> world

        static member updateOptGroup updater address world =
            World.updateOptGroupW (fun group _ -> updater group) address world

        static member updateByOptGroup updater address world : World =
            match World.getOptGroup address world with
            | Some group -> updater group world
            | None -> world

        static member updateGroupW updater address world =
            let group = World.getGroup address world
            let group = updater group world
            World.setGroup group address world
        
        static member updateGroup updater address world =
            World.updateGroupW (fun group _ -> updater group) address world

        static member updateByGroup updater address world : World =
            let group = World.getGroup address world
            updater group world

        static member getGroupHierarchies addresses world =
            Seq.map (fun address -> World.getGroupHierarchy address world) addresses

        static member getGroupsBy by addresses world =
            Seq.map (fst >> by) <| World.getGroupHierarchies addresses world

        static member getGroups addresses world =
            World.getGroupsBy id addresses world

        static member getGroupMapInScreen (screenAddress : Screen Address) world =
            match screenAddress.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenAddress + "'."

        static member getGroupsInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueSeqBy fst groupHierarchies

        static member getGroupAddressesInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueListBy (fun (group : Group, _) -> satoga screenAddress group.Name) groupHierarchies

        static member updateGroupsW updater addresses world =
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses
        
        static member updateGroups updater addresses world =
            World.updateGroupsW (fun group _ -> updater group) addresses world

        static member updateGroupsInScreenW updater screenAddress world =
            let addresses = World.getGroupAddressesInScreen screenAddress world
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses

        static member updateGroupsInScreen updater addresses world =
            World.updateGroupsInScreenW (fun group _ -> updater group) addresses world

        static member filterGroupAddressesW pred addresses world =
            Seq.filter (fun address -> World.getGroupBy (fun group -> pred group world) address world) addresses

        static member filterGroupAddresses pred addresses world =
            World.filterGroupAddressesW (fun group _ -> pred group) addresses world

        static member private registerGroup group address world =
            Group.register group address world

        static member private unregisterGroup group address world =
            Group.unregister group address world

        static member removeGroupImmediate address world =
            let world = World.publish4 () (RemovingEventAddress ->>- address) address world
            match World.getOptGroup address world with
            | Some group ->
                let (group, world) = World.unregisterGroup group address world
                let entityAddresses = World.getEntityAddressesInGroup address world
                let world = snd <| World.removeEntitiesImmediate entityAddresses world
                let world = World.setOptGroupWithoutEvent None address world
                (Some group, world)
            | None -> (None, world)

        static member removeGroup address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeGroupImmediate address world }
            World.addTask task world

        static member removeGroupsImmediate addresses world =
            List.foldBack
                (fun address (groups, world) ->
                    let (group, world) = World.removeGroupImmediate address world
                    (group :: groups, world))
                addresses
                ([], world)

        static member removeGroups addresses world =
            snd <| World.removeGroupsImmediate addresses world

        static member addGroup groupHierarchy address world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup address world then
                let world = World.setGroupWithoutEvent group address world
                let world = snd <| World.addEntities entities address world
                let (group, world) = World.registerGroup group address world
                let world = World.publish4 () (AddEventAddress ->>- address) address world
                (group, world)
            else failwith <| "Adding a group that the world already contains at address '" + acstring address + "'."

        static member addGroups groupHierarchies (screenAddress : Screen Address) world =
            Map.fold
                (fun (groups, world) groupName groupHierarchy ->
                    let (group, world) = World.addGroup groupHierarchy (satoga screenAddress groupName) world
                    (group :: groups, world))
                ([], world)
                groupHierarchies

        static member makeGroup dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let group = Group.make dispatcher optName
            Reflection.attachFields dispatcher group
            group

        static member writeGroupHierarchy (writer : XmlWriter) groupHierarchy world =
            let (group : Group, entities) = groupHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (group.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology3 writer group
            writer.WriteStartElement EntitiesNodeName
            World.writeEntities writer entities world
            writer.WriteEndElement ()

        static member writeGroupHierarchyToFile (filePath : string) groupHierarchy world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create <| (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement RootNodeName
            writer.WriteStartElement GroupNodeName
            World.writeGroupHierarchy writer groupHierarchy world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.WriteEndDocument ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        // TODO: implement these.
        //static member writeGroupHierarchyToAsset (assetTag : AssetTag) groupHierarchy world =
        //static member readGroupHierarchyFromAsset (assetTag : AssetTag) groupHierarchy world =

        static member writeGroupHierarchies (writer : XmlWriter) groupHierarchies world =
            let groupHierarchies =
                List.sortBy
                    (fun (group : Group, _) -> group.CreationTimeNp)
                    (Map.toValueList groupHierarchies)
            let groupHierarchies = List.filter (fun (group : Group, _) -> group.Persistent) groupHierarchies
            for groupHierarchy in groupHierarchies do
                writer.WriteStartElement GroupNodeName
                World.writeGroupHierarchy writer groupHierarchy world
                writer.WriteEndElement ()

        static member readGroupHierarchy (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =

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

        static member readGroupHierarchies (parentNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =
            match parentNode.SelectSingleNode GroupsNodeName with
            | null -> Map.empty
            | groupsNode ->
                let groupNodes = groupsNode.SelectNodes GroupNodeName
                Seq.fold
                    (fun groupHierarchies groupNode ->
                        let groupHierarchy = World.readGroupHierarchy groupNode defaultDispatcherName defaultEntityDispatcherName world
                        let groupName = (fst groupHierarchy).Name
                        Map.add groupName groupHierarchy groupHierarchies)
                    Map.empty
                    (enumerable groupNodes)

        static member readGroupHierarchyFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroupHierarchy groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world