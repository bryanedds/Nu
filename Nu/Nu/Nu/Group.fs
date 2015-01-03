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

        /// Register a group when adding it to a screen.
        static member register (group : Group) address world =
            group.DispatcherNp.Register (group, address, world)
        
        /// Unregister a group when removing it from a screen.
        static member unregister (group : Group) address world =
            group.DispatcherNp.Unregister (group, address, world)

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        static member dispatchesAs (dispatcherTargetType : Type) (group : Group) =
            Reflection.dispatchesAs dispatcherTargetType group.DispatcherNp

        /// Make a group.
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

        /// Query that the world contains a group at the given address.
        static member containsGroup address world =
            Option.isSome <| World.optGroupFinder address world

        /// Try to get a group at the given address.
        static member getOptGroup address world =
            World.optGroupFinder address world

        /// Get a group at the given address (failing with an exception otherwise), then
        /// transform it with the 'by' procudure.
        static member getGroupBy by address world =
            by ^^ Option.get ^^ World.getOptGroup address world

        /// Get a group at the given address (failing with an exception otherwise).
        static member getGroup address world =
            World.getGroupBy id address world

        /// Get a group with the given name in a screen with the given address (failing with an
        /// exception otherwise).
        static member getGroupInScreen groupName screenAddress world =
            World.getGroup (satoga screenAddress groupName) world

        /// Get a group's address with the given name in a group with the given address (failing
        /// with an exception otherwise).
        static member getGroupAddressInScreen groupName screenAddress world =
            let address = satoga screenAddress groupName
            ignore <| World.getGroup address world // ensure address is valid
            address

        /// Try to get a group hierarchy (that is, a group with a map to all of its entities) at
        /// the given address.
        static member getOptGroupHierarchy address world =
            match World.getOptGroup address world with
            | Some group ->
                let entityMap = World.getEntityMapInGroup address world
                Some (group, entityMap)
            | None -> None

        /// Get a group hierarchy (that is, a group with a map to all of its entities) at the given
        /// address (failing with an exception if there isn't one).
        static member getGroupHierarchy address world =
            Option.get <| World.getOptGroupHierarchy address world

        /// Get a group's address with the given name in the screen with the given address (failing
        /// with an exception if there isn't one).
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

        /// Set a group at the given address (failing with an exception if one doesn't exist).
        static member setGroup group address world =
            let oldGroup = Option.get <| World.optGroupFinder address world
            let world = World.groupAdder group address world
            if group.PublishChanges
            then World.publish4 { OldSimulant = oldGroup } (GroupChangeEventAddress ->>- address) address world
            else world

        /// Try to update a group with the given 'updater' procedure at the given address. Also
        /// passes the current world value to the procedure.
        static member updateOptGroupW updater address world =
            match World.getOptGroup address world with
            | Some group ->
                let group = updater group world
                World.setGroup group address world
            | None -> world

        /// Try to update a group with the given 'updater' procedure at the given addres
        static member updateOptGroup updater address world =
            World.updateOptGroupW (fun group _ -> updater group) address world
            
        /// Try to update the world with the given 'updater' procedure that uses the group at
        /// given address in its computation.
        static member updateByOptGroup updater address world : World =
            match World.getOptGroup address world with
            | Some group -> updater group world
            | None -> world
            
        /// Update a group with the given 'updater' procedure at the given address. Also passes
        /// the current world value to the procedure.
        static member updateGroupW updater address world =
            let group = World.getGroup address world
            let group = updater group world
            World.setGroup group address world
        
        /// Update a group with the given 'updater' procedure at the given address.
        static member updateGroup updater address world =
            World.updateGroupW (fun group _ -> updater group) address world
            
        /// Update the world with the given 'updater' procedure that uses the group at given
        /// address in its computation.
        static member updateByGroup updater address world : World =
            let group = World.getGroup address world
            updater group world

        /// Get the group hierarches at the given addresses.
        static member getGroupHierarchies addresses world =
            Seq.map (fun address -> World.getGroupHierarchy address world) addresses
            
        /// Get the groups at the given addresses as transformed them with the 'by'
        /// procedure.
        static member getGroupsBy by addresses world =
            Seq.map (fst >> by) <| World.getGroupHierarchies addresses world

        /// Get the groups at the given addresses.
        static member getGroups addresses world =
            World.getGroupsBy id addresses world

        /// Get all the groups in the screen at the given address as mapped by their names.
        static member getGroupMapInScreen (screenAddress : Screen Address) world =
            match screenAddress.Names with
            | [screenName] ->
                let (_, screenMap) = world.Simulants
                match Map.tryFind screenName screenMap with
                | Some (_, groupMap) -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenAddress + "'."

        /// Get all the groups in the screen at the given address.
        static member getGroupsInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueSeqBy fst groupHierarchies

        /// Get all the group addresses in the screen at the given address.
        static member getGroupAddressesInScreen screenAddress world =
            let groupHierarchies = World.getGroupMapInScreen screenAddress world
            Map.toValueListBy (fun (group : Group, _) -> satoga screenAddress group.Name) groupHierarchies
            
        /// Update the groups at the given address with the given 'updater' procedure. Also
        /// passes the current world value to the procedure.
        static member updateGroupsW updater addresses world =
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses
        
        /// Update the groups at the given address with the given 'updater' procedure.
        static member updateGroups updater addresses world =
            World.updateGroupsW (fun group _ -> updater group) addresses world
            
        /// Update all groups in the screen at the given address with then given the 'updater'
        /// procedure. Also passes the current world value to the procedure.
        static member updateGroupsInScreenW updater screenAddress world =
            let addresses = World.getGroupAddressesInScreen screenAddress world
            Seq.fold (fun world address -> World.updateGroupW updater address world) world addresses
            
        /// Update all groups in the screen at the given address with then given the 'updater' procedure.
        static member updateGroupsInScreen updater addresses world =
            World.updateGroupsInScreenW (fun group _ -> updater group) addresses world
            
        /// Filter the given group addresses by applying the 'pred' procedure to each group at
        /// its respected address. Also passes the current world value to the procedure.
        static member filterGroupAddressesW pred addresses world =
            Seq.filter (fun address -> World.getGroupBy (fun group -> pred group world) address world) addresses
            
        /// Filter the given group addresses by applying the 'pred' procedure to each group at
        /// its respected address.
        static member filterGroupAddresses pred addresses world =
            World.filterGroupAddressesW (fun group _ -> pred group) addresses world

        static member private registerGroup group address world =
            Group.register group address world

        static member private unregisterGroup group address world =
            Group.unregister group address world
            
        /// Remove a group from the world immediately. Can be dangerous if existing in-flight
        /// subscriptions depend on the group's existence. Use with caution.
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
            
        /// Remove a group from the world on the next tick. Use this rather than
        /// removeEntityImmediate unless you need the latter's specific behavior.
        static member removeGroup address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world -> snd <| World.removeGroupImmediate address world }
            World.addTask task world
            
        /// Remove multiple groups from the world immediately. Can be dangerous if existing
        /// in-flight subscriptions depend on any of the groups' existences. Use with caution.
        static member removeGroupsImmediate addresses world =
            List.foldBack
                (fun address (groups, world) ->
                    let (group, world) = World.removeGroupImmediate address world
                    (group :: groups, world))
                addresses
                ([], world)
                
        /// Remove multiple groups from the world. Use this rather than removeEntitiesImmediate
        /// unless you need the latter's specific behavior.
        static member removeGroups addresses world =
            snd <| World.removeGroupsImmediate addresses world

        /// Add a group at the given address to the world.
        static member addGroup groupHierarchy address world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup address world then
                let world = World.setGroupWithoutEvent group address world
                let world = snd <| World.addEntities entities address world
                let (group, world) = World.registerGroup group address world
                let world = World.publish4 () (AddEventAddress ->>- address) address world
                (group, world)
            else failwith <| "Adding a group that the world already contains at address '" + acstring address + "'."

        /// Add multiple groups to the screen at the given address.
        static member addGroups groupHierarchies (screenAddress : Screen Address) world =
            Map.fold
                (fun (groups, world) groupName groupHierarchy ->
                    let (group, world) = World.addGroup groupHierarchy (satoga screenAddress groupName) world
                    (group :: groups, world))
                ([], world)
                groupHierarchies

        /// Make a group (does NOT add the group to the world!)
        static member makeGroup dispatcherName optName world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let group = Group.make dispatcher optName
            Reflection.attachFields dispatcher group
            group

        /// Write a group hierarchy to an xml writer.
        static member writeGroupHierarchy (writer : XmlWriter) groupHierarchy world =
            let (group : Group, entities) = groupHierarchy
            writer.WriteAttributeString (DispatcherNameAttributeName, (group.DispatcherNp.GetType ()).Name)
            Serialization.writePropertiesFromTarget tautology3 writer group
            writer.WriteStartElement EntitiesNodeName
            World.writeEntities writer entities world
            writer.WriteEndElement ()

        /// Write a group hierarchy to an xml file.
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

        /// Write multiple group hierarchies to an xml writer.
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

        /// Read a group hierarchy from an xml node.
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

        /// Read a group hierarchy from an xml file.
        static member readGroupHierarchyFromFile (filePath : string) world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[RootNodeName]
            let groupNode = rootNode.[GroupNodeName]
            World.readGroupHierarchy groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name world

        /// Read multiple group hierarchies from an xml node.
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