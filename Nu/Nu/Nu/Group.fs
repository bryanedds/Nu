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

        static member private groupAdder child (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                match Map.tryFind screenName world.Groups with
                | Some groupMap ->
                    let groupMap = Map.add groupName child groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None ->
                    let groupMap = Map.add screenName (Map.singleton groupName child) world.Groups
                    { world with Groups = groupMap }
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member private groupRemover (address : Group Address) world =
            match address.Names with
            | [screenName; groupName] ->
                match Map.tryFind screenName world.Groups with
                | Some groupMap ->
                    let groupMap = Map.remove groupName groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None -> world
            | _ -> failwith <| "Invalid group address '" + acstring address + "'."

        static member getGroupBy by address world = by ^^ Option.get ^^ World.optGroupFinder address world
        static member getGroup address world = World.getGroupBy id address world
        static member setGroup group address world = World.groupAdder group address world
        static member updateGroupW updater address world =
            let group = World.getGroup address world
            let group = updater group world
            World.setGroup group address world
        static member updateGroup updater address world = World.updateGroupW (fun group _ -> updater group) address world

        static member getOptGroup address world = World.optGroupFinder address world
        static member containsGroup address world = Option.isSome <| World.getOptGroup address world
        static member private setOptGroup optGroup address world =
            match optGroup with
            | Some group -> World.setGroup group address world
            | None -> World.groupRemover address world

        static member getOptGroupHierarchy address world =
            match World.getOptGroup address world with
            | Some group ->
                let entityMap = World.getEntityMap address world
                Some (group, entityMap)
            | None -> None
        
        static member getGroupHierarchy address world =
            Option.get <| World.getOptGroupHierarchy address world

        static member getGroupMap (screenAddress : Screen Address) world =
            match screenAddress.Names with
            | [screenName] ->
                match Map.tryFind screenName world.Groups with
                | Some groupMap -> groupMap
                | None -> Map.empty
            | _ -> failwith <| "Invalid screen address '" + acstring screenAddress + "'."

        static member getGroups screenAddress world =
            let groupMap = World.getGroupMap screenAddress world
            Map.toValueSeq groupMap

        static member getGroupMap3 groupNames (screenAddress : Screen Address) world =
            let groupNames = Set.ofSeq groupNames
            let groupMap = World.getGroupMap screenAddress world
            Map.filter (fun groupName _ -> Set.contains groupName groupNames) groupMap

        static member getGroups3 groupNames screenAddress world =
            let groups = World.getGroupMap3 screenAddress groupNames world
            Map.toValueSeq groups

        static member getGroupHierarchies screenAddress world =
            let groupMap = World.getGroupMap screenAddress world
            Map.map
                (fun groupName group ->
                    let groupAddress = satoga screenAddress groupName
                    let entityMap = World.getEntityMap groupAddress world
                    (group, entityMap))
                groupMap

        static member private registerGroup group address world =
            Group.register group address world

        static member private unregisterGroup group address world =
            Group.unregister group address world

        static member removeGroupImmediate group address world =
            let world = World.publish4 () (RemovingEventAddress ->>- address) address world
            let (group, world) = World.unregisterGroup group address world
            let entityMap = World.getEntityMap address world
            let world = snd <| World.removeEntitiesImmediate entityMap address world
            let world = World.setOptGroup None address world
            (group, world)

        static member removeGroup (group : Group) address world =
            let task =
                { ScheduledTime = world.State.TickTime
                  Operation = fun world ->
                    match World.getOptGroup address world with
                    | Some group -> snd <| World.removeGroupImmediate group address world
                    | None -> world }
            let world = World.addTask task world
            (group, world)

        static member removeGroupIf pred address world =
            let group = World.getGroup address world
            if pred group then World.removeGroup group address world
            else (group, world)

        static member removeGroupsImmediate groups (screenAddress : Screen Address) world =
            World.transformSimulants World.removeGroupImmediate satoga groups screenAddress world

        static member removeGroups groups (screenAddress : Screen Address) world =
            World.transformSimulants World.removeGroup satoga groups screenAddress world

        static member addGroup groupHierarchy address world =
            let (group, entities) = groupHierarchy
            if not <| World.containsGroup address world then
                let (group, world) =
                    match World.getOptGroup address world with
                    | Some _ -> World.removeGroupImmediate group address world
                    | None -> (group, world)
                let world = World.setGroup group address world
                let world = snd <| World.addEntities entities address world
                let (group, world) = World.registerGroup group address world
                let world = World.publish4 () (AddEventAddress ->>- address) address world
                (group, world)
            else failwith <| "Adding a group that the world already contains at address '" + acstring address + "'."

        static member addGroups groupHierarchies screenAddress world =
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