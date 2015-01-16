namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open Prime
open Nu
open Nu.Constants
open Nu.WorldConstants

[<AutoOpen>]
module WorldGroupModule =

    type World with

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
            Reflection.writePropertiesFromTarget tautology3 writer group
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
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName groupNode
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
            Reflection.readPropertiesToTarget groupNode group
            
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
        static member readGroupHierarchies (screenNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =
            match screenNode.SelectSingleNode GroupsNodeName with
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