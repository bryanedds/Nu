// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2015.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Xml
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldGroupModule =

    type Group with

        member this.GetId world = (World.getGroupState this world).Id
        member this.GetName world = (World.getGroupState this world).Name
        member this.GetOptSpecialization world = (World.getGroupState this world).OptSpecialization
        member this.GetCreationTimeStampNp world = (World.getGroupState this world).CreationTimeStampNp
        member this.GetDispatcherNp world = (World.getGroupState this world).DispatcherNp
        member this.GetPublishChanges world = (World.getGroupState this world).PublishChanges
        member this.SetPublishChanges value world = World.updateGroupState (fun (groupState : GroupState) -> { groupState with PublishChanges = value }) this world
        member this.GetPersistent world = (World.getGroupState this world).Persistent
        member this.SetPersistent value world = World.updateGroupState (fun groupState -> { groupState with Persistent = value }) this world
        member this.GetXtension world = (World.getGroupState this world).Xtension
        member this.UpdateXtension updater world = World.updateGroupState (fun groupState -> { groupState with Xtension = updater groupState.Xtension}) this world

        /// Get an xtension field by name.
        member this.GetXField name world =
            let xtension = this.GetXtension world
            let xField = Map.find name xtension.XFields
            xField.FieldValue

        /// Query that a group dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world =
            Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member private registerGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world : GroupDispatcher
            dispatcher.Register (group, world)

        static member private unregisterGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world : GroupDispatcher
            dispatcher.Unregister (group, world)

        static member internal addGroup mayReplace groupState group world =
            let isNew = not ^ World.containsGroup group world
            if isNew || mayReplace then
                let world = World.setGroupStateWithoutEvent groupState group world
                if isNew then
                    let world = World.registerGroup group world
                    World.publish () (Events.GroupAdd ->- group) group world
                else world
            else failwith ^ "Adding a group that the world already contains at address '" + acstring group.GroupAddress + "'."

        /// Remove a group in the world. Can be dangerous if existing in-flight publishing depends on the group's
        /// existence. Use with caution.
        static member internal removeGroup group world =
            let world = World.publish () (Events.GroupRemoving ->- group) group world
            if World.containsGroup group world then
                let world = World.unregisterGroup group world
                let entities = World.proxyEntities group world
                let world = World.destroyEntitiesImmediate entities world
                World.setOptGroupStateWithoutEvent None group world
            else world

        /// Query that the world contains a group.
        static member containsGroup group world =
            Option.isSome ^ World.getOptGroupState group world

        /// Get all the groups in a screen.
        static member proxyGroups screen world =
            let groupStateMap = World.getGroupStateMap screen world
            Seq.map
                (fun (kvp : KeyValuePair<string, _>) -> stog screen kvp.Key)
                groupStateMap

        /// Destroy a group in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// group's existence. Use with caution.
        static member destroyGroupImmediate group world =
            World.removeGroup group world

        /// Destroy a group in the world on the next tick. Use this rather than destroyGroupImmediate unless you need
        /// the latter's specific behavior.
        static member destroyGroup group world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Operation = fun world -> World.destroyGroupImmediate group world }
            World.addTasklet tasklet world
            
        /// Destroy multiple groups in the world immediately. Can be dangerous if existing in-flight publishing depends
        /// on any of the groups' existences. Use with caution.
        static member destroyGroupsImmediate groups world =
            List.foldBack
                (fun group world -> World.destroyGroupImmediate group world)
                (List.ofSeq groups)
                world

        /// Destroy multiple groups from the world. Use this rather than destroyEntitiesImmediate unless you need the
        /// latter's specific behavior.
        static member destroyGroups groups world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Operation = fun world -> World.destroyGroupsImmediate groups world }
            World.addTasklet tasklet world

        /// Create a group and add it to the world.
        static member createGroup dispatcherName optSpecialization optName screen world =
            let dispatcher = Map.find dispatcherName world.Components.GroupDispatchers
            let groupState = GroupState.make dispatcher optSpecialization optName
            Reflection.attachFields dispatcher groupState
            let group = stog screen groupState.Name
            let world = World.addGroup false groupState group world
            (group, world)

        /// Write a group to an xml writer.
        static member writeGroup (writer : XmlWriter) group world =
            let groupState = World.getGroupState group world
            let entities = World.proxyEntities group world
            writer.WriteAttributeString (Constants.Xml.DispatcherNameAttributeName, Reflection.getTypeName groupState.DispatcherNp)
            Reflection.writeMemberValuesFromTarget tautology3 writer groupState
            writer.WriteStartElement Constants.Xml.EntitiesNodeName
            World.writeEntities writer entities world
            writer.WriteEndElement ()

        /// Write a group to an xml file.
        static member writeGroupToFile (filePath : string) group world =
            let filePathTmp = filePath + ".tmp"
            let writerSettings = XmlWriterSettings ()
            writerSettings.Indent <- true
            // NOTE: XmlWriter can also write to an XmlDocument / XmlNode instance by using
            // XmlWriter.Create ^ (document.CreateNavigator ()).AppendChild ()
            use writer = XmlWriter.Create (filePathTmp, writerSettings)
            writer.WriteStartDocument ()
            writer.WriteStartElement Constants.Xml.RootNodeName
            writer.WriteStartElement Constants.Xml.GroupNodeName
            World.writeGroup writer group world
            writer.WriteEndElement ()
            writer.WriteEndElement ()
            writer.WriteEndDocument ()
            writer.Dispose ()
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Write multiple groups to an xml writer.
        static member writeGroups (writer : XmlWriter) groups world =
            let groupsSorted = Seq.sortBy (fun (group : Group) -> group.GetCreationTimeStampNp world) groups
            let groupsPersistent = Seq.filter (fun (group : Group) -> group.GetPersistent world) groupsSorted
            for group in groupsPersistent do
                writer.WriteStartElement Constants.Xml.GroupNodeName
                World.writeGroup writer group world
                writer.WriteEndElement ()

        /// Read a group from an xml node.
        static member readGroup groupNode defaultDispatcherName defaultEntityDispatcherName optName screen world =

            // read in the dispatcher name and create the dispatcher
            let dispatcherName = Reflection.readDispatcherName defaultDispatcherName groupNode
            let dispatcher =
                match Map.tryFind dispatcherName world.Components.GroupDispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    note ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName world.Components.GroupDispatchers
            
            // make the bare group state with name as id
            let groupState = GroupState.make dispatcher None None

            // attach the group state's instrinsic fields from its dispatcher if any
            Reflection.attachFields groupState.DispatcherNp groupState

            // read the group state's value
            Reflection.readMemberValuesToTarget groupNode groupState

            // apply the name if one is provided
            let groupState = match optName with Some name -> { groupState with Name = name } | None -> groupState

            // add the group's state to the world
            let group = stog screen groupState.Name
            let world = World.addGroup true groupState group world

            // read the group's entities
            let world = World.readEntities groupNode defaultEntityDispatcherName group world |> snd
            (group, world)

        /// Read a group from an xml file.
        static member readGroupFromFile (filePath : string) optName screen world =
            use reader = XmlReader.Create filePath
            let document = let emptyDoc = XmlDocument () in (emptyDoc.Load reader; emptyDoc)
            let rootNode = document.[Constants.Xml.RootNodeName]
            let groupNode = rootNode.[Constants.Xml.GroupNodeName]
            World.readGroup groupNode typeof<GroupDispatcher>.Name typeof<EntityDispatcher>.Name optName screen world

        /// Read multiple groups from an xml node.
        static member readGroups (screenNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName screen world =
            match screenNode.SelectSingleNode Constants.Xml.GroupsNodeName with
            | null -> ([], world)
            | groupsNode ->
                let (groupsRev, world) =
                    Seq.fold
                        (fun (groupsRev, world) groupNode ->
                            let (group, world) = World.readGroup groupNode defaultDispatcherName defaultEntityDispatcherName None screen world
                            (group :: groupsRev, world))
                        ([], world)
                        (enumerable ^ groupsNode.SelectNodes Constants.Xml.GroupNodeName)
                (List.rev groupsRev, world)

namespace Debug
open Prime
open Nu
open System.Reflection
type Group =

    /// Provides a view of all the properties of a group. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewProperties group world =
        let state = World.getGroupState group world
        let properties = Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state)) ((state.GetType ()).GetProperties ())
        Map.ofSeq properties
        
    /// Provides a view of all the xtension fields of a group. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXFields group world =
        let state = World.getGroupState group world
        Map.map (fun _ field -> field.FieldValue) state.Xtension.XFields

    /// Provides a full view of all the member values of a group. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view group world = Group.viewProperties group world @@ Group.viewXFields group world

    /// Provides a partitioned view of all the member values of a group. Useful for debugging
    /// such as with the Watch feature in Visual Studio.
    static member peek group world = Watchable (Group.viewProperties group world, Group.viewXFields group world)