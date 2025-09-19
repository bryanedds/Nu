// Nu Game Engine.
// Copyright (C) Bryan Edds.

namespace Nu
open System
open System.IO
open Prime

/// Group functions for the world (2/2).
[<AutoOpen>]
module WorldGroupModule =

    type Group with
    
        member this.GetDispatcher world = World.getGroupDispatcher this world
        member this.Dispatcher = lensReadOnly (nameof this.Dispatcher) this this.GetDispatcher
        member this.GetModelGeneric<'a> world = World.getGroupModelGeneric<'a> this world
        member this.SetModelGeneric<'a> value world = World.setGroupModelGeneric<'a> false value this world |> ignore<bool>
        member this.ModelGeneric<'a> () = lens Constants.Engine.ModelPropertyName this this.GetModelGeneric<'a> this.SetModelGeneric<'a>
        member this.GetVisible world = World.getGroupVisible this world
        member this.SetVisible value world = World.setGroupVisible value this world |> ignore<bool>
        member this.Visible = lens (nameof this.Visible) this this.GetVisible this.SetVisible
        member this.GetProtected world = World.getGroupProtected this world
        member this.Protected = lensReadOnly (nameof this.Protected) this this.GetProtected
        member this.GetPersistent world = World.getGroupPersistent this world
        member this.SetPersistent value world = World.setGroupPersistent value this world |> ignore<bool>
        member this.Persistent = lens (nameof this.Persistent) this this.GetPersistent this.SetPersistent
        member this.GetDestroying world = World.getGroupDestroying this world
        member this.Destroying = lensReadOnly (nameof this.Destroying) this this.GetDestroying
        member this.GetOrder world = World.getGroupOrder this world
        member this.Order = lensReadOnly (nameof this.Order) this this.GetOrder
        member this.GetId world = World.getGroupId this world
        member this.Id = lensReadOnly (nameof this.Id) this this.GetId

        member this.RegisterEvent = Events.RegisterEvent --> this
        member this.UnregisteringEvent = Events.UnregisteringEvent --> this
        member this.ChangeEvent propertyName = Events.ChangeEvent propertyName --> this
        member this.PreUpdateEvent = Events.PreUpdateEvent --> this
        member this.UpdateEvent = Events.UpdateEvent --> this
        member this.PostUpdateEvent = Events.PostUpdateEvent --> this
        member this.TimeUpdateEvent = Events.TimeUpdateEvent --> this

        /// Try to get a property value and type.
        member this.TryGetProperty propertyName world =
            let mutable property = Unchecked.defaultof<_>
            let found = World.tryGetGroupProperty (propertyName, this, world, &property)
            if found then Some property else None

        /// Get a property value and type.
        member this.GetProperty propertyName world =
            World.getGroupProperty propertyName this world

        /// Try to get an xtension property value.
        member this.TryGet<'a> propertyName world : 'a voption =
            World.tryGetGroupXtensionValue<'a> propertyName this world

        /// Get an xtension property value.
        member this.Get<'a> propertyName world : 'a =
            World.getGroupXtensionValue<'a> propertyName this world

        /// Try to set a property value with explicit type.
        member this.TrySetProperty propertyName property world =
            World.trySetGroupProperty propertyName property this world

        /// Set a property value with explicit type.
        member this.SetProperty propertyName property world =
            World.setGroupProperty propertyName property this world |> ignore<bool>

        /// To try set an xtension property value.
        member this.TrySet<'a> propertyName (value : 'a) world =
            World.trySetGroupXtensionValue propertyName value this world

        /// Set an xtension property value.
        member this.Set<'a> propertyName (value : 'a) world =
            World.setGroupXtensionValue<'a> propertyName value this world

        /// Check that a group is selected.
        member this.GetSelected world = World.getGroupSelected this world

        /// Check that a group exists in the world.
        member this.GetExists world = World.getGroupExists this world

        /// Check that a group dispatches in the same manner as the dispatcher with the given type.
        member this.Is (dispatcherType, world) = Reflection.dispatchesAs dispatcherType (this.GetDispatcher world)

        /// Check that a group dispatches in the same manner as the dispatcher with the given type.
        member this.Is<'a> world = this.Is (typeof<'a>, world)

        /// Send a signal to a group.
        member this.Signal (signal : Signal) world = (this.GetDispatcher world).Signal (signal, this, world)

        /// Notify the engine that a group's MMCC model has changed in some automatically undetectable way (such as being mutated directly by user code).
        member this.NotifyModelChange world = World.notifyGroupModelChange this world

    type World with

        static member internal preUpdateGroup (group : Group) world =

            // pre-update via dispatcher
            let dispatcher = group.GetDispatcher world
            dispatcher.PreUpdate (group, world)

            // publish pre-update event
            let eventTrace = EventTrace.debug "World" "preUpdateGroup" "" EventTrace.empty
            World.publishPlus () group.PreUpdateEvent eventTrace group false false world

        static member internal updateGroup (group : Group) world =

            // update via dispatcher
            let dispatcher = group.GetDispatcher world
            dispatcher.Update (group, world)

            // publish update event
            let eventTrace = EventTrace.debug "World" "updateGroup" "" EventTrace.empty
            World.publishPlus () group.UpdateEvent eventTrace group false false world

        static member internal postUpdateGroup (group : Group) world =

            // post-update via dispatcher
            let dispatcher = group.GetDispatcher world
            dispatcher.PostUpdate (group, world)

            // publish post-update event
            let eventTrace = EventTrace.debug "World" "postUpdateGroup" "" EventTrace.empty
            World.publishPlus () group.PostUpdateEvent eventTrace group false false world

        static member internal renderGroup renderPass (group : Group) world =
            let dispatcher = group.GetDispatcher world
            dispatcher.Render (renderPass, group, world)

        /// Edit a game with the given operation using the ImGui APIs.
        /// Intended only to be called by editors like Gaia.
        static member editGroup operation (group : Group) world =
            let dispatcher = group.GetDispatcher world
            dispatcher.Edit (operation, group, world)

        /// Attempt to truncate a group model.
        static member tryTruncateGroupModel<'model> (model : 'model) (group : Group) world =
            let dispatcher = group.GetDispatcher world
            dispatcher.TryTruncateModel<'model> model

        /// Attempt to untruncate a group model.
        static member tryUntruncateGroupModel<'model> (model : 'model) (group : Group) world =
            let dispatcher = group.GetDispatcher world
            dispatcher.TryUntruncateModel<'model> (model, group, world)

        /// Get all the groups in a screen.
        static member getGroups (screen : Screen) (world : World) =
            match world.Simulants.TryGetValue (screen :> Simulant) with
            | (true, groupsOpt) ->
                match groupsOpt with
                | Some groups -> Seq.map cast<Group> groups
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Create a group and add it to the world.
        static member createGroup5 skipProcessing dispatcherName nameOpt (screen : Screen) world =
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match dispatchers.TryGetValue dispatcherName with
                | (true, dispatcher) -> dispatcher
                | (false, _) -> failwith ("Could not find a GroupDispatcher named '" + dispatcherName + "'.")
            let groupState = GroupState.make nameOpt dispatcher
            let groupState = Reflection.attachProperties GroupState.copy groupState.Dispatcher groupState world
            let group = Group (screen.ScreenAddress <-- ntoa<Group> groupState.Name)
            if World.getGroupExists group world then
                if group.GetDestroying world
                then World.destroyGroupImmediate group world
                else failwith ("Group '" + scstring group + "' already exists and cannot be created.")
            World.addGroup false groupState group world
            if not skipProcessing && WorldModule.UpdatingSimulants && group.GetSelected world then
                WorldModule.tryProcessGroup true group world
            group

        /// Create a group from a simulant descriptor.
        static member createGroup3 descriptor screen world =
            let groupNameOpt =
                match descriptor.SimulantSurnamesOpt with
                | None -> None
                | Some [|name|] -> Some name
                | Some _ -> failwith "Group cannot have multiple names."
            let group =
                World.createGroup5 false descriptor.SimulantDispatcherName groupNameOpt screen world
            for (propertyName, property) in descriptor.SimulantProperties do
                World.setGroupProperty propertyName property group world |> ignore<bool>
            for childDescriptor in descriptor.SimulantChildren do
                let entity = World.createEntity4 DefaultOverlay childDescriptor group world
                if not (List.exists (fun (name, _) -> name = nameof entity.Size || name = nameof entity.Offset) childDescriptor.SimulantProperties) then
                    entity.AutoBounds world // auto bounds if neither size not offset were specified by the descriptor properties
            group

        /// Create a group and add it to the world.
        static member createGroup<'d when 'd :> GroupDispatcher> nameOpt screen world =
            World.createGroup5 false typeof<'d>.Name nameOpt screen world

        /// Destroy a group in the world immediately. Can be dangerous if existing in-flight publishing depends on the
        /// group's existence. Consider using World.destroyGroup instead.
        static member destroyGroupImmediate (group : Group) world =
            World.tryRemoveSimulantFromDestruction group world
            EventGraph.cleanEventAddressCache group.GroupAddress
            if World.getGroupExists group world then
                let entities = World.getSovereignEntities group world
                World.unregisterGroup group world
                World.removeTasklets group world
                World.removeSimulantImSim group world
                World.destroyEntitiesImmediate entities world
                World.removeGroupState group world

        /// Destroy a group in the world at the end of the current update.
        static member destroyGroup (group : Group) world =
            World.addSimulantToDestruction group world

        /// Destroy multiple groups in the world immediately. Can be dangerous if existing in-flight publishing depends
        /// on any of the groups' existences. Consider using World.destroyGroups instead.
        static member destroyGroupsImmediate (groups : Group seq) world =
            for group in groups |> Seq.rev |> List.ofSeq do
                World.destroyGroupImmediate group world

        /// Destroy multiple groups from the world at the end of the current update.
        static member destroyGroups groups world =
            World.defer (World.destroyGroupsImmediate groups) Game.Handle world

        /// Rename a group. Note that since this destroys the renamed group immediately, you should not call this
        /// inside an event handler that involves the reassigned group itself. Note this also renames all of its
        /// descendents accordingly.
        static member renameGroupImmediate source (destination : Group) world =
            let groupStateOpt = World.getGroupStateOpt source world
            match groupStateOpt with
            | Some groupState ->
                let groupState = { groupState with Id = Gen.id64; Name = destination.Name; Content = GroupContent.empty }
                let children = World.getSovereignEntities source world
                World.addGroup false groupState destination world
                for child in children do
                    let destination = destination / child.Name
                    World.renameEntityImmediate child destination world
                World.destroyGroupImmediate source world
                if WorldModule.UpdatingSimulants && source.GetSelected world then
                    WorldModule.tryProcessGroup true destination world
            | None -> ()

        /// Rename a group.
        static member renameGroup source destination world =
            World.defer (World.renameGroupImmediate source destination) Game.Handle world

        /// Write a group to a group descriptor.
        static member writeGroup (groupDescriptor : GroupDescriptor) group world =
            let groupState = World.getGroupState group world
            let groupDispatcherName = getTypeName groupState.Dispatcher
            let groupDescriptor = { groupDescriptor with GroupDispatcherName = groupDispatcherName }
            let getGroupProperties = Reflection.writePropertiesFromTarget (fun name _ _ -> name <> "Order") groupDescriptor.GroupProperties groupState
            let groupDescriptor = { groupDescriptor with GroupProperties = getGroupProperties }
            let entities = World.getSovereignEntities group world
            { groupDescriptor with EntityDescriptors = World.writeEntities false true entities world }

        /// Write multiple groups to a screen descriptor.
        static member writeGroups groups world =
            groups
            |> Seq.sortBy (fun (group : Group) -> group.GetOrder world)
            |> Seq.filter (fun (group : Group) -> group.GetPersistent world && not (group.GetProtected world))
            |> Seq.fold (fun groupDescriptors group -> World.writeGroup GroupDescriptor.empty group world :: groupDescriptors) []
            |> Seq.rev
            |> Seq.toList

        /// Write a group to a file.
        static member writeGroupToFile (filePath : string) group world =
            let filePathTmp = filePath + ".tmp"
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<GameDescriptor>).PrettyPrinter
            let groupDescriptor = World.writeGroup GroupDescriptor.empty group world
            let groupDescriptorStr = scstring groupDescriptor
            let groupDescriptorPretty = PrettyPrinter.prettyPrint groupDescriptorStr prettyPrinter
            File.WriteAllText (filePathTmp, groupDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a group from a group descriptor.
        static member readGroup groupDescriptor nameOpt (screen : Screen) world =

            // make the dispatcher
            let dispatcherName = groupDescriptor.GroupDispatcherName
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match dispatchers.TryGetValue dispatcherName with
                | (true, dispatcher) -> dispatcher
                | (false, _) -> failwith ("Could not find a GroupDispatcher named '" + dispatcherName + "'.")

            // make the group state and populate its properties
            let groupState = GroupState.make None dispatcher
            let groupState = Reflection.attachProperties GroupState.copy groupState.Dispatcher groupState world
            let groupState = Reflection.readPropertiesToTarget GroupState.copy groupDescriptor.GroupProperties groupState

            // apply the name if one is provided
            let groupState =
                match nameOpt with
                | Some name -> { groupState with Name = name }
                | None -> groupState

            // add the group's state to the world
            let group = Group (screen.ScreenAddress <-- ntoa<Group> groupState.Name)
            World.addGroup true groupState group world

            // read the group's entities
            World.readEntities false true groupDescriptor.EntityDescriptors group world |> ignore<Entity list>

            // try to process ImSim group first time if in the middle of simulant update phase
            if WorldModule.UpdatingSimulants && group.GetSelected world then
                WorldModule.tryProcessGroup true group world

            // fin
            group

        /// Read multiple groups from a screen descriptor.
        static member readGroups groupDescriptors screen world =
            [for groupDescriptor in groupDescriptors do
                let groupNameOpt = GroupDescriptor.getNameOpt groupDescriptor
                World.readGroup groupDescriptor groupNameOpt screen world]

        /// Read a group from a file.
        static member readGroupFromFile (filePath : string) nameOpt screen world =
            let groupDescriptorStr = File.ReadAllText filePath
            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
            World.readGroup groupDescriptor nameOpt screen world