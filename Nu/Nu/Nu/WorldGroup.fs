// Nu Game Engine.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Nu
open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open OpenTK
open Prime
open Nu

[<AutoOpen>]
module WorldGroupModule =

    type Group with

        member this.GetId world = (World.getGroupState this world).Id
        member this.GetName world = (World.getGroupState this world).Name
        member this.GetXtension world = (World.getGroupState this world).Xtension
        member this.GetDispatcherNp world = (World.getGroupState this world).DispatcherNp
        member this.GetCreationTimeStampNp world = (World.getGroupState this world).CreationTimeStampNp
        member this.GetOptSpecialization world = (World.getGroupState this world).OptSpecialization
        member this.GetPersistent world = (World.getGroupState this world).Persistent
        member this.SetPersistent value world = World.updateGroupState (fun groupState -> { groupState with Persistent = value }) this world

        /// Get a dynamic property.
        member this.Get propertyName world : 'r =
            GroupState.(?) (World.getGroupState this world, propertyName)

        /// Set a dynamic property.
        member this.Set propertyName (value : 'a) world = 
            World.setGroupState (GroupState.(?<-) (World.getGroupState this world, propertyName, value)) this world

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

        static member internal updateGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world
            let world = dispatcher.Update (group, world)
            let eventTrace = EventTrace.record "World" "updateGroup" EventTrace.empty
            World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () (Events.Update ->- group) eventTrace Simulants.Game world

        static member internal actualizeGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world
            dispatcher.Actualize (group, world)

        static member internal addGroup mayReplace groupState group world =
            let isNew = not ^ World.containsGroup group world
            if isNew || mayReplace then
                let world = World.addGroupState groupState group world
                if isNew then
                    let world = World.registerGroup group world
                    let eventTrace = EventTrace.record "World" "addGroup" EventTrace.empty
                    World.publish () (Events.GroupAdd ->- group) eventTrace group world
                else world
            else failwith ^ "Adding a group that the world already contains at address '" + scstring group.GroupAddress + "'."

        /// Remove a group in the world. Can be dangerous if existing in-flight publishing depends on the group's
        /// existence. Use with caution.
        static member internal removeGroup group world =
            let eventTrace = EventTrace.record "World" "removeGroup" EventTrace.empty
            let world = World.publish () (Events.GroupRemoving ->- group) eventTrace group world
            if World.containsGroup group world then
                let world = World.unregisterGroup group world
                let entities = World.proxyEntities group world
                let world = World.destroyEntitiesImmediate entities world
                World.removeGroupState group world
            else world

        /// Query that the world contains a group.
        static member containsGroup group world =
            Option.isSome ^ World.getOptGroupState group world

        /// Get all the groups in a screen.
        static member proxyGroups screen world =
            match Address.getNames screen.ScreenAddress with
            | [screenName] ->
                match Vmap.tryFind screenName ^ World.getScreenDirectory world with
                | Some (_, groupDirectory) ->
                    Vmap.fold (fun state _ (groupAddress, _) -> Group.proxy groupAddress :: state) [] groupDirectory :> _ seq
                | None -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."
            | _ -> failwith ^ "Invalid screen address '" + scstring screen.ScreenAddress + "'."

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
            let dispatchers = World.getGroupDispatchers world
            let dispatcher = Map.find dispatcherName dispatchers
            let groupState = GroupState.make optSpecialization optName dispatcher
            Reflection.attachProperties dispatcher groupState
            let group = stog screen groupState.Name
            let world = World.addGroup false groupState group world
            (group, world)

        /// Write a group to a group descriptor.
        static member writeGroup (group : Group) groupDescriptor world =
            let groupState = World.getGroupState group world
            let groupDispatcherName = getTypeName groupState.DispatcherNp
            let groupDescriptor = { groupDescriptor with GroupDispatcher = groupDispatcherName }
            let getGroupProperties = Reflection.writeMemberValuesFromTarget tautology3 groupDescriptor.GroupProperties groupState
            let groupDescriptor = { groupDescriptor with GroupProperties = getGroupProperties }
            let entities = World.proxyEntities group world
            World.writeEntities entities groupDescriptor world

        /// Write multiple groups to a screen descriptor.
        static member writeGroups groups screenDescriptor world =
            groups |>
            Seq.sortBy (fun (group : Group) -> group.GetCreationTimeStampNp world) |>
            Seq.filter (fun (group : Group) -> group.GetPersistent world) |>
            Seq.fold (fun groupDescriptors group -> World.writeGroup group GroupDescriptor.empty world :: groupDescriptors) screenDescriptor.Groups |>
            fun groupDescriptors -> { screenDescriptor with Groups = groupDescriptors }

        /// Write a group to a file.
        static member writeGroupToFile (filePath : string) group world =
            let filePathTmp = filePath + ".tmp"
            let groupDescriptor = World.writeGroup group GroupDescriptor.empty world
            let groupDescriptorStr = scstring groupDescriptor
            let groupDescriptorPretty = Symbol.prettyPrint String.Empty groupDescriptorStr
            File.WriteAllText (filePathTmp, groupDescriptorPretty)
            File.Delete filePath
            File.Move (filePathTmp, filePath)

        /// Read a group from a group descriptor.
        static member readGroup groupDescriptor optName screen world =

            // create the dispatcher
            let dispatcherName = groupDescriptor.GroupDispatcher
            let dispatchers = World.getGroupDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None ->
                    Log.info ^ "Could not locate dispatcher '" + dispatcherName + "'."
                    let dispatcherName = typeof<GroupDispatcher>.Name
                    Map.find dispatcherName dispatchers
            
            // make the bare group state with name as id
            let groupState = GroupState.make None None dispatcher

            // attach the group state's instrinsic properties from its dispatcher if any
            Reflection.attachProperties groupState.DispatcherNp groupState

            // read the group state's value
            Reflection.readMemberValuesToTarget groupDescriptor.GroupProperties groupState

            // apply the name if one is provided
            let groupState =
                match optName with
                | Some name -> { groupState with Name = name }
                | None -> groupState

            // add the group's state to the world
            let group = stog screen groupState.Name
            let world = World.addGroup true groupState group world

            // read the group's entities
            let world = World.readEntities groupDescriptor group world |> snd
            (group, world)

        /// Read a group from a file.
        static member readGroupFromFile (filePath : string) optName screen world =
            let groupDescriptorStr = File.ReadAllText filePath
            let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
            World.readGroup groupDescriptor optName screen world

        /// Read multiple groups from a screen descriptor.
        static member readGroups screenDescriptor screen world =
            List.foldBack
                (fun groupDescriptor (groups, world) ->
                    let (group, world) = World.readGroup groupDescriptor None screen world
                    (group :: groups, world))
                screenDescriptor.Groups
                ([], world)

namespace Debug
open Prime
open Nu
open System.Reflection
open System.Collections.Generic
type Group =

    /// Provides a view of all the built-in properties of a group. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewProperties group world =
        let state = World.getGroupState group world
        state |>
        getType |>
        getProperties |>
        Array.map (fun (property : PropertyInfo) -> (property.Name, property.GetValue state))
        
    /// Provides a view of all the xtension properties of a group. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXProperties group world =
        let state = World.getGroupState group world
        Xtension.toSeq state.Xtension |>
        Array.ofSeq |>
        Array.sortBy fst |>
        Array.map (fun (name, property) -> (name, property.PropertyValue))

    /// Provides a full view of all the member values of a group. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view group world =
        Array.append (Group.viewProperties group world) (Group.viewXProperties group world)