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
    
        member this.GetId world = World.getGroupId this world
        member this.TagId = PropertyTag.makeReadOnly this Property? Id this.GetId
        member this.GetName world = World.getGroupName this world
        member this.TagName = PropertyTag.makeReadOnly this Property? Name this.GetName
        member this.GetXtension world = World.getGroupXtension this world
        member this.TagXtension = PropertyTag.makeReadOnly this Property? Xtension this.GetXtension
        member this.GetDispatcherNp world = World.getGroupDispatcherNp this world
        member this.TagDispatcherNp = PropertyTag.makeReadOnly this Property? DispatcherNp this.GetDispatcherNp
        member this.GetSpecialization world = World.getGroupSpecialization this world
        member this.TagSpecialization = PropertyTag.makeReadOnly this Property? Specialization this.GetSpecialization
        member this.GetClassification world = (getTypeName ^ this.GetDispatcherNp world, this.GetSpecialization world)
        member this.TagClassification = PropertyTag.makeReadOnly this Property? Classification this.GetClassification
        member this.GetCreationTimeStampNp world = World.getGroupCreationTimeStampNp this world
        member this.TagCreationTimeStampNp = PropertyTag.makeReadOnly this Property? CreationTimeStampNp this.GetCreationTimeStampNp
        member this.GetPersistent world = World.getGroupPersistent this world
        member this.SetPersistent value world = World.setGroupPersistent value this world
        member this.TagPersistent = PropertyTag.makeReadOnly this Property? Persistent this.GetPersistent

        /// Get a property value and type.
        member this.GetProperty propertyName world = World.getGroupProperty propertyName this world

        /// Get a property value.
        member this.Get propertyName world : 'a = World.getGroupPropertyValue propertyName this world

        /// Set a property value.
        member this.Set propertyName (value : 'a) world = World.setGroupPropertyValue propertyName value this world

        /// Check that a group dispatches in the same manner as the dispatcher with the target type.
        member this.DispatchesAs (dispatcherTargetType : Type) world = Reflection.dispatchesAs dispatcherTargetType (this.GetDispatcherNp world)

    type World with

        static member private removeGroup group world =
            let removeEntities group world =
                let entities = World.proxyEntities group world
                World.destroyEntitiesImmediate entities world
            World.removeGroup3 removeEntities group world

        static member internal updateGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world
            let world = dispatcher.Update (group, world)
            let eventTrace = EventTrace.record "World" "updateGroup" EventTrace.empty
            World.publish7 World.getSubscriptionsSorted World.sortSubscriptionsByHierarchy () (Events.Update ->- group) eventTrace Simulants.Game world

        static member internal actualizeGroup (group : Group) world =
            let dispatcher = group.GetDispatcherNp world
            dispatcher.Actualize (group, world)

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
        /// group's existence. Consider using World.destroyGroup instead.
        static member destroyGroupImmediate group world = World.removeGroup group world

        /// Destroy a group in the world on the next tick. Use this rather than destroyGroupImmediate unless you need
        /// the latter's specific behavior.
        static member destroyGroup group world =
            let tasklet =
                { ScheduledTime = World.getTickTime world
                  Command = { Execute = fun world -> World.destroyGroupImmediate group world }}
            World.addTasklet tasklet world
            
        /// Destroy multiple groups in the world immediately. Can be dangerous if existing in-flight publishing depends
        /// on any of the groups' existences. Consider using World.destroyGroups instead.
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
                  Command = { Execute = fun world -> World.destroyGroupsImmediate groups world }}
            World.addTasklet tasklet world

        /// Write a group to a group descriptor.
        static member writeGroup group groupDescriptor world =
            let writeEntities group groupDescriptor world =
                let entities = World.proxyEntities group world
                World.writeEntities entities groupDescriptor world
            World.writeGroup4 writeEntities group groupDescriptor world

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
            World.readGroup5 World.readEntities groupDescriptor optName screen world

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

    /// Provides a view of all the member properties of a group. Useful for debugging such as with
    /// the Watch feature in Visual Studio.
    static member viewMemberProperties group world = World.viewGroupMemberProperties group world

    /// Provides a view of all the xtension properties of a group. Useful for debugging such as
    /// with the Watch feature in Visual Studio.
    static member viewXProperties group world = World.viewGroupXProperties group world

    /// Provides a full view of all the member values of a group. Useful for debugging such
    /// as with the Watch feature in Visual Studio.
    static member view group world = World.viewGroup group world