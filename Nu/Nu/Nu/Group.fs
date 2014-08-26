namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module GroupModule =

    type Group with
        
        static member init (group : Group) (dispatcherContainer : IXDispatcherContainer) : Group = group?Init (group, dispatcherContainer)
        static member register (address : Address) (group : Group) (world : World) : World = group?Register (address, world)
        static member unregister (address : Address) (group : Group) (world : World) : World = group?Unregister (address, world)

    type GroupDispatcher () =

        abstract member Init : Group * IXDispatcherContainer -> Group
        default dispatcher.Init (group, _) = group
        
        abstract member Register : Address * World -> World
        default dispatcher.Register (_, world) = world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (_, world) = world

    type Group with
    
        static member makeDefaultUninitialized dispatcherName =
            { Group.Id = NuCore.makeId ()
              Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}
    
        static member makeDefault dispatcherName dispatcherContainer =
            let group = Group.makeDefaultUninitialized dispatcherName
            Group.init group dispatcherContainer
    
        static member writeToXml overlayer (writer : XmlWriter) group entities =
            writer.WriteStartElement typeof<Group>.Name
            Xtension.writeTargetProperties tautology writer group
            Entity.writeManyToXml overlayer writer entities
    
        static member readFromXml (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName dispatcherContainer =
            let group = Group.makeDefaultUninitialized defaultDispatcherName
            Xtension.readTargetXDispatcher groupNode group
            let group = Group.init group dispatcherContainer
            Xtension.readTargetProperties groupNode group
            let entities = Entity.readManyFromXml (groupNode : XmlNode) defaultEntityDispatcherName dispatcherContainer
            (group, entities)

[<AutoOpen>]
module WorldGroupModule =

    type World with

        static member private optGroupFinder address world =
            let optGroupMap = Map.tryFind (Address.at 0 address) world.Groups
            match optGroupMap with
            | None -> None
            | Some groupMap -> Map.tryFind (Address.at 1 address) groupMap

        static member private groupAdder address world child =
            let optGroupMap = Map.tryFind (Address.at 0 address) world.Groups
            match optGroupMap with
            | None ->
                { world with Groups = Map.singleton (Address.at 0 address) <| Map.singleton (Address.at 1 address) child }
            | Some groupMap ->
                let groupMap = Map.add (Address.at 1 address) child groupMap
                { world with Groups = Map.add (Address.at 0 address) groupMap world.Groups }

        static member private groupRemover address world =
            let optGroupMap = Map.tryFind (Address.at 0 address) world.Groups
            match optGroupMap with
            | None -> world
            | Some groupMap ->
                let groupMap = Map.remove (Address.at 1 address) groupMap
                { world with Groups = Map.add (Address.at 0 address) groupMap world.Groups }

        static member getGroup address world = Option.get <| World.optGroupFinder address world
        static member setGroup address group world = World.groupAdder address world group
        static member getOptGroup address world = World.optGroupFinder address world
        static member containsGroup address world = Option.isSome <| World.getOptGroup address world
        static member private setOptGroup address optGroup world =
            match optGroup with
            | None -> World.groupRemover address world
            | Some group -> World.setGroup address group world
            
        static member withGroup fn address world = Sim.withSimulant World.getGroup World.setGroup fn address world
        static member withGroupAndWorld fn address world = Sim.withSimulantAndWorld World.getGroup World.setGroup fn address world
        static member tryWithGroup fn address world = Sim.tryWithSimulant World.getOptGroup World.setGroup fn address world
        static member tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld World.getOptGroup World.setGroup fn address world
    
        static member getGroups address world =
            match address.AddrList with
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | None -> Map.empty
                | Some groupMap -> groupMap
            | _ -> failwith <| "Invalid group address '" + string address + "'."

        static member registerGroup address (group : Group) world =
            Group.register address group world

        static member unregisterGroup address world =
            let group = World.getGroup address world
            Group.unregister address group world

        static member removeGroupImmediate address world =
            let world = World.publish4 (RemovingEventName + address) address NoData world
            let world = World.unregisterGroup address world
            let world = World.clearEntitiesImmediate address world
            World.setOptGroup address None world

        static member removeGroup address world =
            let task =
                { ScheduledTime = world.TickTime
                  Operation = fun world -> if World.containsGroup address world then World.removeGroupImmediate address world else world }
            { world with Tasks = task :: world.Tasks }

        static member clearGroupsImmediate address world =
            let groups = World.getGroups address world
            Map.fold
                (fun world groupName _ -> World.removeGroupImmediate (addrlist address [groupName]) world)
                world
                groups

        static member clearGroups address world =
            let groups = World.getGroups address world
            Map.fold
                (fun world groupName _ -> World.removeGroup (addrlist address [groupName]) world)
                world
                groups

        static member removeGroupsImmediate screenAddress groupNames world =
            List.fold
                (fun world groupName -> World.removeGroupImmediate (addrlist screenAddress [groupName]) world)
                world
                groupNames

        static member removeGroups screenAddress groupNames world =
            List.fold
                (fun world groupName -> World.removeGroup (addrlist screenAddress [groupName]) world)
                world
                groupNames

        static member addGroup address (group : Group) entities world =
            let world =
                match World.getOptGroup address world with
                | None -> world
                | Some _ -> World.removeGroupImmediate address world
            let world = World.setGroup address group world
            let world = World.addEntities address entities world
            let world = World.registerGroup address group world
            Sim.publish4 (AddEventName + address) address NoData world

        static member addGroups screenAddress groupDescriptors world =
            List.fold
                (fun world (groupName, group, entities) -> World.addGroup (addrlist screenAddress [groupName]) group entities world)
                world
                groupDescriptors