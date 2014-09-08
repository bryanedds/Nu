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
        
        static member init (group : Group) (world : World) : Group = group?Init (group, world)
        static member register (address : Address) (group : Group) (world : World) : World = group?Register (address, world)
        static member unregister (address : Address) (group : Group) (world : World) : World = group?Unregister (address, world)

    type GroupDispatcher () =

        abstract member Init : Group * World -> Group
        default dispatcher.Init (group, _) = group
        
        abstract member Register : Address * World -> World
        default dispatcher.Register (_, world) = world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (_, world) = world

    type Group with
    
        static member make dispatcherName =
            { Group.Id = NuCore.makeId ()
              Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

[<AutoOpen>]
module WorldGroupModule =

    type World with

        static member private optGroupFinder address world =
            match address.AddrList with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap -> Map.tryFind groupName groupMap
                | None -> None
            | _ -> failwith <| "Invalid group address '" + string address + "'."

        static member private groupAdder address world child =
            match address.AddrList with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap ->
                    let groupMap = Map.add groupName child groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None -> { world with Groups = Map.singleton screenName <| Map.singleton groupName child }
            | _ -> failwith <| "Invalid group address '" + string address + "'."

        static member private groupRemover address world =
            match address.AddrList with
            | [screenName; groupName] ->
                let optGroupMap = Map.tryFind screenName world.Groups
                match optGroupMap with
                | Some groupMap ->
                    let groupMap = Map.remove groupName groupMap
                    { world with Groups = Map.add screenName groupMap world.Groups }
                | None -> world
            | _ -> failwith <| "Invalid group address '" + string address + "'."

        static member getGroup address world = Option.get <| World.optGroupFinder address world
        static member setGroup address group world = World.groupAdder address world group
        static member getOptGroup address world = World.optGroupFinder address world
        static member containsGroup address world = Option.isSome <| World.getOptGroup address world
        static member private setOptGroup address optGroup world =
            match optGroup with
            | Some group -> World.setGroup address group world
            | None -> World.groupRemover address world
            
        static member withGroup fn address world = Sim.withSimulant World.getGroup World.setGroup fn address world
        static member withGroupAndWorld fn address world = Sim.withSimulantAndWorld World.getGroup World.setGroup fn address world
        static member tryWithGroup fn address world = Sim.tryWithSimulant World.getOptGroup World.setGroup fn address world
        static member tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld World.getOptGroup World.setGroup fn address world
    
        static member getGroups address world =
            match address.AddrList with
            | [screenStr] ->
                match Map.tryFind screenStr world.Groups with
                | Some groupMap -> groupMap
                | None -> Map.empty
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
                | Some _ -> World.removeGroupImmediate address world
                | None -> world
            let world = World.setGroup address group world
            let world = World.addEntities address entities world
            let world = World.registerGroup address group world
            World.publish4 (AddEventName + address) address NoData world

        static member addGroups screenAddress groupDescriptors world =
            List.fold
                (fun world (groupName, group, entities) -> World.addGroup (addrlist screenAddress [groupName]) group entities world)
                world
                groupDescriptors
    
        static member writeGroupToXml overlayer (writer : XmlWriter) group entities =
            writer.WriteStartElement typeof<Group>.Name
            Serialization.writeTargetProperties tautology writer group
            World.writeEntitiesToXml overlayer writer entities
    
        static member readGroupFromXml (groupNode : XmlNode) defaultDispatcherName defaultEntityDispatcherName world =
            let group = Group.make defaultDispatcherName
            Serialization.readTargetOptXDispatcherName groupNode group
            let group = Group.init group world
            Serialization.readTargetProperties groupNode group
            let entities = World.readEntitiesFromXml (groupNode : XmlNode) defaultEntityDispatcherName world
            (group, entities)

        static member makeGroup dispatcherName world =
            let group = Group.make dispatcherName
            Group.init group world