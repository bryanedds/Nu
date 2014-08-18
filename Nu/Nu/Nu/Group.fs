namespace Nu
open System
open System.IO
open System.Xml
open System.Reflection
open FSharpx
open FSharpx.Lens.Operators
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
    
        static member writeToXml (writer : XmlWriter) group entities =
            writer.WriteStartElement typeof<Group>.Name
            Xtension.writeTargetProperties writer group
            Entity.writeManyToXml writer entities
    
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

        // TODO: remove all lenses

        static member private optGroupFinder (address : Address) world =
            let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
            match optGroupMap with
            | None -> None
            | Some groupMap -> Map.tryFind (List.at 1 address) groupMap

        static member private groupAdder (address : Address) world child =
            let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
            match optGroupMap with
            | None ->
                { world with Groups = Map.singleton (List.at 0 address) <| Map.singleton (List.at 1 address) child }
            | Some groupMap ->
                let groupMap = Map.add (List.at 1 address) child groupMap
                { world with Groups = Map.add (List.at 0 address) groupMap world.Groups }

        static member private groupRemover (address : Address) world =
            let optGroupMap = Map.tryFind (List.at 0 address) world.Groups
            match optGroupMap with
            | None -> world
            | Some groupMap ->
                let groupMap = Map.remove (List.at 1 address) groupMap
                { world with Groups = Map.add (List.at 0 address) groupMap world.Groups }

        static member private worldGroup address =
            { Get = fun world -> Option.get <| World.optGroupFinder address world
              Set = fun group world -> World.groupAdder address world group }

        static member private worldOptGroup address =
            { Get = fun world -> World.optGroupFinder address world
              Set = fun optGroup world ->
                match optGroup with
                | None -> World.groupRemover address world
                | Some group -> set group world <| World.worldGroup address }

        static member private worldGroups address =
            { Get = fun world ->
                match address with
                | [screenStr] ->
                    match Map.tryFind screenStr world.Groups with
                    | None -> Map.empty
                    | Some groupMap -> groupMap
                | _ -> failwith <| "Invalid group address '" + addrToStr address + "'."
              Set = fun _ _ -> failwith "World.worldGroups setter not intended to be called." }
            
        static member getGroup address world = get world <| World.worldGroup address
        static member setGroup address group world = set group world <| World.worldGroup address
        static member withGroup fn address world = Sim.withSimulant World.worldGroup fn address world
        static member withGroupAndWorld fn address world = Sim.withSimulantAndWorld World.worldGroup fn address world

        static member getOptGroup address world = get world <| World.worldOptGroup address
        static member containsGroup address world = Option.isSome <| World.getOptGroup address world
        static member private setOptGroup address optGroup world = set optGroup world <| World.worldOptGroup address
        static member tryWithGroup fn address world = Sim.tryWithSimulant World.worldOptGroup World.worldGroup fn address world
        static member tryWithGroupAndWorld fn address world = Sim.tryWithSimulantAndWorld World.worldOptGroup World.worldGroup fn address world
    
        static member getGroups address world = get world <| World.worldGroups address

        static member registerGroup address (group : Group) world =
            Group.register address group world

        static member unregisterGroup address world =
            let group = World.getGroup address world
            Group.unregister address group world

        static member removeGroupImmediate (address : Address) world =
            let world = World.publish4 (RemovingEventName @ address) address NoData world
            let world = World.unregisterGroup address world
            let world = World.clearEntitiesImmediate address world
            World.setOptGroup address None world

        static member removeGroup address world =
            let task =
                { ScheduledTime = world.TickTime
                  Operation = fun world -> if World.containsGroup address world then World.removeGroupImmediate address world else world }
            { world with Tasks = task :: world.Tasks }

        static member clearGroupsImmediate (address : Address) world =
            let groups = World.getGroups address world
            Map.fold
                (fun world groupName _ -> World.removeGroupImmediate (address @ [groupName]) world)
                world
                groups

        static member clearGroups (address : Address) world =
            let groups = World.getGroups address world
            Map.fold
                (fun world groupName _ -> World.removeGroup (address @ [groupName]) world)
                world
                groups

        static member removeGroupsImmediate (screenAddress : Address) groupNames world =
            List.fold
                (fun world groupName -> World.removeGroupImmediate (screenAddress @ [groupName]) world)
                world
                groupNames

        static member removeGroups (screenAddress : Address) groupNames world =
            List.fold
                (fun world groupName -> World.removeGroup (screenAddress @ [groupName]) world)
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
            Sim.publish4 (AddEventName @ address) address NoData world

        static member addGroups screenAddress groupDescriptors world =
            List.fold
                (fun world (groupName, group, entities) -> World.addGroup (screenAddress @ [groupName]) group entities world)
                world
                groupDescriptors