namespace Nu
open System
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open System.Xml
open System.Xml.Serialization
open OpenTK
open TiledSharp
open Prime
open Nu
open Nu.NuConstants

[<AutoOpen>]
module EntityModule =

    type EntityDispatcher (facetNames : string Set) =

        new () = EntityDispatcher (Set.empty)

        abstract member Init : Entity * IXDispatcherContainer -> Entity
        default dispatcher.Init (entity, _) = entity

        abstract member Register : Address * World -> World
        default dispatcher.Register (_, world) = world

        abstract member Unregister : Address * World -> World
        default dispatcher.Unregister (_, world) = world
                
        abstract member GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (entity, _) = entity.Depth

        abstract member PropagatePhysics : Address * World -> World
        default dispatcher.PropagatePhysics (_, world) = world

        abstract member HandleBodyTransformMessage : Address * BodyTransformMessage * World -> World
        default dispatcher.HandleBodyTransformMessage (_, _, world) = world

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (_, _) = []

        abstract member GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = DefaultEntitySize

        abstract member IsTransformRelative : Entity * World -> bool
        default dispatcher.IsTransformRelative (_, _) = true

        member dispatcher.GetFacetNames (_ : World) = facetNames

    type Entity with

        (* OPTIMIZATION: The following dispatch forwarders are optimized. *)

        static member init (entity : Entity) dispatcherContainer =
            match Xtension.getDispatcher entity.Xtension dispatcherContainer with
            | :? EntityDispatcher as dispatcher -> dispatcher.Init (entity, dispatcherContainer)
            | _ -> failwith "Due to optimization in Entity.init, an entity's base dispatcher must be an EntityDispatcher."
        
        static member register address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Register (address, world)
            | _ -> failwith "Due to optimization in Entity.register, an entity's base dispatcher must be an EntityDispatcher."
        
        static member unregister address (entity : Entity) (world : World) : World =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Unregister (address, world)
            | _ -> failwith "Due to optimization in Entity.unregister, an entity's base dispatcher must be an EntityDispatcher."
        
        static member getFacetNames (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetFacetNames world
            | _ -> failwith "Due to optimization in Entity.getFacetNames, an entity's base dispatcher must be an EntityDispatcher."

        static member propagatePhysics address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.PropagatePhysics (address, world)
            | _ -> failwith "Due to optimization in Entity.propagatePhysics, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member handleBodyTransformMessage address message (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.HandleBodyTransformMessage (address, message, world)
            | _ -> failwith "Due to optimization in Entity.handleBodyTransformMessage, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member getRenderDescriptors (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetRenderDescriptors (entity, world)
            | _ -> failwith "Due to optimization in Entity.getRenderDescriptors, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member getQuickSize  (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetQuickSize (entity, world)
            | _ -> failwith "Due to optimization in Entity.getQuickSize, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member getPickingPriority (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetPickingPriority (entity, world)
            | _ -> failwith "Due to optimization in Entity.getPickingPriority, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member isTransformRelative  (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.IsTransformRelative (entity, world)
            | _ -> failwith "Due to optimization in Entity.isTransformRelative, an entity's 2d dispatcher must be an EntityDispatcher."

        static member usesFacet facetName entity world =
            let facetNames = Entity.getFacetNames entity world
            facetNames.Contains facetName

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : int * int
          TileSize : int * int
          TileSizeF : Vector2
          TileMapSize : int * int
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : int * int }

    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : int * int
          OptTileSetTile : TmxTilesetTile option
          TilePosition : int * int }

    type Entity with
    
        static member makeDefaultUninitialized dispatcherName optName =
            let id = NuCore.makeId ()
            { Id = id
              Name = match optName with None -> string id | Some name -> name
              Position = Vector2.Zero
              Depth = 0.0f
              Size = DefaultEntitySize
              Rotation = 0.0f
              Visible = true
              OptOverlayName = Some dispatcherName
              Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }}

        static member makeDefault dispatcherName optName dispatcherContainer =
            let entity = Entity.makeDefaultUninitialized dispatcherName optName
            Entity.init entity dispatcherContainer

        static member writeToXml overlayer (writer : XmlWriter) (entity : Entity) =
            writer.WriteStartElement typeof<Entity>.Name
            Xtension.writeTargetProperties
                (fun propertyName -> not <| Overlayer.isPropertyOverlaid3 propertyName entity overlayer)
                writer
                entity
            writer.WriteEndElement ()

        static member writeManyToXml overlayer writer (entities : Map<_, _>) =
            for entityKvp in entities do
                Entity.writeToXml overlayer writer entityKvp.Value

        static member readFromXml (entityNode : XmlNode) defaultDispatcherName world =
            let entity = Entity.makeDefaultUninitialized defaultDispatcherName None
            Xtension.readTargetXDispatcher entityNode entity
            let entity = Entity.init entity world
            match entity.OptOverlayName with
            | None -> ()
            | Some overlayName -> Overlayer.applyOverlay None overlayName entity world.Overlayer
            Xtension.readTargetProperties entityNode entity
            entity

        static member readManyFromXml (parentNode : XmlNode) defaultDispatcherName world =
            let entityNodes = parentNode.SelectNodes EntityNodeName
            let entities =
                Seq.map
                    (fun entityNode -> Entity.readFromXml entityNode defaultDispatcherName world)
                    (enumerable entityNodes)
            Seq.toList entities

[<AutoOpen>]
module WorldEntityModule =

    type World with

        static member private optEntityFinder address world =
            // OPTIMIZATION: entity is looked up in EntitiesByAddress
            Map.tryFind address.AddrStr world.EntitiesByAddress

        static member private entityAdder address world (entity : Entity) =
            let world = { world with EntitiesByAddress = Map.add address.AddrStr entity world.EntitiesByAddress }
            let optGroupMap = Map.tryFind (Address.at 0 address) world.Entities
            match optGroupMap with
            | None ->
                let entityMap = Map.singleton (Address.at 2 address) entity
                let groupMap = Map.singleton (Address.at 1 address) entityMap
                { world with Entities = Map.add (Address.at 0 address) groupMap world.Entities }
            | Some groupMap ->
                let optEntityMap = Map.tryFind (Address.at 1 address) groupMap
                match optEntityMap with
                | None ->
                    let entityMap = Map.singleton (Address.at 2 address) entity
                    let groupMap = Map.add (Address.at 1 address) entityMap groupMap
                    { world with Entities = Map.add (Address.at 0 address) groupMap world.Entities }
                | Some entityMap ->
                    let entityMap = Map.add (Address.at 2 address) entity entityMap
                    let groupMap = Map.add (Address.at 1 address) entityMap groupMap
                    { world with Entities = Map.add (Address.at 0 address) groupMap world.Entities }

        static member private entityRemover address world =
            let world = { world with EntitiesByAddress = Map.remove address.AddrStr world.EntitiesByAddress }
            let optGroupMap = Map.tryFind (Address.at 0 address) world.Entities
            match optGroupMap with
            | None -> world
            | Some groupMap ->
                let optEntityMap = Map.tryFind (Address.at 1 address) groupMap
                match optEntityMap with
                | None -> world
                | Some entityMap ->
                    let entityMap = Map.remove (Address.at 2 address) entityMap
                    let groupMap = Map.add (Address.at 1 address) entityMap groupMap
                    { world with Entities = Map.add (Address.at 0 address) groupMap world.Entities }

        static member getEntity address world = Option.get <| World.optEntityFinder address world
        static member private setEntityWithoutEvent address entity world = World.entityAdder address world entity
        static member setEntity address entity world = 
                let oldEntity = Option.get <| World.optEntityFinder address world
                let world = World.entityAdder address world entity
                World.publish4 (ChangeEventName + address) address (EntityChangeData { OldEntity = oldEntity }) world

        static member getOptEntity address world = World.optEntityFinder address world
        static member containsEntity address world = Option.isSome <| World.getOptEntity address world
        static member private setOptEntityWithoutEvent address optEntity world =
            match optEntity with 
            | None -> World.entityRemover address world
            | Some entity -> World.entityAdder address world entity

        static member withEntity fn address world = Sim.withSimulant World.getEntity World.setEntity fn address world
        static member withEntityAndWorld fn address world = Sim.withSimulantAndWorld World.getEntity World.setEntity fn address world
        static member tryWithEntity fn address world = Sim.tryWithSimulant World.getOptEntity World.setEntity fn address world
        static member tryWithEntityAndWorld fn address world = Sim.tryWithSimulantAndWorld World.getOptEntity World.setEntity fn address world
    
        static member getEntities address world =
            match address.AddrList with
            | [screenStr; groupStr] ->
                match Map.tryFind screenStr world.Entities with
                | None -> Map.empty
                | Some groupMap ->
                    match Map.tryFind groupStr groupMap with
                    | None -> Map.empty
                    | Some entityMap -> entityMap
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member registerEntity address (entity : Entity) world =
            Entity.register address entity world

        static member unregisterEntity address world =
            let entity = World.getEntity address world
            Entity.unregister address entity world

        static member removeEntityImmediate address world =
            let world = World.publish4 (RemovingEventName + address) address NoData world
            let world = World.unregisterEntity address world
            World.setOptEntityWithoutEvent address None world

        static member removeEntity address world =
            let task =
                { ScheduledTime = world.TickTime
                  Operation = fun world -> if World.containsEntity address world then World.removeEntityImmediate address world else world }
            { world with Tasks = task :: world.Tasks }

        static member clearEntitiesImmediate address world =
            let entities = World.getEntities address world
            Map.fold
                (fun world entityName _ -> World.removeEntityImmediate (addrlist address [entityName]) world)
                world
                entities

        static member clearEntities address world =
            let entities = World.getEntities address world
            Map.fold
                (fun world entityName _ -> World.removeEntity (addrlist address [entityName]) world)
                world
                entities

        static member removeEntitiesImmediate screenAddress entityNames world =
            List.fold
                (fun world entityName -> World.removeEntityImmediate (addrlist screenAddress [entityName]) world)
                world
                entityNames

        static member removeEntities screenAddress entityNames world =
            List.fold
                (fun world entityName -> World.removeEntity (addrlist screenAddress [entityName]) world)
                world
                entityNames

        static member addEntity address entity world =
            let world =
                match World.getOptEntity address world with
                | None -> world
                | Some _ -> World.removeEntityImmediate address world
            let world = World.setEntityWithoutEvent address entity world
            let world = World.registerEntity address entity world
            World.publish4 (AddEventName + address) address NoData world

        static member addEntities groupAddress entities world =
            List.fold
                (fun world (entity : Entity) -> World.addEntity (addrstr groupAddress entity.Name) entity world)
                world
                entities