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

    type EntityDispatcher () =

        abstract member Init : Entity * EntityFacet list * IXDispatcherContainer -> Entity
        default dispatcher.Init (entity, facets, _) =
            let entity = Entity.setFacetsNp facets entity
            List.fold (fun entity (facet : EntityFacet) -> facet.Attach entity) entity entity.FacetsNp

        abstract member Register : Entity * Address * World -> World
        default dispatcher.Register (entity, address, world) =
            List.fold (fun world (facet : EntityFacet) -> facet.RegisterPhysics (entity, address, world)) world entity.FacetsNp

        abstract member Unregister : Entity * Address * World -> World
        default dispatcher.Unregister (entity, address, world) =
            List.fold (fun world (facet : EntityFacet) -> facet.UnregisterPhysics (entity, address, world)) world entity.FacetsNp

        abstract member PropagatePhysics : Entity * Address * World -> World
        default dispatcher.PropagatePhysics (entity, address, world) =
            List.fold (fun world (facet : EntityFacet) -> facet.PropagatePhysics (entity, address, world)) world entity.FacetsNp

        abstract member HandleBodyTransformMessage : Entity * Address * BodyTransformMessage * World -> World
        default dispatcher.HandleBodyTransformMessage (entity, address, message, world) =
            List.fold (fun world (facet : EntityFacet) -> facet.HandleBodyTransformMessage (entity, address, message, world)) world entity.FacetsNp

        abstract member GetRenderDescriptors : Entity * World -> RenderDescriptor list
        default dispatcher.GetRenderDescriptors (entity, world) =
            List.fold
                (fun renderDescriptors (facet : EntityFacet) ->
                    let descriptors = facet.GetRenderDescriptors (entity, world)
                    descriptors @ renderDescriptors)
                []
                entity.FacetsNp

        abstract member GetQuickSize : Entity * World -> Vector2
        default dispatcher.GetQuickSize (_, _) = DefaultEntitySize

        abstract member IsTransformRelative : Entity * World -> bool
        default dispatcher.IsTransformRelative (_, _) = true

        abstract member GetPickingPriority : Entity * World -> single
        default dispatcher.GetPickingPriority (entity, _) = entity.Depth

    type Entity with

        (* OPTIMIZATION: The following dispatch forwarders are optimized. *)

        static member init (entity : Entity) dispatcherContainer =
            match Xtension.getDispatcher entity.Xtension dispatcherContainer with
            | :? EntityDispatcher as dispatcher -> dispatcher.Init (entity, [], dispatcherContainer)
            | _ -> failwith "Due to optimization in Entity.init, an entity's base dispatcher must be an EntityDispatcher."
        
        static member register address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Register (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.register, an entity's base dispatcher must be an EntityDispatcher."
        
        static member unregister address (entity : Entity) (world : World) : World =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Unregister (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.unregister, an entity's base dispatcher must be an EntityDispatcher."
        
        static member propagatePhysics address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.PropagatePhysics (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.propagatePhysics, an entity's 2d dispatcher must be an EntityDispatcher."
        
        static member handleBodyTransformMessage address message (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.HandleBodyTransformMessage (entity, address, message, world)
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
        
        static member isTransformRelative (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.IsTransformRelative (entity, world)
            | _ -> failwith "Due to optimization in Entity.isTransformRelative, an entity's 2d dispatcher must be an EntityDispatcher."

    type [<StructuralEquality; NoComparison>] TileMapData =
        { Map : TmxMap
          MapSize : Vector2I
          TileSize : Vector2I
          TileSizeF : Vector2
          TileMapSize : Vector2I
          TileMapSizeF : Vector2
          TileSet : TmxTileset
          TileSetSize : Vector2I }

    type [<StructuralEquality; NoComparison>] TileData =
        { Tile : TmxLayerTile
          I : int
          J : int
          Gid : int
          GidPosition : int
          Gid2 : Vector2I
          OptTileSetTile : TmxTilesetTile option
          TilePosition : Vector2I }

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
              Xtension = { XFields = Map.empty; OptXDispatcherName = Some dispatcherName; CanDefault = true; Sealed = false }
              FacetNames = []
              FacetsNp = [] }

        static member makeDefault dispatcherName optName dispatcherContainer =
            let entity = Entity.makeDefaultUninitialized dispatcherName optName
            Entity.init entity dispatcherContainer

[<AutoOpen>]
module WorldEntityModule =

    type World with

        static member private optEntityFinder address world =
            match address.AddrList with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap -> Map.tryFind entityName entityMap
                    | None -> None
                | None -> None
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member private entityAdder address world (entity : Entity) =
            match address.AddrList with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap ->
                        let entityMap = Map.add entityName entity entityMap
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                    | None ->
                        let entityMap = Map.singleton entityName entity
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                | None ->
                    let entityMap = Map.singleton entityName entity
                    let groupMap = Map.singleton groupName entityMap
                    { world with Entities = Map.add screenName groupMap world.Entities }
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

        static member private entityRemover address world =
            match address.AddrList with
            | [screenName; groupName; entityName] ->
                let optGroupMap = Map.tryFind screenName world.Entities
                match optGroupMap with
                | Some groupMap ->
                    let optEntityMap = Map.tryFind groupName groupMap
                    match optEntityMap with
                    | Some entityMap ->
                        let entityMap = Map.remove entityName entityMap
                        let groupMap = Map.add groupName entityMap groupMap
                        { world with Entities = Map.add screenName groupMap world.Entities }
                    | None -> world
                | None -> world
            | _ -> failwith <| "Invalid entity address '" + string address + "'."

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
            | Some entity -> World.entityAdder address world entity
            | None -> World.entityRemover address world

        static member withEntity fn address world = Sim.withSimulant World.getEntity World.setEntity fn address world
        static member withEntityAndWorld fn address world = Sim.withSimulantAndWorld World.getEntity World.setEntity fn address world
        static member tryWithEntity fn address world = Sim.tryWithSimulant World.getOptEntity World.setEntity fn address world
        static member tryWithEntityAndWorld fn address world = Sim.tryWithSimulantAndWorld World.getOptEntity World.setEntity fn address world
    
        static member getEntities address world =
            match address.AddrList with
            | [screenStr; groupStr] ->
                match Map.tryFind screenStr world.Entities with
                | Some groupMap ->
                    match Map.tryFind groupStr groupMap with
                    | Some entityMap -> entityMap
                    | None -> Map.empty
                | None -> Map.empty
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
                | Some _ -> World.removeEntityImmediate address world
                | None -> world
            let world = World.setEntityWithoutEvent address entity world
            let world = World.registerEntity address entity world
            World.publish4 (AddEventName + address) address NoData world

        static member addEntities groupAddress entities world =
            List.fold
                (fun world (entity : Entity) -> World.addEntity (addrlist groupAddress [entity.Name]) entity world)
                world
                entities

        static member tryGetFacetsToAdd facetNames entity world =
            let newFacetNames = Set.ofList facetNames
            let oldFacetNames = Set.ofList entity.FacetNames
            let addedFacetNames = Set.difference newFacetNames oldFacetNames
            Set.fold
                (fun eitherFacetsToAdd facetName ->
                    match eitherFacetsToAdd with
                    | Right facetsToAdd ->
                        match Map.tryFind facetName world.Facets with
                        | Some facet ->
                            match facet with
                            | :? EntityFacet as facet -> Right (facet :: facetsToAdd)
                            | _ -> Left <| "Invalid entity facet '" + facetName + "'."
                        | None -> Left <| "Invalid facet name '" + facetName + "'."
                    | Left _ as left -> left)
                (Right [])
                addedFacetNames

        static member tryGetFacetsToRemove facetNames entity =
            let newFacetNames = Set.ofList facetNames
            let oldFacetNames = Set.ofList entity.FacetNames
            let removedFacetNames = Set.difference oldFacetNames newFacetNames
            Set.fold
                (fun eitherFacetsToRemove facetName ->
                    match eitherFacetsToRemove with
                    | Right facetsToRemove ->
                        match List.tryFind (fun facet -> EntityFacet.getName facet = facetName) entity.FacetsNp with
                        | Some facet -> Right <| facet :: facetsToRemove
                        | None -> Left <| "Invalid facet name '" + facetName + "'."
                    | Left _ as left -> left)
                (Right [])
                removedFacetNames

        static member removeFacet (facet : EntityFacet) address entity world =
            let world = facet.UnregisterPhysics (entity, address, world)
            let entity = facet.Detach entity
            let entity = Entity.setFacetNames (List.remove ((=) (EntityFacet.getName facet)) entity.FacetNames) entity
            let entity = Entity.setFacetsNp (List.remove ((=) facet) entity.FacetsNp) entity
            let world = World.setEntity address entity world
            (entity, world)

        static member tryAddFacet (facet : EntityFacet) address entity world =
            if Entity.isFacetCompatible facet entity then
                let entity = Entity.setFacetsNp (facet :: entity.FacetsNp) entity
                let entity = Entity.setFacetNames (EntityFacet.getName facet :: entity.FacetNames) entity
                let entity = facet.Attach entity
                let world = facet.RegisterPhysics (entity, address, world)
                let world = World.setEntity address entity world
                Right (entity, world)
            else Left <| "Cannot add incompatible facet '" + (facet.GetType ()).Name + "'."

        static member removeFacets facetsToRemove address entity world =
            List.fold
                (fun (entity, world) (facet : EntityFacet) -> World.removeFacet facet address entity world)
                (entity, world)
                facetsToRemove

        static member tryAddFacets facetsToAdd address entity world =
            List.fold
                (fun eitherEntityWorld (facet : EntityFacet) ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryAddFacet facet address entity world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetsToAdd

        static member trySetFacetNames facetNames address entity world =
            match World.tryGetFacetsToAdd facetNames entity world with
            | Right facetsToAdd ->
                match World.tryGetFacetsToRemove facetNames entity with
                | Right facetsToRemove ->
                    let (entity, world) = World.removeFacets facetsToRemove address entity world
                    match World.tryAddFacets facetsToAdd address entity world with
                    | Right (_, world) -> Right world
                    | Left error -> Left error
                | Left error -> Left error
            | Left error -> Left error

        static member writeEntityToXml overlayer (writer : XmlWriter) (entity : Entity) =
            writer.WriteStartElement typeof<Entity>.Name
            Xtension.writeTargetProperties 
                (fun propertyName -> Overlayer.shouldPropertySerialize3 propertyName entity overlayer)
                writer
                entity
            writer.WriteEndElement ()

        static member writeEntitiesToXml overlayer writer (entities : Map<_, _>) =
            for entityKvp in entities do
                World.writeEntityToXml overlayer writer entityKvp.Value

        static member readEntityFromXml (entityNode : XmlNode) defaultDispatcherName world =
            let entity = Entity.makeDefaultUninitialized defaultDispatcherName None
            Xtension.readTargetXDispatcher entityNode entity
            let entity = Entity.init entity world
            match entity.OptOverlayName with
            | Some overlayName -> Overlayer.applyOverlay None overlayName entity world.Overlayer
            | None -> ()
            Xtension.readTargetProperties entityNode entity
            entity

        static member readEntitiesFromXml (parentNode : XmlNode) defaultDispatcherName world =
            let entityNodes = parentNode.SelectNodes EntityNodeName
            let entities =
                Seq.map
                    (fun entityNode -> World.readEntityFromXml entityNode defaultDispatcherName world)
                    (enumerable entityNodes)
            Seq.toList entities