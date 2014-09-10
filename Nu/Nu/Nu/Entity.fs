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

    type Entity with

        (* OPTIMIZATION: The following dispatch forwarders are optimized. *)
        
        static member register address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Register (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.register, an entity's base dispatcher must be an EntityDispatcher."
        
        static member unregister address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.Unregister (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.unregister, an entity's base dispatcher must be an EntityDispatcher."
        
        static member propagatePhysics address (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.PropagatePhysics (entity, address, world)
            | _ -> failwith "Due to optimization in Entity.propagatePhysics, an entity's base dispatcher must be an EntityDispatcher."
        
        static member handleBodyTransformMessage address message (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.HandleBodyTransformMessage (entity, address, message, world)
            | _ -> failwith "Due to optimization in Entity.handleBodyTransformMessage, an entity's base dispatcher must be an EntityDispatcher."
        
        static member getRenderDescriptors (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetRenderDescriptors (entity, world)
            | _ -> failwith "Due to optimization in Entity.getRenderDescriptors, an entity's base dispatcher must be an EntityDispatcher."
        
        static member getQuickSize  (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetQuickSize (entity, world)
            | _ -> failwith "Due to optimization in Entity.getQuickSize, an entity's base dispatcher must be an EntityDispatcher."
        
        static member getPickingPriority (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.GetPickingPriority (entity, world)
            | _ -> failwith "Due to optimization in Entity.getPickingPriority, an entity's base dispatcher must be an EntityDispatcher."
        
        static member isTransformRelative (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as dispatcher -> dispatcher.IsTransformRelative (entity, world)
            | _ -> failwith "Due to optimization in Entity.isTransformRelative, an entity's base dispatcher must be an EntityDispatcher."

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

        static member getEntities1 world =
            seq {
                for screenKvp in world.Entities do
                    for groupKvp in screenKvp.Value do
                        for entityKvp in groupKvp.Value do
                            let address = Address.make [screenKvp.Key; groupKvp.Key; entityKvp.Key]
                            yield (address, entityKvp.Value) }
    
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

        static member registerEntity address entity world =
            Entity.register address entity world

        static member unregisterEntity address entity world =
            Entity.unregister address entity world

        static member removeEntityImmediate address entity world =
            let world = World.publish4 (RemovingEventName + address) address NoData world
            let (entity, world) = World.unregisterEntity address entity world
            let world = World.setOptEntityWithoutEvent address None world
            (entity, world)

        static member removeEntity address entity world =
            let task =
                { ScheduledTime = world.TickTime
                  Operation = fun world ->
                    match World.getOptEntity address world with
                    | Some entity -> snd <| World.removeEntityImmediate address entity world
                    | None -> world }
            let world = { world with Tasks = task :: world.Tasks }
            (entity, world)

        static member removeEntitiesImmediate groupAddress entities world =
            Sim.transformSimulants World.removeEntityImmediate groupAddress entities world

        static member removeEntities groupAddress entities world =
            Sim.transformSimulants World.removeEntity groupAddress entities world

        static member addEntity address entity world =
            let (entity, world) =
                match World.getOptEntity address world with
                | Some _ -> World.removeEntityImmediate address entity world
                | None -> (entity, world)
            let world = World.setEntityWithoutEvent address entity world
            let (entity, world) = World.registerEntity address entity world
            let world = World.publish4 (AddEventName + address) address NoData world
            (entity, world)

        static member addEntities groupAddress entities world =
            Sim.transformSimulants World.addEntity groupAddress entities world

        static member tryGetFacet facetName world =
            match Map.tryFind facetName world.Facets with
            | Some facet -> Right <| facet
            | None -> Left <| "Invalid facet name '" + facetName + "'."

        static member getFacetNamesToAdd oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToAdd = Set.difference newFacetNames oldFacetNames
            List.ofSeq facetNamesToAdd

        static member getFacetNamesToRemove oldFacetNames newFacetNames =
            let newFacetNames = Set.ofList newFacetNames
            let oldFacetNames = Set.ofList oldFacetNames
            let facetNamesToRemove = Set.difference oldFacetNames newFacetNames
            List.ofSeq facetNamesToRemove

        static member tryRemoveFacet syncing facetName optAddress entity world =
            match List.tryFind (fun facet -> Reflection.getTypeName facet = facetName) entity.FacetsNp with
            | Some facet ->
                let (entity, world) =
                    match optAddress with
                    | Some address -> facet.Unregister (entity, address, world)
                    | None -> (entity, world)
                let entity = { entity with Id = entity.Id } // hacky copy
                Reflection.detachFieldsFromSource facet entity
                let entity =
                    if syncing then entity
                    else Entity.setFacetNames (List.remove ((=) (Reflection.getTypeName facet)) entity.FacetNames) entity
                let entity = Entity.setFacetsNp (List.remove ((=) facet) entity.FacetsNp) entity
                let world =
                    match optAddress with
                    | Some address -> World.setEntity address entity world
                    | None -> world
                Right (entity, world)
            | None -> Left <| "Failure to remove facet '" + facetName + "' from entity."

        static member tryAddFacet syncing facetName optAddress entity world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                if Entity.isFacetCompatible facet entity then
                    let entity = Entity.setFacetsNp (facet :: entity.FacetsNp) entity
                    Reflection.attachFieldsFromSource facet entity
                    let entity =
                        if syncing then entity
                        else Entity.setFacetNames (Reflection.getTypeName facet :: entity.FacetNames) entity
                    match optAddress with
                    | Some address ->
                        let (entity, world) = facet.Register (entity, address, world)
                        let world = World.setEntity address entity world
                        Right (entity, world)
                    | None -> Right (entity, world)
                else Left <| "Cannot add incompatible facet '" + Reflection.getTypeName facet + "'."
            | Left error -> Left error

        static member tryRemoveFacets syncing facetNamesToRemove optAddress entity world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryRemoveFacet syncing facetName optAddress entity world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToRemove

        static member tryAddFacets syncing facetNamesToAdd optAddress entity world =
            List.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entity, world) -> World.tryAddFacet syncing facetName optAddress entity world
                    | Left _ as left -> left)
                (Right (entity, world))
                facetNamesToAdd

        static member trySetFacetNames oldFacetNames newFacetNames optAddress entity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames newFacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames newFacetNames
            match World.tryRemoveFacets false facetNamesToRemove optAddress entity world with
            | Right (entity, world) -> World.tryAddFacets false facetNamesToAdd optAddress entity world
            | Left _ as left -> left

        static member trySynchronizeFacets oldFacetNames optAddress entity world =
            let facetNamesToRemove = World.getFacetNamesToRemove oldFacetNames entity.FacetNames
            let facetNamesToAdd = World.getFacetNamesToAdd oldFacetNames entity.FacetNames
            match World.tryRemoveFacets true facetNamesToRemove optAddress entity world with
            | Right (entity, world) -> World.tryAddFacets true facetNamesToAdd optAddress entity world
            | Left _ as left -> left

        static member attachIntrinsicFacets (entity : Entity) world =
            match Xtension.getDispatcher entity.Xtension world with
            | :? EntityDispatcher as entityDispatcher ->
                let entity = { entity with Id = entity.Id } // hacky copy
                Reflection.attachIntrinsicFacetsFromSource entityDispatcher entity world.Facets
                entity
            | _ -> failwith <| "No valid entity dispatcher for entity '" + entity.Name + "'."

        static member writeEntityToXml overlayer (writer : XmlWriter) (entity : Entity) =
            writer.WriteStartElement typeof<Entity>.Name
            Serialization.writeTargetProperties 
                (fun propertyName -> Overlayer.shouldPropertySerialize3 propertyName entity overlayer)
                writer
                entity
            writer.WriteEndElement ()

        static member writeEntitiesToXml overlayer (writer : XmlWriter) (entities : Map<_, _>) =
            writer.WriteStartElement EntitiesNodeName
            for entityKvp in entities do
                World.writeEntityToXml overlayer writer entityKvp.Value
            writer.WriteEndElement ()

        static member readEntityFromXml (entityNode : XmlNode) defaultDispatcherName world =

            // make the bare entity with name as id
            let entity = Entity.make defaultDispatcherName None

            // read in the Xtension.OptXDispatcherName
            Serialization.readTargetOptXDispatcherName entityNode entity

            // attach the entity's intrinsic facets
            let entity = World.attachIntrinsicFacets entity world

            // read the entity's overlay and apply it to its facet names
            Serialization.readTargetOptOverlayName entityNode entity
            match entity.OptOverlayName with
            | Some overlayName ->
                let defaultOptDispatcherName = Some typeof<EntityDispatcher>.Name
                Overlayer.applyOverlayToFacetNames defaultOptDispatcherName overlayName entity world.Overlayer world.Overlayer
            | None -> ()

            // read the entity's facet names, and synchronize its facets 
            Serialization.readTargetFacetNames entityNode entity
            let entity =
                match World.trySynchronizeFacets [] None entity world with
                | Right (entity, _) -> entity
                | Left error -> debug error; entity

            // attach the entity's instrinsic fields from its dispatcher if any
            match entity.Xtension.OptXDispatcherName with
            | Some dispatcherName ->
                match Map.tryFind dispatcherName world.Dispatchers with
                | Some dispatcher ->
                    match dispatcher with
                    | :? EntityDispatcher -> Reflection.attachFieldsFromSource dispatcher entity
                    | _ -> note <| "Dispatcher '" + dispatcherName + "' is not an entity dispatcher."
                | None -> note <| "Could not locate dispatcher '" + dispatcherName + "'."
            | None -> ()

            // apply the entity's overlay
            match entity.OptOverlayName with
            | Some overlayName -> Overlayer.applyOverlay None overlayName entity world.Overlayer
            | None -> ()

            // read the entity's properties
            Serialization.readTargetProperties entityNode entity

            // return the initialized entity
            entity

        static member readEntitiesFromXml (parentNode : XmlNode) defaultDispatcherName world =
            match parentNode.SelectSingleNode EntitiesNodeName with
            | null -> Map.empty
            | entitiesNode ->
                let entityNodes = entitiesNode.SelectNodes EntityNodeName
                Seq.fold
                    (fun entities entityNode ->
                        let entity = World.readEntityFromXml entityNode defaultDispatcherName world
                        Map.add entity.Name entity entities)
                    Map.empty
                    (enumerable entityNodes)

        static member makeEntity dispatcherName optName world =
            let entity = Entity.make dispatcherName optName
            let entity = World.attachIntrinsicFacets entity world
            let entityDispatcher = Map.find dispatcherName world.Dispatchers
            Reflection.attachFieldsFromSource entityDispatcher entity
            match World.trySynchronizeFacets [] None entity world with
            | Right (entity, _) -> entity
            | Left error -> debug error; entity